(require
  dash *
  iotools.struct *)

(import
  dash *
  dash.strtools :as s
  enum [IntEnum]
  random [randbytes]
  base64 [b64encode b64decode]
  hashlib [sha1]
  iotools.struct *
  iotools.util *
  iotools.proto.base *
  iotools.proto.http *)

(setv WS-MAGIC "258EAFA5-E914-47DA-95CA-C5AB0DC85B11")

(defn ws-mask-data [mask data]
  (let [mask (-cycle mask)
        data (bytearray data)]
    (--dotimes (len data) (^= (get data it) (next mask)))
    (bytes data)))

(defn ws-rand-key []
  (s.decode (b64encode (randbytes 16))))

(defn ws-accept [key]
  (-> (+ key WS-MAGIC)
      (s.encode)
      sha1
      (.digest)
      b64encode
      (s.decode)))

(defclass WSType [IntEnum]
  (setv Continue 0x00
        Text     0x01
        Binary   0x02
        Close    0x08
        Ping     0x09
        Pong     0x10))

(defstruct WSFrame
  [struct
   :spec [[#(fin type) [bits :lens [1 7]]]
          [#(M N) [bits :lens [1 7]]]
          [n [cond
              :cond [(= N 126) [int :len 2]
                     (= N 127) [int :len 8]
                     (<= 0 N 125) [const :const N]]]]
          [mask [if
                 :pred M
                 :then [bytes :len 4]
                 :else [const :const b""]]]
          [data [cond
                 :cond [(= n 0) [const :const b""]
                        (> n 0) [bytes :len n]]]]]
   :from (let [#(fin type mask data) it
               n (len data)
               N (cond (< n 126) n (< n 65536) 126 True 127)]
           #((int fin) type (int (bool mask)) N n mask data))
   :to (let [#(fin type M N n mask data) it]
         #((bool fin) type mask data))])



(do/a!
  (defclass (name/a! WSStream) [(name/a! LayeredStream)]
    (defn __init__ [self [do-mask True] #** kwargs]
      (.__init__ (super) #** kwargs)
      (setv self.do-mask do-mask
            self.write-lock ((name/a! Lock))
            self.flush-lock ((name/a! Lock))))

    (defn/a! ws-read-frame [self]
      (let [#(fin type mask data) (wait/a! (.read-loop self.next-layer WSFrame.read))]
        (when mask
          (setv data (ws-mask-data mask data)))
        #(fin type data)))

    (defn/a! ws-read-message [self]
      (let [#(fin type data) (wait/a! (.ws-read-frame self))
            bs [data]]
        (while (not fin)
          (let [#(next-fin next-type next-data) (wait/a! (.ws-read-frame self))]
            (unless (= next-type type)
              (raise RuntimeError))
            (setv fin next-fin)
            (.append bs next-data)))
        #(type (s.concatb-in bs))))

    (defn/a! ws-write-frame [self fin type mask data]
      (ignore
        (with/a! [_ self.write-lock]
          (when mask
            (setv data (ws-mask-data mask data)))
          (wait/a! (.write self.next-layer (WSFrame.pack #(fin type mask data)))))))

    (defn/a! ws-write-message [self type data]
      (ignore
        (let [mask (if self.do-mask (randbytes 4) b"")]
          (wait/a! (.ws-write-frame self True type mask data)))))

    (defn/a! real-read [self]
      (while True
        (let [#(type data) (wait/a! (.ws-read-message self))]
          (cond (= type WSType.Continue) (continue)
                (= type WSType.Text)     (return data)
                (= type WSType.Binary)   (return data)
                (= type WSType.Close)    (return b"")
                (= type WSType.Pong)     (continue)
                (= type WSType.Ping)     (do
                                           (wait/a! (.ws-write-message self WSType.Pong data))
                                           (wait/a! (.flush self))
                                           (continue))
                True (raise RuntimeError)))))

    (defn/a! real-write [self b]
      (ignore (wait/a! (.ws-write-message self WSType.Binary b))))

    (defn/a! shutdown [self]
      (ignore (wait/a! (.ws-write-message self WSType.Close b""))))

    (defn/a! flush [self]
      (ignore
        (with/a! [_ self.flush-lock]
          (wait/a! (.flush (super))))))))



(do/a!
  (defclass (name/a! WSConnector) [(name/a! Handshaker)]
    (defn __init__ [self host [path "/"] [extra-headers None] #** kwargs]
      (.__init__ (super) #** kwargs)
      (setv #(self.host self.path) #(host path)
            self.key (ws-rand-key)
            self.extra-headers extra-headers))

    (defn make-request [self]
      (let [headers {"Host" self.host
                     "Upgrade" "websocket"
                     "Connection" "Upgrade"
                     "Sec-WebSocket-Key" self.key
                     "Sec-WebSocket-Version" "13"}]
        (when self.extra-headers
          (setv headers (-merge headers self.extra-headers)))
        (HTTPRequest.pack
          #("GET" self.path "HTTP/1.1" headers))))

    (defn/a! real-handshake [self next-stream]
      (wait/a! (.write next-stream (.make-request self)))
      (wait/a! (.flush next-stream))
      (let [#(version status reason headers)
            (wait/a! (.read-loop next-stream HTTPResponse.read))]
        (unless (= status "101")
          (raise HTTPStatusError))
        (unless (= (get headers "Sec-WebSocket-Accept") (ws-accept self.key))
          (raise ValueError)))
      ((name/a! WSStream) :do-mask True :next-layer next-stream)))

  (defclass (name/a! WSAcceptor) [(name/a! Handshaker)]
    (defn make-response [self key]
      (let [headers {"Upgrade" "websocket"
                     "Connection" "Upgrade"
                     "Sec-WebSocket-Accept" (ws-accept key)}]
        (HTTPResponse.pack #("HTTP/1.1" "101" "Switching Protocols" headers))))

    (defn/a! real-handshake [self next-stream]
      (let [#(method path version headers)
            (wait/a! (.read-loop next-stream HTTPResponse.read))
            key (get headers "Sec-WebSocket-Key")]
        (wait/a! (.write next-stream (.make-response self key)))
        (wait/a! (.flush next-stream))
        (setv #(self.path self.headers) #(path headers))
        ((name/a! WSStream) :do-mask False :next-layer next-stream)))))



(export
  :objects [WSType WSFrame SyncWSStream AsyncWSStream
            SyncWSConnector AsyncWSConnector SyncWSAcceptor AsyncWSAcceptor])
