(require
  dash *
  iotools.struct *)

(import
  dash *
  dash.strtools :as s
  base64 [b64encode b64decode]
  iotools.struct *
  iotools.proto.base *)

(defclass HTTPStatusError [RuntimeError])



(defn http-pack-addr [host [port 80]]
  ;; example:
  ;;   www.google.com,80 => www.google.com:80
  ;;   240c::6666,53 => [240c::6666]:53
  (s.format
    (if (>= (s.find host ":") 0) "[{}]:{}" "{}:{}")
    host port))

(defn http-unpack-addr [addr [default-port 80]]
  ;; example:
  ;;   www.google.com:80 => www.google.com,80
  ;;   [240c::6666]:53 => 240c::6666,53
  (if (s.starts-with? addr "[")
      (let [end (s.find addr "]")
            hostlen (inc end)]
        (cond (< end 0) (raise ValueError)
              (= hostlen (len addr)) #((cut addr 1 -1) default-port)
              (= (get addr hostlen) ":") #((cut addr 1 end) (int (cut addr (inc hostlen) None)))
              True (raise ValueError)))
      (let [index (s.find addr ":")]
        (if (< index 0)
            #(addr default-port)
            #((cut addr index) (int (cut addr (inc index) None)))))))

(defn http-pack-headers [headers]
  ;; example:
  ;;   {"Host" "www.google.com" "Connection" "close"} =>
  ;;     ["Host: www.google.com", "Connection: close", ""]
  (->
    (->> (-items headers)
         (--map (let [#(k v) it] (s.format "{}: {}" k v)))
         list)
    (-conj! "")))

(defn http-unpack-headers [headers]
  (->> (-pop! headers)
       (--map (let [#(k v) (s.split it ":" 1)] #((s.strip k) (s.strip v))))
       dict))



(defstruct HTTPLine
  [line :sep b"\r\n" :from (s.encode it) :to (s.decode it)])

(defstruct HTTPEmptyLine
  [bytes :len 2 :to-validate (= it b"\r\n")])

(defstruct _HTTPFirstLine
  [struct :spec HTTPLine :from (s.join-in " " it) :to (s.split it :maxsplit 2)])

(defstruct HTTPHeaders
  ;; example:
  ;;   (HTTPHeaders.pack {"Host" "www.google.com" "Connection" "close"}) =>
  ;;     b"Host: www.google.com\r\nConnection: close\r\n\r\n"
  ;;   (HTTPHeaders.unpack b"Host: www.google.com\r\nConnection: close\r\n\r\n") =>
  ;;     {"Host" "www.google.com" "Connection" "close"}
  [while
   :pred it
   :spec HTTPLine
   :from (http-pack-headers it)
   :to (http-unpack-headers it)])

(defstruct HTTPRequest
  [[#(method path version) _HTTPFirstLine]
   [headers HTTPHeaders]])

(defstruct HTTPResponse
  [[#(version status reason) _HTTPFirstLine]
   [headers HTTPHeaders]])

(defstruct HTTPChunk
  [struct
   :spec [[n [struct
              :spec HTTPLine
              :from (str it)
              :to (if it (int it) 0)]]
          [data [cond
                 :cond [(= n 0) [const :const b""]
                        (> n 0) [bytes :len n]]]]
          [empty HTTPEmptyLine]]
   :from #((len it) it b"\r\n")
   :to (let [#(n data empty) it] data)])



(setv HTTP-BASIC-AUTH-MAGIC b"\xc2\xa3")

(defn http-basic-auth-encode [user password]
  (-> (s.encode (s.format "{}:{}" user password))
      (+ HTTP-BASIC-AUTH-MAGIC)
      b64encode
      (s.decode)))

(defn http-basic-auth-decode [auth]
  (when (str? auth)
    (setv auth (s.encode auth)))
  (let [auth (b64decode auth)]
    (when (not (s.ends-with? auth HTTP-BASIC-AUTH-MAGIC))
      (raise ValueError))
    (let [#(user password) (-> auth
                               (s.remove-suffix HTTP-BASIC-AUTH-MAGIC)
                               (s.decode)
                               (s.split ":" 1))]
      #(user password))))

(defn http-basic-auth [user password]
  (s.format "Basic realm=\"{}\"" (http-basic-auth-encode user password)))



(do/a!
  (defclass (name/a! HTTPChunkedStream) [(name/a! LayeredStream)]
    (defn/a! real-read [self]
      (wait/a! (.read-loop self.next-layer HTTPChunk.read)))

    (defn/a! real-write [self b]
      (ignore (wait/a! (.write self.next-layer (HTTPChunk.pack b)))))

    (defn/a! shutdown [self]
      (ignore (wait/a! (.write self.next-layer (HTTPChunk.pack b"")))))))



(do/a!
  (defclass (name/a! HTTPProxyConnector) [(name/a! ProxyConnector)]
    (defn __init__ [self [extra-headers None] #** kwargs]
      (.__init__ (super) #** kwargs)
      (setv self.extra-headers extra-headers))

    (defn make-request [self host port]
      (let [addr (http-pack-addr host port)
            headers {"Host" addr}]
        (when self.extra-headers
          (setv headers (-merge headers self.extra-headers)))
        (HTTPRequest.pack #("CONNECT" addr "HTTP/1.1" headers))))

    (defn/a! proxy-connect [self next-stream host port]
      (wait/a! (.write next-stream (.make-request self host port)))
      (wait/a! (.flush next-stream))
      (let [#(version status reason headers)
            (wait/a! (.read-loop next-stream HTTPResponse.read))]
        (if (= status "200") next-stream (raise HTTPStatusError)))))

  (defclass (name/a! HTTPProxyAcceptor) [(name/a! ProxyAcceptor)]
    (defn make-response [self]
      (HTTPResponse.pack #("HTTP/1.1" "200" "OK" {"Connection" "close"})))

    (defn/a! proxy-accept [self next-stream]
      (let [#(method path version headers)
            (wait/a! (.read-loop next-stream HTTPRequest.read))]
        (if (= method "CONNECT")
            (do
              (wait/a! (.write next-stream (.make-response self)))
              (wait/a! (.flush next-stream)))
            (let [request (HTTPRequest.pack
                            #(method path version
                                     (--filter-keys headers (not (s.starts-with? it "Proxy-")))))]
              (.extendleft next-stream.read-buf request)))
        (let [#(host port) (http-unpack-addr (.get headers "Host" path))]
          #(host port next-stream))))))



(export
  :objects [HTTPStatusError HTTPLine HTTPEmptyLine HTTPHeaders HTTPRequest HTTPResponse HTTPChunk
            http-basic-auth-encode http-basic-auth-decode http-basic-auth
            SyncHTTPChunkedStream AsyncHTTPChunkedStream
            SyncHTTPProxyConnector AsyncHTTPProxyConnector SyncHTTPProxyAcceptor AsyncHTTPProxyAcceptor])
