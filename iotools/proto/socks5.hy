(require
  dash *
  iotools.struct *)

(import
  dash *
  dash.strtools :as s
  enum [IntEnum]
  hashlib [sha224]
  socket
  iotools.struct *
  iotools.proto.base *
  iotools.proto.http *)

(defclass Socks5AddrType [IntEnum]
  (setv DN 3 V4 1 V6 4))

(defstruct Socks5V4Host
  [bytes
   :len 4
   :from (socket.inet-pton socket.AF-INET it)
   :to (socket.inet-ntop socket.AF-INET it)])

(defstruct Socks5V6Host
  [bytes
   :len 16
   :from (socket.inet-pton socket.AF-INET6 it)
   :to (socket.inet-ntop socket.AF-INET6 it)])

(defstruct Socks5DNHost
  [vbytes :len 1 :from (s.encode it) :to (s.decode it)])

(defstruct Socks5Addr
  [[type [int :len 1]]
   [host [cond
          :cond [(= type Socks5AddrType.DN) Socks5DNHost
                 (= type Socks5AddrType.V4) Socks5V4Host
                 (= type Socks5AddrType.V6) Socks5V6Host]]]
   [port [int :len 2]]])



(defstruct Socks5Request
  [struct
   :spec [[version1 [int :len 1 :to-validate (= it 5)]]
          [methods [vbytes :len 1 :to-validate (in 0 it)]]
          [version2 [int :len 1 :to-validate (= it 5)]]
          [command [int :len 1 :to-validate (= it 1)]]
          [reserved [int :len 1 :to-validate (= it 0)]]
          [#(type host port) Socks5Addr]]
   :from (let [#(host port) it]
           #(5 b"\x00" 5 1 0 Socks5AddrType.DN host port))
   :to (let [#(version1 methods version2 command reserved type host port) it]
         #(host port))])

(defstruct Socks5Reply
  [struct
   :spec [[version1 [int :len 1 :to-validate (= it 5)]]
          [method [int :len 1 :to-validate (= it 0)]]
          [version2 [int :len 1 :to-validate (= it 5)]]
          [reply [int :len 1 :to-validate (= it 0)]]
          [reserved [int :len 1 :to-validate (= it 0)]]
          [addr Socks5Addr]]
   :from #(5 0 5 0 0 it)
   :to (let [#(version1 method version2 reply reserved addr) it] addr)])

(defstruct TrojanRequest
  [struct
   :spec [[auth HTTPLine]
          [command [int :len 1 :to-validate (= it 1)]]
          [#(type host port) Socks5Addr]
          [empty HTTPEmptyLine]]
   :from (let [#(auth host port) it]
           #(auth 1 Socks5AddrType.DN host port b"\r\n"))
   :to (let [#(auth command type host port empty) it]
         #(auth host port))])



(setv SOCKS5-REPLY (Socks5Reply.pack #(Socks5AddrType.V4 "0.0.0.0" 0)))

(do/a!
  (defclass (name/a! Socks5Connector) [(name/a! ProxyConnector)]
    (defn/a! proxy-connect [self next-stream host port]
      (wait/a! (.write next-stream (Socks5Request.pack #(host port))))
      (wait/a! (.flush next-stream))
      (wait/a! (.read-loop next-stream Socks5Reply.read))
      next-stream))

  (defclass (name/a! Socks5Acceptor) [(name/a! ProxyAcceptor)]
    (defn/a! proxy-accept [self next-stream]
      (wait/a! (.write next-stream SOCKS5-REPLY))
      (wait/a! (.flush next-stream))
      (let [#(host port) (wait/a! (.read-loop next-stream Socks5Request.read))]
        #(host port next-stream))))

  (defclass (name/a! AutoProxyAcceptor) [(name/a! ProxyAcceptor)]
    (defn/a! proxy-accept [self next-stream]
      (unless next-stream.read-buf
        (wait/a! (.peek-more next-stream)))
      (let [acceptor-class (if (= (get next-stream.read-buf 0) 5)
                               (name/a! Socks5Acceptor)
                               (name/a! HTTPProxyAcceptor))]
        (wait/a! (.proxy-accept (acceptor-class) next-stream))))))



(defn trojan-auth [password]
  (.hexdigest (sha224 (s.encode password))))

(defclass TrojanHandshakerMixin []
  (defn __init__ [self auth #** kwargs]
    (.__init__ (super) #** kwargs)
    (setv self.auth auth)))

(do/a!
  (defclass (name/a! TrojanConnector) [TrojanHandshakerMixin (name/a! ProxyConnector)]
    (defn/a! proxy-connect [self next-stream host port]
      (wait/a! (.write next-stream (TrojanRequest.pack #(self.auth host port))))
      next-stream))

  (defclass (name/a! TrojanAcceptor) [TrojanHandshakerMixin (name/a! ProxyConnector)]
    (defn/a! proxy-accept [self next-stream]
      (let [#(auth host port) (wait/a! (.read-loop next-stream TrojanRequest.read))]
        (unless (= auth self.auth)
          (raise RuntimeError))
        #(host port next-stream)))))



(export
  :objects [Socks5AddrType Socks5Addr
            SyncSocks5Connector AsyncSocks5Connector SyncSocks5Acceptor AsyncSocks5Acceptor
            SyncAutoProxyAcceptor AsyncAutoProxyAcceptor
            trojan-auth SyncTrojanConnector AsyncTrojanConnector SyncTrojanAcceptor AsyncTrojanAcceptor])
