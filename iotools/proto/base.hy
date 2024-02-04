(require
  dash *)

(import
  dash *
  iotools.stream *)

(defclass LayeredMixin []
  (defn __init__ [self [next-layer None] #** kwargs]
    (.__init__ (super) #** kwargs)
    (setv self.next-layer next-layer)))

(do/a!
  (defclass (name/a! LayeredStream) [LayeredMixin (name/a! Stream)]
    (defn/a! real-close [self]
      (ignore (wait/a! (.close self.next-layer))))

    (defn/a! flush [self]
      (ignore
        (wait/a! (.flush (super)))
        (wait/a! (.flush self.next-layer))))))

(do/a!
  (defclass (name/a! Handshaker) [LayeredMixin]
    (defn/a! real-handshake [self next-stream]
      (raise NotImplementedError))

    (defn/a! handshake [self lowest-stream]
      (if self.next-layer
          (let [next-stream (wait/a! (.connect self.next-layer lowest-stream))]
            (try
              (wait/a! (.real-handshake self next-stream))
              (except [Exception]
                (wait/a! (.close next-stream))
                (raise))))
          (wait/a! (.real-handshake self lowest-stream))))))

(do/a!
  (defclass (name/a! ProxyConnector) [(name/a! Handshaker)]
    (defn __init__ [self host port #** kwargs]
      (.__init__ (super) #** kwargs)
      (setv #(self.host self.port) #(host port)))

    (defn/a! proxy-connect [self next-stream host port]
      (raise NotImplementedError))

    (defn/a! real-handshake [self next-stream]
      (wait/a! (.proxy-connect self next-stream self.host self.port))))

  (defclass (name/a! ProxyAcceptor) [(name/a! Handshaker)]
    (defn/a! proxy-accept [self next-stream]
      (raise NotImplementedError))

    (defn/a! real-handshake [self next-stream]
      (let [#(host port stream) (wait/a! (.proxy-accept self next-stream))]
        (setv #(self.host self.port) #(host port))
        stream))))

(export
  :objects [SyncLayeredStream AsyncLayeredStream SyncHandshaker AsyncHandshaker
            SyncProxyConnector AsyncProxyConnector SyncProxyAcceptor AsyncProxyAcceptor])
