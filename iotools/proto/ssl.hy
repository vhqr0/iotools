(require
  dash *)

(import
  dash *
  ssl
  iotools.proto.base *)

(do/a!
  (defclass (name/a! SSLStream) [(name/a! LayeredStream)]
    (defn __init__ [self ssl-object ssl-incoming ssl-outgoing #** kwargs]
      (.__init__ (super) #** kwargs)
      (setv self.ssl-object ssl-object
            self.ssl-incoming ssl-incoming
            self.ssl-outgoing ssl-outgoing))

    (defn/a! [classmethod] wrap [cls next-stream [ssl-context None]
                                 [server-side False] [server-hostname None] [session None]]
      (unless ssl-context
        (setv ssl-context (ssl.create-default-context)))
      (let [ssl-incoming (ssl.MemoryBIO)
            ssl-outgoing (ssl.MemoryBIO)
            ssl-object (ssl-context.wrap-bio ssl-incoming ssl-outgoing
                                             :server-side server-side
                                             :server-hostname server-hostname
                                             :session session)
            stream (cls :ssl-object ssl-object
                        :ssl-incoming ssl-incoming
                        :ssl-outgoing ssl-outgoing
                        :next-layer next-stream)]
        (wait/a! (.ssl-handshake stream))
        stream))

    (defn/a! ssl-read [self]
      (if-let [b (wait/a! (.read self.next-layer))]
        (.write self.ssl-incoming b)
        (raise ssl.SSLEOFError)))

    (defn/a! ssl-write [self]
      (ignore
        (when-let [b (.read self.ssl-outgoing)]
          (wait/a! (.write self.next-layer b)))))

    (defn/a! ssl-operate-loop [self operatefn]
      (while True
        (try
          (let [result (operatefn self.ssl-object)]
            (wait/a! (.ssl-write self))
            (return result))
          (except [ssl.SSLWantReadError]
            (wait/a! (.ssl-write self))
            (wait/a! (.flush self.next-layer))
            (wait/a! (.ssl-read self))))))

    (defn/a! ssl-handshake [self]
      (ignore (wait/a! (.ssl-operate-loop self (fn [ssl-object] (.do-handshake ssl-object))))))

    (defn/a! real-read [self]
      (wait/a! (.ssl-operate-loop self (fn [ssl-object] (.read ssl-object 4096)))))

    (defn/a! real-write [self b]
      (ignore (wait/a! (.ssl-operate-loop self (fn [ssl-object] (.write ssl-object b))))))

    (defn/a! shutdown [self]
      (ignore (wait/a! (.ssl-operate-loop self (fn [ssl-object] (.unwrap ssl-object))))))))

(do/a!
  (defclass (name/a! SSLHandshaker) [(name/a! Handshaker)]
    (defn __init__ [self [ssl-context None]
                    [server-side False] [server-hostname None] [session None]
                    #** kwargs]
      (.__init__ (super) #** kwargs)
      (setv self.ssl-context ssl-context
            self.server-side server-side
            self.server-hostname server-hostname
            self.session session))

    (defn/a! real-handshake [self next-stream]
      (wait/a!
        (.wrap (name/a! SSLStream) next-stream self.ssl-context
               :server-side self.server-side
               :server-hostname self.server-hostname
               :session self.session))))

  (defclass (name/a! SSLConnector) [(name/a! SSLHandshaker)]
    (defn __init__ [self #** kwargs]
      (.__init__ (super) :server-side False #** kwargs)))

  (defclass (name/a! SSLAcceptor) [(name/a! SSLHandshaker)]
    (defn __init__ [self #** kwargs]
      (.__init__ (super) :server-side True #** kwargs))))

(export
  :objects [SyncSSLStream AsyncSSLStream SyncSSLHandshaker AsyncSSLHandshaker
            SyncSSLConnector AsyncSSLConnector SyncSSLAcceptor AsyncSSLAcceptor])
