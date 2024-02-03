(require
  dash *)

(import
  dash *
  iotools.buffer *)

(defclass StreamEOFError [RuntimeError])

(do/a!
  (defclass (name/a! Stream) []
    (defn __init__ [self
                    [peek-threshold 8192]
                    [strip-threshold 81920]
                    [write-threshold 4096]]
      (setv self.read-buf (Buffer)
            self.write-buf (Buffer)
            self.closed False
            self.peek-threshold peek-threshold
            self.strip-threshold strip-threshold
            self.write-threshold write-threshold))

    (defn/a! real-read [self]
      (raise NotImplementedError))

    (defn/a! real-write [self]
      (raise NotImplementedError))

    (defn/a! real-close [self])

    (defn (if/a! __aiter__ __iter__) [self]
      self)

    (defn/a! (if/a! __anext__ __next__) [self]
      (if-let [b (wait/a! (.read self))] b (raise (if/a! StopAsyncIteration StopIteration))))

    (defn/a! (if/a! __aenter__ __enter__) [self]
      self)

    (defn/a! (if/a! __aexit__ __exit__) [self #* args #** kwargs]
      (ignore (wait/a! (.close self))))

    (defn/a! peek [self]
      (ignore
        (unless self.read-buf
          (.write self.read-buf (wait/a! (.real-read self))))))

    (defn/a! peek-more [self]
      (ignore
        (if (and (> self.peek-threshold 0)
                 (> (len self.read-buf) self.peek-threshold))
            (raise BufferWantWriteError)
            (if-let [b (wait/a! (.real-read self))]
              (.write self.read-buf b)
              (raise StreamEOFError)))))

    (defn strip [self]
      (ignore
        (when (>= self.read-buf.pos self.strip-threshold)
          (.strip self.read-buf))))

    (defn/a! read [self]
      (if self.read-buf
          (.readall self.read-buf)
          (wait/a! (.real-read self))))

    (defn/a! read-loop [self readfn]
      (let [pos self.read-buf.pos]
        (while True
          (try
            (return
              (let [result (readfn self.read-buf)]
                (.strip self)
                result))
            (except [BufferWantReadError]))
          (setv self.read-buf.pos pos)
          (wait/a! (.peek-more self)))))

    (defn/a! read-loop-iter [self readfn]
      (while (do (wait/a! (.peek self)) self.read-buf)
        (yield (wait/a! (.read-loop self readfn)))))

    (defn/a! write [self b]
      (ignore
        (when b
          (if (and (> self.write-threshold 0)
                   (< (+ (len b) (len self.write-buf)) self.write-threshold))
              (.write self.write-buf b)
              (do
                (when self.write-buf
                  (setv b (+ (.readall self.write-buf) b)))
                (wait/a! (.real-write self b)))))))

    (defn/a! flush [self]
      (ignore
        (when self.write-buf
          (wait/a! (.real-write self (.readall self.write-buf))))))

    (defn/a! shutdown [self])

    (defn/a! close [self]
      (ignore
        (unless self.closed
          (setv self.closed True)
          (try
            (wait/a! (.shutdown self))
            (wait/a! (.flush self))
            (except [Exception]))
          (try
            (wait/a! (.real-close self))
            (except [Exception])))))))

(export
  :objects [StreamEOFError SyncStream AsyncStream])
