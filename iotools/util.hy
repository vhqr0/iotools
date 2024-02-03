(require
  dash *)

(import
  dash *
  time
  threading
  queue
  io
  socket
  asyncio
  iotools.stream *)

(do/a!
  (setv (name/a! sleep)            (. (if/a! asyncio time)      sleep)
        (name/a! Lock)             (. (if/a! asyncio threading) Lock)
        (name/a! Event)            (. (if/a! asyncio threading) Event)
        (name/a! Condition)        (. (if/a! asyncio threading) Condition)
        (name/a! Semaphore)        (. (if/a! asyncio threading) Semaphore)
        (name/a! BoundedSemaphore) (. (if/a! asyncio threading) BoundedSemaphore)
        (name/a! Barrier)          (. (if/a! asyncio threading) Barrier)
        (name/a! Queue)            (. (if/a! asyncio queue)     Queue)
        (name/a! PriorityQueue)    (. (if/a! asyncio queue)     PriorityQueue)
        (name/a! LifoQueue)        (. (if/a! asyncio queue)     LifoQueue)
        (name/a! QueueEmpty)       (. (if/a! asyncio queue)     (if/a! QueueEmpty Empty))
        (name/a! QueueFull)        (. (if/a! asyncio queue)     (if/a! QueueFull Full))))

(defclass IOStream [SyncStream]
  (defn __init__ [self file #** kwargs]
    (.__init__ (super) #** kwargs)
    (setv self.file file))

  (defn real-read [self]
    (.read self.file 4096))

  (defn real-write [self b]
    (ignore (.write self.file b)))

  (defn real-close [self]
    (ignore (.close self.file))))

(defclass FileStream [IOStream]
  (defn [classmethod] open [cls path [mode "rb"]]
    (cls :file (open path mode))))

(defclass BytesStream [IOStream]
  (defn [classmethod] open [cls [initial-bytes b""]]
    (cls :file (io.BytesIO initial-bytes)))

  (defn getvalue [self]
    (.getvalue self.file)))

(defclass SocketStream [SyncStream]
  (defn __init__ [self sock #** kwargs]
    (.__init__ (super) #** kwargs)
    (setv self.sock sock))

  (defn [classmethod] open [cls host port]
    (cls :sock (socket.create-connection #(host port))))

  (defn real-read [self]
    (.recv self.sock 4096))

  (defn real-write [self b]
    (ignore (.sendall self.sock b)))

  (defn real-close [self]
    (ignore (.close self.sock))))

(defclass AIOStream [AsyncStream]
  (defn __init__ [self reader writer #** kwargs]
    (.__init__ (super) #** kwargs)
    (setv #(self.reader self.writer) #(reader writer)))

  (defn/a real-read [self]
    (await (.read self.reader 4096)))

  (defn/a real-write [self b]
    (ignore
      (.write self.writer b)
      (await (.drain self.writer))))

  (defn/a real-close [self]
    (ignore
      (.close self.writer)
      (await (.wait-closed self.writer)))))

(defclass SyncTCPStream [SocketStream]
  (defn [classmethod] open [cls host port]
    (cls :sock (socket.create-connection #(host port)))))

(defclass AsyncTCPStream [AIOStream]
  (defn/a [classmethod] open [cls host port]
    (let [#(reader writer) (await (asyncio.open-connection host port))]
      (cls :reader reader :writer writer))))

(export
  :objects [sync-sleep async-sleep SyncLock AsyncLock SyncEvent AsyncEvent SyncCondition AsyncCondition
            SyncSemaphore AsyncSemaphore SyncBoundedSemaphore AsyncBoundedSemaphore SyncBarrier AsyncBarrier
            SyncQueue AsyncQueue SyncPriorityQueue AsyncPriorityQueue SyncLifoQueue AsyncLifoQueue
            SyncQueueEmpty AsyncQueueEmpty SyncQueueFull AsyncQueueFull
            IOStream FileStream BytesStream SocketStream AIOStream SyncTCPStream AsyncTCPStream])
