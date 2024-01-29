(require
  dash *)

(import
  dash *
  time
  io
  socket
  asyncio
  iotools.stream *)

(do/a!
  (defn/a! (name/a! sleep) [seconds]
    (wait/a! ((if/a! asyncio.sleep time.sleep) seconds))))

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
      (.close self)
      (await (.wait-closed self)))))

(defclass SyncTCPStream [SocketStream]
  (defn [classmethod] open [cls host port]
    (cls :sock (socket.create-connection #(host port)))))

(defclass AsyncTCPStream [AIOStream]
  (defn/a [classmethod] open [cls host port]
    (let [#(reader writer) (await (asyncio.open-connection host port))]
      (cls :reader reader :writer writer))))

(export
  :objects [sync-sleep async-sleep
            IOStream FileStream BytesStream
            SocketStream AIOStream SyncTCPStream AsyncTCPStream])
