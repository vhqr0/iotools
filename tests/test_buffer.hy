(require
  dash *)

(import
  unittest [TestCase]
  dash *
  iotools *)

(defclass TestBuffer [TestCase]
  (defn test-readwrite [self]
    (let [buf (Buffer b"hello, world")]
      (.assertEqual self (bytes buf) b"hello, world")
      ;; read 2
      (.assertEqual self (.read buf 2) b"he")
      (.assertEqual self (bytes buf) b"llo, world")
      ;; readinto
      (let [ba (bytearray 2)]
        (.assertEqual self (.readinto buf ba) 2)
        (.assertEqual self (bytes ba) b"ll")
        (.assertEqual self (bytes buf) b"o, world"))
      (.assertEqual self (bytes buf.data) b"hello, world")
      (.assertEqual self buf.pos 4)
      ;; strip
      (.strip buf)
      (.assertEqual self (bytes buf) b"o, world")
      (.assertEqual self (bytes buf.data) b"o, world")
      (.assertEqual self buf.pos 0)
      ;; read
      (.assertEqual self (.read buf) b"o, world")
      (.assertEqual self (bytes buf) b"")
      (.assertEqual self (bytes buf.data) b"o, world")
      (.assertEqual self buf.pos 8)
      ;; write
      (.write buf b".")
      (.assertEqual self (bytes buf) b".")
      ;; readall
      (.assertEqual self (.readall buf) b".")
      (.assertEqual self (bytes buf) b"")
      (.assertEqual self (bytes buf.data) b"")
      (.assertEqual self buf.pos 0)))

  (defn test-except [self]
    (let [buf (Buffer b"\x00\x01\x02\x03\r\n\x04\x05\r\n")]
      (.assertEqual self (.readexactly buf 2) b"\x00\x01")
      (.assertEqual self (bytes buf) b"\x02\x03\r\n\x04\x05\r\n")
      (.assertEqual self (.readuntil buf b"\r\n") b"\x02\x03\r\n")
      (.assertEqual self (bytes buf) b"\x04\x05\r\n")
      (.assertEqual self (.readuntil buf b"\r\n" :strip True) b"\x04\x05"))
    (with [_ (.assertRaises self BufferWantReadError)]
      (.readexactly (Buffer b"\x00") 2))
    (with [_ (.assertRaises self BufferWantReadError)]
      (.readuntil (Buffer b"\x00\n") b"\r\n"))))

(export
  :objects [TestBuffer])
