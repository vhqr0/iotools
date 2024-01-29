(require
  dash *
  iotools *)

(import
  unittest [TestCase]
  dash *
  iotools *)

(defclass TestPack [TestCase]
  (defn test-int-pack [self]
    (.assertEqual self (int-pack 0x1234 2 "big") b"\x12\x34")
    (.assertEqual self (int-pack 0x1234 2 "little") b"\x34\x12")
    (.assertEqual self (int-unpack b"\x12\x34" "big") 0x1234)
    (.assertEqual self (int-unpack b"\x12\x34" "little") 0x3412))

  (defn test-bits-pack [self]
    (.assertEqual self (bits-pack #(12 4 0) #(0b0110 0 0b1001) 2) b"\x60\x09")
    (.assertEqual self (bits-unpack #(12 4 0) #(0b1111 0b11111111 0b1111) b"\x60\x09")
                  #(0b0110 0 0b1001))))

(defclass TestStruct [TestCase]
  (defn test-const [self]
    (let [const1 (hy.eval '(struct-type [const :const 1]))]
      (.assertEqual self (const1.pack 1) b"")
      (.assertEqual self (const1.unpack b"") 1)
      (with [_ (.assertRaises self BufferWantWriteError)]
        (const1.unpack b"\x01"))
      (with [_ (.assertRaises self StructValidationError)]
        (const1.pack b""))))

  (defn test-bytes [self]
    (let [bytes2 (hy.eval '(struct-type [bytes :len 2]))]
      (.assertEqual self (bytes2.pack b"\x01\x02") b"\x01\x02")
      (.assertEqual self (bytes2.unpack b"\x01\x02") b"\x01\x02")
      (with [_ (.assertRaises self BufferWantWriteError)]
        (bytes2.unpack b"\x01\x02\x03"))
      (with [_ (.assertRaises self BufferWantReadError)]
        (bytes2.unpack b"\x01"))
      (with [_ (.assertRaises self StructValidationError)]
        (bytes2.pack b"\x01"))
      (with [_ (.assertRaises self StructValidationError)]
        (bytes2.pack b"\x01\x02\x03"))))

  (defn test-vbytes [self]
    (let [vbytes2 (hy.eval '(struct-type [vbytes
                                          :len 2
                                          :to-vlen (- it 2)
                                          :from-vlen (+ it 2)]))]
      (.assertEqual self (vbytes2.pack b"\x01\x02") b"\x00\x04\x01\x02")
      (.assertEqual self (vbytes2.unpack b"\x00\x04\x01\x02") b"\x01\x02")
      (with [_ (.assertRaises self BufferWantWriteError)]
        (vbytes2.unpack b"\x00\x02\x01"))
      (with [_ (.assertRaises self BufferWantReadError)]
        (vbytes2.unpack b"\x00\x04\x01"))))

  (defn test-int [self]
    (let [int2be (hy.eval '(struct-type [int :len 2]))
          int2le (hy.eval '(struct-type [int :len 2 :byteorder "little"]))]
      (.assertEqual self (int2be.pack 0x1234) b"\x12\x34")
      (.assertEqual self (int2le.pack 0x1234) b"\x34\x12")
      (.assertEqual self (int2be.unpack b"\x12\x34") 0x1234)
      (.assertEqual self (int2le.unpack b"\x12\x34") 0x3412)
      (with [_ (.assertRaises self BufferWantWriteError)]
        (int2be.unpack b"\x12\x34\x56"))
      (with [_ (.assertRaises self BufferWantReadError)]
        (int2be.unpack b"\x12"))))

  (defn test-bits [self]
    (let [bits484 (hy.eval '(struct-type [bits :lens [4 8 4]]))]
      (.assertEqual self (bits484.pack #(0b0110 0 0b1001)) b"\x60\x09")
      (.assertEqual self (bits484.unpack b"\x60\x09") #(0b0110 0 0b1001))
      (with [_ (.assertRaises self BufferWantWriteError)]
        (bits484.unpack b"\x60\x09\x00"))
      (with [_ (.assertRaises self BufferWantReadError)]
        (bits484.unpack b"\x60"))))

  (defn test-line [self]
    (let [linecrlf (hy.eval '(struct-type [line :sep b"\r\n"]))]
      (.assertEqual self (linecrlf.pack b"hello") b"hello\r\n")
      (.assertEqual self (linecrlf.unpack b"hello\r\n") b"hello")
      (with [_ (.assertRaises self BufferWantWriteError)]
        (linecrlf.unpack b"hello\r\nworld\r\n"))
      (with [_ (.assertRaises self BufferWantReadError)]
        (linecrlf.unpack b"hello"))))

  (defn test-struct [self]
    ;; slow: :struct meta have better to spec a pre-defined struct,
    ;; struct-type create new struct every time when call
    (let [linecrlf (hy.eval '(struct-type [struct :struct (struct-type [line :sep b"\r\n"])]))]
      (.assertEqual self (linecrlf.pack b"hello") b"hello\r\n")
      (.assertEqual self (linecrlf.unpack b"hello\r\n") b"hello")
      (with [_ (.assertRaises self BufferWantWriteError)]
        (linecrlf.unpack b"hello\r\nworld\r\n"))
      (with [_ (.assertRaises self BufferWantReadError)]
        (linecrlf.unpack b"hello"))))

  (defn test-list [self]
    (let [3int2 (hy.eval '(struct-type [list :len 3 :spec [int :len 2]]))]
      (.assertEqual self (3int2.pack #(1 2 3)) b"\x00\x01\x00\x02\x00\x03")
      (.assertEqual self (3int2.unpack b"\x00\x01\x00\x02\x00\x03") [1 2 3])
      (with [_ (.assertRaises self BufferWantWriteError)]
        (3int2.unpack b"\x00\x01\x00\x02\x00\x03\x00"))
      (with [_ (.assertRaises self BufferWantReadError)]
        (3int2.unpack b"\x00\x01\x00\x02\x00"))
      (with [_ (.assertRaises self StructValidationError)]
        (3int2.pack #(1 2)))))

  (defn test-vlist [self]
    (let [vint2 (hy.eval '(struct-type [vlist :len 1 :spec [int :len 2]]))]
      (.assertEqual self (vint2.pack #(1 2)) b"\x02\x00\x01\x00\x02")
      (.assertEqual self (vint2.unpack b"\x02\x00\x01\x00\x02") [1 2])
      (with [_ (.assertRaises self BufferWantWriteError)]
        (vint2.unpack b"\x01\x00\x01\x00\x02"))
      (with [_ (.assertRaises self BufferWantReadError)]
        (vint2.unpack b"\x02\x00\x01"))))

  (defn test-while [self]
    (let [lines (hy.eval '(struct-type [until :pred (= it b"") :spec [line :sep b"\r\n"]]))]
      (.assertEqual self (lines.pack #(b"hello" b"world" b"")) b"hello\r\nworld\r\n\r\n")
      (.assertEqual self (lines.unpack b"hello\r\nworld\r\n\r\n") [b"hello" b"world" b""])
      (with [_ (.assertRaises self BufferWantWriteError)]
        (lines.unpack b"hello\r\n\r\nworld\r\n\r\n"))
      (with [_ (.assertRaises self BufferWantReadError)]
        (lines.unpack b"hello\r\nworld"))
      (with [_ (.assertRaises self BufferWantReadError)]
        (lines.unpack b"hello\r\nworld\r\n")))
    (let [lines (hy.eval '(struct-type [while :pred it :spec [line :sep b"\r\n"]]))]
      (.assertEqual self (lines.pack #(b"hello" b"world" b"")) b"hello\r\nworld\r\n\r\n")
      (.assertEqual self (lines.unpack b"hello\r\nworld\r\n\r\n") [b"hello" b"world" b""])
      (with [_ (.assertRaises self BufferWantWriteError)]
        (lines.unpack b"hello\r\n\r\nworld\r\n\r\n"))
      (with [_ (.assertRaises self BufferWantReadError)]
        (lines.unpack b"hello\r\nworld"))
      (with [_ (.assertRaises self BufferWantReadError)]
        (lines.unpack b"hello\r\nworld\r\n"))))

  (defn test-if [self]
    (let [tlv (hy.eval '(struct-type [[t [int :len 1]]
                                      [v [if :pred (= t 0)
                                          :then [const :const b""]
                                          :else [vbytes :len 1]]]]))]
      (.assertEqual self (tlv.pack #(0 b"")) b"\x00")
      (.assertEqual self (tlv.pack #(1 b"hello")) b"\x01\x05hello")
      (.assertEqual self (tlv.unpack b"\x00") #(0 b""))
      (.assertEqual self (tlv.unpack b"\x01\x05hello") #(1 b"hello"))
      (with [_ (.assertRaises self BufferWantWriteError)]
        (tlv.unpack b"\x00\x05hello"))
      (with [_ (.assertRaises self BufferWantWriteError)]
        (tlv.unpack b"\x01\x05hello\x00"))
      (with [_ (.assertRaises self BufferWantReadError)]
        (tlv.unpack b"\x01"))
      (with [_ (.assertRaises self BufferWantReadError)]
        (tlv.unpack b"\x01\x06hello"))
      (with [_ (.assertRaises self StructValidationError)]
        (tlv.pack #(0 b"hello")))))

  (defn test-cond [self]
    (let [tlv (hy.eval '(struct-type [[t [int :len 1]]
                                      [v [cond :cond [(= t 0) [const :const b""]
                                                      (= t 1) [vbytes :len 1]
                                                      (= t 2) [line :sep b"\r\n"]]]]]))]
      (.assertEqual self (tlv.pack #(0 b"")) b"\x00")
      (.assertEqual self (tlv.pack #(1 b"hello")) b"\x01\x05hello")
      (.assertEqual self (tlv.pack #(2 b"hello")) b"\x02hello\r\n")
      (.assertEqual self (tlv.unpack b"\x00") #(0 b""))
      (.assertEqual self (tlv.unpack b"\x01\x05hello") #(1 b"hello"))
      (.assertEqual self (tlv.unpack b"\x02hello\r\n") #(2 b"hello"))
      (with [_ (.assertRaises self BufferWantWriteError)]
        (tlv.unpack b"\x00\x05hello"))
      (with [_ (.assertRaises self BufferWantWriteError)]
        (tlv.unpack b"\x01\x05hello\x00"))
      (with [_ (.assertRaises self BufferWantWriteError)]
        (tlv.unpack b"\x02\x05hello\r\nworld\r\n"))
      (with [_ (.assertRaises self BufferWantReadError)]
        (tlv.unpack b"\x01"))
      (with [_ (.assertRaises self BufferWantReadError)]
        (tlv.unpack b"\x01\x06hello"))
      (with [_ (.assertRaises self BufferWantReadError)]
        (tlv.unpack b"\x02hello"))
      (with [_ (.assertRaises self StructValidationError)]
        (tlv.pack #(0 b"hello")))
      (with [_ (.assertRaises self StructValidationError)]
        (tlv.pack #(3 b"hello")))
      (with [_ (.assertRaises self StructValidationError)]
        (tlv.unpack b"\x03hello\r\n")))))

(export
  :objects [TestPack TestStruct])
