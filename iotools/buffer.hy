(require
  dash *)

(import
  dash *
  dash.strtools :as s
  collections.abc [Sequence])

(defclass BufferWantReadError [RuntimeError])
(defclass BufferWantWriteError [RuntimeError])

(defclass Buffer [Sequence]
  (setv __slots__ #("data" "pos"))

  (defn __init__ [self #* args #** kwargs]
    (setv #(self.data self.pos) #((bytearray #* args #** kwargs) 0)))

  (defn [property] view [self]
    (cut (memoryview self.data) self.pos None))

  (defn __eq__ [self b]
    (= self.view b))

  (defn __bool__ [self]
    (bool self.view))

  (defn __bytes__ [self]
    (bytes self.view))

  (defn __iter__ [self]
    (iter self.view))

  (defn __reversed__ [self]
    (reversed self.view))

  (defn __len__ [self]
    (len self.view))

  (defn __contains__ [self v]
    (in v self.view))

  (defn __getitem__ [self k]
    (get self.view k))

  (defn __setitem__ [self k v]
    (setv (get self.view k) v))

  (defn __iadd__ [self b]
    (+= self.data b)
    self)

  (defn __str__ [self]
    (s.format "Buffer({})" (bytes self)))

  (defn __repr__ [self]
    (str self))

  (defn clear [self]
    (.clear self.data)
    (setv self.pos 0))

  (defn pop [self]
    (if self (.pop self.data) (raise IndexError)))

  (defn append [self v]
    (.append self.data v))

  (defn extend [self iterable]
    (.extend self.data iterable))

  (defn popleft [self]
    (let [view self.view]
      (if view
          (do
            (+= self.pos 1)
            (get view 0))
          (raise IndexError))))

  (defn appendleft [self v]
    (ignore
      (if self.pos
          (do
            (-= self.pos 1)
            (-setitem self.data self.pos v))
          (let [view self.view]
            (setv self.data (doto (bytearray) (.append v) (.extend view))
                  self.pos 0)))))

  (defn extendleft [self iterable]
    (ignore
      ;; TODO: check buffer protocol instead
      (when (isinstance iterable #(bytes bytearray memoryview))
        (let [nbytes (len iterable)]
          (when (<= nbytes self.pos)
            (let [from-view (memoryview iterable)
                  to-view (cut (memoryview self.data) (- self.pos nbytes) self.pos)]
              (setv (cut to-view) (cut from-view))
              (-= self.pos nbytes)
              (return)))
          (let [view self.view]
            (setv self.data (doto (bytearray iterable) (.extend view))
                  self.pos 0))))))

  (defn strip [self]
    (ignore
      (let [view self.view]
        (setv #(self.data self.pos) #((bytearray view) 0)))))

  (defn readall [self [clear True]]
    (let [b (bytes self)]
      (if clear
          (.clear self)
          (setv self.pos (len self.data)))
      b))

  (defn readinto [self view]
    (let [from-view self.view
          to-view (if (memoryview? view) view (memoryview view))
          nbytes (min (len from-view) (len to-view))]
      (setv (cut to-view nbytes) (cut from-view nbytes))
      (+= self.pos nbytes)
      nbytes))

  (defn read [self [n -1]]
    (let [view self.view
          nbytes (if (< n 0) (len view) (min n (len view)))]
      (+= self.pos nbytes)
      (bytes (cut view nbytes))))

  (defn readexactly [self n]
    (assert (>= n 0))
    (let [view self.view]
      (if (< (len view) n)
          (raise BufferWantReadError)
          (do
            (+= self.pos n)
            (bytes (cut view n))))))

  (defn readuntil [self [sep b"\n"] [strip False]]
    (let [seplen (len sep)
          view self.view
          index (s.find (bytes view) sep)]
      (if (< index 0)
          (raise BufferWantReadError)
          (let [nbytes (+ index seplen)]
            (+= self.pos nbytes)
            (bytes (cut view (if strip index nbytes)))))))

  (defn write [self b]
    (.extend self b)))

(export
  :objects [Buffer BufferWantReadError BufferWantWriteError])
