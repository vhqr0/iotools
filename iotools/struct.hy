(require
  dash *)

(import
  dash *
  dash.operator :as o
  functools [cached-property]
  iotools.buffer *)

(defn int-pack [i length [byteorder "big"] [signed False]]
  (int.to-bytes i length :byteorder byteorder :signed signed))

(defn int-unpack [b [byteorder "big"] [signed False]]
  (int.from-bytes b :byteorder byteorder :signed signed))

(defn bits-pack [offsets bits length [byteorder "big"]]
  (-> (-map-unzipped o.bit-lshift (-zip bits offsets))
      sum
      (int-pack length :byteorder byteorder)))

(defn bits-unpack [offsets masks b [byteorder "big"]]
  (let [i (int-unpack b :byteorder byteorder)]
    (->>
      (-zip masks offsets)
      (-map-unzipped (fn [mask offset] (& (>> i offset) mask)))
      tuple)))

(defn struct-pack [writefn it]
  (let [writer (Buffer)]
    (writefn writer it)
    (bytes writer)))

(defn struct-unpack [readfn b]
  (let [reader (Buffer b)
        it (readfn reader)]
    (if reader (raise BufferWantWriteError) it)))

(defn struct-pack-many [writefn them]
  (let [writer (Buffer)]
    (--each them (writefn writer it))
    (bytes writer)))

(defn struct-unpack-many [readfn b]
  (let [reader (Buffer b)
        them (list)]
    (while reader
      (.append them (readfn reader)))
    them))



(defclass StructValidationError [RuntimeError])

(defclass Struct []
  (defn [staticmethod] read [reader]
    (raise NotImplementedError))

  (defn [staticmethod] write [writer it]
    (raise NotImplementedError))

  (defn [classmethod] unpack [cls b]
    (struct-unpack cls.read b))

  (defn [classmethod] pack [cls it]
    (struct-pack cls.write it))

  (defn [classmethod] unpack-many [cls b]
    (struct-unpack-many cls.read b))

  (defn [classmethod] pack-many [cls them]
    (struct-pack-many cls.write them)))



(defclass StructSpec []
  (defn [classmethod] from-spec [cls spec]
    (cond (not (isinstance spec hy.models.List)) (ClassStructSpec.from-spec spec)
          (symbol? (get spec 0)) (SimpleStructSpec.from-spec spec)
          True (ComplexStructSpec.from-spec spec)))

  (defn [property] read-form [self]
    (raise NotImplementedError))

  (defn [property] write-form [self]
    (raise NotImplementedError)))

(defclass ClassStructSpec [StructSpec]
  ;; spec: class-form
  (defn [classmethod] from-spec [cls spec]
    (cls spec))

  (defn __init__ [self struct]
    (setv self.struct-form struct))

  (defn [property] read-form [self]
    `(.read ~self.struct-form reader))

  (defn [property] write-form [self]
    `(.write ~self.struct-form writer it)))

(defclass ComplexStructSpec [StructSpec]
  ;; spec: [& [destruct-from spec] ...]
  (defn [classmethod] from-spec [cls spec]
    (cls (list (--map (let [#(destruct-form spec) it]
                        (cons destruct-form (StructSpec.from-spec spec)))
                      spec))))

  (defn __init__ [self fields]
    (setv self.fields fields
          self.names (list (-flatten (-map car self.fields)))))

  (defn [property] read-form [self]
    `(do
       ~@(--map `(setv ~(car it) ~(. (cdr it) read-form)) self.fields)
       #(~@self.names)))

  (defn [property] write-form [self]
    `(let [#(~@self.names) it]
       ~@(--map `(let [it ~(car it)] ~(. (cdr it) write-form)) self.fields))))

(defclass SimpleStructSpec [StructSpec]
  (setv type-dict (dict)
        type None)

  (defn __init-subclass__ [cls #* args #** kwargs]
    (when cls.type
      (-setitem cls.type-dict cls.type cls)))

  ;; spec: [type & [k v] ...]
  ;; type: symbol
  ;; [k v]: [keyword form]
  (defn [classmethod] from-spec [cls spec]
    (let [#(type #* clauses) spec]
      ((-getitem cls.type-dict type)
        (-> (dict (-partition-all 2 clauses))
            (--map-keys (hy.mangle it.name))))))

  (defn __init__ [self meta]
    (setv self.meta meta))

  (defn __getattr__ [self name]
    (-get self.meta name))

  #_(comment
      :read '(->> buffer bytes type field)
      :write '(->> field type bytes buffer)
      :read-form '(let [it *read-type-form*] *type-to-field-form*)
      :write-form '(let [it *type-from-field-form*] *write-type-form*) ; bind field to it outside write form
      :meta [:to "alias of type-to-field"
             :from "alias of type-from-field"
             :validate "validate field, equiv to :to-validate ... :from-validate ..."
             :to-validate "validate field after type-to-field"
             :from-validate "validate field after type-from-field"])

  (defn [property] read-type-form [self]
    (raise NotImplementedError))

  (defn [property] write-type-form [self]
    (raise NotImplementedError))

  (defn [property] read-form [self]
    (let [form self.read-type-form]
      (when-let [to-field-form self.to]
        (setv form
              `(let [it ~form]
                 ~to-field-form)))
      (when-let [validate-form (or self.validate self.to-validate)]
        (setv form
              `(let [it ~form]
                 (if ~validate-form
                     it
                     (raise StructValidationError)))))
      form))

  (defn [property] write-form [self]
    (let [form self.write-type-form]
      (when-let [from-field-form self.from]
        (setv form
              `(let [it ~from-field-form]
                 ~form)))
      (when-let [validate-form (or self.validate self.from-validate)]
        (setv form
              `(if ~validate-form
                   ~form
                   (raise StructValidationError))))
      form)))



(defclass ConstMixin []
  ;; meta: :const
  (defn [cached-property] const-form [self]
    (-getitem self.meta "const")))

(defclass LenMixin []
  ;; meta: :len
  (defn [cached-property] len-form [self]
    (-getitem self.meta "len")))

(defclass ByteOrderMixin []
  ;; meta: :byteorder
  (defn [cached-property] byteorder-form [self]
    (-get self.meta "byteorder" "big")))

(defclass SignedMixin []
  ;; meta: :signed
  (defn [cached-property] signed-form [self]
    (-get self.meta "signed" False)))

(defclass VLenMixin [LenMixin ByteOrderMixin]
  ;; meta: :len :to-vlen :from-vlen

  (defn [cached-property] read-vlen-form [self]
    (let [form `(-> (.readexactly reader ~self.len)
                    (int-unpack :byteorder ~self.byteorder-form))]
      (when-let [to-vlen-form self.to-vlen]
        (setv form
              `(let [it ~form]
                 ~to-vlen-form)))
      form))

  (defn [cached-property] write-vlen-form [self]
    (let [form `(->> (int-pack it ~self.len-form :byteorder ~self.byteorder-form)
                     (.write writer))]
      (when-let [from-vlen-form self.from-vlen]
        (setv form
              `(let [it ~from-vlen-form]
                 ~form)))
      form)))

(defclass BytesMixin []
  ;; meta: :struct :many

  (defn [property] read-bytes-form [self]
    (raise NotImplementedError))

  (defn [property] write-bytes-form [self]
    (raise NotImplementedError))

  (defn [property] read-type-form [self]
    (let [it self.read-bytes-form]
      (when self.struct
        (setv it `(let [it ~it]
                    (try
                      ((. ~self.struct ~(if self.many 'unpack-many 'unpack)) it)
                      (except [e BufferWantReadError]
                        (raise StructValidationError :from e))))))
      it))

  (defn [property] write-type-form [self]
    (let [it self.write-bytes-form]
      (when self.struct
        (setv it `(let [it ((. ~self.struct ~(if self.many 'pack-many 'pack)) it)] ~it)))
      it)))

(defclass SepMixin []
  ;; meta: :sep
  (defn [cached-property] sep-form [self]
    (-getitem self.meta "sep")))

(defclass SpecMixin []
  ;; meta: :spec
  (defn [cached-property] spec [self]
    (StructSpec.from-spec (-getitem self.meta "spec")))

  (defn [cached-property] spec-read-form [self]
    self.spec.read-form)

  (defn [cached-property] spec-write-form [self]
    self.spec.write-form))

(defclass PredMixin []
  ;; meta: :pred
  (defn [cached-property] pred-form [self]
    (-getitem self.meta "pred")))

(defclass ThenElseMixin []
  ;; meta: :then :else

  (defn [cached-property] then-spec [self]
    (StructSpec.from-spec (-getitem self.meta "then")))

  (defn [cached-property] else-spec [self]
    (StructSpec.from-spec (-getitem self.meta "else")))

  (defn [cached-property] then-read-form [self]
    self.then-spec.read-form)

  (defn [cached-property] then-write-form [self]
    self.then-spec.write-form)

  (defn [cached-property] else-read-form [self]
    self.else-spec.read-form)

  (defn [cached-property] else-write-form [self]
    self.else-spec.write-form))

(defclass CondMixin []
  ;; meta: :cond

  (defn [cached-property] cond-specs [self]
    (->> (-getitem self.meta "cond")
         (-partition-all 2)
         (-map-unzipped (fn [pred spec] (cons pred (StructSpec.from-spec spec))))
         list))

  (defn [cached-property] cond-read-form [self]
    `(cond
       ~@(--mapcat
           #((car it) (. (cdr it) read-form))
           self.cond-specs)
       True (raise StructValidationError)))

  (defn [cached-property] cond-write-form [self]
    `(cond
       ~@(--mapcat
           #((car it) (. (cdr it) write-form))
           self.cond-specs)
       True (raise StructValidationError))))



(defclass ConstStructSpec [ConstMixin SimpleStructSpec]
  (setv type 'const)

  (defn [property] read-type-form [self]
    self.const-form)

  (defn [property] write-type-form [self]
    `(unless (= it ~self.const-form)
       (raise StructValidationError))))

(defclass AllStructSpec [BytesMixin SimpleStructSpec]
  (setv type 'all)

  (defn [property] read-bytes-form [self]
    '(.readall reader :clear False))

  (defn [property] write-bytes-form [self]
    '(.write writer it)))

(defclass BytesStructSpec [LenMixin BytesMixin SimpleStructSpec]
  (setv type 'bytes)

  (defn [property] read-bytes-form [self]
    `(.readexactly reader ~self.len-form))

  (defn [property] write-bytes-form [self]
    `(if (= (len it) ~self.len-form)
         (.write writer it)
         (raise StructValidationError))))

(defclass VBytesStructSpec [VLenMixin BytesMixin SimpleStructSpec]
  (setv type 'vbytes)

  (defn [property] read-bytes-form [self]
    `(.readexactly reader ~self.read-vlen-form))

  (defn [property] write-bytes-form [self]
    `(do
       (let [it (len it)]
         ~self.write-vlen-form)
       (.write writer it))))

(defclass IntStructSpec [LenMixin ByteOrderMixin SignedMixin SimpleStructSpec]
  (setv type 'int)

  (defn [property] read-type-form [self]
    `(-> (.readexactly reader ~self.len-form)
         (int-unpack
           :byteorder ~self.byteorder-form
           :signed ~self.signed-form)))

  (defn [property] write-type-form [self]
    `(->>
       (int-pack it ~self.len-form
                 :byteorder ~self.byteorder-form
                 :signed ~self.signed-form)
       (.write writer))))

(defclass BitsStructSpec [ByteOrderMixin SimpleStructSpec]
  (setv type 'bits)

  (defn __init__ [self #* args #** kwargs]
    (.__init__ (super) #* args #** kwargs)
    (let [lens (list (-map int self.lens))
          nbits (sum lens)
          #(nbytes mod) (divmod nbits 8)]
      (assert (= mod 0))
      (setv self.len-form nbytes
            self.offsets-form (->> (reversed lens)
                                   (-reductions-from o.add 0)
                                   -butlast
                                   list
                                   reversed
                                   (hy.models.Tuple))
            self.masks-form (->> lens
                                 (--map (dec (<< 1 it)))
                                 (hy.models.Tuple)))))

  (defn [property] read-type-form [self]
    `(bits-unpack ~self.offsets-form ~self.masks-form
                  (.readexactly reader ~self.len-form)
                  :byteorder ~self.byteorder-form))

  (defn [property] write-type-form [self]
    `(->>
       (bits-pack ~self.offsets-form it ~self.len-form
                  :byteorder ~self.byteorder-form)
       (.write writer))))

(defclass LineStructSpec [SepMixin SimpleStructSpec]
  (setv type 'line)

  (defn [property] read-type-form [self]
    `(.readuntil reader :sep ~self.sep-form :strip True))

  (defn [property] write-type-form [self]
    `(do
       (.write writer it)
       (.write writer ~self.sep-form))))

(defclass StructStructSpec [SpecMixin SimpleStructSpec]
  (setv type 'struct)

  (defn [property] read-type-form [self]
    self.spec-read-form)

  (defn [property] write-type-form [self]
    self.spec-write-form))

(defclass ListStructSpec [SpecMixin LenMixin SimpleStructSpec]
  (setv type 'list)

  (defn [property] read-type-form [self]
    `(let [them (list)]
       (--dotimes ~self.len-form (.append them ~self.spec-read-form))
       them))

  (defn [property] write-type-form [self]
    `(let [them it]
       (if (= (len them) ~self.len-form)
           (--each them ~self.spec-write-form)
           (raise StructValidationError)))))

(defclass VListStructSpec [SpecMixin VLenMixin SimpleStructSpec]
  (setv type 'vlist)

  (defn [property] read-type-form [self]
    `(let [them (list)]
       (--dotimes ~self.read-vlen-form (.append them ~self.spec-read-form))
       them))

  (defn [property] write-type-form [self]
    `(let [them it]
       (let [it (len them)]
         ~self.write-vlen-form)
       (--each them ~self.spec-write-form))))

(defclass UntilStructSpec [SpecMixin PredMixin SimpleStructSpec]
  (setv type 'until)

  (defn [property] read-type-form [self]
    `(let [them (list)]
       (while True
         (let [it ~self.spec-read-form]
           (.append them it)
           (when ~self.pred-form
             (break))))
       them))

  (defn [property] write-type-form [self]
    `(let [them it]
       (--each them ~self.spec-write-form))))

(defclass WhileStructSpec [SpecMixin PredMixin SimpleStructSpec]
  (setv type 'while)

  (defn [property] read-type-form [self]
    `(let [them (list)]
       (while True
         (let [it ~self.spec-read-form]
           (.append them it)
           (unless ~self.pred-form
             (break))))
       them))

  (defn [property] write-type-form [self]
    `(let [them it]
       (--each them ~self.spec-write-form))))

(defclass IfStructSpec [ThenElseMixin PredMixin SimpleStructSpec]
  (setv type 'if)

  (defn [property] read-type-form [self]
    `(if ~self.pred-form
         ~self.then-read-form
         ~self.else-read-form))

  (defn [property] write-type-form [self]
    `(if ~self.pred-form
         ~self.then-write-form
         ~self.else-write-form)))

(defclass CondStructSpec [CondMixin SimpleStructSpec]
  (setv type 'cond)

  (defn [property] read-type-form [self]
    self.cond-read-form)

  (defn [property] write-type-form [self]
    self.cond-write-form))



(defmacro struct-readfn [spec]
  (let [spec (StructSpec.from-spec spec)]
    `(fn [reader] ~spec.read-form)))

(defmacro struct-writefn [spec]
  (let [spec (StructSpec.from-spec spec)]
    `(fn [writer it] (ignore ~spec.write-form))))

(defmacro struct-unpackfn [spec]
  `(fn [b] (struct-unpack (struct-readfn ~spec) b)))

(defmacro struct-packfn [spec]
  `(fn [it] (struct-pack (struct-writefn ~spec) it)))

(defmacro struct-unpack-many-fn [spec]
  `(fn [b] (struct-unpack-many (struct-readfn ~spec) b)))

(defmacro struct-pack-many-fn [spec]
  `(fn [them] (struct-pack-many (struct-writefn ~spec) them)))

(defmacro struct-type [spec]
  (let [spec (StructSpec.from-spec spec)]
    `(type "<struct>" #(Struct)
           {"read" (staticmethod (fn [reader] ~spec.read-form))
            "write" (staticmethod (fn [writer it] (ignore ~spec.write-form)))})))

(defmacro defstruct [name spec]
  (let [spec (StructSpec.from-spec spec)]
    `(defclass ~name [Struct]
       (defn [staticmethod] read [reader] ~spec.read-form)
       (defn [staticmethod] write [writer it] (ignore ~spec.write-form)))))

(export
  :objects [int-pack int-unpack bits-pack bits-unpack
            struct-pack struct-unpack struct-pack-many struct-unpack-many
            StructValidationError Struct]
  :macros [struct-readfn struct-writefn struct-unpackfn struct-packfn
           struct-unpack-many-fn struct-pack-many-fn struct-type defstruct])
