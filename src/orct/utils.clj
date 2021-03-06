;; Open Radio Calibration Toolkit
;; An enhanced Open Source Implementation to replace Qualcomm's QRCT
;;
;; The use and distribution terms for this software are covered by
;; the GNU General Public License
;;
;; (C) 2015, Otto Linnemann
;;
;; utils.cljs - utility functions, mostly for data conversion

(ns orct.utils
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.java.io :as io])
  (:import [java.util.zip Deflater Inflater]
           [java.io ByteArrayOutputStream]
           java.util.Properties))


(defn get-version
  "reads version data from the pom.properties META-INF file
   refer to:
   https://groups.google.com/d/msg/leiningen/7G24ifiYvOA/h6xmjeWaO3gJ"
  [dep]
  (let [path (str "META-INF/maven/" (or (namespace dep) (name dep))
                  "/" (name dep) "/pom.properties")
        props (io/resource path)]
    (when props
      (with-open [stream (io/input-stream props)]
        (let [props (doto (Properties.) (.load stream))]
          (.getProperty props "version"))))))


(defn str2int
  "simple string to integer converter"
  [s]
  (.intValue (Integer. s)))


(defn bytes2str
  "remove C like '0' termination characters from given string"
  [str]
  (let [s (seq (if (= java.lang.String (class str)) (.getBytes str) str))
        t (loop [s (reverse s)] (if (= (first s) 0) (recur (rest s)) s))]
    (String. (byte-array (reverse t)))))


(defn key2str
  "transforms keyword which are usually used as hash keys to
  stringified name without colon: (key2str :a) -> 'a'."
  [k] (if k (subs (str k) 1) ""))


(defn get-lc-filename-ext
  "returns file extention of given name in lower case"
  [s]
  (let [bn (.getName (java.io.File. s))]
    (when-let [e (last (re-find  #"([^.]+)$" bn))]
      (str/lower-case e))))


(defn tabs
  "generates a string of specified numer of blanks used as tabulating characters"
  [level]
  (apply str (repeat (* 2 level) " ")))


(defn println-err
  "println to stderr"
  [& args]
  (binding [*out* *err*] (apply println args)))


(defn bytes2little-endian
  "interprets given byte sequence e.g. specified as Java array
   into corresponding little endian unsigned integer representation.
   example: (bytes2little-endian-uint [0x03 0x01]) -> 259"
  [bytes]
  (let [bytes (or (seq bytes) [0 0])]
    (let [byte-pos-pairs (partition 2 (interleave bytes (range (count bytes)) ))]
      (reduce (fn [result [next-byte pos]]
                (+ result (bit-shift-left (bit-and next-byte 0xff) (* 8 pos))))
              0 byte-pos-pairs))))

(defn bytes2little-endian-uint
  "interprets given byte sequence e.g. specified as Java array
   into corresponding little endian unsigned integer representation.
   example: (bytes2little-endian-uint [0x03 0x01]) -> 259"
  [bytes]
  {:pre [(<= (count bytes) 8)]}
  (bytes2little-endian bytes))

(defn bytes2little-endian-int
  "interprets given byte sequence e.g. specified as Java array
   into corresponding little endian signed integer representation.
   example: (bytes2little-endian-uint [0x03 0x01]) -> 259"
  [bytes]
  (let [bytes (or (seq bytes) [0 0])]
    (let [sign (bit-and 0x80 (last bytes))
          ures (bytes2little-endian bytes)]
      (if (= 0 sign)
        ures
        (- ures (bit-shift-left 1 (* 8 (count bytes))))))))


(defn rest-uint-n-pair
  "take n bytes from given sequence and delivers vector with
   remaining bytes and unsigned integer representation.
   example (rest-uint-n-pair 2 [0x03 0x01 0xFA 0xFB]) -> [(0xFA 0xFB) 259 ]"
  [n bytes]
  [(drop n bytes) (bytes2little-endian-uint (take n bytes))])

(def rest-uint8-pair (partial rest-uint-n-pair 1))
(def rest-uint16-pair (partial rest-uint-n-pair 2))
(def rest-uint32-pair (partial rest-uint-n-pair 4))
(def rest-uint64-pair (partial rest-uint-n-pair 8))


(defn rest-int-n-pair
  "take n bytes from given sequence and delivers vector with
   remaining bytes and signed integer representation.
   example (rest-uint-n-pair 2 [0x03 0x01 0xFA 0xFB]) -> [(0xFA 0xFB) 259 ]"
  [n bytes]
  [(drop n bytes) (bytes2little-endian-int (take n bytes))])

(def rest-int8-pair (partial rest-int-n-pair 1))
(def rest-int16-pair (partial rest-int-n-pair 2))
(def rest-int32-pair (partial rest-int-n-pair 4))
(def rest-int64-pair (partial rest-int-n-pair 8))


(defn rest-str-pair
  "takes n bytes from given sequence and deliver a vector with
  remaining bytes and character string."
  [n bytes]
  [(drop n bytes) (bytes2str (take n bytes))])



(defn proc-parse-struct-with-rest
  "process a parser format structure defintion with byte stream sequence

   The format structure  definition is a vector of the  hash-maps {key parse-fn}
   where parse-fn  is the actual parsing  predicate function. It takes  the byte
   stream sequence  as input and  generates as result  a vector where  the first
   element is the  remaining unprocessed rest of the byte  stream and the second
   element is the parsed result.

   Example invocation:

   (proc-parse-struct
    [{::e_ident (fn [s] [(drop 16 s) (take 16 s)])}
      {::e_type rest-uint16-pair}
      {::e_machine rest-uint16-pair}
      {::e_shoff rest-uint32-pair}]
    s) -> [{::e_ident xxx ::e_type yyy ...} unprocessed-sequence]"
  [parse-struct s]
  (let [result
        (reduce
         (fn [[elf-map s] elem]
           (let [parse (first (vals elem))
                 [r value] (parse s)]
             [(assoc elf-map (first (keys elem)) value) r]))
         [{} s]
         parse-struct)]
    result))


(defn proc-parse-struct
  "process a parser format structure defintion with byte stream sequence

   The format structure  definition is a vector of the  hash-maps {key parse-fn}
   where parse-fn  is the actual parsing  predicate function. It takes  the byte
   stream sequence  as input and  generates as result  a vector where  the first
   element is the  remaining unprocessed rest of the byte  stream and the second
   element is the parsed result.

   Example invocation:

   (proc-parse-struct
    [{::e_ident (fn [s] [(drop 16 s) (take 16 s)])}
      {::e_type rest-uint16-pair}
      {::e_machine rest-uint16-pair}
      {::e_shoff rest-uint32-pair}]
    s) -> {::e_ident xxx ::e_type yyy ...}"
  [parse-struct s]
  (first (proc-parse-struct-with-rest parse-struct s)))


(defn long2byteseq
  "converts an integer value (max 64 bit) into byte array.

  Since the function  uses 64 arithmetic internally  it cannot easily
  process value at  the 64 bit limit.  In order to do  a proper range
  check we reduce the allowed  range by one that is [-2^63-1..2^63-2]
  when the  maximum resolution of 64  bit is specified. In  all other
  cases there might be the full  signed 2th complement value range or
  for positive  values the  appropriate binary positive  value range.
  That  the  algebraic  sign  of  the  results  range  is  determined
  implicitly."

  [bits val]
  {:pre [(let [mask-signed (bit-shift-left (bit-not 0) (dec bits))
               mask-unsigned (bit-shift-left (bit-not 0) bits)]
           (and (and (>= bits 8) (<= bits 64) (== 0 (mod bits 8)))
                (not (and (== 64 bits) ; 64 bit range is checked explicitly with decreased limits!
                          (or (>= val 9223372036854775807) (<= val -9223372036854775808))))
                (not (or
                      (and (> val 0) (> mask-unsigned 0) (not= 0 (bit-and mask-unsigned val)))
                      (and (< val 0) (< mask-signed 0) (not= mask-signed (bit-and mask-signed val)))))))]}
  (loop [bits (- bits 8)
         result '()]
    (if (< bits 0)
      result
      (let [mask (bit-shift-left 0xff bits)]
        (recur (- bits 8) (conj result (bit-shift-right (bit-and mask val) bits)))))))


(comment

  (long2byteseq 32 0x01020304)

  (long2byteseq 32 0xa0b1)
  (long2byteseq 16 0xa0b1)
  (long2byteseq 16 0)
  (long2byteseq 16 32767)
  (long2byteseq 16 32768)
  (long2byteseq 16 65535)
  (long2byteseq 16 65536)
  (long2byteseq 16 -32768)
  (long2byteseq 16 -32769)
  (long2byteseq 32 -32769)

  (long2byteseq 64 (bit-shift-left 1 30))
  (long2byteseq 64 (bit-shift-left 1 63))
  (long2byteseq 64 9223372036854775807)
  (long2byteseq 64 9223372036854775806)
  (long2byteseq 64 -9223372036854775807)
  (long2byteseq 56 1)
  (long2byteseq 64 1)

  (try
    (long2byteseq 16 -32768)
    (catch AssertionError e (str "caught exception: " (.getMessage e))))

  (try
    (long2byteseq 16 -32769)
    (catch AssertionError e (str "caught exception: " (.getMessage e))))


  )


(defn zlib-compress
  "compresses specified byte array by zlib algorithm"
  [data]
  (let [deflater (Deflater.)
        output-stream (ByteArrayOutputStream. (alength data))
        buffer (byte-array 1024)]
    (. deflater setInput data)
    (. deflater finish)
    (loop []
      (let [count (. deflater deflate buffer)]
        (. output-stream write buffer 0 count)
        (when (not (. deflater finished))
          (recur))))
    (. output-stream close)
    (. output-stream toByteArray)))


(defn zlib-uncompress
  "uncompresses specified byte array by zlib algorithm"
  [data]
  (let [inflater (Inflater.)
        output-stream (ByteArrayOutputStream. (alength data))
        buffer (byte-array 1024)]
    (. inflater setInput data)
    (loop []
      (let [count (. inflater inflate buffer)]
        (. output-stream write buffer 0 count)
        (when (not (. inflater finished))
          (recur))))
    (. output-stream close)
    (. output-stream toByteArray)))



(comment

  (def input (byte-array (map int "Hello, World!,  hello, hello    hello again!")))
  (alength input)


  (def compressed (zlib-compress input))
  (alength compressed)

  (String. (zlib-uncompress compressed))

  )


(defn read-file
  "read binary file with specified path and returns byte buffer
   with its content"
  [file-path]
  (with-open [reader (clojure.java.io/input-stream file-path)]
    (let [length (.length (clojure.java.io/file file-path))
          buffer (byte-array length)]
      (.read reader buffer 0 length)
      buffer)))


(defn write-to-file
  "writes content which is expected to come as java byte buffer
   to specified content"
  [file-path content]
  (with-open [w (java.io.FileOutputStream. file-path)]
    (.write w content)))
