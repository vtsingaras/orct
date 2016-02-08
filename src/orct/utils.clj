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
  (:import java.util.Properties))


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
  (let [byte-pos-pairs (partition 2 (interleave bytes (range (count bytes)) ))]
    (reduce (fn [result [next-byte pos]]
              (+ result (bit-shift-left (bit-and next-byte 0xff) (* 8 pos))))
            0 byte-pos-pairs)))

(defn bytes2little-endian-uint
  "interprets given byte sequence e.g. specified as Java array
   into corresponding little endian unsigned integer representation.
   example: (bytes2little-endian-uint [0x03 0x01]) -> 259"
  [bytes]
  {:pre [(<= (count bytes) 8)]
   :post [(>= % 0)]}
  (bytes2little-endian bytes))

(defn bytes2little-endian-int
  "interprets given byte sequence e.g. specified as Java array
   into corresponding little endian signed integer representation.
   example: (bytes2little-endian-uint [0x03 0x01]) -> 259"
  [bytes]
  (let [sign (bit-and 0x80 (last bytes))
        ures (bytes2little-endian bytes)]
    (if (= 0 sign)
      ures
      (- ures (bit-shift-left 1 (* 8 (count bytes)))))))


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
