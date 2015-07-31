(ns orct.nv-xml
  (:require [clojure.xml :as xml])
  (:import java.nio.ByteBuffer java.io.FileInputStream))

(comment

  (println
   (xml/parse "/Users/ol/Entwicklung/Peiker/nv-parsing/LTE_NAD_SW_QCN/Masterfile_SW_QCN_ATnT_VoLTE.xml"))

  (println
   (xml/parse "/Users/ol/Entwicklung/Peiker/nv-parsing/LTE_NAD_SW_QCN/NvDefinition.xml"))

  (def band-pref-schema
    (array-map
     :name :string
     :band1 :int32
     :band2 :int16
     ))

  (def band-pref1
    (hash-map
     :name "lte-band1"
     :band1 132183
     :band2 10211
     ))

  (def band-pref1-wrong
    (hash-map
     :name "lte-band1"
     :band1 132183
     ))

  (for [[k v] band-pref-schema] (println (band-pref1 k)))

  )

(defn- long2bytearray
  "converts an integer value (max 64 bit) into byte array.

  Since the function  uses 64 arithmetic internally  it cannot easily
  process value at  the 64 bit limit.  In order to do  a proper range
  check we reduce the allowed  range by one that is [-2^63-1..2^63-2]
  when the  maximum resolution of 64  bit is specified. In  all other
  cases there might be the full  signed 2th complement value range or
  for positive  values the  appropriate binary positive  value range.
  That  the  algebraic  sign  of  the  results  range  is  determined
  implicitly."

  [val bits]
  {:pre [(let [mask-signed (bit-shift-left (bit-not 0) (dec bits))
               mask-unsigned (bit-shift-left (bit-not 0) bits)]
           (and (and (>= bits 8) (<= bits 64) (== 0 (mod bits 8)))
                (not (and (== 64 bits) ; 64 bit range is checked explicitly with decreased limits!
                          (or (>= val 9223372036854775807) (<= val -9223372036854775808))))
                (not (or
                      (and (> val 0) (> mask-unsigned 0) (not= 0 (bit-and mask-unsigned val)))
                      (and (< val 0) (< mask-signed 0) (not= mask-signed (bit-and mask-signed val)))))))]
   }
  (loop [bits (- bits 8)
         result []]
    (if (< bits 0)
      result
      (let [mask (bit-shift-left 0xff bits)]
        (recur (- bits 8) (conj result (bit-shift-right (bit-and mask val) bits)))))))

(defn marshal-item-value
  [f v]
  (case f
    :string v
    :int8 (long2bytearray v 8)
    :uint8 (long2bytearray v 8)
    :int16 (long2bytearray v 16)
    :uint16 (long2bytearray v 16)
    :int32 (long2bytearray v 32)
    :uint32 (long2bytearray v 32)
    ))

(defn marshal-items [schema values]
  (for [[k f] schema]
    (let [v (values k)]
      (when (not v)
        (throw (IllegalStateException. (str "no definition for key " k " error!"))))
      ;(println k f v)
      (marshal-item-value f v)
      )))


(comment
  (try
    (doall (marshal-items band-pref-schema band-pref1))
    (catch Exception e (str "caught exception: " (.getMessage e))))

  (try
    (doall (marshal-items band-pref-schema band-pref1-wrong))
    (catch Exception e (str "caught exception: " (.getMessage e))))

  (long2bytearray 0xa0b1 32)
  (long2bytearray 0xa0b1 16)
  (long2bytearray 0 16)
  (long2bytearray 32767 16)
  (long2bytearray 32768 16)
  (long2bytearray 65535 16)
  (long2bytearray 65536 16)
  (long2bytearray -32768 16)
  (long2bytearray -32769 16)
  (long2bytearray -32769 32)

  (long2bytearray (bit-shift-left 1 30) 64)
  (long2bytearray (bit-shift-left 1 63) 64)
  (long2bytearray 9223372036854775807 64)
  (long2bytearray 9223372036854775806 64)
  (long2bytearray -9223372036854775807 64)
  (long2bytearray 1 56)
  (long2bytearray 1 64)

  (try
    (long2bytearray -32768 16)
    (catch AssertionError e (str "caught exception: " (.getMessage e))))

  (try
    (long2bytearray -32769 16)
    (catch AssertionError e (str "caught exception: " (.getMessage e))))


  )
