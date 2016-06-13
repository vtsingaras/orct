;; Open Radio Calibration Toolkit
;; An enhanced Open Source Implementation to replace Qualcomm's QRCT
;;
;; The use and distribution terms for this software are covered by
;; the GNU General Public License
;;
;; (C) 2015, Otto Linnemann
;;
;; mbn.cljs - parser for modem configuration binary files (mbn)

(ns orct.mbn
  (:use [orct.utils]
        [orct.elf]
        [clojure.java.io]
        [orct.qcn-parser]
        [orct.nv-xml]
        [orct.qcn-printer])
  (:require [clojure.string :as str]))


(def mcfg-header-len                         16)
(def mcfg-ver-nv-header-len                  4)
(def mcfg-version-len                        4)
(def mcfg-nv-data-offset                     (+ mcfg-header-len mcfg-ver-nv-header-len mcfg-version-len))
(def mcfg-prefix-len                         8)

(def mcfg-int-nv-item                        0x01)
(def mcfg-int-efs-file                       0x02)
(def mcfg-int-sw-only                        0x03)
(def mcfg-int-delete-nv-item                 0x04)
(def mcfg-int-delete-efs-file                0x05)
(def mcfg-int-static-wo-efs-file             0x06)
(def mcfg-int-muxd-nv-item                   0x07)
(def mcfg-int-muxd-sw-only                   0x08)
(def mcfg-int-muxd-efs-file                  0x09)
(def mcfg-int-muxd-sw-only-efs-file          0x0a)
(def mcfg-int-data-profile                   0x0b)
(def mcfg-int-delete-data-profile            0x0c)
(def mcfg-int-static-wo-data-profile         0x0d)
(def mcfg-int-muxed-data-profile             0x0e)
(def mcfg-int-muxd-sw-only-data-profile      0x0f)
(def mcfg-int-config-item                    0x10)


(defn- extract-mbn-elf32-prog-segment
  "function retrieves the mbn item content from an elf32
   byte sequence which e.g. read from a mbn file."
  [s]
  (let [h (parse-elf32-header s)
        progs (parse-elf32-prog-headers h s)
        progs (filter #(= (:orct.elf/p_type %) 1) progs) ; filter LT_LOAD seg types
        prog (first progs)]
    (take (:orct.elf/p_filesz prog) (drop (:orct.elf/p_offset prog) s))))


(defn- extract-mcfg-header
  "extract mcfg header
   returns: [ result-map rest-of-byte-sequence ]"
  [mbn]
  {:post [(= (-> % first ::magic1) 1195787085)
          (<= (-> % first ::mcfg_format_ver_num) 3)]}
  (proc-parse-struct-with-rest
   [{::magic1 rest-uint32-pair}
    {::mcfg_format_ver_num rest-uint16-pair}
    {::mcfg_type rest-uint16-pair}
    {::mcfg_num_items rest-uint32-pair}
    {::mcfg_muxd_carrier_index_info rest-uint16-pair}
    {::spare_crc rest-uint16-pair}]
   mbn))


(defn- extract-mcfg-version
  "extract mcfg version
  returns: [ result-map rest-of-byte-sequence ]"
  [mbn-after-header]
  (proc-parse-struct-with-rest
   [{::mcfg_version_item_type rest-uint16-pair}
    {::mcfg_version-item_length rest-uint16-pair}
    {::mcfg_version rest-uint32-pair}]
   mbn-after-header))



(defn- extract-mcfg-item-prefix
  "extract mcfg item prefix
   returns: [ result-map rest-of-byte-sequence ]"
  [mcg-item-start-seq]
  (proc-parse-struct-with-rest
   [{::item_length rest-uint32-pair}
    {::item_type rest-uint8-pair}
    {::attrib rest-uint8-pair}
    {::sp_ops rest-uint8-pair}
    {::spare1 rest-uint8-pair}]
   mcg-item-start-seq))


(defn- extract-mcfg-nv-item
  "extract legacy nv item content
   returns: [ result-map rest-of-byte-sequence ]"
  [r]
  (let [[header r] (proc-parse-struct-with-rest
                    [{::item_type rest-uint16-pair}
                     {::item_length rest-uint16-pair}]
                    r)
        len (::item_length header)
        [content r] [(take len r) (drop len r)]]
    [{::nv-item {(keyword (str (::item_type header))) content}} r]))


(defn- extract-mcfg-efs-item
  "extract efs item content
   returns: [ result-map rest-of-byte-sequence ]"
  [r]
  (let [[path-header r] (proc-parse-struct-with-rest
                         [{::efs_header_type rest-uint16-pair}
                          {::efs_header_len rest-uint16-pair}] r)
        path-len (::efs_header_len path-header)
        [path-body r] [(take path-len r) (drop path-len r)]

        [content-header r] (proc-parse-struct-with-rest
                         [{::efs_header_type rest-uint16-pair}
                          {::efs_header_len rest-uint16-pair}] r)
        content-len (::efs_header_len content-header)
        [content-body r] [(take content-len r) (drop content-len r)]]
    [{::efs-item {(keyword (bytes2str path-body)) content-body}} r]))


(defn- extract-mcfg-item
  "extract one mcfg item (currently only legacy and efs items are implemented)
   returns: [ result-map rest-of-byte-sequence ]"
  [nv-item-start-seq]
  (let [[mcfg-item-prefix r] (extract-mcfg-item-prefix nv-item-start-seq)
        type (::item_type mcfg-item-prefix)]
    (cond

      (= type mcfg-int-nv-item)
      (extract-mcfg-nv-item r)

      (= type mcfg-int-efs-file)
      (extract-mcfg-efs-item r)

      :else (throw (IllegalStateException. (format "mcfg item -> %s has wrong type %d!"
                                                   (str mcfg-item-prefix) type))))))


(comment
  (def content (extract-mcfg-item (drop mcfg-nv-data-offset mbn)))
  (def p1 (first content))
  (def r2 (second content))
  )



(defn- extract-mcfg-items
  "extract all mcfg items (currently only legacy and efs items are implemented)
   returns: [ mcfg-map rest-of-byte-sequence ]"
  [num-mcfg-items stream]
  (loop [mcfg-item-number 1
         resmap-rest [[] stream]]
    (if (== mcfg-item-number num-mcfg-items)
      (first resmap-rest)
      (recur (inc mcfg-item-number)
             (let [except-fmt-str "Error in transformation of %d-th mcfg-item occurred: %s\n"
                   [item r] (try
                              (extract-mcfg-item (second resmap-rest))
                              (catch Throwable t
                                (throw (IllegalStateException.
                                        (format except-fmt-str mcfg-item-number (.getMessage t))))))]
               [(conj (first resmap-rest) item) r])))))


(comment

  (def item-a {::nv-item {:4 "four"}})
  (def item-b {::nv-item {:5 "five"}})
  (merge-with merge item-a item-b)

  (def a (extract-mcfg-items 10 (drop mcfg-nv-data-offset mbn)))
  (apply merge-with merge a)

  )




(defn- parse-mbn-file-content
  "parses a byte sequence which has been read from an mbn file

   returns: array with parsed items, currently only two content
   types are implemented

   ::nv-items    legacy nv item
   ::efs-items   efs item"
  [s]
  (let [mbn (extract-mbn-elf32-prog-segment s)
        [mcfg-header r] (extract-mcfg-header mbn)
        [mcfg-version r] (extract-mcfg-version r)
        num-mcfg-items (::mcfg_num_items mcfg-header)]
    (conj
     (extract-mcfg-items num-mcfg-items r)
     {::mcfg_header mcfg-header}
     {::mcfg_version mcfg-version})))



(defn- decode-mbn-nv-item-content
  "decode legacy nv items out of mbn map

   return hash map with nv item number as keyed argument
   and parsed parameters as values."
  [nv-definition-schema flat-mbn-item-map]
  (let [resvec
        (map
         (fn [nv-item]
           (let [item-key (first nv-item)
                 item-val (second nv-item)
                 item-index (first item-val)
                 item-val (drop 1 item-val) ;; omit subscriber respectively index id, not part of efs
                 item-schema (-> nv-definition-schema :nv-items item-key)
                 decoded (if item-schema
                           {:params (decode-binary-nv-params (:content item-schema) item-val)}
                           {:errors "missing schema"})]
             {item-key (assoc decoded :data item-val :index item-index :name (:name item-schema))}))
         (::nv-item flat-mbn-item-map))

        error-items (filter #(-> % vals first :errors) resvec)
        collected-errors (map #(format "nv-item %s: %s"
                                       (-> % keys first key2str)
                                       (-> % vals first :errors)) error-items)
        resmap (apply merge resvec)]
    (assoc resmap :errors collected-errors)))



(defn- decode-mbn-efs-item-content
  "decode legacy nv items out of mbn map

   return hash map with nv item number as keyed argument
   and parsed parameters as values."
  [nv-definition-schema flat-mbn-item-map]
  (let [resvec
        (map
         (fn [nv-item]
           (let [item-key (first nv-item)
                 item-val (second nv-item)
                 item-index (first item-val)
                 item-val (drop 1 item-val) ;; omit subscriber respectively index id, not part of efs
                 item-schema (-> nv-definition-schema :efs-items item-key)
                 decoded (if item-schema
                           {:params (decode-binary-nv-params (:content item-schema) item-val)}
                           {:errors "missing schema"})]
             {item-key (assoc decoded :data item-val :index item-index :name (:name item-schema))}))
         (::efs-item flat-mbn-item-map))

        error-items (filter #(-> % vals first :errors) resvec)
        collected-errors (map #(format "%s: %s"
                                       (-> % keys first key2str)
                                       (-> % vals first :errors)) error-items)
        resmap (apply merge resvec)]
    (assoc resmap :errors collected-errors)))



(defn- map-mbn-efs-to-qcn-efs
  "transforms efs-item hash map of form
   {:path_item_1 {:params [] :data [], :path_item_2 { ... } } to

   legacy qcn representation of form

   {:number_1 { :path path_item_1 :params [] :data [] }, :number_2 { ...} }

   we can reuse the qcn_print function so"
  [items]
  (first
   (reduce
    (fn [[res num] item]
      (let [path (first item)
            item (second item)
            item (assoc item :path path)
            ]
        [(assoc res (keyword (str num)) item)
         (inc num)]))
    [{} 1]
    items)))


(defn parse-mbn-data
  "Parse Qualcomm's non-volatile modem configuration binary (mbn) file.

   nv-definition-schema: parsed schema xml file, refer e.g. to parse-nv-definition-file
   mbn-data-file-name:   name of data mbn file, refer e.g. to samples/sample.mbn

   returns nested clojure hash map in same/similar format as generated in qcn_parser.clj:
    :NV_ITEM_ARRAY           -> provides legacy numbered nv item backup data
    :NV_Items                -> provides EFS nv item backup data
    :mcfg-version            -> integer mbn file version identifier
    :carrier-index-info      -> integer with carrier index information
    :errors                  -> contains prarsing errors."
  [nv-definition-schema mbn-data-file-name]
  (let [s (seq (read-file mbn-data-file-name))
        mbn-items (parse-mbn-file-content s)
        flat-item-map (apply merge-with merge mbn-items)
        nv-items  (decode-mbn-nv-item-content nv-definition-schema flat-item-map)
        efs-items (decode-mbn-efs-item-content nv-definition-schema flat-item-map)
        tot-errors (concat
                    (:errors nv-items)
                    (:errors efs-items))
        nv-items (dissoc nv-items :errors)
        efs-items (dissoc efs-items :errors)
        _ (def e (dissoc efs-items :errors))
        efs-items (map-mbn-efs-to-qcn-efs efs-items)]
    {:NV_ITEM_ARRAY nv-items
     :NV_Items efs-items
     :Provisioning_Item_Files {}
     :EFS_Backup {}
     :errors tot-errors
     :mcfg-version (-> flat-item-map ::mcfg_version ::mcfg_version)
     :carrier-index-info (-> flat-item-map ::mcfg_header ::mcfg_muxd_carrier_index_info)}))




(comment

  (def s (seq (read-file "samples/mcfg_sw_dt.mbn")))
  (def mbn (extract-mbn-elf32-prog-segment s))


  (let [mbn (extract-mbn-elf32-prog-segment s)
        [mcfg-header r] (extract-mcfg-header mbn)
        [mcfg-version r] (extract-mcfg-version r)
        num-mcfg-items (::mcfg_num_items mcfg-header)]
    (def mcfg-header mcfg-header)
    (def mcfg-version mcfg-version)
    (def num-mcfg-items num-mcfg-items))


  (::mcfg_muxd_carrier_index_info header)

  (first (extract-mcfg-item-prefix (drop mcfg-nv-data-offset mbn)))
  (first (extract-mcfg-item (drop mcfg-nv-data-offset mbn)))


  (def items (parse-mbn-file-content s))

  (def nv-definition-schema (parse-nv-definition-file "samples/NvDefinition.xml"))

  (def test-item-key :850)
  (def test-item-content (test-item-key (::nv-item flat-item-map)))
  (def test-item-schema (test-item-key (:nv-items nv-definition-schema)))


  (decode-binary-nv-params (:content test-item-schema) (drop 1 test-item-content))

  (decode-mbn-efs-item-content nv-definition-schema flat-item-map)
  (decode-mbn-nv-item-content nv-definition-schema flat-item-map)

  (:errors (decode-mbn-nv-item-content nv-definition-schema flat-item-map))

  (def items (parse-mbn-data nv-definition-schema "samples/mcfg_sw_dt.mbn"))

  (println items)
  (println (:NV_ITEM_ARRAY items))
  (println (:NV_Items items))
  (println (keys (:NV_ITEM_ARRAY items)))
  (keys (dissoc (:NV_ITEM_ARRAY items) :errors))

  (println (:882 (:NV_ITEM_ARRAY items)))

  (print-nv-item-set nv-definition-schema items :efs-subst false)
  (items :File_Version)
  )
