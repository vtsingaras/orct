;; Open Radio Calibration Toolkit
;; An enhanced Open Source Implementation to replace Qualcomm's QRCT
;;
;; The use and distribution terms for this software are covered by
;; the GNU General Public License
;;
;; (C) 2015, Otto Linnemann
;;
;; qcn-parser.cljs - writes QCN file, takes clojure'd xml data as input


(ns orct.qcn-writer
  (:use [orct.utils]
        [orct.nv-xml] ;; only temporary
        )
  (:require [clojure.xml :as xml]
            [clojure.zip :as z]
            [clojure.string :as str]
            [clojure.edn :as edn])
  (:import java.nio.ByteBuffer java.io.FileInputStream
           [org.apache.poi.poifs.filesystem POIFSFileSystem DirectoryNode DocumentNode
            DocumentInputStream DocumentOutputStream]))


(defn- get-ordinal-member-hash
  "transform  efs parameter  member  item list  into  sorted hash  map
  ordinal  item position  as key  and  all other  item attributes  as
  values."
  [nv-efs-params]
  (let [[result idx]
        (reduce
         (fn [[result member-idx]
              member]
           [(assoc result member-idx member)
            (inc member-idx)])
         [(sorted-map #_result)  0 #_idx]
         nv-efs-params)]
    result))


(defn- get-named-member-hash
  "transform  efs parameter  member  item list  into  sorted hash  map
  ordinal  item position  as key  and  all other  item attributes  as
  values."
  [nv-efs-params]
  (let [[result idx]
        (reduce
         (fn [[result member-idx]
              member]
           (let [name (:name member)]
             [(assoc result name (dissoc member name))
              (inc member-idx)]))
         [(sorted-map #_result)  0 #_idx]
         nv-efs-params)]
    result))


(defn- get-member-by-name-or-ordinal-number
  "tries to retrieve nv item  compement member by specified hash name.
  Since  this is  not  always defined,  we retrieve  a  member by  its
  ordinal number otherwise"
  [nv-efs-params  member-name  ordinal-number]
  (if-not (empty? member-name)
    (or ((get-named-member-hash nv-efs-params) member-name)
        ((get-ordinal-member-hash nv-efs-params) ordinal-number))
    ((get-ordinal-member-hash nv-efs-params) ordinal-number)))


(comment

  ;; example for efs nv definition with named components
  (def ims-dpl-config-def (-> nv-definition-schema
                              :efs-items
                              :/nv/item_files/ims/qp_ims_dpl_config
                              :content))

  ;; example for efs nv definition without named components
  (def rpm-info-def (-> nv-definition-schema
                        :efs-items
                        :/nv/item_files/modem/mmode/rpm_info
                        :content))

  (get-ordinal-member-hash ims-dpl-config-def)
  (get-ordinal-member-hash rpm-info-def)

  (get-named-member-hash ims-dpl-config-def)
  (get-named-member-hash rpm-info-def)

  (get-member-by-name-or-ordinal-number  ims-dpl-config-def "e911_ipv6_enabled" 0)
  (get-member-by-name-or-ordinal-number  rpm-info-def "" 3)
  (get-member-by-name-or-ordinal-number  rpm-info-def "xyz" 3)

  )


(defn- byte-array-str-of-size
  "takes string  input argument  and returns  byte array  of specified
  size.  Appends  zeros  when  size  is  larger  than  string  length,
  otherwise cut off to make the result string fit."
  [string size]
  (let [string (seq (str string))
        string-len (count string)
        string-of-size (if (> size string-len)
                         (concat string (take (- size string-len ) (repeat 0)))
                         (take size string))]
    (byte-array (map int string-of-size))))


(comment
  (vec (byte-array-str-of-size "ABC" 5))
  (vec (byte-array-str-of-size "ABC" 2))
  )


(defn- valstr2byte-arrays
  "Takes string of form '[val1,  val2, ...]' and returns corresponding
  byte  arrays.  Function  checks  for  correct  encoding  and  throws
  exeception in case  of wrong declaration. The only  exception is the
  data type  uint8 which can  be used  to declare character  string as
  well."
  [s type size encoding]
  (let [bytes-errors
        (map
         (fn [p error]
           (let [encoding (str/lower-case encoding)
                 ishex? (fn [s] (re-matches #"^(?:0[xX])?[0-9a-fA-F]+$" s))
                 isdec? (fn [s] (re-matches #"^(?:-)?[0-9]+$" s))
                 isnumber? (fn [s] (or (ishex? s) (isdec? s)))
                 hexprefix (fn [s] (if (re-find #"^0[xX]" s) s (str "0x" s)))
                 error (case encoding
                         "hex"
                         (when-not (or (= type "uint8") (isnumber? p))
                           (format "wrong encoding for value %s, should be hex!" p))

                         "dec"
                         (when-not (or (= type "uint8") (isnumber? p))
                           (format "wrong encoding for value %s, should be dec or hex!" p))

                         "string"
                         nil

                         (format "encoding %s invalid!" encoding))

                 p (if (and (= encoding "hex") (ishex? p)) (hexprefix p) p) ; ensure hex prefix
                 op
                 (case type
                   "int8"   (partial long2byteseq 8)
                   "int16"  (partial long2byteseq 16)
                   "int32"  (partial long2byteseq 32)
                   "int64"  (partial long2byteseq 64)
                   "uint8"  #(if (number? %) (long2byteseq 8 %) (byte-array-str-of-size % size))
                   "uint16" (partial long2byteseq 16)
                   "uint32" (partial long2byteseq 32)
                   "uint64" (partial long2byteseq 64)
                   "string" #(byte-array-str-of-size % size)
                   #(throw (IllegalStateException. (format "type %s for value %s invalid!" type %))))]

             (try
               [(byte-array (op (edn/read-string p))) error]
               (catch Throwable t
                 [(byte-array [0]) (if error (str error "," (.getMessage t)) (.getMessage t))]))))

         (str/split s #"[, ]+")
         (repeat nil))
        bytes (map first bytes-errors)
        errors (filter identity (map second bytes-errors))]
    [bytes errors]))



(comment

  (def r (first (valstr2byte-arrays "0x20, 21" "uint8" 2 "hex")))
  (def r (first (valstr2byte-arrays "0x20, 21" "uint8" 2 "dec")))

  (valstr2byte-arrays "0x20, 2a" "uint8" 2 "hex")  ;; -> ok
  (valstr2byte-arrays "0x20, 2az" "uint8" 2 "hex") ;; -> invalid number

  (valstr2byte-arrays "0x20, 2az" "ujnt8" 2 "hex") ;; -> type and encoding invalid

  (map vec r)

  (def r (first (valstr2byte-arrays"IMS" "uint8" 3 "hex")))
  (def r (first (valstr2byte-arrays"0x0102, 0x0304" "uint16" 2 "hex")))
  (def r (first (valstr2byte-arrays"0x0102, 304" "uint16" 2 "hex")))
  (valstr2byte-arrays"102, 304" "uint16" 2 "dec")
  (valstr2byte-arrays"102, 0x304" "uint16" 2 "dec")


  (def r (first (valstr2byte-arrays "0" "uint16" 1 "dec")))

  (def r (first (valstr2byte-arrays "ABC" "string" 3 "string")))

  (def r (first (valstr2byte-arrays "IMS" "uint8" 3 "string")))
  (def r (first (valstr2byte-arrays "IMS" "uint8" 255 "string")))
  (String. (first r))



  (read-string "#java.io.FileWriter[\"myfile.txt\"]")
  (edn/read-string "#java.io.FileWriter[\"myfile.txt\"]")
  (edn/read-string "[1 2 3]")

  (def ishex? (fn [s] (re-matches #"^(?:0[xX])?[0-9a-fA-F]+$" s)))
  (def isdec? (fn [s] (re-matches #"^(?:-)?[0-9]+$" s)))

  (isdec? "-123")
  (isdec? "0x20")

  (ishex? "-0123")
  (ishex? "ABC")
  (ishex? "0123")
  (ishex? "0x0123H")

  )


(defn- valstr2byte-array
  "Takes string of form '[val1,  val2, ...]' and returns corresponding
  byte  array.   Function  checks  for  correct  encoding  and  throws
  exeception in case  of wrong declaration. The only  exception is the
  data type  uint8 which can  be used  to declare character  string as
  well."
  [s type size encoding]
  (let [[result-seq errors] (valstr2byte-arrays s type size encoding)]
    [(byte-array (mapcat vec result-seq)) errors]))


(comment

  (def r (valstr2byte-array "0x0102, 304" "uint16" 2 "hex"))
  (vec (first r))
  )



(defn- transform-efs-item-params-to-qcn-struct
  "transform nv  parameter content  data given  in format  parsed from
  xml  definition  into same  format  as  used  by qcn  parser.  Takes
  nv-definition-schema and and distinctive efs-item as input arguments
  and returns list of processed item elemens with :data member."
  [nv-definition-schema  efs-item]
  (let [[path xml-params] efs-item
        xml-content (:content xml-params)
        efs-schema (-> nv-definition-schema :efs-items path :content)
        encoding (or (:encoding xml-params) "dec")
        result
        (cond

          (empty? efs-schema) ;; when schema defintion not given, we assume just uint8
          (if (> (count xml-content) 1)
            (throw (IllegalStateException.
                    (format "efs item %s lacks schema definition and has more than one element!"
                            path)))
            {:name "" :val xml-content :type "uint8" :member-idx 0})

          (map? (first xml-content)) ;; nv provided as associate array (hash map) for components
          (map
           (fn [{:keys [tag attrs content]} member-idx]
             (let [member-def (get-member-by-name-or-ordinal-number
                               efs-schema (key2str tag) member-idx)
                   [data err] (valstr2byte-array (first content) (:type member-def)
                                                    (:size member-def) encoding)]
               {:name tag
                :val content
                :data data
                :type (:type member-def)
                :size (:size member-def)
                :member-idx member-idx
                :errors err}))
           xml-content
           (iterate inc 0))

          (vector? (first xml-content)) ;; nv components provided just as vector
          (map
           (fn [val member-idx]
             (let [member-def (get-member-by-name-or-ordinal-number
                               efs-schema nil member-idx)
                   [data err] (valstr2byte-array (first val) (:type member-def)
                                                    (:size member-def) encoding)]
               {:name (:name member-def)
                :val val
                :data data
                :type (:type member-def)
                :size (:size member-def)
                :member-idx member-idx
                :errors err}))
           xml-content
           (iterate inc 0))

          (string? (first xml-content))
          (let [val (str/trim (first xml-content))
                [data err] (valstr2byte-array val "uint8" 1 encoding)]
            [{:name "flat" :val [val] :type "uint8" :member-idx 0
              :data data
              :errors err}])

          :else (throw (IllegalStateException.
                        (format "efs item %s has wrong definition!" path))))]
    (vec result)))


(comment ;; usage illustration

  (def nv-xml-data (parse-nv-data-file "samples/Masterfile.xml"))

  (defn- test-transform-efs-item-params
    [path]
    (let [efs-item [path (-> nv-xml-data :efs-items path)]]
      (transform-efs-item-params-to-qcn-struct
       nv-definition-schema
       efs-item)))

  (test-transform-efs-item-params :/nv/item_files/ims/qipcall_octet_aligned_mode_amr_wb)
  (test-transform-efs-item-params :/nv/item_files/ims/qp_ims_dpl_config)
  (test-transform-efs-item-params :/nv/item_files/ims/qp_ims_sms_config)
  (test-transform-efs-item-params :/nv/item_files/modem/uim/uimdrv/feature_support_hotswap)


  (def dpl-config
    (first (test-transform-efs-item-params :/nv/item_files/ims/qp_ims_dpl_config)))

  )


(defn- aggregate-param-member-data
  "iterate through all efs members and concatenate data elements.
  [{:name :data :errors}, ...] -> [byte-array( data1, data2, ...),
                                   [vector of all errors]"
  [efs-item]
  (let [data-seq (reduce
                  (fn [alldata {:keys [name val data]}]
                    (concat alldata data))
                  []
                  efs-item)]
    [(byte-array data-seq)
     (mapcat (fn [{:keys [errors name val]}]
                (when-not (empty? errors)
                  [(format "error in %s, %s: %s" name val (first errors))]))
             efs-item)]))


(defn- transform-efs-items-to-qcn-struct
  "transform nv  efs data given  in format parsed from  xml definition
  (path->item  hash)  to  new  index-item  hash as  used  in  qcn  poi
  structure."
  [nv-definition efs-items]
  (let [[result idx errors]
        (reduce

         (fn [[result idx errors]
              efs-item]
           (let [idx-key (keyword (format "%08d" idx))
                 [path params] efs-item

                 efs-struct
                 (try
                   (transform-efs-item-params-to-qcn-struct nv-definition
                                                            efs-item)
                   (catch Throwable e [nil
                                       (do (def efs-item efs-item) (format "item path %s malformed error: %s"
                                                      path (.getMessage e)))]))
                 [efs-data par-errors] (aggregate-param-member-data efs-struct)
                 par-errors-str (pr-str (map #(str % ",") par-errors))
                 errors (if (not-empty par-errors)
                          (concat errors [(format "errors for efs-nv %s: %s" path par-errors-str)])
                          errors)]

             [(-> result
                  (assoc-in [idx-key :path] path)
                  (assoc-in [idx-key :params] efs-struct)
                  (assoc-in [idx-key :data] efs-data))
              (inc idx)
              errors]))

         [{#_index_nv-item}  0 #_idx  [#_errors]]
         efs-items)]
    [result errors]))



(comment

  (def nv-xml-data (parse-nv-data-file "samples/Masterfile.xml"))

  (def t-efs-errors (transform-efs-items-to-qcn-struct nv-definition-schema (:efs-items nv-xml-data)))
  (def t-efs (first t-efs-errors))
  (def t-errors (second t-efs-errors))

  (:/nv/item_files/ims/qp_ims_dpl_config (:efs-items nv-xml-data))
  (:/nv/item_files/ims/qp_ims_dpl_config (:efs-items nv-definition-schema))

  (doall t-efs-errors)

  (println t-efs)

  (println
   (:00000064 t-efs)) ;; qp_ims_dpl_config

  (println
   (vec (:data (:00000064 t-efs)))) ;; qp_ims_dpl_config

  (println
   (:00000061 t-efs)) ;; rpm_info

  )


(defn- separate-efs-item-stores
  "EFS items  are assigned  to two  different QCN  document streams.
  Those with the  flag :provisioning-store set to true go  to to the
  stream  'Provisioning_Item_Files', all  others  go to  'NV_Items'.
  This function  returns a vector  where the first  element provides
  all items for  the provisiong store, the second the  items for the
  nv item store."
  [parsed-nv-xml]
  (reduce
   (fn [[store efs-backup]
        [path params]]
     (let [prov-store (:provisioning-store params)
           item {path params}
           [store efs-backup] (if prov-store
                                [(merge store item) efs-backup]
                                [store (merge efs-backup item)])]
       [store efs-backup]))
   [{#_prov-store} {#_nv-item-store}] ;; result vector, initially empty
   (:efs-items parsed-nv-xml)))


(comment

  (first (separate-efs-item-stores nv-xml-data))   ;; provisiong item store efs items
  (second (separate-efs-item-stores nv-xml-data))  ;; nv-item store efs items

  )


(defn- transform-nv-item-params-to-qcn-struct
  "transform nv parameter content data given in format parsed from xml
  definition into same format as used by qcn parser."
  [nv-item-schema  nv-item]
  (let [{:keys [name mapping encoding content]} nv-item
        params (str/split (first content) #"[, ]+")
        params (map
                (fn [schema param]
                  (let [{:keys [name type size]} schema
                        [data err] (valstr2byte-array param type
                                                   size encoding)]
                    (-> schema
                        (assoc :val param)
                        (assoc :data data)
                        (assoc :errors err))))
                (:content nv-item-schema)
                params)
        [data errors] (aggregate-param-member-data params)]
    [{:name name :data data :params params} errors]))


(comment

  (def nv-items (:nv-items nv-xml-data))

  (map #(println %) nv-items)

  (def cv-service-table-I (:1014 nv-items))
  (def cv-service-table-I-schema (:1014 (:nv-items nv-definition-schema)))

  (def t (transform-nv-item-params-to-qcn-struct
          cv-service-table-I-schema
          cv-service-table-I))

  (println (vec (:data (first t))))

  )


(defn- transform-nv-items-to-qcn-struct
  "transform  nv item  legacy data  given  in format  parsed from  xml
  definition (path->item hash)  to new index-item hash as  used in qcn
  poi structure."
  [nv-item-schema  nv-items]
  (let [[result errors]

        (reduce
         (fn [[result errors]
              [idx params]]
           (let [idx-schema (nv-item-schema idx)
                 [params par-errors] (if idx-schema
                                       (transform-nv-item-params-to-qcn-struct
                                        idx-schema
                                        params)

                                       [params
                                        ["no schema given!"]])
                 par-errors-str (pr-str (map #(str % ",") par-errors))]

             [(assoc result idx params)
              (if (not-empty par-errors)
                (concat errors [(format "errors for nv %s: %s" idx par-errors-str)])
                errors)]))

         [{#_index_nv-item}  [#_errors]]
         nv-items)]

    [result errors]))

(comment

  (def nv (transform-nv-items-to-qcn-struct (:nv-items nv-definition-schema) nv-items))

  (map #(println %) (first nv))
  (map #(println %) (second nv))

  )
