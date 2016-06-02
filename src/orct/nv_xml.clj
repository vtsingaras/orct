;; Open Radio Calibration Toolkit
;; An enhanced Open Source Implementation to replace Qualcomm's QRCT
;;
;; The use and distribution terms for this software are covered by
;; the GNU General Public License
;;
;; (C) 2015, Otto Linnemann
;;
;; nv-xml.cljs - parser for NV definition schema and QCN xml input data

(ns orct.nv-xml
  (:use [orct.utils])
  (:require [clojure.xml :as xml]
            [clojure.zip :as z]
            [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.set :as set])
  (:import java.nio.ByteBuffer java.io.FileInputStream))


(defn- parse-nv-definition-content
  [id content]
  (map (fn [{:keys [tag attrs content]}]
         (when-not (= tag :Member)
           (throw (IllegalStateException. (format "NV item %s has wrong tag %s!"
                                                  (key2str id) (key2str tag)))))
         (let [{:keys [name type sizeOf]} attrs]
           {:name name :type type :size (str2int sizeOf)}))
       content))


(defn- calculate-item-definition-size
  "calculate overall size of a given parameter within schema definition
   sample invocation:

  This shall  be handled with  care! It is exclusively  required since
  (some, all?)  legacy NV  items do not  carry decent  size information
  within the qcn binary container. So we need to be able to calculate
  the size information from the schema."

  [item-definition-vec]
  (reduce
   (fn [sum item]
     (let [{:keys [size type]} item
           type-bits (str2int (or (re-find (re-pattern "\\d+") type) "0"))
           type-bytes (/ type-bits 8)
           elem-size (* type-bytes size)]
       (+ sum elem-size))
     )
   0 item-definition-vec))


(comment
  (calculate-item-definition-size
   [{:name "par1" :type "uint8" :size 2}
    {:name "par2" :type "int32" :size 1}
    {:name "par3" :type "string" :size 100}
    ])
  )


(defn- is-nvitem-strid-extended?
  "ALL items with ID 20000 or above are stored as an file in EFS area
   /nv/item_files/rfnv/000xxxxx, where xxxxx is the ID. Function returns
   true if this is the case."
  [sid]
  (>= (str2int sid) 20000))


(defn- parse-nv-definition-file-stage-1
  "1st stage of NvDefintion file parse which provides the xml schema definition
  for mobile phone NV item setup."
  [filename]
  (let [nv-def-xml (xml/parse filename)]
    (loop [result {} loc (z/xml-zip nv-def-xml)]
      (if (z/end? loc)
        result
        (let [n (z/node loc)
              content (-> n :content)
              id (-> n :attrs :id)
              errors (:errors result)
              result (cond
                       ;; classical nv item ranging from 0 to 19999
                       (and (= (:tag n) :NvItem) (not (is-nvitem-strid-extended? id)))
                       (let [id (keyword id)
                             errors (if (-> result :nv-items id)
                                      (conj errors (format "schema for nv item %s multiple defined!" (key2str id)))
                                      errors)
                             content (parse-nv-definition-content id content)
                             update (-> result
                                        (assoc-in [:errors] errors)
                                        (assoc-in [:nv-items id :name] (-> n :attrs :name))
                                        (assoc-in [:nv-items id :permission ] (-> n :attrs :permission))
                                        (assoc-in [:nv-items id :content] content)
                                        (assoc-in [:nv-items id :size]
                                                  (calculate-item-definition-size content))
                                        )]
                         update)

                       ;; extended rf nv item from 20000 and above stored in the same way as efs items
                       (and (= (:tag n) :NvItem) (is-nvitem-strid-extended? id))
                       (let [name (keyword (str "/nv/item_files/rfnv/000" id))
                             errors (if (-> result :efs-items name)
                                      (conj errors (format "schema efs item %s multiple defined!" (key2str name)))
                                      errors)
                             update (-> result
                                        (assoc-in [:errors] errors)
                                        (assoc-in [:efs-items name :permission ] (-> n :attrs :permission))
                                        (assoc-in [:efs-items name :compressed ] (-> n :attrs :compressed))
                                        (assoc-in [:efs-items name :variable-size ] (-> n :attrs :variableSize))
                                        (assoc-in [:efs-items name :content ]
                                                  (parse-nv-definition-content name content)))]
                         update)

                       ;; explicit efs stored item
                       (= (:tag n) :NvEfsItem)
                       (let [name (keyword (-> n :attrs :fullpathname))
                             errors (if (-> result :efs-items name)
                                      (conj errors (format "schema efs item %s multiple defined!" (key2str name)))
                                      errors)
                             update (-> result
                                        (assoc-in [:errors] errors)
                                        (assoc-in [:efs-items name :permission ] (-> n :attrs :permission))
                                        (assoc-in [:efs-items name :compressed ] (-> n :attrs :compressed))
                                        (assoc-in [:efs-items name :variable-size ] (-> n :attrs :variableSize))
                                        (assoc-in [:efs-items name :content ]
                                                  (parse-nv-definition-content name content)))]
                         update)

                       ;; data type declaration
                       (= (:tag n) :DataType)
                       (let [name (keyword (-> n :attrs :name))
                             errors (if (-> result :data-type name)
                                      (conj errors (format "data type %s multiple defined!" (key2str name)))
                                      errors)
                             update (-> result
                                        (assoc-in [:errors] errors)
                                        (assoc-in [:data-type name :content]
                                                  (parse-nv-definition-content name content)))]
                         update)

                       :else result)]
          (recur result (z/next loc)))))))



(defn- subst-nv-content-data-types
  "helper function, replaces in specified content schema defintion
   a combined data type by its atomic definition. See following
   comment block for usage illustration."
  [nv-definition-schema item-content-data]
  (let [combined-data-types (dissoc
                             (:data-type nv-definition-schema)
                             :uint8 :uint16 :uint32 :uint64 :int8 :int16 :int32 :int64 :string)]
    (flatten
     (reduce
      (fn [content-members content-member]
        (let [{:keys [name type size]} content-member
              type (keyword type)
              combined-data-type (:content (combined-data-types type))]
          (conj content-members
                (if combined-data-type
                  (flatten (repeat size combined-data-type))
                  content-member))))
      (list)
      (reverse item-content-data)))))



(comment

  (def my-nv-item-content (:content (:29306 (:nv-items nv-definition-schema))))

  (println "before subst-->" my-nv-item-content)
  (println "after subst-->" (subst-nv-content-data-types nv-definition-schema my-nv-item-content))

  )


(defn- subst-nv-data-types
  "helper function which replaces all combined data types within
   given schema items list by the associated atomic definitions.
   Refer to following comment block for usage illustration."
  [nv-definition-schema items]
  (reduce
   (fn [result item]
     (let [ikey (key item)
           ival (val item)
           new-content (subst-nv-content-data-types
                                      nv-definition-schema
                                      (:content ival))
           ival (assoc ival :content new-content)
           ival (assoc ival :size (calculate-item-definition-size new-content))]
       (merge result {ikey ival})))
   {} #_result
   items)
  )


(comment

  (def nv-definition-schema (parse-nv-definition-file-stage-1 "samples/NvDefinition_9640.xml"))

  (def nv-def-subs-items (subst-nv-data-types nv-definition-schema (:nv-items nv-definition-schema)))

  (def my-nv-item-content (:content (:29306 (:nv-items nv-definition-schema))))
  (def my-nv-item-content-2 (:content (:29306 nv-def-subs-items)))

  (def nv28154 ((:efs-items nv-definition-schema) (keyword "/nv/item_files/rfnv/00028154")))


  (def efs-definition-schema (:efs-items nv-definition-schema))
  (def nv28154 (efs-definition-schema (keyword "/nv/item_files/rfnv/00028154")))

  (def efs-definition-schema_1 (subst-nv-data-types nv-definition-schema efs-definition-schema))
  (def nv28154_1 (efs-definition-schema_1 (keyword "/nv/item_files/rfnv/00028154")))

  (def efs-definition-schema_2 (subst-nv-data-types nv-definition-schema efs-definition-schema_1))
  (def nv28154_2 (efs-definition-schema_2 (keyword "/nv/item_files/rfnv/00028154")))

  (def efs-definition-schema_3 (subst-nv-data-types nv-definition-schema efs-definition-schema_2))
  (def nv28154_3 (efs-definition-schema_3 (keyword "/nv/item_files/rfnv/00028154")))

  (def efs-definition-schema_4 (subst-nv-data-types nv-definition-schema efs-definition-schema_3))
  (def nv28154_4 (efs-definition-schema_4 (keyword "/nv/item_files/rfnv/00028154")))

  (println nv28154_4)

  )


(defn- subst-all-combined-schema-data-types
  "Substitute all combined data types which are defined elsewhere
   in the nv-defintion-schema under the tag :data-type by the
   given atomic types. Refer to next comment block for usage
   illustration."
  [nv-definition-schema]
  (loop [iterations 5
         result-schema nv-definition-schema]
    (if (zero? iterations)
      result-schema
      (recur (dec iterations)
             (-> result-schema
                 (assoc :efs-items
                        (subst-nv-data-types result-schema (:efs-items result-schema))))))))


(comment

  (def nv-defintion-schema-2 (subst-all-combined-schema-data-types nv-definition-schema))

  (def my-nv-item-content (:content (:29306 (:nv-items nv-definition-schema))))
  (def my-nv-item-content-2 (:content (:29306 (:nv-items nv-defintion-schema-2))))

  )


(defn parse-nv-definition-file
  "parse NvDefintion file which provides the xml schema definition
  for mobile phone NV item setup."
  [filename]
  (subst-all-combined-schema-data-types
   (parse-nv-definition-file-stage-1 filename))
  )


(defn- extend-to-same-path
  "extends non fully qualified filename with same path
  as fully qualified or accessible template file."
  [template-file-name filename-to-extend]
  (let [sep (java.io.File/separator)
        t (java.io.File. template-file-name)
        p (.getParent t)]
    (if p
      (str p sep filename-to-extend)
      filename-to-extend)))


(defn parse-nv-data-file
  "parse NvDefintion file which provides the xml schema definition
  for mobile phone NV item setup."
  [filename]
  (let [nv-def-xml (xml/parse filename)]
    (loop [result {} loc (z/xml-zip nv-def-xml)]
      (if (z/end? loc)
        result
        (let [n (z/node loc)
              content (-> n :content)
              id (-> n :attrs :id)
              errors (:errors result)
              result (cond
                       ;; include of additional nv definition files
                       (= (:tag n) :xi:include)
                       (let [xml-include (-> n :attrs :href)
                             include-file (first (str/split xml-include #"#"))
                             fully-qualified-input-file (extend-to-same-path filename include-file)]
                         (let [inside (parse-nv-data-file fully-qualified-input-file)
                               errors (concat (:errors result) (:errors inside))
                               result (merge-with merge (dissoc result :errors) (dissoc inside :errors))]
                           (-> result (assoc-in [:errors] errors))))

                       ;; classical nv item ranging from 0 to 19999
                       (and (= (:tag n) :NvItem) (not (is-nvitem-strid-extended? id)))
                       (let [id (keyword id)
                             errors (if (-> result :nv-item id)
                                      (conj errors (format "nv item %s multiple defined!" (key2str id)))
                                      errors)
                             update (-> result
                                        (assoc-in [:errors] errors)
                                        (assoc-in [:nv-items id :name] (-> n :attrs :name))
                                        (assoc-in [:nv-items id :index ] (or (-> n :attrs :index) "1"))
                                        (assoc-in [:nv-items id :mapping ] (-> n :attrs :mapping))
                                        (assoc-in [:nv-items id :encoding] (-> n :attrs :encoding))
                                        (assoc-in [:nv-items id :content]
                                                  content))]
                         update)

                       ;; extended rf nv item from 20000 and above stored in the same way as efs items
                       (and (= (:tag n) :NvItem) (is-nvitem-strid-extended? id))
                       (let [name (keyword (str "/nv/item_files/rfnv/000" id))
                             errors (if (-> result :efs-items name)
                                      (conj errors (format "efs item %s multiple defined!" (key2str name)))
                                      errors)
                             update (-> result
                                        (assoc-in [:errors] errors)
                                        (assoc-in [:rf-items name :index ] (or (-> n :attrs :index) "1"))
                                        (assoc-in [:rf-items name :mapping ] (-> n :attrs :mapping))
                                        (assoc-in [:rf-items name :encoding] (-> n :attrs :encoding))
                                        (assoc-in [:rf-items name :provisioning-store]
                                                  (-> n :attrs :useProvisioningStore))
                                        (assoc-in [:rf-items name :content]
                                                  content))]
                         update)

                       ;; explicit efs stored item
                       (= (:tag n) :NvEfsItem)
                       (let [name (keyword (-> n :attrs :fullpathname))
                             errors (if (-> result :efs-items name)
                                      (conj errors (format "efs item %s multiple defined!" (key2str name)))
                                      errors)
                             update (-> result
                                        (assoc-in [:errors] errors)
                                        (assoc-in [:efs-items name :index ] (or (-> n :attrs :index) "1"))
                                        (assoc-in [:efs-items name :mapping ] (-> n :attrs :mapping))
                                        (assoc-in [:efs-items name :encoding] (-> n :attrs :encoding))
                                        (assoc-in [:efs-items name :provisioning-store]
                                                  (-> n :attrs :useProvisioningStore))
                                        (assoc-in [:efs-items name :content]
                                                  content))]
                         update)

                       :else result)]
          (recur result (z/next loc)))))))



(comment

  (def nv-definition-schema (parse-nv-definition-file "samples/NvDefinition.xml"))
  (def nv-xml-data (parse-nv-data-file "samples/Masterfile.xml"))

  (println nv-xml-data)
  (println (:errors nv-xml-data))

  (map #(println % "\n=========================\n") (:nv-items nv-xml-data))
  (map #(println % "\n=========================\n") (:efs-items nv-xml-data))
  (map #(println %) ((:nv-items nv-xml-data) :855))
  (map #(println %) ((:nv-items nv-xml-data) :1015))
  (map #(println %) ((:nv-items nv-xml-data) :1031))
  (map #(println %) ((:nv-items nv-xml-data) :3532))
  (map #(println %) ((:nv-items nv-xml-data) :6873))

  (println (keys nv-xml-data))
  (println (keys (:nv-items nv-xml-data)))
  (println (keys (:efs-items nv-xml-data)))

  (println ((:efs-items nv-xml-data) :/nv/item_files/ims/qp_ims_sms_config))
  (println ((:efs-items nv-xml-data) :/nv/item_files/ims/qp_ims_dpl_config))


  (map #(println %) (:errors nv-definition-schema))
  (map #(println %) (:nv-items nv-definition-schema))
  (map #(println %) (:efs-items nv-definition-schema))

  (def nv (:nv-items nv-definition-schema))

  )



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


(defn- split-value-list-string
  [s]
  "Takes string  of form 'val1,  val2, ...' and returns  sequence with
  elements separated by commas, tabs, spaces or carriage returns."
  (str/split (str/trim s) #"[, \t\n]+"))



(def ^:private number-types #{"int8" "int16" "int32" "int64" "uint8" "uint16" "uint32" "uint64"})
(def ^:private string-types #{"string"})
(def ^:private safe-number-types (disj number-types "uint8"))


(defn- valstr2byte-seqs
  "Takes  sequence of  value  strings and  returns corresponding  byte
  arrays. Function  checks for correct encoding  and throws exeception
  in case  of wrong declaration. The  only exception is the  data type
  uint8 which can be used to declare character string as well."
  [item-str-seq type no-elements encoding]
  (let [ishex? (fn [s] (re-matches #"^(?:0[xX])?[0-9a-fA-F]+$" s))
        isdec? (fn [s] (re-matches #"^(?:-)?[0-9]+$" s))
        isnumber? (fn [s] (or (ishex? s) (isdec? s)))
        add-hexprefix (fn [s] (if (re-find #"^0[xX]" s) s (str "0x" s)))
        hexprefix (fn [s] (if (= encoding "hex") (add-hexprefix s) s))
        encoding (str/lower-case encoding)
        type (str/lower-case type)]
    (let [result
          (reduce
           (fn [processed-items next-item]
             (let [processed-item
                   (condp contains? type
                     #{"uint8"}
                     (if (isnumber? next-item)
                       (let [val (edn/read-string (hexprefix next-item))]
                         {:type "uint8" :val val :data (long2byteseq 8 val)})
                       {:type "string" :val next-item
                        :data (byte-array-str-of-size next-item no-elements)})

                     safe-number-types
                     (let [bits (case type "int8"8 "int16" 16 "int32" 32 "int64" 64
                                      "uint16" 16 "uint32" 32 "uint64" 64)]
                       (if (isnumber? next-item)
                         (let [val (edn/read-string (hexprefix next-item))]
                           {:type type :val val :data (long2byteseq bits val)})
                         {:type "undefined" :val 0 :data nil
                          :errors (format "parameter %s not a number" next-item)}))

                     #{"string"}
                     (let [val (byte-array-str-of-size next-item no-elements)]
                       {:type "string" :val next-item
                        :data (byte-array-str-of-size next-item no-elements)}))]
               (conj processed-items processed-item)))
           [#_processed]
           item-str-seq)]
      result)))

(comment
  (valstr2byte-seqs (split-value-list-string "0x0102, 304") "uint16" 2 "hex")
  )

(defn- valstr2byte-array
  "Takes string of  form 'val1, val2, ...' and  returns parsing result
  within hash table of the form:

   -> params: parsed parameter list, each parameter with :type, val: and :data
   -> data:   byte array of all parsed data
   -> errors: found parsing errors

  Function checks  for correct encoding  and returns error in  case of
  wrong declaration. The  only exception is the data  type uint8 which
  can be used to declare character string as well."
  [string-or-seq  type  no-elements  encoding]
  (let [errors []
        errors (if (contains? (set/union number-types string-types) type)
                 errors
                 (conj errors (format "wrong type %s declared error!" type)))
        errors (if (contains? #{"hex" "dec" "string"} encoding)
                 errors
                 (conj errors (format "wrong encoding %s declared error!" encoding)))
        error-hash {:data (byte-array [0]) :params {}}
        ;; when more than 20 elements expected, but only one string given
        ;; and no field separator detected ',' we can safely assume that
        ;; the specified element is really a string and overload the type
        type (if (and (string? string-or-seq)
                      (> no-elements 20)
                      (not (re-find #"," string-or-seq))) "string" type)]
    (if (not-empty errors)
      (assoc error-hash :errors errors)
      (let [s (if (string? string-or-seq) (split-value-list-string string-or-seq) string-or-seq)
            result-seq (valstr2byte-seqs s type no-elements encoding)
            no-result-elements (count result-seq)
            strings-included (some #(= "string" %) (map :type result-seq))
            errors (filter identity (map :errors result-seq))
            errors (if (and strings-included (not= (count result-seq) 1))
                     (conj errors (format "only one string element allowed: %s" s))
                     (if (and (not strings-included) (not= no-result-elements no-elements))
                       (conj errors (format "mismatch between %d declared, %d defined elements!"
                                            no-elements no-result-elements))
                       errors))]
        (if (not-empty errors)
          (assoc error-hash :errors errors)
          {:params result-seq
           :val (map #(% :val) result-seq)
           :data (byte-array (mapcat :data result-seq))
           :errors errors})))))


(comment

  (def r (valstr2byte-array "0x0102, 304" "uint16" 2 "hex")) ;; -> ok
  (def r (valstr2byte-array "2az" "uint8" 2 "hex"))          ;; -> ok: could be string of 2 elements
  (def r (valstr2byte-array "0x20, 2az" "uint8" 2 "hex"))    ;; -> not ok: only one string elem allowed
  (def r (valstr2byte-array "0x20, 2az" "ujnt8" 2 "hex"))    ;; -> not ok: wrong type
  (def r (valstr2byte-array "0x20, 2az" "uint16" 2 "hex"))   ;; -> not ok: parameter not a number
  (def r (valstr2byte-array "0x20, 0x21" "uint16" 3 "hex"))  ;; -> not ok: parameter mismatch
  (def r (valstr2byte-array "ims" "uint8" 30 "hex"))         ;; -> ok
  (def r (valstr2byte-array "123" "uint8" 30 "hex"))         ;; -> ok, one string, no comma, elems>20
  (def r (valstr2byte-array "123," "uint8" 30 "hex"))        ;; -> not ok,  comma, prop. list meant

  (vec (:data r))
  (String. (:data r))
  )


(defn- transform-item-params-to-qcn-struct
  "transform item parameter component data given in format parsed from
  xml definition into sequence of hashes with keys

  :name,
  :size   - (no elements),
  :val    - ASCII value representation
  :data   - byte array for marshalling
  :errors - sequence of errors which occured during processing."
  [schema-seq  nv-item]
  (let [{:keys [name mapping encoding content]} nv-item
        param-seq (str/split (str/trim (first content)) #"[, \n]+")

        [result _]
        (reduce
         (fn [[ready-param-lst work-param-lst]
              {:keys [name type size] :as schema}]
           (let [[next-params rest-params] (split-at size work-param-lst)
                 result-param (merge schema (valstr2byte-array next-params type size encoding))]
             [(conj ready-param-lst result-param) rest-params]))
         [[] param-seq]
         schema-seq)]
    result))


(comment

  (def r (transform-item-params-to-qcn-struct
          [{:name "" :type "uint8" :size 1} {:name "" :type "uint8" :size 1} {:name "" :type "uint8" :size 1}]
          {:index 0, :mapping "direct" :encoding "dec" :provisioning-store nil, :content ["0, 1, 0"]}))

  (vec (:data (first r)))
  )



(defn- delta-seq
  "from a given sequence generates a new sequence with elements
     v2(n) = v1(n) - v1(n-1)"
  [v]
  (let [v (vec v)]
    (map (fn [a b] (- b a))
         (conj (seq v) 1) (conj (vec v) (last v)))))

(defn undefined-elems-exclusively-at-end?
  "for a given sequence of ones (ok) and zeros (error) returns
     true when errors occur exclusively at the end of the sequence
     and the first element is not an error (1).
     examples:
     (undefined-elems-exclusively-at-end? '(1 1 1 1 1 1 0 0 0 0 0)) -> 1
     (undefined-elems-exclusively-at-end? '(1 1 1 1 1 1 0 0 1 0 0)) -> 0
     (undefined-elems-exclusively-at-end? '(1 1 0 1 1 1 0 0 0 0 0)) -> 0
     "
  [s]
  (and (first s)
       (not-any? #(= % 1) (delta-seq s))))


(comment

  (undefined-elems-exclusively-at-end? '(1 1 1 1 1 1 0 0 0 0 0))
  (undefined-elems-exclusively-at-end? '(0 1 1 1 1 1 1 0 0 0 0 0))
  (undefined-elems-exclusively-at-end? '(0 0 0 0 0 1 1 1 ))
  (undefined-elems-exclusively-at-end? '(0  0 0 0 1))

  )


(defn- transform-efs-item-params-to-qcn-struct-stage-1
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
            (let [type "uint8"
                  encoding "dec"
                  params (valstr2byte-array (first xml-content) type 1 encoding)]
              [(assoc-in params [:errors] (conj (:errors params) "missing schema!"))]))

          (map? (first xml-content)) ;; nv provided as associate array (hash map) for components
          (map
           (fn [{:keys [tag attrs content]} member-idx]
             (let [member-def (get-member-by-name-or-ordinal-number
                               efs-schema (key2str tag) member-idx)
                   params (valstr2byte-array (first content) (:type member-def)
                                                 (:size member-def) encoding)]
               (merge member-def params)))
           xml-content
           (iterate inc 0))

          (string? (first xml-content)) ;; nv components provided just as vector
          (transform-item-params-to-qcn-struct efs-schema xml-params)

          :else (throw (IllegalStateException.
                        (format "efs item %s has wrong definition!" path))))]
    (vec result)))


(defn- transform-efs-item-params-to-qcn-struct
  "same as transform-efs-item-params-to-qcn-struct-stage-1
   plus additional relaxed error handling if parameter
   variable-size is defined"
  [nv-definition-schema  efs-item]
  (let [r (transform-efs-item-params-to-qcn-struct-stage-1 nv-definition-schema efs-item)
        [path xml-params] efs-item
        efs-schema-variable (-> nv-definition-schema :efs-items path :variable-size)]
    (if efs-schema-variable
      (let [params-defined-flags (map #(if (empty? (:params %)) 0 1) r)
            no-elements-defined (reduce + params-defined-flags)]
        (if (undefined-elems-exclusively-at-end? params-defined-flags)
          (take no-elements-defined r)                        ; ok
          r))
      r)))


(comment ;; usage illustration

  (def nv-xml-data (parse-nv-data-file "samples/Masterfile.xml"))
  (def nv-xml-data (parse-nv-data-file "samples/test.xml"))

  (defn- test-transform-efs-item-params
    [path]
    (let [efs-item [path (-> nv-xml-data :efs-items path)]]
      (transform-efs-item-params-to-qcn-struct
       nv-definition-schema
       efs-item)))

  (defn- test-transform-rf-item-params
    [path]
    (let [efs-item [path (-> nv-xml-data :rf-items path)]]
      (transform-efs-item-params-to-qcn-struct
       nv-definition-schema
       efs-item)))

  ;; structured components
  (def r (test-transform-efs-item-params :/nv/item_files/ims/qp_ims_dpl_config))
  (def r (test-transform-efs-item-params :/nv/item_files/ims/qp_ims_sms_config))
  (def r (test-transform-efs-item-params :/nv/item_files/ims/qp_ims_ut_config))

  (def r (test-transform-rf-item-params (keyword "/nv/item_files/rfnv/00027830")))
  (undefined-elems-exclusively-at-end?
   (map #(if (empty? (:params %)) 0 1) r))

  (not-any? #(= % 1) (vec '(0 1 -1 0)))

  ;; no schema
  (def r (test-transform-efs-item-params :/nv/item_files/modem/utils/a2/bam_dynamic_config))
  (vec (:data (first r)))

  ;; unstructured components
  (def r (test-transform-efs-item-params :/nv/item_files/ims/qipcall_octet_aligned_mode_amr_wb))

  (def r (test-transform-efs-item-params :/nv/item_files/modem/uim/uimdrv/feature_support_hotswap))
  (def r (test-transform-efs-item-params :/nv/item_files/data/3gpp/rpm_suppported_sim))
  (def r (test-transform-efs-item-params :/nv/item_files/modem/mmode/rpm_info))


  (def sms-config (test-transform-efs-item-params :/nv/item_files/ims/qp_ims_sms_config))
  (map #(println %) sms-config)



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
                  [(format "error in %s: %s" name (first errors))]))
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
           (try
             (let [idx-key (keyword (str/upper-case (format "%08x" idx)))
                   [path params] efs-item
                   efs-schema (-> nv-definition :efs-items path)
                   compressed (-> efs-schema :compressed)

                   efs-struct
                   (try
                     (transform-efs-item-params-to-qcn-struct nv-definition
                                                              efs-item)
                     (catch Throwable e [nil
                                         (do (def efs-item efs-item) (format "item path %s malformed error: %s"
                                                                             path (.getMessage e)))]))
                   [efs-data par-errors] (aggregate-param-member-data efs-struct)
                   efs-data (if compressed (zlib-compress efs-data) efs-data)
                   par-errors-str (pr-str (map #(str % ",") par-errors))
                   errors (if (not-empty par-errors)
                            (concat errors [(format "errors for efs-nv %s: %s" path par-errors-str)])
                            errors)]

               [(-> result
                    (assoc-in [idx-key :path] path)
                    (assoc-in [idx-key :params] efs-struct)
                    (assoc-in [idx-key :data] efs-data))
                (inc idx)
                errors])
             (catch Throwable t (throw (IllegalStateException.
                                        (format "Error in transformation of efs-items occurred:\n%s"
                                                (pr-str efs-item)))))))

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
   (:00000059 t-efs)) ;; qp_ims_sms_config

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
  [schema-seq  nv-item]
  (let [params (transform-item-params-to-qcn-struct schema-seq  nv-item)
        [data errors] (aggregate-param-member-data params)]
    [(-> nv-item
          (assoc-in [:data] data)
          (assoc-in [:params] params)
          (assoc-in [:errors] errors))
     errors]))


(comment

  (def nv-definition-schema (parse-nv-definition-file "samples/NvDefinition.xml"))
  (def nv-xml-data (parse-nv-data-file "samples/Masterfile.xml"))
  (def nv-items (:nv-items nv-xml-data))

  (map #(println %) nv-items)

  ; not schema given
  (def uim-first-int-class-I (:896 nv-items))
  (def uim-first-int-class-I-schema (:896 (:nv-items nv-definition-schema)))

  (def t (transform-nv-item-params-to-qcn-struct
          (:content uim-first-int-class-I-schema)
          uim-first-int-class-I))

  (def cv-service-table-I (:1014 nv-items))
  (def cv-service-table-I-schema (:1014 (:nv-items nv-definition-schema)))

  (def t (transform-nv-item-params-to-qcn-struct
          (:content cv-service-table-I-schema)
          cv-service-table-I))

  (println (vec (:data (first t))))

  (def hplmn-search (:6844 nv-items))
  (def hplmn-search-schema (:6844 (:nv-items nv-definition-schema)))

  (def t (transform-nv-item-params-to-qcn-struct
          (:content hplmn-search-schema)
          hplmn-search))


  (def port-list (:6873 nv-items))
  (def port-list-schema (:6873 (:nv-items nv-definition-schema)))

  (def t (transform-nv-item-params-to-qcn-struct
          (:content port-list-schema)
          port-list))

  )


(defn- transform-nv-items-to-qcn-struct
  "transform  nv item  legacy data  given  in format  parsed from  xml
  definition (path->item hash)  to new index-item hash as  used in qcn
  poi structure."
  [nv-definition-schema  nv-items]
  (let [nv-item-schema (:nv-items nv-definition-schema)
        schema-missing-msg "no schema given!"
        default-schema [{:name "" :type "uint8" :size 1}] ;; used when undefined
        [result errors]

        (reduce
         (fn [[result errors]
              [idx params]]
           (try
             (let [idx-schema (nv-item-schema idx)
                   [params par-errors] (if idx-schema
                                         (transform-nv-item-params-to-qcn-struct
                                          (:content idx-schema)
                                          params)

                                         (let [[p e] (transform-nv-item-params-to-qcn-struct
                                                       default-schema
                                                       params)]
                                           [p (conj e schema-missing-msg)]))
                   par-errors-str (pr-str (map #(str % ",") par-errors))]

               [(assoc result idx params)
                (if (not-empty par-errors)
                  (concat errors [(format "errors for nv %s: %s" idx par-errors-str)])
                  errors)])
             (catch Throwable t (throw (IllegalStateException.
                                        (format "Error in transformation of nv-item %s occurred:\n%s"
                                                (pr-str idx) (pr-str params)))))))

         [{#_index_nv-item}  [#_errors]]
         nv-items)]

    [result errors]))


(comment

  (def nv (transform-nv-items-to-qcn-struct nv-definition-schema nv-items))

  (map #(println %) (first nv))
  (map #(println %) (second nv))

  )


(defn transform-to-qcn-struct
  "Transforms  NV item  definition  specified as  parsed Qualcomm  XML
  format into same data structure as used by QCN parser which is given
  as hash table in third argument."
  [nv-definition-schema nv-xml-data qcn-data]
  (let [nv-items (:nv-items nv-xml-data) ;; legacy items
        [nv-items nv-item-errors] (transform-nv-items-to-qcn-struct nv-definition-schema nv-items)
        [prov-item-store nv-item-store] (separate-efs-item-stores nv-xml-data)
        [prov-item-store prov-item-errors] (transform-efs-items-to-qcn-struct
                                            nv-definition-schema prov-item-store)
        [efs-backup-store efs-backup-errors] (transform-efs-items-to-qcn-struct
                                            nv-definition-schema (:rf-items nv-xml-data))
        [nv-item-store nv-item-store-errors] (transform-efs-items-to-qcn-struct
                                              nv-definition-schema nv-item-store)]
    (-> qcn-data
        (assoc-in [:NV_ITEM_ARRAY] nv-items)
        (assoc-in [:NV_Items] nv-item-store)
        (assoc-in [:Provisioning_Item_Files] prov-item-store)
        (assoc-in [:EFS_Backup] efs-backup-store)
        (assoc-in [:errors] (concat (:errors nv-definition-schema) (:errors nv-xml-data)
                                    nv-item-errors prov-item-errors nv-item-store-errors
                                    efs-backup-errors)))))


(comment
  (def nv-definition-schema (parse-nv-definition-file "samples/NvDefinition.xml"))
  (def nv-xml-data (parse-nv-data-file "samples/Masterfile.xml"))

  (def qcn (transform-to-qcn-struct nv-definition-schema nv-xml-data *qcn-default-version*))

  (map #(println %) (:NV_ITEM_ARRAY qcn))
  (map #(println %) (:NV_Items qcn))
  (map #(println %) (:Provisioning_Item_Files qcn))
  (map #(println %) (:Mobile_Property_Info qcn))
  (map #(println %) (:File_Version qcn))
  (map #(println %) (:errors qcn))
  )



(defn parse-nv-data
  "Parse Qualcomm's non-volatile phone item xml definition data set.

   nv-definition-schema: schema data, refer to function parse-nv-definition-file
   nv-data-file-name: name of data xml master file, refer e.g. to samples/Masterfile.xml
   optional: hash map with additional data

   returns nested clojure hash map in same/similar format as generated in qcn_parser.clj:
    :NV_ITEM_ARRAY           -> provides legacy numbered nv item backup data
    :NV_Items                -> provides EFS nv item backup data
    :Provisioning_Item_Files -> provides EFS provisioning item data
    :EFS_Backup              -> provides EFS based RF nv items beginning from 20000
    :errors                  -> contains prarsing errors."

  ([nv-definition-schema  nv-data-file-name]
   (parse-nv-data  nv-definition-schema  nv-data-file-name  {}))

  ([nv-definition-schema  nv-data-file-name  add-on-xml-data]
   (let [nv-xml-data (parse-nv-data-file nv-data-file-name)]
    (transform-to-qcn-struct nv-definition-schema nv-xml-data add-on-xml-data))))



(comment

  (def nv-definition-schema (parse-nv-definition-file "samples/NvDefinition_9640_13bands.xml"))
  (def nv-definition-schema (parse-nv-definition-file "samples/NvDefinition_9640.xml"))
  (def qcn (parse-nv-data nv-definition-schema "samples/UMC-9240P_C2_v0.07_ECE_static.xml"))
  (def qcn (parse-nv-data nv-definition-schema "samples/test.xml"))

  (def qcn (parse-nv-data nv-definition-schema "samples/Masterfile.xml"))


  (println ((:efs-items nv-definition-schema) (keyword "/nv/item_files/rfnv/00028154")))

  (def schema-27830 ((:efs-items nv-definition-schema) (keyword "/nv/item_files/rfnv/00027830")))
  (:variable-size schema-27830)

  (def schema-content-27830 (:content ((:efs-items nv-definition-schema) (keyword "/nv/item_files/rfnv/00027830"))))

  (map #(println %) schema-content-27830)

  (first (:NV_Items  qcn))

  (def qcn_h
    (reduce
     (fn [h entry]
       (let [lfd (key entry)
             item (val entry)
             path (:path item)]
         (assoc h path item)))
     {}
     (:NV_Items  qcn)))


  (def qcn-content-27830 (qcn_h (keyword "/nv/item_files/rfnv/00027830")))

  (def mydata (:data qcn-content-27830))


  (take 50 (bytes mydata))

  )
