;; Open Radio Calibration Toolkit
;; An enhanced Open Source Implementation to replace Qualcomm's QRCT
;;
;; The use and distribution terms for this software are covered by
;; the GNU General Public License
;;
;; (C) 2015, Otto Linnemann
;;
;; qcn-parser.cljs - parser for QCN file format

(ns orct.qcn-parser
  (:use [orct.macros]
        [orct.utils]
        [orct.nv-xml])
  (:require [clojure.edn :as edn]
            [clojure.string :as str])
  (:import java.nio.ByteBuffer java.io.FileInputStream
           [org.apache.poi.poifs.filesystem POIFSFileSystem DirectoryNode DocumentNode
            DocumentInputStream DocumentOutputStream]))

(defn- rest-uint-n-pair
  "take n bytes from given sequence and delivers vector with
   remaining bytes and unsigned integer representation.
   example (rest-uint-n-pair 2 [0x03 0x01 0xFA 0xFB]) -> [(0xFA 0xFB) 259 ]"
  [n bytes]
  [(drop n bytes) (bytes2little-endian-uint (take n bytes))])

(def rest-uint8-pair (partial rest-uint-n-pair 1))
(def rest-uint16-pair (partial rest-uint-n-pair 2))
(def rest-uint32-pair (partial rest-uint-n-pair 4))
(def rest-uint64-pair (partial rest-uint-n-pair 8))


(defn- rest-int-n-pair
  "take n bytes from given sequence and delivers vector with
   remaining bytes and signed integer representation.
   example (rest-uint-n-pair 2 [0x03 0x01 0xFA 0xFB]) -> [(0xFA 0xFB) 259 ]"
  [n bytes]
  [(drop n bytes) (bytes2little-endian-int (take n bytes))])

(def rest-int8-pair (partial rest-int-n-pair 1))
(def rest-int16-pair (partial rest-int-n-pair 2))
(def rest-int32-pair (partial rest-int-n-pair 4))
(def rest-int64-pair (partial rest-int-n-pair 8))


(defn- rest-str-pair
  "takes n bytes from given sequence and deliver a vector with
  remaining bytes and character string."
  [n bytes]
  [(drop n bytes) (bytes2str (take n bytes))])

(defn- repeat-ops
  "execute operation op n times with given byte-buffer.
   returns vector of first element with remaining bytes and
   the second element the actual result."
  [op n byte-buffer]
  (loop [result []  byte-buffer (seq byte-buffer)  remain n]
    (if (> remain 0)
      (let [[byte-buffer r] (op byte-buffer)]
          (recur (conj result r) byte-buffer (dec remain)))
      [byte-buffer result])))


(defn- is-byte-array-str?
  [a]
  (let [s (.getBytes (bytes2str a))]
    (and (> (count s) 2) (every? #(and (> % 31) (< % 128)) s))))


(def rest-uint8-or-str-pair
  (with-meta
    (fn [n byte-buffer]
      (if (is-byte-array-str? (take n byte-buffer))
        [(drop n byte-buffer) [(bytes2str (take n byte-buffer))]]
        (repeat-ops rest-uint8-pair n byte-buffer)))
    {:dont-repeat true}))


(defn decode-binary-nv-params
  "decodes binary parameter bytes sequence accordding to
   vector of schema definitiona entries of the form:
   [{:name <parameter-name> :type <int8, int16, ...>, :size <length in bytes>}, ...].
   The result is stored within the hash table entires under the newly
   added key :val."
  [schema data & {:keys [flag]}]
  (let [res (reduce
              (fn [out
                   {:keys [name type size] :as args}]
                (let [params (or (:params out) [])
                      byte-buffer (:byte-buffer out)
                      op (case type
                           "int8" rest-int8-pair
                           "int16" rest-int16-pair
                           "int32" rest-int32-pair
                           "int64" rest-int64-pair
                           "uint8" rest-uint8-or-str-pair
                           "uint16" rest-uint16-pair
                           "uint32" rest-uint32-pair
                           "uint64" rest-uint64-pair
                           "string" rest-str-pair
                           (throw (IllegalStateException. (format "type %s invalid!" type))))
                      [byte-buffer dec-value] (if (:dont-repeat (meta op))
                                                (op size byte-buffer)
                                                (repeat-ops op size byte-buffer))]
                  (-> out
                      (assoc :params (conj params (-> args (assoc-in [:val] dec-value))))
                      (assoc :byte-buffer byte-buffer))))
              {:byte-buffer data}
              schema)]
    (:params res)))


(comment "usage illustration"
  (def ts
    (vector
     {:name "G1_G0_fall" :type "int32" :size 1}
     {:name "G0_G1_rise" :type "int16" :size 1}
     {:name "G2_G1_fall" :type "int16" :size 1}
     {:name "G1_G2_rise" :type "int16" :size 1}
     {:name "G3_G2_fall" :type "int16" :size 1}
     {:name "G2_G3_rise" :type "int16" :size 1}
     {:name "fall_time_hyst" :type "int16" :size 1}))

  (def tb (take 20 (interleave (iterate inc 0) (repeat 0))))

  (map
   #(println %)
   (decode-binary-nv-params ts tb))

  (map #(println %) ts)
  )


(defn- read-next-numbered-item
  "parse next numbered NV item from given sequence and returns
   the item as Clojure hash map and remaining unprocessed bytes
   of the input sequence."
  [schema result byte-buffer errors]
  (let [orig-byte-buffer byte-buffer
        [byte-buffer stream-size] (rest-uint16-pair byte-buffer)
        [byte-buffer index] (rest-uint16-pair byte-buffer)
        [byte-buffer item] (rest-uint16-pair byte-buffer)
        item (keyword (str item))
        [byte-buffer padding] (rest-uint16-pair byte-buffer)
        payload-size (- stream-size 8)
        [payload byte-buffer] (split-at payload-size byte-buffer)
        item-def (-> schema :nv-items item)
        item-name (:name item-def)
        item-schema (:content item-def)]
    ;(println "======================= " item item-name index " =======================")
    ;(println (take 40 (vec orig-byte-buffer)))
    (if (< payload-size (count payload))
      (throw (IllegalStateException. (format "NV item %d has wrong length error!" item))))
    (let [result (assoc-in result [item :data] payload)
          result (assoc-in result [item :index] index)
          result (assoc-in result [item :name] item-name)
          result (assoc-in result [item :params] (decode-binary-nv-params item-schema payload))
          errors (if-not item-schema
                   (conj errors (format "missing schema definition for legacy nv item %s"
                                        (key2str item)))
                   errors)]
      [result byte-buffer errors])))



(defn- read-nv-numbered-items
  "read all numbered legacy NV items as binary data."
  [schema content]
  (loop [[result stream errors]  [(sorted-map) content []]]
    (if (> (count stream) 0)
      (recur (read-next-numbered-item schema result stream errors))
      [result errors])))


(defn- read-mobile-property-info
  "read the mobile property info tags"
  [byte-buffer]
  (let [[byte-buffer efs] (rest-uint32-pair byte-buffer)
        [byte-buffer mobile-model-no] (rest-uint16-pair byte-buffer)
        [byte-buffer phone-nv-major-rev-no] (rest-uint8-pair byte-buffer)
        [byte-buffer phone-nv-minor-rev-no] (rest-uint8-pair byte-buffer)
        [byte-buffer phone-sw-version-len] (rest-uint16-pair byte-buffer)
        [byte-buffer phone-sw-version] (rest-str-pair phone-sw-version-len byte-buffer)
        [byte-buffer qpst-app-version-str-len] (rest-uint16-pair byte-buffer)
        [byte-buffer qpst-app-version] (rest-str-pair qpst-app-version-str-len byte-buffer)]
    (hash-args efs mobile-model-no phone-nv-major-rev-no phone-nv-minor-rev-no
               phone-sw-version qpst-app-version)))


(defn- read-file-version-info
  "read the file version info tags"
  [byte-buffer]
  (let [[byte-buffer qcn-major-no] (rest-uint16-pair byte-buffer)
        [byte-buffer qcn-minor-no] (rest-uint16-pair byte-buffer)
        [byte-buffer qcn-rev-no] (rest-uint16-pair byte-buffer)]
    (hash-args qcn-major-no qcn-minor-no qcn-rev-no)))


(defn- read-document-node
  "read NV documet storage. Currently the following storage
  types are processed: EFS_Dir, EFS_Data, NV_ITEM_ARRAY (legacy)
  Mobile_Property_Info File_Version"
  [schema result node]
  (let [name (keyword (.getName node))
        size (.getSize node)
        stream (DocumentInputStream. node)
        content (byte-array (.available stream))
        parent-dir (.getName (.getParent node))
        errors (or (:errors result) [])]
    (doto stream (.read content) (.close))
    (cond
      (= parent-dir "EFS_Dir")
      (let [path (keyword (bytes2str content))
            type (keyword (.getName (.getParent (.getParent node))))]
        (assoc-in result [type name :path] path))

      (= parent-dir "EFS_Data")
      (let [type (keyword (.getName (.getParent (.getParent node))))]
        (-> result (assoc-in [type name :data] content)))

      (= name :NV_ITEM_ARRAY)
      (let [[num-items num-errors] (read-nv-numbered-items schema content)
            errors (keep identity (concat errors num-errors))]
        (def c content)
        (-> result
            (assoc-in [name] num-items)
            (assoc-in [:errors] errors)))

      (= name :Mobile_Property_Info)
      (assoc-in result [name] (read-mobile-property-info content))

      (= name :File_Version)
      (assoc-in result [name] (read-file-version-info content))

      :else (assoc-in result [:unprocessed name] content))))


(defn read-poifs-tree
  "first stage  parsing of  poi filesystem  with specified  root node,
  returns result as Clojure nested data structure.

  invocation example:
  (read-poifs-tree nv-definiton schema
      (.getRoot (POIFSFileSystem. (FileInputStream. 'input-file.qcn'))))"
  [schema node]
  (letfn [(readfn [result node]
            (cond
              (= (class node) DirectoryNode)
              (let [s (iterator-seq (.getEntries node))
                    c (.getEntryCount node)
                    path (.getPath node)
                    name (.getName node)]
                (reduce (fn [res node] (readfn res node)) result s))

              (= (class node) DocumentNode)
              (read-document-node schema result node)
              :else
              (throw (IllegalStateException. (str "class " (class node) " undefined error!")))))]
    (readfn {} node)))


(defn- parse-nv-efs-data
  "parse efs-data in a second step"
  [schema efs-data]
  (reduce
   (fn [[result errors] elem]
     (let [key (key elem)
           values (val elem)
           path (:path values)
           data (:data values)
           item-schema (-> schema :efs-items path :content)
           [params error] (if item-schema
                             [(decode-binary-nv-params item-schema data) []]
                             [{} [(format "missing schema definition for nv item path %s"
                                           (key2str path))]])
           update (-> values
                      (assoc :params params)
                      (assoc :errors error))]
       [(assoc result key update) (concat errors error)]))
   [{#_result} [#_errors]]
   efs-data))


(defn subst-with-parsed-nv-efs-data
  "parses/substitute values for efs-items :NV_Items, :Provisioning_Item_Files

   This  can   be  useful  to   get  coninstent  and   enhanced  ASCII
  representation  in :val  fields  e.g. for  more  readible output  of
  character strings such as URL addresses.

   Parameters:
     schema: schema definition structure (parse-nv-definition-file)
     nv:     clojure's nv-item representation, result of functions:
             (read-poifs-tree-first-stage) (parse-nv-data)

   Returns: nv-item representation with asciified value keys."

  [schema nv]
  (let [errors (:errors nv)
        [nv-items errors1] (parse-nv-efs-data schema (nv :NV_Items))
        [prov-items errors2] (parse-nv-efs-data schema (nv :Provisioning_Item_Files))]
    (-> nv
        (assoc :NV_Items nv-items)
        (assoc :Provisioning_Item_Files prov-items)
        (assoc :errors (concat errors errors1 errors2)))))


(defn read-poifs-tree-and-parse-nv-efs-data
  "reads poi filesystem with specified root node and returns result as
   Clojure nested data structure. This convinient function includes

  invocation example:
  (read-poifs-tree nv-definiton schema
      (.getRoot (POIFSFileSystem. (FileInputStream. 'input-file.qcn'))))"
  [schema node]
  (subst-with-parsed-nv-efs-data
   schema
   (read-poifs-tree schema node)))


(defn read-poifs-tree-debug
  "reads poi filesystem with specified root node and returns result as
   Clojure nested data structure.

  invocation example:
  (read-poifs-tree nv-definiton schema
      (.getRoot (POIFSFileSystem. (FileInputStream. 'input-file.qcn'))))"
  [schema node]
  (letfn [(readfn [result node level]
            (cond
              (= (class node) DirectoryNode)
              (let [s (iterator-seq (.getEntries node))
                    c (.getEntryCount node)
                    path (.getPath node)
                    name (.getName node)]
                (println (format "%sDirectoryNode -> path:%s, name:%s, description:%s"
                                 (tabs level) path name (.getShortDescription node)))
                (println (tabs level) "======================")
                (reduce (fn [res node] (readfn res node (inc level))) result s))

              (= (class node) DocumentNode)
              (do
                (println (format "%sDocumentNode -> name:%s, description:%s"
                                 (tabs level) (.getName node) (.getShortDescription node)))
                (read-document-node schema result node))
              :else
              (throw (IllegalStateException. (str "class " (class node) " undefined error!")))))]
    (readfn {} node 0)))


(defn parse-qcn-data
  "Parse Qualcomm's non-volatile phone item qcn definition data set.

   nv-definition-schema: parsed schema xml file, refer e.g. to parse-nv-definition-file
   qcn-data-file-name:  name of data qcn file, refer e.g. to samples/sample.qcn

   returns nested clojure hash map in same/similar format as generated in qcn_parser.clj:
    :NV_ITEM_ARRAY           -> provides legacy numbered nv item backup data
    :NV_Items                -> provides EFS nv item backup data
    :Provisioning_Item_Files -> provides EFS provisioning item data
    :errors                  -> contains prarsing errors."

  [nv-definition-schema  qcn-data-file-name]
  (let [qcn-input-stream (FileInputStream. qcn-data-file-name)
        fs (POIFSFileSystem. qcn-input-stream)]
    (read-poifs-tree nv-definition-schema (.getRoot fs))))



(comment "usage illustration"
  (def nv-definition-schema (parse-nv-definition-file "samples/NvDefinition.xml"))
  (def qcn-input-stream (FileInputStream. "samples/sample.qcn"))
  (def qcn-input-stream (FileInputStream. "samples/nv-item-setup-wlan-board-complete-2015-08-14.qcn"))
  (def qcn-input-stream (FileInputStream. "test.qcn"))
  (def fs (POIFSFileSystem. qcn-input-stream))
  (def nv (read-poifs-tree nv-definition-schema (.getRoot fs)))

  (println "ref   \n==============\n")
  (println "check \n==============\n")
  (read-poifs-tree-debug nv-definition-schema (.getRoot fs))

  (println nv)
  (println (keys nv))
  (println (nv :NV_Items))
  (println (nv :Provisioning_Item_Files))
  (println (nv :NV_ITEM_ARRAY))
  (println (nv :Mobile_Property_Info))
  (println (nv :File_Version))

  (keys (nv :NV_Items))

  (println (-> nv :Provisioning_Item_Files))
  (println (-> nv :Provisioning_Item_Files :00000019)) ;; qp_ims_dpl_config
  (println (-> nv :Provisioning_Item_Files :0000001F)) ;; qp_ims_ut_config

  (println (-> nv :NV_ITEM_ARRAY :1014)) ;; NV_SMS_GW_CB_SERVICE_TABLE_SIZE_I
  )
