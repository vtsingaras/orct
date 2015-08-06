(ns orct.qcn-parser
  (:use [orct.macros]
        [orct.nv-xml] ;; only temporary
        )
  (:import java.nio.ByteBuffer java.io.FileInputStream
           [org.apache.poi.poifs.filesystem POIFSFileSystem DirectoryNode DocumentNode
            DocumentInputStream DocumentOutputStream]))

(defn- bytes2little-endian-uint
  "interprets given byte sequence e.g. specified as Java array
   into corresponding little endian unsigned integer representation.
   example: (bytes2little-endian-uint [0x03 0x01]) -> 259"
  [bytes]
  {:pre [(<= (count bytes) 8)]
   :post [(>= % 0)]}
  (let [byte-pos-pairs (partition 2 (interleave bytes (range (count bytes)) ))]
    (reduce (fn [result [next-byte pos]]
              (+ result (bit-shift-left (bit-and next-byte 0xff) (* 8 pos))))
            0 byte-pos-pairs)))

(defn- bytes2little-endian-int
  "interprets given byte sequence e.g. specified as Java array
   into corresponding little endian signed integer representation.
   example: (bytes2little-endian-uint [0x03 0x01]) -> 259"
  [bytes]
  (let [sign (bit-and 0x80 (last bytes))
        ures (bytes2little-endian-uint bytes)]
    (if (= 0 sign)
      ures
      (- ures (bit-shift-left 1 (* 8 (count bytes)))))))


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

(defn bytes2str
  "remove C like '0' termination characters from given string"
  [str]
  (let [s (seq (if (= java.lang.String (class str)) (.getBytes str) str))
        t (loop [s (reverse s)] (if (= (first s) 0) (recur (rest s)) s))]
    (String. (byte-array (reverse t)))))

(defn- rest-str-pair
  "takes n bytes from given sequence and deliver a vector with
  remaining bytes and character string."
  [n bytes]
  [(drop n bytes) (bytes2str (take n bytes))])



(defn decode-binary-nv-params
  "decodes binary parameter bytes sequence accordding to
   vector of schema definitiona entries of the form:
   [{:name <parameter-name> :type <int8, int16, ...>, :size <length in bytes>}, ...].
   The result is stored within the hash table entires under the newly
   added key :val."
  [schema data]
  (let [res (reduce
              (fn [out
                   {:keys [name type size] :as args}]
                (let [params (or (:params out) [])
                      byte-buffer (:byte-buffer out)
                      ;; ATTENTION: sizeOf is not implemented yet
                      op (case type
                           "int8" rest-int8-pair
                           "int16" rest-int16-pair
                           "int32" rest-int32-pair
                           "int64" rest-int64-pair
                           "uint8" rest-uint8-pair
                           "uint16" rest-uint16-pair
                           "uint32" rest-uint32-pair
                           "uint64" rest-uint32-pair
                           "string" rest-str-pair
                           (throw (IllegalStateException. (format "type %s invalid!" type))))
                      [byte-buffer dec-value] (op byte-buffer)]
                  (-> out
                      (assoc :params (conj params (-> args (assoc-in [:val] dec-value))))
                      (assoc :byte-buffer byte-buffer)
                      )))
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

  (map
   #(println %)
   ts)
  )


(defn- read-next-numbered-item
  "parses next numbered NV item from given sequence and returns
   the item as Clojure hash map and remaining unprocessed bytes
   of the input sequence."
  [schema result byte-buffer]
  (let [[byte-buffer stream-size] (rest-uint16-pair byte-buffer)
        [byte-buffer index] (rest-uint16-pair byte-buffer)
        [byte-buffer item] (rest-uint16-pair byte-buffer)
        [byte-buffer padding] (rest-uint16-pair byte-buffer)
        payload-size (- stream-size 8)
        [byte-buffer payload] [(drop payload-size byte-buffer) (take payload-size byte-buffer)]
        item-def ((:nv-items schema) (str item))
        item-name (:name item-def)
        item-schema (:content item-def)]
    (if (< payload-size (count payload))
      (throw (IllegalStateException. (format "NV item %d has wrong length error!" item))))
    (let [result (assoc-in result [item :data] payload)
          result (assoc-in result [item :index] index)
          result (assoc-in result [item :name] item-name)
          result (assoc-in result [item :params] (decode-binary-nv-params item-schema payload))]
      [result byte-buffer])))


(defn- read-nv-numbered-items
  "read all numbered legacy NV items as binary data."
  [schema content]
  (loop [[result stream]  [{} content]]
    (if (> (count stream) 0)
      (recur (read-next-numbered-item schema result stream))
      result)))

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
  (let [name (.getName node)
        size (.getSize node)
        stream (DocumentInputStream. node)
        content (byte-array (.available stream))
        parent-dir (.getName (.getParent node))]
    (doto stream (.read content) (.close))
    (cond
      (= parent-dir "EFS_Dir")
      (let [path (bytes2str content)
            type (.getName (.getParent (.getParent node)))]
        (def content content)
        (assoc-in result [type name :path] path))

      (= parent-dir "EFS_Data")
      (let [type (.getName (.getParent (.getParent node)))
            nv-type-store (or (result type) {})
            nv (nv-type-store name)
            path (:path nv)
            path-schema ((:efs-items schema) path)
            item-schema (:content path-schema)
            result (assoc-in result [type name :data] content)
            result (assoc-in result [type name :params] (decode-binary-nv-params item-schema content))]
        (when-not path (throw (IllegalStateException. (format "EFS_Dir missing for nv %s,%s" type name))))
        result)

      (= name "NV_ITEM_ARRAY")
      (assoc-in result [name] (read-nv-numbered-items schema content))

      (= name "Mobile_Property_Info")
      (assoc-in result [name] (read-mobile-property-info content))

      (= name "File_Version")
      (assoc-in result [name] (read-file-version-info content))

      :else (assoc-in result [:unprocessed name] content))))

(comment

  type
  nv-type-store
  nv
  my-path
  (println path)
  (.getBytes (str (last path)))
  (count (.getBytes path))
  (partition 2 (.getBytes path))

  (map #(println %)  (.getBytes path))
  (map #(println %)  content)

  (bytes2str content)

  (map (fn [[l h]] (println (format "%x %x" h l)))  (partition 2 (.getBytes path)))



  (map #(println %)  (.getBytes (str (last path))))

  ((:efs-items nv-definition-schema) path)
  ((:efs-items nv-definition-schema) "/nv/item_files/ims/qipcall_is_conf_server_refer_recipient")

  )



(defn read-poifs-tree
  "reads poi filesystem with specified root node and returns result as
   Clojure nested data structure.

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




(defn- tabs
  "generates a string of specified numer of blanks used as tabulating characters"
  [level]
  (apply str (repeat (* 2 level) " ")))

(defn- print-hex-content
  "print binary content with given tabulating level"
  [level content]
  (let [max-columns 16
        max-elements 128
        given-elements (count content)
        content (take max-elements content)
        colums (partition-all max-columns content)
        hex-line
        (fn [x] (apply str (map #(format "%2x " %) x)))]
    (dorun (map #(println (tabs level) (hex-line %)) colums))
    (when (> given-elements max-elements) (println (tabs level) " ..."))))


(defn- print-legacy-items
  [items]
  (dorun (map (fn [[item {:keys [index data name params]}]]
                (println (format "%sitem:%s, index:%s, name:%s" (tabs 3) item index name))
                (dorun (map (fn [{:keys [name val]}]
                              (println (format "%s%s -> %s" (tabs 6) name val))) params))
                ;(print-hex-content 6 data)
                ) items)))

(defn- print-efs-items
  [items]
  (dorun (map (fn [[item {:keys [path data params]}]]
                (println (format "%spath:%s" (tabs 3) path))
                (dorun (map (fn [{:keys [name val]}]
                              (println (format "%s%s -> %s" (tabs 6) name val))) params))
                ;(print-hex-content 6 data)
                ) items)))

(defn- print-mobile-property-info
  [prop]
  (let [{:keys [mobile-model-no phone-sw-version phone-nv-minor-rev-no
                phone-nv-major-rev-no qpst-app-version]} prop
                tabs (tabs 6)]
    (println (format "%smobile phone number: %s" tabs mobile-model-no))
    (println (format "%smobile sw version: %s" tabs phone-sw-version))
    (println (format "%sphone nv version number: %s.%s" tabs phone-nv-major-rev-no phone-nv-minor-rev-no))
    (println (format "%sqpst app version: %s" tabs qpst-app-version))))

(defn- print-file-version-info
  [info]
  (let [{:keys [qcn-rev-no qcn-minor-no qcn-major-no]} info
        tabs (tabs 6)]
    (println (format "%sqcn-rev-number:%s, qcn-version-number:%s.%s"
                     tabs qcn-rev-no qcn-major-no qcn-minor-no))))

(defn print-nv-item-set
  [nv]
  (let [efs-backup-items (nv "NV_Items")
        prov-items (nv "Provisioning_Item_Files")
        backup-items (nv "NV_ITEM_ARRAY")
        mobile-properties (nv "Mobile_Property_Info")
        file-version (nv "File_Version")]
    (println ">>>>> File Info >>>>>")
    (print-file-version-info file-version)
    (println ">>>>> Mobile Property Info >>>>>")
    (print-mobile-property-info mobile-properties)
    (println ">>>>> Item File Backup >>>>>")
    (print-legacy-items backup-items)
    (println ">>>>> EFS Item Backup >>>>>")
    (print-efs-items efs-backup-items)
    (println ">>>>> Provisioning Item Files >>>>>")
    (print-efs-items prov-items)))


(defn print-qcn
  "prints content of specified qcn NV item file. This is mainly
   useful for debugging purposes."
  [schema filename]
  (print-nv-item-set
   (read-poifs-tree schema (.getRoot (POIFSFileSystem. (FileInputStream. filename))))))



(comment
  (def qcn-input-stream (FileInputStream. "/Users/ol/Entwicklung/Peiker/nv-parsing/LTE_NAD_SW_QCN/SW_QCN_BC_01_EU_GSM_Dual_WCDMA_1+8_LTE_3+7+20.qcn"))

  (def qcn-input-stream (FileInputStream. "/Users/ol/Entwicklung/Peiker/nv-parsing/LTE_NAD_SW_QCN/SW_QCN_BC_02_NA_ATnT_GSM_Dual_WCDMA_2+5_LTE_2+4+5+12+17_VoLTE.qcn"))

  (def qcn-input-stream (FileInputStream. "/Users/ol/Entwicklung/Peiker/nv-parsing/LTE_NAD_SW_QCN/SW_QCN_BC_02_NA_ATnT_GSM_Dual_WCDMA_2+5_LTE_2+4+5+12+17_VoLTE.qcn"))

  (def qcn-input-stream (FileInputStream. "/home/ol/Entwicklung/nv_items/SW_QCN_BC_02_NA_ATnT_GSM_Dual_WCDMA_2+5_LTE_2+4+5+12+17_VoLTE.qcn"))


  (def fs (POIFSFileSystem. qcn-input-stream))

  (def node (. fs getRoot))


  (def x (read-poifs-tree nv-definition-schema (.getRoot fs)))

  (println x)

  (println (keys x))
  (println (x "NV_Items"))
  (println (x "Provisioning_Item_Files"))
  (println (x "NV_ITEM_ARRAY"))
  (println (x "Mobile_Property_Info"))
  (println (x "File_Version"))


  (print-nv-item-set x)

  (count nv-definition-schema)

  (print-qcn nv-definition-schema
             "/Users/ol/Entwicklung/Peiker/nv-parsing/LTE_NAD_SW_QCN/SW_QCN_BC_02_NA_ATnT_GSM_Dual_WCDMA_2+5_LTE_2+4+5+12+17_VoLTE.qcn")

  (print-qcn nv-definition-schema
             "/home/ol/Entwicklung/nv_items/SW_QCN_BC_02_NA_ATnT_GSM_Dual_WCDMA_2+5_LTE_2+4+5+12+17_VoLTE.qcn")
  )
