(ns orct.qcn-parser
  (:use [orct.macros])
  (:import java.nio.ByteBuffer java.io.FileInputStream
           [org.apache.poi.poifs.filesystem POIFSFileSystem DirectoryNode DocumentNode
            DocumentInputStream DocumentOutputStream]))

(defn- bytes2little-endian-uint
  "interprets given byte sequence e.g. specified as Java array
   into corresponding little endian unsinged integer representation.
   example: (bytes2little-endian-uint [0x03 0x01]) -> 259"
  [bytes]
  {:pre [(<= (count bytes) 8)]
   :post [(>= % 0)]}
  (let [byte-pos-pairs (partition 2 (interleave bytes (range (count bytes)) ))]
    (reduce (fn [result [next-byte pos]]
              (+ result (bit-shift-left (bit-and next-byte 0xff) (* 8 pos))))
            0 byte-pos-pairs)))

(defn- rest-uint-n-pair
  "take n bytes from given sequence and delivers vector with
   remaining bytes and unsigned integer representation.
   example (rest-uint-n-pair 2 [0x03 0x01 0xFA 0xFB]) -> [(0xFA 0xFB) 259 ]"
  [n bytes]
  [(drop n bytes) (bytes2little-endian-uint (take n bytes))])

(def rest-uint8-pair (partial rest-uint-n-pair 1))
(def rest-uint16-pair (partial rest-uint-n-pair 2))
(def rest-uint32-pair (partial rest-uint-n-pair 4))

(defn- bytes2utf16-str
  "converts given byte sequence in UTF-16 character string
   (without BOM)."
  [bytes]
  (String. (byte-array bytes)))

(defn- rest-utf16-pair
  "takes n bytes from given sequence and deliver a vector with
  remaining bytes and UTF-16 character string."
  [n bytes]
  [(drop n bytes) (bytes2utf16-str (take n bytes))])


(defn- read-next-numbered-item
  "parses next numbered NV item from given sequence and returns
   the item as Clojure hash map and remaining unprocessed bytes
   of the input sequence."
  [result byte-buffer]
  (let [[byte-buffer stream-size] (rest-uint16-pair byte-buffer)
        [byte-buffer index] (rest-uint16-pair byte-buffer)
        [byte-buffer item] (rest-uint16-pair byte-buffer)
        [byte-buffer padding] (rest-uint16-pair byte-buffer)
        payload-size (- stream-size 8)
        [byte-buffer payload] [(drop payload-size byte-buffer) (take payload-size byte-buffer)]]
    (if (< payload-size (count payload))
      (throw (IllegalStateException. (format "NV item %d has wrong length error!" item))))
    (let [result (assoc-in result [item :data] payload)
          result (assoc-in result [item :index] index)]
      [result byte-buffer])))

(defn- read-nv-numbered-items
  "read all numbered legacy NV items as binary data."
  [content]
  (loop [[result stream]  [{} content]]
    (if (> (count stream) 0)
      (recur (read-next-numbered-item result stream))
      result)))

(defn- read-mobile-property-info
  "read the mobile property info tags"
  [byte-buffer]
  (let [[byte-buffer efs] (rest-uint32-pair byte-buffer)
        [byte-buffer mobile-model-no] (rest-uint16-pair byte-buffer)
        [byte-buffer phone-nv-major-rev-no] (rest-uint8-pair byte-buffer)
        [byte-buffer phone-nv-minor-rev-no] (rest-uint8-pair byte-buffer)
        [byte-buffer phone-sw-version-len] (rest-uint16-pair byte-buffer)
        [byte-buffer phone-sw-version] (rest-utf16-pair phone-sw-version-len byte-buffer)
        [byte-buffer qpst-app-version-str-len] (rest-uint16-pair byte-buffer)
        [byte-buffer qpst-app-version] (rest-utf16-pair qpst-app-version-str-len byte-buffer)]
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
  [*result* node]
  (let [name (.getName node)
        size (.getSize node)
        stream (DocumentInputStream. node)
        content (byte-array (.available stream))
        parent-dir (.getName (.getParent node))]
    (doto stream (.read content) (.close))
    (cond
      (= parent-dir "EFS_Dir")
      (let [path (bytes2utf16-str content)
            type (.getName (.getParent (.getParent node)))]
        (swap! *result* assoc-in [type name :path] path)
        )

      (= parent-dir "EFS_Data")
      (let [type (.getName (.getParent (.getParent node)))]
        (swap! *result* assoc-in [type name :data] content)
        )

      (= name "NV_ITEM_ARRAY")
      (swap! *result* assoc-in [name] (read-nv-numbered-items content))

      (= name "Mobile_Property_Info")
      (swap! *result* assoc-in [name] (read-mobile-property-info content))

      (= name "File_Version")
      (swap! *result* assoc-in [name] (read-file-version-info content))

      :else (swap! *result* assoc-in [:unprocessed name] content))))

(defn read-poifs-tree
  "reads poi filesystem with specified root node and returns result as
   Clojure nested data structure.

  invocation example:
  (read-poifs-tree (.getRoot (POIFSFileSystem. (FileInputStream. 'input-file.qcn'))))"
  [node]
  (let [*result* (atom {})]
    (letfn [(readfn [node]
              (cond
                (= (class node) DirectoryNode)
                (let [s (iterator-seq (.getEntries node))
                      c (.getEntryCount node)
                      path (.getPath node)
                      name (.getName node)]
                  (dorun (map readfn s)))

                (= (class node) DocumentNode)
                (dorun (read-document-node *result* node))
                :else
                (throw (IllegalStateException. (str "class " (class node) " undefined error!")))))]
      (readfn node)
      @*result*)))


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
  (dorun (map (fn [[item {:keys [index data]}]]
                (println (format "%sitem:%s, index:%s" (tabs 3) item index))
                (print-hex-content 6 data)) items)))

(defn- print-efs-items
  [items]
  (dorun (map (fn [[item {:keys [path data]}]]
                (println (format "%spath:%s" (tabs 3) path))
                (print-hex-content 6 data)) items)))

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
  [filename]
  (print-nv-item-set
   (read-poifs-tree (.getRoot (POIFSFileSystem. (FileInputStream. filename))))))



(comment
  (def qcn-input-stream (FileInputStream. "/Users/ol/Entwicklung/Peiker/nv-parsing/LTE_NAD_SW_QCN/SW_QCN_BC_01_EU_GSM_Dual_WCDMA_1+8_LTE_3+7+20.qcn"))

  (def qcn-input-stream (FileInputStream. "/Users/ol/Entwicklung/Peiker/nv-parsing/LTE_NAD_SW_QCN/SW_QCN_BC_02_NA_ATnT_GSM_Dual_WCDMA_2+5_LTE_2+4+5+12+17_VoLTE.qcn"))

  (def qcn-input-stream (FileInputStream. "/Users/ol/Entwicklung/Peiker/nv-parsing/LTE_NAD_SW_QCN/SW_QCN_BC_02_NA_ATnT_GSM_Dual_WCDMA_2+5_LTE_2+4+5+12+17_VoLTE.qcn"))


  (def fs (POIFSFileSystem. qcn-input-stream))

  (def node (. fs getRoot))


  (def x (read-poifs-tree (.getRoot fs)))

  (println x)
  (println (keys x))
  (println (x "NV_Items"))
  (println (x "Provisioning_Item_Files"))
  (println (x "NV_ITEM_ARRAY"))
  (println (x "Mobile_Property_Info"))
  (println (x "File_Version"))


  (print-nv-item-set x)


  (print-qcn "/Users/ol/Entwicklung/Peiker/nv-parsing/LTE_NAD_SW_QCN/SW_QCN_BC_02_NA_ATnT_GSM_Dual_WCDMA_2+5_LTE_2+4+5+12+17_VoLTE.qcn")

  )
