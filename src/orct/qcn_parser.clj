(ns orct.qcn-parser
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
  [n bytes] [(drop n bytes) (bytes2little-endian-uint (take n bytes))])

(def rest-uint8-pair (partial rest-uint-n-pair 1))
(def rest-uint16-pair (partial rest-uint-n-pair 2))
(def rest-uint32-pair (partial rest-uint-n-pair 4))

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

(defn- print-next-numbered-item
  "parses in prints out the next numbered NV item from given sequence
   and returns the remaining unprocessed bytes of the input sequence"
  [level byte-buffer]
  (let [[byte-buffer stream-size] (rest-uint16-pair byte-buffer)
        [byte-buffer index] (rest-uint16-pair byte-buffer)
        [byte-buffer item] (rest-uint16-pair byte-buffer)
        [byte-buffer padding] (rest-uint16-pair byte-buffer)
        payload-size (- stream-size 8)
        [byte-buffer payload] [(drop payload-size byte-buffer) (take payload-size byte-buffer)]]
    (if (< payload-size (count payload))
      (throw (IllegalStateException. (format "NV item %d has wrong length error!" item))))
    (println (format "%s item-no %d, index %d ->" (tabs level) item index))
    (print-hex-content (+ 2 level) payload)
    byte-buffer))

(defn- bytes2utf16-str
  "converts given byte sequence in UTF-16 character string
   (without BOM)."
  [bytes]
  (String. bytes))


(defn- print-nv-numbered-items
  "prints all numbered legacy NV items in binary form at given
   tabulator level."
  [level content]
  (println (tabs level) "////// NV NUMBERED ITEM CONTENT ///////")
  (loop [stream content]
  (when (> (count stream) 0)
    (recur (print-next-numbered-item level stream)))))


(defn- print-document-node
  "prints NV documet storage. Currently the following storage
   types are processed: EFS_Dir, EFS_Data, NV_ITEM_ARRAY (legacy)."
  [level node]
  (let [name (.getName node)
        size (.getSize node)
        stream (DocumentInputStream. node)
        content (byte-array (.available stream))
        parent-dir (.getName (.getParent node))]
    (doto stream (.read content) (.close))
    (cond
      (= parent-dir "EFS_Dir") (println (format "%s%s, storage-id:%s"
                                                (tabs level) (bytes2utf16-str content) name))
      (= parent-dir "EFS_Data") (do (println (tabs level) name ", size:" size)
                                    (print-hex-content level content))
      (= name "NV_ITEM_ARRAY") (print-nv-numbered-items level content)
      :else (println (tabs level) "unprocccesed stream " name))))


(defn print-poifs-tree
  "printfs poi filesystem with specified root node
   invocation example:
   (print-poifs-tree (.getRoot (POIFSFileSystem. (FileInputStream. 'input-file.qcn'))))"
  [node]
  (letfn [(printfn [level node]
            (cond
              (= (class node) DirectoryNode)
              (let [s (iterator-seq (.getEntries node))
                    c (.getEntryCount node)
                    path (.getPath node)
                    name (.getName node)]
                (println (tabs level) (str path) (when (> c 0) (str "--> (" c ")")))
                (dorun (map (partial printfn (inc level)) s)))

              (= (class node) DocumentNode)
              (print-document-node level node)
              :else
              (throw (IllegalStateException. (str "class " (class node) " undefined error!")))))]
    (printfn 0 node)))


(defn print-qcn
  "prints content of specified qcn NV item file. This is mainly
   useful for debugging purposes."
  [filename]
  (print-poifs-tree (.getRoot (POIFSFileSystem. (FileInputStream. filename)))))




(comment
  (def qcn-input-stream (FileInputStream. "/Users/ol/Entwicklung/Peiker/nv-parsing/LTE_NAD_SW_QCN/SW_QCN_BC_01_EU_GSM_Dual_WCDMA_1+8_LTE_3+7+20.qcn"))

  (def qcn-input-stream (FileInputStream. "/Users/ol/Entwicklung/Peiker/nv-parsing/LTE_NAD_SW_QCN/SW_QCN_BC_02_NA_ATnT_GSM_Dual_WCDMA_2+5_LTE_2+4+5+12+17_VoLTE.qcn"))

  (def qcn-input-stream (FileInputStream. "/home/ol/Entwicklung/nv_items/SW_QCN_BC_02_NA_ATnT_GSM_Dual_WCDMA_2+5_LTE_2+4+5+12+17_VoLTE.qcn"))


  (def fs (POIFSFileSystem. qcn-input-stream))

  (def node (. fs getRoot))
  (map #(println %) node)
  (print-poifs-tree (.getRoot fs))

  (print-qcn "/Users/ol/Entwicklung/Peiker/nv-parsing/LTE_NAD_SW_QCN/SW_QCN_BC_02_NA_ATnT_GSM_Dual_WCDMA_2+5_LTE_2+4+5+12+17_VoLTE.qcn")

  )
