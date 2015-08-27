;; Open Radio Calibration Toolkit
;; An enhanced Open Source Implementation to replace Qualcomm's QRCT
;;
;; The use and distribution terms for this software are covered by
;; the GNU General Public License
;;
;; (C) 2015, Otto Linnemann
;;
;; qcn-parser.cljs - writes QCN file, takes clojure'd intermediate data
;; as input


(ns orct.qcn-writer
  (:use [orct.utils]
        [orct.nv-xml])
  (:require [clojure.xml :as xml]
            [clojure.zip :as z]
            [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.set :as set])
  (:import java.nio.ByteBuffer
           [java.io ByteArrayInputStream FileInputStream FileOutputStream]
           [org.apache.poi.poifs.filesystem POIFSFileSystem DirectoryNode DocumentNode
            DocumentInputStream DocumentOutputStream]))

(def *qcn-default-version*
  ^{:doc "defaults for various qcn version data" :dynamic true}
  {:Mobile_Property_Info
   {:efs 0
    :mobile-model-no 0
    :phone-nv-major-rev-no 0
    :phone-nv-minor-rev-no 0
    :phone-sw-version ""
    :qpst-app-version "QMSL 6.0.47"}

   :File_Version
   {:qcn-major-no 2
    :qcn-minor-no 0
    :qcn-rev-no 0}})


(defn- file-version-info-bytes
  "read the file version info tags as byte array"
  [nv-xml-data]
  (let [fv (:File_Version nv-xml-data)]
    (byte-array (mapcat (partial long2byteseq 16) (vals fv)))))


(defn- mobile-properaty-info-bytes
  "read mobile property data as byte array"
  [nv-xml-data]
  (let [pi (:Mobile_Property_Info nv-xml-data)
        {:keys [efs mobile-model-no phone-nv-major-rev-no phone-nv-minor-rev-no
                phone-sw-version qpst-app-version-str-len qpst-app-version]} pi]
    (byte-array
     (concat (long2byteseq 32 efs)
             (long2byteseq 16 mobile-model-no)
             (long2byteseq 8 phone-nv-major-rev-no)
             (long2byteseq 8 phone-nv-minor-rev-no)
             (long2byteseq 16 (count phone-sw-version))
             (map int phone-sw-version)
             (long2byteseq 16 (count qpst-app-version))
             (map int qpst-app-version)))))


(defn- nv-item-array-bytes
  "read nv legacy items as byte array"
  [nv-item-data]
  (let [packets
        (reduce
         (fn [result [item {:keys [name data errors] :as params}]]
           (if data
             (let [item (key2str item)
                   item (last (re-find #"^([ 0]*)(.*)" item)) ;; remove preceeding zeros
                   item (or (edn/read-string item) 0)
                   payload-size 128
                   empty-bytes (- payload-size (alength data))
                   stream-size (+ payload-size 8)
                   packet []
                   packet (into packet (long2byteseq 16 stream-size))
                   packet (into packet (long2byteseq 16 1)) ;; index
                   packet (into packet (long2byteseq 16 item))
                   packet (into packet (long2byteseq 16 0)) ;; padding
                   packet (into packet (vec data))
                   packet (into packet (replicate empty-bytes 0))]
               ;(println item name (count packet))
               (conj result packet))
             result))
         []
         nv-item-data)]
    (byte-array (apply concat packets))))


(comment
  (:File_Version qcn)
  (def r (file-version-info-bytes qcn))
  (def r (mobile-properaty-info-bytes qcn))
  (def r (nv-item-array-bytes (:NV_ITEM_ARRAY qcn)))

  (count (:NV_ITEM_ARRAY qcn))
  (alength r)

  (first (write-efs-items nil nil (:Provisioning_Item_Files qcn)))

  (first (:Provisioning_Item_Files qcn))
  )



(defn- create-root-doc-stream
  [root-fs bytes name]
  (.createDocument root-fs (ByteArrayInputStream. bytes) name))


(defn- create-doc-stream
  [poi-fs bytes name]
  (.createDocument poi-fs name (ByteArrayInputStream. bytes)))


(defn- create-dir
  [poi-fs name]
  (.createDirectory poi-fs name))


(defn- write-efs-items
  [efs-dir data-dir items]
  (dorun (map
          (fn [[item params]]
            (let [index (key2str item)
                  path (key2str (:path params))
                  data (:data params)]
              (create-doc-stream efs-dir (.getBytes path) index)
              (create-doc-stream data-dir data index)))
          items)))


(defn write-qcn-struct-to-poi-fs
  "writes  intermediate  item data  structure  as  parsed by  function
  parse-nv-data to POI filesystem."
  [qcn-struct filename]
  (let [fs (POIFSFileSystem.)
        out (FileOutputStream. filename)
        file-version (create-root-doc-stream fs (file-version-info-bytes qcn-struct) "File_Version")
        model-number (create-dir fs "00000000")
        default (create-dir model-number "default")
        mob-prop (create-doc-stream default (mobile-properaty-info-bytes qcn-struct) "Mobile_Property_Info")

        prov-item-dir (create-dir default "Provisioning_Item_Files")
        prov-item-efs-dir (create-dir prov-item-dir "EFS_Dir")
        prov-item-data-dir (create-dir prov-item-dir "EFS_Data")
        prov-items (write-efs-items
                    prov-item-efs-dir prov-item-data-dir (:Provisioning_Item_Files qcn-struct))


        nv-item-dir (create-dir default "NV_Items")
        nv-item-efs-dir (create-dir nv-item-dir "EFS_Dir")
        nv-item-data-dir (create-dir nv-item-dir "EFS_Data")
        nv-items (write-efs-items
                    nv-item-efs-dir nv-item-data-dir (:NV_Items qcn-struct))

        nv-numbered-items (create-dir default "NV_NUMBERED_ITEMS")
        nv-array (create-doc-stream nv-numbered-items
                                    (nv-item-array-bytes (:NV_ITEM_ARRAY qcn-struct))
                                    "NV_ITEM_ARRAY")]
    (.writeFilesystem fs out)
    (.close out)))


(comment

  (def qcn (parse-nv-data "samples/NvDefinition.xml" "samples/Masterfile.xml" *qcn-default-version*))

  (map #(println %) (:NV_ITEM_ARRAY qcn))
  (map #(println %) (:NV_Items qcn))
  (map #(println %) (:Provisioning_Item_Files qcn))
  (map #(println %) (:Mobile_Property_Info qcn))
  (map #(println %) (:File_Version qcn))
  (map #(println %) (:Errors qcn))

  (first (:Provisioning_Item_Files qcn))
  (first (:NV_ITEM_ARRAY qcn))

  (write-qcn-struct-to-poi-fs qcn "test.qcn")

  )
