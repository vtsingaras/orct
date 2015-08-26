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
             (map int qpst-app-version)
             ))))


(comment
  (:File_Version qcn)
  (def r (file-version-info-bytes qcn))
  (def r (mobile-properaty-info-bytes qcn))
  (vec r)
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


(defn write-qcn-struct-to-poi-fs
  [qcn-struct filename]

  (let [fs (POIFSFileSystem.)
        out (FileOutputStream. filename)
        file-version (create-root-doc-stream fs (file-version-info-bytes qcn-struct) "File_Version")
        model-number (create-dir fs "00000000")
        default (create-dir model-number "default")
        mob-prop (create-doc-stream default (mobile-properaty-info-bytes qcn-struct) "Mobile_Property_Info")
        ]
    (def default default)
    (def fs fs)
    (.writeFilesystem fs out)))


(comment

  (def qcn (parse-nv-data "samples/NvDefinition.xml" "samples/Masterfile.xml" *qcn-default-version*))

  (map #(println %) (:NV_ITEM_ARRAY qcn))
  (map #(println %) (:NV_Items qcn))
  (map #(println %) (:Provisioning_Item_Files qcn))
  (map #(println %) (:Mobile_Property_Info qcn))
  (map #(println %) (:File_Version qcn))
  (map #(println %) (:Errors qcn))

  (write-qcn-struct-to-poi-fs qcn "test.qcn" )

  )
