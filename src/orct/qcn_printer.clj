;; Open Radio Calibration Toolkit
;; An enhanced Open Source Implementation to replace Qualcomm's QRCT
;;
;; The use and distribution terms for this software are covered by
;; the GNU General Public License
;;
;; (C) 2015, Otto Linnemann
;;
;; qcn-printer.cljs - various printing function for qcn data

(ns orct.qcn-printer
  (:use [orct.macros]
        [orct.utils]
        [orct.nv-xml]
        [orct.qcn-parser])
  (:require [clojure.edn :as edn]
            [clojure.string :as str])
  (:import java.nio.ByteBuffer java.io.FileInputStream))


(defn- print-val-seq
  "print sequence over multiple lines with given tabulating level"
  [level content format-str & {:keys [max-columns max-elements] :or {max-columns 16
                                                                     max-elements 128}}]
  (let [given-elements (count content)
        content (take max-elements content)
        colums (partition-all max-columns content)
        hex-line
        (if (number? (first content))
          (fn [x] (apply str (map #(format format-str %) x)))
          (fn [x] (apply str (map #(format "%5s " %) x))))]
    (dorun (map #(println (tabs level) (hex-line %)) colums))
    (when (> given-elements max-elements) (println (tabs level) " ..."))))


(defn print-hex-content
  "print binary content in hex format with given tabulating level"
  [level content & args]
  (apply print-val-seq (concat [level content "%2x "] args)))


(defn print-dec-content
  "print binary content in hex format with given tabulating level"
  [level content & args]
  (apply print-val-seq (concat [level content "%3d "] args)))


(defn- print-legacy-items
  [items]
  (dorun (map (fn [[item {:keys [index data name params]}]]
                (println (format "%sitem%s, index:%s, name:%s" (tabs 3) item index name))
                (if params
                  (dorun (map (fn [{:keys [name val]}]
                                (print (format "%s%s -> " (tabs 6) name))
                                (if (<= (count val) 16)
                                  (if (= (count val) 1) (println (first val)) (println val))
                                  (do (println) (print-dec-content 6 val)))) params))
                  (do
                    (println (tabs 6) "-- missing Schema for this parameter! --")
                    (print-hex-content 6 data)))) items)))


(defn- print-efs-items
  [items]
  (dorun (map (fn [[item {:keys [path data params]}]]
                (println (format "%spath%s" (tabs 3) path))
                (if params
                  (dorun (map (fn [{:keys [name val]}]
                                (print (format "%s%s -> " (tabs 6) name))
                                (if (<= (count val) 16)
                                  (if (= (count val) 1) (println (first val)) (println val))
                                  (do (println) (print-dec-content 6 val)))) params))
                  (do
                    (println (tabs 6) "-- missing Schema for this parameter! --")
                    (print-hex-content 6 data)))) items)))


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


(defn- get-sorted-legacy-items
  [nv]
  (let [itemkey2number #(-> % key2str remove-preceding-zeros edn/read-string)
        leg-item-predicate
        (fn [a b]
          (< (itemkey2number a) (itemkey2number b)))]
    (into (sorted-map-by leg-item-predicate) (nv :NV_ITEM_ARRAY))))


(defn- get-sorted-efs-items
  [efs-items]
  (let [get-lc-path #(str/lower-case (-> efs-items % :path))
        efs-item-predicate
        (fn [a b]
          (compare (get-lc-path a) (get-lc-path b)))]
    (into (sorted-map-by efs-item-predicate) efs-items)))


(defn print-nv-parser-errors
  "printfs parser errors
   takes parsed qcn or xml nv data definition as argument"
  [{:keys [errors]}]
  (when-not (empty? errors)
      (do
        (println-err "\n==================== ERRORS ====================")
        (dorun (map #(println-err %) errors))
        (println-err))))


(defn print-nv-item-set

  "prints content of specified qcn data. Function takes
   two arguments:

   schema : parsed nv defintion schema file (parse-nv-definition-file)
   nv     : parsed qcn or xml nv data definition."

  [schema nv]
  (let [nv (subst-with-parsed-nv-efs-data schema nv)]
    (println ">>>>> File Info >>>>>")
    (print-file-version-info (nv :File_Version))
    (println ">>>>> Mobile Property Info >>>>>")
    (print-mobile-property-info (nv :Mobile_Property_Info))
    (println ">>>>> Item File Backup >>>>>")
    (print-legacy-items (get-sorted-legacy-items nv))
    (println ">>>>> EFS Item Backup >>>>>")
    (print-efs-items (get-sorted-efs-items (nv :NV_Items)))
    (println ">>>>> Provisioning Item Files >>>>>")
    (print-efs-items (get-sorted-efs-items (nv :Provisioning_Item_Files)))
    (print-nv-parser-errors nv)))


(comment "usage illustration"
  (def nv-definition-schema (parse-nv-definition-file "samples/NvDefinition.xml"))
  (def nv-ref (parse-qcn-data nv-definition-schema "samples/sample.qcn"))
  (def nv-test (parse-qcn-data nv-definition-schema "test.qcn"))

  (:896 (:NV_ITEM_ARRAY nv-ref))
  (:errors nv-ref)

  (println "REFERENCE\n===================")
  (print-nv-item-set nv-definition-schema nv-ref)
  (print-nv-parser-errors nv-ref)
  (print-nv-parser-errors (subst-with-parsed-nv-efs-data nv-definition-schema nv-ref) )

  (println "CHECK\n===================")
  (print-nv-item-set nv-definition-schema nv-test)
  )
