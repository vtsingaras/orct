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
  "print byte sequence over multiple lines with given tabulating level"
  [level content format-str & {:keys [max-columns max-elements line-sep print-cont-dots]
                               :or {max-columns 16
                                    max-elements 128
                                    line-sep ""
                                    print-cont-dots true}}]
  (let [given-elements (count content)
        content (map #(bit-and 0xff %) (take max-elements content))
        colums (partition-all max-columns content)
        hex-line
        (if (number? (first content))
          (fn [x] (apply str (map #(format format-str %) x)))
          (fn [x] (apply str (map #(format "%5s " %) x))))]
    (dorun (map #(println (tabs level) (hex-line %) line-sep) colums))
    (when (and print-cont-dots (> given-elements max-elements)) (println (tabs level) " ..."))))


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
  (dorun
   (map
    (fn [[item {:keys [index data name params] :as item-args}]]
      (try
        (println (format "%sitem%s, index:%s, name:%s" (tabs 3) item index name))
        (if params
          (dorun (map (fn [{:keys [name val]}]
                        (print (format "%s%s -> " (tabs 6) name))
                        (if (<= (count val) 16)
                          (if (= (count val) 1) (println (first val)) (println val))
                          (do (println) (print-dec-content 6 val)))) params))
          (do
            (println (tabs 6) "-- missing Schema for this parameter! --")
            (print-hex-content 6 data)))
        (catch Throwable t (throw (IllegalStateException.
                                   (format "Error in printing of nv-item %s occurred:\n%s"
                                           (pr-str item) (pr-str item-args))))))
      ) items)))



(defn- print-legacy-items-update-script
  [schema items]
  (dorun
   (map
    (fn [[item {:keys [index data name params] :as item-args}]]
      (let [item-schema ((:nv-items schema) item)
            size-in-schema (or (:size item-schema) 0)
            max-elements (if (> size-in-schema 0) size-in-schema 8000)]
        (try
          (println (format "echo 'update item%s, name:%s'" item name))
          (println (format "nvimgr --item %s 0 \\" (key2str item)))
          (print-dec-content 1 data  :line-sep "\\" :max-elements max-elements
                             :print-cont-dots false)
          (println "\n")
          (catch Throwable t (throw (IllegalStateException.
                                     (format "Error in printing of nv-item %s occurred:\n%s"
                                             (pr-str item) (pr-str item-args)))))))
      ) items)))


(defn- print-efs-items
  [items]
  (dorun
   (map
    (fn [[item {:keys [path data params] :as item-args}]]
      (try
        (println (format "%spath%s" (tabs 3) path))
        (if params
          (dorun (map (fn [{:keys [name val]}]
                        (print (format "%s%s -> " (tabs 6) name))
                        (if (<= (count val) 16)
                          (if (= (count val) 1) (println (first val)) (println val))
                          (do (println) (print-dec-content 6 val)))) params))
          (do
            (println (tabs 6) "-- missing Schema for this parameter! --")
            (print-hex-content 6 data)))
        (catch Throwable t (throw (IllegalStateException.
                                   (format "Error in printing of efs-item %s occurred:\n%s"
                                           (pr-str path) (pr-str item-args))))))
      ) items)))


(defn- print-efs-items-update-script
  [items]
  (dorun
   (map
    (fn [[item {:keys [path data params] :as item-args}]]
      (try
        (println (format "echo ' update efs-item path%s'" path))
        (println (format "nvimgr --item %s %d \\" (key2str path) (count data)))
        (print-dec-content 1 data  :line-sep "\\" :max-elements 8000)
        (println "\n")
        (catch Throwable t (throw (IllegalStateException.
                                   (format "Error in printing of efs-item %s occurred:\n%s"
                                           (pr-str path) (pr-str item-args))))))
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


(defn- get-sorted-legacy-items
  [nv]
  (let [itemkey2number #(-> % key2str str2int)
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



(defn- merge-efs-item-hashes
  "helper function to merge all efs based items together
   by reassigning new temporary keys"
  [& v-efs]
  (apply merge
         (map (fn [n e]
                {(-> n str keyword) (val e)})
              (iterate inc 0)
              (mapcat vec v-efs))))

(comment
  (merge-efs-item-hashes {:0001 "one" :0002 "two"} {:0001 "three" :0002 "four"})
  )


(defn print-nv-item-set

  "prints content of specified qcn data. Function takes
   two arguments:

   schema : parsed nv defintion schema file (parse-nv-definition-file)
   nv     : parsed qcn or xml nv data definition."

  [schema nv & {:keys [flat] :or {flat true}}]
  (let [nv (subst-with-parsed-nv-efs-data schema nv)]
    (println ">>>>> File Info >>>>>")
    (print-file-version-info (nv :File_Version))
    (println ">>>>> Mobile Property Info >>>>>")
    (print-mobile-property-info (nv :Mobile_Property_Info))
    (println ">>>>> NV Items >>>>>")
    (print-legacy-items (get-sorted-legacy-items nv))
    (if flat
      (do
        (println ">>>>> All Items in EFS Store >>>>>")
        (print-efs-items
         (get-sorted-efs-items
          (merge-efs-item-hashes (nv :NV_Items)
                                 (nv :EFS_Backup)
                                 (nv :Provisioning_Item_Files)))))
      (do
        (println ">>>>> Item File Backup >>>>>")
        (print-efs-items (get-sorted-efs-items (nv :NV_Items)))
        (println ">>>>> Provisioning Item Files >>>>>")
        (print-efs-items (get-sorted-efs-items (nv :Provisioning_Item_Files)))
        (println ">>>>> EFS Item Backup >>>>>")
        (print-efs-items (get-sorted-efs-items (nv :EFS_Backup)))))
    (print-nv-parser-errors nv)))


(defn print-nv-item-set-update-script

  "prints update script for qcn data. Function takes
   two arguments:

   schema : parsed nv defintion schema file (parse-nv-definition-file)
   nv     : parsed qcn or xml nv data definition."

  [schema nv]
  (println "#!/bin/sh")
  (println "# Update Script for NV Configuration")
  (println "# Generated by ORCT on " (str (java.util.Date.)))
  (println "# refer to https://github.com/linneman/orct for further information")
  (println "\n\n")
  (let [nv (subst-with-parsed-nv-efs-data schema nv)]
    (print-legacy-items-update-script schema (get-sorted-legacy-items nv))
    (print-efs-items-update-script (get-sorted-efs-items (nv :NV_Items)))
    (print-efs-items-update-script (get-sorted-efs-items (nv :Provisioning_Item_Files)))))



(comment "usage illustration"
  (def nv-definition-schema (parse-nv-definition-file "samples/NvDefinition.xml"))
  (def nv-ref (parse-qcn-data nv-definition-schema "samples/sample.qcn"))
  (def nv-ref (parse-nv-data nv-definition-schema "samples/Masterfile.xml"))
  (def nv-test (parse-qcn-data nv-definition-schema "test.qcn"))

  (:896 (:NV_ITEM_ARRAY nv-ref))
  (count (:data (:896 (:NV_ITEM_ARRAY nv-ref))))

  (:946 (:NV_ITEM_ARRAY nv-ref))
  (def port-list (:data (:6873 (:NV_ITEM_ARRAY nv-ref))))
  (def band-pref (:data (:946 (:NV_ITEM_ARRAY nv-ref))))

  (:errors nv-ref)

  (println "REFERENCE\n===================")
  (print-nv-item-set nv-definition-schema nv-ref)
  (print-nv-item-set-update-script nv-definition-schema nv-ref)
  (print-legacy-items-update-script nv-definition-schema (get-sorted-legacy-items nv-ref))
  (print-efs-items-update-script (get-sorted-efs-items (nv-ref :NV_Items)))
  (print-nv-parser-errors nv-ref)
  (print-nv-parser-errors (subst-with-parsed-nv-efs-data nv-definition-schema nv-ref) )

  (println "CHECK\n===================")
  (print-nv-item-set nv-definition-schema nv-test)
  )
