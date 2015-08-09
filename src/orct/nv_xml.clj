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
            [clojure.zip :as z])
  (:import java.nio.ByteBuffer java.io.FileInputStream))


(defn- parse-nv-definition-content
  [id content]
  (map (fn [{:keys [tag attrs content]}]
         (when-not (= tag :Member)
           (throw (IllegalStateException. (format "NV item %s has wrong tag %s!" id tag))))
         (let [{:keys [name type sizeOf]} attrs]
           {:name name :type type :size (str2int sizeOf)}))
       content))

(defn- get-item
  [nv-def-struct type id]
  (let [nv (type nv-def-struct)]
    (when nv (nv id))))


(defn parse-nv-definition-file
  "parse NvDefintion file which provides the xml schema definition
  for mobile phone NV item setup."
  [filename]
  (let [nv-def-xml (xml/parse filename)]
    (loop [result {} loc (z/xml-zip nv-def-xml)]
      (if (z/end? loc)
        result
        (let [n (z/node loc)
              content (-> n :content)
              errors (:errors result)
              result (cond
                       (= (:tag n) :NvItem)
                       (let [id (-> n :attrs :id)
                             errors (if (get-item result :nv-item id)
                                      (conj errors (format "nv item %s multiple defined!" id))
                                      errors)
                             update (-> result
                                        (assoc-in [:errors] errors)
                                        (assoc-in [:nv-items id :name] (-> n :attrs :name))
                                        (assoc-in [:nv-items id :permission ] (-> n :attrs :permission))
                                        (assoc-in [:nv-items id :content]
                                                  (parse-nv-definition-content id content)))]
                         update)

                       (= (:tag n) :NvEfsItem)
                       (let [name (-> n :attrs :fullpathname)
                             errors (if (get-item result :efs-items name)
                                      (conj errors (format "efs item %s multiple defined!" name))
                                      errors)
                             update (-> result
                                        (assoc-in [:errors] errors)
                                        (assoc-in [:efs-items name :permission ] (-> n :attrs :permission))
                                        (assoc-in [:efs-items name :content ]
                                                  (parse-nv-definition-content name content)))]
                         update)
                       :else result)]
          (recur result (z/next loc)))))))



(comment

  (println
   (xml/parse "/Users/ol/Entwicklung/Peiker/nv-parsing/LTE_NAD_SW_QCN/Masterfile_SW_QCN_ATnT_VoLTE.xml"))

  (println
   (xml/parse "/Users/ol/Entwicklung/Peiker/nv-parsing/LTE_NAD_SW_QCN/NvDefinition.xml"))


  (def x (xml/parse "/Users/ol/Entwicklung/Peiker/nv-parsing/LTE_NAD_SW_QCN/NvDefinition.xml"))
  (def x (xml/parse "/mnt/ssd1/ol/lte_bl5_5/peikertools/nvitems/LTE_NAD_SW_QCN/NvDefinition.xml"))






  (println nv-sample)
  (println nv-efs-sample)

  (def nv-definition-schema (parse-nv-definition-file "/mnt/ssd1/ol/lte_bl5_5/peikertools/nvitems/LTE_NAD_SW_QCN/NvDefinition.xml"))

  (def nv-definition-schema (parse-nv-definition-file "/Users/ol/Entwicklung/Peiker/nv-parsing/LTE_NAD_SW_QCN/NvDefinition.xml"))

  (map #(println %) (:errors nv-definition-schema))

  (def nv (:nv-items nv-definition-schema))
  (def nv-22826 (nv "22826"))
  (println nv-22826)

  (map
   #(println %)
   (:content nv-22826))


  (def efs (:efs-items nv-definition-schema))
  (def qipcall_1xsmsandvoice (efs "/nv/item_files/ims/qipcall_1xsmsandvoice"))
  (def qipcall_1xsmsandvoice (efs "/nv/item_files/ims/qp_ims_ut_config"))

  (map
   #(println %)
   (:content qipcall_1xsmsandvoice))



  (def band-pref-schema
    (array-map
     :name :string
     :band1 :int32
     :band2 :int16
     ))

  (def band-pref1
    (hash-map
     :name "lte-band1"
     :band1 132183
     :band2 10211
     ))

  (def band-pref1-wrong
    (hash-map
     :name "lte-band1"
     :band1 132183
     ))

  (for [[k v] band-pref-schema] (println (band-pref1 k)))

  )


(defn marshal-item-value
  [f v]
  (case f
    :string v
    :int8 (long2bytearray v 8)
    :uint8 (long2bytearray v 8)
    :int16 (long2bytearray v 16)
    :uint16 (long2bytearray v 16)
    :int32 (long2bytearray v 32)
    :uint32 (long2bytearray v 32)
    ))

(defn marshal-items [schema values]
  (for [[k f] schema]
    (let [v (values k)]
      (when (not v)
        (throw (IllegalStateException. (str "no definition for key " k " error!"))))
      ;(println k f v)
      (marshal-item-value f v)
      )))


(comment
  (try
    (doall (marshal-items band-pref-schema band-pref1))
    (catch Exception e (str "caught exception: " (.getMessage e))))

  (try
    (doall (marshal-items band-pref-schema band-pref1-wrong))
    (catch Exception e (str "caught exception: " (.getMessage e))))
  )
