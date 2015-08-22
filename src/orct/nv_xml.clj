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
            [clojure.string :as str])
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
                       (let [id (keyword (-> n :attrs :id))
                             errors (if (-> result :nv-item id)
                                      (conj errors (format "nv item %s multiple defined!" (key2str id)))
                                      errors)
                             update (-> result
                                        (assoc-in [:errors] errors)
                                        (assoc-in [:nv-items id :name] (-> n :attrs :name))
                                        (assoc-in [:nv-items id :permission ] (-> n :attrs :permission))
                                        (assoc-in [:nv-items id :content]
                                                  (parse-nv-definition-content id content)))]
                         update)

                       (= (:tag n) :NvEfsItem)
                       (let [name (keyword (-> n :attrs :fullpathname))
                             errors (if (-> result :efs-items name)
                                      (conj errors (format "efs item %s multiple defined!" (key2str name)))
                                      errors)
                             update (-> result
                                        (assoc-in [:errors] errors)
                                        (assoc-in [:efs-items name :permission ] (-> n :attrs :permission))
                                        (assoc-in [:efs-items name :content ]
                                                  (parse-nv-definition-content name content)))]
                         update)
                       :else result)]
          (recur result (z/next loc)))))))


(defn- extend-to-same-path
  "extends non fully qualified filename with same path
  as fully qualified or accessible template file."
  [template-file-name filename-to-extend]
  (let [sep (java.io.File/separator)
        t (java.io.File. template-file-name)
        p (.getParent t)]
    (str p sep filename-to-extend)))


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
              errors (:errors result)
              result (cond
                       (= (:tag n) :xi:include)
                       (let [xml-include (-> n :attrs :href)
                             include-file (first (str/split xml-include #"#"))
                             fully-qualified-input-file (extend-to-same-path filename include-file)]
                         (let [inside (parse-nv-data-file fully-qualified-input-file)
                               errors (concat (:errors result) (:errors inside))
                               result (merge-with merge (dissoc result :errors) (dissoc inside :errors))]
                           (-> result (assoc-in [:errors] errors))))

                       (= (:tag n) :NvItem)
                       (let [id (keyword (-> n :attrs :id))
                             errors (if (-> result :nv-item id)
                                      (conj errors (format "nv item %s multiple defined!" (key2str id)))
                                      errors)
                             update (-> result
                                        (assoc-in [:errors] errors)
                                        (assoc-in [:nv-items id :name] (-> n :attrs :name))
                                        (assoc-in [:nv-items id :mapping ] (-> n :attrs :mapping))
                                        (assoc-in [:nv-items id :encoding] (-> n :attrs :encoding))
                                        (assoc-in [:nv-items id :content]
                                                  content))]
                         update)

                       (= (:tag n) :NvEfsItem)
                       (let [name (keyword (-> n :attrs :fullpathname))
                             errors (if (-> result :efs-items name)
                                      (conj errors (format "efs item %s multiple defined!" (key2str name)))
                                      errors)
                             update (-> result
                                        (assoc-in [:errors] errors)
                                        (assoc-in [:efs-items name :index ] (-> n :attrs :index))
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


  (def x (xml/parse "samples/Masterfile.xml"))
  (println x)

  (def x (parse-nv-data-file "samples/Masterfile.xml"))
  (println x)
  (println (:errors x))

  (separate-efs-item-stores x)



  (map #(println % "\n=========================\n") (:nv-items x))
  (map #(println % "\n=========================\n") (:efs-items x))
  (map #(println %) ((:nv-items x) :855))
  (map #(println %) ((:nv-items x) :1015))
  (map #(println %) ((:nv-items x) :1031))
  (map #(println %) ((:nv-items x) :3532))
  (map #(println %) ((:nv-items x) :6873))
  (println (keys x))
  (println (keys (:nv-items x)))

  (println (keys (:efs-items x)))
  (println ((:efs-items x) :/nv/item_files/ims/qp_ims_sms_config))
  (println ((:efs-items x) :/nv/item_files/ims/qp_ims_dpl_config))

  (println nv-sample)
  (println nv-efs-sample)


  (map #(println %) (:errors nv-definition-schema))
  (map #(println %) (:nv-items nv-definition-schema))
  (map #(println %) (:efs-items nv-definition-schema))

  (def nv (:nv-items nv-definition-schema))
  (def nv-22826 (nv :22826))
  (println nv-22826)

  (map
   #(println %)
   (:content nv-22826))

  (def efs (:efs-items nv-definition-schema))
  (def qipcall_1xsmsandvoice (efs :/nv/item_files/ims/qipcall_1xsmsandvoice))
  (def qipcall_1xsmsandvoice (efs :/nv/item_files/ims/qp_ims_ut_config))

  (map
   #(println %)
   (:content qipcall_1xsmsandvoice))

  )
