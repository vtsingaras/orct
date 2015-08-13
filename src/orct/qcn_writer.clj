;; Open Radio Calibration Toolkit
;; An enhanced Open Source Implementation to replace Qualcomm's QRCT
;;
;; The use and distribution terms for this software are covered by
;; the GNU General Public License
;;
;; (C) 2015, Otto Linnemann
;;
;; qcn-parser.cljs - writes QCN file, takes clojure'd xml data as input


(ns orct.qcn-writer
  (:use [orct.utils]
        [orct.nv-xml] ;; only temporary
        )
  (:require [clojure.xml :as xml]
            [clojure.zip :as z]
            [clojure.string :as str])
  (:import java.nio.ByteBuffer java.io.FileInputStream
           [org.apache.poi.poifs.filesystem POIFSFileSystem DirectoryNode DocumentNode
            DocumentInputStream DocumentOutputStream]))


(defn- separate-efs-item-stores
  "EFS items  are assigned  to two  different QCN  document streams.
  Those with the  flag :provisioning-store set to true go  to to the
  stream  'Provisioning_Item_Files', all  others  go to  'NV_Items'.
  This function  returns a vector  where the first  element provides
  all items for  the provisiong store, the second the  items for the
  nv item store."
  [parsed-nv-xml]
  (reduce
   (fn [[store efs-backup]
        [path params]]
     (let [prov-store (:provisioning-store params)
           item {path params}
           [store efs-backup] (if prov-store
                                [(merge store item) efs-backup]
                                [store (merge efs-backup item)])]
       [store efs-backup]))
   [{#_prov-store} {#_nv-item-store}] ;; result vector, initially empty
   (:efs-items parsed-nv-xml)))


(defn- get-ordinal-member-hash
  "transform  efs parameter  member  item list  into  sorted hash  map
  ordinal]  item position  as key  and  all other  item attributes  as
  values."
  [nv-efs-params]
  (let [[result idx]
        (reduce
         (fn [[result member-idx]
              member]
           [(assoc result member-idx member)
            (inc member-idx)])
         [(sorted-map #_result)  0 #_idx]
         nv-efs-params)]
    result))


(defn- get-named-member-hash
  "transform  efs parameter  member  item list  into  sorted hash  map
  ordinal]  item position  as key  and  all other  item attributes  as
  values."
  [nv-efs-params]
  (let [[result idx]
        (reduce
         (fn [[result member-idx]
              member]
           (let [name (:name member)]
             [(assoc result name (dissoc member name))
              (inc member-idx)]))
         [(sorted-map #_result)  0 #_idx]
         nv-efs-params)]
    result))


(defn- get-member-by-name-or-ordinal-number
  "tries to retrieve nv item  compement member by specified hash name.
  Since  this is  not  always defined,  we retrieve  a  member by  its
  ordinal number otherwise"
  [nv-efs-params  member-name  ordinal-number]
  (if-not (empty? member-name)
    (or ((get-named-member-hash nv-efs-params) member-name)
        ((get-ordinal-member-hash nv-efs-params) ordinal-number))
    ((get-ordinal-member-hash nv-efs-params) ordinal-number)))


(comment

  ;; example for efs nv definition with named components
  (def ims-dpl-config-def (-> nv-definition-schema
                              :efs-items
                              :/nv/item_files/ims/qp_ims_dpl_config
                              :content))

  ;; example for efs nv definition without named components
  (def rpm-info-def (-> nv-definition-schema
                        :efs-items
                        :/nv/item_files/modem/mmode/rpm_info
                        :content))

  (get-ordinal-member-hash ims-dpl-config-def)
  (get-ordinal-member-hash rpm-info-def)

  (get-named-member-hash ims-dpl-config-def)
  (get-named-member-hash rpm-info-def)

  (get-member-by-name-or-ordinal-number  ims-dpl-config-def "e911_ipv6_enabled" 0)
  (get-member-by-name-or-ordinal-number  rpm-info-def "" 3)
  (get-member-by-name-or-ordinal-number  rpm-info-def "xyz" 3)

  )


(defn transform-efs-item-params-to-qcn-struct
  "transform nv parameter content data given in format parsed from xml
  definition into same format as used by qcn parser."
  [nv-definition  efs-item  errors]
  (let [[path xml-params] efs-item
        xml-content (:content xml-params)
        efs-schema (-> nv-definition-schema :efs-items path :content)
        result
        (cond

          (empty? efs-schema) ;; when schema defintion not given, we assume just uint8
          (if (> (count xml-content) 1)
            (throw (IllegalStateException.
                    (format "efs item %s lacks schema definition and has more than one element!"
                            path)))
            {:name "" :val xml-content :type "uint8" :member-idx 0})

          (map? (first xml-content)) ;; nv provided as associate array (hash map) for components
          (map
           (fn [{:keys [tag attrs content]} member-idx]
             (let [member-def (get-member-by-name-or-ordinal-number
                               efs-schema (key2str tag) member-idx)]
               {:name tag
                :val content
                :type (:type member-def)
                :size (:size member-def)
                :member-idx member-idx}))
           xml-content
           (iterate inc 0))

          (vector? (first xml-content)) ;; nv components provided just as vector
          (map
           (fn [val member-idx]
             (let [member-def (get-member-by-name-or-ordinal-number
                               efs-schema nil member-idx)]
               {:name (:name member-def)
                :val val
                :type (:type member-def)
                :size (:size member-def)
                :member-idx member-idx}))
           xml-content
           (iterate inc 0))

          (string? (first xml-content))
          [{:name "flat" :val [(str/trim (first xml-content))] :type "uint8" :member-idx 0}]

          :else (throw (IllegalStateException.
                        (format "efs item %s has wrong definition!" path))))]
    [(vec result) errors]))

(comment ;; usage illustration

  (defn- test-transform-efs-item-params
    [path]
    (let [efs-item [path (-> nv-xml-data :efs-items path)]]
      ;(println efs-item)
      (transform-efs-item-params-to-qcn-struct
       nv-definition-schema
       efs-item
       [])))

  (test-transform-efs-item-params :/nv/item_files/ims/qipcall_octet_aligned_mode_amr_wb)
  (test-transform-efs-item-params :/nv/item_files/ims/qp_ims_dpl_config)
  (test-transform-efs-item-params :/nv/item_files/ims/qp_ims_sms_config)
  (test-transform-efs-item-params :/nv/item_files/modem/uim/uimdrv/feature_support_hotswap)
  )



(defn transform-efs-items-to-qcn-struct
  "transform nv  efs data given  in format parsed from  xml definition
  (path->item  hash)  to  new  index-item  hash as  used  in  qcn  poi
  structure."
  [nv-definition efs-items errors]
  (let [[result idx errors]
        (reduce

         (fn [[result idx errors]
              efs-item]
           (let [idx-key (keyword (format "%08d" idx))
                 [path params] efs-item

                 [efs-struct errors]
                 (transform-efs-item-params-to-qcn-struct nv-definition
                                                          efs-item
                                                          errors)]
             [(-> result
                  (assoc-in [idx-key :path] path)
                  (assoc-in [idx-key :params] efs-struct))
              (inc idx)
              errors]))

         [{#_index_nv-item}  0 #_idx errors]
         efs-items)]
    [result errors]))



(comment

  (def nv-xml-data (parse-nv-data-file "samples/Masterfile.xml"))

  (def t-efs-errors (transform-efs-items-to-qcn-struct nv-definition-schema (:efs-items nv-xml-data) []))
  (def t-efs (first t-efs-errors))
  (def t-errors (second t-efs-errors))

  (:/nv/item_files/ims/qp_ims_dpl_config (:efs-items nv-xml-data))
  (:/nv/item_files/ims/qp_ims_dpl_config (:efs-items nv-definition-schema))

  (doall t-efs-errors)

  (println t-efs)

  (println
   (:00000064 t-efs)) ;; qp_ims_dpl_config

  (println
   (:00000061 t-efs)) ;; rpm_info


  )
