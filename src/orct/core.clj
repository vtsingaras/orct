;; Open Radio Calibration Toolkit
;; An enhanced Open Source Implementation to replace Qualcomm's QRCT
;;
;; The use and distribution terms for this software are covered by
;; the GNU General Public License
;;
;; (C) 2015, Otto Linnemann
;;
;; core.cljs - command line iterface

(ns orct.core
  (:require [clojure.xml :as xml]
            [clojure.tools.cli :refer [parse-opts]])
  (:use [orct.qcn-parser]
        [orct.nv-xml])
  (:import java.nio.ByteBuffer java.io.FileInputStream)
  (:gen-class))


(comment
  (def nv-definition-schema (parse-nv-definition-file "samples/NvDefinition.xml"))
  (print-qcn nv-definition-schema "samples/sample.qcn")
 )


; command line interface (leiningen)

(def cli-options
  [["-s" "--schema SCHEMA-FILE" "xml schema definition file"
    :validate [#(.exists (java.io.File. %)) "files must exist"]]
   ["-p" "--print QCN-FILE" "print QCN-FILE with given SCHEMA definition file."
    :validate [#(.exists (java.io.File. %)) "file must exist"]]
   ["-h" "--help" "this help string"]])


(defn- cli
  "process command line arguments and execute commands"
  [& args]
  (let [opts (parse-opts args cli-options)
        options (:options opts)
        arguments (:arguments opts)
        summary (:summary opts)
        errors (:errors opts)
        schema (:schema options)
        print (:print options)
        invalid-opts (not-empty errors)
        title-str (str
                   "ORCT: Parsing and Gerenation of Qualcomm Radio Calibration Data Files (QCN)\n")
        start-msg-fmt (str
                       "starting application with\n"
                       "\t      schema definition file: %s\n"
                       "\t              qcn input file: %s\n")
        start-msg (format start-msg-fmt schema print)]
    (println title-str)
    (when (or (:help options) (not (or schema print)) invalid-opts)
      (do
        (println "  Invocation:\n")
        (println summary)
        -1))
    (if invalid-opts
      (println invalid-opts)
      (if errors
        (println errors)
        (if (and schema print)
          (let [nv (parse-nv-definition-file schema)]
            (println (format "Parsing result for file %s using schema definition %s" print schema))
            (print-qcn nv print)
            0)
          (do
            (println "both, schema and processed file needs to be specified!")
            -1))))))

(comment
  (cli "-s" "samples/NvDefinition.xml" "-p" "samples/sample.qcn")
  (cli "-s" "samples/NvDefinition.xml")
  (cli "-sxadf" "samples/NvDefinition.xml")
  (cli "--help")
  (cli)
  )

(defn -main
  "main function wrapper"
  [& args]
  (try
    (System/exit (apply cli args))
    (catch Exception e (println  (str "caught exception: " (.getMessage e)))
           (System/exit -1))))
