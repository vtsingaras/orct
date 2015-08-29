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
        [orct.qcn-printer]
        [orct.qcn-writer]
        [orct.nv-xml]
        [orct.utils])
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
   ["-p" "--print QCN-FILE|XML-Masterfile" "print qcn data with given SCHEMA definition file."
    :validate [#(.exists (java.io.File. %)) "file must exist"]]
   ["-c" "--compile XML-Masterfile QCN-Outputfile" "compile xml data with given SCHEMA definition file."
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
        schema-file (:schema options)
        print-file (:print options)
        compile-file (:compile options)
        invalid-opts (not-empty errors)
        title-str (str
                   "ORCT: Parsing and Gerenation of Qualcomm Radio Calibration Data Files (QCN)\n"
                   "      (C) 2015, GNU General Public Licence by Otto Linnemann\n")
        start-msg-fmt (str
                       "starting application with\n"
                       "\t      schema definition file: %s\n"
                       "\t              qcn input file: %s\n")
        start-msg (format start-msg-fmt schema-file print-file)
        file-ext-pred #(= %1 (get-lc-filename-ext %2))]
    (println title-str)
    (if (or (:help options) (not (or schema-file print-file)) invalid-opts)
      (do
        (println "  Invocation:\n")
        (println summary)
        (if invalid-opts (println-err invalid-opts) )
        -1)
      (if invalid-opts
        (println-err invalid-opts)
        (if errors
          (println-err errors)
          (if schema-file
            (if print-file
              (let [schema (parse-nv-definition-file schema-file)
                    print
                    (condp file-ext-pred print-file
                      "qcn" (parse-qcn-data schema print-file)
                      "xml" (parse-nv-data schema print-file))]
                (println (format "Parsing result for file %s using schema definition %s"
                                 print-file schema-file))
                (print-nv-item-set schema print)
                0)
              (if compile-file
                (let [schema (parse-nv-definition-file schema-file)
                      [output-file] arguments]
                  (if output-file
                    (if (= (get-lc-filename-ext output-file) "qcn")
                      (let [qcn (parse-nv-data schema compile-file)]
                        (write-qcn-struct-to-poi-fs qcn output-file)
                        (println (format "file %s written!" output-file))
                        (print-nv-parser-errors qcn)
                        0)
                      (println-err (format "output file %s has wrong extention!" output-file)))
                    (println-err "no outputfile specified error!")))))
            (do
              (println-err "both, schema and processed file needs to be specified!")
              -1)))))))

(comment
  (cli "-s" "samples/NvDefinition.xml" "-p" "samples/sample.qcn")
  (cli "-s" "samples/NvDefinition.xml" "-p" "samples/Masterfile.xml")
  (cli "-s" "samples/NvDefinition.xml" "-p" "samples/Masterfile.xml" "abc")
  (cli "-s" "samples/NvDefinition.xml" "-c" "samples/Masterfile.xml" "abc.qcn")
  (cli "-s" "samples/NvDefinition.xml" "-p" "abc.qcn")
  (cli "-s" "samples/NvDefinition.xml" "-c" "samples/Masterfile.xml" "abc.qxn")
  (cli "-s" "samples/NvDefinition.xml" "-c" "samples/Masterfile.xml")
  (cli "-sxadf" "samples/NvDefinition.xml")
  (cli "--help")
  (cli)
  )

(defn -main
  "main function wrapper"
  [& args]
  (try
    (System/exit (apply cli args))
    (catch Throwable t
      (let [msg (format "%s" (.getMessage t))
            cause (.getCause t)
            st (.getStackTrace t)]
        (println msg)
        (when cause (println "cause: " cause))
        (dorun (map #(println %) st))))))
