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
            [clojure.tools.cli :refer [parse-opts]]
            [clojure.java.shell :refer [sh]])
  (:use [orct.qcn-parser]
        [orct.qcn-printer]
        [orct.qcn-writer]
        [orct.nv-xml]
        [orct.utils]
        [orct.macros])
  (:import java.nio.ByteBuffer java.io.FileInputStream)
  (:gen-class))


(comment
  (def nv-definition-schema (parse-nv-definition-file "samples/NvDefinition.xml"))
  (def p-result (parse-qcn-data nv-definition-schema "samples/sample.qcn"))
  (print-nv-item-set nv-definition-schema p-result)
 )


; command line interface (leiningen)

(def cli-options
  [["-s" "--schema SCHEMA-FILE" "xml schema definition file"
    :validate [#(.exists (java.io.File. %)) "files must exist"]]
   ["-p" "--print QCN-FILE|XML-Masterfile" "print qcn data with given SCHEMA definition file."
    :validate [#(.exists (java.io.File. %)) "file must exist"]]
   ["-u" "--update QCN-FILE|XML-Masterfile" "generate update script with given SCHEMA definition file."
    :validate [#(.exists (java.io.File. %)) "file must exist"]]
   ["-c" "--compile XML-Masterfile QCN-Outputfile" "compile xml data with given SCHEMA definition file."
    :validate [#(.exists (java.io.File. %)) "file must exist"]]
   ["-d" "--diff XML-Masterfile file1 file2 " "diff qcn or xml data with given SCHEMA definition file."
    :validate [#(.exists (java.io.File. %)) "file must exist"]]
   ["-t" "--diff-tool diff-executable" "diff tool to used, defaults to diff"]
   ["-v" "--verbose" "0: default flat output, 1: show efs streams separately."
    :id :verbosity
    :default 0
    :assoc-fn (fn [m k _] (update-in m [k] inc))]
   ["-h" "--help" "this help string"]])


(defn- file-ext-pred
  [file-ext filename]
  (= file-ext (get-lc-filename-ext filename)))


(defn- print-file
  [schema-file schema file-to-print & {:keys [style output verbose] :or {style :ascii verbose false}}]
  (let [p-result (condp file-ext-pred file-to-print
                   "qcn" (parse-qcn-data schema file-to-print)
                   "xml" (parse-nv-data schema file-to-print))]
    (condp = style
      :ascii
      (do
        (println (format "Parsing result for file %s using schema definition %s"
                         file-to-print schema-file))
        (print-nv-item-set schema p-result :flat (not verbose))
        0)
      :update-script
      (if output
        (do
          (redir-out output (print-nv-item-set-update-script schema p-result))
          (println (format "file %s written!" output))
          (print-nv-parser-errors p-result)
          0)
        (do (println-err "no outputfile specified error!") -1)))))


(defn- compile-file
  [schema-file schema file-to-compile output-file]
  (if output-file
    (if (= (get-lc-filename-ext output-file) "qcn")
      (let [qcn (parse-nv-data schema file-to-compile)]
        (write-qcn-struct-to-poi-fs qcn output-file)
        (println (format "file %s written!" output-file))
        (print-nv-parser-errors qcn)
        0)
      (do (println-err (format "output file %s has wrong extention!" output-file)) -1))
    (do (println-err "no outputfile specified error!") -1)))


(defn- compile-first-then-print-file-to
  "enforce compilation and read back compile data for exact diffing"
  [schema-file  schema  file-to-print  output-txt-file]
  (let [p-result (condp file-ext-pred file-to-print
                   "qcn" (parse-qcn-data schema file-to-print)
                   "xml" (let [output-file (java.io.File/createTempFile "orct-intermediate-diff" ".qcn")
                               ccres (compile-file schema-file schema file-to-print (str output-file))
                               result (parse-qcn-data schema output-file)
                               _ (clojure.java.io/delete-file output-file)]
                           result))]
    (redir-out output-txt-file (print-nv-item-set schema p-result)))
  0)


(defn- diff-files
  [schema-file schema diff-file1 diff-file2 tool]
  (if diff-file2
    (let [tmp1 (java.io.File/createTempFile diff-file1 ".txt")
          tmp2 (java.io.File/createTempFile diff-file2 ".txt")
          cc1 (compile-first-then-print-file-to  schema-file schema diff-file1 tmp1)
          cc2 (compile-first-then-print-file-to  schema-file schema diff-file2 tmp2)
          {:keys [exit out err]}
          (if (or (not= cc1 0) (not= cc2 0))
            {:err (format "could not open/process tempory files %s1, %s2" tmp1 tmp2)}
            (clojure.java.shell/sh tool (str tmp1) (str tmp2)))
          del1 (clojure.java.io/delete-file tmp1)
          del2 (clojure.java.io/delete-file tmp2)]
      (println (format "======= diff result %s %s =======" diff-file1 diff-file2))
      (println out)
      (when (not-empty err) (println-err err))
      exit)
    (do (println-err "second file to diff to not specified error!") -1)))


(defn- cli
  "process command line arguments and execute commands"
  [& args]
  (let [opts (parse-opts args cli-options)
        options (:options opts)
        arguments (:arguments opts)
        summary (:summary opts)
        errors (:errors opts)
        schema-file (:schema options)
        print-file-opt (:print options)
        update-file-opt (:update options)
        compile-file-opt (:compile options)
        diff-file-opt (:diff options)
        diff-tool-opt (or (:diff-tool options) "diff")
        verbosity (:verbosity options)
        invalid-opts (not-empty errors)
        title-str (str
                   "ORCT: Parsing and Generation of Qualcomm Radio Calibration Data Files (QCN)\n"
                   (format "      Version: %s, refer to https://github.com/linneman/orct for more information\n" (get-version 'orct))
                   "      (C) 2015, GNU General Public Licence by Otto Linnemann\n")
        start-msg-fmt (str
                       "starting application with\n"
                       "\t      schema definition file: %s\n"
                       "\t              qcn input file: %s\n")
        start-msg (format start-msg-fmt schema-file print-file-opt)]
    (println title-str)
    (if (or (:help options) (not (or schema-file print-file-opt update-file-opt)) invalid-opts)
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
            (let [schema (parse-nv-definition-file schema-file)]
              (condp #(%1 %2) options
                :print (print-file schema-file schema print-file-opt :verbose (> verbosity 0))
                :update (print-file schema-file schema update-file-opt
                                    :style :update-script :output (first arguments))
                :compile (compile-file schema-file schema compile-file-opt (first arguments))
                :diff (diff-files schema-file schema diff-file-opt (first arguments) diff-tool-opt)))
            (do
              (println-err "both, schema and processed file needs to be specified!")
              -1)))))))

(comment
  (cli "-s" "samples/NvDefinition.xml" "-p" "samples/sample.qcn")
  (cli "-s" "samples/NvDefinition.xml" "-p" "samples/sample.qcn" "-v")
  (cli "-s" "samples/NvDefinition.xml" "-u" "samples/sample.qcn" "samples/update.sh")
  (cli "-s" "samples/NvDefinition.xml" "-u" "samples/sample.qcn")
  (cli "-u" "samples/sample.qcn" "samples/update.sh")
  (cli "-p" "samples/sample.qcn")
  (cli "-u" "samples/sample.qcn")
  (cli "-s" "samples/NvDefinition.xml" "-p" "samples/Masterfile.xml")
  (cli "-s" "samples/NvDefinition.xml" "-p" "samples/Masterfile.xml" "abc")
  (cli "-s" "samples/NvDefinition.xml" "-c" "samples/Masterfile.xml" "abc.qcn")
  (cli "-s" "samples/NvDefinition.xml" "-p" "abc.qcn")
  (cli "-s" "samples/NvDefinition.xml" "-c" "samples/Masterfile.xml" "abc.qxn")
  (cli "-s" "samples/NvDefinition.xml" "-c" "samples/Masterfile.xml")
  (cli "-s" "samples/NvDefinition.xml" "-d" "samples/Masterfile.xml" "abc.qcn")
  (cli "-s" "samples/NvDefinition.xml" "-d" "samples/Masterfile.xml" "samples/Masterfile_changed.xml")
  (cli "-s" "samples/NvDefinition.xml" "-d" "samples/Masterfile.xml" "samples/Masterfile_changed.xml" "-t" "meld")
  (cli "-sxadf" "samples/NvDefinition.xml")
  (cli "--help")
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
