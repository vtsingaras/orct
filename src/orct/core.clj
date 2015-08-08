(ns orct.core
  (:require [clojure.xml :as xml])
  (:use [orct.qcn-parser]
        [orct.nv-xml])
  (:import java.nio.ByteBuffer java.io.FileInputStream))


(comment

  (def nv-definition-schema (parse-nv-definition-file "samples/NvDefinition.xml"))

  (print-qcn nv-definition-schema "samples/sample.qcn")

 )
