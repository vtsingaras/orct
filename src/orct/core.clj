(ns orct.core
  (:require [clojure.xml :as xml])
  (:use [orct.qcn-parser]
        [orct.nv-xml])
  (:import java.nio.ByteBuffer java.io.FileInputStream))

(comment
 (print-qcn "/Users/ol/Entwicklung/Peiker/nv-parsing/LTE_NAD_SW_QCN/SW_QCN_BC_02_NA_ATnT_GSM_Dual_WCDMA_2+5_LTE_2+4+5+12+17_VoLTE.qcn")
 )
