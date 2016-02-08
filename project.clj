(defproject orct "0.2.5-SNAPSHOT"
  :description "An enhanced Open Source Implementation to replace Qualcomm's QRCT"
  :url "https://github.com/linneman/orct"
  :license {:name "GNU General Public Licence v2"
            :url "http://www.gnu.org/licence"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.apache.poi/poi "3.10-FINAL"]
                 [org.clojure/tools.cli "0.3.1"]]
  :source-paths ["src"]
  :main orct.core
  :aot [orct.core]
  ;:resource-paths ["resources/poi-3.12-20150511.jar"]
  )
