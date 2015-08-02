;;;
;;; The use and distribution terms for this software are covered by
;;; the GNU General Public License
;;;
;;; ====== utility macros ======
;;;
;;; 2012-04-08, Otto Linnemann


(ns orct.macros)

(defmacro hash-args
  "constructs a hash map with given arguments is value
   and the corresponding keywords as keys.
   example:  (let [a 42 b 43] (hash-args a b))
          -> {:b 43, :a 42}"
  [& symbols]
  (doall (reduce #(assoc %1 (keyword %2) %2) {} symbols)))



(defmacro apply-hash
  "like apply but uses a hash list instead of a vector
   example (f :a 42 :b 43) is equivalent to
           (apply-hash {:a 42 :b 43} f)

   Attention: does not work under clojurescript!"
    [h f]
    `(apply ~f (mapcat #(vector (key %) (val %)) ~h)))
