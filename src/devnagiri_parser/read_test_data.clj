(ns devnagiri-parser.read-test-data
  (:require  [clojure.edn :as edn]))

(def test-data-file "output/test-data.edn")
(def class-test-data-file "output/classified-test-data.edn")

(defn test-data
  []
  (-> test-data-file
      slurp
      edn/read-string))

(defn classifications
  []
  (-> class-test-data-file
      slurp
      edn/read-string))
