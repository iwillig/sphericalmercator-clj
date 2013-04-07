(ns sphericalmercator-clj.core-test
  (:use clojure.test
        sphericalmercator-clj.core))

(def sm (spherical-mercator 256))

(deftest test-bbox
  (testing ""
    (let [bbox (xyz->bbox sm 0 0 0)]
      (is (= [[180.0 -85.05112877980659] [-180.0 85.0511287798066]] bbox)))))
