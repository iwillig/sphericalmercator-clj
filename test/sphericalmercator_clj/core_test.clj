(ns sphericalmercator-clj.core-test
  (:use clojure.test
        sphericalmercator-clj.core))


(def sm (spherical-mercator 256))

(deftest test-bbox

  ;; (testing ""
  ;;   (let [bbox (xyz->bbox sm 0 0 0)]
  ;;     (is (= [180.0 -85.05112877980659 -180.0 85.0511287798066] bbox))))

  (testing "bbox conversation one"
    (let [bbox (xyz->bbox sm 37 46 6)]
      (println bbox)
      (is (= bbox []))))

  (testing "bbox conversation two"
    (let [bbox (xyz->bbox sm 75 -97 8)]
      (is (= '(-74.53125 -89.54203477001494 -73.125 -89.53065566495872)
             bbox)))))
