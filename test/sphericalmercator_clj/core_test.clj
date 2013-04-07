(ns sphericalmercator-clj.core-test
  (:use clojure.test
        sphericalmercator-clj.core))

(def sm (spherical-mercator 256))

(deftest test-pixel->longlat
  (testing ""
    (let [ll (pixel->ll sm [1 2] 1)]
      (is (= [179.296875 84.92832092949963] ll)))))

(deftest text-longlat->pixel
  (testing ""
    (let [pixel (lonlat->pixel sm [1 2] 1)]
      (is (= [257 253] pixel)))))

(deftest test-bbox
  (testing ""
    (let [bbox (xyz->bbox sm 0 0 0)]
      (is (= [[180.0 -85.05112877980659] [-180.0 85.0511287798066]] bbox)))))
