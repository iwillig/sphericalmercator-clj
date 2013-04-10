(ns sphericalmercator-clj.core-test
  (:use clojure.test
        sphericalmercator-clj.core))

;; 15/9646/12322
;; 15/5570/13076

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
      (is (= [180.0 -85.05112877980659 -180.0 85.0511287798066] bbox))))

  (testing "bbox conversation"
    (let [bbox (xyz->bbox sm 0 0 1)
          bk   (xyz->bbox sm 603 770 11)
          poco (xyz->bbox sm 4825 6162 14)]
      (println bk)
      (is (= [-180 -85.05112877980659 0 0] bbox)))))
