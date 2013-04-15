(ns sphericalmercator-clj.core
  (:import [java.lang Math]))

;; take from openstreetmap's
;; http://svn.openstreetmap.org/applications/rendering/mapnik/generate_tiles.py
;; also ported from MapBox's sphericalmercator.js
;; https://github.com/mapbox/node-sphericalmercator;
;; based on the TMS spec
;; http://wiki.osgeo.org/wiki/Tile_Map_Service_Specification


(def cache     (atom {}))
(def EPSLN     1.0e-10)
(def D2R       (/ Math/PI 180))
(def R2D       (/ 180 Math/PI))
(def A         6378137)
(def MAXEXTENT 20037508.34)

(defprotocol Convertable
  "A protocol that encapsulates the methods we need to convert between pixel and latlong"
  (lonlat->pixel   [self ll zoom] "Converts a lon lat array to a screen pixel value")
  (pixel->ll       [self px zoom] "Converts screen pixel value to a lon lat")
  (xyz->bbox       [self x y zoom] "Converts xyz to a bounding box of [w s e n]"))

(defrecord SphericalMercator [size Bc Cc Zc Ac]
  Convertable

  (lonlat->pixel [self ll zoom]
    (let [d       ((:Zc self) zoom)
          acZoom  ((:Ac self) zoom)
          f       (Math/min (Math/max (Math/sin (* D2R (ll 1)))  -0.9999) 0.9999)
          x       (Math/round (float (+ d (* (ll 0)  ((:Bc self) zoom)))))
          y       (Math/round
                   (+ d (* (* 0.5 (Math/log (/ (+ 1 f)
                                               (- 1 f))))
                           (- ((:Cc self) zoom)))))]
      [(if (> x acZoom) acZoom x)
       (if (> y acZoom) acZoom y)]))

  (pixel->ll [self px zoom]
    (let [zcZoom     ((:Zc self) zoom)
          g          (/ (- (px 1) zcZoom)  (- ((:Cc self) zoom)))

          lon        (/ (- (px 0) zcZoom) ((:Bc self) zoom))
          lat        (* R2D (-  (* 2 (Math/atan (Math/exp g))) (* 0.5 Math/PI)))]

      [lon lat]))

  ;; returns a vector with the following values
  ;; [left bottom right top]
  (xyz->bbox      [self x y zoom]
    (let [size         (:size self)
          y            (- (- (Math/pow 2 zoom) 1) y)
          lower-left   [(* x size)  (* (+ y 1) size)]
          upper-right  [(* (+ x 1) size) (* y size)]]

      (concat (pixel->ll self lower-left zoom)
              (pixel->ll self upper-right zoom)))))

(defn build-tileset [size]
  (let [steps (take 30 (iterate (partial * 2) size))]
    (SphericalMercator.
     size
     (vec (map #(/ % 360.0) steps))
     (vec (map #(/ % (* 2 Math/PI)) steps))
     (vec (map #(/ % 2) steps))
     (vec steps))))


(defn spherical-mercator [& [size]]
  (let [c (get @cache size)]
    ;; check to see if we already have a cache
    ;; if we don't build the four arrays that we need for
    (when (nil? c)
      (swap! cache assoc size
             (build-tileset size)))
    (@cache size)))
