(ns soko.ssr-test
  (:require [clojure.test :refer [deftest is]]
            [shitsuke.hiccup :as hic]
            [soko.ssr :as ssr]
            [soko.views :as views]))

(deftest root-html-stable-test
  (let [html (ssr/root-html)]
    (is (clojure.string/starts-with? html "<!doctype html>"))
    (is (clojure.string/includes? html "Warehouse / logistics"))
    (is (clojure.string/includes? html "shp_1"))
    (is (clojure.string/includes? html "1234")))) ; tracking

(deftest ssr-parity-test
  (is (= (hic/->html (views/root (ssr/sample-db)))
         (hic/->html (views/root (ssr/sample-db))))))
