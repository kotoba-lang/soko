(ns soko.fulfillment-test
  (:require [clojure.test :refer [deftest is]]
            [soko.warehouse :as wh]
            [soko.fulfillment :as ful]))

(def st (wh/stock-from [["ph-m" "wh-tokyo" 10] ["ph-l" "wh-osaka" 5]]))

(deftest can-fulfill-test
  (is (ful/can-fulfill? st "wh-tokyo" [{:sku "ph-m" :qty 5}]))
  (is (not (ful/can-fulfill? st "wh-tokyo" [{:sku "ph-l" :qty 1}])))) ; ph-l only at osaka

(deftest pick-list-test
  (let [pl (ful/pick-list st [{:sku "ph-m" :qty 2} {:sku "ph-l" :qty 1}])]
    (is (= 2 (count pl)))
    (is (= "wh-tokyo" (:warehouse (first pl))))
    (is (= "wh-osaka" (:warehouse (second pl))))))

(deftest fulfillment-plan-test
  (let [r (ful/fulfillment-plan st "ord_1" [{:sku "ph-m" :qty 2} {:sku "ph-l" :qty 1}])]
    (is (:pick-list r))
    (is (= 2 (count (:shipments r))))))

(deftest cannot-fulfill-test
  (let [r (ful/fulfillment-plan st "ord_1" [{:sku "nope" :qty 1}])]
    (is (= :cannot-fulfill (:error r)))))
