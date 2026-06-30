(ns soko.shipment
  "Shipment model (pure). Shipment{:id :order-id :warehouse :carrier :tracking
  :status :lines}. Status: :pending → :picked → :packed → :shipped → :delivered
  (| :cancelled from pending/picked). Lines are [{:sku :qty}]."
  (:refer-clojure :exclude [deliver])
  (:require [chobo.ledger :as ledger]))

(defrecord Shipment [id order-id warehouse carrier tracking status lines])

(def statuses #{:pending :picked :packed :shipped :delivered :cancelled})
(def transitions
  {:pending  #{:picked :cancelled}
   :picked   #{:packed :cancelled}
   :packed   #{:shipped}
   :shipped  #{:delivered}
   :delivered #{}
   :cancelled #{}})

(defn shipment [m] (merge {:status :pending :lines []} m))

(defn can-transition? [from to] (contains? (get transitions from #{}) to))
(defn transition [s to] (when (can-transition? (:status s :pending) to) (assoc s :status to)))

(defn pick [s] (transition s :picked))
(defn pack [s] (transition s :packed))
(defn ship [s] (transition s :shipped))
(defn deliver [s] (transition s :delivered))
(defn cancel [s] (transition s :cancelled))

(defn set-tracking [s tracking] (assoc s :tracking tracking))

(defn shipment-activity
  "Project a shipment event onto chobo.ledger as a :warehouse activity (kind :shipment)."
  [s opts]
  (ledger/activity
   (merge {:lane :warehouse :kind :shipment
           :title (str "Shipment " (:id s))
           :state (:status s :pending)
           :props {:shipment-id (:id s) :order-id (:order-id s)
                   :warehouse (:warehouse s) :carrier (:carrier s)}}
          opts)))
