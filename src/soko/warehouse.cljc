(ns soko.warehouse
  "Warehouse + multi-location stock (pure). Warehouse{:id :name :location :zones}.
  Stock is {sku → {warehouse-id → qty}} (location-aware, vs mise.inventory's
  single {sku→qty}). Transfers move qty between warehouses. Projects to
  chobo.ledger lane :warehouse."
  (:refer-clojure :exclude [available?])
  (:require [chobo.ledger :as ledger]))

(defrecord Warehouse [id name location zones])

(defrecord Stock [levels])

(defn stock [m] (->Stock (or m {})))
(defn stock-from
  "Build stock from a seq of [sku warehouse-id qty]."
  [triples]
  (->Stock
   (reduce (fn [acc [sku wh qty]]
             (assoc-in acc [sku wh] (max 0 qty))) {} triples)))

(defn on-hand
  "Qty of sku at a specific warehouse."
  [st sku wh]
  (get-in st [:levels sku wh] 0))

(defn total-on-hand
  "Total qty of a sku across all warehouses."
  [st sku]
  (reduce + 0 (vals (get-in st [:levels sku] {}))))

(defn available?
  ([st sku wh]
   (available? st sku wh 1))
  ([st sku wh needed]
   (>= (on-hand st sku wh) (max 0 needed))))

(defn adjust
  "Add qty to sku@wh (clamped at 0). Returns new Stock."
  [st sku wh qty]
  (let [cur (on-hand st sku wh)
        new-level (max 0 (+ cur qty))]
    (assoc-in st [:levels sku wh] new-level)))

(defn reserve
  "Decrement sku@wh by qty (clamped at 0)."
  [st sku wh qty]
  (adjust st sku wh (- (max 0 qty))))

(defn restock
  "Increment sku@wh by qty."
  [st sku wh qty]
  (adjust st sku wh (max 0 qty)))

(defn transfer
  "Move qty of sku from wh-from to wh-to (no-op if insufficient). Returns new
  Stock. Does NOT model in-transit (v1 atomic)."
  [st sku wh-from wh-to qty]
  (let [q (max 0 qty)]
    (if (available? st sku wh-from q)
      (-> st (reserve sku wh-from q) (restock sku wh-to q))
      st)))

(defn warehouses-with-sku [st sku]
  (keys (get-in st [:levels sku] {})))

(defn warehouse-activity
  "Project a warehouse event onto chobo.ledger as a :warehouse activity."
  [opts]
  (ledger/activity
   (merge {:lane :warehouse :kind :stock-move} opts)))
