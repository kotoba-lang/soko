(ns soko.fulfillment
  "Fulfillment planning (pure). Given an order's lines [{:sku :qty}] and a
  Warehouse Stock, allocate to a warehouse (first-fit) and produce a pick-list +
  Shipment. Pure; does not mutate stock (caller reserves via soko.warehouse)."
  (:require [soko.warehouse :as wh]
            [soko.shipment :as ship]))

(defn can-fulfill?
  "True if a single warehouse has all lines in stock."
  [st wh-id lines]
  (every? #(wh/available? st (:sku %) wh-id (:qty % 1)) lines))

(defn pick-list
  "Produce a pick-list [{:sku :qty :warehouse}] for lines, allocating each line
  to the first warehouse that has it. Returns nil if any line can't be placed."
  [st lines]
  (let [alloc (fn [line]
                (let [sku (:sku line)
                      qty (:qty line 1)
                      wh (some (fn [w] (when (wh/available? st sku w qty) w))
                               (wh/warehouses-with-sku st sku))]
                  (when wh (assoc line :warehouse wh))))]
    (let [picked (map alloc lines)]
      (when (every? some? picked) (vec picked)))))

(defn fulfillment-plan
  "Plan fulfillment for an order: pick-list + a draft Shipment per warehouse.
  Returns {:pick-list [...] :shipments [...]} or {:error :cannot-fulfill}."
  [st order-id lines]
  (if-let [pl (pick-list st lines)]
    (let [by-wh (group-by :warehouse pl)]
      {:pick-list pl
       :shipments (for [[wh-id wh-lines] by-wh]
                    (ship/shipment {:id (str "shp_" order-id "_" wh-id)
                                    :order-id order-id
                                    :warehouse wh-id
                                    :lines (map #(select-keys % [:sku :qty]) wh-lines)}))})
    {:error :cannot-fulfill}))
