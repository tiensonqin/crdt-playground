(ns crdt.cause
  "Demo of cause usage including list, map and causal.base merges."
  (:require [causal.core :as cause]
            [causal.collections.shared :as s]))

(def site-id-1 (s/new-site-id))
(def site-id-2 (s/new-site-id))

(defn get-node
  ([causal-tree site-id value]
   (get-node causal-tree site-id value ::last))
  ([causal-tree site-id value idx]
   (let [ids (keys (::s/nodes (.-ct causal-tree)))
         id (if (= ::last idx)
              (last ids)
              (nth ids idx))]
     (cause/node (inc (cause/get-ts causal-tree))
                site-id
                id
                value))))

;;; List merge

(def cl (atom (cause/list :a :b)))

(def node1 (get-node @cl site-id-1 :c))
(def cl1 (cause/insert @cl node1))

(def node2 (get-node @cl site-id-2 cause/hide))
(def cl2 (cause/insert @cl node2))

(def merge-cl (cause/merge cl1 cl2))
;; #causal/list (:a :c)


;;; Map merge

(def cm (atom (cause/map :a 1 :b 2)))
(def cm1 (atom @cm))
(def cm2 (atom @cm))

(defn get-map-node
  [causal-tree site-id key value]
  (let [ids (keys (::s/nodes (.-ct causal-tree)))]
    (s/new-node (inc (cause/get-ts causal-tree))
                site-id
                key
                value)))

(swap! cm1 cause/insert (get-map-node @cm1 site-id-1 :c 3))
(swap! cm1 cause/insert (get-map-node @cm1 site-id-1 :d 4))

(swap! cm2 cause/insert (get-map-node @cm1 site-id-2 :e 5))

(swap! cm1 dissoc :b)

(def merge-cm (cause/merge @cm1 @cm2))
;; {[5 "p7iePGVtun6br" 0] :e,
;;  [4 "p7iePGVtun6br" 0] :d,
;;  [3 "p7iePGVtun6br" 0] :c,
;;  [1 "d4jPTj4d2N7Va" 0] :a}


;;; Base merge

(def cb (atom (cause/base)))

;; pages
(def starting-data {:page-1
                    [{:id 3 :content 3 :parent 1 :left 1}
                     {:id 4 :content 4 :parent 1 :left 3}]})
(swap! cb cause/transact [[nil nil starting-data]])
(def root-uuid (cause/get-uuid (cause/get-collection @cb)))

;; Another client
(def cb2 (atom (cause/set-site-id @cb (s/new-site-id))))

;; Add another page
(def tx-1 [[root-uuid :page-2
            [{:id 5 :content 5 :parent 2 :left 2}
             {:id 6 :content 6 :parent 2 :left 5}]]])
(swap! cb cause/transact tx-1)

;; Add another block to page 1
(def page-1-ref (:page-1 (cause/get-collection @cb)))
(def cause (first (last (seq (cause/get-collection @cb (:page-1 (cause/get-collection @cb)))))))
(def tx-2 [[(name page-1-ref) cause
            {:id 7 :content 7 :parent 1 :left 4}]])
(swap! cb cause/transact tx-2)
;; #causal/base {:page-1 ({:id 3, :content 3, :parent 1, :left 1}
;;                        {:id 4, :content 4, :parent 1, :left 3}
;;                        {:id 7, :content 7, :parent 1, :left 4}),
;;               :page-2 ({:id 5, :content 5, :parent 2, :left 2}
;;                        {:id 6, :content 6, :parent 2, :left 5})}


;; Delete the last block in page 1
(def page-1-ref (:page-1 (cause/get-collection @cb2)))
(def cause (first (last (seq (cause/get-collection @cb2 (:page-1 (cause/get-collection @cb2)))))))
(def tx-3 [[(name page-1-ref) cause cause/hide]])
(swap! cb2 cause/transact tx-3)
;; #causal/base {:page-1 ({:id 3, :content 3, :parent 1, :left 1})}

;; (def merge-base cause/transact)

(swap! cb cause/transact tx-3)
(swap! cb2 cause/transact (concat tx-1 tx-2))

(assert (= (cause/causal->edn @cb)
           (cause/causal->edn @cb2)))

;; TODO: sync between clients and client/server
