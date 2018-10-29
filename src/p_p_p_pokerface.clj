(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank _] card] 
    (get {\2 2 \3 3 \4 4 \5 5 \6 6 \7 7 \8 8 \9 9 \T 10 \J 11 \Q 12 \K 13 \A 14} rank)
  )
)

(defn suit [card]
  (let [[_ suit] card] (str suit)))

(defn pair? [hand]
  (let [count (apply max (vals (frequencies (map rank hand)))) ]
    (> count 1)
    ))

(defn three-of-a-kind? [hand]
  (let [count (apply max (vals (frequencies (map rank hand)))) ]
    (== count 3)
    ))

(defn four-of-a-kind? [hand]
  (let [count (apply max (vals (frequencies (map rank hand)))) ]
    (== count 4)
    ))

(defn flush? [hand]
  (== (apply max (vals (frequencies (map suit hand))))5))

(defn full-house? [hand]
  (= (seq [2 3]) (sort (vals (frequencies (map rank hand))))))

(defn two-pairs? [hand]
  (or
   (= 2 (get (frequencies (vals (frequencies (map rank hand))))2))
   (= 1 (get (frequencies (vals (frequencies (map rank hand))))4)) 
  )
)

(defn straight? [hand]
  (let [ sorted (sort (map rank hand)) smallest (first sorted)
        sorted2 (sort (replace {14 1} sorted)) secondSmallest (first sorted2) ]
    (or
      (= sorted (range smallest (+ smallest 5)))
      (= sorted2 (range secondSmallest (+ secondSmallest 5))))))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn value [hand]
 (cond
    (straight-flush? hand) 8
    (four-of-a-kind? hand) 7
    (full-house? hand) 6
    (flush? hand) 5
    (straight? hand) 4
    (three-of-a-kind? hand) 3
    (two-pairs? hand) 2
    (pair? hand) 1
    :else 0))
