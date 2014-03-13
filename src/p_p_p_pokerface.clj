(ns p-p-p-pokerface)

(def replacements {\T 10, \J 11, \Q 12, \K 13, \A 14})
(def low-ace {\T 10, \J 11, \Q 12, \K 13, \A 1})

(def high-seven                   ["2H" "3S" "4C" "5C" "7D"])
(def pair-hand                    ["2H" "2S" "4C" "5C" "7D"])
(def two-pairs-hand               ["2H" "2S" "4C" "4D" "7D"])
(def three-of-a-kind-hand         ["2H" "2S" "2C" "4D" "7D"])
(def four-of-a-kind-hand          ["2H" "2S" "2C" "2D" "7D"])
(def straight-hand                ["2H" "3S" "6C" "5D" "4D"])
(def low-ace-straight-hand        ["2H" "3S" "4C" "5D" "AD"])
(def high-ace-straight-hand       ["TH" "AS" "QC" "KD" "JD"])
(def flush-hand                   ["2H" "4H" "5H" "9H" "7H"])
(def full-house-hand              ["2H" "5D" "2D" "2C" "5S"])
(def straight-flush-hand          ["2H" "3H" "6H" "5H" "4H"])
(def low-ace-straight-flush-hand  ["2D" "3D" "4D" "5D" "AD"])
(def high-ace-straight-flush-hand ["TS" "AS" "QS" "KS" "JS"])

(defn rank [card]
  (let [[rnk _] card]
    (cond
     (Character/isDigit rnk) (Integer/valueOf (str rnk))
     :else (replacements rnk))))

(defn low-rank [card]
  (let [[rnk _] card]
    (cond
     (Character/isDigit rnk) (Integer/valueOf (str rnk))
     :else (low-ace rnk))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))


(defn pair? [hand]
(= 2 (apply max (vals (frequencies (map rank hand))))))


(defn three-of-a-kind? [hand]
  (= 3 (apply max (vals (frequencies (map rank hand))))))

(defn four-of-a-kind? [hand]
  (= (seq [1 4]) (sort (vals (frequencies (map rank hand))))))

(defn flush? [hand]
  (apply = (map suit hand)))


(defn full-house? [hand]
  (= (seq [2 3]) (sort (vals (frequencies (map rank hand))))))


(defn two-pairs? [hand]
  (or (= (seq [1 2 2]) (sort (vals (frequencies (map rank hand)))))
      (= (seq [1 4]) (sort (vals (frequencies (map rank hand)))))))


;;i'd like to clean this up so alt is only calculated when A is present
(defn straight? [hand]
 ;;re-map for alternate ace handling, give A a value of 1 for use in rank
  (let [[alt-hand] [(replace {"AH" "1H", "AD" "1D", "AC" "1C", "AS" "1S"} hand)]]
   (let [[min-hand max-hand] [(apply min (map rank hand)) (apply max (map rank hand))]]
     (let [[min-alt max-alt] [(apply min (map rank alt-hand)) (apply max (map rank alt-hand))]]
      (or (= (sort (map rank hand)) (range min-hand (+ 1 max-hand)))
          (= (sort (map rank alt-hand)) (range min-alt (+ 1 max-alt))))))))


(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))



(defn value [hand]
  nil)
