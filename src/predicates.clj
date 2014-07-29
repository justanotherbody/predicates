(ns predicates)

(defn sum-f [f g x]
  (+ (f x) (g x)))

(defn less-than [n]
  (fn [k] (< k n)))

(defn equal-to [n]
  (fn [k] (== n k)))

(defn set->predicate [a-set]
  (fn [k] (contains? a-set k)))

(defn pred-and [pred1 pred2]
  (fn [k] (and (pred1 k) (pred2 k))))

(defn pred-or [pred1 pred2]
  (fn [k] (or (pred1 k) (pred2 k))))

(defn whitespace? [character]
  (Character/isWhitespace character))

(defn blank? [string]
  (every? whitespace? string))

(defn has-award? [book award]
  (contains? (:awards book) award))

(defn HAS-ALL-THE-AWARDS? [book awards]
  (let [book-has-award? (fn [award] (has-award? book award))]
    (every? book-has-award? awards)))

(defn my-some [pred a-seq]
  (let [is-truthy? (fn [x] (boolean x))
        pred-map (map pred a-seq)
        true-preds (filter is-truthy? pred-map)]
    (first true-preds)))

(defn my-every? [pred a-seq]
  (let [complement-pred (complement pred)
        complement-map (map complement-pred a-seq)
        falsy-complements (filter true? complement-map)]
    (empty? falsy-complements)))

(defn prime? [n]
  (let [divides-evenly (fn [x] (= 0 (mod n x)))]
    (not (some divides-evenly (range 2 n)))))

;^^
