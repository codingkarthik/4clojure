(ns four-clojure.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(def product-digits
  "4clojure solution for product-digits #99"
  (fn [x y]
    (let [digits-fn (fn [result rem]
                      (if (>= rem 1)
                        (recur (cons (mod rem 10) result)
                               (int (/ rem 10)))
                        result))]
      (digits-fn [] (* x y)))))

(def group-a-seq
  "4clojure #63"
  (fn [f coll]
    (let [outcomes (map (fn [x]
                          {(f x) x})
                        coll)
          keys-1 (map f coll)
          ans (reduce (fn [a b]
                        (merge-with conj
                                    a
                                    b))
                      (reduce (fn [a b]
                                (assoc a b []))
                              {}
                              (distinct keys-1))
                      outcomes)]
      ans)))

#_(def symmetric-difference
  "4clojure #88"
  (fn [set1 set2]
    (union (difference set1 set2)
           (difference set2 set1))))

(def dot-product
  "4clojure #143"
  (fn [coll1 coll2]
    (->> (map * coll1 coll2)
         (reduce +))))

(def binary
  "4clojure"
  (fn [num-string]
    (let [pow-fn (fn [pow]
                   (apply * (repeat pow 2)))]
      (->> (map-indexed (fn [idx a]
                          (if (= "1" (str a))
                            (pow-fn idx)
                            0))
                        (reverse num-string))
           (reduce +)))))

(def infix
  (fn kth
    ([& args]
     (if-not (= 1 (count args))
       (let [result (first args)
             function (second args)
             input (second (rest args))
             next-args  (drop 3 args)
             final-args (cons (function result input) next-args)]
         (apply kth final-args))
       (first args)))))

(def pascal
  (fn [x]
    (reduce (fn [a b]
              (conj a (/ (* (nth a b) (- (dec x) b))
                         (inc b))))
            [1]
            (range (dec x)))))

(fn mmap
  ([f coll]
   (mmap f coll []))
  ([f coll res]
   (if (empty? coll)
     res
     (lazy-seq (cons (f (first coll))
                     (mmap f (rest coll)))))))

(fn binary-tree? [coll]
  (if (and (coll? coll))
    (if (= 3 (count coll))
      (boolean (and (binary-tree? (second coll))
                    (binary-tree? (second (rest coll)))))
      false)
    (if (= false coll)
      false
      true)))

(def sum-square
  (fn [coll]
    (let [n-t-d (fn ntd
                  ([x]
                   (ntd x []))
                  ([x coll]
                   (if (< x 10)
                     (conj coll x)
                     (ntd (int (/ x 10))
                          (conj coll (rem x 10))))))
          h-fn (fn [x]
                 (let [coll (n-t-d x)]
                   (->> coll
                        (map #(* % %))
                        (apply +)
                        (< x))))]
      (count (filter h-fn coll)))))

(def playing-cards
  "4clojure #128"
  (fn [card]
    (let [rank-map (merge {:D :diamond
                           :S :spade
                           :H :heart
                           :C :club}
                          (apply merge
                                 (map (fn [x]
                                        {(keyword (str x)) (- x 2)})
                                      (range 2 10)))
                          {:T 8
                           :J 9
                           :Q 10
                           :K 11
                           :A 12})]
      rank-map
      (reduce (fn [x y]
                (if (empty? x)
                  (assoc x :suit ((keyword (str y)) rank-map))
                  (assoc x :rank ((keyword (str y)) rank-map))))
              {}
              card))))

(def lcmargs
  (fn [& args]
    (let [hcf (fn hc [x y]
                (cond
                  (= x y) y
                  (> x y) (hc (- x y) y)
                  (< x y) (hc x (- y x))))
          lcm (fn [m n]
                (/ (* m n) (hcf m n)))
          denom (fn [x]
                  (if (= (class x) (class 1))
                    1
                    (denominator x)))]
      (reduce (fn [a b]
                (let [lcm1 (lcm (denom a)
                                (denom b))
                      a-num (/ lcm1 (denom a))
                      b-num (/ lcm1 (denom b))
                      res (/ (+ a-num b-num)
                             lcm1)]))
              1
              args))))
