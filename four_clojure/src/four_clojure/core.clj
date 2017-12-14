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

(def symmetric-difference
  "4clojure #88"
  (fn [set1 set2]
    (clojure.set/union (clojure.set/difference set1 set2)
                       (clojure.set/difference set2 set1))))

(def dot-product
  "4clojure #143"
  (fn [coll1 coll2]
    (->> (map * coll1 coll2)
         (reduce +))))

(def read-binary-number
  "4clojure #122"
  (fn [num]
    (let [pow (fn [b]
                (if (zero? b)
                  1
                  (apply * (repeat b 2))))]
      (map (fn [a b]
             (* (read-string b)
                (pow a)))
           (range (count num))
           (->> num
                (#(clojure.string/split % #""))
                (map read-string)
                reverse)))))
