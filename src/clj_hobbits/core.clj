(ns clj-hobbits.core
  (:gen-class))

;; fwpd test ;;;;;;;;;;;;;;;;;;;;;
(def filename "suspects.csv")

(def vamp-keys [:name :glitter-index])

(defn str->int
  [str]
  (Integer. str))

(def conversions {:name identity 
                  :glitter-index str->int})

(defn convert
  [vamp-key value]
  ((get conversions vamp-key) value))

(defn parse
  "Convert a CSV into rows of columns"
  [string]
  (map #(clojure.string/split % #",")
       (clojure.string/split string #"\r\n")))

(defn mapify-test
  "Return a seq of maps like {:name \"Edward Cullen\" :glitter-index 10"
  [rows]
  (map #(into {}
              {:name (first %)
               :glitter-index (convert :glitter-index (second %))})
       rows))

(defn mapify
  "Return a seq of maps like {:name \"Edward Cullen\" :glitter-index 10"
  [rows]
  (map (fn [unmapped]
         (into {} (map (fn [key val] (vector key (convert key val))) vamp-keys unmapped)))
       rows))

(defn glitter-filter
  [min-glitter records]
  (filter #(>= (:glitter-index %) min-glitter) records))
;; end fwpd test ;;;;;;;;;;;;;;;;;

(def asym-hobbit-body-parts [{:name "head" :size 3}
                             {:name "left-eye" :size 1}
                             {:name "left-ear" :size 1}
                             {:name "mouth" :size 1}
                             {:name "nose" :size 1}
                             {:name "neck" :size 2}
                             {:name "left-shoulder" :size 3}
                             {:name "left-upper-arm" :size 3}
                             {:name "chest" :size 10}
                             {:name "back" :size 10}
                             {:name "left-forearm" :size 3}
                             {:name "abdomen" :size 6}
                             {:name "left-kidney" :size 1}
                             {:name "left-hand" :size 2}
                             {:name "left-knee" :size 2}
                             {:name "left-thigh" :size 4}
                             {:name "left-lower-leg" :size 3}
                             {:name "left-achilles" :size 1}
                             {:name "left-foot" :size 2}])

(defn matching-part
  [part]
  {:name (clojure.string/replace (:name part) #"^left-" "right-")
   :size (:size part)})

(defn symmetrize-body-parts
  "Expects a seq of maps that have a :name and :size"
  [asym-body-parts]
  (loop [remaining-asym-parts asym-body-parts
         final-body-parts []]
    (if (empty? remaining-asym-parts)
      final-body-parts
      (let [[part & remaining] remaining-asym-parts]
        (recur remaining
               (into final-body-parts
                     (set [part (matching-part part)])))))))

;;为reduce提供的匿名函数的一般格式为:(f final part). 其中final为输出结果，part为序列项。
(defn better-symmetrize-body-parts
                    "Expects a seq of maps that have a :name and :size"
                    [asym-body-parts]
                    (reduce (fn [final-body-parts part]
                              (into final-body-parts (set [part (matching-part part)])))
                            []
                            asym-body-parts))
;; ！！！最后，可以将symmerize-body-parts抽象成更一般的形式expand-body-parts. 
;; ！！！例如：fn变为函数变量。！！！

(defn hit
  [asym-body-parts]
  (let [sym-parts (better-symmetrize-body-parts asym-body-parts)
        body-part-size-sum (reduce + (map :size sym-parts))
        target (rand body-part-size-sum)]
    (loop [[part & remaining] sym-parts
           accumulated-size (:size part)]
      (if (> accumulated-size target)
        part
        (recur remaining (+ accumulated-size (:size (first remaining))))))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
