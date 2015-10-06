(ns clojure-solution.core
  (:require [clojure.string :as string]))

(defn parse-int [s]
  (+ 64 (Integer. s)))

(defn parse-line [s]
  (apply str (map (comp char parse-int) (string/split s #" "))))

(defn get-test-rows []
  (let [numSeqs (parse-int (read-line))]
    (vec (map #(%1) (repeat 3 (comp parse-line read-line))))))

(defn index-belongs-to [lstr i]
    (quot i lstr)
)

(defn find-lcp-len [ss1, ss2]
  (apply (comp count str) (take-while (comp not nil?) (map #(when (= %1 %2)  %1) ss1 ss2)))
)

(defn gen-sorted-suffixes [s]
    (->> (range (count s))
         (map (fn [st] [st (subs s st)])) 
         (sort-by last)
         (map first)
         (vec))
)

(defn gen-lcp-array [suffs]
    (let [str-at (fn [i] (last (nth suffs i)))]
    (->> (range (- (count suffs) 1))
         (map (fn[i] 
                (find-lcp-len (str-at i)  (str-at (+ 1 i)))))
    ))
)

(defn gen-sorted-suffixes-str [s]
    (->> (range (count s))
         (map (fn [st] [st (subs s st)])) 
         (sort-by last)
    )
)
  
(defn srcs-predicate-ix [lstr sfs ix win]
    (->> (map (partial + ix) (range win))
         (map (fn [i] (get sfs i)))
         (map (partial index-belongs-to lstr))
         (set)
         (count)
         (= 3)))


(defn find-lcp [ss1, ss2, ss3]
  (apply str (take-while (comp not nil?) (map #(when (and (= %1 %2) (= %1 %3))  %1) ss1 ss2 ss3)))
)



(defn min-lcp-for-window [lcps ix win]
    (->> (map (partial + ix) (range (- win 1)))
         (map (fn [i] (nth lcps i)))
         (vec)
         (apply min)
    )
)

(defn find-lcs [s1 s2 s3]
    (let [s  (clojure.string/join [s1 s2 s3])
          lstr (count s1)
          suffs (gen-sorted-suffixes-str s)
          lcps (gen-lcp-array suffs)
          srcs-pred (partial srcs-predicate-ix lstr (vec (map first suffs)) )
          min-lcps (partial min-lcp-for-window lcps)]
    ;(println suffs)
    ;(println lcps)
    (loop
        [idx 0
         win 2
         mins '()]
        ;(println idx win (srcs-pred idx win))
        ;(println mins)
        (cond
            (and (not (srcs-pred idx win)) 
                (< (+ idx win) (count lcps) ))
                (recur idx (+ 1 win) mins)
            (srcs-pred idx win)
                (recur (+ 1 idx) 2 (concat mins (cons (min-lcps idx win) '())))
            :else
                (apply max (vec mins))
        )
         
    ))
)

(def numTests (parse-int (read-line)))

(dotimes [n numTests] (println (apply find-lcs (get-test-rows))))
