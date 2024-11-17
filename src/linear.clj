(ns linear)

(defn v+ [& vectors]
  {:pre [(and (not (empty? vectors)) (apply = (map count vectors)) (every? vector? vectors))]}
  (apply mapv + vectors))

(defn v- [& vectors]
  {:pre [(and (not (empty? vectors)) (apply = (map count vectors)) (every? vector? vectors))]}
  (apply mapv - vectors))

(defn v* [& vectors]
  {:pre [(and (not (empty? vectors)) (apply = (map count vectors)) (every? vector? vectors))]}
  (apply mapv * vectors))

(defn vd [& vectors]
  {:pre [(and (not (empty? vectors)) (apply = (map count vectors)) (every? vector? vectors))]}
  (apply mapv / vectors))

(defn dot [& vectors]
  {:pre [(and (not (empty? vectors)) (apply = (map count vectors)) (every? vector? vectors))]}
  (if (empty? vectors)
    0
    (apply + (apply v* vectors))))

(defn v*s [vector & scalars]
  {:pre [(and (every? number? scalars) (vector? vector))]}
  (mapv #(* % (apply * scalars)) vector))

(defn m+ [& matrices]
  {:pre [(and (not (empty? matrices))
              (apply = (map #(count %) matrices))
              (apply = (map #(count (first %)) matrices))
              (reduce #(and %1 %2) (vec (map #(apply = (map count %)) matrices))))]}
  (apply mapv v+ matrices))

(defn m- [& matrices]
  {:pre [(and (not (empty? matrices))
              (apply = (map #(count %) matrices))
              (apply = (map #(count (first %)) matrices))
              (reduce #(and %1 %2) (vec (map #(apply = (map count %)) matrices))))]}
  (apply mapv v- matrices))

(defn m* [& matrices]
  {:pre [(and (not (empty? matrices))
              (apply = (map #(count %) matrices))
              (apply = (map #(count (first %)) matrices))
              (reduce #(and %1 %2) (vec (map #(apply = (map count %)) matrices))))]}
  (apply mapv v* matrices))

(defn md [& matrices]
  {:pre [(and (not (empty? matrices))
              (apply = (map #(count %) matrices))
              (apply = (map #(count (first %)) matrices))
              (reduce #(and %1 %2) (vec (map #(apply = (map count %)) matrices))))]}
  (apply mapv vd matrices))

(defn m*s [matrix & scalars]
  (mapv #(apply v*s % scalars) matrix))

(defn transpose [matrix]
  (apply mapv vector matrix))

(defn m*v [matrix vector]
  (mapv #(dot % vector) matrix))

(defn m*m [& matrices]
  (reduce (fn [cur next] (mapv #(m*v (transpose next) %) cur)) matrices))

(print (m+ [1 [1 2]]))