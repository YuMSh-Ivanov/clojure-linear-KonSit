(ns linear)

(defn v+ [& vectors]
  (apply mapv + vectors))

(defn v- [& vectors]
  (apply mapv - vectors))

(defn v* [& vectors]
  (apply mapv * vectors))

(defn vd [& vectors]
  (apply mapv / vectors))

(defn dot [& vectors]
  (if (empty? vectors)
    0
    (apply + (apply v* vectors))))

(defn v*s [vector & scalars]
  (mapv #(* % (apply * scalars)) vector))

(defn m+ [& matrices]
  (apply mapv v+ matrices))

(defn m- [& matrices]
  (apply mapv v- matrices))

(defn m* [& matrices]
  (apply mapv v* matrices))

(defn md [& matrices]
  (apply mapv vd matrices))

(defn m*s [matrix & scalars]
  (mapv #(apply v*s % scalars) matrix))

(defn transpose [matrix]
  (apply mapv vector matrix))

(defn m*v [matrix vector]
  (mapv #(dot % vector) matrix))

(defn m*m [& matrices]
  (reduce (fn [cur next] (mapv #(m*v (transpose next) %) cur)) matrices))

(print ((v+ -19.01 -51.14)))