(ns labyrinth.solution)

(defn start-pos [lab]
  (if (->> (apply concat lab)
           (filter #{1})
           (count)
           (= 1))
    [0 0]))

(defn can-go? [cell]
  (not (or (= cell :W)
           (= cell 1)
           (nil? cell))))

(defn neighbours [lab pos]
  (let [directions [[0 1]
                    [1 0]
                    [0 -1]
                    [-1 0]]
        [posx posy] pos]
    (->> (for [[dx dy] directions]
           (let [x (+ dx posx)
                 y (+ dy posy)
                 what-is-there (get-in lab [x y])]
             [x y what-is-there]))
         (filter (fn [[x y what-is-there]]
                   (can-go? what-is-there)))
         (map butlast))))

(def choose-from
  first)

(defn solve-from [lab pos]
  (let [next-pos (choose-from (neighbours lab pos))
        next-lab (if next-pos
                   (assoc-in lab next-pos 1))]
    (cond
      (= :Exit (get-in lab next-pos)) next-lab
      (nil? next-pos) ["unable to exit"]
      :else (solve-from next-lab next-pos))))

(defn solve [lab]
  (let [pos (start-pos lab)]
    (if (nil? pos)
      lab
      (solve-from lab pos))))
