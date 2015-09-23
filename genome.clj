(defn sphere [args]
  (reduce + (for [x args] (* x x))))

(defn exp2 [n]
  (reduce * (repeat n 2)))

(def mutation-rate 0.05)
(def bits-per-arg 10)
(def nargs 2)
(def arg-range [-5.12 5.12])
(def breeding-factor 3)
(def population-size 20)

(def genome-length (* bits-per-arg nargs))
(def states-per-arg (exp2 bits-per-arg))
(def arg-span (let [[amin amax] arg-range] (- amax amin)))


(defn pack [v]
  (reduce + 
    (map * v (iterate (partial * 2) 1))))

(defn unpack [bits x]
  (if (zero? x)
    [0]
    (loop [b bits
           n x
           v []]
      (if (zero? b)
        v
        (recur 
          (dec b)
          (quot n 2) 
          (conj v (mod n 2)))))))

(defn gray [x] 
  (bit-xor x (bit-shift-right x 1)))

(defn ungray [x]
  (loop [r 0
         n x]
    (if (zero? n)
      r
      (recur
        (bit-xor r n)
        (bit-shift-right n 1)))))

(defn encode-arg [x]
  (let [[amin amax] arg-range]
    (unpack bits-per-arg
      (gray 
        (int
          (* (- x amin) 
             (/ states-per-arg arg-span)))))))

(defn decode-arg [v]
  (let [[amin amax] arg-range]
    (+ (* (/ (ungray (pack v))
             states-per-arg) 
          arg-span)
       amin)))

(defn to-args [genome]
  (map decode-arg (partition bits-per-arg genome)))

(defn to-genome [args]
  (into [] 
        (flatten 
          (for [a args] (encode-arg a)))))

(defn rand-genome []
  (take genome-length (repeatedly #(rand-int 2))))

(defn rand-population []
  (repeatedly population-size #(rand-genome)))

(defn breed [genome1 genome2]
  (let [c (rand-int (inc bits-per-arg))]
    (concat (take c genome1) (drop c genome2))))

(defn breed-population [p]
  (apply 
    concat 
    (repeatedly 
      breeding-factor 
      #(map breed p (shuffle p)))))

(defn selection [f p]
  (take 
    population-size
    (sort-by #(f (to-args %)) p)))

(defn mutate-gene [x]
  (if (< (rand) mutation-rate) 
    (let [y (dec x)] (* y y))
    x))

(defn mutate-genome [genome]
  (for [x genome] (mutate-gene x)))

(defn fits? [genome]
  (zero? (sphere (to-args genome))))

(defn trained? [population]
  (some fits? population))

(defn generation [p]
  (selection 
    sphere 
    (map mutate-genome 
         (concat p 
                 (breed-population p)))))

(defn train []
  (loop [p (rand-population)]
    (print-population p)
    (if (trained? p)
      (map to-args p)
      (recur (generation p)))))

(defn print-population [p]
  (doseq [g p] 
    (pprint {:genome g 
             :args   (to-args g) 
             :func   (sphere (to-args g))})))

(defn print-population2 [p]
  (doseq [g p] 
    (pprint (sphere (to-args g)))))