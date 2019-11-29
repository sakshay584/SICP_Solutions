;; ex 2.1

(defn make-rat [n d]
  (if (< (* n d) 0) [(* -1 (Math/abs n)) (Math/abs d)] [(Math/abs n) (Math/abs d)]))
(def a (make-rat 3 4))

;; ex 2.2

(defn make-point [x y]
  [x y])
(defn x-point [a]
  (first a))
(defn y-point [a]
  (second a))

(defn make-segment [s e]
  [s e])
(defn start-point [segment]
  (first segment))
(defn end-point [segment]
  (second segment))

(defn mid-point [segment]
  [(/ (+ (x-point (start-point segment)) (x-point (end-point segment))) 2)
   (/ (+ (y-point (start-point segment)) (y-point (end-point segment))) 2) ])

;; ex 2.4

(defn conss [x y]
  (fn [m] (m x y)))

(defn car [lst]
  (lst (fn [x y] x)))

(defn cdr [lst]
  (lst (fn [x y] y)))

;; ex 2.5
(defn conss [a b]
  (* (Math/pow 2 a) (Math/pow 3 b)))

(defn find-pow [n d]
  (letfn [(iter [n x]
            (if (= (rem n d) 0)
              (iter (/ n d) (+ 1 x))
              x))]
    (iter n 0)))
(defn car [n]
  (find-pow (int n) 2))
(defn cdr [n]
  (find-pow (int n) 3))

;; ex 2.6

(def one (fn [f]
            (fn [x] (f x))))
(def two (fn [f] (fn [x] (f (f x)))))

;; ex 2.7

(defn interval [a b]
  [a b])
(defn upper-bound [x]
  (apply max x))
(defn lower-bound [x]
  (apply min x))

;; ex 2.8

(defn sub-interval [x y]
  (interval (- (upper-bound x) (lower-bound y))
    (- (lower-bound x) (upper-bound y))))

;; ex 2.9

;; addition
width = 1/2 * ((aH + bH) - (aL + bL))
= 1/2 * ((aH - aL) + (bH - bL))
= width of interval a + width of interval b
;; subtraction
width = 1/2 * ((aH - bL) - (aL - bH))
= 1/2 * ((aH - aL) + (bH - bL))
= width of interval a + width of interval b

;; ex 2.12
(defn make-center-percent [c p]
  (let [ratio (/ p 100.0)]
    (make-interval (+ c (* c ratio))
      (- c (* c ratio)))))

(defn center [i]
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(defn percent [i]
  (let [width (- (upper-bound i) (center i))]
    (* 100 (/ width (center i)))))
