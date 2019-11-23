(defproject chapter_1 "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.0"]]
  :repl-options {:init-ns Chapter_1.core})


; ex 1.2
(/ (+ 5
     4
     (- 2 (- 3 (+ 6 (/ 4 5)))))
  (* 3
    (- 6 2)
    (- 2 7)))

;; ex 1.3

(defn larger-square [x y z]
  (cond
    (and (> x y) (> x z)) (if (> y z) (+ (* x x) (* y y)) (+ (* x x) (* z z)))
    (and (> y x) (> y z)) (if (> x z) (+ (* y y) (* x x)) (+ (* y y) (* z z)))
    (and (> z x) (> z y)) (if (> x y) (+ (* z z) (* x x)) (+ (* z z) (* y y)))
    ))

;; ex 1.3 Alt

(defn square [x] (* x x))

(defn sum-sq [x y] (+ (square x) (square y)))

(defn larger-sq [x y z]
  (cond
   (and (> x z) (> y z)) (sum-sq x y)
    (and (> x y) (> z y )) (sum-sq x z)
    (and (> y x) (> z x )) (sum-sq  y z)

    ))

;; ex 1.5
(defn p [] (p))
(defn test [x y]
  (if (= x 0)
    0 (p)))

;; ex 1.7

;;(defn good-enough? [guess x] (< (obs (- guess (improve guess x))) (* 0.001 guess)))

(defn average [x y] (/ (+ x y) 2))
(defn improve [guess x] (average guess (/ x guess)))
;;(defn good-enough? [guess x] (< (Math/abs (- x (* guess guess)) )  0.001))
(defn good-enough? [guess x] (< (Math/abs (- guess (improve guess x))) (* 0.001 guess)))
(defn srt [guess x]
  (if (good-enough? guess x) (improve guess x) (srt (improve guess x) x)))

;; ex 1.8

(defn cbrt [x]
  (letfn [  (improve [guess]
              (/ (+ (/ x (* guess guess))
                   (* 2 guess))
                3))
          (good-enough? [guess] (< (Math/abs (- guess (improve guess))) (* 0.001 guess)))
          (cb-irt [guess] (if (good-enough? guess) (improve guess) (cb-irt (improve guess) )))]
    (cb-irt 1.0)))


;; factorial iter ]

(defn fact [x]
  (letfn [(fact-itr [ans n]
          (if (= n 1) ans (fact-itr (* n ans) (- n 1))))]
    (fact-itr 1 x)))

;; ex 1.11

(defn f [x]
  (if (< x 3)
    x
    (+ (f (- x 1)) (* 2 (f (- x 2))) (* 3 (f (- x 3))))))


(defn fi [a b c n]
  (if (< n 3)
    c
    (fi b c (+ c (* 2 b) (* 3 a )) (- n 1))))

;; ex 1.12
(defn av [] (str "Pro " (* 2 3) "Sum " (+ 2 3)))

(defn ple [n e]
  (if (or (= e n) (= e 1))
    1
    (+ (ple (- n 1) (- e 1)) (ple (- n 1) e ) )))

(defn print-ele [a]
  (str "." a))

;; ex 1.16

(defn square [n] (* n n))

(defn fast-exp [ans b n]
  (cond (= n 0) ans
    (odd? n) (fast-exp (* ans b) b (- n 1))
    (even? n) (fast-exp ans (square b) (/ n 2))))

;; ex 1.18

(defn multi [a b]
  (letfn [(m-itr [ans a b]
            (cond (= b 0) ans
              (odd? b) (m-itr (+ ans a) a (- b 1))
              (even? b) (m-itr ans (* 2 a) (/ b 2))))]
    (m-itr 0 a b)))

;; ex 1.19
(fib-iter a b
  (+ (* p p) (* q q))
  (+ (* q q) (* 2 p q))
  (/ count 2))


;; ex 1.21
(defn smallest-dvsr [n]
  (letfn [(dvsr-itr [a]
            (cond (> (square a) n) n
              (= (rem n a) 0) a
              :else (dvsr-itr (+ a 1))
                          ))]
    (dvsr-itr 2)))

;;ex 1.22

(defn prime? [x]
  (= (smallest-dvsr x) x) )


;; ex 1.27


(defn expmod [base exp m]

  (cond (= exp 0) 1
    (even? exp) (mod (square (expmod base (/ exp 2) m))
                  m)
    :else (mod (* base (expmod base (dec exp) m))
            m)))

(defn fermat-test [n]
  (let [rand (int (inc (rand (dec n))))]
    (= (expmod rand n n) rand)))


(defn check-fermat [n a]
  (= (expmod a n n) a))

(defn fast-prime [n times]
  (every? identity
    (map fermat-test (take times (repeat n)))))


(defn fool-fermat [n]
  (every? identity
    (map #(check-fermat n %) (range 1 n))))

;; ex 1.29

(defn sqr [x] (* x x))
(defn cube [x] (* x x x))

(defn sum [term a next b]
  (if (> a b)
    0
    (+ (term a) (sum term (next a) next b))))



(defn simple-intg [f a b n]
  (let [h (/ (- b a) n)]
    (letfn [(term [k]
              (* (f (+ a (* k h)))
                (if (even? k) 2 4)))]
      (/ (* h
           (sum term 1 inc n))
        3))))

;; ex 1.30

(defn product [term a next b]
  (if (> a b)
    1
    (* (term a) (product term (next a) next b))))

;; ex 1.32

(defn accumulate  [combiner null-value term a next b]
  (if (> a b)
    null-value
    (combiner (term a) (accumulate combiner null-value term (next a) next b))))

;; ex 1.34

(defn f [g]
  (g 2))


;; ex 1.35

(defn fixed-point [f guess]
  (letfn [(next [x] (println "Trying" guess) (f x))
          (close-enough? [x y] (< (Math/abs (- x y)) 0.0001))]
    (if (close-enough? guess (next guess))
    (next guess)
    (fixed-point f (next guess)))))


;; ex 1.36


(def ex-1-36-answer (fixed-point #(/ (Math/log 1000)
                                    (Math/log %)) 1.1))

(def ex-1-36-avg-damped
  (fixed-point #(/ (+ %
                     (/ (Math/log 1000)
                       (Math/log %)))
                  2) 1.1))

;; exp 1.37
(defn id [x] 1.0)

(defn cont-fract [n d k]
  (letfn [(iter [i]
            (if (= i k)
              0
              (/ (n i) (+ (d i) (iter (inc i))))))]
    (iter 1)))

;; ex 1.39
(defn tan-d [x]
  (- (* 2 x) 1))
(defn tan-n [x]
  (sqr x))
(defn tan [x n d k]
  (letfn [(iter [i]
            (if (= i k)
              0
              (/ (n x) (- (d i) (iter (inc i))))))]
    (/ x (iter 1))))


;; ex 1.40

(defn damp [f]
  #(average % (f %)))
(def dx 0.0001)


(< ((cubic 1 2 3) (newtons-method (cubic 1 2 3) 1.0))
  0.00001)

(defn deriv [g]
  (fn [x]
    (/ (- (g (+ x dx)) (g x))
      dx)))
(defn newton-transform [g]
  (fn [x]
    (- x (/ (g x) ((deriv g) x)))))

(defn newtons-method [g guess]
  (fixed-point (newton-transform g) guess))


(defn cubic [a b c]
  (fn [x]
    (+ (* x x x)
      (* a x x)
      (* b x)
      c)))


;; ex 1.41

(defn duble [f]
  (fn [x]
    (f (f x))))

(((duble (duble duble)) inc) 5)

;; ex 1.42
((compose square inc) 6)
(defn compose [f g]
  (fn [x]
    (f (g x))))

;; ex 1.43

(defn repeated [f n]
  (if (= n 0) (fn [x] x)
              (compose f (repeated f (dec n)))))

;; ex 1.44

(defn smooth [f]
  (fn [x]
    (/ (+ (f (- x dx))
         (f x)
         (f (+ x dx)))
      3)))

(defn n-fold-smooth [f n]
  (repeated (smooth f) n))

;; ex 1.46

(defn iterative-improve [good-enough? improve]
  (fn [guess]
    (letfn [(iter [x]
              (if (good-enough? x)
                x
                (iter (improve x))))]
      (iter guess))))














































































































































































