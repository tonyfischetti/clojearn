#!/usr/bin/env clj

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Univariate linear regression ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; computes the hypothesis function given
; parameters theta0 and theta1
; (which are the intercept and the only
;  beta coefficient)
(defn make-prediction [intercept coeff x-vec]
  (map #(+ intercept %)
       (map #(* coeff %) x-vec)))

; computed the sum of squares residual for a
; predicted vector to an observed vector
(defn residuals [x-vec y-vec]
  (map #(Math/pow % 2)
       (map - x-vec y-vec)))

; returns the mean of a vector
(defn mean [a-vector]
  (/ (reduce + a-vector) (count a-vector)))

; returns highest value minus value
; in vector
(defn vect-range [a-vector]
  (- (apply max a-vector)
     (apply min a-vector)))


; computes sample standard deviation
(defn st-dev [a-vector]
  (let [themean (mean a-vector)]
    (->> (map #(- % themean) a-vector)
         (map #(Math/pow % 2))
         (reduce +)
         (#(/ % (- (count a-vector) 1)))
         (#(Math/pow % 0.5)))))


; returns a transformed vector where each
; element is subtracted by 'centerer' and
; divided by 'scaler'
; a vector can be converted to z-scores by
; using the mean as the 'centerer' and the
; standard deviation as the 'scaler'
(defn scale [a-vector scaler centerer]
  (map #(/ % scaler)
       (map #(- % centerer) a-vector)))



;   COST FUNCTION
; this uses 0.5 times the mean sum-of-squared
; residuals to make the partial derivatives
; in the gradient descent algorithm easier
(defn cost-function [x-vec y-vec]
  (* 0.5 (mean (residuals x-vec y-vec))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ?!??!???!! (does the fitting and everything)
(defn fit-straight-line [x-vec y-vec]
  (println x-vec)
  (println y-vec))
; adaptive learning rate?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



; testing
(let [test-vec [1 8 2 4 10 3 6]]
  (println (st-dev test-vec))
  (println (scale test-vec 10 5))
  (println (scale test-vec (vect-range test-vec)
                           (mean test-vec)))
  (println (mean test-vec))
  (println (cost-function (make-prediction 0 0 [1 2 3]) [1 2 3])))





