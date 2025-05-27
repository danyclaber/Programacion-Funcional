#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                  SUMA DE POLINOMIOS                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (sumar-polinomios p1 p2)
  (cond
    [(empty? p1) p2]
    [(empty? p2) p1]
    [else 
     (cons (+ (car p1) (car p2))
           (sumar-polinomios (cdr p1) (cdr p2)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;               FUNCIONES AUXILIARES                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (agregar-ceros p n)
  (if (= n 0)
      p
      (cons 0 (agregar-ceros p (- n 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;               MULTIPLICACIÓN DE POLINOMIOS         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (multiplicar-coef-polinomio coef p)
  (map (lambda (x) (* coef x)) p))

(define (multiplicar-polinomios p1 p2)
  (define (aux p1 n)
    (if (empty? p1)
        '()
        (sumar-polinomios
         (agregar-ceros (multiplicar-coef-polinomio (car p1) p2) n)
         (aux (cdr p1) (+ n 1)))))
  (aux p1 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;               DERIVADA DE POLINOMIO                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (derivar pol)
  (define (derivar-aux pol grado)
    (cond
      [(empty? pol) '()]
      [(= grado 0) (derivar-aux (cdr pol) 1)]
      [else (cons (* grado (car pol)) (derivar-aux (cdr pol) (+ grado 1)))]))
  (derivar-aux pol 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;               FORMATEAR POLINOMIO A STRING         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (formatear-pol-string p)
  (define (term coef grado)
    (cond
      [(= coef 0) ""]
      [(= grado 0) (number->string coef)]
      [(= grado 1) (string-append (number->string coef) "x")]
      [else (string-append (number->string coef) "x^" (number->string grado))]))
  (define (aux p grado)
    (cond
      [(empty? p) ""]
      [else
       (define t (term (car p) grado))
       (define resto (aux (cdr p) (+ grado 1)))
       (cond
         [(string=? t "") resto]
         [(string=? resto "") t]
         [else (string-append t " + " resto)])]))
  (aux p 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                      EJEMPLOS                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define p1 '(3 -2 1))  ; 3 - 2x + x^2
(define p2 '(1 4))     ; 1 + 4x

(displayln "Suma de polinomios:")
(displayln (formatear-pol-string (sumar-polinomios p1 p2))) ; 4 + 2x + x^2

(displayln "Multiplicación de polinomios:")
(displayln (formatear-pol-string (multiplicar-polinomios p1 p2))) ; 3 + 10x + 2x^2 + 4x^3

(displayln "Derivada de p1:")
(displayln (formatear-pol-string (derivar p1))) ; -2 + 2x


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                      RESULTADOS                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Suma de polinomios:
;4 + 2x + 1x^2
;Multiplicación de polinomios:
;3 + 10x + -7x^2 + 4x^3
;Derivada de p1:
;-2 + 2x