#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exportación de funciones públicas (API del módulo)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide sumar-polinomios
         multiplicar-polinomios
         derivar)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Función: sumar-polinomios
;; Descripción: Suma dos polinomios representados como listas de coeficientes.
;; Ejemplo: '(1 2 3) + '(4 5) → '(5 7 3)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (sumar-polinomios p1 p2)
  (cond
    [(empty? p1) p2]                             ; Si p1 está vacío, devolver p2
    [(empty? p2) p1]                             ; Si p2 está vacío, devolver p1
    [else                                        ; Sumar elemento a elemento
     (cons (+ (car p1) (car p2))
           (sumar-polinomios (cdr p1) (cdr p2)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Función: agregar-ceros
;; Descripción: Añade `n` ceros al inicio de una lista de coeficientes.
;; Utilizada para alinear potencias al multiplicar polinomios.
;; Ejemplo: (agregar-ceros '(3 4) 2) → '(0 0 3 4)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (agregar-ceros p n)
  (if (= n 0)
      p
      (cons 0 (agregar-ceros p (- n 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Función: multiplicar-coef-polinomio
;; Descripción: Multiplica todos los coeficientes de un polinomio por un escalar.
;; Ejemplo: 3 * '(1 2 3) → '(3 6 9)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (multiplicar-coef-polinomio coef p)
  (map (lambda (x) (* coef x)) p))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Función: multiplicar-polinomios
;; Descripción: Realiza la multiplicación de dos polinomios.
;; Usa multiplicación distributiva y desplazamiento por potencias.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (multiplicar-polinomios p1 p2)
  (if (empty? p1)
      '() ; Caso base: si p1 está vacío, el resultado es el neutro de la suma
      (sumar-polinomios
       (agregar-ceros (multiplicar-coef-polinomio (car p1) p2) 0)
       (agregar-ceros (multiplicar-polinomios (cdr p1) p2) 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Función: derivar
;; Descripción: Deriva un polinomio dado como lista de coeficientes.
;; Ejemplo: '(5 3 2) representa 5 + 3x + 2x², su derivada es 3 + 4x → '(3 4)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (derivar pol)
  ;; Función auxiliar con contador de grado
  (define (derivar-aux pol grado)
    (cond
      [(empty? pol) '()]                         ; Caso base: fin de la lista
      [(= grado 0) (derivar-aux (rest pol) 1)]   ; El término constante desaparece
      [else
       (cons (* grado (first pol))
             (derivar-aux (rest pol) (+ grado 1)))]))
  (derivar-aux pol 0))
