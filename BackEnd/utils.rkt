#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exportación de funciones públicas (disponibles para otros módulos)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide parse-coefs
         formatear-polinomio
         log-entrada-salida)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Requerimientos de módulos estándar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require racket/string) ; Para usar funciones de manipulación de cadenas

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Función: parse-coefs
;; Descripción: Convierte una cadena de coeficientes separados por comas
;;              (ej. "1,2,3") en una lista de números (ej. '(1 2 3))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (parse-coefs str)
  (map string->number (string-split str ",")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Función: formatear-polinomio
;; Descripción: Transforma una lista de coeficientes en una representación
;;              con potencias de x. Ejemplo: '(1 2 3) → '((1 x 0) (2 x 1) (3 x 2))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (formatear-polinomio p)
  ;; Función auxiliar recursiva que lleva el contador de potencias
  (define (aux p n)
    (if (empty? p)
        '()
        (cons (list (car p) 'x n)
              (aux (cdr p) (+ n 1)))))
  (aux p 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Función: log-entrada-salida
;; Descripción: Imprime en consola los datos recibidos desde el front y el
;;              resultado devuelto, con formato legible para depuración.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (log-entrada-salida p1-str p2-str accion polinomio resultado)
  (displayln "--------------------------------------------")
  (printf "Datos recibidos del front: p1='~a', p2='~a', accion='~a', polinomio='~a'\n"
          p1-str p2-str accion polinomio)
  (printf "Resultado enviado al front: ~a\n" resultado)
  ;; Muestra la forma polinómica formateada si el resultado es una lista
  (displayln (formatear-polinomio (if (list? resultado) resultado '())))
  (displayln "--------------------------------------------"))
