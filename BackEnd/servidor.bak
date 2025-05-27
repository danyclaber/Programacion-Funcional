#lang racket

(require web-server/servlet
         web-server/servlet-env
         net/url
         json)

;; ========================================
;; CONVERSIÓN DE CADENAS A LISTAS DE COEFICIENTES
;; ----------------------------------------
;; Convierte un string separado por comas en una lista de números
;; Ejemplo: "3,5,2" -> '(3 5 2)
;; ========================================
(define (parse-coefs str)
  (map string->number (string-split str ",")))

;; ========================================
;; OPERACIONES CON POLINOMIOS
;; ----------------------------------------

;; SUMA DE POLINOMIOS
;; ----------------------------------------
;; Suma dos polinomios representados como listas de coeficientes
;; Ejemplo: '(1 2 3) + '(4 5) -> '(5 7 3)
;; ========================================
(define (sumar-polinomios p1 p2)
  (cond
    [(empty? p1) p2]
    [(empty? p2) p1]
    [else (cons (+ (car p1) (car p2)) (sumar-polinomios (cdr p1) (cdr p2)))]))

;; MULTIPLICACIÓN DE POLINOMIOS
;; ----------------------------------------
;; Funciones auxiliares para multiplicar polinomios
;; ========================================
(define (agregar-ceros p n)
  (if (= n 0)
      p
      (cons 0 (agregar-ceros p (- n 1)))))

(define (multiplicar-coef-polinomio coef p)
  (map (lambda (x) (* coef x)) p))

(define (multiplicar-polinomios p1 p2)
  (if (empty? p1)
      '()
      (sumar-polinomios
       (agregar-ceros (multiplicar-coef-polinomio (car p1) p2) 0)
       (agregar-ceros (multiplicar-polinomios (cdr p1) p2) 1))))

;; DERIVADA DE POLINOMIO
;; ----------------------------------------
;; Calcula la derivada de un polinomio representado como lista de coeficientes
;; Ejemplo: '(3 -2 1) -> '(-2 2)
;; ========================================
(define (derivar pol)
  (define (derivar-aux pol grado)
    (cond
      [(empty? pol) '()]
      [(= grado 0) (derivar-aux (rest pol) 1)] ; omite término constante
      [else (cons (* grado (first pol))
                  (derivar-aux (rest pol) (+ grado 1)))]))
  (derivar-aux pol 0))

;; ========================================
;; FORMATEO DE POLINOMIOS PARA SALIDA AMIGABLE
;; ----------------------------------------
;; Representa un polinomio como una lista de listas (coef x grado)
;; ========================================
(define (formatear-polinomio p)
  (define (aux p n)
    (if (empty? p)
        '()
        (cons (list (car p) 'x n)
              (aux (cdr p) (+ n 1)))))
  (aux p 0))

;; ========================================
;; SERVICIO WEB: MANEJO DE PETICIONES HTTP
;; ----------------------------------------
;; Lee parámetros, realiza la operación solicitada y devuelve resultado JSON
;; ========================================
(define (start req)
  (define bindings (request-bindings req))
  (define p1-str (and (assoc 'p1 bindings) (cdr (assoc 'p1 bindings))))
  (define p2-str (and (assoc 'p2 bindings) (cdr (assoc 'p2 bindings))))
  (define accion  (and (assoc 'accion bindings) (cdr (assoc 'accion bindings))))
  (define polinomio (and (assoc 'polinomio bindings) (cdr (assoc 'polinomio bindings))))

  (displayln "--------------------------------------------")
  (printf "Datos recibidos del front: p1='~a', p2='~a', accion='~a', polinomio='~a'\n"
          p1-str p2-str accion polinomio)

  (define p1 (if p1-str (parse-coefs p1-str) '()))
  (define p2 (if p2-str (parse-coefs p2-str) '()))

  (define resultado
    (cond
      [(equal? accion "sumar") (sumar-polinomios p1 p2)]
      [(equal? accion "multiplicar") (multiplicar-polinomios p1 p2)]
      [(equal? accion "derivar")
       (cond
         [(equal? polinomio "p1") (derivar p1)]
         [(equal? polinomio "p2") (derivar p2)]
         [else "Falta parámetro 'polinomio' para derivar"])]
      [else "Acción no reconocida"]))

  (printf "Resultado enviado al front: ~a\n" resultado)
  (displayln (formatear-polinomio (if (list? resultado) resultado '())))
  (displayln "--------------------------------------------")

  (response/output
   #:code 200
   #:headers (list 
               (make-header (string->bytes/utf-8 "Content-Type")
                            (string->bytes/utf-8 "application/json"))
               (make-header (string->bytes/utf-8 "Access-Control-Allow-Origin")
                            (string->bytes/utf-8 "*")))
   (lambda (out)
     (write-json (hash 'resultado resultado) out))))

;; ========================================
;; INICIO DEL SERVIDOR WEB
;; ----------------------------------------
;; Levanta el servidor en el puerto 8000 en la ruta /api
;; ========================================
(serve/servlet start
               #:launch-browser? #f
               #:servlet-path "/api"
               #:port 8000)
