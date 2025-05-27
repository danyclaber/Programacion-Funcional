#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Requerimientos de librerías y módulos propios
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require web-server/servlet          ; Provee funciones para crear el servidor web
         web-server/servlet-env     ; Ambiente de ejecución del servlet
         net/url                    ; Manejo de URLs
         json                       ; Soporte para codificación/decodificación JSON
         "polinomios.rkt"           ; Módulo con operaciones sobre polinomios
         "utils.rkt")               ; Módulo con utilidades auxiliares (parseo, logs, etc.)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Función principal del servidor: 'start'
;; Recibe una solicitud HTTP, procesa parámetros y devuelve un resultado JSON
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (start req)
  ;; === Obtener los parámetros del request ===
  (define bindings (request-bindings req))
  (define p1-str (and (assoc 'p1 bindings) (cdr (assoc 'p1 bindings))))
  (define p2-str (and (assoc 'p2 bindings) (cdr (assoc 'p2 bindings))))
  (define accion  (and (assoc 'accion bindings) (cdr (assoc 'accion bindings))))
  (define polinomio (and (assoc 'polinomio bindings) (cdr (assoc 'polinomio bindings))))

  ;; === Parsear los coeficientes de los polinomios ===
  ;; Si no se recibe el parámetro, se usa una lista vacía
  (define p1 (if p1-str (parse-coefs p1-str) '()))
  (define p2 (if p2-str (parse-coefs p2-str) '()))

  ;; === Ejecutar la operación solicitada (sumar, multiplicar, derivar) ===
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

  ;; === Mostrar en consola los datos recibidos y el resultado (debug/log) ===
  (log-entrada-salida p1-str p2-str accion polinomio resultado)

  ;; === Devolver respuesta al cliente en formato JSON ===
  (response/output
   #:code 200
   #:headers (list 
              (make-header (string->bytes/utf-8 "Content-Type")
                           (string->bytes/utf-8 "application/json"))
              (make-header (string->bytes/utf-8 "Access-Control-Allow-Origin")
                           (string->bytes/utf-8 "*"))) ; Permite CORS
   (lambda (out)
     (write-json (hash 'resultado resultado) out)))) ; Enviar resultado como JSON

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Iniciar el servidor en el puerto 8000 con ruta /api
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(serve/servlet start
               #:launch-browser? #f     ; No abrir navegador automáticamente
               #:servlet-path "/api"   ; Ruta de acceso al endpoint
               #:port 8000)            ; Puerto donde escucha el servidor
