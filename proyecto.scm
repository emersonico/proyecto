#lang eopl

;******************************************************************************************
;;;;; Interpretador para lenguaje con condicionales, ligadura local, procedimientos,
;;;;; procedimientos recursivos, ejecución secuencial y asignación de variables
;;;;; con tipos de datos extendidos, bucles y estructuras de control

;; La definición BNF para las expresiones del lenguaje:
;;
;;  <program>       ::= <expression>
;;                      <a-program (exp)>
;;  <expression>    ::= <number>
;;                      <lit-exp (datum)>
;;                  ::= <boolean>
;;                      <bool-exp (datum)>
;;                  ::= <imaginary-number>
;;                      <imag-exp (datum)>
;;                  ::= <string>
;;                      <string-exp (str)>
;;                  ::= <identifier>
;;                      <var-exp (id)>
;;                  ::= <primitive> ({<expression>}*(,))
;;                      <primapp-exp (prim rands)>
;;                  ::= if <expresion> then <expresion> else <expression>
;;                      <if-exp (exp1 exp2 exp23)>
;;                  ::= let {<identifier> = <expression>}* in <expression>
;;                      <let-exp (ids rands body)>
;;                  ::= var {identifier = <expression>}* in <expression>
;;                      <var-exp (ids rands body)>
;;                  ::= const {identifier = <expression>}* in <expression>
;;                      <const-exp (ids rands body)>
;;                  ::= proc({<identificador>}*(,)) <expression>
;;                      <proc-exp (ids body)>
;;                  ::= (<expression> {<expression>}*)
;;                      <app-exp proc rands>
;;                  ::= letrec  {identifier ({identifier}*(,)) = <expression>}* in <expression>
;;                     <letrec-exp(proc-names idss bodies bodyletrec)>
;;                  ::= begin <expression> {; <expression>}* end
;;                     <begin-exp (exp exps)>
;;                  ::= set <identifier> = <expression>
;;                     <set-exp (id rhsexp)>
;;                  ::= switch (<expression>) {case <expression>: {<expression>}* default: {<expression>}}
;;                     <switch-exp (switch-expr case-vals case-bodies default-body)>
;;                  ::= for <identifier> = <expression> to <expression> do <expression>
;;                     <for-exp (var start-exp end-exp body)>
;;                  ::= while <expression> do <expression>
;;                     <while-exp (condition body)>
;;  <primitive>     ::= + | - | * | add1 | sub1 | complex | sum-complex | sub-complex | mult-complex | div-complex
;;                  ::= vacio | crear-lista | cabeza | cola | vacio? | lista? | append | ref-lista | set-lista
;;                  ::= crear-diccionario | diccionario-set | diccionario-get | diccionario? | claves | valores
;;                  ::= = | < | > | <= | >= | and | or | not | print

;******************************************************************************************

;******************************************************************************************
;Especificación Léxica

(define scanner-spec-simple-interpreter
'((white-sp
   (whitespace) skip)
  (comment
   ("%" (arbno (not #\newline))) skip)  
  (identifier
   (letter (arbno (or letter digit "?" "-" "_"))) symbol)
  (number
   (digit (arbno digit)) number)
  (number
   ("-" digit (arbno digit)) number)
  (number
   (digit (arbno digit) "." digit (arbno digit)) number)
  (number
   ("-" digit (arbno digit) "." digit (arbno digit)) number)
  (imaginary-number
   (digit (arbno digit) "i") symbol)
  (imaginary-number
   ("-" digit (arbno digit) "i") symbol)
  (string
   ("\"" (arbno (not #\")) "\"") string)
  (boolean
   ("true") symbol)
  (boolean
   ("false") symbol)))

;Especificación Sintáctica (gramática)

(define grammar-simple-interpreter
  '((program (expression) a-program)
    (expression (number) lit-exp)
    (expression (boolean) bool-exp)
    (expression (imaginary-number) imag-exp)
    (expression (string) string-exp)
    (expression (identifier) var-exp)
    (expression
     (primitive "(" (separated-list expression ",")")")
     primapp-exp)
    (expression ("if" expression "then" expression "else" expression)
                if-exp)
    (expression ("let" (arbno identifier "=" expression) "in" expression)
                let-exp)
    (expression ("var" identifier "=" expression (arbno ";" identifier "=" expression) "in" expression)
                var-decl-exp)
    (expression ("const" identifier "=" expression (arbno ";" identifier "=" expression) "in" expression)
                const-exp)
    (expression ("proc" "(" (arbno identifier) ")" expression)
                proc-exp)
    (expression ( "(" expression (arbno expression) ")")
                app-exp)
    (expression ("letrec" (arbno identifier "(" (separated-list identifier ",") ")" "=" expression)  "in" expression) 
                letrec-exp)
    
    ; características adicionales
    (expression ("begin" expression (arbno ";" expression) "end")
                begin-exp)
    (expression ("set" identifier "=" expression)
                set-exp)
    (expression ("switch" "(" expression ")" "{" 
                (arbno "case" expression ":" "{" expression "}")
                "default" ":" "{" expression "}"
                "}") switch-exp)
    ; BUCLES AÑADIDOS
    (expression ("for" identifier "=" expression "to" expression "do" expression) for-exp)
    (expression ("while" expression "do" expression) while-exp)
    ;;;;;;

    (primitive ("+") add-prim)
    (primitive ("-") substract-prim)
    (primitive ("*") mult-prim)
    (primitive ("add1") incr-prim)
    (primitive ("sub1") decr-prim)
    (primitive ("complex") complex-prim)
    (primitive ("sum-complex") sum-complex-prim)
    (primitive ("sub-complex") sub-complex-prim)
    (primitive ("mult-complex") mult-complex-prim)
    (primitive ("div-complex") div-complex-prim)
    (primitive ("vacio") vacio-prim)
    (primitive ("crear-lista") crear-lista-prim)
    (primitive ("cabeza") cabeza-prim)
    (primitive ("cola") cola-prim)
    (primitive ("vacio?") vacio-check-prim)
    (primitive ("lista?") lista-check-prim)
    (primitive ("append") append-prim)
    (primitive ("ref-lista") ref-lista-prim)
    (primitive ("set-lista") set-lista-prim)
    (primitive ("longitud") longitud-prim)
    (primitive ("concatenar") concatenar-prim)
    ; Diccionarios
    (primitive ("crear-diccionario") crear-diccionario-prim)
    (primitive ("diccionario-set") diccionario-set-prim)
    (primitive ("diccionario-get") diccionario-get-prim)
    (primitive ("diccionario?") diccionario-check-prim)
    (primitive ("claves") claves-prim)
    (primitive ("valores") valores-prim)
    ; Operaciones básicas
    (primitive ("=") equal-prim)
    (primitive ("<") less-prim)
    (primitive (">") greater-prim)
    (primitive ("<=") less-equal-prim)
    (primitive (">=") greater-equal-prim)
    (primitive ("and") and-prim)
    (primitive ("or") or-prim)
    (primitive ("not") not-prim)
    (primitive ("print") print-prim)))

;Construidos automáticamente:

(sllgen:make-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)))

;*******************************************************************************************
;Parser, Scanner, Interfaz

;El FrontEnd (Análisis léxico (scanner) y sintáctico (parser) integrados)

(define scan&parse
  (sllgen:make-string-parser scanner-spec-simple-interpreter grammar-simple-interpreter))

;El Analizador Léxico (Scanner)

(define just-scan
  (sllgen:make-string-scanner scanner-spec-simple-interpreter grammar-simple-interpreter))

;El Interpretador (FrontEnd + Evaluación + señal para lectura )

(define interpretador
  (sllgen:make-rep-loop  "--> "
    (lambda (pgm) 
      (let ((result (eval-program pgm)))
        (cond
          ((string? result) result)
          ((boolean? result) (if result 'true 'false))
          ((complex-num? result) (display-complex result))
          ((dictionary? result) "#<dictionary>")
          (else result)))) 
    (sllgen:make-stream-parser 
      scanner-spec-simple-interpreter
      grammar-simple-interpreter)))

;*******************************************************************************************
;El Interprete

;eval-program: <programa> -> numero
; función que evalúa un programa teniendo en cuenta un ambiente dado (se inicializa dentro del programa)

(define eval-program
  (lambda (pgm)
    (cases program pgm
      (a-program (body)
                 (eval-expression body (init-env))))))

; Ambiente inicial
(define init-env
  (lambda ()
    (extend-env
     '(x y z)
     (list (direct-target 1)
           (direct-target 5)
           (direct-target 10))
     (empty-env))))

;eval-expression: <expression> <enviroment> -> numero
; evalua la expresión en el ambiente de entrada

;**************************************************************************************
;Definición tipos de datos referencia y blanco

(define-datatype target target?
  (direct-target (expval expval?))
  (indirect-target (ref ref-to-direct-target?)))

(define-datatype reference reference?
  (a-ref (position integer?)
         (vec vector?)))

;**************************************************************************************

(define eval-expression
  (lambda (exp env)
    (cases expression exp
      (lit-exp (datum) datum)
      (bool-exp (datum) (convert-symbol-to-bool datum))
      (imag-exp (datum) 
                (let ((num-str (symbol->string datum)))
                  (let ((num-part (string->number (substring num-str 0 (- (string-length num-str) 1)))))
                    num-part)))
      (string-exp (str) str)
      (var-exp (id) (apply-env-no-ref env id))
      (primapp-exp (prim rands)
                   (let ((args (eval-primapp-exp-rands rands env)))
                     (apply-primitive prim args)))
      (if-exp (test-exp true-exp false-exp)
              (if (true-value? (eval-expression test-exp env))
                  (eval-expression true-exp env)
                  (eval-expression false-exp env)))
      (let-exp (ids rands body)
               (let ((args (eval-let-exp-rands rands env)))
                 (eval-expression body (extend-env ids args env))))
      (var-decl-exp (id1 rand1 ids rands body)
                    (let ((val1 (eval-expression rand1 env)))
                      (let ((all-ids (cons id1 ids))
                            (all-vals (cons (direct-target val1) (eval-let-exp-rands rands env))))
                        (eval-expression body (extend-env all-ids all-vals env)))))
      (const-exp (id1 rand1 ids rands body)
                 (let ((val1 (eval-expression rand1 env)))
                   (let ((all-ids (cons id1 ids))
                         (all-vals (cons (direct-target val1) (eval-let-exp-rands rands env))))
                     (eval-expression body (extend-env-const all-ids all-vals env)))))
      (proc-exp (ids body)
                (closure ids body env))
      (app-exp (rator rands)
               (let ((proc (eval-expression rator env))
                     (args (eval-rands rands env)))
                 (if (procval? proc)
                     (apply-procedure proc args)
                     (eopl:error 'eval-expression
                                 "Attempt to apply non-procedure ~s" proc))))
      (letrec-exp (proc-names idss bodies letrec-body)
                  (eval-expression letrec-body
                                   (extend-env-recursively proc-names idss bodies env)))
      (set-exp (id rhs-exp)
               (begin
                 (setref!
                  (apply-env-ref env id)
                  (eval-expression rhs-exp env))
                 1))
      (begin-exp (exp exps)
                 (let loop ((acc (eval-expression exp env))
                            (exps exps))
                   (if (null? exps) 
                       acc
                       (loop (eval-expression (car exps) env)
                             (cdr exps)))))
      (switch-exp (switch-expr case-vals case-bodies default-body)
                  (process-switch-statement switch-expr case-vals case-bodies default-body env))
      (for-exp (var start-exp end-exp body)
               (eval-for-loop var start-exp end-exp body env))
      (while-exp (condition body)
                 (eval-while-loop condition body env)))))

; Procesa una estructura switch-case
(define process-switch-statement
  (lambda (test-expr case-exprs case-bodies fallback-body env)
    (let ((test-result (eval-expression test-expr env)))
      (define (check-cases remaining-cases remaining-bodies)
        (cond
          ((null? remaining-cases) (eval-expression fallback-body env))
          ((equal? test-result (eval-expression (car remaining-cases) env))
           (eval-expression (car remaining-bodies) env))
          (else (check-cases (cdr remaining-cases) (cdr remaining-bodies)))))
      (check-cases case-exprs case-bodies))))

; Evalúa un bucle for
(define eval-for-loop
  (lambda (iteration-var start-expr end-expr body-expr env)
    (let ((start-value (eval-expression start-expr env))
          (end-value (eval-expression end-expr env)))
      (if (> start-value end-value)
          0
          (let loop ((current-value start-value))
            (if (<= current-value end-value)
                (begin
                  (eval-expression body-expr 
                                   (extend-env (list iteration-var) 
                                               (list (direct-target current-value)) 
                                               env))
                  (loop (+ current-value 1)))
                0))))))

; Evalúa un bucle while
(define eval-while-loop
  (lambda (condition-expr body-expr env)
    (let loop ()
      (if (true-value? (eval-expression condition-expr env))
          (begin
            (eval-expression body-expr env)
            (loop))
          0))))

; funciones auxiliares para aplicar eval-expression a cada elemento de una 
; lista de operandos (expresiones)
(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands)))

(define eval-rand
  (lambda (rand env)
    (cases expression rand
      (var-exp (id)
               (indirect-target
                (let ((ref (apply-env-ref env id)))
                  (cases target (primitive-deref ref)
                    (direct-target (expval) ref)
                    (indirect-target (ref1) ref1)))))
      (else
       (direct-target (eval-expression rand env))))))

(define eval-primapp-exp-rands
  (lambda (rands env)
    (map (lambda (x) (eval-expression x env)) rands)))

(define eval-let-exp-rands
  (lambda (rands env)
    (map (lambda (x) (eval-let-exp-rand x env))
         rands)))

(define eval-let-exp-rand
  (lambda (rand env)
    (direct-target (eval-expression rand env))))

;apply-primitive: <primitiva> <list-of-expression> -> numero
(define apply-primitive
  (lambda (prim args)
    (cases primitive prim
      (add-prim () (+ (car args) (cadr args)))
      (substract-prim () (- (car args) (cadr args)))
      (mult-prim () (* (car args) (cadr args)))
      (incr-prim () (+ (car args) 1))
      (decr-prim () (- (car args) 1))
      (complex-prim () (make-complex (car args) (cadr args)))
      (sum-complex-prim () 
                        (let ((complex1 (car args)) (complex2 (cadr args)))
                          (cases complex-num complex1
                            (make-complex (real1 imag1)
                                     (cases complex-num complex2
                                       (make-complex (real2 imag2)
                                                (make-complex (+ real1 real2) (+ imag1 imag2))))))))
      (sub-complex-prim () 
                        (let ((complex1 (car args)) (complex2 (cadr args)))
                          (cases complex-num complex1
                            (make-complex (real1 imag1)
                                     (cases complex-num complex2
                                       (make-complex (real2 imag2)
                                                (make-complex (- real1 real2) (- imag1 imag2))))))))
      (mult-complex-prim () 
                         (let ((complex1 (car args)) (complex2 (cadr args)))
                           (cases complex-num complex1
                             (make-complex (real1 imag1)
                                      (cases complex-num complex2
                                        (make-complex (real2 imag2)
                                                 (make-complex (- (* real1 real2) (* imag1 imag2)) 
                                                         (+ (* real1 imag2) (* imag1 real2)))))))))
      (div-complex-prim () 
                        (let ((complex1 (car args)) (complex2 (cadr args)))
                          (cases complex-num complex1
                            (make-complex (real1 imag1)
                                     (cases complex-num complex2
                                       (make-complex (real2 imag2)
                                                (let ((denominator (+ (* real2 real2) (* imag2 imag2))))
                                                  (make-complex (/ (+ (* real1 real2) (* imag1 imag2)) denominator)
                                                          (/ (- (* imag1 real2) (* real1 imag2)) denominator)))))))))
      (vacio-prim () '())
      (crear-lista-prim () (cons (car args) (cadr args)))
      (cabeza-prim () (car (car args)))
      (cola-prim () (cdr (car args)))
      (vacio-check-prim () (if (null? (car args)) #t #f))
      (lista-check-prim () (if (list? (car args)) #t #f))
      (append-prim () (append (car args) (cadr args)))
      (ref-lista-prim () (list-ref (car args) (cadr args)))
      (set-lista-prim () (list-set-helper (car args) (cadr args) (caddr args)))
      (longitud-prim () 
                     (let ((str (car args)))
                       (if (and (>= (string-length str) 2)
                                (equal? (string-ref str 0) #\")
                                (equal? (string-ref str (- (string-length str) 1)) #\"))
                           (- (string-length str) 2)  ; No contar las comillas
                           (string-length str))))     ; Contar normalmente si no tiene comillas
      (concatenar-prim () 
                       (let ((str1 (car args))
                             (str2 (cadr args)))
                         ; Remover comillas dobles del inicio y final si existen
                         (define (remove-quotes s)
                           (cond
                             ((and (>= (string-length s) 2)
                                   (equal? (string-ref s 0) #\")
                                   (equal? (string-ref s (- (string-length s) 1)) #\"))
                              (substring s 1 (- (string-length s) 1)))
                             (else s)))
                         (string-append (remove-quotes str1) (remove-quotes str2))))
     
      ; Operaciones básicas
      (equal-prim () (if (equal? (car args) (cadr args)) #t #f))
      (less-prim () (if (< (car args) (cadr args)) #t #f))
      (greater-prim () (if (> (car args) (cadr args)) #t #f))
      (less-equal-prim () (if (<= (car args) (cadr args)) #t #f))
      (greater-equal-prim () (if (>= (car args) (cadr args)) #t #f))
      (and-prim () (if (and (true-value? (car args)) (true-value? (cadr args))) #t #f))
      (or-prim () (if (or (true-value? (car args)) (true-value? (cadr args))) #t #f))
      (not-prim () (if (true-value? (car args)) #f #t))
      (print-prim () (display-value (car args)))

      ; Operaciones de diccionarios
      (crear-diccionario-prim () (create-dictionary args))
      (diccionario-set-prim () 
                     (let ((dict (car args))
                           (key (cadr args))
                           (value (caddr args)))
                       (dictionary-set dict key value)))
      (diccionario-get-prim () 
                     (let ((dict (car args))
                           (key (cadr args)))
                       (dictionary-get dict key)))
      (diccionario-check-prim () (if (dictionary? (car args)) #t #f))
      (claves-prim () (dictionary-keys (car args)))
      (valores-prim () (dictionary-values (car args))))))

;*******************************************************************************************
;Helper functions for lists

; filter-list: filters a list based on a predicate
(define filter-list
  (lambda (predicate-fn element-list)
    (cond
      ((null? element-list) '())
      ((predicate-fn (car element-list)) 
       (cons (car element-list) (filter-list predicate-fn (cdr element-list))))
      (else (filter-list predicate-fn (cdr element-list))))))

; find-first: finds the first element that satisfies the predicate
(define find-first
  (lambda (predicate-fn element-list)
    (cond
      ((null? element-list) #f)
      ((predicate-fn (car element-list)) (car element-list))
      (else (find-first predicate-fn (cdr element-list))))))

;*******************************************************************************************
;Helper functions for lists 

; list-set-helper: replaces the element at position i with value
(define list-set-helper
  (lambda (target-list position new-value)
    (cond
      ((null? target-list) (eopl:error 'list-set-helper "Index ~s out of bounds" position))
      ((= position 0) (cons new-value (cdr target-list)))
      (else (cons (car target-list) (list-set-helper (cdr target-list) (- position 1) new-value))))))

;true-value?: determina si un valor dado corresponde a un valor booleano falso o verdadero
(define true-value?
  (lambda (x)
    (cond
      ((boolean? x) x)
      ((number? x) (not (zero? x)))
      (else #t))))

; Convierte símbolos booleanos a valores booleanos de Scheme
(define convert-symbol-to-bool
  (lambda (symb)
    (cond
      ((eq? symb 'true) #t)
      ((eq? symb 'false) #f)
      (else (eopl:error 'convert-symbol-to-bool "Símbolo booleano no válido: ~s" symb)))))

;*******************************************************************************************
;Complex Numbers
(define-datatype complex-num complex-num?
  (make-complex
   (real-part number?)
   (imag-part number?)))

;*******************************************************************************************
;Procedimientos
(define-datatype procval procval?
  (closure
   (ids (list-of symbol?))
   (body expression?)
   (env environment?)))

;apply-procedure: evalua el cuerpo de un procedimientos en el ambiente extendido correspondiente
(define apply-procedure
  (lambda (proc args)
    (cases procval proc
      (closure (ids body env)
               (eval-expression body (extend-env ids args env))))))

;*******************************************************************************************
;Ambientes

;definición del tipo de dato ambiente
(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
   (syms (list-of symbol?))
   (vec vector?)
   (env environment?))
  (extended-env-const-record
   (syms (list-of symbol?))
   (vec vector?)
   (env environment?))
  (recursively-extended-env-record (proc-names (list-of symbol?))
                                   (idss (list-of (list-of symbol?)))
                                   (bodies (list-of expression?))
                                   (env environment?)))

(define scheme-value? (lambda (v) #t))

;empty-env:      -> enviroment
;función que crea un ambiente vacío
(define empty-env  
  (lambda ()
    (empty-env-record)))

;extend-env: <list-of symbols> <list-of numbers> enviroment -> enviroment
;función que crea un ambiente extendido
(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms (list->vector vals) env)))

; Crea un entorno extendido para constantes (no modificables)
(define extend-env-const
  (lambda (symbols values parent-env)
    (extended-env-const-record symbols (list->vector values) parent-env)))

;extend-env-recursively: <list-of symbols> <list-of <list-of symbols>> <list-of expressions> environment -> environment
;función que crea un ambiente extendido para procedimientos recursivos
(define extend-env-recursively
  (lambda (proc-names idss bodies old-env)
    (let ((len (length proc-names)))
      (let ((vec (make-vector len)))
        (let ((env (extended-env-record proc-names vec old-env)))
          (for-each
            (lambda (pos ids body)
              (vector-set! vec pos (direct-target (closure ids body env))))
            (iota len) idss bodies)
          env)))))

;iota: number -> list
;función que retorna una lista de los números desde 0 hasta end
(define iota
  (lambda (end)
    (let loop ((next 0))
      (if (>= next end) '()
        (cons next (loop (+ 1 next)))))))

;función que busca un símbolo en un ambiente
(define apply-env
  (lambda (env sym)
    (deref (apply-env-ref env sym))))

(define apply-env-ref
  (lambda (env sym)
    (cases environment env
      (empty-env-record ()
                        (eopl:error 'apply-env-ref "No binding for ~s" sym))
      (extended-env-record (syms vals env)
                           (let ((pos (rib-find-position sym syms)))
                             (if (number? pos)
                                 (a-ref pos vals)
                                 (apply-env-ref env sym))))
      (extended-env-const-record (syms vals env)
                                 (let ((pos (rib-find-position sym syms)))
                                   (if (number? pos)
                                       (eopl:error 'set "Cannot modify constant ~s" sym) ; <- ERROR aquí
                                       (apply-env-ref env sym))))
      (recursively-extended-env-record (proc-names idss bodies old-env)
                                       (let ((pos (rib-find-position sym proc-names)))
                                         (if (number? pos)
                                             (let ((vec (make-vector 1)))
                                               (vector-set! vec 0 (direct-target (closure (list-ref idss pos)
                                                                                          (list-ref bodies pos)
                                                                                          env)))
                                               (a-ref 0 vec))
                                             (apply-env-ref old-env sym)))))))

; Función auxiliar para aplicar el ambiente sin referencia (solo para lectura)
(define apply-env-no-ref
  (lambda (env sym)
    (cases environment env
      (empty-env-record ()
                        (eopl:error 'apply-env "No binding for ~s" sym))
      (extended-env-record (syms vals env)
                           (let ((pos (rib-find-position sym syms)))
                             (if (number? pos)
                                 (deref (a-ref pos vals))
                                 (apply-env-no-ref env sym))))
      (extended-env-const-record (syms vals env)
                                 (let ((pos (rib-find-position sym syms)))
                                   (if (number? pos)
                                       (deref (a-ref pos vals)) ; Permite lectura
                                       (apply-env-no-ref env sym))))
      (recursively-extended-env-record (proc-names idss bodies old-env)
                                       (let ((pos (rib-find-position sym proc-names)))
                                         (if (number? pos)
                                             (deref (let ((vec (make-vector 1)))
                                                      (vector-set! vec 0 (direct-target (closure (list-ref idss pos)
                                                                                                (list-ref bodies pos)
                                                                                                env)))
                                                      (a-ref 0 vec)))
                                             (apply-env-no-ref old-env sym)))))))

; Localiza la posición de un símbolo en una lista
(define rib-find-position
  (lambda (sym los)
    (list-find-position sym los)))

(define list-find-position
  (lambda (sym sos)
    (list-index (lambda (sym1) (eqv? sym1 sym)) sos)))

(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else (let ((list-index-r (list-index pred (cdr ls))))
              (if (number? list-index-r)
                  (+ list-index-r 1)
                  #f))))))

;*******************************************************************************************
;Blancos y Referencias

(define expval?
  (lambda (x)
    (or (number? x) (boolean? x) (string? x) (complex-num? x) (list? x) (dictionary? x) (procval? x))))

(define ref-to-direct-target?
  (lambda (x)
    (and (reference? x)
         (cases reference x
           (a-ref (pos vec)
                  (cases target (vector-ref vec pos)
                    (direct-target (v) #t)
                    (indirect-target (v) #f)))))))

(define deref
  (lambda (ref)
    (cases target (primitive-deref ref)
      (direct-target (expval) expval)
      (indirect-target (ref1)
                       (cases target (primitive-deref ref1)
                         (direct-target (expval) expval)
                         (indirect-target (p)
                                          (eopl:error 'deref
                                                      "Illegal reference: ~s" ref1)))))))

(define primitive-deref
  (lambda (ref)
    (cases reference ref
      (a-ref (pos vec)
             (vector-ref vec pos)))))

(define setref!
  (lambda (ref expval)
    (let
        ((ref (cases target (primitive-deref ref)
                (direct-target (expval1) ref)
                (indirect-target (ref1) ref1))))
      (primitive-setref! ref (direct-target expval)))))

(define primitive-setref!
  (lambda (ref val)
    (cases reference ref
      (a-ref (pos vec)
             (vector-set! vec pos val)))))

;****************************************************************************************
;Funciones Auxiliares

; Function to display complex numbers in readable format
(define display-complex
  (lambda (complex-value)
    (cases complex-num complex-value
      (make-complex (real-component imag-component)
               (let ((real-string (if (integer? real-component) 
                                   (number->string (inexact->exact real-component))
                                   (number->string real-component)))
                     (imag-string (if (integer? imag-component) 
                                   (number->string (inexact->exact imag-component))
                                   (number->string imag-component))))
                 (cond
                   ((= imag-component 0) real-string)
                   ((= real-component 0) 
                    (cond
                      ((= imag-component 1) "i")
                      ((= imag-component -1) "-i")
                      (else (string-append imag-string "i"))))
                   ((= imag-component 1) (string-append real-string "+i"))
                   ((= imag-component -1) (string-append real-string "-i"))
                   ((> imag-component 0) (string-append real-string "+" imag-string "i"))
                   (else (string-append real-string imag-string "i"))))))))

; display-value: prints a value and returns the same value
(define display-value
  (lambda (input-value)
    (cond
      ((boolean? input-value)
       (begin
         (display (if input-value "true" "false"))
         (newline)
         input-value))
      ((complex-num? input-value)
       (begin
         (display (display-complex input-value))
         (newline)
         input-value))
      ((list? input-value)
       (begin
         (display input-value)
         (newline)
         input-value))
      ((dictionary? input-value)
       (begin
         (display "#<dictionary>")
         (newline)
         input-value))
      (else
       (begin
         (display input-value)
         (newline)
         input-value)))))

;*******************************************************************************************
; DICTIONARY IMPLEMENTATION - 
; =============================================================================

; Dictionary data type definition
(define-datatype dictionary dictionary?
  (dict
   (bindings list?)))

; Creates a new dictionary instance
; Parameters:
;   - key-value-pairs: list of arguments representing key-value pairs
; Returns: a dictionary object containing the provided key-value mappings
; Throws error if number of arguments is odd (unbalanced key-value pairs)
(define create-dictionary
  (lambda (key-value-pairs)
    (let ((pairs-list key-value-pairs))
      (cond
        ; Empty dictionary case
        ((null? pairs-list) 
         (dict '()))
        ; Validate even number of arguments for proper key-value pairing
        ((odd? (length pairs-list))
         (eopl:error 'create-dictionary 
                     "Invalid number of arguments - expected even number for key-value pairs"))
        ; Recursively build dictionary from key-value pairs
        (else
         (let build-dict ((remaining-pairs pairs-list) 
                          (accumulated-bindings '()))
           (if (null? remaining-pairs)
               (dict (reverse accumulated-bindings)) ; Reverse to maintain insertion order
               (let ((current-key (car remaining-pairs))
                     (current-value (cadr remaining-pairs)))
                 (build-dict (cddr remaining-pairs)
                            (cons (cons current-key current-value) 
                                  accumulated-bindings))))))))))

; Inserts or updates a key-value mapping in the dictionary
; Parameters:
;   - dict-obj: the dictionary to modify
;   - target-key: the key to insert/update
;   - new-value: the value to associate with the key
; Returns: new dictionary with the updated mapping
(define dictionary-set
  (lambda (dict-obj target-key new-value)
    (cases dictionary dict-obj
      (dict (bindings)
            ; Remove existing binding for the key (if any) and add new one
            (let ((filtered-bindings 
                   (filter-list (lambda (binding) 
                                 (not (equal? (car binding) target-key)))
                               bindings)))
              (dict (cons (cons target-key new-value) filtered-bindings)))))))

; Retrieves the value associated with a key from the dictionary
; Parameters:
;   - dict-obj: the dictionary to search
;   - target-key: the key to look up
; Returns: the value associated with the key
; Throws error if key is not found
(define dictionary-get
  (lambda (dict-obj target-key)
    (cases dictionary dict-obj
      (dict (bindings)
            (let search-binding ((binding-list bindings))
              (cond
                ((null? binding-list)
                 (eopl:error 'dictionary-get 
                             "Key ~s not found in dictionary" target-key))
                ((equal? (caar binding-list) target-key)
                 (cdar binding-list))
                (else
                 (search-binding (cdr binding-list)))))))))

; Extracts all keys from the dictionary
; Parameters:
;   - dict-obj: the dictionary to process
; Returns: list of all keys in the dictionary
(define dictionary-keys
  (lambda (dict-obj)
    (cases dictionary dict-obj
      (dict (bindings)
            (map (lambda (pair) (car pair)) bindings)))))

; Extracts all values from the dictionary
; Parameters:
;   - dict-obj: the dictionary to process
; Returns: list of all values in the dictionary
(define dictionary-values
  (lambda (dict-obj)
    (cases dictionary dict-obj
      (dict (bindings)
            (map (lambda (pair) (cdr pair)) bindings)))))

; Checks if a dictionary contains a specific key
; Parameters:
;   - dict-obj: the dictionary to check
;   - target-key: the key to search for
; Returns: #t if key exists, #f otherwise
(define dictionary-contains?
  (lambda (dict-obj target-key)
    (cases dictionary dict-obj
      (dict (bindings)
            (if (find-first (lambda (pair) 
                             (equal? (car pair) target-key)) 
                           bindings)
                #t
                #f)))))

; Returns the number of key-value pairs in the dictionary
; Parameters:
;   - dict-obj: the dictionary to measure
; Returns: integer count of entries
(define dictionary-size
  (lambda (dict-obj)
    (cases dictionary dict-obj
      (dict (bindings)
            (length bindings)))))

; Creates a copy of the dictionary
; Parameters:
;   - dict-obj: the dictionary to copy
; Returns: new dictionary with same bindings
(define dictionary-copy
  (lambda (dict-obj)
    (cases dictionary dict-obj
      (dict (bindings)
            (dict (map (lambda (pair) 
                        (cons (car pair) (cdr pair))) 
                      bindings))))))

;******************************************************************************************
(interpretador)