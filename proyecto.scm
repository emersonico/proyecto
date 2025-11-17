#lang eopl

;******************************************************************************************
;;;;; Interpretador para lenguaje con condicionales, ligadura local, procedimientos y 
;;;;; procedimientos recursivos

;; La definición BNF para las expresiones del lenguaje:
;;
;;  <program>       ::= <expression>   
;;                      <a-program (exp)>
;;  <expression>    ::= <number>
;;                      <lit-exp (datum)>
;;                  ::= <identifier>
;;                      <variable-exp (id)>
;;                  ::= <primitive> ({<expression>}*(,))
;;                      <primapp-exp (prim rands)>
;;                  ::= if <expresion> then <expresion> else <expression>
;;                      <if-exp (exp1 exp2 exp23)>
;;                  ::= var {identifier = <expression>}* in <expression>
;;                      <var-exp (ids rands body)>
;;                  ::= const {identifier = <expression>}* in <expression>
;;                      <const-exp (ids rands body)>
;;                  ::= let {identifier = <expression>}* in <expression>
;;                      <let-exp (ids rands body)>
;;                  ::= proc({<identificador>}*(,)) <expression>
;;                      <proc-exp (ids body)>
;;                  ::= (<expression> {<expression>}*)
;;                      <app-exp proc rands>
;;                  ::= letrec  {identifier ({identifier}*(,)) = <expression>}* in <expression>
;;                     <letrec-exp proc-names idss bodies bodyletrec>
;;  <primitive>     ::= + | - | * | add1 | sub1 

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
    (expression (identifier) variable-exp)
    (expression
     (primitive "(" (separated-list expression ",")")")
     primapp-exp)
    (expression ("if" expression "then" expression "else" expression)
                if-exp)
    (expression ("var" identifier "=" expression (arbno ";" identifier "=" expression) "in" expression)
                var-exp)
    (expression ("const" identifier "=" expression (arbno ";" identifier "=" expression) "in" expression)
                const-exp)
    (expression ("let" identifier "=" expression (arbno ";" identifier "=" expression) "in" expression)
                let-exp)
    (expression ("proc" "(" (arbno identifier) ")" expression)
                proc-exp)
    (expression ( "(" expression (arbno expression) ")")
                app-exp)
    
    ; características adicionales
    (expression ("letrec" (arbno identifier "(" (separated-list identifier ",") ")" "=" expression)  "in" expression) 
                letrec-exp)
    (expression ("switch" "(" expression ")" "{" 
                (arbno "case" expression ":" "{" expression "}")
                "default" ":" "{" expression "}"
                "}") switch-exp)
    (expression ("begin" (arbno expression) "end") begin-exp)
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
    (empty-env)))

;eval-expression: <expression> <enviroment> -> numero
; evalua la expresión en el ambiente de entrada
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
      (variable-exp (id) (apply-env env id))
      (primapp-exp (prim rands)
                   (let ((args (eval-rands rands env)))
                     (apply-primitive prim args)))
      (if-exp (test-exp true-exp false-exp)
              (if (true-value? (eval-expression test-exp env))
                  (eval-expression true-exp env)
                  (eval-expression false-exp env)))
      (var-exp (id1 rand1 ids rands body)
               (let ((val1 (eval-expression rand1 env)))
                 (let ((all-ids (cons id1 ids))
                       (all-vals (cons val1 (eval-rands rands env))))
                   (eval-expression body (extend-env all-ids all-vals env)))))
      (const-exp (id1 rand1 ids rands body)
                 (let ((val1 (eval-expression rand1 env)))
                   (let ((all-ids (cons id1 ids))
                         (all-vals (cons val1 (eval-rands rands env))))
                     (eval-expression body (extend-env-const all-ids all-vals env)))))
      (let-exp (id1 rand1 ids rands body)
               (let ((val1 (eval-expression rand1 env)))
                 (let ((all-ids (cons id1 ids))
                       (all-vals (cons val1 (eval-rands rands env))))
                   (eval-expression body (extend-env all-ids all-vals env)))))
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
      (switch-exp (switch-expr case-vals case-bodies default-body)
                  (process-switch-statement switch-expr case-vals case-bodies default-body env))
      (begin-exp (exps)
                 (execute-expression-sequence exps env)))))

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

; Ejecuta una secuencia de expresiones en orden
(define execute-expression-sequence
  (lambda (expr-list env)
    (if (null? expr-list)
        0
        (let ((first-result (eval-expression (car expr-list) env)))
          (if (null? (cdr expr-list))
              first-result
              (execute-expression-sequence (cdr expr-list) env))))))

; funciones auxiliares para aplicar eval-expression a cada elemento de una 
; lista de operandos (expresiones)
(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands)))

(define eval-rand
  (lambda (rand env)
    (eval-expression rand env)))

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
      (equal-prim () (if (equal? (car args) (cadr args)) #t #f))
      (less-prim () (if (< (car args) (cadr args)) #t #f))
      (greater-prim () (if (> (car args) (cadr args)) #t #f))
      (less-equal-prim () (if (<= (car args) (cadr args)) #t #f))
      (greater-equal-prim () (if (>= (car args) (cadr args)) #t #f))
      (and-prim () (if (and (true-value? (car args)) (true-value? (cadr args))) #t #f))
      (or-prim () (if (or (true-value? (car args)) (true-value? (cadr args))) #t #f))
      (not-prim () (if (true-value? (car args)) #f #t))
      (print-prim () (display-value (car args))))))

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
;Helper functions for lists - continuation

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
  (extended-env-record (syms (list-of symbol?))
                       (vals (list-of scheme-value?))
                       (env environment?))
  (extended-env-const-record (syms (list-of symbol?))
                             (vals (list-of scheme-value?))
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
    (empty-env-record)))       ;llamado al constructor de ambiente vacío 

;extend-env: <list-of symbols> <list-of numbers> enviroment -> enviroment
;función que crea un ambiente extendido
(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms vals env)))

; Crea un entorno extendido para constantes (no modificables)
(define extend-env-const
  (lambda (symbols values parent-env)
    (extended-env-const-record symbols values parent-env)))

;extend-env-recursively: <list-of symbols> <list-of <list-of symbols>> <list-of expressions> environment -> environment
;función que crea un ambiente extendido para procedimientos recursivos
(define extend-env-recursively
  (lambda (proc-names idss bodies old-env)
    (recursively-extended-env-record
     proc-names idss bodies old-env)))

;función que busca un símbolo en un ambiente
(define apply-env
  (lambda (env sym)
    (cases environment env
      (empty-env-record ()
                        (eopl:error 'empty-env "No binding for ~s" sym))
      (extended-env-record (syms vals old-env)
                           (let ((pos (list-find-position sym syms)))
                             (if (number? pos)
                                 (list-ref vals pos)
                                 (apply-env old-env sym))))
      (extended-env-const-record (syms vals old-env)
                                 (let ((pos (list-find-position sym syms)))
                                   (if (number? pos)
                                       (list-ref vals pos)
                                       (apply-env old-env sym))))
      (recursively-extended-env-record (proc-names idss bodies old-env)
                                       (let ((pos (list-find-position sym proc-names)))
                                         (if (number? pos)
                                             (closure (list-ref idss pos)
                                                      (list-ref bodies pos)
                                                      env)
                                             (apply-env old-env sym)))))))

;****************************************************************************************
;Funciones Auxiliares

; Localiza la posición de un símbolo en una lista
(define list-find-position
  (lambda (target-symbol symbol-list)
    (find-index (lambda (sym) (eqv? sym target-symbol)) symbol-list)))

; Encuentra el índice del primer elemento que satisface el predicado
(define find-index
  (lambda (predicate-fn lst)
    (cond
      ((null? lst) #f)
      ((predicate-fn (car lst)) 0)
      (else 
       (let ((remaining-index (find-index predicate-fn (cdr lst))))
         (if (number? remaining-index)
             (+ remaining-index 1)
             #f))))))

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
      (else
       (begin
         (display input-value)
         (newline)
         input-value)))))

;******************************************************************************************
(interpretador)