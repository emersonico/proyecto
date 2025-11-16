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
  (boolean
   ("true") symbol)
  (boolean
   ("false") symbol)))

;Especificación Sintáctica (gramática)

(define grammar-simple-interpreter
  '((program (expression) a-program)
    (expression (number) lit-exp)
    (expression (boolean) bool-exp)
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
    (primitive ("=") equal-prim)
    (primitive ("<") less-prim)
    (primitive (">") greater-prim)
    (primitive ("<=") less-equal-prim)
    (primitive (">=") greater-equal-prim)
    (primitive ("and") and-prim)
    (primitive ("or") or-prim)
    (primitive ("not") not-prim)))

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
      (eval-program pgm))
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
      (equal-prim () (if (equal? (car args) (cadr args)) #t #f))
      (less-prim () (if (< (car args) (cadr args)) #t #f))
      (greater-prim () (if (> (car args) (cadr args)) #t #f))
      (less-equal-prim () (if (<= (car args) (cadr args)) #t #f))
      (greater-equal-prim () (if (>= (car args) (cadr args)) #t #f))
      (and-prim () (if (and (true-value? (car args)) (true-value? (cadr args))) #t #f))
      (or-prim () (if (or (true-value? (car args)) (true-value? (cadr args))) #t #f))
      (not-prim () (if (true-value? (car args)) #f #t)))))

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

;******************************************************************************************
(interpretador)