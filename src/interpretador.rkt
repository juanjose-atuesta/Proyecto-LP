#lang eopl

;;Especificacion lexica
(define scanner-spec
  '((white-sp (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identificador (letter (arbno (or letter digit "_" "?"))) symbol)
    (numero (digit (arbno digit)) number)
    (numero ("-" digit (arbno digit)) number)
    (numero (digit (arbno digit) "." digit (arbno digit)) number)
    (numero ("-" digit (arbno digit) "." digit (arbno digit)) number)
    (texto ("\"" (arbno (not #\")) "\"") string)))

;;Especificacion gramatical

(define grammar
  '((programa ("GLOBALS" "{" (arbno declaracion-global) "}" "PROGRAM" "{" "proc" "main" "(" ")" "{" expresion "}" "}") un-programa)
    ;;Tipos de datos
    (tipo ("int") tipo-int)
    (tipo ("float") tipo-float)
    (tipo ("string") tipo-string)
    (tipo ("bool") tipo-bool)
    (tipo ("void") tipo-void)
    (tipo ("sat") tipo-sat)
    (tipo ("list" "<" tipo ">") tipo-list)
    (tipo ("vector" "<" tipo ">") tipo-vector)
    (tipo ("dict" "<" tipo "," tipo ">") tipo-dict)
    (tipo ("proc") tipo-proc)
    ;;Declaraciones globales
    (declaracion-global (tipo identificador "=" expresion ";") decl-var-global)
    (declaracion-global ("const" tipo identificador "=" expresion ";") decl-const-global)
    ;;Expresiones basicas y asignacion mutable
    (expresion (numero) numero-lit)
    (expresion (texto) texto-lit)
    (expresion (identificador) var-exp)
    (expresion ("set" identificador "->" expresion) asignacion-exp)))

;;Generar Datatypes y parser
(sllgen:make-define-datatypes scanner-spec grammar)
(define scan&parse (sllgen:make-string-parser scanner-spec grammar))

;;Sistema de tipos y validacion

(define-datatype tipo-exp tipo-exp?
  (int-type)
  (float-type)
  (string-type)
  (bool-type)
  (void-type)
  (sat-type)
  (list-type (elem-type tipo-exp?))
  (vector-type (elem-type tipo-exp?))
  (dict-type (key-type tipo-exp?) (val-type tipo-exp?))
  (proc-type))
;;Predicado para validad coincidencia entre tipo y valor expresado
(define type-matches?
  (lambda (tipo val)
    (cases tipo-exp tipo
      (void-type () #t)
      (int-type () (integer? val))
      (float-type () (number? val))
      (string-type () (string? val))
      (bool-type () (boolean? val))
      (sat-type () (and (pair? val) (eqv? (car val) 'sat-instance)))
      (list-type (t) (list? val))
      (vector-type (t) (vector? val))
      (dict-type (t1 t2) (and (pair? val) (eqv? (car val) 'dict)))
      (proc-type () (and (pair? val) (eqv? (car val) 'closure))))))
  
;;Memoria y ambientes
(define-datatype referencia referencia?
  (a-ref (pos integer?) (vec vector?)))

(define deref
  (lambda (ref)
    (cases referencia ref
    (a-ref (pos vec) (vector-ref vec pos)))))

(define primitive-setref!
  (lambda (ref val)
    (cases referencia ref
      (a-ref (pos vec) (vector-set! vec pos val)))))

;;Representacion del ambiente
(define-datatype ambiente ambiente?
  (empty-env-record)
  (extended-env-record
   (syms (list-of symbol?))
   (refs (list-of referencia?))
   (tipos (list-of scheme-value?))
   (consts (list-of boolean?))
   (parent ambiente?)))
(define scheme-value? (lambda (v) #t))
(define empty-env (lambda () (empty-env-record)))

(define extend-env-vars
  (lambda (syms vals tipos consts-flags parent-env)
    (let ((refs (map (lambda (v) (a-ref 0 (vector v))) vals)))
      (extended-env-record syms refs tipos consts-flags parent-env))))

(define apply-env-binding
  (lambda (env search-sym)
    (cases ambiente env
      (empty-env-record () (eopl:error 'apply-env "Variable no declarada: ~s" search-sym))
      (extended-env-record (syms refs tipos consts parent)
                           (let loop ((s syms) (r refs) (t tipos) (c consts))
                                      (cond
                                        ((null? s) (apply-env-binding parent search-sym))
                                        ((eqv? (car s) search-sym) (list (car r) (car t) (car c)))
                                        (else (loop (cdr s) (cdr r) (cdr t) (cdr c)))))))))

(define apply-env
  (lambda (env search-sym)
    (let ((binding (apply-env-binding env search-sym)))
      (deref (car binding)))))
;;Asignacion mutable con operador ->
(define assign-var!
  (lambda (env search-sym new-val)
    (let* ((binding (apply-env-binding env search-sym))
           (ref (car binding))
           (tipo (cadr binding))
           (is-const (caddr binding)))
      (cond
        (is-const (eopl:error 'assign-var! "Error: No se puede modificar la constante ~s" search-sym))
        ((not (type-matches? tipo new-val))
         (eopl:error 'assign-var! "Error de Tipo: El valor no coincide con el tipo de ~s" search-sym))
        (else
         (primitive-setref! ref new-val))))))

;;Evaluador principal
(define eval-program
  (lambda (pgm)
    (cases programa pgm
      (un-programa (decls-globales cuerpo-main)
                   (let ((env-global (eval-globals decls-globales (empty-env))))
                   (eval-expression cuerpo-main env-global))))))
(define eval-globals
  (lambda (decls env)
    (if (null? decls)
        env
        (eval-globals (cdr decls) env))))

(define eval-expression
  (lambda (exp env)
    (cases expresion exp
      (numero-lit (num) num)
      (texto-lit (txt) txt)
      (var-exp (id) (apply-env env id))
      (asignacion-exp (id rhs)
                      (let ((val (eval-expression rhs env)))
                        (assign-var! env id val)))
      (else (eopl:error 'eval-expresion "Expresion no sortada ")))))
                     
                    
                
                                