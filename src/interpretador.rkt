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
  '((programa ("GLOBALS" "{" (arbno declaracion-global) "}" "PROGRAM" "{" "proc" "main" "(" ")" "{" "return" expresion ";" "}" "}") un-programa)
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
    (expresion ("True") true-lit)
    (expresion ("False") false-lit)
    (expresion (identificador) var-exp)
    (expresion ("set" identificador "->" expresion) asignacion-exp)))

;;Generar Datatypes y parser
(sllgen:make-define-datatypes scanner-spec grammar)
(define scan&parse (sllgen:make-string-parser scanner-spec grammar))
(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes scanner-spec grammar)))
(define just-scan
  (sllgen:make-string-scanner scanner-spec grammar))
(define interpretador
  (sllgen:make-rep-loop "-->"
     (lambda (pgm) (eval-program pgm))
     (sllgen:make-stream-parser scanner-spec grammar)))

;;Sistema de tipos y validacion
;;Usa el datatype tipo que se genera con sllgen
(define type-matches?
  (lambda (t val)
    (cases tipo t
      (tipo-void () #t)
      (tipo-int () (integer? val))
      (tipo-float () (number? val))
      (tipo-string () (string? val))
      (tipo-bool () (boolean? val))
      (tipo-sat () #t)
      (tipo-proc () #t)
      (tipo-list (t1) (list? val))
      (tipo-vector (t1) (vector? val))
      (tipo-dict (t1 t2) #t))))
;;tipo -> texto: tipo para los mensajes de error
(define tipo->texto
  (lambda (t)
    (cases tipo t
      (tipo-int () 'int)
      (tipo-float () 'float)
      (tipo-string () 'string)
      (tipo-bool () 'bool)
      (tipo-void () 'void)
      (tipo-sat () 'sat)
      (tipo-proc () 'proc)
      (tipo-list (t1) (list 'list (tipo->texto t1)))
      (tipo-vector (t1) (list 'vector (tipo->texto t1)))
      (tipo-dict (t1 t2) (list 'dict (tipo->texto t1) (tipo->texto t2))))))

  
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
      (true-lit () #t)
      (false-lit () #f)
      (var-exp (id) (apply-env env id))
      (asignacion-exp (id rhs)
                      (let ((val (eval-expression rhs env)))
                        (assign-var! env id val)))
      (else (eopl:error 'eval-expresion "Expresion no sortada ")))))
                     

;;FUnciones auxiliares,se tomaron de interpretador_asign
(define iota
  (lambda (end)
    (let loop ((next 0))
      (if (>= next end) '()
          (cons next (loop (+ 1 next)))))))

(define rib-find-position
  (lambda (sym los)
    (list-find-position sym los)))

(define list-find-position
  (lambda (sym los)
    (list-index (lambda (sym1) (eqv? sym1 sym)) los)))
(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else (let ((list-index-r (list-index pred (cdr ls))))
              (if (number? list-index-r)
                  (+ list-index-r 1)
                  #f))))))

                                