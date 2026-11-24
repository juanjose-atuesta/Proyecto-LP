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

;;Referencias basado en el interpretador_asign, ademas de la posicion y el vector, guarda el tipo si es const
(define-datatype reference reference?
  (a-ref (position integer?)
         (vec vector?)
         (tipo tipo?)
         (const? boolean?)))

(define deref
  (lambda (ref)
    (primitive-deref ref)))

(define primitive-deref
  (lambda (ref)
    (cases reference ref
      (a-ref (pos vec tipo const?)
             (vector-ref vec pos)))))

;;setref!: Modifica una variable, operador ->. Valida const y tipo.
(define setref!
  (lambda (ref val)
    (cases reference ref
      (a-ref (pos vec tipo const?)
             (cond
               (const? (eopl:error 'setref! "No se puede modificar una constante (const)"))
               ((type-matches? tipo val) (primitive-setref! ref val))
               (else (eopl:error 'setref! "Error de tipo: se esperaba ~s y llego ~s"
                                 (tipo->texto tipo) val)))))))

(define primitive-setref!
  (lambda (ref val)
    (cases reference ref
      (a-ref (pos vec tipo const?)
             (vector-set! vec pos val)))))

;;Ambientes basado en interpretador_asign
(define-datatype ambiente ambiente?
  (empty-env-record)
  (extended-env-record
   (syms (list-of symbol?))
   (vec vector?)
   (tipos (list-of tipo?))
   (consts (list-of boolean?))
   (env ambiente?)))

(define empty-env
  (lambda ()
    (empty-env-record)))
;;extend-env: variables void y mutables
(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms
                         (list->vector vals)
                         (map (lambda (sym) (tipo-void)) syms)
                         (map (lambda (sym) #f) syms)
                         env)))
;; extend-env-globals: Hace la validacion de tipo al crear la variable
(define extend-env-globals
  (lambda (decls old-env)
    (let ((len (length decls)))
      (let ((vec (make-vector len)))
        (let ((env (extended-env-record (map decl->id decls)
                                        vec
                                        (map decl->tipo decls)
                                        (map decl->const? decls)
                                        old-env)))
          (for-each
           (lambda (pos decl)
             (let ((val (eval-expression (decl->exp decl) env))
                   (tipo (decl->tipo decl)))
               (if (type-matches? tipo val)
                   (vector-set! vec pos val)
                   (eopl:error 'extend-env-globals "Error de tipo al crear ~s: Se esperaba ~s y llego ~s" (decl->id decl) (tipo->texto tipo) val))))
           (iota len) decls)
          env)))))

(define apply-env
  (lambda (env sym)
    (deref (apply-env-ref env sym))))

(define apply-env-ref
  (lambda (env sym)
    (cases ambiente env
      (empty-env-record ()
                        (eopl:error 'apply-env-ref "No existe la variable ~s" sym))
      (extended-env-record (syms vec tipos consts old-env)
                           (let ((pos (rib-find-position sym syms)))
                             (if (number? pos)
                                 (a-ref pos vec (list-ref tipos pos) (list-ref consts pos))
                                 (apply-env-ref old-env sym)))))))
;;Evaluador
(define eval-program
  (lambda (pgm)
    (cases programa pgm
      (un-programa (decls-globales cuerpo-main)
                   (eval-expression cuerpo-main
                                    (extend-env-globals decls-globales (empty-env)))))))

(define eval-expression
  (lambda (exp env)
    (cases expresion exp
      (numero-lit (num) num)
      (texto-lit (txt) (substring txt 1 (- (string-length txt) 1)))
      (true-lit () #t)
      (false-lit () #f)
      (var-exp (id) (apply-env env id))
      (asignacion-exp (id rhs-exp)
                      (begin
                        (setref!
                         (apply-env-ref env id)
                         (eval-expression rhs-exp env))
                        1)))))

;;Extractores
(define decl->id
  (lambda (decl)
    (cases declaracion-global decl
      (decl-var-global (texp id rhs) id)
      (decl-const-global (texp id rhs) id))))

(define decl->tipo
  (lambda (decl)
    (cases declaracion-global decl
      (decl-var-global (texp id rhs) texp)
      (decl-const-global (texp id rhs) texp))))

(define decl->exp
  (lambda (decl)
    (cases declaracion-global decl
      (decl-var-global (texp id rhs) rhs)
      (decl-const-global (texp id rhs) rhs))))

(define decl->const?
  (lambda (decl)
    (cases declaracion-global decl
      (decl-var-global (texp id rhs) #f)
      (decl-const-global (texp id rhs) #t))))

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

                                