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
  
            