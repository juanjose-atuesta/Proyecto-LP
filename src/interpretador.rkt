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
    (texto ("\"" (arbno (not "\"")) "\"") string)))

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
    (declaracion-global ("proc" identificador "=" expresion ";") decl-proc-global)
    ))
            