#lang racket
(require parser-tools/lex)
(require parser-tools/yacc)
(require parser-tools/cfg-parser)

(require (prefix-in re- parser-tools/lex-sre))

(define-tokens a (NUM VAR))
(define-empty-tokens b (+ - * / % BIND CREATE FALSE TRUE IN EOF BIOR))

(define-lex-trans
 number
 (syntax-rules ()
   [(_ digit) (re-: (re-? (re-or)) (uinteger digit) (re-? (re-: "." (re-? (uinteger digit)))))]))

(define-lex-trans uinteger
                  (syntax-rules ()
                    [(_ digit) (re-+ digit)]))

(define-lex-abbrevs [digits (char-range "0" "9")]
                    [number10 (number digits)]
                    [identifier-characters (re-or (char-range "A" "z"))]
                    [identifier (re-+ identifier-characters)])

(define abrv_lex
  (lexer ["sbtn" (token--)]
         ["adtn" (token-+)]
         ["crte" (token-CREATE)]
         ["in" (token-IN)]
         ["mltp" (token-*)]
         ["dvsn" (token-/)]
         ["rmdr" (token-%)]
         ["bior" (token-BIOR)]
         ["bind" (token-BIND)]
         ["#f" (token-FALSE)]
         ["#t" (token-TRUE)]
         [(re-+ number10) (token-NUM (string->number lexeme))]
         [identifier (token-VAR lexeme)]
         [whitespace (abrv_lex input-port)] ; recursively calls the lexer
         [(eof) (token-EOF)]))

(define abrv_parse
  (parser (start exp)
          (end EOF)
          (error void)
          (tokens a b)
          (precs (left + -) (left * / %) (nonassoc EOF))
          (grammar (exp ;
                    [(exp % exp) (make-arith-exp remainder $1 $3)]
                    [(exp * exp) (make-arith-exp * $1 $3)]
                    [(exp / exp) (make-arith-exp / $1 $3)]
                    [(exp + exp) (make-arith-exp + $1 $3)]
                    [(exp - exp) (make-arith-exp - $1 $3)]
                    [(exp BIOR exp) (make-arith-exp test_BIOR $1 $3)]
                    [(exp BIND exp) (make-arith-exp test_BIND $1 $3)]
                    [(CREATE VAR NUM IN exp) (make-let-exp $2 (num-exp $3) $5)]
                    [(NUM) (num-exp $1)]
                    [(VAR) (var-exp $1)]))))

(define-struct let-exp (var num exp))
(define-struct arith-exp (op e1 e2))
(define-struct num-exp (n))
(define-struct var-exp (i))

(define (eval parsed-exp)
  (match parsed-exp
    [(let-exp var num exp) (eval (subst var num exp))]
    [(arith-exp op e1 e2) (op (eval e1) (eval e2))]
    [(num-exp n) n]
    [(var-exp i) (error 'eval "undefined identifier ~a" i)]))

(define (subst var num exp)
  (match exp
    [(let-exp var2 num2 exp2) (if (eq? var var2) exp (let-exp var2 num2 (subst var num exp2)))]
    [(arith-exp op e1 e2) (arith-exp op (subst var num e1) (subst var num e2))]
    [(var-exp id) (if (equal? id var) num exp)]
    [(num-exp n) exp]))

(define (lex-this lexer input)
  (lambda () (lexer input)))

(define (test_BIOR param1 param2)
  (or param1 param2))

(define (test_BIND param1 param2)
  (and param1 param2))

(let ([input (open-input-string "3 bior 4")]) (eval (abrv_parse (lex-this abrv_lex input))))
(let ([input (open-input-string "3 bind 4")]) (eval (abrv_parse (lex-this abrv_lex input))))

(let ([input (open-input-string "6 mltp 4 adtn 5")]) (eval (abrv_parse (lex-this abrv_lex input))))
