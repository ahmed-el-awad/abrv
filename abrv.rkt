#lang racket
(require parser-tools/lex)
(require parser-tools/yacc)
(require parser-tools/cfg-parser)

(require (prefix-in re- parser-tools/lex-sre))

(define-tokens a (NUM VAR))
(define-empty-tokens
 b
 (+ - * / % BIND CREATE FALSE TRUE IN EOF BIOR GREATER LESS NOTEQUALS ISEQUALS ASGN LPRN RPRN))

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
         ["isgr" (token-GREATER)]
         ["isls" (token-LESS)]
         ["isne" (token-NOTEQUALS)]
         ["iseq" (token-ISEQUALS)]
         ["asgn" (token-ASGN)]
         ["lprn" (token-LPRN)]
         ["rprn" (token-RPRN)]
         [(re-+ number10) (token-NUM (string->number lexeme))]
         [identifier (token-VAR lexeme)]
         [whitespace (abrv_lex input-port)]
         [(eof) (token-EOF)]))

(define-struct let-exp (var num exp))
(define-struct arith-exp (op e1 e2))
(define-struct num-exp (n))
(define-struct var-exp (i))
(define abrv_parse
  (parser (start exp)
          (end EOF)
          (error void)
          (tokens a b)
          (precs (left + -) (left * / %) (left GREATER LESS NOTEQUALS ISEQUALS) (right ASGN))
          (grammar
           (exp ;
            ((exp % exp) (make-arith-exp remainder $1 $3))
            ((exp * exp) (make-arith-exp * $1 $3))
            ((exp / exp) (make-arith-exp / $1 $3))
            ((exp + exp) (make-arith-exp + $1 $3))
            ((exp - exp) (make-arith-exp - $1 $3))
            ((exp BIOR exp) (make-arith-exp test_BIOR $1 $3))
            ((exp BIND exp) (make-arith-exp test_BIND $1 $3))
            ((exp GREATER exp) (make-arith-exp > $1 $3))
            ((exp LESS exp) (make-arith-exp < $1 $3))
            ((exp NOTEQUALS exp) (make-arith-exp (lambda (x y) (not (= x y))) $1 $3))
            ((exp ISEQUALS exp) (make-arith-exp = $1 $3))
            ((VAR ASGN exp) (make-let-exp $1 (var-exp $1) $3))
            ((CREATE VAR NUM IN exp) (make-let-exp $2 (num-exp $3) $5))
            ((NUM) (num-exp $1))
            ((VAR) (var-exp $1))
            ((LPRN exp RPRN)
             $2))))) ; This line should be within the same set of parentheses as the other rules

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

;;;;;;;;;;;;;;;;;
;     Tests
;;;;;;;;;;;;;;;;;

;; paren
; (let ([input (open-input-string "x asgn lprn 5 isne 4 rprn ")])
;   (eval (abrv_parse (lex-this abrv_lex input))))

; (let ([input (open-input-string "lprn 3 adtn 3 rprn mltp 3")])
;   (eval (abrv_parse (lex-this abrv_lex input))))

; (let ([input (open-input-string "lprn 3 mltp 3 rprn adtn 3")])
; (eval (abrv_parse (lex-this abrv_lex input))))

;; logic operators
; (let ([input (open-input-string "3 iseq 3")]) (eval (abrv_parse (lex-this abrv_lex input))))

; (let ([input (open-input-string "3 iseq 4")]) (eval (abrv_parse (lex-this abrv_lex input))))

; (let ([input (open-input-string "3 isne 4")]) (eval (abrv_parse (lex-this abrv_lex input))))

; (let ([input (open-input-string "3 isne 3")]) (eval (abrv_parse (lex-this abrv_lex input))))

; (let ([input (open-input-string "3 isgr 3")]) (eval (abrv_parse (lex-this abrv_lex input))))

; (let ([input (open-input-string "4 isgr 3")]) (eval (abrv_parse (lex-this abrv_lex input))))

; (let ([input (open-input-string "3 isls 3")]) (eval (abrv_parse (lex-this abrv_lex input))))

; (let ([input (open-input-string "2 isls 3")]) (eval (abrv_parse (lex-this abrv_lex input))))
