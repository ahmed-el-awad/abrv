#lang racket
(require parser-tools/lex)
(require parser-tools/yacc)
(require parser-tools/cfg-parser)

(require (prefix-in re- parser-tools/lex-sre))

(define-tokens a (NUM VAR))
(define-empty-tokens
 b
 (+ - * / % BIND FALSE TRUE EOF BIOR GREATER LESS NOTEQUALS ISEQUALS ASGN LPRN RPRN IFOP THOP ELOP))

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
  (lexer ["sbtn"
          (begin
            (printf "Next token is: SUB_OP , next lexeme is ")
            (display lexeme)
            (newline)
            (token--))]
         ["adtn"
          (begin
            (printf "Next token is: ADD_OP , next lexeme is ")
            (display lexeme)
            (newline)
            (token-+))]
         ["mltp"
          (begin
            (printf "Next token is: MULT_OP , next lexeme is ")
            (display lexeme)
            (newline)
            (token-*))]
         ["dvsn"
          (begin
            (printf "Next token is: DIV_OP , next lexeme is ")
            (display lexeme)
            (newline)
            (token-/))]
         ["rmdr"
          (begin
            (printf "Next token is: MODULO , next lexeme is ")
            (display lexeme)
            (newline)
            (token-%))]
         ["bior"
          (begin
            (printf "Next token is: BIOR , next lexeme is ")
            (display lexeme)
            (newline)
            (token-BIOR))]
         ["bind"
          (begin
            (printf "Next token is: BIND , next lexeme is ")
            (display lexeme)
            (newline)
            (token-BIND))]
         ["flse"
          (begin
            (printf "Next token is: FALSE , next lexeme is ")
            (display lexeme)
            (newline)
            (token-FALSE))]
         ["true"
          (begin
            (printf "Next token is: TRUE , next lexeme is ")
            (display lexeme)
            (newline)
            (token-TRUE))]
         ["isgr"
          (begin
            (printf "Next token is: GREATER , next lexeme is ")
            (display lexeme)
            (newline)
            (token-GREATER))]
         ["isls"
          (begin
            (printf "Next token is: LESS , next lexeme is ")
            (display lexeme)
            (newline)
            (token-LESS))]
         ["isne"
          (begin
            (printf "Next token is: NOTEQUALS , next lexeme is ")
            (display lexeme)
            (newline)
            (token-NOTEQUALS))]
         ["iseq"
          (begin
            (printf "Next token is: ISEQUALS , next lexeme is ")
            (display lexeme)
            (newline)
            (token-ISEQUALS))]
         ["asgn"
          (begin
            (printf "Next token is: ASGN , next lexeme is ")
            (display lexeme)
            (newline)
            (token-ASGN))]
         ["lprn"
          (begin
            (printf "Next token is: LPRN , next lexeme is ")
            (display lexeme)
            (newline)
            (token-LPRN))]
         ["rprn"
          (begin
            (printf "Next token is: RPRN , next lexeme is ")
            (display lexeme)
            (newline)
            (token-RPRN))]
         ["ifop"
          (begin
            (printf "Next token is: IFOP , next lexeme is ")
            (display lexeme)
            (newline)
            (token-IFOP))]
         ["thop"
          (begin
            (printf "Next token is: THOP , next lexeme is ")
            (display lexeme)
            (newline)
            (token-THOP))]
         ["elop"
          (begin
            (printf "Next token is: ELOP , next lexeme is ")
            (display lexeme)
            (newline)
            (token-ELOP))]
         [(re-+ number10)
          (begin
            (printf "Next token is: NUM , next lexeme is ")
            (display lexeme)
            (newline)
            (token-NUM (string->number lexeme)))]
         [identifier
          (begin
            (printf "Next token is: VAR , next lexeme is ")
            (display lexeme)
            (newline)
            (token-VAR lexeme))]
         [whitespace (abrv_lex input-port)] ; Skip whitespace
         [(eof)
          (begin
            (printf "Next token is: EOF , next lexeme is ")
            (display lexeme)
            (newline)
            (token-EOF))]))

(define-struct let-exp (var num exp))
(define-struct arith-exp (op e1 e2))
(define-struct num-exp (n))
(define-struct var-exp (i))
(define-struct if-exp (condition then-branch else-branch))

(define abrv_parse
  (parser (start exp)
          (end EOF)
          (error void)
          (tokens a b)
          (precs (left + -) (left * / %) (left GREATER LESS NOTEQUALS ISEQUALS) (right ASGN))
          (grammar (exp ;
                    ((FALSE) (num-exp #f))
                    ((TRUE) (num-exp #t))
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
                    ((IFOP exp THOP exp ELOP exp) (make-if-exp $2 $4 $6))
                    ((NUM) (num-exp $1))
                    ((VAR) (var-exp $1))
                    ((LPRN exp RPRN) $2)))))

(define (eval parsed-exp)
  (match parsed-exp
    [(let-exp var num exp) (eval (subst var num exp))]
    [(arith-exp op e1 e2) (op (eval e1) (eval e2))]
    [(num-exp n) n]
    [(var-exp i) (error 'eval "undefined identifier ~a" i)]
    [(if-exp condition then-branch else-branch)
     (if (eval condition) (eval then-branch) (eval else-branch))]))

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

; helper function for the input
(define (input str)
  (let ([input (open-input-string str)]) (eval (abrv_parse (lex-this abrv_lex input)))))

; paren
(input "lprn 5 rprn")
(input "x asgn lprn 5 isne 4 rprn")
(input "lprn 3 adtn 3 rprn mltp 3")
(input "lprn 3 mltp 3 rprn adtn 3")
(input "lprn 3 adtn 3 rprn isne 6")
(input "lprn 3 adtn 2 rprn mltp 5")

; assign
(input "42")
(input "x asgn 4")
(input "x asgn 5 y asgn x mltp 2 x mltp y")
(input "x asgn lprn 10 dvsn lprn 2 adtn 3 rprn rprn")

; mathmetical
(input "3 adtn 4")
(input "5 sbtn 2")
(input "7 mltp 3")
(input "10 dvsn 2")
(input "10 rmdr 3")

; relational
(input "3 iseq 3")
(input "3 iseq 4")
(input "3 isne 4")
(input "3 isne 3")
(input "3 isgr 3")
(input "4 isgr 3")
(input "3 isls 3")
(input "2 isls 3")
(input "5 isgr 3")
(input "2 isls 3")
(input "5 isne 5")

; logic
(input "true")
(input "flse")
(input "true bind flse")
(input "true bior flse")
(input "3 adtn 2 mltp 5")
