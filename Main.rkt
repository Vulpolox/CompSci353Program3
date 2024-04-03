#lang racket

(require "FileReader.rkt") ; provides formatted data in "refined-account-lines" and "refined-transaction-lines"


; pre  -- takes no arguments on initial call; takes updated account data and an output on subsequent recursive calls
; post -- returns a list of all unique account numbers
(define (get-account-numbers [data refined-account-lines] [output '()])

  (cond

    [(empty? data)
     output]

    [else
     (let* ([number-to-add (first (first data))]
            [updated-output (append output (list number-to-add))])
       
       (get-account-numbers (cdr data) updated-output))]))


; pre  -- takes an account number
; post -- returns the name associated with the account number
(define (get-name account-number)

  (define (predicate lst)
    (equal? account-number (list-ref lst 0)))

  (define target-entry (filter predicate refined-account-lines))

  (second (first target-entry)))


; pre  -- takes an account number
; post -- returns the initial balance associated with the account number
(define (get-starting-balance account-number)

  (define (predicate lst)
    (equal? account-number (list-ref lst 0)))

  (define target-entry (filter predicate refined-account-lines))

  (third (first target-entry)))


; pre  -- takes an account number
; post -- returns a list of all relevant transaction information associated with the account sorted by timestamp;
;         each element of the list will be formatted as such: '(timestamp transaction-type method amount)
(define (get-transaction-data account-number)

  (define (predicate lst)                      ; predicate function to help with filtering transactions
    (equal? account-number (list-ref lst 1)))

  (define (compare current next)               ; comparison function to help with sorting transactions by timestamps
    (< (string->number (first current))
       (string->number (first next))))

  (define target-entries (filter predicate refined-transaction-lines)) ; a list containing all the transactions associated with the account number

  (define (loop target-entries [output '()])                           ; helper function for extracting all the data that will be used from the transactions
    
    (cond
      
      [(empty? target-entries)                                                     ; if there are no more transactions, return the output list (base case)
       output]

      [else
       (let* ([timestamp (third (car target-entries))]                             ; integer representing order of transactions
             [transaction-type (first (car target-entries))]                       ; purchase or payment
             [method (fourth (car target-entries))]                                ; credit, debit, check, cash, or vendor
             [amount (last (car target-entries))]                                  ; the amount of money transferred
             [transaction-to-add (list timestamp transaction-type method amount)]) ; list containing all the above values

         (loop (cdr target-entries) (cons transaction-to-add output)))]))          ; make recursive call

  (sort (loop target-entries) compare))         ; return list of relevant transaction data sorted by timestamp


; pre  -- takes an account number
; post -- returns the sum of all payments made by the account
(define (get-total-payment account-number)
  (_get-total account-number "Payment"))

; pre  -- takes an account number
; post -- returns the sum of all purchases made by the account
(define (get-total-purchase account-number)
  (_get-total account-number "Purchase"))

; pre  -- takes an account number
; post -- returns the ending balance of the account after all transactions
(define (get-ending-balance account-number)

  (define starting-balance-converted (_mod-string->number (get-starting-balance account-number)))
  
  (+ starting-balance-converted
     (- (get-total-purchase account-number) (get-total-payment account-number))))

;-- helper functions------------------------------------------------------------------

; pre  -- takes an account number and a transaction type
; post -- returns the sum of all specified transactions made by the account
(define (_get-total account-number transaction-type)

  (define (predicate lst)
    (equal? transaction-type (second lst)))
    
  (define transaction-data (get-transaction-data account-number))
  (define target-data (filter predicate transaction-data))
  (define target-elements (_get-at-index target-data 3))

  (if (empty? target-elements)
      
      0
      
      (let ([converted-elements (map string->number target-elements)])
        (foldl + 0 converted-elements))))
    

; pre  -- takes a list of lists and an integer index; all sublists must have at least index+1 elements
; post -- returns a list containing all elements of the sublists at the specified index
(define (_get-at-index lst index [output '()])

  (cond
    
    [(empty? lst)
     output]

    [else
     (let ([element-to-add (list-ref (first lst) index)])

       (_get-at-index (cdr lst) index (cons element-to-add output)))]))


; pre  -- takes a string
; post -- strips the string of all whitespace and then converts it to a number
(define (_mod-string->number str)

  (define intermediate-list (string-split str " "))
  (string->number (first intermediate-list)))


; -- account object------------------------------------------------------------------

; pre  -- takes an account number
; post -- creates and returns a list of account information based on account number
;         format: '(account-number name starting-balance transaction-list total-purchase total-payment ending-balance)
(define (create-account-object account-number)

  (define name (get-name account-number))
  (define starting-balance (get-starting-balance account-number))
  (define transaction-list (get-transaction-data account-number))
  (define total-purchase (get-total-purchase account-number))
  (define total-payment (get-total-payment account-number))
  (define ending-balance (get-ending-balance account-number))

  (list account-number name starting-balance transaction-list total-purchase total-payment ending-balance))


; pre  -- takes a list of account numbers
; post -- returns a list of account objects
(define (create-account-object-list account-number-list)
  (map create-account-object account-number-list))

(define account-object-list (create-account-object-list (get-account-numbers)))
