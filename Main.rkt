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
; post -- returns a list of all relevant transaction information associated with the account
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

(get-transaction-data "982340982348")
      
      
