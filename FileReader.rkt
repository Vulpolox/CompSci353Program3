#lang racket

; provide the formatted data for use in other .rkt files
(provide refined-account-lines)
(provide refined-transaction-lines)

; pre  -- takes a list of strings and a delimiter
; post -- splits each of the strings in the list into sublists of strings separated by specified delimiter
(define (refine-lines lines delimiter)
  
  (define (delim-split line)        ; helper function for splitting a string on a specified character
    (string-split line delimiter))

  (map delim-split lines))          ; return the list with delim-split applied to all elements


; raw data directly from input files
(define raw-account-lines (file->lines "ACCOUNTS.txt"))
(define raw-transaction-lines (file->lines "TRANSACTIONS.txt"))

; formatted data
(define refined-account-lines (refine-lines raw-account-lines "  "))
(define refined-transaction-lines (refine-lines raw-transaction-lines "\t"))