(ql:quickload :lisp-unit)
(load "assembler")

(in-package :in.asutoshpalai.sic)

(use-package :lisp-unit)

(define-test asm-firstpass
            (assert-equal 59 (hash-table-count +optab+))
            (assert-false (is-mnemonic "MYNAME"))
            (assert-true (is-mnemonic "RSUB"))
            (assert-true (is-mnemonic "MUL"))
            (assert-equal 1 (optab-entry-number-of-operands (is-mnemonic "ADD")))
            (assert-equal 2 (optab-entry-number-of-operands (is-mnemonic "RMO")))
            (assert-equal 0 (optab-entry-number-of-operands (is-mnemonic "RSUB")))
            (assert-true (string= "DLOOP TD INDEV" (remove-comment "DLOOP TD INDEV .This is a comment")))
            (assert-true (string= "" (remove-comment "  .This is a full line comment")))
            (assert-equalp (make-source-line
                             :label "LABC"
                             :mnemonic "SUB"
                             :operand '("FOUR")
                             :mne-details (gethash "SUB" +optab+)) 
                           (parse-line "LABC SUB FOUR "))
            (assert-equalp (make-source-line
                             :mnemonic "TIX"
                             :operand '("NINE")
                             :mne-details (gethash "TIX" +optab+)) 
                           (parse-line "TIX NINE"))
            (assert-equalp (make-source-line
                             :label "EXIT"
                             :mnemonic "RSUB"
                             :mne-details (gethash "RSUB" +optab+)) 
                           (parse-line "EXIT RSUB ")))

(setq *print-failures* t)
(run-tests)
