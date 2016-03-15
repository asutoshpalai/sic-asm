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
                           (parse-line "EXIT RSUB "))
            (assert-error 'error (parse-line "EXIT BLAH BLAH2"))
            (assert-equalp (make-source-line
                             :mnemonic "START"
                             :operand '("1000")'
                             :mne-details  (gethash "START" +dirtab+)
                             :asm-dir t)
                           (parse-line "   START 1000"))
            (assert-equalp (make-source-line
                             :label "EOF"
                             :mnemonic "BYTE"
                             :operand '("C'EOF'")'
                             :mne-details  (gethash "BYTE" +dirtab+)
                             :asm-dir t)
                           (parse-line "EOF BYTE C'EOF'"))
            (assert-equal '(8 42) (int->bytelist 2090))
            (assert-equal '(143 11 156) (int->bytelist 9374620))
            (assert-equal '(66 97 100) (literal->bytelist "C'Bad'"))
            (assert-equal '(69 185) (literal->bytelist "X'45B9'"))
            (assert-equal '(252) (literal->bytelist "X'FC'"))
            (assert-equal '(143 11 156) (literal->bytelist "9374620"))
            (multiple-value-bind (symtab start len) (pass1 "testfiles/test1.asm")
              (assert-equal 4102 (gethash "inloop" symtab))
              (assert-equal 4114 (gethash "outlp" symtab))
              (assert-equal 4135 (gethash "indev" symtab))
              (assert-equal 4136 (gethash "outdev" symtab))
              (assert-equal 4137 (gethash "data" symtab))
              (assert-equal 4138 (gethash "dest" symtab))
              (assert-equal 4141 (gethash "four" symtab))
              (assert-equal 4144 (gethash "nine" symtab))
              (assert-equal 4096 start)
              (assert-equal 51 len)))

(setq *print-failures* t)
(run-tests)
