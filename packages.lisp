(in-package :cl-user)

(ql:quickload :cl-ppcre)

(defpackage :in.asutoshpalai.sic
  (:use :common-lisp :cl-ppcre)
  (:export
   :assemble-sic
   :+optab+))
