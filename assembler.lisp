(load "packages")

(in-package :in.asutoshpalai.sic)

(defstruct optab-entry
  "Structure for storing the opcodes and other details like number of operands 
  and format"
  (mnemonic (error "must supply the menemonic") :type string)
  (number-of-operands (error "must supply the number of operands") :type integer)
  (opcode (error "must supply the opcode") :type integer)
  (format (error "must supply the format") :type string))

(defconstant +optab+
  (let ((ht (make-hash-table :test 'equalp)))
    (with-open-file (infile "optab.txt")
      (loop for line = (read-line infile nil)
	 while line do
	   (destructuring-bind (menem nop opcode format)
	       (split "[\\s\\t]+" line :omit-unmatched-p t)
	     (setf (gethash menem ht)
		   (make-optab-entry
		    :mnemonic menem
		    :number-of-operands (parse-integer nop)
		    :opcode (parse-integer opcode :radix 16)
		    :format format)))))
    ht)
    "Hash table storing the optab-entries structs with opcode as key")

(defstruct source-line
  "Represents a parsed source line"
  loc
  label
  mnemonic
  operand
  mne-details)

(defun is-mnemonic (st)
  "Checks if the symbol is present in +optab+. If present it returns the
  optab-entry else nil" 
  (gethash st +optab+))

(defun remove-comment (line)
  "Removes the comment part of a single line"
  (regex-replace-all "[\\s\\t]*\\..*$" line ""))

(defun parse-line (line)
  "Takes a line from the source code and creates the source-line strucure.
  Comments in line should be removed beforehand"
  (let* ((parts (split "[\\s\\t]+" line :omit-unmatched-p t))
	 (size (length parts))
	 (sline (make-source-line)))
    (cond
      ((setf (source-line-mne-details  sline) (is-mnemonic (first parts)))
	(progn
	  (setf (source-line-mnemonic sline) (first parts))
	  (setf parts (rest parts))
	  (setf size (- size 1))))
      ((setf (source-line-mne-details  sline) (is-mnemonic (second parts)))
       (progn
	  (setf (source-line-label sline) (first parts))
	  (setf (source-line-mnemonic sline) (second parts))
	  (setf parts (cddr parts))
	  (setf size (- size 2))))
      (t
       (error (concatenate 'string "Source line is in wrong format: " line))))
    (setf (source-line-operand sline) parts)
    sline))
