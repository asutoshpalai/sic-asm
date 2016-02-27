(load "packages")

(in-package :in.asutoshpalai.sic)

(defun concat-syms (&rest syms)
    (intern (apply #'concatenate 'string (mapcar #'symbol-name syms))))

(defun make-keyword (&rest names)
  (values (intern (apply #'concatenate 'string (mapcar #'symbol-name names)) "KEYWORD")))

(defun interleave (list1 list2)
  (if (null list1)
    list2
    (cons (car list1) (interleave list2 (cdr list1)))))

(defun create-tab-struct-entries (parts)
  (loop for part in parts
         collect
       `(,(first part)
          (error ,(concatenate 'string "Must supply the " (symbol-name (first part))))
          :type ,(second part))))

(defun create-tab-make-arguments (parts)
  (loop for part in parts
     append
       `(,(make-keyword (first part)) ,(cond
					((equal (first part) 'opcode)
					 `(parse-integer ,(first part) :radix 16))
					 ((equal (second part) 'integer)
					  `(parse-integer ,(first part)))
					 (t
					  (first part))))))

(defmacro create-tab (name filename parts)
  (let ((st-name (concat-syms name '-entry))
        (con-name (concat-syms '+ name '+))
        (fstream (gensym))
        (htab (gensym))
        (line (gensym))
        (part-names (mapcar 'first parts)))
    `(progn
      (defstruct ,st-name
       ,@(create-tab-struct-entries parts))
    (defconstant ,con-name
                  (let ((,htab (make-hash-table :test 'equalp)))
                    (with-open-file (,fstream ,filename)
                      (loop for ,line = (read-line ,fstream nil)
                            while ,line do
                            (destructuring-bind ,part-names
                              (split "[\\s\\t]+" ,line :omit-unmatched-p t)
                              (setf (gethash ,(first (first parts)) ,htab)
				    (,(concat-syms 'make- st-name)
				      ,@(create-tab-make-arguments parts)
                              )))))
                    ,htab)))))

(create-tab optab "optab.txt"
	    ((mnemonic string) (number-of-operands integer) (opcode integer) (format string)))

(create-tab dirtab "asmdir.txt"
	    ((mnemonic string) (number-of-operands integer)))

#+(or)
(defstruct optab-entry
  "Structure for storing the opcodes and other details like number of operands
  and format"
  (mnemonic (error "must supply the menemonic") :type string)
  (number-of-operands (error "must supply the number of operands") :type integer)
  (opcode (error "must supply the opcode") :type integer)
  (format (error "must supply the format") :type string))

#+(or)
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
  (asm-dir nil :type boolean)
  operand
  mne-details)

(defun is-mnemonic (st)
  "Checks if the symbol is present in +optab+ or in +dirtab+. If present it returns the
  optab-entry else nil. The second return value is wether it was from +dirtab+."
  (gethash st +dirtab+ (gethash st +optab+)))

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
      ((setf (values (source-line-mne-details  sline) (source-line-asm-dir  sline)) (is-mnemonic (first parts)))
	(progn
	  (setf (source-line-mnemonic sline) (first parts))
	  (setf parts (rest parts))
	  (setf size (- size 1))))
      ((setf (values (source-line-mne-details  sline) (source-line-asm-dir  sline)) (is-mnemonic (second parts)))
       (progn
	  (setf (source-line-label sline) (first parts))
	  (setf (source-line-mnemonic sline) (second parts))
	  (setf parts (cddr parts))
	  (setf size (- size 2))))
      (t
       (error (concatenate 'string "Source line is in wrong format: " line))))
    (setf (source-line-operand sline) parts)
    sline))
