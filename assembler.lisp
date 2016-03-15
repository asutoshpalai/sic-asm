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
  (let* ((parts (all-matches-as-strings "[^\\s\\t]+" line))
	 (size (length parts))
	 (sline (make-source-line)))
    (cond
      ((setf
	(values (source-line-mne-details  sline) (source-line-asm-dir  sline))
	(is-mnemonic (first parts)))
	(progn
	  (setf (source-line-mnemonic sline) (first parts))
	  (setf parts (rest parts))
	  (setf size (- size 1))))
      ((setf
	(values (source-line-mne-details  sline) (source-line-asm-dir  sline))
	(is-mnemonic (second parts)))
       (progn
	  (setf (source-line-label sline) (first parts))
	  (setf (source-line-mnemonic sline) (second parts))
	  (setf parts (cddr parts))
	  (setf size (- size 2))))
      (t
       (error (concatenate 'string "Source line is in wrong format: " line))))
    (setf (source-line-operand sline) parts)
    sline))

(defun process-line (line)
  (if line
      (let ((line (remove-comment line)))
        (if (> (length line) 0)
          (parse-line line)
          nil))
      nil))

(defun int->bytelist (num)
  (loop for x = num then (ash x -8) with res = nil
     while  (> x 0) do
       (push (ldb (byte 8 0) x) res)
       finally (return res)))

(defun literal->bytelist (literal)
  (cond
    ((char-equal #\C (char literal 0))
     (map 'list 'char-code (scan-to-strings "(?<=[Cc]')(.*)(?=')" literal)))
    ((char-equal #\X (char literal 0))
     (int->bytelist (parse-integer (scan-to-strings "(?<=[Xx]')(.*)(?=')" literal) :radix 16)))
    (t
     (int->bytelist (parse-integer literal)))))
    

(defun pass1 (filename)
  "Assigns locations to the symbols and returns the hash-table for location, starting address, length of program"
  (with-open-file (file filename)
    (with-open-file(outfile (concatenate 'string filename ".imd") :direction :output :if-exists :supersede)
      (let ((locctr 0)
            (start 0)
            (symtab (make-hash-table :test 'equalp)))
        ;; check for start
        (loop for sline = (process-line (read-line file nil))
	 if sline do
	     (progn
               (if (equalp (source-line-mnemonic sline) "START")
                   (progn
                     (setf locctr
                           (setf start (parse-integer (first (source-line-operand sline)) :radix 16)))
                     (format outfile "~S~%" sline))
                   (file-position file 0))
               (return)))
        (loop for line = (read-line file nil)
           while line do
             (let ((sline (process-line line)))
               (if sline
		 (let ((label (source-line-label sline))
		       (mnemonic (source-line-mnemonic sline)))
                   (setf (source-line-loc sline) locctr)
                   (if label
		       ;; entering label into symtab
		       (if (gethash label symtab)
			   (error (concatenate 'string "Redefination of symbol: " line))
			   (setf (gethash label symtab) locctr)))
                   (format outfile "~S~%" sline)
		   (if (equalp mnemonic "END")
		       (return) ; from loop
                       (progn
                         (setf locctr (+ locctr
				   (cond
				     ((not (source-line-asm-dir sline)) 3)
				     ((equalp mnemonic "WORD") 3)
				     ((equalp mnemonic "BYTE")
				      (length (literal->bytelist (first (source-line-operand sline)))))
				     ((equalp mnemonic "RESB")
				      (parse-integer (first (source-line-operand sline))))
				     ((equalp mnemonic "RESW")
				      (* 3 (parse-integer (first (source-line-operand sline)))))
				     (t
				      (error (concatenate 'string "Assember directive not yet supported" mnemonic))))))
                        #+(or) (format outfile "\"~4,'0X\"~C~S~C~S~C~S~%"
                                 locctr #\Tab
                                 (source-line-label sline) #\Tab
                                 (source-line-mnemonic sline) #\Tab
                                 (source-line-operand sline))))))))
        (values symtab start (- locctr start))))))

