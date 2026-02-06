;;; -----------------------------------------------------------------------------
;;; COMPILATEUR LISP -> ASM (Machine Accumulateur)
;;; -----------------------------------------------------------------------------

(defvar *label-counter* 0)

(defun gen-label (&optional (prefix "L"))
  (incf *label-counter*)
  (intern (format nil "~a~d" prefix *label-counter*)))

;; Structure de l'environnement de compilation
;; Liste : (locals args)
;; locals: Variables du LET courant
;; args: Arguments de la fonction courante
(defun make-env (locals args) (list locals args))

(defun compile-expr (expr env code)
  (cond
    ((null expr) (cons `(LOAD (:LIT nil)) code))
    ((eq expr t) (cons `(LOAD (:LIT t)) code))
    ((numberp expr) (cons `(LOAD (:LIT ,expr)) code))
    ((symbolp expr) (compile-var expr env code))
    ((atom expr) (cons `(LOAD (:LIT ,expr)) code))
    ((listp expr)
     (case (car expr)
       ('quote (cons `(LOAD (:LIT ,(cadr expr))) code))
       ('progn (compile-progn (cdr expr) env code))
       ('if    (compile-if (cadr expr) (caddr expr) (cadddr expr) env code))
       ('let   (compile-let (cadr expr) (cddr expr) env code))
       ('defun (compile-defun (cadr expr) (caddr expr) (cdddr expr) env code))
       (t      (compile-call (car expr) (cdr expr) env code))))))

(defun compile-var (var env code)
  (let ((locals (car env))
        (args   (cadr env)))
    (let ((p-loc (position var locals))
          (p-arg (position var args)))
      (cond
        (p-loc 
         ;; Variable Locale (LET) : FP - 1 - index
         ;; Stack: [SavedFP] [Loc1] [Loc2] ...
         ;; Index 0 (Loc1) -> FP - 1
         ;; Index 1 (Loc2) -> FP - 2
         (cons `(LREF (:LIT ,(- -1 p-loc))) code))
        (p-arg 
         ;; Argument : FP + 1 + (N - 1 - index)
         ;; Stack: [Arg1] [Arg2] [PC] [SavedFP]
         ;; FP points to PC.
         ;; Arg2 (index 1, len 2) -> FP + 1
         ;; Arg1 (index 0, len 2) -> FP + 2
         (let ((offset (+ 2 (- (length args) 1 p-arg))))
           (cons `(LREF (:LIT ,offset)) code)))
        (t 
         ;; Globale
         (cons `(LOAD (:LIT ,var)) code))))))

(defun compile-progn (exprs env code)
  (if (null exprs)
      code
      (let ((c (compile-expr (car exprs) env code)))
        (compile-progn (cdr exprs) env c))))

(defun compile-if (cond-expr then-expr else-expr env code)
  (let ((l-else (gen-label "ELSE"))
        (l-end (gen-label "END")))
    (let* ((c-cond (compile-expr cond-expr env nil))
           (c-then (compile-expr then-expr env nil))
           (c-else (compile-expr else-expr env nil)))
      (append code
              c-cond
              `((JNIL ,l-else))
              c-then
              `((JMP ,l-end))
              `((LABEL ,l-else))
              c-else
              `((LABEL ,l-end))))))

(defun compile-let (bindings body env code)
  (let ((vars (mapcar #'car bindings))
        (vals (mapcar #'cadr bindings)))
    (let ((load-code '()))
      (dolist (val vals)
        (setf load-code (append load-code (compile-expr val env nil) '((PUSH R0)))))
      ;; Etendre l'environnement local (prepend vars)
      (let ((new-env (make-env (append vars (car env)) (cadr env))))
        (let ((body-code (compile-progn body new-env nil)))
          ;; Nettoyage pile (POP poubelle)
          (let ((cleanup (loop repeat (length vars) collect '(POP R1))))
            (append code load-code body-code cleanup)))))))

(defun compile-call (func args env code)
  (cond
    ((member func '(+ - * / < > =))
     (compile-primitive func args env code))
    ((eq func 'print)
     (append code 
             (compile-expr (car args) env nil)
             '((PRINT))))
    (t 
     ;; Appel de fonction standard
     (let ((args-code '()))
       (dolist (arg args)
         (setf args-code (append args-code (compile-expr arg env nil) '((PUSH R0)))))
       (append code
               args-code
               `((JSR ,func))
               (loop repeat (length args) collect '(POP R1)))))))

(defun compile-primitive (op args env code)
  (let ((arg1 (car args))
        (arg2 (cadr args))
        (l-true (gen-label "TRUE"))
        (l-end (gen-label "END")))
    (append code
            (compile-expr arg1 env nil) ;; Arg1 -> R0
            '((PUSH R0))
            (compile-expr arg2 env nil) ;; Arg2 -> R0
            '((PUSH R0))
            '((POP R1))  ;; R1 = Arg2
            '((POP R0)) ;; R0 = Arg1
            (case op
              (+ '((ADD R1)))
              (- '((SUB R1)))
              (* '((MUL R1)))
              (/ '((DIV R1)))
              (< `((CMP R0 R1) (JLT ,l-true) (LOAD (:LIT nil)) (JMP ,l-end) (LABEL ,l-true) (LOAD (:LIT t)) (LABEL ,l-end)))
              (> `((CMP R0 R1) (JGT ,l-true) (LOAD (:LIT nil)) (JMP ,l-end) (LABEL ,l-true) (LOAD (:LIT t)) (LABEL ,l-end)))
              (= `((CMP R0 R1) (JEQ ,l-true) (LOAD (:LIT nil)) (JMP ,l-end) (LABEL ,l-true) (LOAD (:LIT t)) (LABEL ,l-end)))))))

(defun compile-defun (name args body env code)
  (let ((l-start (gen-label (string name))))
    ;; Env fonction: locals=nil, args=args
    (let ((new-env (make-env nil args)))
      (let ((body-code (compile-progn body new-env nil)))
        (append code
                `((JMP ,(intern (format nil "END_~a" name))))
                `((LABEL ,name))
                `((SAVE-FP))
                `((SET-FP))
                body-code
                `((RESTORE-FP))
                `((RTN))
                `((LABEL ,(intern (format nil "END_~a" name)))))))))

(defun vm-compile (vm expr)
  (declare (ignore vm))
  (append (compile-expr expr (make-env nil nil) nil)
          '((HALT))))

(defun vm-cle (vm-or-name expr)

  (let ((vm (resolve-vm vm-or-name)))

    (let ((code (vm-compile vm expr)))

      (format t "ASM Généré: ~a~%" code)

      (vm-load vm code)

      (vm-run vm)

      (let ((res (vm-r0 vm)))

        (format t "RESULTAT : ~a~%" res)

        res))))



;;; -----------------------------------------------------------------------------

;;; COMPILATEUR EXTENDED (MAX)

;;; Supporte: setq, while, listes (cons/car/cdr), arrays (aref)

;;; -----------------------------------------------------------------------------



(defun vm-compile-max (vm expr)

  (declare (ignore vm))

  (append '((PUSH (:LIT 0)) (SET-FP)) ;; Fake SavedFP pour aligner le cadre TopLevel

          (compile-expr-max expr (make-env nil nil) nil)

          '((HALT))))



(defun vm-cle-max (vm-or-name expr)

  (let ((vm (resolve-vm vm-or-name)))

    (let ((code (vm-compile-max vm expr)))

      (format t "ASM Généré: ~a~%" code)

      (vm-load vm code)

      (vm-run vm)

      (let ((res (vm-r0 vm)))

        (format t "RESULTAT : ~a~%" res)

        res))))

(defun compile-var (var env code)
  (let ((locals (car env))
        (args   (cadr env)))
    (let ((p-loc (position var locals))
          (p-arg (position var args)))
      (cond
        (p-loc 
         (append code `((LREF (:LIT ,(- -1 p-loc)))))) ;; Changed to append
        (p-arg 
         (let ((offset (+ 2 (- (length args) 1 p-arg))))
           (append code `((LREF (:LIT ,offset))))))    ;; Changed to append
        (t 
         (append code `((LOAD (:LIT ,var)))))))))      ;; Changed to append

(defun compile-expr-max (expr env code)
  (cond
    ((null expr) (append code `((LOAD (:LIT nil)))))       ;; Changed to append
    ((eq expr t) (append code `((LOAD (:LIT t)))))         ;; Changed to append
    ((numberp expr) (append code `((LOAD (:LIT ,expr)))))  ;; Changed to append
    ((symbolp expr) (compile-var expr env code))
    ((atom expr) (append code `((LOAD (:LIT ,expr)))))     ;; Changed to append
    ((listp expr)
     (case (car expr)
       ((quote QUOTE) (append code `((LOAD (:LIT ,(cadr expr)))))) ;; Changed to append
       ((progn PROGN) (compile-progn-max (cdr expr) env code))
       ((if IF)    (compile-if-max (cadr expr) (caddr expr) (cadddr expr) env code))
       ((let LET)   (compile-let-max (cadr expr) (cddr expr) env code))
       ((defun DEFUN) (compile-defun-max (cadr expr) (caddr expr) (cdddr expr) env code))
       ((while WHILE) (compile-while (cadr expr) (cddr expr) env code))
       ((setq SETQ)  (compile-setq (cadr expr) (caddr expr) env code))
       ((cons CONS)  (compile-cons (cadr expr) (caddr expr) env code))
       ((car CAR)   (compile-unary 'CAR (cadr expr) env code))
       ((cdr CDR)   (compile-unary 'CDR (cadr expr) env code))
       ((rplaca RPLACA) (compile-binary 'RPLACA (cadr expr) (caddr expr) env code)) 
       ((rplacd RPLACD) (compile-binary 'RPLACD (cadr expr) (caddr expr) env code))
       ((aref AREF)   (compile-aref (cadr expr) env code)) 
       ((set-aref SET-AREF) (compile-set-aref (cadr expr) (caddr expr) env code)) 
       (t      (compile-call-max (car expr) (cdr expr) env code))))))

(defun compile-progn-max (exprs env code)
  (if (null exprs)
      code
      (let ((c (compile-expr-max (car exprs) env code)))
        (compile-progn-max (cdr exprs) env c))))

(defun compile-if-max (cond-expr then-expr else-expr env code)
  (let ((l-else (gen-label "ELSE"))
        (l-end (gen-label "END")))
    (let* ((c-cond (compile-expr-max cond-expr env nil))
           (c-then (compile-expr-max then-expr env nil))
           (c-else (compile-expr-max else-expr env nil)))
      (append code
              c-cond
              `((JNIL ,l-else))
              c-then
              `((JMP ,l-end))
              `((LABEL ,l-else))
              c-else
              `((LABEL ,l-end))))))

(defun compile-let-max (bindings body env code)
  (let ((vars (mapcar #'car bindings))
        (vals (mapcar #'cadr bindings)))
    (let ((load-code '()))
      (dolist (val vals)
        (setf load-code (append load-code (compile-expr-max val env nil) '((PUSH R0)))))
      (let ((new-env (make-env (append vars (car env)) (cadr env))))
        (let ((body-code (compile-progn-max body new-env nil)))
          (let ((cleanup (loop repeat (length vars) collect '(POP R1))))
            (append code load-code body-code cleanup)))))))

(defun compile-defun-max (name args body env code)
  (let ((l-start (gen-label (string name))))
    (let ((new-env (make-env nil args)))
      (let ((body-code (compile-progn-max body new-env nil)))
        (append code
                `((JMP ,(intern (format nil "END_~a" name))))
                `((LABEL ,name))
                `((SAVE-FP))
                `((SET-FP))
                body-code
                `((RESTORE-FP))
                `((RTN))
                `((LABEL ,(intern (format nil "END_~a" name)))))))))

(defun compile-call-max (func args env code)
  (cond
    ((member func '(+ - * / < > =))
     (compile-primitive-max func args env code))
    ((eq func 'print)
     (append code 
             (compile-expr-max (car args) env nil)
             '((PRINT))))
    (t 
     (let ((args-code '()))
       (dolist (arg args)
         (setf args-code (append args-code (compile-expr-max arg env nil) '((PUSH R0)))))
       (append code
               args-code
               `((JSR ,func))
               (loop repeat (length args) collect '(POP R1)))))))

(defun compile-primitive-max (op args env code)
  (let ((arg1 (car args))
        (arg2 (cadr args))
        (l-true (gen-label "TRUE"))
        (l-end (gen-label "END")))
    (append code
            (compile-expr-max arg1 env nil) 
            '((PUSH R0))
            (compile-expr-max arg2 env nil)
            '((PUSH R0))
            '((POP R1))
            '((POP R0))
            (case op
              (+ '((ADD R1)))
              (- '((SUB R1)))
              (* '((MUL R1)))
              (/ '((DIV R1)))
              (< `((CMP R0 R1) (JLT ,l-true) (LOAD (:LIT nil)) (JMP ,l-end) (LABEL ,l-true) (LOAD (:LIT t)) (LABEL ,l-end)))
              (> `((CMP R0 R1) (JGT ,l-true) (LOAD (:LIT nil)) (JMP ,l-end) (LABEL ,l-true) (LOAD (:LIT t)) (LABEL ,l-end)))
              (= `((CMP R0 R1) (JEQ ,l-true) (LOAD (:LIT nil)) (JMP ,l-end) (LABEL ,l-true) (LOAD (:LIT t)) (LABEL ,l-end)))))))

;; --- Nouvelles Fonctionnalités MAX ---

(defun compile-while (cond body env code)
  (let ((l-start (gen-label "WHILE_START"))
        (l-end (gen-label "WHILE_END")))
    (append code
            `((LABEL ,l-start))
            (compile-expr-max cond env nil)
            `((JNIL ,l-end))
            (compile-progn-max body env nil)
            `((JMP ,l-start))
            `((LABEL ,l-end)))))

(defun compile-setq (var val env code)
  (let ((locals (car env))
        (args   (cadr env)))
    (let ((p-loc (position var locals))
          (p-arg (position var args)))
      (append code
              (compile-expr-max val env nil) ;; Val -> R0
              (cond
                (p-loc 
                 ;; SREF attend un offset positif ou négatif ? SREF implémenté comme: Mem[FP + offset] = R0
                 ;; Variable Locale: index 0 -> FP-1. Offset = -1 - index.
                 `((SREF (:LIT ,(- -1 p-loc)))))
                (p-arg 
                 ;; Argument: index 0 (len 2) -> FP+2. Offset = 2 + len - 1 - index
                 (let ((offset (+ 2 (- (length args) 1 p-arg))))
                   `((SREF (:LIT ,offset)))))
                (t 
                 ;; Globale
                 `((STORE ,var))))))))

(defun compile-cons (car-expr cdr-expr env code)
  (append code
          (compile-expr-max car-expr env nil) ;; CAR -> R0
          '((PUSH R0))                       ;; Push CAR
          (compile-expr-max cdr-expr env nil) ;; CDR -> R0
          '((CONS))))                         ;; CONS (utilise CDR=R0 et CAR=POP)

(defun compile-unary (op arg env code)
  (append code
          (compile-expr-max arg env nil)
          `((,op))))

(defun compile-binary (op arg1 arg2 env code)
  (append code
          (compile-expr-max arg2 env nil) ;; Val (arg2) -> R0
          '((PUSH R0))
          (compile-expr-max arg1 env nil) ;; Cell (arg1) -> R0
          ;; Op attend: Cell dans R0, Val sur Pile
          `((,op))))

(defun compile-aref (index-expr env code)
  ;; (aref index) -> Charge Mem[index]
  (append code
          (compile-expr-max index-expr env nil) ;; Index -> R0
          '((LDI))))                            ;; LDI: R0 = Mem[R0]

(defun compile-set-aref (index-expr val-expr env code)
  ;; (set-aref index val) -> Mem[index] = val
  (append code
          (compile-expr-max index-expr env nil) ;; Index -> R0
          '((PUSH R0))                         ;; Push Index
          (compile-expr-max val-expr env nil)   ;; Val -> R0
          '((POP R1))                           ;; R1 = Index
          '((STI))))                            ;; STI: Mem[R1] = R0

