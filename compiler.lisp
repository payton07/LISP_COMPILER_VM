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
        (setf load-code (append load-code (compile-expr val env nil) '((PUSH ACC)))))
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
         (setf args-code (append args-code (compile-expr arg env nil) '((PUSH ACC)))))
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
            (compile-expr arg1 env nil) ;; Arg1 -> ACC
            '((PUSH ACC))
            (compile-expr arg2 env nil) ;; Arg2 -> ACC
            '((PUSH ACC))
            '((POP R1))  ;; R1 = Arg2
            '((POP ACC)) ;; ACC = Arg1
            (case op
              (+ '((ADD R1)))
              (- '((SUB R1)))
              (* '((MUL R1)))
              (/ '((DIV R1)))
              (< `((CMP ACC R1) (JLT ,l-true) (LOAD (:LIT nil)) (JMP ,l-end) (LABEL ,l-true) (LOAD (:LIT t)) (LABEL ,l-end)))
              (> `((CMP ACC R1) (JGT ,l-true) (LOAD (:LIT nil)) (JMP ,l-end) (LABEL ,l-true) (LOAD (:LIT t)) (LABEL ,l-end)))
              (= `((CMP ACC R1) (JEQ ,l-true) (LOAD (:LIT nil)) (JMP ,l-end) (LABEL ,l-true) (LOAD (:LIT t)) (LABEL ,l-end)))))))

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

(defun vm-compile (expr)
  (append (compile-expr expr (make-env nil nil) nil)
          '((HALT))))

(defun vm-cle (vm expr)
  (let ((code (vm-compile expr)))
    (format t "ASM Généré: ~a~%" code)
    (vm-load vm code)
    (vm-run vm)
    (vm-acc vm)))