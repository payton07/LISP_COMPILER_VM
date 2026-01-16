(defstruct vm
  (memory (make-array 10000 :initial-element nil)) ;; Mémoire principale (Code + Tas + Pile)
  (acc nil)      ;; Accumulateur principal
  (r1 nil)       ;; Registre secondaire/temporaire
  (pc 0)         ;; Program Counter
  (sp 9999)      ;; Stack Pointer (descendant)
  (fp 9999)      ;; Frame Pointer
  (labels (make-hash-table :test 'equal)) ;; Table des étiquettes pour les sauts
  (running t))   ;; État de la VM

;; -----------------------------------------------------------------------------
;; Utils d'Affichage (Couleurs)
;; -----------------------------------------------------------------------------

(defconstant +ansi-reset+ (format nil "~c[0m" #\ESC))
(defconstant +ansi-cyan+ (format nil "~c[36m" #\ESC))
(defconstant +ansi-magenta+ (format nil "~c[35m" #\ESC))
(defconstant +ansi-green+ (format nil "~c[32m" #\ESC))
(defconstant +ansi-bold+ (format nil "~c[1m" #\ESC))

;; -----------------------------------------------------------------------------
;; Helpers de la VM
;; -----------------------------------------------------------------------------

(defun vm-error (vm msg)
  (format t "Erreur VM (PC=~a): ~a~%" (vm-pc vm) msg)
  (setf (vm-running vm) nil))

(defun vm-push (vm val)
  (let ((sp (vm-sp vm)))
    (if (<= sp 0)
        (vm-error vm "Stack Overflow")
        (progn
          (setf (aref (vm-memory vm) sp) val)
          (setf (vm-sp vm) (- sp 1))))))

(defun vm-pop (vm)
  (let ((sp (vm-sp vm)))
    (if (>= sp 9999)
        (progn (vm-error vm "Stack Underflow") nil)
        (progn
          (setf (vm-sp vm) (+ sp 1))
          (aref (vm-memory vm) (+ sp 1))))))

(defun resolve-arg (vm arg)
  "Résout un argument :
   - (:LIT x) -> retourne x
   - (:REG 'ACC) -> retourne contenu ACC
   - (:XXX ...) -> autres modes si nécessaire
   - symbole -> considère comme une étiquette (pour les sauts) ou adresse absolue"
  (cond
    ((and (consp arg) (eq (car arg) :LIT)) (cadr arg))
    ((eq arg 'ACC) (vm-acc vm))
    ((eq arg 'R1) (vm-r1 vm))
    ((numberp arg) arg) ;; Adresse directe ou valeur immédiate selon contexte
    (t arg)))

;; -----------------------------------------------------------------------------
;; Instructions (Jeu d'instructions adapté Accumulateur)
;; -----------------------------------------------------------------------------

;; LOAD <src> : Charge src dans ACC
(defun op-load (vm src)
  (setf (vm-acc vm) (resolve-arg vm src)))

;; STORE <dest> : Stocke ACC dans dest (Mémoire ou Registre)
;; Si dest est un index (entier), on écrit en mémoire.
;; Si dest est (:FP relative), on écrit relatif au Frame Pointer.
(defun op-store (vm dest)
  (cond
    ((numberp dest) (setf (aref (vm-memory vm) dest) (vm-acc vm)))
    ((eq dest 'R1) (setf (vm-r1 vm) (vm-acc vm)))
    ((and (consp dest) (eq (car dest) :FP)) ;; Stockage relatif au FP (ex: (:FP -2))
     (let ((addr (+ (vm-fp vm) (cadr dest))))
       (setf (aref (vm-memory vm) addr) (vm-acc vm))))
    (t (vm-error vm (format nil "Destination invalide pour STORE: ~a" dest)))))

;; MOVE <src> <dest> (Legacy support, peut-être utile)
(defun op-move (vm src dest)
  (let ((val (resolve-arg vm src)))
    (cond
      ((eq dest 'ACC) (setf (vm-acc vm) val))
      ((eq dest 'R1)  (setf (vm-r1 vm) val))
      (t (vm-error vm "MOVE supporte seulement ACC ou R1 en dest")))))

;; Opérations Arithmétiques (Résultat dans ACC)
(defun op-add (vm src) (setf (vm-acc vm) (+ (vm-acc vm) (resolve-arg vm src))))
(defun op-sub (vm src) (setf (vm-acc vm) (- (vm-acc vm) (resolve-arg vm src))))
(defun op-mul (vm src) (setf (vm-acc vm) (* (vm-acc vm) (resolve-arg vm src))))
(defun op-div (vm src) (setf (vm-acc vm) (/ (vm-acc vm) (resolve-arg vm src))))

;; Comparaison
;; CMP <a1> <a2> -> Met ACC à T, EQ, LT, GT ou NIL
(defun op-cmp (vm src1 src2)
  (let ((v1 (resolve-arg vm src1))
        (v2 (resolve-arg vm src2)))
    (setf (vm-acc vm) (cond ((= v1 v2) 'EQ)
                            ((< v1 v2) 'LT)
                            ((> v1 v2) 'GT)
                            (t nil)))))

;; Sauts
(defun jump-to (vm label)
  (let ((addr (gethash label (vm-labels vm))))
    (if addr
        (setf (vm-pc vm) addr)
        (vm-error vm (format nil "Label inconnu: ~a" label)))))

(defun op-jmp (vm label) (jump-to vm label))

(defun op-jeq (vm label) (when (eq (vm-acc vm) 'EQ) (jump-to vm label)))
(defun op-jlt (vm label) (when (eq (vm-acc vm) 'LT) (jump-to vm label)))
(defun op-jgt (vm label) (when (eq (vm-acc vm) 'GT) (jump-to vm label)))
;; Sauts génériques sur booléens (si ACC != nil)
(defun op-jtrue (vm label) (when (vm-acc vm) (jump-to vm label)))
(defun op-jnil (vm label)  (unless (vm-acc vm) (jump-to vm label)))

;; Pile
(defun op-push (vm src) (vm-push vm (resolve-arg vm src)))
(defun op-pop (vm dest)
  (let ((val (vm-pop vm)))
    (cond
      ((eq dest 'ACC) (setf (vm-acc vm) val))
      ((eq dest 'R1) (setf (vm-r1 vm) val))
      (t nil)))) ;; POP simple sans destination (discard)

;; Appels de fonction et Sauts
;; JSR (Jump to Subroutine) : Pousse PC, saute à label
(defun op-jsr (vm label)
  (vm-push vm (vm-pc vm))
  (jump-to vm label))

;; RTN (Return) : Pop PC
(defun op-rtn (vm)
  (let ((ret-addr (vm-pop vm)))
    (setf (vm-pc vm) ret-addr)))

;; Gestion de l'environnement (Variables locales)
;; ARGS <n> : Prépare le FramePointer. (Legacy style, ou nous le faisons manuellement dans le compilo)
;; Pour ce projet, nous ferons la gestion FP manuellement via instructions PUSH/MOVE si possible,
;; ou des instructions dédiées pour simplifier le compilateur.
;; Ajoutons LDFP (Load Frame Pointer) et SVFP (Save Frame Pointer) pour faciliter.
(defun op-save-fp (vm) (vm-push vm (vm-fp vm)))
(defun op-set-fp (vm) (setf (vm-fp vm) (+ (vm-sp vm) 1))) ;; FP pointe vers le bas de la nouvelle frame
(defun op-restore-fp (vm) (setf (vm-fp vm) (vm-pop vm)))

;; Instruction spéciale pour lire depuis la pile relative à FP
;; LREF <offset> -> ACC = Stack[FP - offset]
;; offset positif car stack descend : FP est en haut, les args sont au dessus, les vars locales en dessous.
;; Convention :
;;   ...
;;   ARG 1  (FP + 2)
;;   ARG 0  (FP + 1)
;;   SAVED FP (FP)
;;   LOCALS   (FP - 1 ...)
(defun op-lref (vm offset)
  (let ((addr (+ (vm-fp vm) offset)))
    (setf (vm-acc vm) (aref (vm-memory vm) addr))))

;; SREF <offset> : Stack[FP + offset] = ACC
(defun op-sref (vm offset)
  (let ((addr (+ (vm-fp vm) offset)))
    (setf (aref (vm-memory vm) addr) (vm-acc vm))))

;; Fermetures
;; MAKE-CLOSURE <label> <env-size>
;; Crée une fermeture : (vector 'CLOSURE label environment-copy)
(defun op-make-closure (vm label n-env)
  (let ((env (make-array n-env)))
    ;; On suppose que les variables libres sont déjà sur la pile ou accessibles
    ;; Pour simplifier : on pop N valeurs de la pile pour remplir l'env
    (loop for i from 0 below n-env do
       (setf (aref env i) (vm-pop vm)))
    (setf (vm-acc vm) (vector 'CLOSURE label env))))

;; LOAD-ENV <index> : Charge la i-ème variable de l'environnement (stocké dans R1)
(defun op-load-env (vm index)
  (let ((env (vm-r1 vm)))
    (if (arrayp env)
        (setf (vm-acc vm) (aref env index))
        (vm-error vm "LOAD-ENV: R1 ne contient pas d'environnement valide"))))

;; CALL-CLOSURE <n-args>
;; La fermeture est dans ACC (ou sur la pile ? Disons dans ACC pour simplifier le dispatch).
;; Les arguments sont sur la pile.
(defun op-apply (vm)
  (let ((closure (vm-acc vm)))
    (if (and (vectorp closure) (eq (aref closure 0) 'CLOSURE))
        (let ((label (aref closure 1))
              (env (aref closure 2)))
           ;; On stocke l'environnement dans R1 pour que la fonction appelée puisse l'utiliser (via LOAD-ENV)
           (setf (vm-r1 vm) env)
           (op-jsr vm label))
        (vm-error vm "APPLY appelé sur non-clôture"))))

(defun op-halt (vm) (setf (vm-running vm) nil))

;; -----------------------------------------------------------------------------
;; Boucle principale
;; -----------------------------------------------------------------------------

(defun vm-exec (vm op args)
  ;;(format t "EXEC: ~a ~a~%" op args) ;; DEBUG TRACE
  (case op
    (LOAD  (op-load vm (car args)))
    (STORE (op-store vm (car args)))
    (MOVE  (op-move vm (car args) (cadr args)))
    (ADD   (op-add vm (car args)))
    (SUB   (op-sub vm (car args)))
    (MUL   (op-mul vm (car args)))
    (DIV   (op-div vm (car args)))
    (CMP   (op-cmp vm (car args) (cadr args)))
    (JMP   (op-jmp vm (car args)))
    (JEQ   (op-jeq vm (car args)))
    (JLT   (op-jlt vm (car args)))
    (JGT   (op-jgt vm (car args)))
    (JTRUE (op-jtrue vm (car args)))
    (JNIL  (op-jnil vm (car args)))
    (PUSH  (op-push vm (car args)))
    (POP   (op-pop vm (car args)))
    (JSR   (op-jsr vm (car args)))
    (RTN   (op-rtn vm))
    (HALT  (op-halt vm))
    ;; Instructions spécifiques pile/fonctions
    (SAVE-FP (op-save-fp vm))
    (SET-FP  (op-set-fp vm))
    (RESTORE-FP (op-restore-fp vm))
    (LREF    (op-lref vm (cadr (car args)))) ;; Arg is (:LIT offset) usually
    (SREF    (op-sref vm (cadr (car args))))
    (MAKE-CLOSURE (op-make-closure vm (car args) (cadr args)))
    (LOAD-ENV (op-load-env vm (cadr (car args))))
    (APPLY   (op-apply vm))
    (PRINT   (print (vm-acc vm))) ;; Debug
    (t (vm-error vm (format nil "Instruction inconnue: ~a" op)))))

(defun vm-run (vm)
  (setf (vm-running vm) t)
  (loop while (vm-running vm) do
    (let* ((pc (vm-pc vm))
           (instr (aref (vm-memory vm) pc)))
      (if (null instr)
          (vm-error vm "Segfault (Instruction NIL)")
          (progn
            (setf (vm-pc vm) (1+ pc))
            (vm-exec vm (car instr) (cdr instr)))))))

;; -----------------------------------------------------------------------------
;; Chargeur
;; -----------------------------------------------------------------------------

(defun vm-load (vm code)
  ;; Passe 1 : Identifier labels
  (let ((addr 0))
    (dolist (instr code)
      (if (eq (car instr) 'LABEL)
          (setf (gethash (cadr instr) (vm-labels vm)) addr)
          (incf addr))))
  ;; Passe 2 : Charger en mémoire (en sautant les définitions de labels)
  (let ((addr 0))
    (dolist (instr code)
      (unless (eq (car instr) 'LABEL)
        (setf (aref (vm-memory vm) addr) instr)
        (incf addr))))
  (format t "Code chargé (~a instructions).~%" (hash-table-count (vm-labels vm))))

;; -----------------------------------------------------------------------------
;; Bannière de Lancement
;; -----------------------------------------------------------------------------

(defun print-banner ()
  (format t "~%")
  (format t "~a    .____    .____   _____________ __________    ____   ____ _____  ~a~%" +ansi-cyan+ +ansi-reset+)
  (format t "~a    |    |   |    | /   _____/\______   \   \ /   /  /     \\   \ ~a~%" +ansi-cyan+ +ansi-reset+)
  (format t "~a    |    |   |    | \_____  \  |     ___/\   Y   /  /  \ /  \\   \~a~%" +ansi-cyan+ +ansi-reset+)
  (format t "~a    |    |___|    | /        \ |    |     \     /  /    Y    \\   \~a~%" +ansi-cyan+ +ansi-reset+)
  (format t "~a    |_______ \____|/_______  / |____|      \___/   \____|__  / \___\~a~%" +ansi-cyan+ +ansi-reset+)
  (format t "~a            \/             \/                              \/       ~a~%" +ansi-cyan+ +ansi-reset+)
  (format t "~%")
  (format t "    ~a[ SYSTEM ONLINE ]~a :: ~aVersion 2.0 (Redux)~a~%" +ansi-green+ +ansi-reset+ +ansi-magenta+ +ansi-reset+)
  (format t "    ~a[ ARCHITECTURE  ]~a :: ~aAccumulator Based VM~a~%" +ansi-green+ +ansi-reset+ +ansi-magenta+ +ansi-reset+)
  (format t "~%")
  (format t "    ~aAVAILABLE COMMANDS:~a~%" +ansi-bold+ +ansi-reset+)
  (format t "    ~a●~a (make-vm)        :: Create a new VM instance~%" +ansi-cyan+ +ansi-reset+)
  (format t "    ~a●~a (vm-compile exp) :: Compile Lisp expression to ASM~%" +ansi-cyan+ +ansi-reset+)
  (format t "    ~a●~a (vm-load vm asm) :: Load ASM code into VM~%" +ansi-cyan+ +ansi-reset+)
  (format t "    ~a●~a (vm-run vm)      :: Execute VM~%" +ansi-cyan+ +ansi-reset+)
  (format t "    ~a●~a (vm-cle vm exp)  :: Compile, Load, and Execute~%" +ansi-cyan+ +ansi-reset+)
  (format t "~%"))

(print-banner)
