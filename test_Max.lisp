(load "vm.lisp")
(load "compiler.lisp")

(defun run-test-max ()
  (format t "~%--- DEBUT DU TEST MAX (Listes et Boucles) ---~%")
  (vm-create 'MAX-VM)
  
  ;; Test : Création d'une liste (1 2) et calcul de la somme via WHILE
  ;; Code équivalent:
  ;; (let ((L (cons 1 (cons 2 nil)))
  ;;       (sum 0))
  ;;   (while L
  ;;     (setq sum (+ sum (car L)))
  ;;     (setq L (cdr L)))
  ;;   sum)
  
  (let ((result (vm-cle-max 'MAX-VM 
    '(let ((L (cons 10 (cons 20 nil)))
           (sum 0))
       (while L
         (setq sum (+ sum (car L)))
         (setq L (cdr L)))
       sum))))
       
    (if (= result 30)
        (format t "SUCCESS: Somme (10 20) = ~a.~%" result)
        (format t "FAILURE: Attendu 30, recu ~a.~%" result)))
        
  (format t "--- FIN DU TEST MAX ---~%"))

(run-test-max)
