(load "vm.lisp")
(load "compiler.lisp")

(defun run-test-persistence ()
  (format t "~%--- DEBUT DU TEST PERSISTANCE (Definition puis Appel) ---~%")
  (vm-create 'PERSIST-VM)
  
  ;; Etape 1 : Charger la définition de FIB
  (format t "~%[Step 1] Definition de FIB...~%")
  (vm-cle 'PERSIST-VM 
    '(defun fib (n)
       (if (< n 2)
           n
           (+ (fib (- n 1)) (fib (- n 2))))))
           
  ;; Etape 2 : Appeler FIB 10
  (format t "~%[Step 2] Appel de (fib 10)...~%")
  (let ((result (vm-cle 'PERSIST-VM '(fib 10))))
       
    (if (= result 55)
        (format t "SUCCESS: Fib(10) = ~a.~%" result)
        (format t "FAILURE: Attendu 55, recu ~a.~%" result)))
        
  (format t "--- FIN DU TEST PERSISTANCE ---~%"))

(run-test-persistence)
