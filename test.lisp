(load "vm.lisp")
(load "compiler.lisp")

(defun run-test ()
  (format t "~%--- DEBUT DU TEST (Fibonacci) ---~%")
  (let ((vm (make-vm)))
    ;; Test: Fibonacci de 6 (doit retourner 8)
    (let ((result (vm-cle vm '(progn
                                (defun fib (n)
                                  (if (< n 2)
                                      n
                                      (+ (fib (- n 1)) (fib (- n 2)))))
                                (fib 6)))))
      (format t "~%--- RESULTAT ---~%")
      (format t "Fib(6) = ~a~%" result)
      (if (= result 8)
          (format t "SUCCESS: Le resultat est correct.~%")
          (format t "FAILURE: Attendu 8, recu ~a.~%" result))))
  (format t "--- FIN DU TEST ---~%"))

(run-test)
(quit)
