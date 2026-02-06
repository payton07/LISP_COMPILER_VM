(load "vm.lisp")
(load "compiler.lisp")

(defun run-test ()
  (format t "~%--- DEBUT DU TEST (Fibonacci) ---~%")
  (vm-create 'TER)
  ;; Test: Fibonacci de 6 (doit retourner 8)
  (let ((result (vm-cle 'TER '(progn
                                (defun fib (n)
                                  (if (< n 2)
                                      n
                                      (+ (fib (- n 1)) (fib (- n 2)))))
                                (fib 6)))))
    (if (= result 8)
        (format t "SUCCESS: Le resultat est correct.~%")
        (format t "FAILURE: Attendu 8, recu ~a.~%" result)))
  (format t "--- FIN DU TEST ---~%"))

(run-test)
(quit)
