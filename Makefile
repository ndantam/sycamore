

check: check-sbcl check-ccl check-clisp check-ecl


check-sbcl:
	sbcl --script run-test.lisp

check-clisp:
	clisp  run-test.lisp

check-ccl:
	ccl --quiet --load run-test.lisp

check-ecl:
	ecl -load run-test.lisp -eval '(quit)'
