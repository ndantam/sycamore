

check: check-sbcl check-ccl check-clisp


check-sbcl:
	sbcl --script run-test.lisp

check-clisp:
	clisp  run-test.lisp

check-ccl:
	ccl --quiet --load run-test.lisp
