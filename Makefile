

check: check-sbcl check-ccl check-clisp check-ecl


check-sbcl:
	@echo "-------- SBCL --------"
	sbcl --script run-test.lisp
	@echo ""

check-clisp:
	@echo "-------- CLISP --------"
	clisp  run-test.lisp

check-ccl:
	@echo "-------- CCL  --------"
	ccl --quiet --load run-test.lisp

check-ecl:
	@echo "-------- ECL  --------"
	ecl -load run-test.lisp -eval '(quit)'
