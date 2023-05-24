TEST_COV_D = /tmp/schedcov

clean:
	dune clean

.PHONY: test_coverage coverage
test_coverage: coverage
coverage:
	if [ -d $(TEST_COV_D) ]; then rm -r $(TEST_COV_D); fi
	mkdir -p $(TEST_COV_D)
	BISECT_FILE=$(TEST_COV_D)/sched dune runtest Schedule \
          --instrument-with bisect_ppx --force
	bisect-ppx-report html --coverage-path $(TEST_COV_D)
	bisect-ppx-report summary --coverage-path $(TEST_COV_D)

send_coverage: clean test_coverage
	bisect-ppx-report send-to Coveralls --coverage-path $(TEST_COV_D)