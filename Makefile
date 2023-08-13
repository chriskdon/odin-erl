.PHONY: build_tests run_tests clean

build_tests:
	@./scripts/builder.sh build_integration_tests

run_tests: build_tests
	@./scripts/builder.sh run_integration_tests

clean:
	@./builder.sh clean_builds