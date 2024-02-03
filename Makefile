.PHONY: build
build:
	poetry build

.PHONY: test
test:
	poetry run python -B -m unittest tests -v

.PHONY: test-proto
test-proto:
	poetry run python -B -m unittest tests.test_proto -v
