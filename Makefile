all:

clean:

lint:
	./scripts/lint.sh

build:
	./scripts/build.sh

test:
	./scripts/test.sh

.PHONY: all clean lint build test
