.PHONY: test

repl:
	clj -M:nrepl:test

test:
	clj -A:test:kaocha

test-ci:
	clojure -A:test:kaocha
