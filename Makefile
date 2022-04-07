.PHONY: build test nrepl repl repl/rebl repl/nrepl test

build repl repl/rebl repl/nrepl:
	clojure -M:$@

test:
	clojure -X:test

nrepl: repl/nrepl
rebl: repl/rebl

