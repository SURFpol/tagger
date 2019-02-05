.PHONY: repl

repl:
	clj -A:repl

rebl:
	clj -A:rebl

outdated:
	clojure -Aoutdated -a outdated
