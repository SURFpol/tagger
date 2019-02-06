.PHONY: repl

repl:
	clj -A:repl

rebl:
	clj -A:rebl

ui:
	clj -m tagger.core

outdated:
	clojure -Aoutdated -a outdated
