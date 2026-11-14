.PHONY: dsl test clean

dsl:
	dune build ./bin/main.exe
	cp -f _build/default/bin/main.exe dsl
	chmod u+w dsl

# positive/*.dsl : must be accepted AND have no deadlock in any schedule
# negative/*.dsl : must be rejected by the type checker (exit 1)
#                  AND really deadlock in some schedule (exit 3 without type check)
test: dsl
	@for f in tests/deadlock/positive/*.dsl; do \
	  [ -e "$$f" ] || continue; \
	  if ./dsl --explore "$$f" > /dev/null 2>&1; then echo "OK:     $$f"; \
	  else echo "FAILED: $$f (should be accepted and deadlock-free)"; fi; \
	done
	@for f in tests/deadlock/negative/*.dsl; do \
	  [ -e "$$f" ] || continue; \
	  ./dsl --check "$$f" > /dev/null 2>&1; rc=$$?; \
	  ./dsl --deadlocks "$$f" > /dev/null 2>&1; dl=$$?; \
	  if [ $$rc -eq 1 ] && [ $$dl -eq 3 ]; then echo "OK:     $$f"; \
	  else echo "FAILED: $$f (should be rejected [got $$rc] and deadlock [got $$dl])"; fi; \
	done

clean:
	dune clean
	rm -f dsl