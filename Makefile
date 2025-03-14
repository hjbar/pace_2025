default: clear clean fmt build exec

outsave:
	@mkdir -p saveouts
	@find . -type f -name "*.out" -exec mv -n {} saveouts/. \;

fullclean: clean outclean
	@find . -type f -name "*.dot" -delete
	@find . -type f -name "*.pdf" -delete

outclean: clean
	@find . -type f -name "*.out" -delete
	@rm -rf saveouts

clean:
	@dune clean

build:
	@dune build

fmt:
	@dune fmt

clear:
	@clear

exec:
	@dune exec src/main.exe
