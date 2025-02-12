default: clear clean fmt build exec

clean:
	@dune clean
	@find . -type f -name "*.dot" -delete
	@find . -type f -name "*.pdf" -delete

build:
	@dune build

fmt:
	@dune fmt

clear:
	@clear

exec:
	@dune exec src/main.exe
