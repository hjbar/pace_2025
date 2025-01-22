default: clear clean fmt build exec

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
