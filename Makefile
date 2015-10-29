run:
	date
	cabal run

hlint:
	hlint -r src

stylish-haskell:
	find src -name "*.hs" -exec stylish-haskell -i {} \;

pdf:
	cabal build
	dist/build/caronch-code-generator/caronch-code-generator > out.md
	pandoc out.md -o out.pdf -V geometry:a4paper -V geometry:margin=2cm

doc: sourcegraph haddock

sourcegraph:
	docker run -v $$(pwd):/src --rm taeradan/haskell-sourcegraph hahp.cabal

haddock:
	cabal haddock
