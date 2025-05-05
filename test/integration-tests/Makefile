.PHONY: clean test

clean:
	rm -rf ./node_modules

node_modules:
	nix-shell -p 'nodejs' --run 'npm install'

test: node_modules
	PATH=$(GHC):$$PATH ./node_modules/.bin/mocha
