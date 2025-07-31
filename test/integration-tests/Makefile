all: test

.PHONY: clean test

node := $(shell nix-shell -p nodejs --run 'which node')

clean:
	rm -rf ./node_modules

node_modules:
	nix-shell -p 'nodejs' --run 'npm install'

test: node_modules
	# PATH=$(dir $(GHC)):$(dir $(DEBUGGER)):$$PATH ./node_modules/.bin/mocha -f 'allow arbitrarily deep'
	# nix-shell -p nodejs --run 'PATH=$(dir $(GHC)):$(dir $(DEBUGGER)):$$PATH ./node_modules/.bin/mocha'
	@echo "NODE: $(node)"
	PATH=$(dir $(GHC)):$(dir $(DEBUGGER)):$(dir $(node)):$$PATH ./node_modules/.bin/mocha # --parallel
