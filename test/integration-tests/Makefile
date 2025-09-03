all: test

.PHONY: clean test

node := $(shell nix develop --command bash -c "which node")

clean:
	rm -rf ./node_modules

node_modules:
	nix develop --command bash -c "npm install && npm run compile"

test: node_modules
	# PATH=$(dir $(GHC)):$(dir $(DEBUGGER)):$$PATH ./node_modules/.bin/mocha -f 'allow arbitrarily deep'
	# nix-shell -p nodejs --run 'PATH=$(dir $(GHC)):$(dir $(DEBUGGER)):$$PATH ./node_modules/.bin/mocha'
	@echo "NODE: $(node)"
	PATH=$(dir $(GHC)):$(dir $(DEBUGGER)):$(dir $(node)):$$PATH npm run test
