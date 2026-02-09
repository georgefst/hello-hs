repl:
	wasm32-unknown-wasi-cabal repl hello-hs -finteractive

watch:
	ghciwatch --after-startup-ghci :main --after-reload-ghci :main --watch . --debounce 50ms --command \
		'wasm32-unknown-wasi-cabal repl hello-hs -finteractive \
		--repl-options="-ignore-dot-ghci -fghci-browser -fghci-browser-port=8080"'

build:
	wasm32-unknown-wasi-cabal build
	rm -rf dist
	cp -r static dist
	$(eval my_wasm=$(shell wasm32-unknown-wasi-cabal list-bin hello-hs))
	$(shell wasm32-unknown-wasi-ghc --print-libdir)/post-link.mjs --input $(my_wasm) --output dist/ghc_wasm_jsffi.js
	cp -v $(my_wasm) dist/app.wasm

optim: build
	wasm-opt -all -O2 dist/app.wasm -o dist/app.wasm
	wasm-tools strip -o dist/app.wasm dist/app.wasm

serve: build
	simple-http-server dist --index --nocache --open
