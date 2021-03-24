all:
	elm make src/Main.elm --output build/elm.js
	elm make src/Main.elm
	cp index.html build/index.html
	cp -r src/img build