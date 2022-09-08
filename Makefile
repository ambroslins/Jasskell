.PHONY: all
all: server client

run: all
	cabal run jasskell-server:exe:jasskell-server

core:
	cabal build core

server: core
	cabal build server

client: elm tailwind

elm:
	cd client && elm make ./src/Main.elm --output ./static/main.js

tailwind:
	cd client && npx tailwindcss -i ./style.css -o ./static/style.css