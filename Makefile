test: build

build:
	erlc -o ebin src/*.erl src/client/*.erl src/common/*.erl src/mutex/*.erl tests/*.erl
