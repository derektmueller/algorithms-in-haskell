
all :
	stack build

run :
	stack exec algorithms-in-haskell-exe

watch :
	while true; \
	do \
		change=$$(inotifywait -e close_write,moved_to,create app src) \
		make all && make run; \
	done;

