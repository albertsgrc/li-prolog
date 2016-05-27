all: main

main: main.pl A-caballo.pl A-haceraguas.pl B-puzzle.pl C-dieta.pl D-mainroads.pl
	swipl -O -q -g main --stand_alone=true -o main -c main.pl

clean:
	rm -f main
