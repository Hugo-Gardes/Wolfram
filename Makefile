##
## EPITECH PROJECT, 2022
## B-CPE-210-MPL-2-1-solostumper05-hugo.gardes
## File description:
## Makefile
##

all:
	rm -f wolfram
	stack build
	cp .stack-work/install/x86_64-linux-tinfo6/*/8.10.7/bin/wolfram-exe .
	mv wolfram-exe wolfram

clean:
	stack clean

fclean:
	rm -f wolfram
	stack purge
	rm -f wolfram-exe

re:	fclean all