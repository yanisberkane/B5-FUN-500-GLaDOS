##
## EPITECH PROJECT, 2023
## B-FUN-500-PAR-5-2-glados-yanis.berkane
## File description:
## Makefile
##

all: build

setup:
	stack setup

build: clean
	stack build --copy-bins --local-bin-path .

test: clean
	stack build --copy-bins --local-bin-path .
	stack test --coverage
	./functional_test.sh

clean:
	stack clean

fclean: clean
	rm -rf .stack-work
	rm -f glados
	rm -f glados-vm
	rm -rf *.dz

re: fclean build

.PHONY: all setup re build test clean fclean