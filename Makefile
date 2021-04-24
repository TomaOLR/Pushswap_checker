##
## EPITECH PROJECT, 2021
## Makefile
## File description:
## PushSwap Checker
##

SRC	=	pushswap_checker.hs

OBJ	= 	pushswap_checker.hi \
		pushswap_checker.o

NAME	=	pushswap_checker

all:	$(NAME)

$(NAME):
	ghc -o $(NAME) $(SRC)

clean:
	rm -rf $(OBJ)

fclean: clean
	rm -rf $(NAME)

re:	fclean all

.PHONY: all clean fclean re
