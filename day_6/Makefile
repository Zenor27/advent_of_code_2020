SRC = main.hs

BIN = main

OBJ = $(SRC:.hs=.o)
HI = $(SRC:.hs=.hi)

all:
		ghc $(SRC) -o $(BIN)

clean:
		rm -rf $(BIN) $(OBJ) $(HI)
