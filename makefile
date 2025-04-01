# FLP 1.projekt 
# Makefile
# Author: Lucia Mária Šmotlákova (xsmotl00)

GHC      = ghc
GHCFLAGS = -Wall
TARGET   = flp-fun
SRC      = flp-fun.hs

.PHONY: all clean

all: $(TARGET)

$(TARGET): $(SRC)
	$(GHC) $(GHCFLAGS) -o $(TARGET) $(SRC)

clean:
	rm -f $(TARGET) *.o *.hi