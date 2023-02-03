# Makefile for xmobar

CC ?= gcc
CFLAGS ?= -O2 -march=native -pipe -std=c99 -Wall

PACKAGES = libpulse
CFLAGS += $(shell pkg-config --cflags $(PACKAGES))
LDFLAGS += $(shell pkg-config --libs $(PACKAGES))

TARGET = xmobar
HASKELL_SOURCES = xmobar.hs $(shell find lib -type f -name '*.hs')
HASKELL_INTERFACES = $(HASKELL_SOURCES:.hs=.hi)
HASKELL_OBJECTS = $(HASKELL_SOURCES:.hs=.o)
C_SOURCES = $(shell find cbits -type f -name '*.c')
C_OBJECTS = $(C_SOURCES:.c=.o)

all: $(TARGET)

$(TARGET): $(HASKELL_SOURCES) $(C_OBJECTS)
	ghc --make -i -ilib -threaded -l pulse -o $@ $< $(C_OBJECTS)

%.o: %.c
	$(CC) -c $(CFLAGS) $< -o $@

strip: $(TARGET)
	strip $<

clean:
	rm -f $(HASKELL_INTERFACES) $(HASKELL_OBJECTS) $(C_OBJECTS) $(TARGET)

.PHONY: all strip clean
