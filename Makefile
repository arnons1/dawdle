.DEFAULT_GOAL := all
.PHONY : all

all : build/dawdle

build/dawdle : src/Database/Dawdle/*.lhs
	-mkdir -p $(@D)
	@echo HC-MAKE $@
	$(HC) $(HC_MAKE_OPTS) bin/main.lhs -o $@ -outputdir $(@D) -isrc

BUILD = build

HC_MAKE_OPTS = --make -funfolding-use-threshold=16 -O3 -optc-O3 -fasm -split-objs -optc "-pipe" -v0 -Wall -rtsopts -threaded -stubdir $(BUILD) -fno-spec-constr -fno-warn-overlapping-patterns
HC = ghc
HL = $(HC)
