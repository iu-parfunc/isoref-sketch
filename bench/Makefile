
HOSTNAME=$(shell hostname -s)

REP1=report_$(HOSTNAME)_lts-5.16_constant
REP2=report_$(HOSTNAME)_sc0.4-opt_constant

REP3=report_$(HOSTNAME)_lts-5.16_increasing
REP4=report_$(HOSTNAME)_sc0.4-opt_increasing

IMG1= --docker --docker-image=fpco/stack-build:lts-5.16   bench
IMG2= --docker --docker-image=parfunc/sc-haskell:v0.4-opt bench

all: $(REP1) $(REP2)

$(REP1):
	stack $(IMG1) --ghc-options="-DCONSTANT" --benchmark-arguments="-o $@.html --csv $@.csv"

$(REP2):
	stack $(IMG2) --ghc-options="-DCONSTANT" --benchmark-arguments="-o $@.html --csv $@.csv"

$(REP3):
	stack $(IMG1) --benchmark-arguments="-o $@.html --csv $@.csv"

$(REP4):
	stack $(IMG2) --benchmark-arguments="-o $@.html --csv $@.csv"
