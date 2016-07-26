
HOSTNAME=$(shell hostname -s)

REP1=report_$(HOSTNAME)_lts-5.16_constant
REP2=report_$(HOSTNAME)_sc0.4-opt_constant

REP3=report_$(HOSTNAME)_lts-5.16_increasing
REP4=report_$(HOSTNAME)_sc0.4-opt_increasing

IMG1= --docker --docker-image=fpco/stack-build:lts-5.16   bench
IMG2= --docker --docker-image=parfunc/sc-haskell:v0.4-opt bench

all: $(REP1).html $(REP2).html $(REP3).html $(REP4).html

$(REP1).html:
	stack $(IMG1) --ghc-options="-DCONSTANT" --benchmark-arguments="-o $(REP1).html --csv $(REP1).csv"

$(REP2).html:
	stack $(IMG2) --ghc-options="-DCONSTANT" --benchmark-arguments="-o $(REP2).html --csv $(REP2).csv"

$(REP3).html:
	stack $(IMG1) --benchmark-arguments="-o $(REP3).html --csv $(REP3).csv"

$(REP4).html:
	stack $(IMG2) --benchmark-arguments="-o $(REP4).html --csv $(REP4).csv"
