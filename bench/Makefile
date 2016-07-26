
HOSTNAME=$(shell hostname -s)

REP1=report_$(HOSTNAME)_lts-5.16_constant.html
REP2=report_$(HOSTNAME)_sc0.4-opt_constant.html

all: $(REP1) $(REP2)

$(REP1):
	stack --docker --docker-image=fpco/stack-build:lts-5.16 bench  --benchmark-arguments="-o $@"

$(REP2):
	stack --docker --docker-image=parfunc/sc-haskell:v0.4-opt bench  --benchmark-arguments="-o $@"
