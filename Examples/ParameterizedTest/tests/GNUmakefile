.PHONY: clean tests

include $(PFUNIT)/include/base.mk

SRCS = $(wildcard *.pf)
OBJS = $(SRCS:.pf=$(OBJ_EXT))

tests: $(OBJS)

testSuites.inc: $(SRCS)

FFLAGS += -I$(SRC_DIR) -I$(PFUNIT)/mod

%.F90: %.pf
	$(PFUNIT)/bin/pFUnitParser.py $<  $@

%$(OBJ_EXT): %.F90
	$(FC) -c $(FFLAGS) $<

clean: local-E1-clean

local-E1-clean:
	$(RM) *$(OBJ_EXT) *.mod
