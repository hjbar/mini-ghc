DIFF=diff --ignore-all-space

SRC=$(wildcard *.f)
SRCJ=$(wildcard *.fj)
SRCJREC=$(wildcard *.fjrec)

BAD=$(wildcard bad/*.f)
BADJ=$(wildcard bad/*.fj)
BADJREC=$(wildcard bad/*.fjrec)

ECHO=$(SRC:.f=.echo)

TYPECHECK=$(SRC:.f=.typecheck)
TYPECHECKJ=$(SRCJ:.fj=.typecheckj)
TYPECHECKJREC=$(SRCJREC:.fjrec=.typecheckjrec)

TYPEFAIL=$(BAD:.f=.typefail)
TYPEFAILJ=$(BADJ:.fj=.typefailj)
TYPEFAILJREC=$(BADJREC:.fjrec=.typefailjrec)

OPTIMIZE2=$(SRC:.f=.opt2)
OPTIMIZE3=$(SRCJ:.fj=.opt3) $(SRC:.f=.opt3)

ERASE=$(SRC:.f=.erase)
ERASEJ=$(SRCJ:.fj=.erasej)

JOUJOU ?= ../src/joujou.exe

################################################################
test: test1 test2 test3

################################################################
# For debugging purposes
# eg., `ARGS=--optimize make caseofcase.echo`

%.echo: %.f force
	$(JOUJOU) --echo $(ARGS) $<

echo: $(ECHO)

################################################################
# Test Task1

%.typecheck: %.f force
	$(JOUJOU) $< > /dev/null

%.typefail: %.f force
	! $(JOUJOU) $< &> /dev/null

test1-success: $(TYPECHECK)

test1-fail: $(TYPEFAIL)

test1: $(TYPECHECK) $(TYPEFAIL)

################################################################
# Test Task2

%.opt2: %.f force
	$(JOUJOU) --optimize $< > $*.opt2
	@$(DIFF) $*.opt2 $*.spec2; true

test2: $(OPTIMIZE2)

################################################################
# Test Task3

%.typecheckj: %.fj force
	$(JOUJOU) $< > /dev/null

%.typefailj: %.fj force
	! $(JOUJOU) $< &> /dev/null

%.opt3: %.fj force
	$(JOUJOU) --optimize --case-of-case $< > $*.opt3
	@$(DIFF) $*.opt3 $*.spec3; true

%.opt3: %.f force
	$(JOUJOU) --optimize --case-of-case $< > $*.opt3
	@$(DIFF) $*.opt3 $*.spec3; true

test3a-success: $(TYPECHECKJ) $(TYPECHECK)

test3a-fail: $(TYPEFAILJ)

test3a: $(TYPECHECKJ) $(TYPEFAILJ) $(TYPECHECK) $(TYPECHECK)

test3b: $(OPTIMIZE3)

test3: test3a test3b

################################################################
# Typecheck fjrec

%.typecheckjrec: %.fjrec force
	$(JOUJOU) $< > /dev/null

%.typefailjrec: %.fjrec force
	! $(JOUJOU) $< &> /dev/null

testrec-success: $(TYPECHECKJREC) $(TYPECHECKJ) $(TYPECHECK)

testrec-fail : $(TYPEFAILJREC) $(TYPEFAILJ) $(TYPEFAIL)

testrec: testrec-success testrec-fail

################################################################
# Test Erase

%.erase: %.f force
	$(JOUJOU) --optimize --case-of-case --erase $< > $*.eopt
	@$(DIFF) $*.eopt $*.espec; true

%.erasej: %.fj force
	$(JOUJOU) --optimize --case-of-case --erase $< > $*.eopt
	@$(DIFF) $*.eopt $*.espec; true

test-erase: $(ERASE)

test-erasej: $(ERASEJ)

erase: test-erase test-erasej

################################################################
# Global tests

typecheck-success: test1-success test3a-success testrec-success

typecheck-fail: test1-fail test3a-fail testrec-fail

typecheck: test1 test3a testrec

optimize: test2 test3b

################################################################
# Misc.

force:
	@true

clean:
	rm -f *.opt2 *.opt3 *.eopt
