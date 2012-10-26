# Mac OS X Defaults
OPEN=open

# Windows
ifeq (${MSYSTEM},MINGW32)
	OPEN=
endif

all: bloodline.hs bloodline.tex
	runhaskell bloodline.hs

clean:
	-rm *.out
	-rm *.log
	-rm *.aux
	-rm *-intermediate.pdf
	-rm *.dot