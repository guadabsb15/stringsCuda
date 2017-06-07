
FCOMP := pgf90
FFLAGS := -Mcuda=cuda8.0


F90sources += sendString.F90

ALL: main

main: 
	$(FCOMP) -o thing $(FFLAGS) $(F90sources) 

clean:
	rm -f *.mod


.PHONY: clean
