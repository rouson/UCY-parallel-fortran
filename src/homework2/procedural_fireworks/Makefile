# Choose desired platform (options: virtual, cees):
machine=virtual
# Choose desired compiler (options: sourcery, gnu, intel, cray, tau_intel):
compiler=sourcery

executable=procedural_fireworks

homework_root=../../..
lib_dir=$(homework_root)/lib
mod_dir=$(homework_root)/mod
install_dir=$(homework_root)/bin

ifeq ($(compiler),sourcery)
  compile=caf -O3 -Wall -fcheck=all
else
  ifeq ($(compiler),gnu)
    lib=-lcaf_mpi
    compile=mpif90 -fcoarray=lib
    ifeq ($(machine),virtual)
      opt=-L /opt/opencoarrays/lib
    else
      ifeq ($(machine),cees)
      opt=-L /opt/opencoarrays-src/mpi
      endif
    endif
  else
    ifeq ($(compiler),intel)
      compile=ifort -coarray=shared -standard-semantics -O3 -coarray-num-images=2
    else
      ifeq ($(compiler),cray)
        compile=ftn -ew -h caf
      else
        ifeq ($(compiler),tau_intel)
          compile=tauf90 -tau:serial,icpc,pdt  -coarray=shared -standard-semantics -O3 -coarray-num-images=4 -DTAU=1
        else
          ifeq ($(compiler),tau_aciss)
            tau_aciss=tau_f90.sh -v -Bdynamic -coarray=shared -standard-semantics -O3
          else
            ifeq ($(compiler),tau_cray)
              tau_hopper=tau_f90.sh -optCompInst -ew -h caf -g
            endif
          endif
        endif
      endif
    endif
  endif
endif

objects=sky_procedures_interface.o sky_procedures_implementation.o stars_procedures_interface.o stars_procedures_implementation.o

$(executable): main.o $(objects) Makefile
	$(compile) $(opt) main.o $(objects) -o $(executable) -L $(lib_dir) -I $(mod_dir) -lsourcery $(lib)

install: $(executable)
	mkdir -p $(install_dir)
	cp $(executable) $(install_dir)

main.o: main.f90 sky_procedures_interface.mod stars_procedures_interface.mod Makefile 
	$(compile) -c main.f90 -I $(mod_dir)

# Produce .o, .mod, and .smod files from sky interface
sky_procedures_interface.mod: sky_procedures_interface.f90 Makefile
	$(compile) -c sky_procedures_interface.f90 -I $(mod_dir)

sky_procedures_interface.smod: sky_procedures_interface.f90 Makefile
	$(compile) -c sky_procedures_interface.f90 -I $(mod_dir)

# Produce .o, .mod, and .smod files from sky interface
stars_procedures_interface.mod: stars_procedures_interface.f90 Makefile
	$(compile) -c stars_procedures_interface.f90 -I $(mod_dir)

stars_procedures_interface.smod: stars_procedures_interface.f90 Makefile
	$(compile) -c stars_procedures_interface.f90 -I $(mod_dir)

# Produce .o and .smod files from sky implemenation
sky_procedures_implementation.o: sky_procedures_implementation.f90 sky_procedures_interface.smod Makefile
	$(compile) -c sky_procedures_implementation.f90 -I $(mod_dir)

sky_procedures_implementation.mod: sky_procedures_implementation.f90 sky_procedures_interface.smod Makefile
	$(compile) -c sky_procedures_implementation.f90 -I $(mod_dir)

# Produce .o, .mod, and .smod files from sky interface
stars_procedures_implementation.mod: stars_procedures_implementation.f90 stars_procedures_interface.smod sky_procedures_interface.mod Makefile
	$(compile) -c stars_procedures_implementation.f90 -I $(mod_dir)

stars_procedures_implementation.smod: stars_procedures_implementation.f90 stars_procedures_interface.smod sky_procedures_interface.mod Makefile
	$(compile) -c stars_procedures_implementation.f90 -I $(mod_dir)

stars_procedures_implementation.o: stars_procedures_implementation.f90 stars_procedures_interface.smod sky_procedures_interface.mod Makefile
	$(compile) -c stars_procedures_implementation.f90 -I $(mod_dir)


.PHONY : clean uninstall

clean :
	-rm -f  *.o *.mod *.smod $(executable)


uninstall: 
	rm -f $(install_dir)/$(executable)
