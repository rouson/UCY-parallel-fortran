# target: prerequisites - the rule head
target = libsourcery.a

# Fortran compiler
compile = caf

# Top-level directory containing homework files
homework_root=../../..
install_dir= $(homework_root)/lib
mod_dir = $(homework_root)/mod

# Linker
link = $(compile)

args = -O3 -Wall -fcheck=all

# archiver
archive = ar

# $@ - means the target
# $^ - means all prerequisites
$(target): kind_parameters.o physical_constants.o math_constants.o random.o debugging_parameters.o
	$(archive) rcs $@ $^

install: $(target)
	mkdir -p $(install_dir)
	mkdir -p $(mod_dir)
	cp $(target) $(install_dir)
	cp *.mod $(mod_dir)

#$< - means just the first prerequisite
math_constants.mod: math_constants.f90 kind_parameters.mod
	$(compile) $(args) -c -o $@ $<

math_constants.o: math_constants.f90 kind_parameters.mod
	$(compile) $(args) -c -o $@ $<

physical_constants.mod: physical_constants.f90 kind_parameters.mod
	$(compile) $(args) -c -o $@ $<

physical_constants.o: physical_constants.f90 kind_parameters.mod
	$(compile) $(args) -c -o $@ $<

kind_parameters.mod: kind_parameters.f90
	$(compile) $(args) -c -o $@ $<

kind_parameters.o: kind_parameters.f90
	$(compile) $(args) -c -o $@ $<

debugging_parameters.mod: debugging_parameters.f90
	$(compile) $(args) -c -o $@ $<

debugging_parameters.o: debugging_parameters.f90
	$(compile) $(args) -c -o $@ $<

random.o: random.f90
	$(compile) $(args) -c -o $@ $<

.PHONY : clean uninstall

clean:
	rm -f *.o *.mod *.smod $(target)

uninstall:
	rm -f $(install_dir)/$(target) 
	rm -f $(mod_dir)/kind_parameters.mod $(mod_dir)/math_constants.mod $(mod_dir)/physical_constants.mod 
	rm -f $(mod_dir)/debugging_parameters.mod $(mod_dir)/random_module.mod
