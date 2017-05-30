cmake_minimum_required(VERSION 3.7)

set(ucy2017_version "2.0")
project(UCY2017 VERSION "${ucy2017_version}" LANGUAGES Fortran)
message( STATUS "Building UCY2017 version ${ucy2017_version}" )

#Print an error message on an attempt to build inside the source directory tree:
if ("${CMAKE_CURRENT_SOURCE_DIR}" STREQUAL "${CMAKE_CURRENT_BINARY_DIR}")
  message(FATAL_ERROR "ERROR! "
    "CMAKE_CURRENT_SOURCE_DIR=${CMAKE_CURRENT_SOURCE_DIR}"
    " == CMAKE_CURRENT_BINARY_DIR=${CMAKE_CURRENT_BINARY_DIR}"
    "\nThis archive does not support in-source builds:\n"
    "Please delete the CMakeCache.txt file and the CMakeFiles/ directory under the 'src' directory"
    "\n and execute something like the following::\n"
    "  $ rm -r CMakeCache.txt CMakeFiles/"
    "\n"
    "Please create a directory at a location other than the top level of this source archive and build there"
    "in a manner such as\n"
    "  $ mkdir build\n"
    "  $ cd build\n"
    "  $ FC=caf cmake <path-to-opencoarrays-source-directory> -DCMAKE_INSTALL_PREFIX=<path-to-install-directory>\n"
    "\nsubstituting the appropriate syntax for your shell (the above line assumes the bash shell)."
    )
endif()

#Report untested Fortran compiler unless explicitly directed to build all examples.
if ("${CMAKE_Fortran_COMPILER_ID}" MATCHES "GNU" )
  set(gfortran_compiler true)
else()
  message(WARNING
    "\n"
    "Attempting to build with untested Fortran compiler: ${CMAKE_Fortran_COMPILER_ID}. "
    "Please report any failures to opencoarrays@googlegroups.com\n\n"
  )
endif()

if(gfortran_compiler)
  set(OLD_REQUIRED_FLAGS ${CMAKE_REQUIRED_FLAGS})
  set(CMAKE_REQUIRED_FLAGS "-fcoarray=single -ffree-form")
endif()
include(CheckFortranSourceCompiles)
CHECK_Fortran_SOURCE_COMPILES("
  program main
    implicit none
    integer :: i
    i = this_image()
  end program
" Check_Simple_Coarray_Fortran_Source_Compiles)
if(gfortran_compiler)
  set (CMAKE_REQUIRED_FLAGS ${OLD_REQUIRED_FLAGS})
  unset(OLD_REQUIRED_FLAGS)
endif()


add_subdirectory(src)

enable_testing()

# We invoke these tests via bash scripts that redirect standard input to a file and that echo the program output 
# to the script's standard output stream.
add_test(my_message_test ${CMAKE_CURRENT_SOURCE_DIR}/src/homework1-refactored/run-my-message-test.sh)
set_property(TEST my_message_test PROPERTY PASS_REGULAR_EXPRESSION "Test passed.")

add_test(homework1_solution ${CMAKE_CURRENT_SOURCE_DIR}/src/solutions/homework1/run-homework1-solution.sh)
set_property(TEST homework1_solution PROPERTY PASS_REGULAR_EXPRESSION "Test passed.")

# All other tess will be defined via add_mpi_test below from OpenCoarrays (https://github.com/sourceryinstitute/opencoarrays)
include( ProcessorCount )
ProcessorCount(N)
function(add_mpi_test name num_mpi_proc path)
   if ( ((N LESS num_mpi_proc) OR (N EQUAL 0)) )
     message(STATUS "Test ${name} is oversubscribed: ${num_mpi_proc} ranks requested with ${N} system processor available.")
     if ( openmpi )
       if ( N LESS 2 )
         set( num_mpi_proc 2 )
         set (test_parameters --oversubscribe)
       else()
         set ( num_mpi_proc ${N} )
       endif()
       message( STATUS "Open-MPI detected, over-riding oversubscribed test, ${name}, with ${num_mpi_proc} ranks." )
     endif()
   endif()
   set(test_parameters ${test_parameters} ${MPIEXEC_NUMPROC_FLAG} ${num_mpi_proc} )
   add_test(NAME ${name} COMMAND ${MPIEXEC} ${test_parameters} "${path}")
   set_property(TEST ${name} PROPERTY PASS_REGULAR_EXPRESSION "Test passed.")
endfunction(add_mpi_test)

set(tests_root ${CMAKE_CURRENT_BINARY_DIR}/src/tests)

add_mpi_test(homework2_solution procedural-fireworks 4 src/solutions/homework2/procedural_fireworks)
