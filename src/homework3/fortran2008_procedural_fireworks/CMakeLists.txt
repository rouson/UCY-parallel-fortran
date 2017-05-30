include_directories(${CMAKE_BINARY_DIR}/mod)

set(homework2_solution ${CMAKE_SOURCE_DIR}/src/solutions/homework2/procedural_fireworks)

add_executable( fortran2008-procedural-fireworks 
  main.f90
  ${homework2_solution}/sky_procedures_implementation.f90
  ${homework2_solution}/sky_procedures_interface.f90
  ${homework2_solution}/stars_procedures_implementation.f90
  ${homework2_solution}/stars_procedures_interface.f90
)

target_link_libraries(fortran2008-procedural-fireworks sourcery)
install(TARGETS fortran2008-procedural-fireworks DESTINATION bin)

# copy input file to the build tree
configure_file( fireworks_input.txt fireworks_input.txt COPYONLY)
