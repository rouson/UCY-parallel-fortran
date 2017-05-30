include_directories(${CMAKE_BINARY_DIR}/mod)

set(homework2_solution ${CMAKE_SOURCE_DIR}/src/solutions/homework2/procedural_fireworks)

add_executable( homework3-solution
  main.f90
  ${homework2_solution}/sky_procedures_implementation.f90
  ${homework2_solution}/sky_procedures_interface.f90
  ${homework2_solution}/stars_procedures_implementation.f90
  ${homework2_solution}/stars_procedures_interface.f90
)

target_link_libraries(homework3-solution sourcery)
install(TARGETS homework3-solution DESTINATION bin)

# copy input file to the build tree
configure_file( fireworks_input.txt fireworks_input.txt COPYONLY)
