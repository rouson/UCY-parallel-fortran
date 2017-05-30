include_directories(${CMAKE_BINARY_DIR}/mod)

add_executable( procedural-fireworks 
  main.f90
  sky_procedures_implementation.f90
  sky_procedures_interface.f90
  stars_procedures_implementation.f90
  stars_procedures_interface.f90
)

target_link_libraries(procedural-fireworks sourcery)
install(TARGETS procedural-fireworks DESTINATION bin)

# copy input file to the build tree
configure_file( fireworks_input.txt fireworks_input.txt COPYONLY)
