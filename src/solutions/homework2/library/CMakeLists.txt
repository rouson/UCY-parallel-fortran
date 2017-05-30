set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/mod)

add_library( sourcery
 debugging_parameters.f90
 kind_parameters.f90
 math_constants.f90
 physical_constants.f90
 random.f90
)
install(TARGETS sourcery DESTINATION lib)
