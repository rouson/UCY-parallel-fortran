# Build the test-my-message executable file
add_executable(
  test_my_message
  test-my-message.f90
  my-message-interface.f90
  my-message-implementation.f90
)

# Copy the test input file from source tree to build tree
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/greeting.txt ${CMAKE_CURRENT_BINARY_DIR}/greeting.txt COPYONLY)

# Copy the test execution script from source tree to build tree in order to facilitate redirecting
# the standard input stream to the greeting.txt file when ctest runs the code that the script launches.
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/run-my-message-test.sh ${CMAKE_CURRENT_BINARY_DIR}/run-my-message-test.sh COPYONLY)
