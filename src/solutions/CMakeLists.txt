#Specify all the subdirectories containing codes that produces executable files or libraries:
set(subdirectories homework1 homework2 homework3)
foreach(subdirectory ${subdirectories})
  add_subdirectory(${subdirectory})
endforeach()
