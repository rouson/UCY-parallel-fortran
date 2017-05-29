submodule(my_message_interface) my_message_implementation
  implicit none
contains
  module procedure my_message
    character(len=3) :: i,n
    write(i,"(i3)") this_image() !! Convert integer to string
    write(n,"(i3)") num_images() !! Convert integer to string
    message = preface // i // " of " // trim(adjustl(n)) // "."
      !! Concatenate preface and remaining strings
  end procedure
end submodule
