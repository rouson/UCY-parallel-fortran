program main
  
  implicit none
  integer, parameter :: max_length=64
  character(len=max_length) :: input

  associate(me=>this_image())

    broadcast_preface: block
      use iso_fortran_env, only : input_unit,output_unit
      integer, parameter :: preconnected=1
      write(output_unit,advance='no',fmt="(a)") "Enter preface: "
      read (input_unit,fmt="(a)") input
      call co_broadcast(input,source_image=preconnected)
    end block broadcast_preface
  
    test_my_message: block
      integer, parameter :: image_digits=3
      character(len=:), allocatable :: pretext,greeting
      integer :: image
      pretext = input // " from image "
      greeting = my_message(pretext) 
      read(greeting,"(a,i3)") pretext, image
      call assert(image==me,description="read image number")
    end block test_my_message
  
    sync all
  
    report_result: block
      integer, parameter :: reporter=1
      if (reporter==me) print *,"Test passed."
    end block report_result
 
  end associate

contains
  subroutine assert(assertion,description) 
    logical, intent(in) :: assertion
    character(len=*) :: description
    if ( .not. assertion ) error stop "Assertion "//description//"failed"
  end subroutine
end program
