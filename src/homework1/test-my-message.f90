program main
  !! Unit test for my_message
  implicit none
  character(len=128) :: input

  associate(me=>this_image())
    !! Define a local alias for the image number

    broadcast_preface: block 
      use iso_fortran_env, only : input_unit,output_unit
      integer, parameter :: preconnected=1

      if (preconnected==me) then
        !! Read text if I'm the image preconnected to input_unit
         write(output_unit,advance='no',fmt='(a)') "Enter preface: "
         read(input_unit,*) input
      end if

      call co_broadcast(input,source_image=preconnected)
        !! Broadcast the received text to all images

    end block broadcast_preface


    test_image_number: block
      !! Verify image number location in my_message result
      use my_message_interface, only : my_message
      integer :: image
      character(len=:), allocatable :: constructed_message,pretext

      pretext = trim(input) // " from image "
      constructed_message = my_message(pretext)
      read(constructed_message,"(a,i3)") pretext,image
      call assert(image==me,"recover image number from message")

    end block test_image_number

    sync all
      !! Wait for all images to survive the assertion

    report_result: block
      !! Only one image reports a passing test
      integer, parameter :: reporter=1
      if (reporter==me) print *,"Test passed."
    end block report_result

  end associate
contains

  ! Write an implementaiton of my_message() here

  subroutine assert(assertion,description)
    !! If assertion fails, error terminate & print description
    logical, intent(in) :: assertion
    character(len=*), intent(in) :: description
    if (.not. assertion) &
      error stop "assertion '" // description // "' failed "
  end subroutine

end program
