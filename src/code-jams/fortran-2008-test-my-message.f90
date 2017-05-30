program main
  !! Unit test for my_message
  implicit none
  character(len=128) :: input[*]

  associate(me=>this_image())
    !! Define a local alias for the image number

    broadcast_preface: block 
      !! We can roll our own collective subroutines if we are using a compiler that supports
      !! Fortran 2008 coarray features but not Fortran 2015 collective subroutines
      use iso_fortran_env, only : input_unit,output_unit
      integer, parameter :: preconnected=1
      logical, parameter :: I_like_bottlenecks=.false.

      if (preconnected==me) then
        !! Read text if I'm the image preconnected to input_unit
         write(output_unit,advance='no',fmt='(a)') "Enter preface: "
         read(input_unit,*) input
      end if
     
      if (I_like_bottlenecks) then
        call co_broadcast_bottleneck(input,source_image=preconnected)
      else
        call co_broadcast_binary(input,source_image=preconnected)
          !! Broadcast the received text to all images
      end if

    end block broadcast_preface


    test_image_number: block
      !! Verify image number location in my_message result
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

  subroutine co_broadcast_bottleneck(A,source_image)
     character(len=128) :: A[*]
     integer, intent(in) :: source_image
     A = A[source_image] 
  end subroutine

  subroutine co_broadcast_binary(A,source_image)
     character(len=128) :: A[*]
     integer, intent(in) :: source_image
     associate(me=>this_image(),numimages=>num_images())
       associate(parent=>me/2)
         if (parent>0) then
           sync images(parent)
           A = A[parent]
         else
           A = A[source_image] 
         end if
       end associate
       associate(even_child=>2*me,odd_child=>2*me+1)
         if (even_child<=numimages) sync images(even_child)
         if (odd_child<=numimages) sync images(odd_child)
       end associate
     end associate
  end subroutine

  function my_message(preface) result(message)
    !! author: Damian Rouson
    !! Satisfy test_image_number unit test & use assertions to enforce contract
    !! assurances an requirements
    character(len=*), intent(in) :: preface 
    character(len=:), allocatable :: message
    integer, parameter :: max_digits=3
    character(len=max_digits) :: my_image 

    ! Requires
    call assert(num_images()<1000,"acceptable number of digits in image number")

    write(my_image,fmt="(i3)") this_image()
    message = preface // my_image

    ! Ensures
    call assert(len(message)>len(preface),"characters appended to preface")

  end function

  subroutine assert(assertion,description)
    !! If assertion fails, error terminate & print description
    logical, intent(in) :: assertion
    character(len=*), intent(in) :: description
    if (.not. assertion) &
      error stop "assertion '" // description // "' failed "
  end subroutine

end program
