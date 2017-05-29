module my_message_interface
  !! Export a function for forming messages
  implicit none
  interface
    module function my_message(preface) result(message)
      !! Produce a string with a prepended preface
      implicit none
      character(len=*), intent(in) :: preface
      character(len=:), allocatable :: message
    end function
  end interface
end module
