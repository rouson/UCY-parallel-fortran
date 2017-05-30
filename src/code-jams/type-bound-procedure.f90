module vector_type
  implicit none

  type vector
    real, allocatable :: x(:)
  contains
    procedure :: magnitude
  end type

contains

  function magnitude(v) result(length_v)
    class(vector) :: v
    real :: length_v
    length_v = norm2(v%x)
  end function

end module

module velocity_type
  use vector_type, only : vector
  implicit none

  type, extends(vector) :: velocity
  contains
    procedure :: magnitude
  end type

contains

  function magnitude(v) result(length_v)
    class(velocity) :: v
    real :: length_v
    length_v = norm2(v%x)
  end function

end module 

program main
  implicit none
  type(velocity) :: u
  real :: flow_speed
  flow_speed = u%magnitude()
end program

