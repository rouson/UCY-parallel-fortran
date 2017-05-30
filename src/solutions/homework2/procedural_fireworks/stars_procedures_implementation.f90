! stars_procedures_implementation.f90

! -- Define a procedural interface for modeling fireworks stars motion

! Copyright (c) 2015-2016, Sourcery Institute
! Copyright (c) 2015-2016, Sourcery, Inc.
! All rights reserved.
!
! Redistribution and use in source and binary forms, with or without
! modification, are permitted provided that the following conditions are
! met:
!
! 1. Redistributions of source code must retain the above copyright
! notice, this list of conditions and the following disclaimer.
!
! 2. Redistributions in binary form must reproduce the above copyright
! notice, this list of conditions and the following disclaimer in the
! documentation and/or other materials provided with the distribution.
!
! 3. Neither the name of the copyright holder nor the names of its
! contributors may be used to endorse or promote products derived from
! this software without specific prior written permission.
!
! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
! "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
! LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
! A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
! HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
! SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
! LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
! DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
! THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
! (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
! OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
!
submodule(stars_procedures_interface) stars_procedures_implementation

  implicit none

contains

  module function random_spherical_distribution(num_stars) result(positions)
    use math_constants ,only : pi,two
    use physical_constants ,only : space_dimension
    use random_module, only : init_random_seed
    logical, save :: first_call=.true.
    ! Input variables
    integer(ikind) ,intent(in) :: num_stars
    ! Local constants
    real(rkind) ,parameter :: r=1._rkind ! radius
    integer(ikind) ,parameter :: num_angles=2 ! = size([theta, phi])
    ! Local variables: random numbers
    real(rkind) :: harvest(num_angles)             
    integer(ikind) :: star
    ! Result
    real(rkind) :: positions(num_stars,space_dimension)

    if (first_call) then
      first_call=.false.
      ! Ensure the generation of a unique psuedo-random number sequence on each image
      call init_random_seed 
    end if
    do star=1,size(positions,1)
      call random_number(harvest)
      associate(theta=>pi*harvest(1),phi=>two*pi*harvest(2))
        positions(star,1) = r*sin(theta)*cos(phi)
        positions(star,2) = r*sin(theta)*sin(phi)
        positions(star,3) = r*cos(theta)
      end associate
    end do 

  end function

  module function reached_terminal_velocity(position,velocity,response_time,tolerance) result(is_terminal)
    use debugging_parameters ,only : verbose
    use physical_constants ,only : g_vector
    use sky_procedures_interface ,only : local_wind_velocity
    ! Input variables
    real(rkind) ,intent(in)  :: position(:,:),velocity(:,:),tolerance,response_time(:)
    ! Local variables
    real(rkind) ,allocatable ,dimension(:,:) :: terminal_velocity,velocity_error,relative_velocity,wind_velocity
    logical :: is_terminal
    integer(ikind) :: i,j

    wind_velocity = local_wind_velocity(position)
    relative_velocity = velocity - wind_velocity
    allocate(terminal_velocity(size(relative_velocity,1),size(relative_velocity,2)))
    do concurrent (i=1:size(relative_velocity,1)) 
      terminal_velocity(i,:) = response_time(i)*g_vector(:)
    end do
    velocity_error = relative_velocity - terminal_velocity
    is_terminal=.true.
    if (verbose)  print '(a,4f10.3)','v_error,tolerance=',velocity_error(1,:),tolerance
    do i=1,size(terminal_velocity,1)
      do j=1,size(terminal_velocity,2)
        ! For a negligible terminal velocity (e.g., zero in horizontal direction), check for small relative velocity.
        if (abs(terminal_velocity(i,j))<tolerance) then
          if (abs(relative_velocity(i,j))>tolerance  ) is_terminal=.false.
        ! For a finite terminal velocity (e.g., vertical direction), check for small percentage difference from terminal velocity.
        else if (abs(velocity_error(i,j)/terminal_velocity(i,j))>tolerance) then
          if (verbose)  print *,'abs(v_error/terminal_velocity)=',abs(velocity_error(i,j)/terminal_velocity(i,j))
          is_terminal=.false.
        end if
      end do
    end do
  end function

end submodule
