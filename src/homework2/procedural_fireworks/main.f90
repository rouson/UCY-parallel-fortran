! MoFo is distributed under the OSI-approved BSD 3-clause License:
! MoFo -- Modern Fortran programming examples 
! Copyright (c) 2012-2015, Sourcery, Inc.
! Copyright (c) 2015, Sourcery Institute
! All rights reserved.

! Redistribution and use in source and binary forms, with or without modification, 
! are permitted provided that the following conditions are met:
! 
! 1. Redistributions of source code must retain the above copyright notice, this 
!    list of conditions and the following disclaimer.
! 2. Redistributions in binary form must reproduce the above copyright notice, this 
!    list of conditions and the following disclaimer in the documentation and/or 
!    other materials provided with the distribution.
! 3. Neither the names of the copyright holders nor the names of their contributors 
!    may be used to endorse or promote products derived from this software without 
!    specific prior written permission.
! 
! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND 
! ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED 
! WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. 
! IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, 
! INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT 
! NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR 
! PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
! WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) 
! ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE 
! POSSIBILITY OF SUCH DAMAGE.
  
program main
  use debugging_parameters, only : verbose
  use kind_parameters, only : rkind,ikind,ckind
  use math_constants, only : pi 
  use physical_constants, only : space_dimension,g_vector ! Dimension of space and gravitational acceleration
  use stars_procedures_interface, only : random_spherical_distribution,reached_terminal_velocity
  use sky_procedures_interface, only : local_wind_velocity,local_air_viscosity
  implicit none
  ! Dynamically allocated local variables:
  real(rkind) ,dimension(:,:) ,allocatable :: x,v,wind_velocity,relative_velocity ! Position, velocities
  real(rkind) ,dimension(:) ,allocatable :: response_time ! Star aerodynamic response time
  ! Other local variables:
  integer(ikind) :: i ! loop iteration count
  ! Input variables with default initializations:
  integer(ikind) :: num_stars=1_ikind
  real(rkind) :: t=1._rkind,t_final=48._rkind,dt=1._rkind
  real(rkind) :: star_mass=0.001_rkind, star_radius=0.2_rkind
  real(rkind) :: sea_level_temperature=293.15_rkind, reference_wind_speed=10._rkind, reference_wind_height=10._rkind
  ! Constants
  real(rkind) ,parameter :: tolerance=0.05_rkind  ! convergence parameter 
  real(rkind) ,parameter :: burst_elevation=5000._rkind  ! convergence parameter 
  integer(ikind) ,parameter :: lone_star=1_ikind ! default number of stars
  real(rkind) ,dimension(lone_star,space_dimension) ,parameter :: default_star_position = &
    reshape([0._rkind,0._rkind,1._rkind],[lone_star,space_dimension]) 
  ! Input data format:
  namelist /fireworks_properties/ num_stars,star_mass,star_radius
  namelist /timeline/ t,t_final,dt
  namelist /sky_properties/ sea_level_temperature, reference_wind_speed, reference_wind_height
  
  ! Initial condition:
  if (input_namelists()) then                     ! Production runs read input data from a file and 
    x = random_spherical_distribution(num_stars)  ! start stars at random locations on a unit sphere.
  else                        ! CTest runs use the above default initializations   
    x = default_star_position ! and the deterministic initial star location at left.
  end if 
  v = x/t ! Initialize the star velocities to the average time of travel from the origin to the sphere's surface.
  x(:,3) = x(:,3) + burst_elevation

  ! Time marching:
  march_forward_in_time: do while (t<t_final) 
    x = x + v*dt ! March position forward
    wind_velocity = local_wind_velocity(x)
    associate(m=>star_mass,a=>star_radius,six=>6._rkind)
      response_time = m/(six*pi*local_air_viscosity(x,sea_level_temperature)*a)
      relative_velocity =  v - wind_velocity 
      do concurrent (i=1:size(v,1)) 
        v(i,:) = v(i,:) + ( g_vector(:) - relative_velocity(i,:)/response_time(i) )*dt ! March velocity forward
      end do
      if (verbose) print "(a,9f10.3)","x,v,v_terminal=",x(1,:),v(1,:),response_time(1)*g_vector(:)
      t=t+dt       ! March time forward
    end associate
  end do march_forward_in_time

  ! Assignment:
  ! 1. In a new block labeled integration_test, declare an integer variable image_failed and set it to 
  !     {  0 if .true. is the logical result of the function reference reached_terminal_velocity(x,v,response_time,tolerance)
  !     {  1 otherwise
  ! 2. Use the co_sum collective subroutine to sum the number of image failures, providing the result to image 1 only.   
  ! 3. Error terminate if the sum is non-zero.
  ! 4. Otherwise, print "Test passed."

contains

  logical function input_namelists()
    character(kind=ckind,len=19) ,parameter :: filename='fireworks_input.txt'
    integer(ikind), parameter :: found=0,file_unit=99
    integer(ikind) :: namelist_status,file_status

    open(file_unit,file=filename,form='formatted',status='old',iostat=file_status)
    if (file_status==found) then 
      read(file_unit,nml=fireworks_properties,iostat=namelist_status) 
      if (verbose .and. namelist_status/=found) print *,'main.F90: Using default initializations for fireworks_properties.'
      read(file_unit,nml=timeline,iostat=namelist_status)             
      if (verbose .and. namelist_status/=found) print *,'main.F90: Using default initializations for timeline.'
      read(file_unit,nml=sky_properties,iostat=namelist_status)
      if (verbose .and. namelist_status/=found) print *,'main.F90: Using default initializations for sky_properties.'
      close(file_unit)
      input_namelists = .true.
    else
      input_namelists = .false.
    end if
  end function

end program
