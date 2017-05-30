! sky_interface.f90
! 
! -- Define an interface for calculating atmospheric properties
!
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
module sky_procedures_interface

  use kind_parameters ,only : rkind

  implicit none

  private

  public :: local_wind_velocity
  public :: local_air_viscosity
  public :: local_air_temperature
  
  interface
    
    ! Return an array of wind velocities at the provided particle positions 
    pure module function local_wind_velocity(positions) result(velocity)
      real(rkind), intent(in) :: positions(:,:)
      real(rkind) :: velocity(size(positions,1),size(positions,2))
    end function

    ! Return an array of air temperatures at the provided particle positions 
    ! and sea-level conditions
    pure module function local_air_temperature(positions,T_sea_level) result(temperature)
      real(rkind), intent(in) :: positions(:,:),T_sea_level
      real(rkind), allocatable :: temperature(:)
    end function
  
    ! Sutherland's formula (http://www.absoluteastronomy.com/topics/Viscosity)
    pure module function local_air_viscosity(positions,T_sea_level) result(viscosity)
      real(rkind), intent(in) :: positions(:,:),T_sea_level
      real(rkind) :: viscosity(size(positions,1))
    end function

  end interface

end module
