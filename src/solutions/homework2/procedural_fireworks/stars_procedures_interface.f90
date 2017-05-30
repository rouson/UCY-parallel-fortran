! stars_procedures_interface.f90
! 
! -- Define an interface for calculating fireworks stars dynamics
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
module stars_procedures_interface

  use kind_parameters ,only : rkind,ikind

  implicit none

  private
  public :: random_spherical_distribution
  public :: reached_terminal_velocity
  
  interface

    module function random_spherical_distribution(num_stars) result(positions)
      use physical_constants, only : space_dimension
      integer(ikind) ,intent(in) :: num_stars
      real(rkind) :: positions(num_stars,space_dimension)
    end function

    module function reached_terminal_velocity(position,velocity,response_time,tolerance) result(is_terminal)
      real(rkind) ,intent(in)  :: position(:,:),velocity(:,:),response_time(:),tolerance
      logical :: is_terminal
    end function

  end interface

end module
