 program main
   !! summary: Multimage estimate of a first derivative via central differences
   use iso_fortran_env, only : error_unit,output_unit
   implicit none
   real, allocatable :: f(:)[:],df_dx(:),x(:) 
     !! function, derivative, sampling locations
   integer, parameter :: num_points=1024
     !! number of samples
   real, parameter :: pi = acos(-1.), dx = 2.*pi/num_points  
     !! pi, grid spacing
   integer :: i
   enum, bind(C)
     !! enumerated boundaries
     enumerator left_boundary,right_boundary 
   end enum
   real :: inbox(left_boundary:right_boundary)[*]

   associate(me=>this_image(),ni=>num_images()) !! immutable runtime values

   if (mod(num_points,ni)/=0) error stop "num_points not evenly divisible by num_images()"
     !! the language standard requires equal-sized allocations across images

     associate(my_num_points=>num_points/ni) !! immutable runtime values
     associate( my_first=>(me-1)*my_num_points+1, my_last=> me*my_num_points  )
       x = [(i-1,i=my_first,my_last)]*dx  ! implied do loop
     end associate
     allocate( f(my_num_points)[*], source = sin(x) ) 
       !! every image must allocate the same coarray bounds and cobounds
       !! hence, the bounds of f differ from those of x (and df_dx)

     associate( left_neighbor=>merge(ni,me-1,me==1), right_neighbor=>merge(1,me+1,me==ni) ) !! immutable runtime values

       inbox(right_boundary)[left_neighbor] = f(1)
       inbox(left_boundary)[right_neighbor] = f(my_num_points)
       select case (ni) 
         case(1)
         case(2)
          if (me==1) sync images(2)
          if (me==2) sync images(1)
         case default
          sync images([left_neighbor,right_neighbor])
       end select
   
       allocate( df_dx(my_num_points) )

       df_dx(2:my_num_points-1) = (f(3:my_num_points) - f(1:my_num_points-2))/(2.*dx)
     
       get_halo: block
         !! Because the "f" coarray allocation coarray imposes an implicit synchronization, we proceed to
         !! getting the surrounding "halo" elements at the boundaries of the neighboring images' f coarrays.
         integer :: boundary
         do concurrent(boundary=left_boundary:right_boundary)
           select case(boundary)
             case(left_boundary) 
               df_dx(1) = ( f(2) - inbox(left_boundary) )/(2.*dx)
             case(right_boundary) 
               df_dx(my_num_points) = ( inbox(right_boundary) - f(my_num_points-1) )/(2.*dx)
             case default
               error stop "main: invalid boundary"
           end select 
         end do
       end block get_halo

       test: block
         !! Check whether L-infinity norm of error is acceptable
         real, parameter :: max_acceptable_error= 0.0001
         real :: L_infinity_error 
         L_infinity_error = maxval(abs(df_dx - cos(x))) 
         call co_max(L_infinity_error,result_image=1)
           !! every image must call a collective subroutine (we send the result to image 1 only)
         if (me==1) then
           if ( L_infinity_error > max_acceptable_error) then
             write(error_unit,"(2(a,f6.3))") &
               "L_infinity error norm ",L_infinity_error," exceeds allowable error ",max_acceptable_error
             error stop
           else
             write(output_unit,"(a)") "Test passed."
           end if
         end if 
       end block test
     end associate
   end associate
 end associate
!! Program termination involves an implicit, three-step process:
!! 1. Initiate termination (by reaching a "stop" or "end program")
!! 2. Syncrhonize
!! 3. Terminate
!! Therefore, no image terminates before image 1 either terminates normally or error-terminates.
end program main
