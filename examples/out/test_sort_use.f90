module test_sort_use
   use alpha
   use, intrinsic :: iso_fortran_env
   use :: middle
   use zebra

   use beta, only: alfa => tango, oscar, zulu
   use gamma, only: beta, delta

   ! a comment breaks the group
   use echo
   use foxtrot

   use kilo, only: and_one_more_name_here, another_long_name, plus_a_final_long_name_to_overflow, this_is_a_long_name, &
                   yet_another_long_one

   use lima, only: assignment(=), operator(+), operator(.myop.), zulu

   use novem  !&
   use bravo

contains

   subroutine inner()
      use romeo
      use sierra
      implicit none
   end subroutine inner

end module test_sort_use

program p
   #:if defined('WITH_MPI')
      use mpi
   #:else
      use serial
   #:endif
   use alpha
   use omega
end program p
