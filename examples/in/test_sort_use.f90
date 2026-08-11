module test_sort_use
   use zebra
   use alpha
   use, intrinsic :: iso_fortran_env
   use :: middle

   use gamma, only: delta, beta
   use beta, only: zulu, alfa => tango, oscar

   ! a comment breaks the group
   use foxtrot
   use echo

   use kilo, only: this_is_a_long_name, another_long_name, yet_another_long_one, &
                   and_one_more_name_here, plus_a_final_long_name_to_overflow

   use lima, only: operator(+), zulu, assignment(=), operator(.myop.)

   use novem  !&
   use bravo

contains

   subroutine inner()
      use sierra
      use romeo
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
