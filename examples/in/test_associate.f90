program main_hello
    implicit none
    integer :: i
    i = 1

    associate (j => i + 5)
    do i = 1, 3
        write (*, *) 'Hello, World!', j
    end do
    write (*, *) 'Done'
    end associate
end program
