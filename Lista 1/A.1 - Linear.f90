program linear
    implicit none

    integer :: n
    real, dimension(0:100) :: x
    real :: a, b=0

    read(*,*) a

    x(0) = 1.0

    open(unit=127, file="saida.dat", status="unknown")

    do n=0,99
        x(n+1) = a*x(n) + b
        write(127,*) x(n), x(n+1)
    end do
    
    close(127)

end program linear
