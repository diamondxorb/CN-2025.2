! gfortran -o teste 241039270.f90
! ./teste

! Júlia Paulo Amorim - 241039270

program teste
    implicit none
    
    integer :: n
    double precision, dimension(0:100) :: x

    ! O chute inical deve ser maior que 0
    x(0) = 1.d0

    do n=0,99
        x(n+1) = x(n) - (f(x(n))/flinha(x(n)))
        
        write(*,*) n, x(n)
    end do

    write(*,*) ""
    write(*,*) "A raiz x* desta equação com erro menor que 0.01 é"
    write(*,*) x(99)

contains
    function f(x_f)
        implicit none
        double precision :: f, x_f

        f = (log(x_f) + x_f - 5.d0)

        return
    
    end function f

    function flinha(x_fl)
        implicit none
        double precision :: flinha, x_fl

        flinha = ((1.d0/x_fl) + 1.d0)

        return
    end function flinha

end program teste
