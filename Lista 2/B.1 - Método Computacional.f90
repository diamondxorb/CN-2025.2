program comp
    implicit none
    
    integer :: i
    real, dimension(0:100) :: x

    write(*,*) "Escolha o chute inicial:"
    read(*,*) x(0)

    ! Os dois zeros reais são:
    ! x1 = -4.06592861E-02
    ! x2 = 0.962398469
    ! Para achar x2 o chute inicial precisa ser x(0) > 0.589

    do i=0,99
        x(i+1) = (x(i) - (f(x(i))/flinha(x(i))))
        write(*,*) i, x(i+1)
    end do

contains
    function f(x_f)
        implicit none
        
        real :: f, x_f

        f = (230*x_f**4.0 + 18*x_f**3.0 + 9*x_f**2.0 - 221*x_f - 9) 

        return
    end function

    function flinha(x_fl)
        implicit none
        
        real :: flinha, x_fl

        flinha = (940*x_fl**3 + 54*x_fl**2 + 18*x_fl - 221)

        return
    end function flinha
end program comp
