program aula
    implicit none
    
    integer :: i
    real :: seno, x = 0
    real, parameter :: pi = 3.141592653589793


    open(unit=127, file="saida.dat", status="unknown")

    do i=0,24

        seno = sin(x)
        
        write(127,*) x, seno

        !O 0.08 foi calculado a partir do espaço [0, 2pi], dado na questão, dividido por 25
        x = x + (0.08*pi)

    end do

    close(127)

end program aula