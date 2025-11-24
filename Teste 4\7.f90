! 241039270 - Júlia Paulo Amorim
! Programa da questão 7

program teste
    implicit none

    double precision :: a, b, h, soma
    double precision :: t1,t2,t3,a1,a2,a3
    double precision, dimension (:), allocatable :: t
    integer :: i,N

    N = 10

    allocate(t(0:N))

    a = 0.d0
    b = 5.d0
    h = dabs(b-a)/dble(N)

    do i=0,N
        t(i) = a + i*h
    end do

    t1 = ((b-a)/2.d0)*(-dsqrt(3.d0/5.d0))+((b+a)/2.d0)
    t2 = ((b-a)/2.d0)*0.d0+((b+a)/2.d0)
    t3 = ((b-a)/2.d0)*(dsqrt(3.d0/5.d0))+((b+a)/2.d0)

    a1 = 5.d0/9.d0
    a2 = 8.d0/9.d0
    a3 = 5.d0/9.d0

    soma = (a1*f(t1)+a2*f(t2)+a3*f(t3))*((b-a)/2.d0)
    write(*,*) 'A integral é (gaussiana)', soma

    deallocate(t)

contains

    function f(x)
        implicit none
  
        double precision :: x, f, e
        
        e = exp(1.d0)
     
        f = e**(-x**2)*cos(x)
      
        return
    end function f
    
end program teste