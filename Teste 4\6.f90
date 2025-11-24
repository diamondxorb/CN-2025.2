! 241039270 - Júlia Paulo Amorim
! Programa da questão 6

program test2
    implicit none
    
    double precision, dimension(:), allocatable :: t, y, ylinha, yduaslinhas
    integer :: i, N, k
    double precision :: h, variavel, resp
    
    N = 10
    
    allocate(t(0:N))
    allocate(y(0:N))
    allocate(ylinha(0:N))
    allocate(yduaslinhas(0:N))
    
    t(0) = 0.0d0
    t(1) = 0.1d0
    t(2) = 0.2d0
    t(3) = 0.3d0
    t(4) = 0.4d0
    t(5) = 0.5d0
    t(6) = 0.6d0
    t(7) = 0.7d0
    t(8) = 0.8d0
    t(9) = 0.9d0
    t(10) = 1.0d0
    
	y(0) = 1.000d0
    y(1) = 0.995d0
    y(2) = 0.980d0
    y(3) = 0.954d0
    y(4) = 0.916d0
    y(5) = 0.866d0
    y(6) = 0.799d0
    y(7) = 0.714d0
    y(8) = 0.599d0
    y(9) = 0.434d0
    y(10) = 0.000d0
	
	h = 0.1d0
    
    !Cálculo da primeira derivada
    ylinha(0) = 0.5d0*(-3.d0*y(0)+4.d0*y(1)-y(2))/h
    do i=1, N-1
        ylinha(i) = 0.5d0*(y(i+1)-y(i-1))/h
    end do
    ylinha(N) = -0.5d0*(-3.d0*y(N)+4.d0*y(N-1)-y(N-2))/h
    
    !Cálculo da segunda derivada
    yduaslinhas(0) = 0.5d0*(-3.d0*ylinha(0)+4.d0*ylinha(1)-ylinha(2))/h
    do i=1, N-1
        yduaslinhas(i) = 0.5d0*(ylinha(i+1)-ylinha(i-1))/h
    end do
    yduaslinhas(N) = -0.5d0*(-3.d0*ylinha(N)+4.d0*ylinha(N-1)-ylinha(N-2))/h
    
    do i=0, N
    	write(*,*) t(i), y(i), ylinha(i), yduaslinhas(i)
    end do
    
    write(*,*) "No ponto 0.6:", ylinha(6), yduaslinhas(6)
    
    variavel = yduaslinhas(6)/(1.d0+ylinha(6)**2.d0)**3/2.d0
    
    write(*,*) "Resposta da curvatura:", variavel
    
    resp = variavel/(-1.d0)
    
    write(*,*) "Resposta do erro relativo:", resp
    
    deallocate(t)
    deallocate(y)
    deallocate(ylinha)
    deallocate(yduaslinhas)

end program test2