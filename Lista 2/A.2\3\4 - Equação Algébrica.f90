! Esse programa resolve os números 2, 3 e 4 da lista!
program equacao
    implicit none
    
    integer :: i, j, choice, quant_itera
    real :: chute_inicial, inter_a, inter_b
    real, dimension(0:100) :: x
    logical :: flag = .true.

    do while(.true.)
        write(*,*) ""
        write(*,*) "Escolha por qual método:"
        write(*,*) ""
        write(*,*) "1. Ponto Fixo"
        write(*,*) "2. Bisseção"
        write(*,*) "3. Newton-Raphson"
        write(*,*) "4. Sair"
        write(*,*) ""

        read(*,*) choice
        
        ! Saindo do programa caso a opção seja 4
        if(choice==4) then
            call system("clear")
            stop
        end if

        ! Determina o intervalo da função
        write(*,*) "Digite qual o intervalo:"
        read(*,*) inter_a, inter_b
        write(*,*) ""
        
        if(choice .ne. 2) then
            ! Um do while para certificar que o chute inicial está dentro do intervalo
            do while(flag)
                write(*,*) "Escolha o chute inicial:"
                read(*,*) chute_inicial
                write(*,*) ""
                
                if(chute_inicial>inter_b .or. chute_inicial<inter_a) then
                    write(*,*) "O chute inicial não está dentro do intervalo!"
                    write(*,*) ""
                else
                    x(0) = chute_inicial
                    flag = .false.
                end if
            end do
        end if

        ! Resolvendo a equação algébrica por ponto fixo
        if(choice==1) then
            do i=0,99
                ! Essa primeira equação funciona muito bem no intervalo [1,3.2]
                ! Mas não tanto nos outros, por isso ficou como comentário
                ! x(i+1) = ((x(i)**3 + 14*x(i) -6)/(7*x(i)))
                x(i+1) = ((-x(i)**3 + 7*(x(i)**2) + 6)/14.0) 
                write(*,*) i, x(i+1)
            end do
        
        ! Resolvendo a equação algébrica por bisseção
        else if(choice==2) then
            do i=0,99
                if(f(inter_a)*f(inter_b)<0) then
                    x(i) = (inter_a+inter_b)/2.0
                    write(*,*) i, x(i)

                    if(f(inter_b)*f(x(i))<0) then
                        inter_a = x(i)
                    else
                        inter_b = x(i)
                    end if
                else
                    j = i
                    exit
                end if
            end do
        
        ! Resolvendo a equação algébrica por Newton-Raphson
        else if(choice==3) then
            do i=0,99
                x(i+1) = (x(i) - (f(x(i))/flinha(x(i))))
                write(*,*) i, x(i+1)
            end do

        else
            write(*,*) ""
            write(*,*) "Esse número não é uma opção!"
        end if

        ! Passa por todos x(i) até ver quando começa a se repetir
        do i=0,99
            if(choice .ne. 2) then
                if(i>2) then
                    if(x(i)==x(i-2)) then
                        quant_itera = i-2
                        exit
                    end if
                end if
            else
                quant_itera = j
            end if
        end do

        write(*,*) ""
        write(*,*) quant_itera, " iterações"
        stop
    end do

    contains
        function f(x_f)
            implicit none

            real :: f, x_f

            f = (x_f**3.0 -7*(x_f**2) + 14*x_f - 6)

            return
        end function f

        function flinha(x_fl)
            implicit none
            
            real :: flinha, x_fl

            flinha = (3*(x_fl**2) -14*x_fl + 14)

            return
        end function flinha

end program equacao
