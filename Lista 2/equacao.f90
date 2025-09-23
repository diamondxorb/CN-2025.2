program resolutor_nao_linear
    implicit none
    
    ! Declaração de variáveis
    integer :: metodo, iter, max_iter
    real :: a, b, x0, precisao, raiz, erro
    real :: x_ant, x_prox, f_x, df_x
    logical :: convergiu
    character(len=1) :: continuar
    
    ! Interface para as funções
    interface
        function f(x) result(y)
            real, intent(in) :: x
            real :: y
        end function f
        
        function df(x) result(dy)
            real, intent(in) :: x
            real :: dy
        end function df
        
        function g(x) result(y)
            real, intent(in) :: x
            real :: y
        end function g
    end interface
    
    write(*,*) "=========================================="
    write(*,*) "  RESOLUTOR DE EQUAÇÕES NÃO-LINEARES"
    write(*,*) "=========================================="
    write(*,*) "Equação: x√x - cos(x) = 0"
    write(*,*) ""

    do
        ! Menu de escolha do método
        write(*,*) "Escolha o método de solução:"
        write(*,*) "1 - Ponto Fixo"
        write(*,*) "2 - Bisseção"
        write(*,*) "3 - Newton-Raphson"
        write(*,*) "0 - Sair"
        write(*,*) ""
        write(*,'(A)',advance='no') "Opção: "
        read(*,*) metodo
        
        if (metodo == 0) exit
        
        ! Entrada da precisão
        write(*,'(A)',advance='no') "Precisão desejada (ex: 1e-6): "
        read(*,*) precisao
        
        ! Entrada do intervalo ou ponto inicial
        select case(metodo)
        case(1) ! Ponto Fixo
            write(*,'(A)',advance='no') "Ponto inicial x0: "
            read(*,*) x0
            a = x0 - 0.1  ! Intervalo para verificação
            b = x0 + 0.1
            
        case(2) ! Bisseção
            write(*,'(A)',advance='no') "Extremo inferior do intervalo [a,b]: "
            read(*,*) a
            write(*,'(A)',advance='no') "Extremo superior do intervalo [a,b]: "
            read(*,*) b
            
        case(3) ! Newton-Raphson
            write(*,'(A)',advance='no') "Ponto inicial x0: "
            read(*,*) x0
            a = x0 - 0.1
            b = x0 + 0.1
            
        case default
            write(*,*) "Método inválido!"
            cycle
        end select
        
        ! Verificar se o intervalo é adequado
        if (.not. verificar_intervalo(a, b, metodo)) then
            write(*,*) "Intervalo inadequado para o método escolhido!"
            cycle
        end if
        
        ! Resolver a equação
        call resolver_equacao(metodo, a, b, x0, precisao, raiz, iter, convergiu)
        
        ! Mostrar resultados
        write(*,*) ""
        write(*,*) "=== RESULTADOS ==="
        if (convergiu) then
            write(*,'(A,F12.8)') "Raiz encontrada: x = ", raiz
            write(*,'(A,F12.8)') "f(x) = ", f(raiz)
            write(*,'(A,I5)') "Iterações necessárias: ", iter
        else
            write(*,*) "O método não convergiu!"
            write(*,'(A,I5,A)') "Máximo de ", iter, " iterações atingido."
        end if
        write(*,*) ""
        
        ! Perguntar se deseja continuar
        write(*,'(A)',advance='no') "Deseja resolver outra equação? (s/n): "
        read(*,*) continuar
        if (continuar == 'n' .or. continuar == 'N') exit
        write(*,*) ""
    end do
    
    write(*,*) "Programa finalizado."
    
contains

    ! Função f(x) = x√x - cos(x)
    function f(x) result(y)
        real, intent(in) :: x
        real :: y
        
        if (x >= 0.0) then
            y = x * sqrt(x) - cos(x)
        else
            y = -cos(x)  ! Para x negativo, √x não é real
        end if
    end function f

    ! Derivada de f(x): f'(x) = (3/2)√x + sen(x)
    function df(x) result(dy)
        real, intent(in) :: x
        real :: dy
        
        if (x > 0.0) then
            dy = 1.5 * sqrt(x) + sin(x)
        else if (x == 0.0) then
            dy = sin(x)  ! Derivada em 0 é 0 + 0 = 0
        else
            dy = sin(x)  ! Para x negativo
        end if
    end function df

    ! Função de iteração para ponto fixo: g(x) = (cos(x))^(2/3)
    function g(x) result(y)
        real, intent(in) :: x
        real :: y
        
        y = (cos(x))**(2.0/3.0)
    end function g

    ! Verificar se o intervalo é adequado
    logical function verificar_intervalo(a, b, metodo)
        real, intent(in) :: a, b
        integer, intent(in) :: metodo
        
        verificar_intervalo = .true.
        
        select case(metodo)
        case(2) ! Bisseção
            if (a >= b) then
                write(*,*) "Erro: a deve ser menor que b!"
                verificar_intervalo = .false.
            else if (f(a) * f(b) > 0.0) then
                write(*,*) "Erro: f(a) e f(b) devem ter sinais opostos!"
                write(*,'(A,F10.6)') "f(a) = ", f(a)
                write(*,'(A,F10.6)') "f(b) = ", f(b)
                verificar_intervalo = .false.
            end if
            
        case(3) ! Newton-Raphson
            if (abs(df(x0)) < 1e-10) then
                write(*,*) "Erro: derivada muito próxima de zero!"
                verificar_intervalo = .false.
            end if
        end select
    end function verificar_intervalo

    ! Subrotina principal de resolução
    subroutine resolver_equacao(metodo, a, b, x0, precisao, raiz, iter, convergiu)
        integer, intent(in) :: metodo
        real, intent(in) :: a, b, x0, precisao
        real, intent(out) :: raiz
        integer, intent(out) :: iter
        logical, intent(out) :: convergiu
        
        real :: x, fa, fb, fx, x_ant
        integer :: max_iter = 1000
        
        convergiu = .false.
        iter = 0
        
        select case(metodo)
        case(1) ! Ponto Fixo
            x = x0
            do iter = 1, max_iter
                x_ant = x
                x = g(x_ant)
                
                if (abs(x - x_ant) < precisao) then
                    convergiu = .true.
                    raiz = x
                    exit
                end if
            end do
            
        case(2) ! Bisseção
            fa = f(a)
            fb = f(b)
            x = a
            
            do iter = 1, max_iter
                x_ant = x
                x = (a + b) / 2.0
                fx = f(x)
                
                if (abs(fx) < precisao .or. abs(b - a) < precisao) then
                    convergiu = .true.
                    raiz = x
                    exit
                end if
                
                if (fa * fx < 0.0) then
                    b = x
                    fb = fx
                else
                    a = x
                    fa = fx
                end if
            end do
            
        case(3) ! Newton-Raphson
            x = x0
            
            do iter = 1, max_iter
                x_ant = x
                fx = f(x_ant)
                
                if (abs(df(x_ant)) < 1e-15) then
                    write(*,*) "Aviso: derivada próxima de zero!"
                    exit
                end if
                
                x = x_ant - fx / df(x_ant)
                
                if (abs(x - x_ant) < precisao .or. abs(fx) < precisao) then
                    convergiu = .true.
                    raiz = x
                    exit
                end if
            end do
            
        end select
        
        if (iter >= max_iter) then
            convergiu = .false.
            raiz = x
        end if
    end subroutine resolver_equacao

end program resolutor_nao_linear