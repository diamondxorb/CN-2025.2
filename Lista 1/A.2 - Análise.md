# Questão A.2

> A. Considere o processo iterativo x(n+1) = ax(n) + b, com a, b ∈ IR.
>
> 2. Ainda para o caso b = 0, identifique todos os 7 casos relevantes para os valores constante
> a e avalie, a partir de um gráfico para cada um dos casos, a estabilidade do ponto fixo x⋆ = 0.

Código da questão anterior:

```
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
```

# Análise dos casos

### 1. a = 0
<img width="640" height="480" alt="caso1" src="https://github.com/user-attachments/assets/88780179-0d65-4dba-8283-aecb8bca4ae9" />

Independente de x(0), a sequência converge imediatamente para 0.

- Estabilidade: Superestável


### 2. 0 < a < 1
![Caso2](/home/helen/Códigos/caso2.png)
A sequência decai exponencialmente para 0.

- Estabilidade: Estável


### 3. a = 1
![Caso3](/home/helen/Códigos/caso3.png)
Todos os pontos são fixos. A sequência permanece em x(0).

- Estabilidade: Neutro


### 4. a > 1
![Caso4](/home/helen/Códigos/caso4.png)
A sequência cresce exponencialmente e diverge de 0.

- Estabilidade: Instável


### 5. -1 < a < 0
![Caso5](/home/helen/Códigos/caso5.png)
A sequência oscila com amplitude decrescente e converge para 0.

- Estabilidade: Estável


### 6. a = -1
![Caso6](/home/helen/Códigos/caso6.png)
A sequência oscila entre x(0)​ e −x(0)​ indefinidamente.

- Estabilidade: Neutro

  
### 7. a < -1
![Caso7](/home/helen/Códigos/caso7.png)
A sequência oscila com amplitude crescente e diverge.

- Estabilidade: Instável
