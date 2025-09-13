program  cncn
    implicit none
    
    integer :: i
    real, dimension(0:24) :: x, seno, arcsen, diferenca

    open(unit=127, file="saida.dat", status="unknown")

    do i=0,24
        read(127, *) x(i), seno(i)
        arcsen(i) = asin(seno(i))
        diferenca(i) = abs(arcsen(i)-x(i))

    end do

    close(127)

    open(unit=128, file="saida.dat", status="unknown")

    do i=0,24
        write(128, *) x(i), seno(i), arcsen(i), diferenca(i)
    end do

    close(128)

end program  cncn
