program limit

implicit none
!Declaracion de variables
double precision :: z, fac, frac, fract, gamma, nd
integer :: n, i

!se le pide al usuario que ingrese el valor de z
print*, "Ingrese valor de z:"
read*, z

frac=1
fac=1
nd=170-z
n=nd

!Se verifica que el valor ingresado sea acceptable
if (z>=1) then
    print*, "Calculando el valor de gamma..."
    !factorial de n
    do i=1, n
        fac=fac*i
    end do

    !fraccion summa
    do i=0, n
        fract=z+i
        frac=frac*fract
    end do

    !gamma
    gamma=(fac/frac)*(n**z)

    print*, "Para Gamma(", z, ") el valor es:", gamma
else
    print*, "Esta aproximaciones es para valores mayores a 1."
end if

end program limit