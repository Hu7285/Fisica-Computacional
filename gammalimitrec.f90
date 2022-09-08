program limit

implicit none
double precision :: z, x, frac, fract, gamma(1), nd
integer :: n, i

print*, "Ingrese valor de z:"
read*, z

frac=1
nd=170-z
n=nd
if (z>=1) then
    print*, "Calculando el valor de gamma..."
    !fraccion summa
    do i=0, n
        fract=z+i
        frac=frac*fract
    end do

    !gamma
    gamma=(factorial(n)/frac)*(n**z)

    print*, "Para Gamma(", z, ") el valor es:", gamma
else
    print*, "Esta aproximaciones es para valores mayores a 1."
end if

contains

recursive function factorial(k)  result(f)
  real(16) :: f(1)
  integer, intent(in) :: k
  if(k==0) then
    f=1
  else
    f=k*factorial(k-1)
  end if
end function factorial

end program limit