program integral

implicit none
double precision :: summ, z, x
integer :: n, i

print *, "Ingrese valor de z:"
read *, z

n=1e8

if (z>=1) then
  print*, "Calculando el valor de gamma..."
  do i=0, n
    x=i
    summ=summ+exp(-x)*(x**(z-1))
  end do

  print *, "Con:", n, "iteraciones"
  print *, "Para Gamma(", z, ") el valor es:", summ
else
  print*, "Esta aproximaciones es para valores mayores a 1."
end if

end program integral