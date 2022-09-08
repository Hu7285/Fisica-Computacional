program gammasimpson

implicit none
real,external::f
real::a, b, h, integral, z
integer::i, n

a=0
n=1e8
b=n

print*, "ingrese valor de z:"
read*, z

if (z>=1) then
    print*, "Calculando el valor de gamma..."
    h=(b-a)/(2*n)

    integral=h/3*(f(a,z)+f(b,z))

    do i=1, 2*n-1, 2
        integral=integral+4*h/3*(f(a+i*h,z))
    end do

    do i=2, 2*n-2, 2
        integral=integral+2*h/3*(f(a+i*h,z))
    end do

    print*, "Gamma de(", z, ") es:", integral
else
    print*, "Esta aproximaciones es para valores mayores a 1."
end if

end program gammasimpson

real function f(x,z)
    f=exp(-x)*x**(z-1)
end function