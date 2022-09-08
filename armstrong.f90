!  Fecha: 17 de mayo de 2021
!  Nombre del programa: armstrong.f90
!  Creado por: Hugo Galileo Cardona Gonzalez (hu.galileo@gmail.com)
!  Copyright (C) 2021
!  Pablo Martínez
!  pabloversion1.0@gmail.com
!
!  Este programa esta basado en la estructura del codigo realizado y expuesto 
!  por Pablo Martínez en el curso de Fisica computacional 2021
!  autorizando su uso sin ningun problema.
!
!  Codificación del texto: UTF8
!  Compiladores probados: GNU Fortran (SUSE Linux) 4.8.5
!
!  Instrucciones de compilación: Ingresar las siguientes líneas una por una en la terminal:
!    gfortran -Wall -pedantic -std=f95 -c -o armstrong.o armstrong.f90
!    gfortran -o armstrong.x armstrong.o 
!    /usr/bin/time -f "%e %M %P" ./armstrong.x
!
!    This program is free software: you can redistribute it and/or
!    modify it under the terms of the GNU General Public License as
!    published by the Free Software Foundation, either version 3 of
!    the License, or (at your option) any later version.
!
!    This program is distributed in the hope that it will be useful,
!    but WITHOUT ANY WARRANTY; without even the implied warranty of
!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
!    General Public License for more details.
!
!    You should have received a copy of the GNU General Public License
!    along with this program.  If not, see
!    <http://www.gnu.org/licenses/>.

program armstrong
implicit none
!variables para el algoritmo
integer :: d, f
integer :: i, j, k
real :: xr, br
!variables para los ciclos do
integer :: n
!base a calcular
integer :: x

!declaracion de la base a calcular y en numero por base
x=10
n=1e8
print*, "Calculando..."

!se crea un archivo para guardar los datos
open(12, file='armstrong.txt')
write(12,*) 'números por base=', n
write(12,*) 'base=', x

do i=0, n
    xr=real(i)
    br=real(x)
    !se calcula el valor del coeficiente k
    call klog(xr,br,k)
    !ciclo do para la sumatoria
    do j=0, k-1
        !se calcula el valor del coeficiente di
        call di(j,i,x,d)
        f=d**k+f
    end do
    !se hace la verificacion para encontrar los numeros de armstrong
    !estos numeros se guardan en un archivo
    if (f==i) then
        write(12,*) f
    end if
    f=0
end do

print*, "se ha creado un archivo con los valores"

end program armstrong

!funcion para encontrar el coeficiente establecido de di
subroutine di(i,n,b,f)
    integer, intent(in) :: i, n, b
    integer, intent(out) :: f
    f=(mod(n,b**(i+1))-mod(n,b**i))/(b**i)
end subroutine di

!funcion para encontrar el coeficiente establecido de k
subroutine klog(x,b,f)
    real, intent(in) :: b, x
    integer, intent(out) :: f
    real :: z
    z=log(x)/log(b)
    f=int(z)+1
end subroutine klog