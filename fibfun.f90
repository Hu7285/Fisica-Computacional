!  Fecha: 17 de mayo de 2021
!  Nombre del programa: fibfun.f90
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
!    gfortran -Wall -pedantic -std=f95 -c -o fibfun.o fibfun.f90
!    gfortran -o fibfun.x fibfun.o 
!    /usr/bin/time -f "%e %M %P" ./fibfun.x
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

program fibonacci
implicit none
!Declaracion de variables
integer(8) :: fun, i
integer :: x=55

!se hace una variables para poder ingresar el valor de x en la funcion recursiva
i=int(x,8)

print*, "El valor de fibonacci de: ", x
print*, "Calculado con la funcion: ", fun(i)

end program fibonacci

!Funcion para calcular el valor de fibonacci
recursive function fun(n) result (f)
    integer(8), intent(in) :: n
    integer(8) :: f
    !caso base, cuando el valor es 1 o 0 se da los valores ya conocidos
    if (n<=2) then
        f=1
        if (n<=0) then
            f=0
        end if
    !funcion recursiva para calcular el valor de fibonacci
    else
        f=2*fun(n-2)+fun(n-3)
    end if
end function fun