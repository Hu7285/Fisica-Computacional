!  Fecha: 17 de mayo de 2021
!  Nombre del programa: fibmat.f90
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
!    gfortran -Wall -pedantic -std=f95 -c -o fibmat.o fibmat.f90
!    gfortran -o fibmat.x fibmat.o 
!    /usr/bin/time -f "%e %M %P" ./fibmat.x
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
!matriz base
integer, dimension(2,2) :: a
!matriz para calculo de valor de fibonaccis
integer(8), dimension(2,2) :: b
!valor a calcular
integer :: x=85

!definiendo la matriz base
a(1,1)=1
a(1,2)=1
a(2,1)=1
a(2,2)=0

!llamado a la subrutina que calcula el valor
call mat(a,x-1,b)
print*, "El valor de fibonacci de: ", x
print*, "Calculado con la matriz: ", b(1,1)

end program fibonacci

!Matriz para calcular el valor de fibonacci
subroutine mat(a,b,c)
    !variables de entradas y salidas
    integer, dimension(2,2), intent(in) :: a
    integer, intent(in) :: b
    integer(8), dimension(2,2), intent(out) :: c
    !variables internas para los algoritmos
    integer :: i, j, k
    integer(8), dimension(2,2) :: d

    c(1,1)=1
    !caso base en que el valor es 1 o 0
    if (b==-1) then
        c(1,1)=0
    end if
    !se copia la matriz a a una variable temporal d
    do i=1, 2
        do j=1, 2
            d(i,j)=a(i,j)
        end do
    end do
    !se hace el ciclo do hasta que se obtiene el valor
    do k=1, b-1
        do i=1, 2
            !algoritmo para calculo de la multiplicacion de matrices
            do j=1, 2
                c(i,j)=a(i,1)*d(1,j)+a(i,2)*d(2,j)
            end do
        end do
        do i=1, 2
            do j=1, 2
                d(i,j)=c(i,j)
            end do
        end do 
    end do
    
end subroutine mat