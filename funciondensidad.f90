!  Fecha: 17 de mayo de 2021
!  Nombre del programa: funciondensidad.f90
!  Creado por: Hugo Galileo Cardona Gonzalez (hu.galileo@gmail.com)
!  Copyright (C) 2021
!  Bryant Morazán
!  bryant.morazan@gmail.com
!
!  Este programa esta basado en la estructura del codigo realizado y expuesto 
!  por Bryant Morazán en el curso de Fisica computacional 2021
!  autorizando su uso sin ningun problema.
!
!  Codificación del texto: UTF8
!  Compiladores probados: GNU Fortran (SUSE Linux) 4.8.5
!
!  Instrucciones de compilación: Ingresar las siguientes líneas una por una en la terminal:
!    gfortran -Wall -pedantic -std=f95 -c -o funciondensidad.o funciondensidad.f90
!    gfortran -o funciondensidad.x funciondensidad.o 
!    /usr/bin/time -f "%e %M %P" ./funciondensidad.x
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

program funciondensidad

implicit none
!Declaracion de variables
!variables para las iteraciones                                         
integer(8) :: n, i
real(8) :: ran
!variables dinamicas para el almacenamiento de uniforme (uni), normal (nor), exponencial (expo)
real(8), ALLOCATABLE, dimension(:) :: uni, nor, expo

n=12000000


!se define que el tamaño de los arreglos que se utilizaran
allocate(uni(n), nor(n), expo(n))
!se generan los valores aleatorios para la distribucion
do i=1, n
    call random_number(ran)
    uni(i)=ran
end do
!generacion de distribucion normal y exponencial
nor = (-log( uni**2 ))**0.5
expo = -log( uni**2)*(1/1)

!se abre los archivos de texto en los cuales de va a guardar los datos
!archivo para normal
open(12, file="normal.txt")
        do i=1,n
            write(12,*) nor(i), uni(i)
            write(12,*) -nor(i), uni(i)
        end do
close(12)
!archivo para exponencial
open(13, file="exponencial.txt")
        do i=1,n
            write(13,*) expo(i), uni(i)
        end do
close(13)

deallocate(uni, nor, expo)

end program funciondensidad