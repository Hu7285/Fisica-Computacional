!  Fecha: 17 de mayo de 2021
!  Nombre del programa: kronecker.f90
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
!    gfortran -Wall -pedantic -std=f95 -c -o kronecker.o kronecker.f90
!    gfortran -o kronecker.x kronecker.o 
!    /usr/bin/time -f "%e %M %P" ./kronecker.x
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

program kronecker
implicit none
!declaracion de variables
!se crean 3 variables para cada matriz (a b y c)
!estos tendran memoria allocatable que sera para definir sus dimensiones
real, dimension(:,:), allocatable :: ma
real, dimension(:,:), allocatable :: mb
real, dimension(:,:), allocatable :: mc
!variables para definir la dimension de cada matriz
integer :: a1, a2
integer :: b1, b2
integer :: c1, c2
!variables para el algoritmo
integer :: alfa, beta
integer :: i, j, k, l

!definicion de la dimension de las matrices, estas seran de (a1xa2), (b1xb2)
!para este caso solo se analizaran matrices cuadradas, por esta razon se defini a1=a2
!este proceso se repite para la matriz b y la c
a1=50
a2=a1
b1=a1
b2=b1
c1=a1*b1
c2=a2*b2

!se asigna espacio en la memoria para cada matriz
allocate(ma(a1,a2))
allocate(mb(b1,b2))
allocate(mc(c1,c2))

!se crea la matriz a y b de forma aleatoria
call random_number(ma)
call random_number(mb)

!ciclo do para el algoritmo establecido
do i=1, a1
    do j=1, a2
        do k=1, b1
            do l=1, b2
                alfa=b1*(i-1)+k
                beta=b2*(j-1)+l
                mc(alfa,beta)=ma(i,j)*mb(k,l)
            end do
        end do
    end do
end do

!se crea un archivo que guarda todos los datos de las matrices
open(11, file="krone.txt")
    write(11,*) 'a=', ma
    write(11,*) 'b=', mb
    write(11,*) 'c=', mc
close(11)

end program kronecker