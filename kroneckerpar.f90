!  Fecha: 17 de mayo de 2021
!  Nombre del programa: hanoi.f90
!  Creado por: Hugo Galileo Cardona Gonzalez (hu.galileo@gmail.com)
!  Copyright (C) 2021
!  Hugo Galileo Cardona Gonzalez
!  hu.galileo@gmail.com
!
!  Codificación del texto: UTF8
!  Compiladores probados: GNU Fortran (SUSE Linux) 4.8.5
!
!  Instrucciones de compilación: Ingresar las siguientes líneas una por una en la terminal:
!    gfortran -Wall -pedantic -std=f95 -c -o hanoi.o hanoi.f90
!    gfortran -o hanoi.x hanoi.o 
!    /usr/bin/time -f "%e %M %P" ./hanoi.x
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
!se utilizara el modulo MPI para el manejo de nucleos
use mpi
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
integer :: i, j, k, l, split
!variables para el mpi
integer(4) :: rank, ierr, size

!se inicia el mpi
call mpi_init(ierr)
if ( ierr /= 0 ) stop "error al inicializar mpi"
call mpi_comm_size(mpi_comm_world, size, ierr)
call mpi_comm_rank(mpi_comm_world, rank, ierr)

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

!se crea una variables temporal para deividir el trabajo del ciclo do entre los dos nucleos
!como solo sera para 2 nucleos entonces la dimension de 
split=a1/2

if (rank==0)then
    do i=1, split
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
    open(11, file="kronen1.txt")
        write(11,*) 'a=', ma
        write(11,*) 'b=', mb
        write(11,*) 'c=', mc
    close(11)
end if

if (rank==1)then
    do i=split, a1
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
    open(11, file="kronen2.txt")
        write(11,*) 'a=', ma
        write(11,*) 'b=', mb
        write(11,*) 'c=', mc
    close(11)
end if
call mpi_finalize(ierr)
end program kronecker