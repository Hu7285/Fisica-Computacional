!  Fecha: 17 de mayo de 2021
!  Nombre del programa: funciondensidadpar.f90
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
!    Instrucciones de compilación: no requiere nada mas
!    mpifort -Wall -pedantic -std=f95 -c -o funciondensidadpar.o funciondensidadpar.f90
!    mpifort -o funciondensidadpar.x funciondensidadpar.o
!    mpirun -np 2 ./funciondensidadpar.x
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

program funciondensidadparalelo
!Se llama el modulo de MPI para el manejo de varios nucleos
USE mpi

implicit none
!Declaracion de variables
!variables para las iteraciones
integer(8) :: n=10000
integer(8) :: i
real(8) :: ran
!variables dinamicas para el almacenamiento de uniforme (uni), normal (nor), exponencial (expo)
real(8), ALLOCATABLE, dimension(:) :: uni, nor, expo
!variables para el mpi
integer(4) :: rank, ierr, size

!Se inicia el MPI y se declara cada nucleo que se va a utilizar
CALL MPI_INIT(ierr)
if ( ierr /= 0 ) stop "Error al inicializar MPI" 
CALL MPI_COMM_SIZE(MPI_COMM_WORLD, size, ierr)
call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)

!se define que el tamaño de los arreglos que se utilizaran
allocate(uni(n), nor(n), expo(n))
!se generan los valores aleatorios para la distribucion
do i=1, n
    call random_number(ran)
    uni(i)=ran
end do
!similar al proceso secuencial, se crean dos archivos para guardar los datos
!se usaran dos nucleos, uno para el calculo de la distribucion normal y otro para el exponencial
!nucleo para el calculo de la distribucion normal
if (rank==0) then
    nor = (-log( uni**2 ))**0.5
    open(12, file="normalp.txt")
        do i=1,n
            write(12,*) nor(i), uni(i)
            write(12,*) - nor(i), uni(i)
        end do
    close(12)
end if
!nucleo para el calculo de la distribucion exponencial
if (rank==1) then
    expo = -log( uni**2)*(1/100)
    open(13, file="exponencialp.txt")
        do i=1,n
            write(13,*) expo(i), uni(i)
        end do
    close(13)
end if 

deallocate(uni, nor, expo)

!finaliza el modulo MPI
call MPI_FINALIZE(ierr)

end program funciondensidadparalelo