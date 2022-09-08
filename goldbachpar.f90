!  Fecha: 17 de mayo de 2021
!  Nombre del programa: goldbachpar.f90
!  Creado por: Hugo Galileo Cardona Gonzalez (hu.galileo@gmail.com)
!  Copyright (C) 2021
!  Laura Portillo
!  lauraportillo720@gmail.com
!
!  Este programa esta basado en la estructura del codigo realizado y expuesto 
!  por Laura Portillo en el curso de Fisica computacional 2021
!  autorizando su uso sin ningun problema.
!
!  Codificación del texto: UTF8
!  Compiladores probados: GNU Fortran (SUSE Linux) 4.8.5
! 
!    Instrucciones de compilación: no requiere nada mas
!    mpifort -Wall -pedantic -std=f95 -c -o goldbachpar.o goldbachpar.f90
!    mpifort -o goldbachpar.x goldbachpar.o
!    mpirun -np 2 ./goldbachpar.x
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



program goldbachpar
!uso del modulo MPI para manejo de nucleos
use mpi
implicit none
!declaracion de variables
!funciones externas para la verificacion de primo y sumas
integer,external :: primo
integer,external :: sumadeprimos2
integer,external :: sumadeprimos3
!variables para los ciclos
integer :: i, n
!variables para uso de mpi
integer(4) :: rank, ierr, size

!se llama el mpi
call mpi_init(ierr)
if ( ierr /= 0 ) stop "error al inicializar mpi"
call mpi_comm_size(mpi_comm_world, size, ierr)
call mpi_comm_rank(mpi_comm_world, rank, ierr)

!numero a calcular
n=3000

print*, "Calculando..."
!para el nucleo 0 se realiza la verificacion de suma entre dos numeros
!estos datos se guardan en un archivo

!se usa la funcion externa para verificar si es primo y se imprime
do i=2, n-1
    if(primo(i).ne.0)then
        !print*, primo(i)
    end if
end do

if (rank==0)then
    open(11, file="vn2p.txt")
        do i=1, n
            if(mod(i,2)==0)then
                write(11,*) i, sumadeprimos2(i)
            end if
        end do
    close(11)
end if

!para el nucleo 1 se realiza la verificacion de suma entre tres numeros
!estos datos se guardan en otro archivo
if (rank==1)then
    open(12, file="vn3p.txt")
        do i=1, n
            write(12,*) i, sumadeprimos3(i)
        end do
    close(12)
end if
call mpi_finalize(ierr)
print*, "Se han creado dos archivos txt"
end program goldbachpar

!funcion para verificar si es primo
integer function primo(n)
    integer, intent(in) :: n
    integer :: incremen 
    logical :: pri
    !se utiliza una funcion interna para realizar esta verificacion
    pri = .true.
    incremen = 2
    !ciclo do para verificar cada numero
    do while(pri .eqv. .true. .and. incremen<n)
        if (mod(n,incremen)==0) then
        pri= .false.
        end if
        incremen = incremen + 1
    end do
    if(pri .eqv..true.)then
        primo=n
    else  
        primo=0
    end if    
end function primo


!funcion para verificar la suma de dos numeros primos
integer function sumadeprimos2 (n)
    integer,external :: primo
    integer, intent(in) :: n
    integer :: v2, i, m 
    m = n/2
    v2=0
    !algoritmo para verifica si la suma entre dos numeros da el mismo y se lleva la cuenta con v2
    do i=2, m
        if(primo(i)== i) then
            if (primo(n-i)== n-i) then
                v2 = v2 + 1
            end if    
        end if
    end do
    sumadeprimos2 = v2
end function sumadeprimos2

!funcion para verificar la suma de tres numeros primos
integer function sumadeprimos3 (n)
    integer,external :: primo
    integer, intent(in) :: n
    integer, dimension(n) :: v
    integer :: i, p, q, r, v3, c, suma
    v3=0
    c=1
    do i=2, n
        if(primo(i)==i)then
            v(c)=i
            c=c+1
        end if 
    end do
    !algoritmo para verifica si la suma entre dos numeros da el mismo y se lleva la cuenta con v3
    do p=1, c
        do q=p, c
            do r=q, c
                suma= v(p) + v(q) + v(r)
                if (primo(v(p)).ne.0  .and. primo(v(q)).ne.0 .and.  primo(v(r)).ne.0 .and. suma==n )then 
                    v3= v3 + 1
                end if
            end do
        end do
    end do
    sumadeprimos3 = v3  
end function sumadeprimos3