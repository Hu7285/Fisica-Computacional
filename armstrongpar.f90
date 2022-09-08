!  Fecha: 17 de mayo de 2021
!  Nombre del programa: armstrongpar.f90
!  Creado por: Hugo Galileo Cardona Gonzalez (hu.galileo@gmail.com)
!  Copyright (C) 2021
!  Hugo Galileo Cardona Gonzalez
!  hu.galileo@gmail.com
!
!  Codificación del texto: UTF8
!  Compiladores probados: GNU Fortran (SUSE Linux) 4.8.5
!
!    Instrucciones de compilación: no requiere nada mas
!    mpifort -Wall -pedantic -std=f95 -c -o armstrongpar.o armstrongpar.f90
!    mpifort -o armstrongpar.x armstrongpar.o
!    mpirun -np 2 ./armstrongpar.x
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

program armstrongparalelo
use mpi
implicit none
!variables para el algoritmo
integer :: d, f
integer :: i, j, k
real :: xr, br
!variables para los ciclos do
integer :: n, split
!base a calcular
integer :: x

!variables para el mpi
integer, dimension(2) :: wvec=(/0,0/)
integer :: ierr
integer :: rank
integer :: nprocs
!se verifica que el numero de nucleos este bien y se inicia el mpi
call mpi_init(ierr)
if (ierr/=0) stop 'mpi_init error'
call mpi_comm_size(mpi_comm_world,nprocs,ierr)
if (ierr/=0) stop 'mpi_comm_size error'
call mpi_comm_rank(mpi_comm_world,rank,ierr)
if (ierr/=0) stop 'mpi_comm_rank error'

!declaracion de la base a calcular y en numero por base
x=1
n=1e8
!variables para dividir el trabajo entre los dos nucleos
split=n/2

print*, "Calculando..."

!trabajo para el primer nucleo
if (rank==0) then
    !se crea un archivo para guardar los datos
    open(rank,file='armstrong_n0.txt')
    write(rank,*) 'base=', x
    write(rank,*) 'números por base'
    write(rank,*) 'entre: 0 hasta:', split
    do i=0, split
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
            write(rank,*) f
        end if
        f=0
    end do
!trabajo para el segundo nucleo
!este es el mismo algoritmo para el primer nucleo, lo unico que varia son los rangos del ciclo do
else if (rank==1) then
    open(rank,file='armstrong_n1.txt')              
    write(rank,*) 'base=', x
    write(rank,*) 'números por base'
    write(rank,*) 'entre:', split, 'hasta:', n
    do i=split, n
        xr=real(i)
        br=real(x)
        call klog(xr,br,k)
        do j=0, k-1
            call di(j,i,x,d)
            f=d**k+f
        end do
        if (f==i) then
            write(rank,*) f
        end if
        f=0
    end do
end if
call mpi_finalize(ierr)
print*, "se ha creado dos archivo con los valores"
end program armstrongparalelo

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