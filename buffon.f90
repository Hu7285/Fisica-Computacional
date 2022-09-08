!  Fecha: 17 de mayo de 2021
!  Nombre del programa: buffon.f90
!  Creado por: Hugo Galileo Cardona Gonzalez (hu.galileo@gmail.com)
!  Copyright (C) 2021
!  José-Tecún Cano
!  josetecun@gmail.com
!
!  Este programa esta basado en la estructura del codigo realizado y expuesto 
!  por José-Tecún Cano en el curso de Fisica computacional 2021
!  autorizando su uso sin ningun problema.
!
!  Codificación del texto: UTF8
!  Compiladores probados: GNU Fortran (SUSE Linux) 4.8.5
!
!  Instrucciones de compilación: Ingresar las siguientes líneas una por una en la terminal:
!    gfortran -Wall -pedantic -std=f95 -c -o buffon.o buffon.f90
!    gfortran -o buffon.x buffon.o 
!    /usr/bin/time -f "%e %M %P" ./buffon.x
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

program buffon

implicit none
!Declaracion de variables
!variables para los ciclos
integer :: N, i
!variables para las caracteristicas de la aguja y del gap entre lineas
integer :: gap, len
integer :: aguja
!varaibles para calculo de valor de pi
real :: yp, punta, dist
real :: lsin, sintheta
real :: pi

!se pide al usuario que ingrese el valor de N
print*, "Calculando..."

N=100000
gap=1
len=1
aguja=0

open(11, file="buffon.txt")
!se inicia el ciclo do para el calculo de cada aguja
do i = 1, N
    !se obtiene un valor aleatorio para la punta del aguja
    call random_number(punta)
    call random_number(sintheta)
    !se hacen los calculos para verificar si la aguja toca la linea
    lsin = len*sintheta
    dist = punta+lsin
    if (dist >= gap) then
        aguja = aguja + 1
    end if
    pi=(2*N)/aguja
    write(11,*) i, pi
end do
close(11)

!calculo de pi
pi=(2*N)/aguja

!se imprime los resultados
print*, "Número de agujas que tocaron una linea:", aguja
print*, "Valor de pi calculado:", pi

end program buffon