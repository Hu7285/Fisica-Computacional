!  Fecha: 17 de mayo de 2021
!  Nombre del programa: hanoi.f90
!  Creado por: Hugo Galileo Cardona Gonzalez (hu.galileo@gmail.com)
!  Copyright (C) 2021
!  Jefferson S. Rodríguez
!  jefersonrodriguezleon@gmail.com
!
!  Este programa esta basado en la estructura del codigo realizado y expuesto 
!  por Jefferson S. Rodríguez en el curso de Fisica computacional 2021
!  autorizando su uso sin ningun problema.
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

program hanoi
implicit none
!Declaracion de variables
!se tiene variables que seran caracteres para representar cada torre
character(*), parameter :: A='A', B='B', C='C'
!variables para el arlgoritmo
integer(4):: d, i=1

d=27

!se verifica que el valor de los discos ingresados sea valido
if (d>=1) then
  !se crea un documento para guardar los pasos que el algoritmo realizo
  open(5, file ='pasos.txt')
  call pasos(i,d,A,B,C)
  close(5)
else
    print*, "El numero de discos debe ser mayor o igual a 1"
end if

contains
!funcion recursiva para el algoritmo
recursive subroutine pasos (i,d,inicio,medio,final)
  implicit none
  !variables para el algoritmo
  integer(4), intent(inout) :: i
  integer(4), intent(in) :: d
  CHARACTER(*), intent(in):: inicio, medio, final

  !se toma el caso base en que el numero de discos es igual a 1
  if (d==1) then
    write(5,*) i,' Mover de ', inicio,' a ', final
    i=i+1
  else
    !se realiza el algoritmo en donde se pasan los d-1 discos al poste de B, luego se pasa el disco mas grande al poste C
    !y por ultimo se pasan los d-1 discos al poste C
    call pasos(i, d-1, inicio, final, medio)
    call pasos(i, 1, inicio, medio, final)
    call pasos(i, d-1, medio, inicio, final)
  end if
  
end subroutine pasos


end program hanoi