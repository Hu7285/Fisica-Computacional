!  Fecha: 17 de mayo de 2021
!  Nombre del programa: biseccion.f90
!  Creado por: Hugo Galileo Cardona Gonzalez (hu.galileo@gmail.com)
!  Copyright (C) 2021
!  Felipe Ixcamparic
!  felipechoy1@gmail.com
!
!  Este programa esta basado en la estructura del codigo realizado y expuesto 
!  por Felipe Ixcamparic en el curso de Fisica computacional 2021
!  autorizando su uso sin ningun problema.
!
!  Codificación del texto: UTF8
!  Compiladores probados: GNU Fortran (SUSE Linux) 4.8.5
!
!  Instrucciones de compilación: Ingresar las siguientes líneas una por una en la terminal:
!    gfortran -Wall -pedantic -std=f95 -c -o biseccion.o biseccion.f90
!    gfortran -o biseccion.x biseccion.o 
!    /usr/bin/time -f "%e %M %P" ./biseccion.x
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

program biseccion
implicit none
!Declaracion de variables
!variables n para el numero de iteraciones
integer(8) :: n
!variables para el limite superior (a) e inferior (b), el error (err) y para la precision (pres) que se desea tener
real(8) :: a, b, err, pres
!llamado a las funciones externas para los calculos del algoritmo
real(8) :: bisec, func

!definimos las iteraciones n al igual que los limites superior e inferior
pres=1e-6
n=35
a=0
b=6.38

!se realiza la prueba para verificar que la funcion ingresada tenga raices en el intervalo [a,b] al igual que verificar
!que los limites ingresados sean acceptables
if (func(b)*func(a)>0) then
  print *, "La funcion en el intevalo no tiene cero"
else if (a>b) then
  print *,"los limites no son acceptables, se debe cumplir que b>a"
else
  print *, "Con:", n, "iteraciones"
  print *, "Existe un 0 en:" , bisec(a,b,n,err,pres)
  print *, "con un error de:", err
end if
end program biseccion

!funcion que contiene la funcion que se desea evaluar

function func(x) result(y)
  !variables para le definicion de la funcion
  real(8), intent(in) :: x
  real(8) :: y
  !se escribe la funcion a se evaluada
  y=sin(x)
end function

!funcion para el algoritmo de biseccion

function bisec(a, b, n, err, pres) result(w)
!se toman las variables para el intervalo superior, inferior y el numero de iteraciones
real(8), intent(in) :: a, b, pres
integer(8), intent(in) :: n
!variables para asistir en el algoritmo
real(8) :: inf, sup
real(8) :: w, h, err, func
integer(8) :: i

!se usan las variables temporales para el algoritmo
inf=a
sup=b

!algoritmo para el metodo de biseccion
do i=1, n
  !se empieza con boteniendo el punto medio
  h=(inf+sup)/2
  !se hace la verificacion para ver si se tiene un cero
  !no se busca un cero exacto sino en un intervalo muy pequeño para verificar el resultado
  if (func(h)<=pres .and. func(h)>=-1*pres) then
    w=h
  !Se busca que cumpla con la condicion de cambio de signo, si se satisface se reduce el limite superior
  else if (func(h)*func(inf)<0) then
    sup=h
  else
    inf=h
  end if
end do
!calculo del valor absoluto
err=ABS(inf-sup)/2
w=h
end function