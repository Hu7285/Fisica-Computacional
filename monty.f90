!  Fecha: 17 de mayo de 2021
!  Nombre del programa: monty.f90
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
!    gfortran -Wall -pedantic -std=f95 -c -o monty.o monty.f90
!    gfortran -o monty.x monty.o 
!    /usr/bin/time -f "%e %M %P" ./monty.x
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

program monty
implicit none
!declaracion de variables
!variables para el juego
integer :: pos, pue, dec, mos, res, i
!variables para llevar conteo de cuantas veces se gano cambiando o no cambiando
!estas empiezan en 0
integer :: camb=0, nocamb=0
!variables para la posicion de cada puerta
integer, dimension(3) :: puerta, cabras, puertaf
!variables para el algortimo
integer :: n
real :: a, b, c
real :: pc, pnc, np

!numero de iteraciones que se realizaran
n=1000000000
np=n

!ciclo do para n veces que se realiza el juego
do i=1, n
    res=0
    !se inicia creado las puertas de forma aleatoria
    call random_number(a)
    call random_number(b)
    call random_number(c)
    pue=int(a*3)
    pos=int(b*3)
    dec=int(c*2)
    !se hacen las pruebas de la decision inicial del participante y la verificacion si se debe cambiar o no
    call parin(pue,puerta)
    call parin(pos,cabras)
    call mostrar(puerta,cabras,mos)
    call cambiar(puerta,mos,dec,puertaf)
    call resultado(puertaf,cabras,res)

    !se verifica si se gano o si se perdio
    !este toma cuenta de cuantas veces se cambio de decision y se guarda las veces que se gano
    !similarmente se tomo cuenta de cuantas veces no se cambio de decision y se guarda las veces que se gano
    if(dec==0)then
        if (res==1)then
            nocamb=nocamb+1
        else
            camb=camb+1
        end if
    else
        if(res==1)then
            camb=camb+1
        else 
            nocamb=nocamb+1
        end if
    end if
end do
pc=(camb/np)*100
pnc=(nocamb/np)*100
print*, "De n=", n, "iteraciones"
print*, "Se cambio de decision y se gano= ", camb, "un porcentage de:", pc
print*, "No se cambio de decision y se gano=", nocamb, "un porcentage de:", pnc
end program monty

!funcion inicial que establece que puerta el participante decide
subroutine parin(a,y)
    integer, intent(in) :: a
    integer, dimension(3), intent(out) :: y
    if(a==0)then
        y=(/1,0,0/)
    else if(a==1)then
        y=(/0,1,0/)
    else if(a==2)then
        y=(/0,0,1/)
    end if
end subroutine parin

!funcion para asignar el carro a una puerta
subroutine carro(a,y)
    integer, intent(in) :: a
    integer, dimension(3), intent(out) :: y
    if(a==0)then
        y=(/1,0,0/)
    else if(a==1)then
        y=(/0,1,0/)
    else if(a==2)then
        y=(/0,0,1/)
    end if
end subroutine carro

!funcion que muestra una puerta que contiene a la una cabra
subroutine mostrar(a,b,y)
    integer, dimension(3), intent(in) :: a, b
    integer, intent(out) :: y
    if(a(1)==1)then
        if(b(2)==0)then
            y=2
        else
            y=3
        end if
    else if(a(2)==1)then
        if (b(1)==0)then
            y=1
        else
            y=3
        end if
    else if(a(3)==1)then
        if (b(1)==0)then
            y=1
        else
            y=2
        end if
    end if
end subroutine mostrar

!funcion para decidir si se cambia de puerta o no
subroutine cambiar(a,b,c,y)
    integer, dimension(3), intent(in) :: a
    integer, intent(in) :: b,c
    integer, dimension(3), intent(out) :: y
    y=a
    if(c==1)then
        if(b==1)then
            y(2)=a(3)
            y(3)=a(2)
        else if(b==2)then
            y(1)=a(3)
            y(3)=a(1)
        else if(b==3)then
            y(2)=a(1)
            y(1)=a(2)
        end if
    else
    end if
end subroutine cambiar

!funcion para analisar si el participante gano con la decision que tomo
subroutine resultado(a,b,y)
    integer, dimension(3), intent(in) :: a,b
    integer, intent(out) :: y
    integer, dimension(3) :: c
    integer :: i
    c=a+b
    do i=1, 3
        if (c(i)==2)then
            y=1
        end if
    end do
end subroutine resultado