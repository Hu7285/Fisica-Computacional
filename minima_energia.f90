!    2021-05-24
!    minima_energia.f90
!    Felipe Ixcamparic (felipechoy1@gmail.com)
!    Hugo Cardona (hugo.ecfm@gmail.com)

!    Este programa utiliza el método de Monte Carlo
!    para encontrar configuraciones de mínima energía
!    con n partículas con carga similar en la superficie
!    de una esfera unitaria.

!    Codificación del texto: UTF8
!    Compiladores probados: GNU Fortran (WSL Ubuntu 20.04) 7.5.0
!    Instrucciones de compilación: no requiere nada mas
!    gfortran -Wall -pedantic -std=f95 -c -o minima_energia.o minima_energia.f90
!    gfortran -o minima_energia.x minima_energia.o 
!    gnuplot splot "posiciones_finales.txt"
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
!


PROGRAM problema3
    IMPLICIT NONE
    !Número de partículas (como parámetro para el vector)
    INTEGER,PARAMETER::N=8
    !Número de partículaes (como variable para valuarlo en función)
    INTEGER::ene=N
    INTEGER::iteraciones=100000
    !Vector que almacenará cada partícula con su posición
    REAL(8),DIMENSION(N,3)::posicionesviejas,posicionesnuevas

    !Se llama a la función que calculará el potencial entre las partículas
    REAL(8),EXTERNAL::potencial

    !Variables auxiliares 
    REAL(8)::thetatemp=0,phitemp=0,e_antigua=0,e_nueva=0

    !Iteradores
    INTEGER::i,j




    !Se inicializan valores aleatorios para las posiciones antes de llamar a iterar
    !estas funcionarán como base
    DO i=1,n

        thetatemp=0
        phitemp=0

        !Generación de valores aleatorios para los ángulos theta y phi 
        !ya que se trabaja en una esfera unitaria
        CALL RANDOM_NUMBER(thetatemp)
        thetatemp=thetatemp*2*3.141516
        CALL RANDOM_NUMBER(phitemp)
        phitemp=ACOS(2*phitemp-1)

            
        !POsiciones en forma cartesiana (transformación)
        posicionesviejas(i,1)=SIN(phitemp)*COS(thetatemp)
        posicionesviejas(i,2)=SIN(phitemp)*SIN(thetatemp)
        posicionesviejas(i,3)=COS(phitemp)

    END DO

    !Asignación de valor de potencial antiguo para comparar en la minimización
    !de energía
    e_antigua=potencial(ene,posicionesviejas)
    
    !           MINIMIZANDO LA ENERGÍA Y GUARDANDO LAS POSICIONES               !
    !--------------------------------------------------------------------------!

    !Iteraciones para minimizar la energía en cada iteración posible
    DO j=1,iteraciones

        !Generación de una posicion aleatoria 
        DO i=1,n
            thetatemp=0
            phitemp=0
            
            !Proceso de generación de números aleatorios y almacenamiento en vectores
            CALL RANDOM_NUMBER(thetatemp)
            thetatemp=thetatemp*2*3.141516   !Ángulo theta de 0 a 2pi
            CALL RANDOM_NUMBER(phitemp)
            phitemp=ACOS(2*phitemp-1)        !Ángulo phi de 0 a pi, se genera de esta forma para evitar
                                                  !acumulamiento de puntos en los polos
            !Transforación de las posiciones a su forma cartesiana
            posicionesnuevas(i,1)=SIN(phitemp)*COS(thetatemp)
            posicionesnuevas(i,2)=SIN(phitemp)*SIN(thetatemp)
            posicionesnuevas(i,3)=COS(phitemp)
        END DO

        !Se almacena la neuva energía entre las partículas
        e_nueva=potencial(ene,posicionesnuevas)


        !Comparación entre los valores de energía, en caso que esta sea
        !menor a la antigua, se guardará entonces este en la variable e_antigua

        IF (e_nueva < e_antigua) THEN
            e_antigua=e_nueva
            ! PRINT*,"Energía nueva encontrada",e_antigua !Debugging

            !Se cambian las posiciones de ser el caso
            DO i=1, N
                posicionesviejas(i,1)=posicionesnuevas(i,1)
                posicionesviejas(i,2)=posicionesnuevas(i,2)
                posicionesviejas(i,3)=posicionesnuevas(i,3)
            END DO
        END IF

    END DO

    PRINT*,"La energía mínima encontrada para N = ",N,"es",e_antigua
   
    !Se imprimen las posiciones de cada partícula en un archivo de texto para poder visualizar
    ! la configuración de cada uno de ellos
    OPEN(10,file="posiciones_finales.txt")
    
    DO i=1,N
        WRITE(10,*)posicionesviejas(i,1),posicionesviejas(i,2),posicionesviejas(i,3)
    END DO

    CLOSE(10)

END PROGRAM problema3


!  ENERGÍA POTENCIAL ELÉCTRICA   Creada por Hugo Cardona(hugo.ecfm@gmail.com)!
!Función para calcular la energía potencial eléctrica entre n partículas    !
!Se requiere que las posiciones de todas esten dentro de un                 !
!vector de n,3 dimensiones con sus compoentes rectanculares                 !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! x,y,z
REAL(8) FUNCTION potencial(ene,posiciones)
    INTEGER(8)::i=0,j=0                                 !Iteradores
    INTEGER(4),INTENT(IN)::ene                          !Lee la dimensión de la matríz
    REAL(8),DIMENSION(ene,3),INTENT(IN)::posiciones     !Lee la matríz que almacena las posiciones
    REAL(8)::aux                                     !Variable auxiliar
    aux=0


    !Se itera desde 2 hasta n veces siempre y cuando i>j para obtener los pares de energía potencial 
    !eléctrica.
    DO i=2,ene
        DO j=1,ene
            IF(i > j) THEN
                    !Se encuentra la magnitud de la diferencia de las posiciones
                    aux = aux + 1 / SQRT( (posiciones(j,1)-posiciones(i,1))**2 + &
                    &(posiciones(j,2)-posiciones(i,2))**2 + (posiciones(j,3)-posiciones(i,3))**2)
                    
            END IF    
        END DO
       
    END DO
    !El valor que retorna es igual al obtenido en la variable auxiliar.
    potencial=aux


END FUNCTION potencial
