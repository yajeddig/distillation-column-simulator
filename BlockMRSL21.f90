subroutine block21(X,FX,blockA,blockB,blockC)
  USE MODULE_DIMENSION
  USE MODULE_INITIALISATION
  USE MODULE_OPERATOIRE
  USE MODULE_THERMODYNAMIC
    implicit none

    double precision, dimension ((2*Nc+3)*n) :: X,FX
    double precision, dimension (:), allocatable :: Xpert,FXpert
    double precision,dimension(2*Nc+3,2*Nc+3,n):: blockA,blockB,blockC
    double precision :: DVAR
    integer :: i,j,k

    allocate (Xpert((2*Nc+3)*n),FXpert((2*Nc+3)*n))
    Xpert=0.d0
    FXpert=0.d0
    blockA=0.d0
    blockB=0.d0
    blockC=0.d0
    DVAR=1.d-3

    call residu(X,FX)
    Xpert=X
    FXpert=0

!*************************Calcul des blocks B*************************

    do i=1,n   ! Calcul pour chaque �tage
        do j=1,2*Nc+3

            ! Calcul de FXpert
            Xpert((2*Nc+3)*(i-1)+j)=X((2*Nc+3)*(i-1)+j)*(1.d0+DVAR)
            call residu(Xpert,FXpert)

            ! Calcul de la d�riv� partielle de FX(k) par X(j)
            do k=1,2*Nc+3
                if (X((2*Nc+3)*(i-1)+j)==0) then
                    blockB(k,j,i)=(FXpert((2*Nc+3)*(i-1)+k)-FX((2*Nc+3)*(i-1)+k))/(DVAR)
                else
                    blockB(k,j,i)=(FXpert((2*Nc+3)*(i-1)+k)-FX((2*Nc+3)*(i-1)+k))&
                    /(X((2*Nc+3)*(i-1)+j)*DVAR)
                end if
            end do

            ! R�initialisation de Xpert
            Xpert((2*Nc+3)*(i-1)+j)=X((2*Nc+3)*(i-1)+j)

        end do
    end do

!*************************Calcul des blocks A*************************

    blockA(:,:,1)=0.d0
    do i=2,n
        do j=1,2*Nc+3
            ! Calcul de FXpert
            Xpert((2*Nc+3)*(i-2)+j)=X((2*Nc+3)*(i-2)+j)*(1.d0+DVAR)
            call residu(Xpert,FXpert)
            ! Calcul de la d�riv� partielle de FX(k) par X(j-1)
            do k=1,2*Nc+3
                if (X((2*Nc+3)*(i-2)+j)==0) then
                    blockA(k,j,i)=(FXpert((2*Nc+3)*(i-1)+k)-FX((2*Nc+3)*(i-1)+k))/(DVAR)
                else
                    blockA(k,j,i)=(FXpert((2*Nc+3)*(i-1)+k)-FX((2*Nc+3)*(i-1)+k))&
                    /(X((2*Nc+3)*(i-2)+j)*DVAR)
                end if
            end do
            ! R�initialisation de Xpert
            Xpert((2*Nc+3)*(i-2)+j)=X((2*Nc+3)*(i-2)+j)
        end do
    end do

!*************************Calcul des blocks C*************************

    blockC(:,:,n)=0.d0
    do i=1,n-1
        do j=1,2*Nc+3

            ! Calcul de FXpert
            Xpert((2*Nc+3)*i+j)=X((2*Nc+3)*i+j)*(1.d0+DVAR)
            call residu(Xpert,FXpert)

            ! Calcul de la d�riv� partielle de FX(k) par X(j+1)
            do k=1,2*Nc+3
                if (X((2*Nc+3)*i+j)==0) then
                    blockC(k,j,i)=(FXpert((2*Nc+3)*(i-1)+k)-FX((2*Nc+3)*(i-1)+k))/(DVAR)
                else
                    blockC(k,j,i)=(FXpert((2*Nc+3)*(i-1)+k)-FX((2*Nc+3)*(i-1)+k))&
                    /(X((2*Nc+3)*i+j)*DVAR)
                end if
            end DO

            ! R�initialisation de Xpert
            Xpert((2*Nc+3)*i+j)=X((2*Nc+3)*i+j)

        end do
    end do



end subroutine
