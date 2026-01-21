SUBROUTINE NRTL(gamm,X,TEMP)
  USE MODULE_DIMENSION
  USE MODULE_INITIALISATION
  USE MODULE_OPERATOIRE
  USE MODULE_THERMODYNAMIC
  IMPLICIT NONE

    integer :: i, j, k, l
    DOUBLE PRECISION,DIMENSION(NC) :: gamm
    DOUBLE PRECISION,DIMENSION(NC) :: X
    DOUBLE PRECISION,DIMENSION(1:NC,1:NC) :: TAU, G
    DOUBLE PRECISION :: A, B, CC, D, E, H, TEMP





    !Initialisation
    TAU=0
    G=0
    GAMM=0

    ! calcul des tau et G

    do i=1,nc
      do j=1,nc
        TAU(i,j) = C(i,j)/(TEMP*(1.989))
      enddo
    enddo

    do i=1,nc
      do j=1,nc
        G(i,j) = exp(-ALPHA(i,j)*TAU(i,j))
      enddo
    enddo

  ! calcul des termes du gamma

  do k=1,nc
    A=0
    B=0
    do l=1,nc
      A=A + TAU(l,k)*G(l,k)*X(l)
      B = B + G(l,k)*X(l)
    enddo

    H=0

    do j=1,nc
      CC=0
      D=0
      E=0
      do l=1,nc
        CC = CC + G(l,j)*X(l)
        D = D + TAU(l,j)*G(l,j)*X(l)
        E = E + G(l,j)*X(l)
      enddo

      H = H + ((X(j)*G(k,j))/CC)*(TAU(k,j)-(D/E))

    enddo

    gamm(k) = exp((A/B)+H)

  enddo







endsubroutine
