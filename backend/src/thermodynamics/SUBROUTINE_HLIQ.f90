subroutine mHL(X,TEMP,HLIQ)
  USE MODULE_DIMENSION
  USE MODULE_INITIALISATION
  USE MODULE_OPERATOIRE
  USE MODULE_THERMODYNAMIC
  IMPLICIT NONE

  DOUBLE PRECISION,DIMENSION(NC) :: X
  DOUBLE PRECISION :: HLIQ,DT,TEMPDT,TEMP
  INTEGER :: i,J
  DOUBLE PRECISION,DIMENSION(:), ALLOCATABLE :: GAMM,GAMMADT,LN_GAMM,LN_GAMMADT,DLN_GAMMADT

  ALLOCATE(GAMM(1:NC),GAMMADT(1:NC),LN_GAMM(1:NC),LN_GAMMADT(1:NC),DLN_GAMMADT(1:NC))

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !CALCUL DE LA DERIVEE DE LN(GAMMA_I) PAR RAPPORT A LA TEMPERATURE PAR PERTURBATION NUMERIQUE
  ! POUR POUVOIR INTRODUIRE LES ENTHALPIES D'EXCES

  DT=0.001
  TEMPDT=TEMP+DT

  call NRTL(gamm,X,TEMP)
  call NRTL(gammaDT,X,TEMPDT)

  DO I=1,NC
    LN_GAMM(i) = log(GAMM(i))
    LN_GAMMADT(i) = log(gammaDT(i))
  enddo

  DO I=1,NC
    DLN_GAMMADT(i) = ( LN_GAMMADT(i) - LN_GAMM(i) )/DT
  enddo
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  hliq=0 ! REFERENCE GAZ À T = 298.15 K

  do I=1,NC
    HLIQ = HLIQ +X(I)*(CPVAP(I)*(TEB(I)-Tref) -HVAPO(I)+ CPLIQ(I)*(TEMP-TEB(I))) - 8.314*TEMP*TEMP*DLN_GAMMADT(i)
  enddo


  DEALLOCATE(GAMM,GAMMADT,LN_GAMM,LN_GAMMADT,DLN_GAMMADT)


endsubroutine
