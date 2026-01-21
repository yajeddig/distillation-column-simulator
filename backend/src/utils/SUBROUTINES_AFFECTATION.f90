SUBROUTINE DIMENSION
  USE MODULE_DIMENSION
  IMPLICIT NONE

  OPEN(UNIT=33,FILE='OPERATOIRE.txt')
  READ(33,*) N
  READ(33,*)
  READ(33,*) NB_ALIM
  CLOSE(33)

  OPEN(UNIT=33,FILE='ALIMENTATION_PARAMETRE.txt')
  READ(33,*) NC
  CLOSE(33)

ENDSUBROUTINE

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! CONDITIONS OPERATOIRE DE LA COLONNE
SUBROUTINE OPERATOIRE
  USE MODULE_DIMENSION
  USE MODULE_OPERATOIRE
  USE MODULE_INITIALISATION
  IMPLICIT NONE
  INTEGER :: I,J

  ALLOCATE(F(1:N),Z(1:NC,1:N),DV(1:N),DL(1:N))

  F=0
  Z=0
  DV=0
  DL=0

  OPEN(UNIT=33,FILE='OPERATOIRE.txt')
  READ(33,*)N
  READ(33,*)
  READ(33,*)NB_ALIM
  ALLOCATE(ETAGE_ALIM(1:NB_ALIM))
  READ(33,*)
  READ(33,*)(ETAGE_ALIM(I),I=1,NB_ALIM)
  READ(33,*)
  READ(33,*)(F(ETAGE_ALIM(I)),I=1,NB_ALIM)
  READ(33,*)
  DO I=1,NB_ALIM
    READ(33,*)(Z(J,ETAGE_ALIM(I)),J=1,NC)
  ENDDO
  READ(33,*)
  READ(33,*)
  READ(33,*)QBOUILLEUR
  READ(33,*)QCONDENS
  READ(33,*)
  READ(33,*)PRESSION
  READ(33,*)
  READ(33,*)NB_DL
  ALLOCATE(ETAGE_DL(1:NB_DL))
  READ(33,*)
  IF (NB_DL>0) THEN
    READ(33,*)(ETAGE_DL(I),I=1,NB_DL)
    READ(33,*)
    READ(33,*)(DL(ETAGE_DL(I)),I=1,NB_DL)
  ELSE
    DL(:)=0.
    READ(33,*)
    READ(33,*)
    READ(33,*)
  ENDIF
  READ(33,*)
  READ(33,*)NB_DV
  ALLOCATE(ETAGE_DV(1:NB_DV))
  READ(33,*)
  IF (NB_DV>0) THEN
    READ(33,*)(ETAGE_DV(I),I=1,NB_DV)
    READ(33,*)
    READ(33,*)(DV(ETAGE_DV(I)),I=1,NB_DV)
  ELSE
    DV(:)=0.
    READ(33,*)
    READ(33,*)
    READ(33,*)
  ENDIF
  CLOSE(33)

  PRESSION = PRESSION*101325.



ENDSUBROUTINE

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE THERMODYNAMICS
  USE MODULE_DIMENSION
  USE MODULE_OPERATOIRE
  USE MODULE_THERMODYNAMIC

  IMPLICIT NONE
  INTEGER :: I,J

  ALLOCATE(TEB(1:NC),HVAPO(1:NC),CPVAP(1:NC),CPLIQ(1:NC),COEFPSAT(1:5,1:NC),ALPHA(1:NC,1:NC),C(1:NC,1:NC))

  OPEN(UNIT=33,FILE='ALIMENTATION_PARAMETRE.txt')
  READ(33,*)
  READ(33,*)
  DO I=1,NC
    READ(33,*)TEB(I)
  ENDDO
  READ(33,*)
  DO I=1,NC
    READ(33,*)HVAPO(I)
  ENDDO
  READ(33,*)
  DO I=1,NC
    READ(33,*)CPVAP(I)
  ENDDO
  READ(33,*)
  DO I=1,NC
    READ(33,*)CPLIQ(I)
  ENDDO
  READ(33,*)
  READ(33,*)
  DO I=1,5
      READ(33,*) (COEFPSAT(I,J),J=1,NC)
  ENDDO
  READ(33,*)
  READ(33,*)
  DO I=1,NC
      READ(33,*) (C(I,J),J=1,NC)
  ENDDO
  READ(33,*)
  READ(33,*)
  DO I=1,NC
      READ(33,*) (ALPHA(I,J),J=1,NC)
  ENDDO
  CLOSE(33)

  TREF = 298.15
ENDSUBROUTINE

SUBROUTINE INITIALISATION(X)
  USE MODULE_DIMENSION
  USE MODULE_OPERATOIRE
  USE MODULE_INITIALISATION
  IMPLICIT NONE
  INTEGER :: I,J
  DOUBLE PRECISION :: H1,H2,H3,H4,A1,A2,A3,T,HF,FZA
  DOUBLE PRECISION, DIMENSION(1:NC) :: GAMM,PSAT,K,YY
  DOUBLE PRECISION, DIMENSION(1:N*(2*NC+3)) :: X


  ALLOCATE(XPIED(1:NC),YPIED(1:NC),XTETE(1:NC),YTETE(1:NC),Q_COL(1:N))
  TBULLE=320.   !INITIALISATION POUR METHODE DE NEWTON
  TROSEE=TBULLE
  Q_COL=0
  Q_COL(1) = QCONDENS
  Q_COL(N) = QBOUILLEUR
  XTETE = Z(:,minval(ETAGE_ALIM, dim=1, mask=(ETAGE_ALIM>0)))
  CALL CALCUL_TBULLE(XTETE,TBULLE)
  CALL NRTL(gamm,XTETE,TBULLE)
  CALL CALCULPSAT(TBULLE,PSAT)
  CALL EQUILIBRE(K,gamm,PSAT)
  DO I=1,NC
    YTETE(I)=K(I)*XTETE(I)
  ENDDO
  PRINT*,'————————————————————————————————————————'
  PRINT*, 'LA COMPOSITION INITIALE EN TETE EST :'
  PRINT*, 'VAPEUR : ', (YTETE(I),I=1,NC)
  PRINT*, 'LIQUIDE', (XTETE(I),I=1,NC)
  PRINT*, 'TEMPERATURE DE BULLE', TBULLE
  PRINT*,'————————————————————————————————————————'

  YPIED=Z(:,maxval(ETAGE_ALIM, dim=1, mask=(ETAGE_ALIM>0)))
  CALL CALCUL_TROSEE(YPIED,TROSEE,XPIED)
  PRINT*,'————————————————————————————————————————'
  PRINT*, 'LA COMPOSITION INITIALE EN PIED EST :'
  PRINT*, 'VAPEUR : ', (YPIED(I),I=1,NC)
  PRINT*, 'LIQUIDE', (XPIED(I),I=1,NC)
  PRINT*, 'TEMPERATURE DE ROSÉE', TROSEE
  PRINT*,'————————————————————————————————————————'

  IDV=1
  ICV=IDV+1
  IT=ICV+NC
  ICL=IT+1
  IDL=ICL+NC

   X(:)=0

  DO I=1,N
    X(IT+(2*NC+3)*(I-1))=TBULLE + (TROSEE - TBULLE)/(N-1)*(I-1)
    DO J=1,NC
      X(ICV+(2*NC+3)*(I-1)+J-1)=YTETE(J) + (YPIED(J)-YTETE(J))*(I-1)/(N-1)
      X(ICL+(2*NC+3)*(I-1)+J-1)=XTETE(J) + (XPIED(J)-XTETE(J))*(I-1)/(N-1)
    ENDDO
  ENDDO

  FZA=0
  DO I=1,N
    FZA=FZA+F(I)*Z(1,I)
  ENDDO
    X(IDV) = ( FZA - sum(F)*X(ICL+(2*NC+3)*(N-1))  )  /(X(ICV)-X(ICL+(2*NC+3)*(N-1)))

    X(IDL+(2*NC+3)*(N-1)) = sum(F) - X(IDV)


    CALL mHV(X(ICV),H1,X(IT))
    CALL mHL(X(ICL),X(IT),H3)

    X(IDL) = Q_COL(1)*4.184/(H1-H3)

    H1=0
    DO I=2,N-1
      IF(F(I)/=0) H1=H1+F(I)
      X(IDL+(2*NC+3)*(I-1)) = X(IDL)+H1
    ENDDO


    DO I=2,N
      X(IDV+(2*NC+3)*(I-1)) = X(IDV)+X(IDL)
    ENDDO





ENDSUBROUTINE
