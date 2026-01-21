PROGRAM TEST
USE MODULE_DIMENSION
USE MODULE_INITIALISATION
USE MODULE_OPERATOIRE
USE MODULE_THERMODYNAMIC
IMPLICIT NONE

DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: X,FX,JACK
DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE :: JACOB
DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE ::AA,BB,CC,AAA,BBB,CCC
DOUBLE PRECISION :: NORME,EPSILON,RELAXATION,T1,T2
INTEGER :: I,J,TAILLE,IER,IRAZ,IMET,INDIC,K,ISTATE


IER=0
IRAZ=1
IMET=0
RELAXATION=0.01
EPSILON=1D-6


CALL DIMENSION()
TAILLE=N*(2*NC+3)
ALLOCATE(X(1:TAILLE),FX(1:TAILLE),JACOB(1:TAILLE,1:TAILLE),JACK(1:TAILLE))
ALLOCATE(AA(1:(2*NC+3),1:(2*NC+3),1:N),BB(1:(2*NC+3),1:(2*NC+3),1:N),CC(1:(2*NC+3),1:(2*NC+3),1:N))
ALLOCATE(AAA(1:(2*NC+3),1:(2*NC+3),1:N),BBB(1:(2*NC+3),1:(2*NC+3),1:N),CCC(1:(2*NC+3),1:(2*NC+3),1:N))

CALL OPERATOIRE()
CALL THERMODYNAMICS()
CALL INITIALISATION(X)
CALL RESIDU(X,FX)
CALL NORME_VECTEUR(FX,TAILLE,NORME)
I=0
ISTATE=0
DO WHILE (ISTATE/=1 .AND. ISTATE/=2)
    PRINT*, 'TAPEZ 1 POUR MRSL01 (JACOBIENNE NON ORDONNÉE)'
    PRINT*, 'TAPEZ 2 POUR MRSL21 (JACOBIENNE TRIDIAGONNALE PAR BLOC)'
    READ(*,*)ISTATE
    IF(ISTATE/=1 .AND. ISTATE/=2) PRINT*, 'FAITES CELA CORRECTEMENT!!'
ENDDO

                            CALL CPU_TIME(T1)  !un chrono pour le temps de calcul

                              DO WHILE(DABS(NORME)>=EPSILON)
                                IF(ISTATE==2)THEN
                                    CALL METHOD_BLOCK_MRSL21(X,FX,AA,BB,CC)
                                    FX=-FX
                                    CALL MRSL21(AA,BB,CC,FX,2*NC+3,N,1,2*NC+3,2*NC+3,IER,IRAZ,IMET)
                                          IF(IER==1)THEN
                                            PRINT*,'ERREUR RESOLUTION MRSL21 : IER=',IER
                                            stop "message"
                                          ENDIF
                                    X=X+RELAXATION*FX
                                ELSE
                                    CALL JACOBIEN_MRSL01(X,FX,JACOB,TAILLE)
                                    FX=-FX
                                    CALL  MRSL01(JACOB,FX,TAILLE,TAILLE,IER,INDIC)
                                    X=X+RELAXATION*FX
                                ENDIF
                                CALL RESIDU(X,FX)
                                CALL NORME_VECTEUR(FX,TAILLE,NORME)
                                I=I+1
                                PRINT*, '________________________________________________________________________________'
                                PRINT*, '________________________________________________________________________________'
                              ENDDO

                              CALL CPU_TIME(T2)  !un chrono pour le temps de calcul

PRINT*,'FIN DE LA RESOLUTION'
PRINT*,'NOMBRE ITERATION:', I
PRINT*,'NORME RESIDU   FINAL :',NORME
IF(ISTATE==1)THEN
  PRINT*,'MRSL01'
ELSE
  PRINT*,'MRSL21'
ENDIF
PRINT*,'IER=',IER
PRINT*, 'TEMPS DE CALCUL',T2-T1,'SECONDES'


OPEN(UNIT=35,FILE='RESULTS.csv',STATUS='unknown')
WRITE(35,*)'ÉTAGE PLATEAU',';','DEBIT VAPEUR',';','DEBIT LIQUIDE',&
            ';','COMPO VAPEUR 1',';','COMPO VAPEUR 2',';','COMPO VAPEUR 3',';',&
            'COMPO LIQUIDE 1',';','COMPO LIQUIDE 2',';','COMPO LIQUIDE 3',';','TEMPERATURE'
DO I=1,N
  WRITE(35,*)I,';',X(IDV+(2*NC+3)*(I-1)),';',X(IDL+(2*NC+3)*(I-1))&
  ,(';',X(ICV+(2*NC+3)*(I-1)+J-1),J=1,NC),(';',X(ICL+(2*NC+3)*(I-1)+J-1),J=1,NC),';',X(IT+(2*NC+3)*(I-1))
ENDDO
CLOSE(35)


END PROGRAM
