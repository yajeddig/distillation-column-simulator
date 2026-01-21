SUBROUTINE METHOD_BLOCK_MRSL21(X,FX,AA,BB,CC)
  USE MODULE_DIMENSION
  USE MODULE_INITIALISATION
  USE MODULE_OPERATOIRE
  USE MODULE_THERMODYNAMIC
  IMPLICIT NONE

  DOUBLE PRECISION :: D
  DOUBLE PRECISION, DIMENSION(1:N*(2*NC+3)) :: X,FX
  DOUBLE PRECISION, DIMENSION(1:(2*NC+3),1:(2*NC+3),1:N) :: AA,BB,CC
  DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: XX,FXX
  INTEGER :: I,J,K

  ALLOCATE(XX(1:N*(2*NC+3)),FXX(1:N*(2*NC+3)))

  XX=0.
  FXX=0.
  AA=0.
  BB=0.
  CC=0.
  D=1.D-3

  CALL RESIDU(X,FX)
  XX=X
  FXX=0.


  ! DO I=1,N
  !   DO J=1,2*NC+3
  !     XX((2*NC+3)*(I-1)+J) = X((2*NC+3)*(I-1)+J)*(1.+D)
  !     CALL RESIDU(XX,FXX)
  !     DO K=1,2*NC+3
  !       IF(DABS(X((2*NC+3)*(I-1)+J))<1.D-12)THEN
  !         BB(K,J,I) = (FXX((2*NC+3)*(I-1)+K)-FX((2*NC+3)*(I-1)+K))/D
  !       ELSE
  !         BB(K,J,I) = (FXX((2*NC+3)*(I-1)+K)-FX((2*NC+3)*(I-1)+K))/(X((2*NC+3)*(I-1)+J)*D)
  !       ENDIF
  !     ENDDO
  !     XX((2*NC+3)*(I-1)+J) = X((2*NC+3)*(I-1)+J)
  !   ENDDO
  ! ENDDO
  !
  !
  ! DO I=2,N
  !   DO J=1,2*NC+3
  !     XX((2*NC+3)*(I-2)+J) = X((2*NC+3)*(I-2)+J)*(1.+D)
  !     CALL RESIDU(XX,FXX)
  !     DO K=1,2*NC+3
  !       IF(DABS(X((2*NC+3)*(I-2)+J))<1.D-12)THEN
  !         AA(K,J,I) = (FXX((2*NC+3)*(I-1)+K)-FX((2*NC+3)*(I-1)+K))/D
  !       ELSE
  !         AA(K,J,I) = (FXX((2*NC+3)*(I-1)+K)-FX((2*NC+3)*(I-1)+K))/(X((2*NC+3)*(I-2)+J)*D)
  !       ENDIF
  !     ENDDO
  !     XX((2*NC+3)*(I-2)+J) = X((2*NC+3)*(I-2)+J)
  !   ENDDO
  ! ENDDO

  !
  ! DO I=1,N-1
  !   DO J=1,2*NC+3
  !     XX((2*NC+3)*I+J) = X((2*NC+3)*I+J)*(1.+D)
  !     CALL RESIDU(XX,FXX)
  !     DO K=1,2*NC+3
  !       IF(DABS(X((2*NC+3)*I+J))<1.D-12)THEN
  !         CC(K,J,I) = (FXX((2*NC+3)*(I-1)+K)-FX((2*NC+3)*(I-1)+K))/D
  !       ELSE
  !         CC(K,J,I) = (FXX((2*NC+3)*(I-1)+K)-FX((2*NC+3)*(I-1)+K))/(X((2*NC+3)*I+J)*D)
  !       ENDIF
  !     ENDDO
  !     XX((2*NC+3)*I+J) = X((2*NC+3)*I+J)
  !   ENDDO
  ! ENDDO
  !
  ! OPEN(UNIT=21,FILE='AA1.csv',STATUS='unknown')
  ! DO K=1,N
  !   DO I=1,2*NC+3
  !     WRITE(21,*)(CC(I,J,K),J=1,2*NC+3)
  !   ENDDO
  !   WRITE(21,*)'::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::'
  ! ENDDO
  ! CLOSE(21)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  DO I=1,N
    DO J=1,2*NC+3

      XX((2*NC+3)*(I-1)+J) = X((2*NC+3)*(I-1)+J)*(1.+D)
      CALL RESIDU(XX,FXX)

      DO K=1,2*NC+3
        IF(DABS(X((2*NC+3)*(I-1)+J))<1.D-12)THEN
          BB(K,J,I) = (FXX((2*NC+3)*(I-1)+K)-FX((2*NC+3)*(I-1)+K))/D
        ELSE
          BB(K,J,I) = (FXX((2*NC+3)*(I-1)+K)-FX((2*NC+3)*(I-1)+K))/(X((2*NC+3)*(I-1)+J)*D)
        ENDIF
      ENDDO

      DO K=1,2*NC+3
        IF(DABS(X((2*NC+3)*(I-1)+J))<1.D-12 .AND. I/=N)THEN
          AA(K,J,I+1) = (FXX((2*NC+3)*(I)+K)-FX((2*NC+3)*(I)+K))/D
        ELSEIF(DABS(X((2*NC+3)*(I-1)+J))>1.D-12 .AND. I/=N)THEN
          AA(K,J,I+1) = (FXX((2*NC+3)*(I)+K)-FX((2*NC+3)*(I)+K))/(X((2*NC+3)*(I-1)+J)*D)
        ENDIF
      ENDDO

      DO K=1,2*NC+3
        IF(DABS(X((2*NC+3)*(I-1)+J))<1.D-12 .AND. I/=1)THEN
          CC(K,J,I-1) = (FXX((2*NC+3)*(I-2)+K)-FX((2*NC+3)*(I-2)+K))/D
        ELSEIF(DABS(X((2*NC+3)*(I-1)+J))>1.D-12 .AND. I/=1)THEN
          CC(K,J,I-1) = (FXX((2*NC+3)*(I-2)+K)-FX((2*NC+3)*(I-2)+K))/(X((2*NC+3)*(I-1)+J)*D)
        ENDIF
      ENDDO
      XX((2*NC+3)*(I-1)+J) = X((2*NC+3)*(I-1)+J)
    ENDDO
  ENDDO

  ! OPEN(UNIT=21,FILE='AA2.csv',STATUS='unknown')
  ! DO K=1,N
  !   DO I=1,2*NC+3
  !     WRITE(21,*)(CC(I,J,K),J=1,2*NC+3)
  !   ENDDO
  !   WRITE(21,*)'::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::'
  ! ENDDO
  ! CLOSE(21)
  ! STOP


    DEALLOCATE(XX,FXX)
ENDSUBROUTINE
