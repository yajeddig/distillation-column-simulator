SUBROUTINE RESIDU(X,FX)
  USE MODULE_DIMENSION
  USE MODULE_INITIALISATION
  USE MODULE_OPERATOIRE
  USE MODULE_THERMODYNAMIC
  IMPLICIT NONE
  INTEGER :: I,J,M
  DOUBLE PRECISION :: T
  DOUBLE PRECISION, DIMENSION(1:(2*NC+3)*N) :: X,FX
  DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: GAMM,PSAT,K,HL,HV,HF,YY,XX,ZZ

  IBMT=1
  IBMP=IBMT+1
  IBE=IBMP+NC
  IEQ=IBE+1
  IST=IEQ+NC



  ALLOCATE(PSAT(NC),GAMM(NC),K(NC),HV(N),HL(N),HF(N),YY(NC),XX(NC),ZZ(NC))
  GAMM(:)=0
  PSAT(:)=0
  K(:)=0
  HL(:)=0
  HV(:)=0
  HF(:)=0
  YY(:)=0
  XX(:)=0
  ZZ(:)=0
  FX=0.
  T=320.




  DO I=1,N
    DO J=1,NC
      XX(J)=X(ICL+(2*NC+3)*(I-1)+(J-1))
      YY(J)=X(ICV+(2*NC+3)*(I-1)+(J-1))
    ENDDO
    CALL mHV(YY,HV(I),X(IT+(2*NC+3)*(I-1)))
    CALL mHL(XX,X(IT+(2*NC+3)*(I-1)),HL(I))
        IF(F(I)==0)THEN
          HF(I)=0.
        ELSE
          ZZ(:)=Z(:,I)
          CALL CALCUL_TBULLE(ZZ,T)
          CALL mHL(ZZ,T,HF(I))
        ENDIF
  ENDDO




  !CONDENSEUR
  DO I=1,NC
    YY(I)=X(ICL+I-1)
  ENDDO
  CALL NRTL(GAMM,YY,X(IT))
  CALL CALCULPSAT(X(IT),PSAT)
  CALL EQUILIBRE(K,GAMM,PSAT)

  FX(IBMT) =  F(1)-X(IDV)+X(IDV+2*NC+3)-(X(IDL)+DL(1))

  FX(IBE)  =  -(X(IDL)+DL(1))*HL(1)   &
              -X(IDV)*HV(1)           &
              +X(IDV+2*NC+3)*HV(2)    &
              +F(1)*HF(1) - Q_COL(1)*4.184



  DO I=1,NC
    FX(IBMP+I-1)  = -(X(IDL)+DL(1))*X(ICL+I-1)        &
                    - X(IDV)*X(ICV+I-1)               &
                    + X(IDV+2*NC+3)*X(ICV+2*NC+3+I-1) &
                    + F(1)*Z(I,1)

    FX(IEQ+I-1)   = X(ICV+I-1) - K(I)*X(ICL+I-1)

    FX(IST)       = FX(IST) + X(ICV+I-1) - X(ICL+I-1)
  ENDDO


  !BOUILLEUR
  DO I=1,NC
    YY(I)=X(ICL+(N-1)*(2*NC+3)+I-1)
  ENDDO
  CALL NRTL(GAMM,YY,X(IT+(2*NC+3)*(N-1)))
  CALL CALCULPSAT(X(IT+(2*NC+3)*(N-1)),PSAT)
  CALL EQUILIBRE(K,GAMM,PSAT)

  FX(IBMT+(2*NC+3)*(N-1)) =  + X(IDL+(2*NC+3)*(N-2))           &
                             - X(IDL+(2*NC+3)*(N-1))           &
                             - (X(IDV+(2*NC+3)*(N-1))+ DV(N))  &
                             + F(N)

  FX(IBE+(2*NC+3)*(N-1))=  + X(IDL+(2*NC+3)*(N-2))*HL(N-1)        &
                            - X(IDL+(2*NC+3)*(N-1))*HL(N)         &
                            - (X(IDV+(2*NC+3)*(N-1))+DV(N))*HV(N) &
                            + F(N)*HF(N) + Q_COL(N)*4.184

  DO I=1,NC
    FX(IBMP+(2*NC+3)*(N-1)+(i-1))=  + X(IDL+(2*NC+3)*(N-2))*X(ICL+(2*NC+3)*(N-2)+(i-1))          &
                                    - (X(IDV+(2*NC+3)*(N-1))+DV(N))*X(ICV+(2*NC+3)*(N-1)+(i-1))  &
                                    - X(IDL+(2*NC+3)*(N-1))*X(ICL+(2*NC+3)*(N-1)+(i-1))          &
                                    + F(N)* Z(I,N)

    FX(IEQ+(2*NC+3)*(N-1)+I-1) = X(ICV+(2*NC+3)*(N-1)+I-1) - K(I)*X(ICL+(2*NC+3)*(N-1)+I-1)

    FX(IST+(2*NC+3)*(N-1)) =  + FX(IST+(2*NC+3)*(N-1))    &
                              + X(ICL+(2*NC+3)*(N-1)+I-1) &
                              - X(ICV+(2*NC+3)*(N-1)+I-1)
  ENDDO


  !TOUS LES AUTRES PLATEAUX

  DO I=2,N-1

    DO J=1,NC
      YY(J)=X(ICL+(I-1)*(2*NC+3)+J-1)
    ENDDO

    CALL NRTL(GAMM,YY,X(IT+(2*NC+3)*(I-1)))
    CALL CALCULPSAT(X(IT+(2*NC+3)*(I-1)),PSAT)
    CALL EQUILIBRE(K,GAMM,PSAT)

    FX(IBMT+(2*NC+3)*(I-1)) =  + X(IDL+(2*NC+3)*(I-2))           &
                               - (X(IDL+(2*NC+3)*(I-1))+DL(I))   &
                               - (X(IDV+(2*NC+3)*(I-1))+DV(I))   &
                               + X(IDV+(2*NC+3)*I)+F(I)

    FX(IBE+(2*NC+3)*(I-1))=    + X(IDL+(2*NC+3)*(I-2))*HL(I-1)        &
                               - (X(IDL+(2*NC+3)*(I-1))+DL(I))*HL(I)  &
                               - (X(IDV+(2*NC+3)*(I-1))+DV(I))*HV(I)  &
                               + X(IDV+(2*NC+3)*I)*HV(I+1)            &
                               + F(I)* HF(I) - Q_COL(I)*4.184

          DO J=1,NC
            FX(IBMP+(2*NC+3)*(I-1)+J-1)= + X(IDL+(2*NC+3)*(I-2))*X(ICL+(2*NC+3)*(I-2)+(J-1))          &
                                         - (X(IDV+(2*NC+3)*(I-1))+DV(I))*X(ICV+(2*NC+3)*(I-1)+(J-1))  &
                                         - (X(IDL+(2*NC+3)*(I-1))+DL(I))*X(ICL+(2*NC+3)*(I-1)+(J-1))  &
                                         + X(IDV+(2*NC+3)*I)*X(ICV+(2*NC+3)*I+(J-1))+ F(I)* Z(J,I)

            FX(IEQ+(2*NC+3)*(I-1)+J-1) = X(ICV+(2*NC+3)*(I-1)+J-1) - K(J)*X(ICL+(2*NC+3)*(I-1)+J-1)

            FX(IST+(2*NC+3)*(I-1)) =   + FX(IST+(2*NC+3)*(I-1))     &
                                       + X(ICL+(2*NC+3)*(I-1)+J-1)  &
                                       - X(ICV+(2*NC+3)*(I-1)+J-1)
          ENDDO
  ENDDO


  DEALLOCATE(PSAT,GAMM,K,HV,HL,HF,XX,YY,ZZ)

ENDSUBROUTINE
