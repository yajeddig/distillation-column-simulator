SUBROUTINE JACOBIEN_MRSL01(X,FX,JACOB,TAILLE)
  USE MODULE_DIMENSION
  USE MODULE_INITIALISATION
  USE MODULE_OPERATOIRE
  USE MODULE_THERMODYNAMIC
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: TAILLE
  DOUBLE PRECISION, INTENT(IN), DIMENSION(1:TAILLE) :: X
  DOUBLE PRECISION, INTENT(OUT), DIMENSION(1:TAILLE) :: FX
  DOUBLE PRECISION, INTENT(OUT), DIMENSION(1:TAILLE,1:TAILLE) :: JACOB
  
  INTEGER :: I, J
  DOUBLE PRECISION :: DX, XI_ORIG, DX_INV
  DOUBLE PRECISION, PARAMETER :: DX_REL = 1.D-6  ! Smaller perturbation for accuracy
  DOUBLE PRECISION, PARAMETER :: DX_ABS = 1.D-10 ! Absolute perturbation for zero values
  DOUBLE PRECISION, DIMENSION(TAILLE) :: XX, FXX  ! Automatic arrays (stack)

  ! Initialize Jacobian to zero
  JACOB = 0.D0
  
  ! Compute base residual
  CALL RESIDU(X, FX)
  
  ! Copy X to working array
  XX = X

  ! Compute Jacobian by forward finite differences
  DO I = 1, TAILLE
      XI_ORIG = X(I)
      
      ! Adaptive step size: relative for non-zero, absolute for zero
      IF (ABS(XI_ORIG) > DX_ABS) THEN
          DX = DX_REL * XI_ORIG
      ELSE
          DX = DX_ABS
      END IF
      DX_INV = 1.D0 / DX
      
      ! Perturb
      XX(I) = XI_ORIG + DX
      
      ! Compute perturbed residual
      CALL RESIDU(XX, FXX)
      
      ! Fill column I of Jacobian
      DO J = 1, TAILLE
          JACOB(J, I) = (FXX(J) - FX(J)) * DX_INV
      END DO
      
      ! Restore
      XX(I) = XI_ORIG
  END DO

END SUBROUTINE
