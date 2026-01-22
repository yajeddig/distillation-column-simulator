SUBROUTINE NRTL(gamm, X, TEMP)
  USE MODULE_DIMENSION
  USE MODULE_INITIALISATION
  USE MODULE_OPERATOIRE
  USE MODULE_THERMODYNAMIC
  IMPLICIT NONE

  DOUBLE PRECISION, INTENT(OUT), DIMENSION(NC) :: gamm
  DOUBLE PRECISION, INTENT(IN), DIMENSION(NC) :: X
  DOUBLE PRECISION, INTENT(IN) :: TEMP
  
  ! Local variables - automatic arrays on stack (faster than allocatable)
  DOUBLE PRECISION, DIMENSION(NC, NC) :: TAU, G
  DOUBLE PRECISION, DIMENSION(NC) :: sum_GX, sum_tauGX  ! Pre-computed sums
  DOUBLE PRECISION :: RT_inv, A, B, H, CC_sum, D_sum, term
  INTEGER :: i, j, k, l

  ! Pre-compute 1/(R*T) - avoid repeated division
  RT_inv = 1.D0 / (TEMP * 1.989D0)

  ! Compute TAU and G matrices
  DO j = 1, NC
    DO i = 1, NC
      TAU(i, j) = C(i, j) * RT_inv
      G(i, j) = EXP(-ALPHA(i, j) * TAU(i, j))
    END DO
  END DO

  ! Pre-compute common sums: sum_j(G_lj * X_l) and sum_j(tau_lj * G_lj * X_l)
  DO j = 1, NC
    sum_GX(j) = 0.D0
    sum_tauGX(j) = 0.D0
    DO l = 1, NC
      term = G(l, j) * X(l)
      sum_GX(j) = sum_GX(j) + term
      sum_tauGX(j) = sum_tauGX(j) + TAU(l, j) * term
    END DO
  END DO

  ! Compute activity coefficients
  DO k = 1, NC
    ! First term: sum(tau_lk * G_lk * X_l) / sum(G_lk * X_l)
    A = sum_tauGX(k)
    B = sum_GX(k)
    
    ! Second term: sum over j
    H = 0.D0
    DO j = 1, NC
      CC_sum = sum_GX(j)
      D_sum = sum_tauGX(j)
      H = H + (X(j) * G(k, j) / CC_sum) * (TAU(k, j) - D_sum / CC_sum)
    END DO
    
    gamm(k) = EXP(A / B + H)
  END DO

END SUBROUTINE
