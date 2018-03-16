************************
      SUBROUTINE WRTGSFL  (ERROR)
************************
*     write input data, sum of fitted gaussians, and individual
*     gaussians to a disk file
*
*     CMDP command parameters used:
*     1 = command mnemonic GW
*     2 = output file name for data and Gaussian profiles
*
*     other subroutines called:
*     getdate
*
      IMPLICIT  NONE
*
      INTEGER    MAXLINESL
      PARAMETER (MAXLINESL = 30)
*
      LOGICAL   EXISTS                  ! local file existence
      INTEGER   ERROR                   ! output error status
      INTEGER   I                       ! local loop index
      INTEGER   K                       ! local loop index
      INTEGER   L                       ! local loop index
      INTEGER   NCHAR                   ! external function
      REAL*8    GP(MAXLINESL)           ! local gaussian profiles at each X
      REAL*8    SIGMAGAUSS              ! local sum of gaussians
*
      INCLUDE 'lines.inc'
*
*     write fit data to disc file, append to end if exists
*     first see if output file is already open
*
      IF (DB) PRINT *,'in WRTGSFL'
      ERROR = 0
      IF (MEMGF .LT. 1) THEN
          ERROR = 1
          PRINT *,'no Gaussian fit done'
          GO TO 900
      END IF
*
      WRITE (GWWRTFILE, '(A)') 'gausfile.out'
      IF (NCMD .GE. 2) THEN
          GWWRTFILE = CMDP(2) 
      END IF
*
      INQUIRE (FILE=GWWRTFILE, EXIST=EXISTS)
      IF (EXISTS) THEN
          OPEN (GWWRTUNIT,FILE=GWWRTFILE,STATUS='OLD',ERR=650)
  650     DO  I = 1, 327670
              READ (GWWRTUNIT,'(A)',END=660) BUF
          END DO
*         (JQ) backspace over EOF marker so write appends correctly
  660     BACKSPACE (GWWRTUNIT)
          WRITE (GWWRTUNIT,'(/A)')
     &    '----------------------'
      ELSE
          OPEN (GWWRTUNIT,FILE=GWWRTFILE,STATUS='NEW')
      END IF
*
*     get the date and time in FITSDATE
      CALL GETDATE
*
      WRITE (GWWRTUNIT,*)
     &'Gaussians fitted to ',OBJECT(MEMGF)(1:20),
     &' on ',FITSDATE
*
      WRITE (GWWRTUNIT,1010) 'Xin,','Yin,','SigmaGauss',
     &('Gauss',I,I=1,NPAR/3)
*
      DO I = 1, NP, 1
          SIGMAGAUSS = P(NPAR+1)      ! baseline
          L = 0
          DO K = 1, NPAR, 3
*             compute each component Gaussian and their sum
              L = L + 1
              GP(L) = P(K+1)*EXP(((X(I)-P(K)) / P(K+2))**2 * (-2.7726))
              SIGMAGAUSS = SIGMAGAUSS + GP(L)
              GP(L) = P(NPAR+1) + GP(L)
          END DO
          WRITE (GWWRTUNIT,1020) X(I), Y(I), SIGMAGAUSS, 
     &    (GP(L),L=1,NPAR/3)
      END DO
      WRITE (GWWRTUNIT,'(A)') '  '   ! blank line for safety
      CLOSE (GWWRTUNIT)
*
      WRITE (BUF,*)'Data and fitted Gaussians written to ',
     &    GWWRTFILE(1:NCHAR(GWWRTFILE))
      PRINT *,BUF(1:NCHAR(BUF))
      IF (WRITELOG) WRITE (LOGWRTUNIT,*) BUF(1:NCHAR(BUF))
*
  900 RETURN
*
 1010 FORMAT (2(A14,1X),A13,100(',',7X,A5,I2.2))
 1020 FORMAT (100(E13.7,',',1X))      
      END
************************
