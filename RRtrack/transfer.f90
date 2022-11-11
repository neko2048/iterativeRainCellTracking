PROGRAM write_srv
USE netcdf
IMPLICIT NONE

INTEGER, PARAMETER :: NX=1024, NY=1024, NT=120 ! dimension
REAL, DIMENSION(NX,NY) :: RR ! rain rate
INTEGER, DIMENSION(8) :: srv_header ! header foe SERVICE
INTEGER :: i,e ! counter
CHARACTER(255) :: path="/data3/mog/les/mjo_std_mg/" ! path of the outputs
CHARACTER(255), DIMENSION(NT+1) :: fname ! name of netCDF file
INTEGER :: status ! status for netCDF file
INTEGER :: ncID ! ID for netCDF file
INTEGER :: varID ! ID for variable

OPEN(20,FILE="irt_objects_input_00.srv",FORM='unformatted', ACTION='write')

e=1

DO i=0,NT
   
   RR = 0.

   !!! Read netCDF of Surface Parameters !!!
   ! filename
   WRITE(fname(i+1),100) TRIM(path),"/","archive/mjo_std_mg.C.Surface-",i,".nc"
   100 FORMAT(4A,I6.6,A)
   write(*,*) fname(i+1)
   ! open netCDF file
   status=NF90_OPEN(TRIM(fname(i+1)),NF90_NOWRITE,ncID)
   IF (status /= nf90_noerr) THEN
      WRITE(*,*) "open fail .nc"
      GOTO 1000
   ENDIF
   ! read data: surface precipitation rate
   status=NF90_INQ_VARID(ncID,"sprec",varID)
   IF (status /= nf90_noerr) THEN
      WRITE(*,*) "var_inq fail RR"
      GOTO 1000
   ENDIF
   status=NF90_GET_VAR(ncID,varID,RR,START=(/1,1,1/),COUNT=(/nx,ny,1/))
   IF (status /= nf90_noerr) THEN
      WRITE(*,*) "read fail RR"
      GOTO 1000
   ENDIF
   1000 CONTINUE
   ! data processing
   RR = RR * 3600    

   !!! Prepare Headers for SERVICE !!!
   !srv_header(1) = 1             ! Code
   !srv_header(2) = 1             ! Level
   !srv_header(3) = 20190101      ! Datum
   !srv_header(4) = i             ! Zeitinkrement
   !srv_header(5) = NX
   !srv_header(6) = NY
   !srv_header(7) = 0
   !srv_header(8) = 0

   !!! Write SERVICE !!!
   !WRITE(20) srv_header
   !WRITE(20) RR

ENDDO
!CLOSE(20)

END PROGRAM write_srv

