PROGRAM write_srv
USE netcdf
IMPLICIT NONE

INTEGER, PARAMETER :: NX=1024, NY=1024, NT=144 ! dimension
REAL, DIMENSION(NX,NY) :: RR ! rain rate
INTEGER, DIMENSION(8) :: srv_header ! header foe SERVICE
INTEGER :: i,e ! counter
CHARACTER(255) :: path="/data3/mog/taiwanvvm" ! path of the outputs
CHARACTER(255), DIMENSION(30) :: expt ! name of experiment
CHARACTER(255), DIMENSION(NT+1) :: fname ! name of netCDF file
INTEGER :: status ! status for netCDF file
INTEGER :: ncID ! ID for netCDF file
INTEGER :: varID ! ID for variable

OPEN(20,FILE="irt_objects_input_00.srv",FORM='unformatted', ACTION='write')

expt=(/"tpe20050712","tpe20050723","tpe20060508","tpe20060718","tpe20060721", &
       "tpe20070830","tpe20080715","tpe20090707","tpe20090817","tpe20090827", &
       "tpe20100629","tpe20100630","tpe20100802","tpe20100803","tpe20100912", &
       "tpe20110615","tpe20110616","tpe20110702","tpe20110723","tpe20110802", &
       "tpe20110816","tpe20110821","tpe20120715","tpe20120819","tpe20130723", &
       "tpe20130807","tpe20130825","tpe20140525","tpe20140703","tpe20140825"/)
e=1

DO i=0,NT
   
   RR = 0.

   !!! Read netCDF of Surface Parameters !!!
   ! filename
   WRITE(fname(i+1),100) TRIM(path),"/",TRIM(expt(e)),"cln/archive/exp.C.Surface-",i,".nc"
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
   srv_header(1) = 1             ! Code
   srv_header(2) = 1             ! Level
   srv_header(3) = 20190101      ! Datum
   srv_header(4) = i             ! Zeitinkrement
   srv_header(5) = NX
   srv_header(6) = NY
   srv_header(7) = 0
   srv_header(8) = 0

   !!! Write SERVICE !!!
   WRITE(20) srv_header
   WRITE(20) RR

ENDDO
CLOSE(20)

END PROGRAM write_srv

