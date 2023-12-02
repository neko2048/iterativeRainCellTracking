PROGRAM assignIdxToCloud
USE netcdf
IMPLICIT NONE


! -----------------------------------------------------
! Set up Basic Params
! -----------------------------------------------------
!   File name
!CHARACTER(255) :: vvmPath="/data3/mog/les/mjo_std_mg"
CHARACTER(255) :: irtPath="/home/atmenu10246/iterativeRainCellTracking/RRtrack/mjo-cwp25e-3/"
CHARACTER(255) :: reIndexLWPPath="/home/atmeu10246/transition/dat/reindexLWP/"
CHARACTER(255) :: trackIdxPath = "/home/atmenu10246/transition/src/cloudTracking/mergeSplitIRT/"
CHARACTER(255) :: cloudObjectPath="/data3/atmenu10246/cloudLabel/cloud2LayerFilter/"
CHARACTER(255) :: qcObjectPath="/data3/atmenu10246/collocateCloud/qcCollocate2Layer/"
CHARACTER(255) :: ncOutputPath="/data3/atmenu10246/collocateCloud/collocateCloudByQc2Layer/"
CHARACTER(255) :: errorOutputPath="/home/atmenu10246/transition/src/cloudTracking/assignCloudIndex/"
!CHARACTER(255), DIMENSION(1) :: expt="mjo_std_mg" ! name of experiment
CHARACTER(255) :: fname ! name of netCDF file
!   Dimension
INTEGER, PARAMETER :: nx=1024
INTEGER, PARAMETER :: ny=1024
INTEGER, PARAMETER :: nz=120
INTEGER, PARAMETER :: nt=628
INTEGER, PARAMETER :: trackIDMax=15277 ! for 25e-3
!11521 ! # for 1e-1 
!max value for track ID ( !!! should be checked personally)
INTEGER, PARAMETER :: initTimeStep = 470, endTimeStep = 470
!   geographic coordinate
REAL, DIMENSION(nx) :: xc
REAL, DIMENSION(ny) :: yc
REAL, DIMENSION(nz) :: zc
!   ID
INTEGER :: ncID ! ID for netCDF file
INTEGER :: timeID, xcID, ycID, zcID, varID ! ID for variable
INTEGER :: timeDimID, xcDimID, ycDimID, zcDimID
!   needed data
REAL, DIMENSION(nx, ny, nz) :: cloudObject=0. ! cloud objects with meanless index
REAL, DIMENSION(nx, ny, nz) :: cloudObjectZeroTo9999=99999.99 ! cloud objects with meanless index
REAL, DIMENSION(nx, ny, nz) :: collocateCloudObject=0. ! cloud objects to output
REAL, DIMENSION(nx, ny, nz) :: qcObject=0. ! qc objects with collocated index
INTEGER, DIMENSION(trackIDMax) :: originTrackIDs=0. ! array of index of cloud objects
INTEGER, DIMENSION(trackIDMax) :: finalTrack=0. ! trackID consider merge and split
REAL, DIMENSION(64) :: inCloudIndexTemp=0., correspondTrackID=0.
!   counting index
INTEGER :: xIdx, yIdx, zIdx, timeIdx, cloudIdx, qcIdx
INTEGER :: i, inCloudIndexCount
!   other
!INTEGER :: lrec
INTEGER :: status ! status for netCDF file
INTEGER :: cloudMax, cloudMin
!   output array

! -----------------------------------------------------
! Read trackID to trackID array - originTrackID
! -----------------------------------------------------
DO i = 1, trackIDMax
   originTrackIDs(i) = i
ENDDO

! -----------------------------------------------------
! Read trackID to trackID array - finalTrack
! -----------------------------------------------------
WRITE(fname, 100) TRIM(trackIdxPath), "finalTrack-25e-3.txt"
100 FORMAT(A, A)
OPEN(10, FILE=fname, FORM="FORMATTED", STATUS="OLD")
DO i = 1, trackIDMax
    READ(10, *) finalTrack(i)
ENDDO
CLOSE(10)

! -----------------------------------------------------
! Open error log
! -----------------------------------------------------
WRITE(fname, 104) TRIM(errorOutputPath), "error", initTimeStep, endTimeStep, ".txt"
104 FORMAT(2A, I3.3, I3.3, A)
OPEN(20, FILE=fname, FORM="FORMATTED", STATUS="UNKNOWN")

! -----------------------------------------------------
! Read xc, yc, zc
! -----------------------------------------------------
WRITE(fname, 103) TRIM(qcObjectPath),"cloudLabel-",0,".nc"
103 FORMAT(2A,I6.6,A)
! open netCDF file
status=NF90_OPEN(TRIM(fname),NF90_NOWRITE,ncID)
IF (status /= nf90_noerr) THEN ! check reading success or fail
   WRITE(*, *) "open fail qcLabel.nc"
ENDIF
! read data: xc, yc, zc, Qc
status=NF90_INQ_VARID(ncID,"xc",xcID)
IF (status /= nf90_noerr) THEN
   WRITE(*, *) "var_inq fail xc"
ENDIF
status=NF90_GET_VAR(ncID,xcID,xc,START=(/1/),COUNT=(/nx/))
IF (status /= nf90_noerr) THEN
   WRITE(*, *) "read fail xc"
ENDIF
status=NF90_INQ_VARID(ncID,"yc",ycID)
IF (status /= nf90_noerr) THEN
   WRITE(*, *) "var_inq fail yc"
ENDIF
status=NF90_GET_VAR(ncID,ycID,yc,START=(/1/),COUNT=(/ny/))
IF (status /= nf90_noerr) THEN
   WRITE(*, *) "read fail yc"
ENDIF
status=NF90_INQ_VARID(ncID,"zc",zcID)
IF (status /= nf90_noerr) THEN
   WRITE(*, *) "var_inq fail zc"
ENDIF
status=NF90_GET_VAR(ncID,zcID,zc,START=(/1/),COUNT=(/nz/))
IF (status /= nf90_noerr) THEN
   WRITE(*, *) "read fail zc"
ENDIF
! close netCDF file
status=NF90_CLOSE(ncID)
WRITE(*, *) "read qcLabel successfully at 0"

DO timeIdx = initTimeStep, endTimeStep
! -----------------------------------------------------
! Reset all array to zeros
! -----------------------------------------------------
   cloudObject=0.
   cloudObjectZeroTo9999=99999.99
   qcObject=0.
   collocateCloudObject=0.
! -----------------------------------------------------
! Read Qc objects
! -----------------------------------------------------
   WRITE(fname, 103) TRIM(qcObjectPath),"cloudLabel-",timeIdx,".nc"
   status=NF90_OPEN(TRIM(fname),NF90_NOWRITE,ncID)
   IF (status /= nf90_noerr) THEN ! check reading success or fail
      WRITE(*, *) "open fail qc"
   ENDIF
! read data: xc, yc, zc, Qc
   status=NF90_INQ_VARID(ncID,"cloudLabel",varID)
   IF (status /= nf90_noerr) THEN
      WRITE(*, *) "var_inq fail qc"
   ENDIF
   status=NF90_GET_VAR(ncID,varID,qcObject,START=(/1,1,1,1/),COUNT=(/nx,ny,nz,1/))
   IF (status /= nf90_noerr) THEN
      WRITE(*, *) "read fail qc"
   ENDIF
   ! close netCDF file
   status=NF90_CLOSE(ncID)
   WRITE(*, *) "read qcObject successfully at ",timeIdx
! -----------------------------------------------------
! MAIN PROGRAM
! -----------------------------------------------------
   IF (MAXVAL(qcObject) > 0) THEN
! Read cloud object
      WRITE(fname, 103) TRIM(cloudObjectPath),"cloudLabel-",timeIdx,".nc"
      status=NF90_OPEN(TRIM(fname),NF90_NOWRITE,ncID)
      IF (status /= nf90_noerr) THEN ! check reading success or fail
         WRITE(*, *) "open fail cloud"
      ENDIF
! Read data: cloud object
      status=NF90_INQ_VARID(ncID,"cloudLabel",varID)
      IF (status /= nf90_noerr) THEN
         WRITE(*, *) "var_inq fail cloud"
      ENDIF
      status=NF90_GET_VAR(ncID,varID,cloudObject,START=(/1,1,1,1/),COUNT=(/nx,ny,nz,1/))
      IF (status /= nf90_noerr) THEN
         WRITE(*, *) "read fail cloud"
      ENDIF
      status=NF90_CLOSE(ncID)
      WRITE(*, *) "read cloudObject successfully at ",timeIdx
! Get min/max of cloud object index
      cloudObjectZeroTo9999 = cloudObject
      WHERE(cloudObject==0.) cloudObjectZeroTo9999 = 99999.99
      cloudMin = INT(MINVAL(cloudObjectZeroTo9999))
      cloudMax = INT(MAXVAL(cloudObject))
! Check each cloud's index
      DO cloudIdx = cloudMin, cloudMax
         inCloudIndexCount = 0
         inCloudIndexTemp = 0
         !correspondTrackID = 0.
         WRITE(*, 101) "Cloud ID: ", cloudIdx, "/", cloudMax
         IF (SUM((cloudObject == cloudIdx) * qcObject) /= 0) THEN
            101 FORMAT(A,I6.6,A,I6.6)
! Record counts, corresponding index(s), and maximum
            DO zIdx = 1, nz
               DO yIdx = 1, ny
                  DO xIdx = 1, nx
                     qcIdx = qcObject(xIdx, yIdx, zIdx)
                     IF (cloudObject(xIdx, yIdx, zIdx) == cloudIdx .AND. COUNT(inCloudIndexTemp == qcIdx) == 0) THEN
                        inCloudIndexCount = inCloudIndexCount + 1
                        inCloudIndexTemp(inCloudIndexCount) = qcIdx
                     ENDIF
                  ENDDO
               ENDDO
            ENDDO

            DO i = 1, inCloudIndexCount
               inCloudIndexTemp(i) = finalTrack(inCloudIndexTemp(i))
            ENDDO
! If only one trackID corresponds to this cloud
            IF (inCloudIndexCount == 1) THEN
               WHERE(cloudObject == cloudIdx) collocateCloudObject = finalTrack(inCloudIndexTemp(1))
               WRITE(*, *) "    1 TrackID: ", INT(inCloudIndexTemp(1))
! If multiple trackIDs correspond to this cloud
            ELSE IF (inCloudIndexCount >= 2) THEN
               WRITE(*, *) "    >= 2 TrackIDs: "
               IF (inCloudIndexTemp(1) == inCloudIndexTemp(2)) THEN
                  WRITE(*, *) "    are same TrackID: ", INT(inCloudIndexTemp(1))
                  WHERE(cloudObject == cloudIdx) collocateCloudObject = inCloudIndexTemp(1)
               ELSE
                  WRITE(20, 102) "time: ", timeIdx, "->", cloudIdx
                  102 FORMAT(A, I6.6, A, I6.6)
                  WRITE(*, *) "    are not same TrackIDs: ", INT(inCloudIndexTemp(1))
                  WHERE(cloudObject == cloudIdx) collocateCloudObject = inCloudIndexTemp(1)
                  DO i = 1, inCloudIndexCount
                     WRITE(*, *) inCloudIndexTemp(i)
                  ENDDO
               ENDIF
            ENDIF
         ENDIF
      ENDDO
   ENDIF

! -----------------------------------------------------
! Write out new cloud objects (cloudLabel-******.nc)
! -----------------------------------------------------
   WRITE(fname, 103) TRIM(ncOutputPath),"cloudLabel-",timeIdx,".nc"
! open netCDF file
   status=NF90_CREATE(TRIM(fname),NF90_NETCDF4,ncID)
   IF (status /= nf90_noerr) THEN ! check reading success or fail
      WRITE(*, *) "create fail new cloudLabel.nc"
   ENDIF
! def dimension: time(nt(1), ), xc(nx, ), yc(ny, ), zc(nz, )
   status=NF90_DEF_DIM(ncID, "time", 1, timeDimID)
   IF (status /= nf90_noerr) THEN
      WRITE(*, *) "fail to def dimension of time"
   ENDIF
   status=NF90_DEF_DIM(ncID, "xc", nx, xcDimID)
   IF (status /= nf90_noerr) THEN
      WRITE(*, *) "fail to def dimension of xc"
   ENDIF
   status=NF90_DEF_DIM(ncID, "yc", ny, ycDimID)
   IF (status /= nf90_noerr) THEN
      WRITE(*, *) "fail to def dimension of yc"
   ENDIF
   status=NF90_DEF_DIM(ncID, "zc", nz, zcDimID)
   IF (status /= nf90_noerr) THEN
      WRITE(*, *) "fail to def dimension of zc"
   ENDIF
! create variable: time(nt(1), ), xc(nx, ), yc(ny, ), zc(nz, ), collocateCloudObject(nx, ny, nz, nt(1))
   status=NF90_DEF_VAR(ncID, "time", NF90_INT, &
                       (/ timeDimID /), timeID)
   IF (status /= nf90_noerr) THEN
      WRITE(*, *) "fail to create var of time"
   ENDIF
   status=NF90_DEF_VAR(ncID, "xc", NF90_DOUBLE, &
                       (/ xcDimID /), xcID)
   IF (status /= nf90_noerr) THEN
      WRITE(*, *) "fail to create var of xc"
   ENDIF
   status=NF90_DEF_VAR(ncID, "yc", NF90_DOUBLE, &
                       (/ ycDimID /), ycID)
   IF (status /= nf90_noerr) THEN
      WRITE(*, *) "fail to create var of yc"
   ENDIF
   status=NF90_DEF_VAR(ncID, "zc", NF90_DOUBLE, &
                       (/ zcDimID /), zcID)
   IF (status /= nf90_noerr) THEN
      WRITE(*, *) "fail to create var of zc"
   ENDIF
   status=NF90_DEF_VAR(ncID, "cloudLabel", NF90_INT, &
                       (/ xcDimID, ycDimID, zcDimID, timeDimID /), varID)
   IF (status /= nf90_noerr) THEN
      WRITE(*, *) "fail to create var of cloudLabel"
   ENDIF
!   Write variable: time(nt(1), ), xc(nx, ), yc(ny, ), zc(nz, ), collocateCloudObject(nx, ny, nz, nt(1))
   status=NF90_PUT_VAR(ncID, timeID, (/ 1 /))
   IF (status /= nf90_noerr) THEN
      WRITE(*, *) "fail to put var of time"
   ENDIF
   status=NF90_PUT_VAR(ncID, xcID, xc)
   IF (status /= nf90_noerr) THEN
      WRITE(*, *) "fail to put var of xc"
   ENDIF
   status=NF90_PUT_VAR(ncID, ycID, yc)
   IF (status /= nf90_noerr) THEN
      WRITE(*, *) "fail to put var of yc"
   ENDIF
   status=NF90_PUT_VAR(ncID, zcID, zc)
   IF (status /= nf90_noerr) THEN
      WRITE(*, *) "fail to put var of zc"
   ENDIF
   status=NF90_PUT_VAR(ncID, varID, collocateCloudObject)
   IF (status /= nf90_noerr) THEN
      WRITE(*, *) "fail to put var of cloudLabel"
   ENDIF
!   close netCDF file
   status=NF90_CLOSE(ncID)
   WRITE(*, *) "[Done] Write out collocated cloudLabel"

ENDDO

END PROGRAM assignIdxToCloud
