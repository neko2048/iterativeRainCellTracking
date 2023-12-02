PROGRAM cloudObjectFilter_2Layer
USE netcdf
IMPLICIT NONE

! -----------------------------------------------------
! Set up Basic Params
! -----------------------------------------------------
!   File name
CHARACTER(255) :: zzPath="/home/atmenu10246/transition/src/datTXT/"
CHARACTER(255) :: cloudLabelPath="/data3/atmenu10246/cloudLabel/cloud2LayerOrigin/"
CHARACTER(255) :: cloudOutputPath="/data3/atmenu10246/cloudLabel/cloud2LayerFilter/"
!CHARACTER(255), DIMENSION(1) :: expt="mjo_std_mg" ! name of experiment
CHARACTER(255) :: fname ! name of netCDF file
!   Dimension
INTEGER, PARAMETER :: nx=1024
INTEGER, PARAMETER :: ny=1024
INTEGER, PARAMETER :: nz=120
INTEGER, PARAMETER :: nt=628
INTEGER, PARAMETER :: initTimeStep=546, endTimeStep=546
REAL, PARAMETER :: cloudBaseLim = 1000 ! m
!   Resolution
!   geographic coordinate
REAL, DIMENSION(nx) :: xc
REAL, DIMENSION(ny) :: yc
REAL, DIMENSION(nz) :: zc
REAL, DIMENSION(nz+1) :: zz
REAL, DIMENSION(nz) :: deltaZZ
!   ID
INTEGER :: ncID ! ID for netCDF file
INTEGER :: timeID, xcID, ycID, zcID, varID ! ID for variable
INTEGER :: timeDimID, xcDimID, ycDimID, zcDimID
!   needed data
REAL, DIMENSION(nx, ny, nz) :: cloudLabel=0. ! cloud objects with meanless index
REAL, DIMENSION(nx+2, ny+2, nz) :: expCloudLabel=0. ! expended cloudLabel to deal periodic boundary
REAL, DIMENSION(nx, ny, nz) :: filterCloudLabel=0. ! cloud objects that are filtered
!   counting index
INTEGER :: xIdx, yIdx, zIdx, timeIdx
INTEGER :: i, validCount
!   other
!INTEGER :: lrec
INTEGER :: status ! status for netCDF file
INTEGER :: numCloud, totalCount ! number of cloud objects
INTEGER :: outerBoundVal, innerBoundVal ! boundary values of expCloudLabel and cloudLabel
INTEGER, DIMENSION(:), ALLOCATABLE :: cloudLabelIdxArr ! array of index of cloud objects
REAL :: cloudDepth
!REAL :: junk ! unneeded but read variables
!   time
!REAL :: tarray(2), time, dtime

WRITE(fname, 103) TRIM(zzPath),"zz-mjo.txt"
103 FORMAT(2A)
OPEN(UNIT=10, FILE=fname, FORM="FORMATTED", ACTION="read", STATUS="OLD")
READ(10, *) zz(:)
CLOSE(10)
deltaZZ = zz(2:nz+1) - zz(1:nz)
! -----------------------------------------------------
! Read xc, yc, zc
! -----------------------------------------------------
WRITE(fname, 100) TRIM(cloudLabelPath),"cloudLabel-",0,".nc"
100 FORMAT(2A,I6.6,A)
! open netCDF file
status=NF90_OPEN(TRIM(fname),NF90_NOWRITE,ncID)
IF (status /= nf90_noerr) THEN ! check reading success or fail
   WRITE(*, *) "open fail cloudLabel.nc"
ENDIF
! read data: xc, yc, zc
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
   WRITE(*, *) "read fail cloudLabel"
ENDIF
! close netCDF file
status=NF90_CLOSE(ncID)
WRITE(*, *) "read cloudLabel successfully at ",timeIdx



DO timeIdx=initTimeStep, endTimeStep
! -----------------------------------------------------
! Reset all cloud-related array to zeros
! -----------------------------------------------------
   cloudLabel=0.
   expCloudLabel = 0.
   filterCloudLabel = 0.

! -----------------------------------------------------
! Read cloud objects (cloudLabel-******.nc)
! -----------------------------------------------------
   WRITE(fname, 100) TRIM(cloudLabelPath),"cloudLabel-",timeIdx,".nc"
   ! open netCDF file
   status=NF90_OPEN(TRIM(fname),NF90_NOWRITE,ncID)
   IF (status /= nf90_noerr) THEN ! check reading success or fail
      WRITE(*, *) "open fail cloudLabel.nc"
   ENDIF
! read data: cloud objects
   status=NF90_INQ_VARID(ncID,"cloudLabel",varID)
   IF (status /= nf90_noerr) THEN
      WRITE(*, *) "var_inq fail cloudLabel"
   ENDIF
   status=NF90_GET_VAR(ncID,varID,cloudLabel,START=(/1,1,1,1/),COUNT=(/nx,ny,nz,1/))
   IF (status /= nf90_noerr) THEN
      WRITE(*, *) "read fail cloudLabel"
   ENDIF
! close netCDF file
   status=NF90_CLOSE(ncID)
   WRITE(*, *) "read cloudLabel successfully at ",timeIdx
   WRITE(*, *) MAXVAL(cloudLabel)
   IF (MAXVAL(cloudLabel) /= 0.) THEN
   ! -----------------------------------------------------
   ! New copied cloudLabel with expended dimension
   ! -----------------------------------------------------
   !   Copy cloudLabel to newCloudLabe
      expCloudLabel(2:nx+1, 2:ny+1, :) = cloudLabel(1:nx, 1:ny, :)
   !   Copy bounday of cloudLabel to expCloudLabel
      expCloudLabel(2:nx+1, 1, :) = cloudLabel(1:nx, ny, :)
      expCloudLabel(2:nx+1, ny+2, :) = cloudLabel(1:nx, 1, :)
      expCloudLabel(1, 2:ny+1, :) = cloudLabel(nx, 1:ny, :)
      expCloudLabel(nx+2, 2:ny+1, :) = cloudLabel(1, 1:ny, :)
   ! -----------------------------------------------------
   ! Check uncontinuous boundary
   ! -----------------------------------------------------
      DO zIdx = 1, nz
         DO xIdx = 1, nx+2
            outerBoundVal = expCloudLabel(xIdx, 1, zIdx)
            innerBoundVal = expCloudLabel(xIdx, 2, zIdx)
            IF (outerBoundVal * innerBoundVal /= 0 .AND. outerBoundVal /= innerBoundVal) THEN
               WHERE (expCloudLabel == MAX(outerBoundVal, innerBoundVal)) & 
               expCloudLabel = MIN(outerBoundVal, innerBoundVal)
            ENDIF
   
            outerBoundVal = expCloudLabel(xIdx, ny+2, zIdx)
            innerBoundVal = expCloudLabel(xIdx, ny+1, zIdx)
            IF (outerBoundVal * innerBoundVal /= 0 .AND. outerBoundVal /= innerBoundVal) THEN
               WHERE (expCloudLabel == MAX(outerBoundVal, innerBoundVal)) & 
               expCloudLabel = MIN(outerBoundVal, innerBoundVal)
            ENDIF
         ENDDO
         DO yIdx = 1, ny+2
            outerBoundVal = expCloudLabel(1, yIdx, zIdx)
            innerBoundVal = expCloudLabel(2, yIdx, zIdx)
            IF (outerBoundVal * innerBoundVal /= 0 .AND. outerBoundVal /= innerBoundVal) THEN
               WHERE (expCloudLabel == MAX(outerBoundVal, innerBoundVal)) & 
               expCloudLabel = MIN(outerBoundVal, innerBoundVal)
            ENDIF
   
            outerBoundVal = expCloudLabel(nx+2, yIdx, zIdx)
            innerBoundVal = expCloudLabel(nx+1, yIdx, zIdx)
            IF (outerBoundVal * innerBoundVal /= 0 .AND. outerBoundVal /= innerBoundVal) THEN
               WHERE (expCloudLabel == MAX(outerBoundVal, innerBoundVal)) & 
               expCloudLabel = MIN(outerBoundVal, innerBoundVal)
            ENDIF
         ENDDO
      ENDDO
   
      cloudLabel(:, :, :) = expCloudLabel(2:nx+1, 2:ny+1, :)
      WRITE(*, *) "[Done] Deal Boundary Object Index"
   ! -----------------------------------------------------
   ! remove cloud objects whose cloud base is higher than 1000 m
   ! -----------------------------------------------------
   !   List index of cloud objects
      numCloud = INT(MAXVAL(cloudLabel))
      IF (numCloud /= 0) THEN
         ALLOCATE(cloudLabelIdxArr(numCloud))
         cloudLabelIdxArr = 0
         DO zIdx = 1, nz
            IF (zz(zIdx) <= 1000) THEN
               DO yIdx = 1, ny
                  DO xIdx = 1, nx
                     i = INT(cloudLabel(xIdx, yIdx, zIdx))
                     IF (i > 0 .AND. cloudLabelIdxArr(i) == 0) THEN
                        cloudLabelIdxArr(i) = i
                     ENDIF
                  ENDDO
               ENDDO
            ENDIF
         ENDDO
         WRITE(*, *) "[Done] Find Candicates"
         validCount = 0
         totalCount = COUNT(cloudLabelIdxArr /= 0)
         DO i = 1, numCloud
            IF (cloudLabelIdxArr(i) == i) THEN
               cloudDepth = - SUM(ANY(ANY(cloudLabel == i, DIM=1), DIM=1) * deltaZZ)
               ! I have no idea why "-" sign should added but it works
               IF (cloudDepth >= 500) THEN
                  IF (MOD(validCount, 100) == 0) THEN
                  WRITE(*, 102) i, "/", numCloud, "(", validCount, "/", totalCount, ") @ ", timeIdx
                  102 FORMAT(I4, A, I4 ,A, I4, A, I4, A, I6.6)
                  WRITE(*, *) "Cloud Depth: ", cloudDepth
                  ENDIF
                  WHERE(cloudLabel == i) filterCloudLabel = i
                  validCount = validCount + 1
               ENDIF
            ENDIF
         ENDDO
         DEALLOCATE(cloudLabelIdxArr)
      ENDIF
      WRITE(*, *) "[Done] Remove Too High Cloud Objects"
   ENDIF
! -----------------------------------------------------
! Write out new cloud objects (cloudLabel-******.nc)
! -----------------------------------------------------
   WRITE(fname, 101) TRIM(cloudOutputPath),"cloudLabel-",timeIdx,".nc"
   101 FORMAT(2A,I6.6,A)
   ! open netCDF file
   status=NF90_CREATE(TRIM(fname),NF90_NETCDF4,ncID)
   IF (status /= nf90_noerr) THEN ! check reading success or fail
      WRITE(*, *) "create fail new cloudLabel.nc"
   ENDIF
!   def dimension: time(nt(1), ), xc(nx, ), yc(ny, ), zc(nz, )
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
!   create variable: time(nt(1), ), xc(nx, ), yc(ny, ), zc(nz, ), filterCloudLabel(nx, ny, nz, nt(1))
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
!   Write variable: time(nt(1), ), xc(nx, ), yc(ny, ), zc(nz, ), filterCloudLabel(nx, ny, nz, nt(1))
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
   status=NF90_PUT_VAR(ncID, varID, filterCloudLabel)
   IF (status /= nf90_noerr) THEN
      WRITE(*, *) "fail to put var of cloudLabel"
   ENDIF
!   close netCDF file
   status=NF90_CLOSE(ncID)
   WRITE(*, *) "[Done] Write out filtered cloudLabel"

ENDDO ! for time iteration
END PROGRAM cloudObjectFilter_2Layer
