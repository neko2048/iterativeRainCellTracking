PROGRAM getHistoryTrackID
USE netcdf
IMPLICIT NONE


! -----------------------------------------------------
! Set up Basic Params
! -----------------------------------------------------
!   File name
!CHARACTER(255) :: vvmPath="/data3/mog/les/mjo_std_mg"
CHARACTER(255) :: irtPath="/home/atmenu10246/iterativeRainCellTracking/RRtrack/mjo-cwp-1e-1/"
CHARACTER(255) :: cloudObjectPath="/data3/atmenu10246/cloudLabel/filter/"
CHARACTER(255) :: qcObjectPath="/data3/atmenu10246/collocateCloud/qcCollocate/"
CHARACTER(255) :: outputPath="/data3/atmenu10246/collocateCloud/collocateCloudByQc/"
CHARACTER(255) :: histroyTrackPath="/home/atmenu10246/transition/src/"
!CHARACTER(255), DIMENSION(1) :: expt="mjo_std_mg" ! name of experiment
CHARACTER(255) :: fname ! name of netCDF file
!   Dimension
INTEGER, PARAMETER :: nx=1024
INTEGER, PARAMETER :: ny=1024
INTEGER, PARAMETER :: nz=120
INTEGER, PARAMETER :: nt=628
INTEGER, PARAMETER :: trackIDMax=11521 ! max value for track ID ( !!! should be checked personally)
INTEGER, PARAMETER :: initTimeStep = 451, endTimeStep = 540
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
REAL, DIMENSION(nx, ny, nz) :: qcObject=0. ! qc objects with collocated index
REAL, DIMENSION(trackIDMax) :: listTrackID=0.! array of index of cloud objects
REAL, DIMENSION(64) :: inCloudIndexTemp=0.
!   counting index
INTEGER :: xIdx, yIdx, zIdx, timeIdx, cloudIdx, qcIdx
INTEGER :: i, inCloudIndexCount, inCloudIndexMax, tempIndex=0
!   other
!INTEGER :: lrec
INTEGER :: status ! status for netCDF file
INTEGER :: cloudMax, cloudMin
!   output array
INTEGER, DIMENSION(trackIDMax) :: historyTrackPath=0.
DO i = 1, trackIDMax
   listTrackID(i) = i
ENDDO


! -----------------------------------------------------
! Read xc, yc, zc
! -----------------------------------------------------
WRITE(fname, 100) TRIM(qcObjectPath),"cloudLabel-",0,".nc"
100 FORMAT(2A,I6.6,A)
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
   cloudObjectZeroTo9999=0.
   qcObject=0.
   historyTrackPath=0

! -----------------------------------------------------
! Read Qc objects
! -----------------------------------------------------
   WRITE(fname, 100) TRIM(qcObjectPath),"cloudLabel-",timeIdx,".nc"
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
      WRITE(fname, 100) TRIM(cloudObjectPath),"cloudLabel-",timeIdx,".nc"
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
         inCloudIndexMax = 0
         inCloudIndexTemp = 0.
         IF (ANY(cloudObject == cloudIdx)) THEN
            WRITE(*, 101) "Cloud ID: ", cloudIdx, "/", cloudMax
            101 FORMAT(A,I6.6,A,I6.6)
! Record counts, corresponding index(s), and maximum
            DO zIdx = 1, nz
               DO yIdx = 1, ny
                  DO xIdx = 1, nx
                     qcIdx = qcObject(xIdx, yIdx, zIdx)
                     IF (cloudObject(xIdx, yIdx, zIdx) == cloudIdx .AND. COUNT(inCloudIndexTemp == qcIdx) == 0) THEN
                        inCloudIndexCount = inCloudIndexCount + 1
                        inCloudIndexTemp(inCloudIndexCount) = qcIdx
                        inCloudIndexMax = MAX(inCloudIndexMax, qcIdx)
                     ENDIF
                  ENDDO
               ENDDO
            ENDDO
! If only one trackID corresponds to this cloud
            IF (COUNT(inCloudIndexTemp /= 0.) == 1) THEN
               WRITE(*, *) "Cloud", cloudIdx, " - 1 trackID"
               DO i = 1, 64
                  IF (inCloudIndexTemp(i) /= 0. .AND. historyTrackPath(INT(inCloudIndexTemp(i))) == 0.) THEN
                  ! Choose the index in the cloud and the correspond history track should be non-recorded yet.
                     historyTrackPath(INT(inCloudIndexTemp(i))) = inCloudIndexMax
                     WRITE(*, *) historyTrackPath(INT(inCloudIndexTemp(i)))
                  ELSE IF (inCloudIndexTemp(i) /= 0. .AND. historyTrackPath(INT(inCloudIndexTemp(i))) /= 0.) THEN
                  ! Choose the index in the cloud and the correspond history track should be non-recorded yet.
                     WRITE(*, *) "It has been recorded", historyTrackPath(INT(inCloudIndexTemp(i)))
                  ELSE IF (inCloudIndexTemp(i) == 0.) THEN
                     EXIT
                  ENDIF
               ENDDO
! If multiple trackIDs correspond to this cloud
            ELSE IF (COUNT(inCloudIndexTemp /= 0.) >= 2) THEN
               WRITE(*, *) "Cloud", cloudIdx, " - >=2 trackIDs"
               DO i = 1, 64
                  !WRITE(*, *) inCloudIndexTemp(i)
                  IF (inCloudIndexTemp(i) /= 0. .AND. historyTrackPath(INT(inCloudIndexTemp(i))) == 0.) THEN
                     ! If this cloud has many LWP regions and no cloud that has recorded in historyTrackPath.
                     historyTrackPath(INT(inCloudIndexTemp(i))) = inCloudIndexMax
                     WRITE(*, *) "cloud take new historyTrack", historyTrackPath(INT(inCloudIndexTemp(i)))
                  ELSE IF (inCloudIndexTemp(i) /= 0. .AND. historyTrackPath(INT(inCloudIndexTemp(i))) /= 0.) THEN
                     ! If this cloud has overlapped LWP regions with the cloud that already record historyTrackPath.
                     tempIndex = historyTrackPath(INT(inCloudIndexTemp(i)))
                     WRITE(*, *) "HistoryTrack is already taken", historyTrackPath(INT(inCloudIndexTemp(i))), "->", tempIndex
                     EXIT
                  ELSE IF (inCloudIndexTemp(i) == 0.) THEN
                     EXIT
                  ENDIF
               ENDDO
               IF (tempIndex /= 0) THEN
                  DO i = 1, 64
                     IF (inCloudIndexTemp(i) /= 0.) THEN
                     historyTrackPath(INT(inCloudIndexTemp(i))) = tempIndex
                     ENDIF
                  ENDDO
                  tempIndex = 0
               ENDIF
            ENDIF
         ENDIF
      ENDDO
   ENDIF
! Save historyTrackPath
   WRITE(fname, 104) TRIM(outputPath),"historyTrackPath-",timeIdx,".txt"
   104 FORMAT(2A,I6.6,A)
   OPEN(UNIT=10, FILE=fname, FORM="FORMATTED", ACTION="write", STATUS="UNKNOWN")
   WRITE(10, *) historyTrackPath(:)
   CLOSE(10)
ENDDO

END PROGRAM


!SUBROUTINE readNC(fname, nx, ny, nz, xc, yc, zc, timeIdx, outputVar, varName, subName)
!   !IMPLICIT NONE
!   CHARACTER(255) :: fname
!   INTEGER, INTENT(IN) :: nx
!   INTEGER, INTENT(IN) :: ny
!   INTEGER, INTENT(IN) :: nz
!   INTEGER :: ncID, xcID, ycID, zcID, varID ! ID for variable
!   REAL, DIMENSION(nx, ny, nz), INTENT(INOUT) :: outputVar
!   ! open netCDF file
!   status=NF90_OPEN(TRIM(fname),NF90_NOWRITE,ncID)
!   IF (status /= nf90_noerr) THEN ! check reading success or fail
!      WRITE(*, *) "open fail ", subName
!   ENDIF
!! read data: xc, yc, zc, Qc
!   status=NF90_INQ_VARID(ncID,varName,varID)
!   IF (status /= nf90_noerr) THEN
!      WRITE(*, *) "var_inq fail", subName
!   ENDIF
!   status=NF90_GET_VAR(ncID,varID,outputVar,START=(/1,1,1,1/),COUNT=(/nx,ny,nz,1/))
!   IF (status /= nf90_noerr) THEN
!      WRITE(*, *) "read fail ", subName
!   ENDIF
!! close netCDF file
!   status=NF90_CLOSE(ncID)
!   WRITE(*, *) "read", subName, "successfully @",timeIdx
!END SUBROUTINE readNC



!SUBROUTINE unique(nx, ny, nz, varArr, uniqueArr)
!! -----------------------------------------------------
!! INPUT: Array
!! OUPUT: unique nonzero values in array
!! -----------------------------------------------------
!IMPLICIT NONE
!INTEGER, INTENT(IN) :: nx
!INTEGER, INTENT(IN) :: ny
!INTEGER, INTENT(IN) :: nz
!REAL, DIMENSION(nx, ny, nz), INTENT(IN) :: varArr
!REAL, DIMENSION(nx, ny, nz) :: varArrDeplicate
!REAL, DIMENSION(:), ALLOCATABLE, INTENT(INOUT) :: uniqueArr
!REAL :: maxValOfVarArr, minValOfVarArr
!INTEGER :: numUniqueVal=0
!INTEGER :: xIdx, yIdx, zIdx, i
!varArrDeplicate = varArr
!WHERE(varArr == 0.) varArrDeplicate = 99999.99
!! Count number of unique values except zero
!maxValOfVarArr = MAXVAL(varArr)
!minValOfVarArr = MINVAL(varArrDeplicate)
!WRITE(*, *) minValOfVarArr, maxValOfVarArr
!DO i = minValOfVarArr, maxValOfVarArr
!   IF (ANY(varArr == i)) THEN
!      numUniqueVal = numUniqueVal + 1 ! not including zero
!   ENDIF
!ENDDO
!WRITE(*, *) "SIZE: ", numUniqueVal
!ALLOCATE(uniqueArr(numUniqueVal))
!uniqueArr=0.
!DO i = 1, maxValOfVarArr
!   IF (ANY(varArr == i)) THEN
!      WRITE(*, *) "Find element: ", i
!      uniqueArr(i) = i
!   ENDIF
!ENDDO
!RETURN
!END SUBROUTINE unique


