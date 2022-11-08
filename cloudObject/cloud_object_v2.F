PROGRAM cloud_object
USE netcdf
IMPLICIT NONE

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! folder and file
CHARACTER(255) :: path="/data3/mog/taiwanvvm" ! path of the outputs
CHARACTER(255), DIMENSION(30) :: expt ! name of experiment
CHARACTER(255) :: fname ! name of netCDF file
! dimension
INTEGER, PARAMETER :: nx=1024
INTEGER, PARAMETER :: ny=1024
INTEGER, PARAMETER :: nz=70
! resolution [m]
REAL, PARAMETER :: dx=500.
REAL, PARAMETER :: dy=500.
REAL, DIMENSION(nz) :: dz
! geographic coordinate
REAL, DIMENSION(nx) :: lon ! longitude
REAL, DIMENSION(ny) :: lat ! latitude
INTEGER, DIMENSION(nx,ny) :: TOPO ! topography index
REAL, DIMENSION(nx,ny) :: terrain=0. ! topography [m]
! ID
INTEGER :: ncID ! ID for netCDF file
INTEGER :: varID ! ID for variable
! 1D variables
REAL, DIMENSION(nz+1) :: H, Hc ! height [m]
REAL, DIMENSION(nz+1) :: rho ! air density [kg/m^3]
REAL, DIMENSION(nz+1) :: theta_bar ! potential temperature [K]
REAL, DIMENSION(nz+1) :: P_bar ! pressure [Pa]
! 2D variables
REAL, DIMENSION(nx,ny) :: RR ! rainrate [mm/hr]
INTEGER, DIMENSION(nx,ny) :: flagmap
INTEGER, DIMENSION(nx,ny) :: flagmap_con
! 3D variables
REAL, DIMENSION(nx,ny,nz) :: w ! vertical velocity [m/s]
REAL, DIMENSION(nx,ny,nz) :: V ! volume [m^3]
REAL, DIMENSION(nx,ny,nz) :: qc ! cloud water mixing ratio [kg/kg]
REAL, DIMENSION(nx,ny,nz) :: qi ! cloud ice mixing ratio [kg/kg]
REAL, DIMENSION(nx,ny,nz) :: qtcw ! cloud total condensed water mixing ratio [kg/kg]
INTEGER, DIMENSION(nx,ny,nz) :: flag
! result
INTEGER, DIMENSION(nx*ny*nz) :: flagcloud ! index of clouds
INTEGER, DIMENSION(nx*ny*nz) :: flagconcloud ! index of clouds
INTEGER, DIMENSION(nx*ny*nz) :: FCC ! index of convective clouds
REAL, DIMENSION(nx*ny*nz) :: CTH ! cloud top height [m]
REAL, DIMENSION(nx*ny*nz) :: CBH ! cloud base height [m]
REAL, DIMENSION(nx*ny*nz) :: thick ! cloud thickness [m]
REAL, DIMENSION(nx*ny*nz) :: V_C ! volume of cloud [km^3]
REAL, DIMENSION(nx*ny*nz) :: maxw ! maximum vertical velocity [m/s]
REAL, DIMENSION(nx*ny*nz) :: maxRR ! maximum rain rate [mm/hr]
REAL, DIMENSION(3,nx*ny*nz) :: Centroid ! centroid of cloud (in grid)
REAL, DIMENSION(nx*ny*nz) :: COM ! centre of mass [m]
REAL, DIMENSION(nx*ny*nz) :: q_ratio, V_ratio
! statistics
INTEGER :: Nconcld, Ncld ! total number of convective clouds and all clouds
! counter
INTEGER :: i,j,k,t,e,n
! other
INTEGER :: lrec
INTEGER :: status ! status for netCDF file
REAL :: junk ! unneeded but read variables
! time
REAL :: tarray(2), time, dtime

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

INQUIRE(IOLENGTH=lrec) flagmap_con


expt=(/"tpe20050712","tpe20050723","tpe20060508","tpe20060718","tpe20060721", &
       "tpe20070830","tpe20080715","tpe20090707","tpe20090817","tpe20090827", &
       "tpe20100629","tpe20100630","tpe20100802","tpe20100803","tpe20100912", &
       "tpe20110615","tpe20110616","tpe20110702","tpe20110723","tpe20110802", &
       "tpe20110816","tpe20110821","tpe20120715","tpe20120819","tpe20130723", &
       "tpe20130807","tpe20130825","tpe20140525","tpe20140703","tpe20140825"/)

DO e=1,30
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!! Read Initial Profile !!!
! filename
WRITE(fname,99) "/data3/C.brobbtyh/fort98/TaiwanVVM/",TRIM(expt(e)),"_fort.98"
99 FORMAT(3A)
! open file
OPEN(UNIT=10,FILE=fname)
! read data
DO i=1,188
   READ(10,*)
ENDDO
DO i=1,nz+1
   READ(10,*) junk, H(i), Hc(i) ! height on the edge and in the center of a grid
ENDDO
DO i=1,3
   READ(10,*)
ENDDO
DO i=1,nz+1
   READ(10,*) junk, rho(i), theta_bar(i), P_bar(i) ! density, potential temperature and pressure
ENDDO
! close file
CLOSE(10)
! calculation
DO i=1,nz
   dz(i) = H(i+1)-H(i) ! depth
ENDDO
DO i=1,nz
   V(:,:,i) = dx * dy * dz(i) / 10**9 ! volume
ENDDO

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! Read Topography Information !!!
! filename
WRITE(fname,100) TRIM(path),"/",TRIM(expt(e)),"cln/TOPO.nc"
100 FORMAT(4A)
! open netCDF file
status=NF90_OPEN(TRIM(fname),NF90_NOWRITE,ncID)
IF (status /= nf90_noerr) THEN
   WRITE(*,*) "open fail TOPO.nc"
ENDIF
! read data: longitude
status=NF90_INQ_VARID(ncID,"lon",varID)
IF (status /= nf90_noerr) THEN
   WRITE(*,*) "var_inq fail lon"
ENDIF
status=NF90_GET_VAR(ncID,varID,lon,START=(/1/),COUNT=(/nx/))
IF (status /= nf90_noerr) THEN
   WRITE(*,*) "read fail lon"
ENDIF
! read data: latitude
status=NF90_INQ_VARID(ncID,"lat",varID)
IF (status /= nf90_noerr) THEN
   WRITE(*,*) "var_inq fail lat"
ENDIF
status=NF90_GET_VAR(ncID,varID,lat,START=(/1/),COUNT=(/ny/))
IF (status /= nf90_noerr) THEN
   WRITE(*,*) "read fail lat"
ENDIF
! read data: topography
status=NF90_INQ_VARID(ncID,"TOPO",varID)
IF (status /= nf90_noerr) THEN
   WRITE(*,*) "var_inq fail TOPO"
ENDIF
status=NF90_GET_VAR(ncID,varID,TOPO,START=(/1,1/),COUNT=(/nx,ny/))
IF (status /= nf90_noerr) THEN
   WRITE(*,*) "read fail TOPO"
ENDIF
! data processing
WHERE (TOPO > 0)
      terrain=(TOPO-1)*100
ENDWHERE
! close netCDF file
status=NF90_CLOSE(ncID)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

DO t=0,144
time=dtime(tarray)
   !!! Read netCDF of Thermodynamic Field !!!
   ! filename
   WRITE(fname,101) TRIM(path),"/",TRIM(expt(e)),"cln/archive/exp.L.Thermodynamic-",t,".nc"
   101 FORMAT(4A,I6.6,A)
   ! open netCDF file
   status=NF90_OPEN(TRIM(fname),NF90_NOWRITE,ncID)
   IF (status /= nf90_noerr) THEN 
      WRITE(*,*) "open fail thermo.nc"
      GOTO 1000
   ENDIF
   ! read data: cloud water mixing ratio
   status=NF90_INQ_VARID(ncID,"qc",varID)
   IF (status /= nf90_noerr) THEN
      WRITE(*,*) "var_inq fail qc"
      GOTO 1000
   ENDIF
   status=NF90_GET_VAR(ncID,varID,qc,START=(/1,1,1,1/),COUNT=(/nx,ny,nz,1/))
   IF (status /= nf90_noerr) THEN
      WRITE(*,*) "read fail qc"
      GOTO 1000
   ENDIF
   ! read data: cloud ice mixing ratio
   status=NF90_INQ_VARID(ncID,"qi",varID)
   IF (status /= nf90_noerr) THEN
      WRITE(*,*) "var_inq fail qi"
      GOTO 1000
   ENDIF
   status=NF90_GET_VAR(ncID,varID,qi,START=(/1,1,1,1/),COUNT=(/nx,ny,nz,1/))
   IF (status /= nf90_noerr) THEN
      WRITE(*,*) "read fail qi"
      GOTO 1000
   ENDIF
   ! close netCDF file
   status=NF90_CLOSE(ncID)

   !!! Read netCDF of Dynamic Field !!!
   ! filename
   WRITE(fname,101) TRIM(path),"/",TRIM(expt(e)),"cln/archive/exp.L.Dynamic-",t,".nc"
   ! open netCDF file
   status=NF90_OPEN(TRIM(fname),NF90_NOWRITE,ncID)
   IF (status /= nf90_noerr) THEN
      WRITE(*,*) "open fail dyn.nc"
      GOTO 1000
   ENDIF
   ! read data: cloud water mixing ratio
   status=NF90_INQ_VARID(ncID,"w",varID)
   IF (status /= nf90_noerr) THEN
      WRITE(*,*) "var_inq fail w"
      GOTO 1000
   ENDIF
   status=NF90_GET_VAR(ncID,varID,w,START=(/1,1,1,1/),COUNT=(/nx,ny,nz,1/))
   IF (status /= nf90_noerr) THEN
      WRITE(*,*) "read fail w"
      GOTO 1000
   ENDIF
   ! close netCDF file
   status=NF90_CLOSE(ncID)

   !!! Read netCDF of Surface Parameters !!!
   ! filename
   WRITE(fname,101) TRIM(path),"/",TRIM(expt(e)),"cln/archive/exp.C.Surface-",t,".nc"
   ! open netCDF file
   status=NF90_OPEN(TRIM(fname),NF90_NOWRITE,ncID)
   IF (status /= nf90_noerr) THEN
      WRITE(*,*) "open fail sfc.nc"
      GOTO 1000
   ENDIF
   ! read data: cloud water mixing ratio
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
   ! data processing
   RR = RR * 3600
   ! close netCDF file
   status=NF90_CLOSE(ncID)

time=dtime(tarray)
write(*,*) t, time

   !!! Cloud Object !!!
   ! total condensed water in cloud
   qtcw=qc+qi
   ! connection
   CALL six_connected(nx,ny,nz,lon,lat,terrain,H,Hc,qtcw,1.E-4,w,0.5,RR,V,qc,qi,flag &
                     ,flagcloud,V_C,CTH,CBH,thick,maxw,maxRR,Centroid,COM,q_ratio,V_ratio)
   ! find convective clouds and check the number of the clouds
   Nconcld = 0
   DO n=1,nx*ny*nz
      ! all clouds examined
      IF (flagcloud(n) == -1) THEN 
         GOTO 1001
      ENDIF
      ! find convective clouds
      IF (thick(n) >= 1000. .AND. CBH(n) <= 1000. .AND. COM(n) >= 500.) THEN
         Nconcld = Nconcld+1
         flagconcloud(Nconcld) = flagcloud(n)
      ELSE
         flagcloud(n) = -2
      ENDIF
   ENDDO
   1001 CONTINUE
   Ncld = n-1
   ! make the map with 2D cloud flag
   flagmap = 0
   DO j=1,ny
   DO i=1,nx
      DO k=1,nz
         IF (flag(i,j,k) /= -1 .AND. thick(flag(i,j,k)) >= 1000. .AND. &
             CBH(flag(i,j,k)) <= 1000. .AND. COM(flag(i,j,k)) >= 500.) THEN
            flagmap(i,j) = flag(i,j,k)
            GOTO 1002
         ENDIF
      ENDDO
      1002 CONTINUE
   ENDDO
   ENDDO
   ! extract the convective clouds
   k = 0
   flagmap_con = 0
   DO i=1,Ncld
      IF (flagcloud(i) /= -2) THEN
         k = k+1
         WHERE (flagmap == i)
               flagmap_con = k
         ENDWHERE
         FCC(k) = i         
      ENDIF
   ENDDO
   WHERE (flagmap == -1)
         flagmap_con = -1
   ENDWHERE   

   !!! Write Result --- Map !!!
   ! filename
   WRITE(fname,102) TRIM(expt(e)),"_clean_cloudmap_",t,".dat"
   102 FORMAT(2A,I6.6,A)
   ! open file
   OPEN(UNIT=12,FILE=TRIM(fname),FORM='UNFORMATTED',STATUS='UNKNOWN',ACCESS='DIRECT',RECL=lrec)
   ! write data
   WRITE(UNIT=12,REC=1) ((flagmap_con(i,j), i=1,nx), j=1,ny)
   ! close file
   CLOSE(12)

   !!! Write Result --- Metadata !!!
   ! filename
   WRITE(fname,102) TRIM(expt(e)),"_clean_cloudproperties_",t,".txt"
   ! open file
   OPEN(UNIT=13,FILE=TRIM(fname),FORM="FORMATTED",STATUS="UNKNOWN")
   ! write data
   DO i=1,Nconcld
      WRITE(13,103) i, V_C(FCC(i)), CTH(FCC(i)), CBH(FCC(i)), thick(FCC(i)), maxw(FCC(i)), maxRR(FCC(i)), &
                    Centroid(1,FCC(i)), Centroid(2,FCC(i)), Centroid(3,FCC(i)), COM(FCC(i)), &
                    q_ratio(FCC(i)), V_ratio(FCC(i)), flagcloud(FCC(i))
      103 FORMAT(I6.6,3X,F15.3,3X,F8.2,3X,F8.2,3X,F8.2,3X,F6.2,3X,F6.2,3X, &
                 F6.2,3X,F5.2,3X,F8.2,3X,F8.2,3X, &
                 F5.3,3X,F5.3,3X,I6.6)
   ENDDO
   ! close file
   CLOSE(13)

!time=dtime(tarray)
!write(*,*) t, time, Nconcld
   1000 CONTINUE
ENDDO

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
ENDDO


END PROGRAM cloud_object

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE six_connected(nx,ny,nz,longitude,latitude,TOPO,height,Hc &
                        ,data_array,criteria &
                        ,data_array2,criteria2 &
                        ,data_array3,volume_array,qc_array,qi_array,cloudflag_array &
                        ,cloudflag_seq,cldV,top,bottom,thick,maxw,maxRR,Centroid,COM,q_ratio,V_ratio)
IMPLICIT NONE

! input 
INTEGER, INTENT(in) :: nx, ny, nz 
REAL   , INTENT(in) :: criteria     ! criteria for defining objects
REAL   , INTENT(in) :: criteria2    ! criteria2 for defining objects
REAL   , DIMENSION(nx,ny,nz), INTENT(in) :: data_array ! data array
REAL   , DIMENSION(nx,ny,nz), INTENT(in) :: data_array2 ! data array2
REAL   , DIMENSION(nx,ny), INTENT(in) :: data_array3 ! data array3
REAL   , DIMENSION(nx,ny,nz), INTENT(in) :: volume_array ! volume array
REAL   , DIMENSION(nx), INTENT(in) :: longitude ! longitude
REAL   , DIMENSION(ny), INTENT(in) :: latitude ! latitude
REAL   , DIMENSION(nz+1), INTENT(in) :: height, Hc ! height
REAL   , DIMENSION(nx,ny), INTENT(in) :: TOPO ! topography
REAL   , DIMENSION(nx,ny,nz), INTENT(in) :: qc_array, qi_array ! water array
! output
INTEGER, DIMENSION(nx,ny,nz), INTENT(inout) :: cloudflag_array ! cloud flag array
INTEGER, DIMENSION(nx*ny*nz), INTENT(inout) :: cloudflag_seq ! cloud flag 
REAL   , DIMENSION(nx*ny*nz), INTENT(inout) :: cldV, top, bottom, thick, maxw, maxRR, COM, q_ratio, V_ratio
REAL   , DIMENSION(3,nx*ny*nz), INTENT(inout) :: Centroid ! centroid of the cloud
! local variables
REAL :: con_size, con_size_high
INTEGER :: ct, cb
INTEGER :: i, j, k
INTEGER :: i_end, j_end, k_end
INTEGER :: nex , ney , nez , myx , myy , myz
INTEGER :: label, waiting_num, dir
INTEGER, DIMENSION(6), PARAMETER :: &
         move_x_3d(6) =  (/ -1,  0, +1,  0,  0,  0 /), &
         move_y_3d(6) =  (/  0, -1,  0, +1,  0,  0 /), &
         move_z_3d(6) =  (/  0,  0,  0,  0, +1, -1 /)
INTEGER, DIMENSION(3,nx*ny*nz) :: waiting_grid
LOGICAL, DIMENSION(nx,ny,nz) :: search_flag
REAL :: GN, x, y, z, qtcwz, qtcw, qc, qi
REAL :: ctTOPO, cbTOPO, w_max, RR_max

! initialization
i_end = nx
j_end = ny
k_end = nz

label           = 1
cloudflag_array = -1
cloudflag_seq   = -1
cldV            = -1.
search_flag     = .FALSE.

! searching loop
k_loop: DO k=1,k_end
j_loop: DO j=1,j_end
i_loop: DO i=1,i_end

  grid_searched_or_not: IF (.not.(search_flag(i,j,k))) THEN
 
  search_flag(i,j,k) = .TRUE.
  waiting_num   = 1
  con_size      = 0.
  con_size_high = 0.
  ct            = 1
  cb            = k_end
  ctTOPO        = height(nz)
  cbTOPO        = 0.
  w_max         = -100.
  RR_max        = 0.
  GN            = 0. 
  x             = 0.
  y             = 0.
  z             = 0.
  qtcwz         = 0.
  qtcw          = 0.
  qc            = 0.
  qi            = 0.

  waiting_grid(1,waiting_num) = i
  waiting_grid(2,waiting_num) = j
  waiting_grid(3,waiting_num) = k  

  searching_loop: DO 
    
    IF (waiting_num < 1) GOTO 888

    myx = waiting_grid(1,waiting_num)
    myy = waiting_grid(2,waiting_num)
    myz = waiting_grid(3,waiting_num)

    waiting_num = waiting_num - 1

!    IF (data_array(myx,myy,myz) >= criteria .AND. &
!        data_array2(myx,myy,myz) >= criteria2 )THEN
     IF (data_array(myx,myy,myz) >= criteria) THEN

      cloudflag_array(myx,myy,myz) = label
     
      con_size = con_size + 1. * volume_array(myx,myy,myz)
      IF (data_array2(myx,myy,myz) >= criteria2) THEN
         con_size_high = con_size_high + 1. * volume_array(myx,myy,myz)
      ENDIF

      w_max = max(w_max,data_array2(myx,myy,myz))
      RR_max = max(RR_max,data_array3(myx,myy))

      IF (myz > ct) THEN
         ct = myz
         ctTOPO=TOPO(myx,myy)
      ELSEIF (myz == ct) THEN
         IF (TOPO(myx,myy) < ctTOPO) THEN
            ctTOPO=TOPO(myx,myy)
         ENDIF
      ENDIF

      IF (myz < cb) THEN
         cb = myz
         cbTOPO=TOPO(myx,myy)
      ELSEIF (myz == cb) THEN
         IF (TOPO(myx,myy) > cbTOPO) THEN         
            cbTOPO=TOPO(myx,myy)
         ENDIF
      ENDIF

      GN = GN + 1
      x  = x + longitude(myx)
      y  = y + latitude(myy)
      z  = z + Hc(myz+1)
 
      qtcwz = qtcwz + data_array(myx,myy,myz) * Hc(myz+1)
      qtcw = qtcw + data_array(myx,myy,myz)
      qc = qc + qc_array(myx,myy,myz)
      qi = qi + qi_array(myx,myy,myz)

      check_neighbor: DO dir=1,6
        nex = myx + move_x_3d(dir)
        ney = myy + move_y_3d(dir)
        nez = myz + move_z_3d(dir)
       
        IF ( nex==0       ) nex = i_end
        IF ( nex==i_end+1 ) nex = 1
        IF ( ney==0       ) ney = j_end
        IF ( ney==j_end+1 ) ney = 1

        IF ( nex>=1 .AND. nex<=i_end .AND. &
             ney>=1 .AND. ney<=j_end .AND. &
             nez>=1 .AND. nez<=k_end )THEN
          
          ne_search_or_not: IF (.not.(search_flag(nex,ney,nez)))THEN

            waiting_num = waiting_num +1
            waiting_grid(1,waiting_num) = nex
            waiting_grid(2,waiting_num) = ney
            waiting_grid(3,waiting_num) = nez

            search_flag(nex,ney,nez) = .TRUE.
          ENDIF ne_search_or_not
        ENDIF            
        
      ENDDO check_neighbor

    ENDIF

  ENDDO searching_loop
  888 CONTINUE

  IF (con_size /= 0)THEN 
    cloudflag_seq(label) = label
    cldV(label) = con_size
    top(label) = height(ct+1)-ctTOPO
    bottom(label) = height(cb)-cbTOPO
    thick(label) = height(ct+1)-height(cb)
    Centroid(1,label) = x / GN
    Centroid(2,label) = y / GN
    Centroid(3,label) = z / GN
    maxw(label) = w_max
    maxRR(label) = RR_max
    COM(label) = qtcwz / qtcw
    q_ratio(label) = qi / qtcw
    V_ratio(label) = con_size_high / con_size
    label = label + 1
  ENDIF
  ENDIF grid_searched_or_not
ENDDO i_loop
ENDDO j_loop
ENDDO k_loop

END SUBROUTINE six_connected
