MODULE irt_parameters

! grid information
INTEGER, PARAMETER    :: domainsize_x = 1024
INTEGER, PARAMETER    :: domainsize_y = 1024

LOGICAL, PARAMETER    :: llonlatgrid = .FALSE.
REAL, PARAMETER       :: unit_area = 0.25 ! in 0.1x0.1 deg grid boxes
! only used if llonlatgrid=.TRUE., otherwise set to arbitrary value:
REAL, PARAMETER       :: lat_first = -45.
REAL, PARAMETER       :: lat_inc = 0.1
REAL, PARAMETER       :: lon_inc = 0.1

LOGICAL, PARAMETER    :: lperiodic_x = .TRUE.
LOGICAL, PARAMETER    :: lperiodic_y = .TRUE.

INTEGER, PARAMETER    :: n_fields = 0   ! number of additional averaging fields

! bins of coarse velocity field
INTEGER, PARAMETER    :: time_steps = 145    ! total number of timesteps
INTEGER, PARAMETER    :: nt_bins = 1         ! 6 hourly
INTEGER, PARAMETER    :: nx_bins = 1
INTEGER, PARAMETER    :: ny_bins = 1

REAL, PARAMETER       :: threshold = 5.0            ! for intensity
REAL, PARAMETER       :: minimum_size = 1       ! events smaller than that will be sorted out

REAL, PARAMETER       :: termination_sensitivity=1.0      ! Choose value between 0.0 and 1.0

REAL, PARAMETER       :: max_velocity = 10.   ! adjust acordingly
                                              ! velocities>max_velocity will be ignored to remove outliers
! define a minimal number of cells required for a coarse grained coordinate to be evaluated 
! if there are less, missing value will be assigned to that coarse cell
INTEGER, PARAMETER    :: min_cells = 10

INTEGER, PARAMETER    :: max_no_of_cells=5000  ! buffer size, increase if necessary
INTEGER, PARAMETER    :: max_no_of_tracks=5000    ! buffer size, increase if necessary
INTEGER, PARAMETER    :: max_length_of_track=100  ! buffer size, increase if necessary

REAL, PARAMETER       :: miss=-998.           ! value<miss ==> missing_value

END MODULE irt_parameters
