program unit_m_path

!-------------------------------------------------------------------------------
! Copyright : 2022, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
! Synopsis  : Unit tests driver for m_path
!-------------------------------------------------------------------------------

!---USE statements--------------------------------------------------------------

  use m_xfunit

  use unit_m_path_tests

!---End of use statements-------------------------------------------------------

  implicit none

  character(len=*), parameter :: package = 'utilities'
  character(len=*), parameter :: module = 'm_path'
  
!---Declaration of local variables----------------------------------------------


!---End of declaration of local variables---------------------------------------

!---Executable Code-------------------------------------------------------------

! Local variables
  character(len=256) :: xfunit_root_dir
  logical :: junit_strict
  type(t_xfunit_suite), allocatable :: suite
  type(t_xfunit_unit), allocatable :: ut

! Initialise report generation flag
  junit_strict = .false.

! Initialise unit test infrastructure
  allocate( manager )
  allocate( suite )
  allocate( ut )

! Initialise the unit test manager
  call get_environment_variable( 'XFUNIT_ROOT_DIR', xfunit_root_dir )
  manager = xfunit_manager_eclipse( module, xfunit_root_dir, junit_strict )

! Initialise test suite
  suite = xfunit_suite( package=package, &
                        source='m_path.f03', &
                        annotation='Generic Path' )

! Create test
  ut = xfunit_unit( name='unit_m_path_test_001', &
                    annotation='Absolute path', &
                    executer=unit_m_path_test_001 )
  call suite%add_unit_test( ut )

! Create test
  ut = xfunit_unit( name='unit_m_path_test_002', &
                    annotation='Relative path', &
                    executer=unit_m_path_test_002 )
  call suite%add_unit_test( ut )

! Create test
  ut = xfunit_unit( name='unit_m_path_test_003', &
                    annotation='Singular cases', &
                    executer=unit_m_path_test_003 )
  call suite%add_unit_test( ut )

! Create test
  ut = xfunit_unit( name='unit_m_path_test_004', &
                    annotation='Temporary paths', &
                    executer=unit_m_path_test_004 )
  call suite%add_unit_test( ut )

! Execute tests
  call manager%execute( suite )
  if( manager%is_error() ) then
    call manager%dump_error( 0 )
  end if

! Generate output
  call manager%write_xml( suite )

! Terminate unit test infrastructure
  deallocate( manager )
  deallocate( suite )
  deallocate( ut )

end program unit_m_path

