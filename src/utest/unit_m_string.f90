program unit_m_string

!-------------------------------------------------------------------------------
! Copyright : 2022, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
! Synopsis  : Unit tests for m_string
!-------------------------------------------------------------------------------

!---USE statements--------------------------------------------------------------

  use m_xfunit

  use unit_m_string_tests

!---End of use statements-------------------------------------------------------

  implicit none

  character(len=*), parameter :: package = 'utilities'
  character(len=*), parameter :: module = 'm_string'
  
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
                        source='m_string.f03', &
                        annotation='Dynamic string type', &
                        before=unit_m_string_suite_before, &
                        after=unit_m_string_suite_after )

! Create test
  ut = xfunit_unit( name='m_string_001', &
                    classname="t_string", &
                    annotation='Basic string operations', &
                    executer=unit_m_string_test_001, &
                    before=unit_m_string_before_001, &
                    after=unit_m_string_after_001 )
  call suite%add_unit_test( ut )

! Create test
  ut = xfunit_unit( name='m_string_002', &
                    classname="t_string", &
                    annotation='Write string to file', &
                    executer=unit_m_string_test_002, &
                    before=unit_m_string_before_002, &
                    after=unit_m_string_after_002 )
  call suite%add_unit_test( ut )

! Create test
  ut = xfunit_unit( name='m_string_003', &
                    classname="t_string", &
                    annotation='Reading from file', &
                    executer=unit_m_string_test_003, &
                    before=unit_m_string_before_003, &
                    after=unit_m_string_after_003 )
  call suite%add_unit_test( ut )

! Create test
  ut = xfunit_unit( name='m_string_004', &
                    classname="t_string", &
                    annotation='Operators and assignments', &
                    executer=unit_m_string_test_004, &
                    before=unit_m_string_before_004, &
                    after=unit_m_string_after_004 )
  call suite%add_unit_test( ut )

! Create test
  ut = xfunit_unit( name='m_string_005', &
                    classname="t_string", &
                    annotation='Logical operators', &
                    executer=unit_m_string_test_005, &
                    before=unit_m_string_before_005, &
                    after=unit_m_string_after_005 )
  call suite%add_unit_test( ut )

! Create test
  ut = xfunit_unit( name='m_string_006', &
                    classname="t_string", &
                    annotation='String matching', &
                    executer=unit_m_string_test_006, &
                    before=unit_m_string_before_006, &
                    after=unit_m_string_after_006 )
  call suite%add_unit_test( ut )

! Create test
  ut = xfunit_unit( name='m_string_007', &
                    classname="t_string", &
                    annotation='String widlcard matching', &
                    executer=unit_m_string_test_007, &
                    before=unit_m_string_before_007, &
                    after=unit_m_string_after_007 )
  call suite%add_unit_test( ut )

! Create test
  ut = xfunit_unit( name='m_string_008', &
                    classname="t_string", &
                    annotation='String split and join', &
                    executer=unit_m_string_test_008, &
                    before=unit_m_string_before_008, &
                    after=unit_m_string_after_008 )
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

end program unit_m_string

