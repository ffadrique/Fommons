program unit_m_object

!-------------------------------------------------------------------------------
! Copyright : 2022, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
! Synopsis  : Unit tests driver for m_object
!-------------------------------------------------------------------------------

!---USE statements--------------------------------------------------------------

  use m_xfunit

  use unit_m_object_tests

!---End of use statements-------------------------------------------------------

  implicit none

  character(len=*), parameter :: package = 'system'
  character(len=*), parameter :: module = 'm_object'
  
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
                        source='m_object.f03', &
                        annotation='Object base class for all classes' )

! Create test
  ut = xfunit_unit( name='unit_m_object_test_001', &
                    classname='t_object', &
                    executer=unit_m_object_test_001, &
                    annotation='Equality and hash' )
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

end program unit_m_object
