module unit_m_iso3166_country_code_tests

!-------------------------------------------------------------------------------
! Copyright © 2020, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
! Synopsis  : Unit tests for m_iso3166_country_code
!-------------------------------------------------------------------------------

!---USE statements--------------------------------------------------------------

  use m_xfunit
  use m_messages
  use m_string

  use m_iso3166_country_code

!---End of use statements-------------------------------------------------------

  implicit none

!---Public/Private declarations-------------------------------------------------

  private

  public unit_m_iso3166_country_code_test_001

  public unit_m_iso3166_country_code_test_002

  public unit_m_iso3166_country_code_test_003

  public unit_m_iso3166_country_code_test_004

  public manager, suite

!---End of public/private declarations------------------------------------------

  character(len=*), parameter :: package = 'utilities'
  character(len=*), parameter :: module = 'm_iso3166_country_code'

!---Declaration of local variables----------------------------------------------

! The unit test management structures
  type(t_xfunit_manager), allocatable, save :: manager
  type(t_xfunit_suite), allocatable, save :: suite

! The error handling structure
  type(t_messages), save :: msg

!---End of declaration of local variables---------------------------------------

!---Executable Code-------------------------------------------------------------

contains

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_iso3166_country_code_test_001( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! The tested strcuture
  type(t_iso3166_country_code) :: object

! Local variables
  integer :: status

! Reset error handling structures
  call msg%reset_error()

! Constructor from alpha-2
  object = iso3166_country_code( 'ES' )
  call ut%assert_equal( 'Constructor (ES)', object%get_alpha2(), string('ES') )
  call ut%assert_equal( 'Constructor (ES -> ESP)', object%get_alpha3(), string('ESP') )
  call ut%assert_equal( 'Constructor (ES -> #)', object%get_numeric(), 724 )
  call ut%assert_equal( 'Constructor (ES -> Country)', object%get_name(), string('Spain') )
  call ut%assert_equal( 'Constructor (ES -> Capital)', object%get_capital_name(), string('Madrid') )
  call ut%assert_equal( 'Constructor (ES -> Region)', object%get_region_name(), string('Europe') )
  call ut%assert_equal( 'Constructor (ES -> Longitude)', object%get_longitude(), -4.0 )
  call ut%assert_equal( 'Constructor (ES -> Latitude)', object%get_latitude(), 40.0 )

! Constructor from alpha-2 (lowercase)
  call ut%assert_equal( 'Constructor (es)', object%get_alpha2(), string('ES') )
  call ut%assert_equal( 'Constructor (es -> ESP)', object%get_alpha3(), string('ESP') )
  call ut%assert_equal( 'Constructor (es -> #)', object%get_numeric(), 724 )
  call ut%assert_equal( 'Constructor (es -> Country)', object%get_name(), string('Spain') )
  call ut%assert_equal( 'Constructor (es -> Capital)', object%get_capital_name(), string('Madrid') )
  call ut%assert_equal( 'Constructor (es -> Region)', object%get_region_name(), string('Europe') )
  call ut%assert_equal( 'Constructor (es -> Longitude)', object%get_longitude(), -4.0 )
  call ut%assert_equal( 'Constructor (es -> Latitude)', object%get_latitude(), 40.0 )

! Constructor from alpha-3
  object = iso3166_country_code( 'ESP' )
  call ut%assert_equal( 'Constructor (ESP)', object%get_alpha3(), string('ESP') )
  call ut%assert_equal( 'Constructor (ESP -> ESP)', object%get_alpha2(), string('ES') )
  call ut%assert_equal( 'Constructor (ESP -> #)', object%get_numeric(), 724 )
  call ut%assert_equal( 'Constructor (ESP -> Country)', object%get_name(), string('Spain') )
  call ut%assert_equal( 'Constructor (ESP -> Capital)', object%get_capital_name(), string('Madrid') )
  call ut%assert_equal( 'Constructor (ESP -> Region)', object%get_region_name(), string('Europe') )
  call ut%assert_equal( 'Constructor (ESP -> Longitude)', object%get_longitude(), -4.0 )
  call ut%assert_equal( 'Constructor (ESP -> Latitude)', object%get_latitude(), 40.0 )

! Constructor from alpha-3 (lowercase)
  object = iso3166_country_code( 'esp' )
  call ut%assert_equal( 'Constructor (esp)', object%get_alpha3(), string('ESP') )
  call ut%assert_equal( 'Constructor (esp -> ESP)', object%get_alpha2(), string('ES') )
  call ut%assert_equal( 'Constructor (esp -> #)', object%get_numeric(), 724 )
  call ut%assert_equal( 'Constructor (esp -> Country)', object%get_name(), string('Spain') )
  call ut%assert_equal( 'Constructor (esp -> Capital)', object%get_capital_name(), string('Madrid') )
  call ut%assert_equal( 'Constructor (esp -> Region)', object%get_region_name(), string('Europe') )
  call ut%assert_equal( 'Constructor (esp -> Longitude)', object%get_longitude(), -4.0 )
  call ut%assert_equal( 'Constructor (esp -> Latitude)', object%get_latitude(), 40.0 )

! Constructor from name
  object = iso3166_country_code( 'Spain' )
  call ut%assert_equal( 'Constructor (Spain)', object%get_name(), string('Spain') )
  call ut%assert_equal( 'Constructor (Spain -> ES)', object%get_alpha2(), string('ES') )
  call ut%assert_equal( 'Constructor (Spain -> ESP)', object%get_alpha3(), string('ESP') )
  call ut%assert_equal( 'Constructor (Spain -> #)', object%get_numeric(), 724 )
  call ut%assert_equal( 'Constructor (Spain -> Capital)', object%get_capital_name(), string('Madrid') )
  call ut%assert_equal( 'Constructor (Spain -> Region)', object%get_region_name(), string('Europe') )
  call ut%assert_equal( 'Constructor (Spain -> Longitude)', object%get_longitude(), -4.0 )
  call ut%assert_equal( 'Constructor (Spain -> Latitude)', object%get_latitude(), 40.0 )

! Constructor from name (lowercase)
  object = iso3166_country_code( 'spain' )
  call ut%assert_equal( 'Constructor (spain)', object%get_name(), string('Spain') )
  call ut%assert_equal( 'Constructor (spain -> ES)', object%get_alpha2(), string('ES') )
  call ut%assert_equal( 'Constructor (spain -> ESP)', object%get_alpha3(), string('ESP') )
  call ut%assert_equal( 'Constructor (spain -> #)', object%get_numeric(), 724 )
  call ut%assert_equal( 'Constructor (spain -> Capital)', object%get_capital_name(), string('Madrid') )
  call ut%assert_equal( 'Constructor (spain -> Region)', object%get_region_name(), string('Europe') )
  call ut%assert_equal( 'Constructor (spain -> Longitude)', object%get_longitude(), -4.0 )
  call ut%assert_equal( 'Constructor (spain -> Latitude)', object%get_latitude(), 40.0 )

! Not found
  object = iso3166_country_code( 'zzz' )
  call ut%assert_equal( 'Constructor (zzz)', object%get_alpha3(), string('') )
  call ut%assert_equal( 'Constructor (zzz -> alpha2)', object%get_alpha2(), string('') )
  call ut%assert_equal( 'Constructor (zzz -> #)', object%get_numeric(), 0 )
  call ut%assert_equal( 'Constructor (zzz -> Country)', object%get_name(), string('') )
  call ut%assert_equal( 'Constructor (zzz -> Capital)', object%get_capital_name(), string('') )
  call ut%assert_equal( 'Constructor (zzz -> Region)', object%get_region_name(), string('') )
  call ut%assert_equal( 'Constructor (zzz -> Longitude)', object%get_longitude(), 0.0 )
  call ut%assert_equal( 'Constructor (zzz -> Latitude)', object%get_latitude(), 0.0 )

! Invalid entry
  object = iso3166_country_code( 'z' )
  call ut%assert_equal( 'Constructor (z)', object%get_alpha3(), string('') )
  call ut%assert_equal( 'Constructor (z -> alpha2)', object%get_alpha2(), string('') )
  call ut%assert_equal( 'Constructor (z -> #)', object%get_numeric(), 0 )
  call ut%assert_equal( 'Constructor (z -> Country)', object%get_name(), string('') )
  call ut%assert_equal( 'Constructor (z -> Capital)', object%get_capital_name(), string('') )
  call ut%assert_equal( 'Constructor (z -> Region)', object%get_region_name(), string('') )
  call ut%assert_equal( 'Constructor (z -> Longitude)', object%get_longitude(), 0.0 )
  call ut%assert_equal( 'Constructor (z -> Latitude)', object%get_latitude(), 0.0 )

! String interface
  object = iso3166_country_code( string('ES') )
  call ut%assert_equal( 'Constructor string (ES)', object%get_alpha2(), string('ES') )
  call ut%assert_equal( 'Constructor string (ES -> ESP)', object%get_alpha3(), string('ESP') )
  call ut%assert_equal( 'Constructor string (ES -> #)', object%get_numeric(), 724 )
  call ut%assert_equal( 'Constructor string (ES -> Country)', object%get_name(), string('Spain') )
  call ut%assert_equal( 'Constructor string (ES -> Capital)', object%get_capital_name(), string('Madrid') )
  call ut%assert_equal( 'Constructor string (ES -> Region)', object%get_region_name(), string('Europe') )
  call ut%assert_equal( 'Constructor string (ES -> Longitude)', object%get_longitude(), -4.0 )
  call ut%assert_equal( 'Constructor string (ES -> Latitude)', object%get_latitude(), 40.0 )

! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_m_iso3166_country_code_test_001

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_iso3166_country_code_test_002( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! The tested strcuture
  type(t_iso3166_country_code) :: object

! Local variables
  integer :: status

! Reset error handling structures
  call msg%reset_error()

! Constructor from numeric code
  object = iso3166_country_code( 724 )
  call ut%assert_equal( 'Constructor (724)', object%get_numeric(), 724 )
  call ut%assert_equal( 'Constructor (724 -> ES)', object%get_alpha2(), string('ES') )
  call ut%assert_equal( 'Constructor (724 -> ESP)', object%get_alpha3(), string('ESP') )
  call ut%assert_equal( 'Constructor (724 -> Country)', object%get_name(), string('Spain') )
  call ut%assert_equal( 'Constructor (724 -> Capital)', object%get_capital_name(), string('Madrid') )
  call ut%assert_equal( 'Constructor (724 -> Region)', object%get_region_name(), string('Europe') )
  call ut%assert_equal( 'Constructor (724 -> Longitude)', object%get_longitude(), -4.0 )
  call ut%assert_equal( 'Constructor (724 -> Latitude)', object%get_latitude(), 40.0 )

! Not found
  object = iso3166_country_code( 11 )
  call ut%assert_equal( 'Constructor (50)', object%get_numeric(), 0 )
  call ut%assert_equal( 'Constructor (50 -> alpha2)', object%get_alpha2(), string('') )
  call ut%assert_equal( 'Constructor (50 -> alpha3)', object%get_alpha3(), string('') )
  call ut%assert_equal( 'Constructor (50 -> Country)', object%get_name(), string('') )
  call ut%assert_equal( 'Constructor (50 -> Capital)', object%get_capital_name(), string('') )
  call ut%assert_equal( 'Constructor (50 -> Region)', object%get_region_name(), string('') )
  call ut%assert_equal( 'Constructor (50 -> Longitude)', object%get_longitude(), 0.0 )
  call ut%assert_equal( 'Constructor (50 -> Latitude)', object%get_latitude(), 0.0 )

! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_m_iso3166_country_code_test_002

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_iso3166_country_code_test_003( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! The tested strcuture
  type(t_iso3166_country_code) :: object

! Local variables
  integer :: status

! Reset error handling structures
  call msg%reset_error()

! Constructor from geolocation
  object = iso3166_country_code( -3.0, 41.0 )
  call ut%assert_equal( 'Constructor (geoloc -> ES)', object%get_alpha2(), string('ES') )
  call ut%assert_equal( 'Constructor (geoloc -> ESP)', object%get_alpha3(), string('ESP') )
  call ut%assert_equal( 'Constructor (geoloc -> #)', object%get_numeric(), 724 )
  call ut%assert_equal( 'Constructor (geoloc -> Country)', object%get_name(), string('Spain') )
  call ut%assert_equal( 'Constructor (geoloc -> Capital)', object%get_capital_name(), string('Madrid') )
  call ut%assert_equal( 'Constructor (geoloc -> Region)', object%get_region_name(), string('Europe') )
  call ut%assert_equal( 'Constructor (geoloc -> Longitude)', object%get_longitude(), -4.0 )
  call ut%assert_equal( 'Constructor (geoloc -> Latitude)', object%get_latitude(), 40.0 )

! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_m_iso3166_country_code_test_003

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_iso3166_country_code_test_004( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! The tested strcuture
  type(t_iso3166_country_code) :: object

! Local variables
  integer :: status

! Reset error handling structures
  call msg%reset_error()

! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_m_iso3166_country_code_test_004

end module unit_m_iso3166_country_code_tests
