module unit_m_iso8601_time_tests

!-------------------------------------------------------------------------------
! Copyright : 2022, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
! Synopsis  : Unit tests for m_iso8601
!
! License   : This file is part of Fommons.
!
!             Fommons is free software: you can redistribute it and/or modify
!             it under the terms of the GNU Lesser General Public License as
!             published by the Free Software Foundation, either version 3 of
!             the License, or (at your option) any later version.
!
!             Fommons is distributed in the hope that it will be useful,
!             but WITHOUT ANY WARRANTY; without even the implied warranty of
!             MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
!             See the GNU Lesser General Public License for more details.
!
!             You should have received a copy of the GNU Lesser General Public
!             License along with Fommons.  
!             If not, see <http://www.gnu.org/licenses/>.
!-------------------------------------------------------------------------------

!---USE statements--------------------------------------------------------------

  use m_string

  use m_xfunit
  use m_messages
  use m_util_convert

  use m_iso8601_time

!---End of use statements-------------------------------------------------------

  implicit none

!---Public/Private declarations-------------------------------------------------

  private


  public unit_m_iso8601_time_test_001

  public unit_m_iso8601_time_test_002

  public unit_m_iso8601_time_test_003

  public manager

!---End of public/private declarations------------------------------------------

  character(len=*), parameter :: package = 'utilities'
  character(len=*), parameter :: module = 'm_iso8601_time'
  
!---Declaration of local variables----------------------------------------------

! The unit test management structures
  type(t_xfunit_manager), allocatable, save :: manager
  type(t_xfunit_suite), allocatable, save :: suite

! The error handling structure
  type(t_messages), save :: msg


!---End of declaration of local variables---------------------------------------

! Date/time reference initialisation variables (default integer)
  integer, parameter :: hour        =   23
  integer, parameter :: minute      =   17
  integer, parameter :: second      =   53
  integer, parameter :: millisecond =  352
  integer, parameter :: microsecond =  114
  integer, parameter :: nanosecond  =  814
  character(len=*), parameter :: zone_hour = '+00'
  character(len=*), parameter :: zone_full = '+01:30'

! Date/time reference initialisation variables (specific integer)
  integer(kind=1), parameter :: hour2        =   13_1
  integer(kind=1), parameter :: minute2      =   27_1
  integer(kind=1), parameter :: second2      =   43_1
  integer(kind=2), parameter :: millisecond2 =  156_2
  integer(kind=2), parameter :: microsecond2 =  314_2
  integer(kind=2), parameter :: nanosecond2  =  914_2
  character(len=*), parameter :: zone2_hour = '-00'
  character(len=*), parameter :: zone2_full = '-01:00'

! Reference formats for the expected outputs
  character(len=*), parameter :: time_fmt = '(i2.2,2(":",i2.2),a)'

!---Executable Code-------------------------------------------------------------

contains

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_iso8601_time_test_001( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! The ISO8601 type
  type(t_iso8601_time) :: iso

! Local variables
  character(len=32) :: test
  integer, dimension(8) :: values
  character(len=5) :: lzone
  

! Reset error handling structures
  call msg%reset_error()

! Default time constructors
  iso = iso8601_time()
  write( test, time_fmt ) 0, 0, 0, 'Z'
  call ut%assert_equal( 'Default constructor', iso%to_string(), test )
  iso = iso8601_time( hour, minute, second )
  write( test, time_fmt ) hour, minute, second, 'Z'
  call ut%assert_equal( 'Minimum constructor; default integer', &
                        iso%to_string(), test )
  iso = iso8601_time( hour2, minute2, second2 )
  write( test, time_fmt ) hour2, minute2, second2, 'Z'
  call ut%assert_equal( 'Minimum constructor; specific integer', &
                        iso%to_string(), test )

! Time minimum constructors with time zone
  iso = iso8601_time( hour, minute, second, time_zone=zone_full )
  write( test, time_fmt ) hour, minute, second, zone_full
  call ut%assert_equal( 'Time minimum with full zone; default integer', &
                        iso%to_string(), test )
  iso = iso8601_time( hour, minute, second, time_zone=zone_hour )
  write( test, time_fmt ) hour, minute, second, zone_hour
  call ut%assert_equal( 'Time minimum with zone; default integer', &
                        iso%to_string(), test )
  iso = iso8601_time( hour, minute, second, time_zone='' )
  write( test, time_fmt ) hour, minute, second, 'Z'
  call ut%assert_equal( 'Time minimum with empty zone; deault integer', &
                        iso%to_string(), test )
  iso = iso8601_time( hour2, minute2, second2, time_zone=zone2_full )
  write( test, time_fmt ) hour2, minute2, second2, zone2_full
  call ut%assert_equal( 'Time minimum with full zone; specific integer', &
                        iso%to_string(), test )
  iso = iso8601_time( hour2, minute2, second2, time_zone=zone2_hour )
  write( test, time_fmt ) hour2, minute2, second2, zone2_hour
  call ut%assert_equal( 'Time minimum with zone; specific integer', &
                        iso%to_string(), test )
  iso = iso8601_time( hour2, minute2, second2, time_zone='' )
  write( test, time_fmt ) hour2, minute2, second2, 'Z'
  call ut%assert_equal( 'Time minimum with empty zone; specific integer', &
                        iso%to_string(), test )

! Full constructor
  iso = iso8601_time( hour, minute, second, &
                      millisecond, microsecond, nanosecond, &
                      time_zone=zone_full )
  call ut%assert_equal( 'Full constructor millisecond; default integer', &
                        iso%get_millisecond(), int(millisecond,2) )
  call ut%assert_equal( 'Full constructor micorsecond;default integer', &
                        iso%get_microsecond(), int(microsecond,2) )
  call ut%assert_equal( 'Full constructor nanosecond; default integer', &
                        iso%get_nanosecond(), int(nanosecond,2) )
  iso = iso8601_time( hour, minute, second, &
                      millisecond, nanosecond=nanosecond, &
                      time_zone=zone_full )
  call ut%assert_equal( 'Full incomplete constructor millisecond; default integer', &
                        iso%get_millisecond(), int(millisecond,2) )
  call ut%assert_equal( 'Full incomplete constructor micorsecond; default integer', &
                        iso%get_microsecond(), 0_2 )
  call ut%assert_equal( 'Full incomplete constructor nanosecond; default integer', &
                        iso%get_nanosecond(), 0_2 )
  iso = iso8601_time( hour2, minute2, second2, &
                      millisecond2, microsecond2, nanosecond2, &
                      time_zone=zone_full )
  call ut%assert_equal( 'Full constructor millisecond; specific integer', &
                        iso%get_millisecond(), millisecond2 )
  call ut%assert_equal( 'Full constructor micorsecond;specific integer', &
                        iso%get_microsecond(), microsecond2 )
  call ut%assert_equal( 'Full constructor nanosecond; specific integer', &
                        iso%get_nanosecond(), nanosecond2 )
  iso = iso8601_time( hour2, minute2, second2, &
                      millisecond2, nanosecond=nanosecond2, &
                      time_zone=zone_full )
  call ut%assert_equal( 'Full incomplete constructor millisecond; specific integer', &
                        iso%get_millisecond(), millisecond2 )
  call ut%assert_equal( 'Full incomplete constructor micorsecond; specific integer', &
                        iso%get_microsecond(), 0_2 )
  call ut%assert_equal( 'Full incomplete constructor nanosecond; specific integer', &
                        iso%get_nanosecond(), 0_2 )

! Machine time constructor
  call date_and_time( zone=lzone, values=values )
  iso = iso8601_time_now()
  write( test, time_fmt ) values(5), values(6), values(7), &
                          lzone(1:3) // ":" // lzone(4:5)
  call ut%assert_equal( 'From machine time', iso%to_string(), test )

! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

end subroutine unit_m_iso8601_time_test_001

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_iso8601_time_test_002( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! The ISO8601 type
  type(t_iso8601_time) :: iso

! Reset error handling structures
  call msg%reset_error()

! Initialise
  iso = iso8601_time( hour, minute, second )

! Hour
  call iso%set_hour( 5 )
  call ut%assert_equal( 'Hour get/set', iso%get_hour(), 5_1 )
  call iso%set_hour( 6_1 )
  call ut%assert_equal( 'Hour get/set', iso%get_hour(), 6_1 )

! Minute
  call iso%set_minute( 45 )
  call ut%assert_equal( 'Minute get/set', iso%get_minute(), 45_1 )
  call iso%set_minute( 46_1 )
  call ut%assert_equal( 'Minute get/set', iso%get_minute(), 46_1 )

! Second
  call iso%set_second( 17 )
  call ut%assert_equal( 'Second get/set', iso%get_second(), 17_1 )
  call iso%set_second( 18_1 )
  call ut%assert_equal( 'Second get/set', iso%get_second(), 18_1 )

! Millisecond
  call iso%set_millisecond( 171 )
  call ut%assert_equal( 'Millisecond get/set', iso%get_millisecond(), 171_2 )
  call iso%set_millisecond( 181_2 )
  call ut%assert_equal( 'Millisecond get/set', iso%get_millisecond(), 181_2 )

! Microsecond
  call iso%set_microsecond( 671 )
  call ut%assert_equal( 'Microsecond get/set', iso%get_microsecond(), 671_2 )
  call iso%set_microsecond( 681_2 )
  call ut%assert_equal( 'Microsecond get/set', iso%get_microsecond(), 681_2 )

! Nanosecond
  call iso%set_nanosecond( 371 )
  call ut%assert_equal( 'Nanosecond get/set', iso%get_nanosecond(), 371_2 )
  call iso%set_nanosecond( 381_2 )
  call ut%assert_equal( 'Nanosecond get/set', iso%get_nanosecond(), 381_2 )

! Time zone
  call iso%set_time_zone( "+00:30" )
  call ut%assert_equal( 'Time zone get/set', iso%get_time_zone(), "+00:30" )

! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

end subroutine unit_m_iso8601_time_test_002

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_iso8601_time_test_003( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! The ISO8601 type
  type(t_iso8601_time) :: iso

! Local variables
  character(len=32) :: test
  character(len=:), allocatable :: zone_base

! Reset error handling structures
  call msg%reset_error()

! Construct full structure
  iso = iso8601_time( hour, minute, second, &
                      millisecond, microsecond, nanosecond, &
                      time_zone=zone_full )

! Formatting extended
  write( test, time_fmt ) hour, minute, second, ''
  call ut%assert_equal( 'iso8601_hhmmss (default)', &
                        iso%to_string(), trim(test)//trim(zone_full) )
  call ut%assert_equal( 'iso8601_hhmmss', &
                        iso%to_string(iso8601_hhmmss), trim(test)//trim(zone_full) )
  call ut%assert_equal( 'iso8601_hhmmss no zone', &
                        iso%to_string(iso8601_hhmmss,zone=.false.), trim(test) )
  write( test, '(i2.2,2(":",i2.2),".",i1.1)' ) hour, minute, second, nint(millisecond/100.0)
  call ut%assert_equal( 'iso8601_hhmmss_s', &
                        iso%to_string(iso8601_hhmmss_s), trim(test)//trim(zone_full) )
  write( test, '(i2.2,2(":",i2.2),".",i2.2)' ) hour, minute, second, nint(millisecond/10.0)
  call ut%assert_equal( 'iso8601_hhmmss_ss', &
                        iso%to_string(iso8601_hhmmss_ss), trim(test)//trim(zone_full) )
  write( test, '(i2.2,2(":",i2.2),".",i3.3)' ) hour, minute, second, millisecond
  call ut%assert_equal( 'iso8601_hhmmss_sss', &
                        iso%to_string(iso8601_hhmmss_sss), trim(test)//trim(zone_full) )
  write( test, '(i2.2,2(":",i2.2),".",2i3.3)' ) hour, minute, second, millisecond, microsecond
  call ut%assert_equal( 'iso8601_hhmmss_sss_sss', &
                        iso%to_string(iso8601_hhmmss_sss_sss), trim(test)//trim(zone_full) )
  write( test, '(i2.2,2(":",i2.2),".",3i3.3)' ) hour, minute, second, millisecond, microsecond, nanosecond
  call ut%assert_equal( 'iso8601_hhmmss_sss_sss_sss', &
                        iso%to_string(iso8601_hhmmss_sss_sss_sss), trim(test)//trim(zone_full) )
  write( test, '(i2.2,":",i2.2)' ) hour, minute
  call ut%assert_equal( 'iso8601_hhmm', &
                        iso%to_string(iso8601_hhmm), trim(test)//trim(zone_full) )
  write( test, '(i2.2)' ) hour
  call ut%assert_equal( 'iso8601_hh', &
                        iso%to_string(iso8601_hh), trim(test)//trim(zone_full) )

! Formatting base
  zone_base = zone_full(1:3)//zone_full(5:6)
  write( test, '(3i2.2)' ) hour, minute, second
  call ut%assert_equal( 'iso8601_hhmmss base (default)', &
                        iso%to_string(size=iso8601_time_base_format), trim(test)//trim(zone_base) )
  call ut%assert_equal( 'iso8601_hhmmss base', &
                        iso%to_string(iso8601_hhmmss,size=iso8601_time_base_format), trim(test)//trim(zone_base) )
  call ut%assert_equal( 'iso8601_hhmmss base no zone', &
                        iso%to_string(iso8601_hhmmss,size=iso8601_time_base_format,zone=.false.), trim(test) )
  write( test, '(3i2.2,".",i1.1)' ) hour, minute, second, nint(millisecond/100.0)
  call ut%assert_equal( 'iso8601_hhmmss_s base', &
                        iso%to_string(iso8601_hhmmss_s,size=iso8601_time_base_format), trim(test)//trim(zone_base) )
  write( test, '(3i2.2,".",i2.2)' ) hour, minute, second, nint(millisecond/10.0)
  call ut%assert_equal( 'iso8601_hhmmss_ss base', &
                        iso%to_string(iso8601_hhmmss_ss,size=iso8601_time_base_format), trim(test)//trim(zone_base) )
  write( test, '(3i2.2,".",i3.3)' ) hour, minute, second, millisecond
  call ut%assert_equal( 'iso8601_hhmmss_sss base', &
                        iso%to_string(iso8601_hhmmss_sss,size=iso8601_time_base_format), trim(test)//trim(zone_base) )
  write( test, '(3i2.2,".",2i3.3)' ) hour, minute, second, millisecond, microsecond
  call ut%assert_equal( 'iso8601_hhmmss_sss_sss base', &
                        iso%to_string(iso8601_hhmmss_sss_sss,size=iso8601_time_base_format), trim(test)//trim(zone_base) )
  write( test, '(3i2.2,".",3i3.3)' ) hour, minute, second, millisecond, microsecond, nanosecond
  call ut%assert_equal( 'iso8601_hhmmss_sss_sss_sss base', &
                        iso%to_string(iso8601_hhmmss_sss_sss_sss,size=iso8601_time_base_format), trim(test)//trim(zone_base) )
  write( test, '(2i2.2)' ) hour, minute
  call ut%assert_equal( 'iso8601_hhmm base', &
                        iso%to_string(iso8601_hhmm,size=iso8601_time_base_format), trim(test)//trim(zone_base) )
  write( test, '(i2.2)' ) hour
  call ut%assert_equal( 'iso8601_hh base', &
                        iso%to_string(iso8601_hh,size=iso8601_time_base_format), trim(test)//trim(zone_base) )


! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

end subroutine unit_m_iso8601_time_test_003

end module unit_m_iso8601_time_tests

