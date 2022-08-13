module unit_m_iso8601_date_time_tests

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

  use m_iso8601_date
  use m_iso8601_time
  use m_iso8601_date_time

!---End of use statements-------------------------------------------------------

  implicit none

!---Public/Private declarations-------------------------------------------------

  private


  public unit_m_iso8601_date_time_test_001

  public unit_m_iso8601_date_time_test_002

  public unit_m_iso8601_date_time_test_003

  public manager

!---End of public/private declarations------------------------------------------

  character(len=*), parameter :: package = 'utilities'
  character(len=*), parameter :: module = 'm_iso8601_date_time'
  
!---Declaration of local variables----------------------------------------------

! The unit test management structures
  type(t_xfunit_manager), allocatable, save :: manager
  type(t_xfunit_suite), allocatable, save :: suite

! The error handling structure
  type(t_messages), save :: msg


!---End of declaration of local variables---------------------------------------

! Date/time reference initialisation variables (default integer)
  integer, parameter :: year        = 2014
  integer, parameter :: month       =    1
  integer, parameter :: day         =    5
  integer, parameter :: hour        =   23
  integer, parameter :: minute      =   17
  integer, parameter :: second      =   53
  integer, parameter :: millisecond =  356
  integer, parameter :: microsecond =  114
  integer, parameter :: nanosecond  =  814
  character(len=*), parameter :: zone = '+01:30'

! Date/time reference initialisation variables (specific integer)
  integer(kind=2), parameter :: year2        = 2013_2
  integer(kind=1), parameter :: month2       =    2_1
  integer(kind=1), parameter :: day2         =    7_1
  integer(kind=1), parameter :: hour2        =   13_1
  integer(kind=1), parameter :: minute2      =   27_1
  integer(kind=1), parameter :: second2      =   43_1
  integer(kind=2), parameter :: millisecond2 =  156_2
  integer(kind=2), parameter :: microsecond2 =  314_2
  integer(kind=2), parameter :: nanosecond2  =  914_2
  character(len=*), parameter :: zone2 = '-01:00'

! Reference formats for the expected outputs
  character(len=*), parameter :: date_time_fmt = '(i4.4,2("-",i2.2),"T",i2.2,2(":",i2.2),a)'
  character(len=*), parameter :: date_time_base_fmt = '(i4.4,2(i2.2),"T",i2.2,2(i2.2),a)'

!---Executable Code-------------------------------------------------------------

contains

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_iso8601_date_time_test_001( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! The ISO8601 type
  type(t_iso8601_date_time) :: iso

! Local variables
  character(len=32) :: test
  type(t_iso8601_date) :: idate
  type(t_iso8601_time) :: itime
  integer, dimension(8) :: values
  character(len=5) :: lzone
  

! Reset error handling structures
  call msg%reset_error()

! Default minimum constructors
  iso = iso8601_date_time( year, month, day )
  write( test, date_time_fmt ) year, month, day, 0, 0, 0, 'Z'
  call ut%assert_equal( 'Default minimum; default integer', &
                        iso%to_string(), test )
  iso = iso8601_date_time( year2, month2, day2 )
  write( test, date_time_fmt ) year2, month2, day2, 0, 0, 0, 'Z'
  call ut%assert_equal( 'Default minimum; specific integer', &
                        iso%to_string(), test )

! Default date/time constructors
  iso = iso8601_date_time( year, month, day, hour, minute, second )
  write( test, date_time_fmt ) year, month, day, hour, minute, second, 'Z'
  call ut%assert_equal( 'All date/time values; default integer', &
                        iso%to_string(), test )
  iso = iso8601_date_time( year2, month2, day2, hour2, minute2, second2 )
  write( test, date_time_fmt ) year2, month2, day2, hour2, minute2, second2, 'Z'
  call ut%assert_equal( 'All date/time values; specific integer', &
                        iso%to_string(), test )

! Default date/time constructors with missing intermediate value
  iso = iso8601_date_time( year, month, day, hour, second=second )
  write( test, date_time_fmt ) year, month, day, hour, 0, second, 'Z'
  call ut%assert_equal( 'Missing time value; default integer', &
                        iso%to_string(), test )
  iso = iso8601_date_time( year2, month2, day2, hour2, second=second2 )
  write( test, date_time_fmt ) year2, month2, day2, hour2, 0, second2, 'Z'
  call ut%assert_equal( 'Missing time value; specific integer', &
                        iso%to_string(), test )

! Date/time minimum constructors
  iso = iso8601_date_time( year, month, day )
  write( test, date_time_fmt ) year, month, day, 0, 0, 0, 'Z'
  call ut%assert_equal( 'Date/time minimum; default integer', &
                        iso%to_string(), test )
  iso = iso8601_date_time( year2, month2, day2 )
  write( test, date_time_fmt ) year2, month2, day2, 0, 0, 0, 'Z'
  call ut%assert_equal( 'Date/time minimum; specific integer', &
                        iso%to_string(), test )

! Date/time minimum constructors with time zone
  iso = iso8601_date_time( year, month, day, time_zone=zone )
  write( test, date_time_fmt ) year, month, day, 0, 0, 0, zone
  call ut%assert_equal( 'Date/time minimum with zone; default integer', &
                        iso%to_string(zone=.true.), test )
  iso = iso8601_date_time( year2, month2, day2, time_zone=zone2 )
  write( test, date_time_fmt ) year2, month2, day2, 0, 0, 0, zone2
  call ut%assert_equal( 'Date/time minimum with zone; specific integer', &
                        iso%to_string(zone=.true.), test )

! Date/time generic constructor
  idate = iso8601_date( year, month, day )
  itime = iso8601_time( hour, minute, second )
  iso = iso8601_date_time( idate, itime )
  write( test, date_time_fmt ) year, month, day, hour, minute, second, 'Z'
  call ut%assert_equal( 'From date and time', iso%to_string(), test )
  
! Machine time constructor
  call date_and_time( zone=lzone, values=values )
  iso = iso8601_date_time_now()
  write( test, date_time_fmt ) values(1), values(2), values(3), values(5), values(6), values(7), &
                               lzone(1:3) // ":" // lzone(4:5)
  call ut%assert_equal( 'From machine time', iso%to_string(), test )

! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

end subroutine unit_m_iso8601_date_time_test_001

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_iso8601_date_time_test_002( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! The ISO8601 type
  type(t_iso8601_date_time) :: iso

! Reset error handling structures
  call msg%reset_error()

! Initialise
  iso = iso8601_date_time( year, month, day, hour, minute, second )

! Year
  call iso%set_year( 2015 )
  call ut%assert_equal( 'Year get/set', iso%get_year(), 2015_2 )
  call iso%set_year( 2016_2 )
  call ut%assert_equal( 'Year get/set', iso%get_year(), 2016_2 )

! Leap year
  call ut%assert_true( 'Leap year (2016)', iso%is_leap_year() )

! Month
  call iso%set_month( 10 )
  call ut%assert_equal( 'Month get/set', iso%get_month(), 10_1 )
  call iso%set_month( 11_1 )
  call ut%assert_equal( 'Month get/set', iso%get_month(), 11_1 )

! Day
  call iso%set_day( 23 )
  call ut%assert_equal( 'Day get/set', iso%get_day(), 23_1 )
  call iso%set_day( 24_1 )
  call ut%assert_equal( 'Day get/set', iso%get_day(), 24_1 )

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

end subroutine unit_m_iso8601_date_time_test_002

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_iso8601_date_time_test_003( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! The ISO8601 type
  type(t_iso8601_date_time) :: iso

! Local variables
  character(len=32) :: test

! Reset error handling structures
  call msg%reset_error()

! Formatting for date/time
! This testing is not exhaustive becaus the architecture of t_iso8601_date_time
! is such that the detailed tesing of all combinations is covered by the 
! unit testing of t_iso8601_date and t_iso8601_time
  iso = iso8601_date_time( year, month, day, hour, minute, second )
  write( test, date_time_fmt ) year, month, day, hour, minute, second, 'Z'
  call ut%assert_equal( 'iso8601_YYYYMMDDhhmmss (default)', &
                        iso%to_string(), test )
  call ut%assert_equal( 'iso8601_YYYYMMDDhhmmss', &
                        iso%to_string(iso8601_YYYYMMDDhhmmss), test )
  write( test, date_time_fmt ) year, month, day, hour, minute, second, ''
  call ut%assert_equal( 'iso8601_YYYYMMDDhhmmss (no zone)', &
                        iso%to_string(iso8601_YYYYMMDDhhmmss,zone=.false.), test )
  write( test, date_time_fmt ) year, month, day, hour, minute, second, 'Z'
  call ut%assert_equal( 'iso8601_YYYYMMDDhhmmss (with sign)', &
                        iso%to_string(iso8601_YYYYMMDDhhmmss,sign=.true.), '+'//test )
  write( test, date_time_base_fmt ) year, month, day, hour, minute, second, 'Z'
  call ut%assert_equal( 'iso8601_YYYYMMDDhhmmss (base format)', &
                        iso%to_string(iso8601_YYYYMMDDhhmmss,size=iso8601_base_format), test )

! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

end subroutine unit_m_iso8601_date_time_test_003

end module unit_m_iso8601_date_time_tests

