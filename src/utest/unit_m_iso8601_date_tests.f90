module unit_m_iso8601_date_tests

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

!---End of use statements-------------------------------------------------------

  implicit none

!---Public/Private declarations-------------------------------------------------

  private


  public unit_m_iso8601_date_test_001

  public unit_m_iso8601_date_test_002

  public unit_m_iso8601_date_test_003

  public unit_m_iso8601_date_test_004

  public manager

!---End of public/private declarations------------------------------------------

  character(len=*), parameter :: package = 'utilities'
  character(len=*), parameter :: module = 'm_iso8601_date'
  
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
  integer, parameter :: doy         =    5
  integer, parameter :: week        =    1
  integer, parameter :: dow         =    7

! Date/time reference initialisation variables (specific integer)
  integer(kind=2), parameter :: year2        = 2013_2
  integer(kind=1), parameter :: month2       =    2_1
  integer(kind=1), parameter :: day2         =    7_1

! Reference formats for the expected outputs
  character(len=*), parameter :: date_fmt = '(i4.4,2("-",i2.2))'
  character(len=*), parameter :: date_base_fmt = '(i4.4,2(i2.2))'

!---Executable Code-------------------------------------------------------------

contains

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_iso8601_date_test_001( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! The ISO8601 type
  type(t_iso8601_date) :: iso

! Local variables
  character(len=32) :: test
  type(t_iso8601_date) :: idate
  integer, dimension(8) :: values
  character(len=5) :: lzone
  

! Reset error handling structures
  call msg%reset_error()

! Default constructors
  iso = iso8601_date( year, month, day )
  write( test, date_fmt ) year, month, day
  call ut%assert_equal( 'Default; default integer', iso%to_string(), test )
  iso = iso8601_date( year2, month2, day2 )
  write( test, date_fmt ) year2, month2, day2
  call ut%assert_equal( 'Default; specific integer', iso%to_string(), test )

! Machine time constructor
  call date_and_time( zone=lzone, values=values )
  iso = iso8601_date_now()
  write( test, date_fmt ) values(1), values(2), values(3)
  call ut%assert_equal( 'From machine time', iso%to_string(), test )

! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

end subroutine unit_m_iso8601_date_test_001

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_iso8601_date_test_002( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! The ISO8601 type
  type(t_iso8601_date) :: iso

! Reset error handling structures
  call msg%reset_error()

! Initialise
  iso = iso8601_date( year, month, day )

! Year
  call iso%set_year( 2015 )
  call ut%assert_equal( 'Year get/set', iso%get_year(), 2015_2 )
  call iso%set_year( 2016_2 )
  call ut%assert_equal( 'Year get/set', iso%get_year(), 2016_2 )

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


! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

end subroutine unit_m_iso8601_date_test_002

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_iso8601_date_test_003( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! The ISO8601 type
  type(t_iso8601_date) :: iso

! Local variables
  character(len=32) :: test
  type(t_iso8601_date) :: idate
  character(len=:), allocatable :: isostring

! Reset error handling structures
  call msg%reset_error()

! Days in a month
  iso = iso8601_date( 2015, 1, 2 )
  call ut%assert_equal( 'Days in January', iso%days_in_month(), 31 )
  iso = iso8601_date( 2015, 2, 2 )
  call ut%assert_equal( 'Days in February', iso%days_in_month(), 28 )
  iso = iso8601_date( 2016, 2, 2 )
  call ut%assert_equal( 'Days in February (leap)', iso%days_in_month(), 29 )
  iso = iso8601_date( 2015, 3, 2 )
  call ut%assert_equal( 'Days in March', iso%days_in_month(), 31 )
  iso = iso8601_date( 2015, 4, 2 )
  call ut%assert_equal( 'Days in April', iso%days_in_month(), 30 )
  iso = iso8601_date( 2015, 5, 2 )
  call ut%assert_equal( 'Days in May', iso%days_in_month(), 31 )
  iso = iso8601_date( 2015, 6, 2 )
  call ut%assert_equal( 'Days in June', iso%days_in_month(), 30 )
  iso = iso8601_date( 2015, 7, 2 )
  call ut%assert_equal( 'Days in July', iso%days_in_month(), 31 )
  iso = iso8601_date( 2015, 8, 2 )
  call ut%assert_equal( 'Days in August', iso%days_in_month(), 31 )
  iso = iso8601_date( 2015, 9, 2 )
  call ut%assert_equal( 'Days in September', iso%days_in_month(), 30 )
  iso = iso8601_date( 2015, 10, 2 )
  call ut%assert_equal( 'Days in Ocotober', iso%days_in_month(), 31 )
  iso = iso8601_date( 2015, 11, 2 )
  call ut%assert_equal( 'Days in November', iso%days_in_month(), 30 )
  iso = iso8601_date( 2015, 12, 2 )
  call ut%assert_equal( 'Days in December', iso%days_in_month(), 31 )
  call ut%assert_equal( 'Days in January (static)', iso8601_date_days_in_month(1), 31 )
  call ut%assert_equal( 'Days in February', iso8601_date_days_in_month(2), 28 )
  call ut%assert_equal( 'Days in February (leap)', iso8601_date_days_in_month(2,2016), 29 )
  call ut%assert_equal( 'Days in March', iso8601_date_days_in_month(3), 31 )
  call ut%assert_equal( 'Out of range (13)', iso8601_date_days_in_month(13), 0 )
  call ut%assert_equal( 'Out of range (0)', iso8601_date_days_in_month(0), 0 )

! Check for leap year
  iso = iso8601_date( 2015, 10, 2 )
  call ut%assert_false( 'Not leap year 2015-10-02', iso%is_leap_year() )
  iso = iso8601_date( 2016, 10, 2 )
  call ut%assert_true( 'Leap year 2016-10-02', iso%is_leap_year() )
  iso = iso8601_date( 1900, 10, 2 )
  call ut%assert_false( 'Not leap year (100) 1900-10-02', iso%is_leap_year() )
  iso = iso8601_date( 2000, 10, 2 )
  call ut%assert_true( 'Leap year (400) 2000-10-02', iso%is_leap_year() )
  call ut%assert_false( 'Not leap year (static) 2015', iso8601_date_is_leap_year(2015) )
  call ut%assert_true( 'Leap year (static) 2016', iso8601_date_is_leap_year(2016) )

! Compute day of the year
  iso = iso8601_date( 2016, 1, 1 )
  call ut%assert_equal( 'Reference 2016-01-01', iso%day_of_year(), 1 )
  iso = iso8601_date( 2015, 5, 14 )
  call ut%assert_equal( 'No leap 2015-05-14', iso%day_of_year(), 134 )
  iso = iso8601_date( 2016, 5, 14 )
  call ut%assert_equal( 'Beyond leap 2016-05-14', iso%day_of_year(), 135 )
  iso = iso8601_date( 2016, 2, 14 )
  call ut%assert_equal( 'Before leap 2016-02-14', iso%day_of_year(), 45 )
  iso = iso8601_date( 2016, 2, 29 )
  call ut%assert_equal( 'At leap 2016-02-14', iso%day_of_year(), 60 )
  iso = iso8601_date( 2016, 3, 1 )
  call ut%assert_equal( 'One after leap 2016-02-14', iso%day_of_year(), 61 )

! Day of the week
  iso = iso8601_date( 1999, 12, 1 )
  call ut%assert_equal( 'Day of week 1999-12-01', iso%day_of_week(), 3 )
  iso = iso8601_date( 1999, 12, 29 )
  call ut%assert_equal( 'Day of week 1999-12-29', iso%day_of_week(), 3 )
  iso = iso8601_date( 1999, 12, 30 )
  call ut%assert_equal( 'Day of week 1999-12-30', iso%day_of_week(), 4 )
  iso = iso8601_date( 1999, 12, 31 )
  call ut%assert_equal( 'Day of week 1999-12-31', iso%day_of_week(), 5 )
  iso = iso8601_date( 2000, 1, 1 )
  call ut%assert_equal( 'Day of week 2000-01-01', iso%day_of_week(), 6 )
  iso = iso8601_date( 2000, 1, 2 )
  call ut%assert_equal( 'Day of week 2000-01-02', iso%day_of_week(), 7 )
  iso = iso8601_date( 2000, 1, 3 )
  call ut%assert_equal( 'Day of week 2000-01-03', iso%day_of_week(), 1 )
  iso = iso8601_date( 2000, 1, 4 )
  call ut%assert_equal( 'Day of week 2000-01-04', iso%day_of_week(), 2 )
  iso = iso8601_date( 2000, 1, 5 )
  call ut%assert_equal( 'Day of week 2000-01-05', iso%day_of_week(), 3 )

  iso = iso8601_date( 2016, 1, 1 )
  call ut%assert_equal( 'Day of week 2016-01-01', iso%day_of_week(), 5 )
  iso = iso8601_date( 2016, 10, 2 )
  call ut%assert_equal( 'Day of week 2016-10-02', iso%day_of_week(), 7 )
  iso = iso8601_date( 1965, 10, 2 )
  call ut%assert_equal( 'Day of week 1965-10-02', iso%day_of_week(), 6 )
  iso = iso8601_date( 1999, 1, 1 )
  call ut%assert_equal( 'Day of week 1999-01-01', iso%day_of_week(), 5 )
  
! Week of the year
  iso = iso8601_date( 2014, 1, 1 )
  call ut%assert_equal( 'Week of year 2014-01-01', iso%week_of_year(), 1 )
  iso = iso8601_date( 2015, 1, 1 )
  call ut%assert_equal( 'Week of year 2015-01-01', iso%week_of_year(), 1 )
  iso = iso8601_date( 2016, 1, 1 )
  call ut%assert_equal( 'Week of year 2016-01-01', iso%week_of_year(), 0 )
  iso = iso8601_date( 2017, 1, 1 )
  call ut%assert_equal( 'Week of year 2017-01-01', iso%week_of_year(), 0 )
  iso = iso8601_date( 2018, 1, 1 )
  call ut%assert_equal( 'Week of year 2018-01-01', iso%week_of_year(), 1 )

  iso = iso8601_date( 2016, 1, 4 )
  call ut%assert_equal( 'Week of year 2016-01-04', iso%week_of_year(), 1 )
  iso = iso8601_date( 2016, 1, 5 )
  call ut%assert_equal( 'Week of year 2016-01-05', iso%week_of_year(), 1 )
  iso = iso8601_date( 2016, 1, 10 )
  call ut%assert_equal( 'Week of year 2016-01-10', iso%week_of_year(), 1 )
  iso = iso8601_date( 2016, 1, 11 )
  call ut%assert_equal( 'Week of year 2016-01-11', iso%week_of_year(), 2 )


! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

end subroutine unit_m_iso8601_date_test_003

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_iso8601_date_test_004( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! The ISO8601 type
  type(t_iso8601_date) :: iso

! Local variables
  character(len=32) :: test
  type(t_iso8601_date) :: idate
  character(len=:), allocatable :: isostring

! Reset error handling structures
  call msg%reset_error()

! Date formatting extended
  iso = iso8601_date( year, month, day )
  write( test, date_fmt ) year, month, day
  isostring = iso%to_string()
  call ut%assert_equal( 'iso8601_YYYYMMDD (default)', isostring, test )
  isostring = iso%to_string(iso8601_YYYYMMDD)
  call ut%assert_equal( 'iso8601_YYYYMMDD', isostring, test )
  isostring = iso%to_string(iso8601_YYYYMM)
  call ut%assert_equal( 'iso8601_YYYYMM', isostring, test(:7) )
  isostring = iso%to_string(iso8601_YYYY)
  call ut%assert_equal( 'iso8601_YYYY', isostring, test(:4) )
  isostring = iso%to_string(iso8601_YY)
  call ut%assert_equal( 'iso8601_YY', isostring, test(3:4) )
  isostring = iso%to_string(iso8601_YYYYDDD)
  write( test, '(i4.4,"-",i3.3)' ) year, doy
  call ut%assert_equal( 'iso8601_YYYYDDD', isostring, test )
  isostring = iso%to_string(iso8601_YYYYWwwD)
  write( test, '(i4.4,"-W",i2.2,"-",i1.1)' ) year, week, dow
  call ut%assert_equal( 'iso8601_YYYYWwwD', isostring, test )
  isostring = iso%to_string(iso8601_YYYYWww)
  write( test, '(i4.4,"-W",i2.2)' ) year, week
  call ut%assert_equal( 'iso8601_YYYYWww', isostring, test )

! Date formatting base
  iso = iso8601_date( year, month, day )
  write( test, date_base_fmt ) year, month, day
  isostring = iso%to_string(size=iso8601_date_base_format)
  call ut%assert_equal( 'iso8601_YYYYMMDD base (default)', isostring, test )
  isostring = iso%to_string(iso8601_YYYYMMDD,iso8601_date_base_format)
  call ut%assert_equal( 'iso8601_YYYYMMDD base', isostring, test )
  isostring = iso%to_string(iso8601_YYYYMM,iso8601_date_base_format)
  call ut%assert_equal( 'iso8601_YYYYMM base', isostring, test(:6) )
  isostring = iso%to_string(iso8601_YYYY,iso8601_date_base_format)
  call ut%assert_equal( 'iso8601_YYYY base', isostring, test(:4) )
  isostring = iso%to_string(iso8601_YY,iso8601_date_base_format)
  call ut%assert_equal( 'iso8601_YY base', isostring, test(3:4) )
  isostring = iso%to_string(iso8601_YYYYDDD,iso8601_date_base_format)
  write( test, '(i4.4,i3.3)' ) year, doy
  call ut%assert_equal( 'iso8601_YYYYDDD', isostring, test )
  isostring = iso%to_string(iso8601_YYYYWwwD,iso8601_date_base_format)
  write( test, '(i4.4,"W",i2.2,i1.1)' ) year, week, dow
  call ut%assert_equal( 'iso8601_YYYYWwwD', isostring, test )
  isostring = iso%to_string(iso8601_YYYYWww,iso8601_date_base_format)
  write( test, '(i4.4,"W",i2.2)' ) year, week
  call ut%assert_equal( 'iso8601_YYYYWww', isostring, test )

! Date formatting base with sign
  iso = iso8601_date( year, month, day )
  write( test, date_base_fmt ) year, month, day
  test = '+'//test
  isostring = iso%to_string(size=iso8601_date_base_format,sign=.true.)
  call ut%assert_equal( 'iso8601_YYYYMMDD base (default)', isostring, test )
  isostring = iso%to_string(iso8601_YYYYMMDD,iso8601_date_base_format,.true.)
  call ut%assert_equal( 'iso8601_YYYYMMDD base', isostring, test )
  iso = iso8601_date( -year, month, day )
  write( test, date_base_fmt ) year, month, day
  test = '-'//test
  isostring = iso%to_string(iso8601_YYYYMM,iso8601_date_base_format,.true.)
  call ut%assert_equal( 'iso8601_YYYYMM base', isostring, test(:7) )
  isostring = iso%to_string(iso8601_YYYY,iso8601_date_base_format,.true.)
  call ut%assert_equal( 'iso8601_YYYY base', isostring, test(:5) )
  isostring = iso%to_string(iso8601_YY,iso8601_date_base_format,.true.)
  call ut%assert_equal( 'iso8601_YY base', isostring, test(1:1)//test(4:5) )

! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

end subroutine unit_m_iso8601_date_test_004

end module unit_m_iso8601_date_tests

