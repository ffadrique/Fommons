module unit_m_file_handler_tests

!-------------------------------------------------------------------------------
! Copyright : 2022, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
! Synopsis  : Unit tests for m_file_handler
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

  use m_file_handler

!---End of use statements-------------------------------------------------------

  implicit none

!---Public/Private declarations-------------------------------------------------

  private


  public unit_m_file_handler_test_001

  public unit_m_file_handler_test_002

  public unit_m_file_handler_test_003

  public manager

!---End of public/private declarations------------------------------------------

  character(len=*), parameter :: package = 'utilities'
  character(len=*), parameter :: module = 'm_file_handler'
  
!---Declaration of local variables----------------------------------------------


! The unit test manager
  type(t_xfunit_manager), allocatable, save :: manager

  type(t_xfunit_unit), save :: ut

!---End of declaration of local variables---------------------------------------

!---Executable Code-------------------------------------------------------------

contains

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_file_handler_test_001( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! The file handler structure
  type(t_file_handler) :: handler

! Local variables
  character(len=32) :: file = 'xfunit_test.dat'
  character(len=256) :: filename
  type(t_string) :: tfilename

! Intialise handler
  filename = '/home/user/software/..' // '\' // file
  handler = file_handler( filename )


end subroutine unit_m_file_handler_test_001

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_file_handler_test_002( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! The file handler
  type(t_file_handler) :: handler

! Local variables
  character(len=32) :: file = 'unit_m_file_handler_002.dat'
  type(t_string) :: filename
  integer :: i

! The text file
  character(len=132), dimension(6), parameter :: lines = (/ &
    'This is a test file                                                                                              ', &
    'Second record in the file                                                                                        ', &
    '                                                                                                                 ', &
    'Previous record is an empty record                                                                               ', &
    'And this record is much longer than all the rest --> one two three for five six seven eith nine ten end_of_record', &
    'And now another one to verify that previous long record is complete                                              ' /)
  type(t_string) :: text

! Intialise handler
  filename = trim(manager%get_unit_data_dir()) // '/' // file
  handler = file_handler( filename%character(), eol=file_handler_unix_eol )

! Convert to string without carriage control
  text = ''
  do i = 1, size(lines)
    text = text // trim(lines(i))
  end do
  call ut%assert_equal( 'Convert to string without carriage control', handler%to_string(), text )

! Convert to string with carriage control
  text = ''
  do i = 1, size(lines)
    text = text // trim(lines(i)) // trim(file_handler_eol(file_handler_unix_eol))
  end do
  call ut%assert_equal( 'Convert to string with carriage control', handler%to_string( .true. ), text )

end subroutine unit_m_file_handler_test_002

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_file_handler_test_003( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! The file handler structure
  type(t_file_handler) :: handler

! Local variables
  character(len=32) :: file = 'xfunit_test.dat'
  character(len=256) :: filename

! Intialise handler
  filename = '../../dir/../local/' // file
  handler = file_handler( filename )

  
  
  
end subroutine unit_m_file_handler_test_003

end module unit_m_file_handler_tests

