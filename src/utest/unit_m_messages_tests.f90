module unit_m_messages_tests

!-------------------------------------------------------------------------------
! Copyright : 2022, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
! Synopsis  : Unit tests for m_messages
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

  use m_xfunit

  use m_util_convert
  use m_string
  use m_messages

!---End of use statements-------------------------------------------------------

  implicit none

!---Public/Private declarations-------------------------------------------------

  private

  public unit_m_messages_test_001
  public unit_m_messages_test_002
  public unit_m_messages_test_003
  public unit_m_messages_test_004

  public manager

!---End of public/private declarations------------------------------------------

  character(len=*), parameter :: package = 'utilities'
  character(len=*), parameter :: module = 'm_messages'
  
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

subroutine unit_m_messages_test_001( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! The messages type
  type(t_messages) :: msg

! Local variables
  type(t_string) :: actual, expected
  integer, parameter :: unit = 99
  integer :: ios

! Initialise the file names
  expected = trim(manager%get_unit_ref_dir()) // '/unit_m_messages_001.ref'
  actual = 'unit_m_messages_001.out'

! Initialise the structure
  msg = messages()

! Open the output temporary file
  open( unit, file=actual%character(), status='unknown', action='write', iostat=ios )
  if( ios == 0 ) then

!   Add messages
    call msg%error( 'unit_m_messages', 'unit_m_messages_test_001', 100, 'Message 100' )
    call msg%error( 'unit_m_messages', 'unit_m_messages_test_001', 200, 'error 200' )
    call msg%error( 'unit_m_messages', 'unit_m_messages_test_001', 300, 'Message error 300' )

!   Add warnings
    call msg%warning( 'unit_m_messages', 'unit_m_messages_test_001', 100, 'Message Warning 100' )
    call msg%warning( 'unit_m_messages', 'unit_m_messages_test_001', 200, 'Message Warning 200' )
    call msg%warning( 'unit_m_messages', 'unit_m_messages_test_001', 300, 'Message Warning 300' )

!   Dump the error messages
    call msg%dump_errors( unit )

!   Dump the Warning messages
    call msg%dump_warnings( unit )

!   Flush buffers
    flush( unit )

!   Close
    close( unit )

!   Compare with reference
    call ut%assert_compare_files( 'Basic message generation', actual, expected )

  end if

end subroutine unit_m_messages_test_001

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_messages_test_002( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! The messages type
  type(t_messages) :: msg, msg2

! Local variables
  type(t_string) :: actual, expected
  integer, parameter :: unit = 99
  integer :: ios

! Initialise the file names
  expected = trim(manager%get_unit_ref_dir()) // '/unit_m_messages_002.ref'
  actual = 'unit_m_messages_002.out'

! Initialise the structures
  msg = messages()
  msg2 = messages()

! Open the output temporary file
  open( unit, file=actual%character(), status='unknown', action='write', iostat=ios )
  if( ios == 0 ) then

!   Add messages
    call msg%error( 'unit_m_messages', 'unit_m_messages_test_001', 100, 'Message 100' )
    call msg%error( 'unit_m_messages', 'unit_m_messages_test_001', 200, 'error 200' )
    call msg%error( 'unit_m_messages', 'unit_m_messages_test_001', 300, 'Message error 300' )

!   Add warnings
    call msg%warning( 'unit_m_messages', 'unit_m_messages_test_001', 100, 'Message Warning 100' )
    call msg%warning( 'unit_m_messages', 'unit_m_messages_test_001', 200, 'Message Warning 200' )
    call msg%warning( 'unit_m_messages', 'unit_m_messages_test_001', 300, 'Message Warning 300' )

!   Copy the messages structure
    msg2 = msg

!   Dump the error messages
    call msg2%dump_errors( unit )

!   Dump the Warning messages
    call msg2%dump_warnings( unit )

!   Flush buffers
    flush( unit )

!   Close
    close( unit )

!   Compare with reference
    call ut%assert_compare_files( 'Message generation from copy', actual, expected )

  end if

end subroutine unit_m_messages_test_002

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_messages_test_003( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! The messages type
  type(t_messages) :: msg, msg1, msg2

! Local variables
  type(t_string) :: actual, expected
  integer, parameter :: unit = 99
  integer :: ios

! Initialise the file names
  expected = trim(manager%get_unit_ref_dir()) // '/unit_m_messages_003.ref'
  actual = 'unit_m_messages_003.out'

! Initialise the structures
  msg1 = messages()
  msg2 = messages()

! Open the output temporary file
  open( unit, file=actual%character(), status='unknown', action='write', iostat=ios )
  if( ios == 0 ) then

!   Add messages to the first structure
    call msg1%error( 'unit_m_messages', 'unit_m_messages_test_001', 100, 'Message 100' )
    call msg1%error( 'unit_m_messages', 'unit_m_messages_test_001', 200, 'error 200' )
    call msg1%error( 'unit_m_messages', 'unit_m_messages_test_001', 300, 'Message error 300' )
    call msg1%error( 'unit_m_messages', 'unit_m_messages_test_001', 400, 'Message error 400' )

!   Add warnings to the first structure
    call msg1%warning( 'unit_m_messages', 'unit_m_messages_test_001', 100, 'Message Warning 100' )
    call msg1%warning( 'unit_m_messages', 'unit_m_messages_test_001', 200, 'Message Warning 200' )

!   Add messages to the second structure
    call msg2%error( 'unit_m_messages', 'unit_m_messages_test_001', 1009, 'Message 1009' )
    call msg2%error( 'unit_m_messages', 'unit_m_messages_test_001', 2009, 'error 2009' )
    call msg2%error( 'unit_m_messages', 'unit_m_messages_test_001', 3009, 'Message error 3009' )

!   Add warnings to the second structure
    call msg2%warning( 'unit_m_messages', 'unit_m_messages_test_001', 1009, 'Message Warning 1009' )
    call msg2%warning( 'unit_m_messages', 'unit_m_messages_test_001', 2009, 'Message Warning 2009' )
    call msg2%warning( 'unit_m_messages', 'unit_m_messages_test_001', 3009, 'Message Warning 3009' )

!   Copy first structure into the reference one
    msg = msg1

!   Stack the second structure after the reference one
    call msg%stack( msg2 )    
    
!   Dump the error messages
    call msg%dump_errors( unit )

!   Dump the Warning messages
    call msg%dump_warnings( unit )
    write( unit, '(a)' ) '---'
    
!   Restore the reference structure
    msg = msg1    
    
!   Stack the second structure before the reference one
    call msg%stack( msg2, .true. )

!   Dump the error messages
    call msg%dump_errors( unit )

!   Dump the Warning messages
    call msg%dump_warnings( unit )
    
!   Flush buffers
    flush( unit )

!   Close
    close( unit )

!   Compare with reference
    call ut%assert_compare_files( 'Message generation from copy', actual, expected )

  end if

end subroutine unit_m_messages_test_003

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_messages_test_004( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! The messages type
  type(t_messages) :: msg

! Counter
  integer :: i

! Force more messages than available in default internal buffer
  do i = 1, 11
    call msg%error( 'unit_m_messages', 'unit_m_messages_test_001', 100*i, 'Message error '//character(100*i) )
  end do

! Verify error condition
  call ut%assert_true( 'Error condition set', msg%on_error() )

! Reset error condition
  call msg%reset_error()

! Verify error condition
  call ut%assert_false( 'Error condition reset', msg%on_error() )

end subroutine unit_m_messages_test_004

end module unit_m_messages_tests

