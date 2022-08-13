module unit_m_object_tests

!-------------------------------------------------------------------------------
! Copyright : 2022, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
! Synopsis  : Unit tests for m_object
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

  use m_object

!---End of use statements-------------------------------------------------------

  implicit none

!---Public/Private declarations-------------------------------------------------

  private

  public unit_m_object_test_001

  public manager

!---End of public/private declarations------------------------------------------

  character(len=*), parameter :: package = 'system'
  character(len=*), parameter :: module = 'm_object'
  
!---Declaration of local variables----------------------------------------------

! The unit test management structures
  type(t_xfunit_manager), allocatable, save :: manager
  type(t_xfunit_suite), allocatable, save :: suite

! The error handling structure
  type(t_messages), save :: msg

! Type derived from objecy
  type, extends(t_object) :: t_derived
    private
    integer :: i = 123456
    real :: r = 3.141592654
    character(len=32) :: c = "Lorem ipsum dolor sit amet"
    type(t_derived), pointer :: p => null()
  end type t_derived
  
!---End of declaration of local variables---------------------------------------

!---Executable Code-------------------------------------------------------------

contains

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_object_test_001( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! The tested strcuture
  type(t_derived), target :: obj1, obj2

! Local variables
  integer :: hash1, hash2

! Reset error handling structures
  call msg%reset_error()

! Compute the hash
  hash1 = obj1%get_hash_code()
  call ut%assert_equal( 'Hash object 1', hash1, 778111771 )
  hash2 = obj2%get_hash_code()
  call ut%assert_equal( 'Hash object 2', hash2, hash1 )

! Object equality
  call ut%assert_true( 'Equality', obj1%equals(obj2) )
  
! Compute hash on modified object
  obj1%i = -obj1%i
  hash1 = obj1%get_hash_code()
  call ut%assert_equal( 'Hash object 1 modified', hash1, 545196754 )
  
! Object equality
  call ut%assert_false( 'Equality (modified 1)', obj1%equals(obj2) )

! Modifed pointers
! Pointers will be different between execution because the address of
! the pointed object wil lbe different
  obj1 = obj2
  obj1%p => obj2
  hash1 = obj1%get_hash_code()
  obj2%p => obj2
  hash2 = obj2%get_hash_code()
  call ut%assert_equal( 'Hashes pointer', hash2, hash1 )
  
! Object equality
  call ut%assert_true( 'Equality (pointer)', obj1%equals(obj2) )
  
! Verify the execution completion
  if( msg%on_error() ) then
      call ut%error( 1, 'Errors during unit test execution', msg )
  end if

! Reset error handling structures
  call msg%reset_error()

end subroutine unit_m_object_test_001

end module unit_m_object_tests
