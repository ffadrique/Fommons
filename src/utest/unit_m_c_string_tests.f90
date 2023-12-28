module unit_m_c_string_tests

!-------------------------------------------------------------------------------
! Copyright : 2022, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
! Synopsis  : Unit tests for m_c_string
!-------------------------------------------------------------------------------

!---USE statements--------------------------------------------------------------

  use, intrinsic :: iso_c_binding

  use m_xfunit

  use m_string
  use m_c_string

!---End of use statements-------------------------------------------------------

  implicit none

!---Public/Private declarations-------------------------------------------------

  private


  public unit_m_c_string_test_001

  public unit_m_c_string_test_002

  public unit_m_c_string_test_003

  public manager

!---End of public/private declarations------------------------------------------

  character(len=*), parameter :: package = 'utilities'
  character(len=*), parameter :: module = 'm_c_string'
  
!---Declaration of local variables----------------------------------------------

! The unit test manager
  type(t_xfunit_manager), allocatable, save :: manager


!---End of declaration of local variables---------------------------------------

!---Executable Code-------------------------------------------------------------

contains

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_c_string_test_001( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! Local variables
  integer, parameter :: n = 16
  character(len=n) :: fstring
  type(t_string) :: tstring
  type(t_c_string) :: tcstring
  character, dimension(n+1) :: carray
  character, dimension(n) :: farray
  integer :: idx

! Initialise C representation
  fstring = 'Hello world !'
  farray = transfer( fstring, farray )
  tstring = fstring

! Constructors
  tcstring = c_string(n)
  call ut%assert_equal( 'Empty constructor', tcstring%character(), '' )

  tcstring = c_string(fstring)
  call ut%assert_equal( 'Character constructor', tcstring%character(), fstring )
  tcstring = c_string(fstring,3,8)
  call ut%assert_equal( 'Character selected constructor', tcstring%character(), fstring(3:8) )

  tcstring = c_string(tstring)
  call ut%assert_equal( 'String constructor', tcstring%character(), fstring )
  tcstring = c_string(tstring,3,8)
  call ut%assert_equal( 'String selected constructor', tcstring%character(), fstring(3:8) )

  tcstring = c_string(farray)
  call ut%assert_equal( 'Array constructor', tcstring%array(), farray )
  tcstring = c_string(farray,3,8)
  call ut%assert_equal( 'Array selected constructor', tcstring%array(), farray(3:8) )

  carray(:len_trim(fstring)+1) = [ farray(:len_trim(fstring)), c_null_char ]
  tcstring = c_string(carray)
  call ut%assert_equal( 'Array with null constructor', tcstring%array(), farray(:len_trim(fstring)) )
  tcstring = c_string(carray,3,8)
  call ut%assert_equal( 'Array with null selected constructor', tcstring%array(), farray(3:8) )

end subroutine unit_m_c_string_test_001

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_c_string_test_002( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! Local variables
  integer, parameter :: n = 16
  character(len=n) :: fstring
  type(t_string) :: tstring
  type(t_c_string) :: tcstring
  character, dimension(n+1) :: carray
  character, dimension(n) :: farray
  integer :: idx

! Initialise C representation
  fstring = 'Hello world !'
  farray = transfer( fstring, farray )
  tstring = fstring

! Convert Fortran to C
  tcstring = c_string(fstring)
  carray(:len(fstring)+1) = tcstring%to_c()
  call ut%assert_equal( 'Fortran to c (value)', carray(:n), farray(:n) )
  call ut%assert_true( 'Fortran to c (null)', carray(n+1) == c_null_char )

! Convert Fortran to C (without trailing blanks)
  idx = len_trim(fstring)
  tcstring = c_string(trim(fstring))
  carray(:len_trim(fstring)+1) = tcstring%to_c()
  call ut%assert_equal( 'Fortran to c no blanks (value)', carray(:idx), farray(:idx) )
  call ut%assert_true( 'Fortran to c no blanks (null)', carray(idx+1) == c_null_char )

! Convert a limited number of characters
  tcstring = c_string(fstring)
  carray(:5+1) = tcstring%to_c(5)
  call ut%assert_equal( 'Fortran to c selected (value)', carray(:5), farray(:5) )
  call ut%assert_true( 'Fortran to c  selected(null)', carray(5+1) == c_null_char )


end subroutine unit_m_c_string_test_002

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_c_string_test_003( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! Local variables
  integer, parameter :: n = 16
  character(len=n) :: fstring
  character(len=n+1) :: cstring
  character, dimension(n+1) :: carray
  type(t_c_string) :: tcstring

! Initialise C representation
  fstring = 'Hello world !'
  tcstring = c_string(trim(fstring))
  carray(:len_trim(fstring)+1) = tcstring%to_c()

! Convert C to Fortran
  call tcstring%from_c(carray)
  call ut%assert_equal( 'C array to fortran', tcstring%character(), fstring )
  cstring = trim(fstring)//c_null_char
  call tcstring%from_c(cstring)
  call ut%assert_equal( 'C string to fortran', tcstring%character(), fstring )


end subroutine unit_m_c_string_test_003

end module unit_m_c_string_tests

