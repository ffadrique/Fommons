module m_c_string

!-------------------------------------------------------------------------------
! Copyright : 2022, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
! Synopsis  : Conversion from fortran character to C null teminated strings
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

  use, intrinsic :: iso_c_binding

  use m_string

!---End of use statements-------------------------------------------------------

  implicit none

!---Public/Private declarations-------------------------------------------------

  private

  public t_c_string
  public c_string

!---End of public/private declarations------------------------------------------

!---Declaration of module variables---------------------------------------------

! String type that supports conversion to/from C representation (null terminated)
  type, extends(t_string) :: t_c_string
    private

    contains

!     Interface conversion to C
      procedure :: to_c => c_string_to_c

!     Interface conversion from C
      generic :: from_c => c_string_from_c_character, &
                           c_string_from_c_array
      procedure, private :: c_string_from_c_character
      procedure, private :: c_string_from_c_array

  end type t_c_string

! Constructor interface
  interface c_string
    module procedure c_string_from_empty
    module procedure c_string_from_character
    module procedure c_string_from_character_selected
    module procedure c_string_from_array
    module procedure c_string_from_array_selected
    module procedure c_string_from_string
    module procedure c_string_from_string_selected
  end interface c_string

!---End of declaration of module variables--------------------------------------

contains

! Constructor for empty string of given size
elemental function c_string_from_empty( n ) result(this)

! The string size in bytes
  integer, intent(in) :: n

! The string
  type(t_c_string) :: this

! Return the string
  this = string(n)

end function c_string_from_empty


! Constructor from character
elemental function c_string_from_character( c ) result(this)

! The character string to use as initialisation (optional)
  character(len=*), optional, intent(in) :: c

! The string
  type(t_string) :: this

! Return the string
  this = string(c)

end function c_string_from_character


! Constructor from part of character
elemental function c_string_from_character_selected( c, start, end ) result(this)

! The character string to use as initialisation
  character(len=*), intent(in) :: c

! The first character to select
  integer, intent(in) :: start

! The last character to select (defaults to last)
  integer, optional, intent(in) :: end

! The string
  type(t_string) :: this

! Return the string
  this = string( c, start, end )

end function c_string_from_character_selected


! Constructor from character array
pure function c_string_from_array( c ) result(this)

! The character string to use as initialisation
  character, dimension(:), intent(in) :: c

! The string
  type(t_string) :: this

! Local variables
  integer :: idx

! Check if the input is a null terminated string
  idx = minloc( ichar(c), 1)
  if( c(idx) == c_null_char ) then

!   Build skipping the null character
    this = string(c(:idx-1))

  else

!   Build using the whole input array
    this = string(c)

  end if

end function c_string_from_array


! Constructor from part of character array
pure function c_string_from_array_selected( c, start, end ) result(this)

! The character string to use as initialisation (optional)
  character, dimension(:), intent(in) :: c

! The first character to select
  integer, intent(in) :: start

! The last character to select (defaults to last)
  integer, optional, intent(in) :: end

! The string
  type(t_string) :: this

! Local variables
  integer :: idx

! Check if the input is a null terminated string
  idx = minloc( ichar(c), 1)
  if( c(idx) == c_null_char ) then

!   Build skipping the null character
    this = string( c(:idx-1), start, end )

  else

!   Build using the whole input string
    this = string( c, start, end )

  end if

end function c_string_from_array_selected


! Constructor from string
elemental function c_string_from_string( s ) result(this)

! The character string to use as initialisation (optional)
  type(t_string), intent(in) :: s

! The string
  type(t_string) :: this

! Return the string
  this = s

end function c_string_from_string


! Constructor from part of string
elemental function c_string_from_string_selected( s0, start, end ) result(this)

! The character string to use as initialisation (optional)
  class(t_string), intent(in) :: s0

! The first character to select
  integer, intent(in) :: start

! The last character to select (defaults to last)
  integer, optional, intent(in) :: end

! The string
  type(t_string) :: this

! Return the string
  this = string( s0, start, end )

end function c_string_from_string_selected


! Initialise using a C formatted string given as character array
pure subroutine c_string_from_c_array( this, cstr )

! The calling object
  class(t_c_string), intent(inout) :: this

! The C formatted character string
  character(c_char), dimension(:), intent(in) :: cstr

! Local variables
  integer :: i

! Look for the null terminator
  do i = 1, size(cstr)
    if( cstr(i) == c_null_char ) exit
  end do

! Assign the output string
  this = string( cstr(:i-1) )

end subroutine c_string_from_c_array


! Initialise from C formatted string given as C formatted character string
pure subroutine c_string_from_c_character( this, cstr )

! The calling object
  class(t_c_string), intent(inout) :: this

! The C formatted character string
  character(len=*,kind=c_char), intent(in) :: cstr

! Local variables
  integer :: i

! Look for the null terminator
  do i = 1, len(cstr)
    if( cstr(i:i) == c_null_char ) exit
  end do

! Assign the output string
  this = string( cstr(:i-1) )

end subroutine c_string_from_c_character


! Return a C formatted array
pure function c_string_to_c( this, chars ) result(cstr)

! The calling object
  class(t_c_string), intent(in) :: this

! The number of characters to include (optional, defaults to len_trim(this))
  integer, optional, intent(in) :: chars

! The resulting C formatted string
  character(c_char), dimension(:), allocatable :: cstr

! Local variables
  integer :: i

! Compute the length to process
  i = this%len()
  if( present(chars) ) then
    i = min( i, chars )
  end if

! Generate the C formatted string
  allocate(cstr(i+1))
  cstr = ''
  cstr(:i) = transfer( this%character(), cstr(:i) )
  cstr(i+1) = c_null_char

end function c_string_to_c

end module m_c_string
