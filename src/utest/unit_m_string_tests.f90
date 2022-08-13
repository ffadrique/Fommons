module unit_m_string_tests

!-------------------------------------------------------------------------------
! Copyright : 2022, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
! Synopsis  : Unit tests for m_string
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

  use m_string

!---End of use statements-------------------------------------------------------

  implicit none

!---Public/Private declarations-------------------------------------------------

  private

  public unit_m_string_suite_before, unit_m_string_suite_after

  public unit_m_string_test_001, &
         unit_m_string_before_001, &
         unit_m_string_after_001
  public unit_m_string_test_002, &
         unit_m_string_before_002, &
         unit_m_string_after_002
  public unit_m_string_test_003, &
         unit_m_string_before_003, &
         unit_m_string_after_003
  public unit_m_string_test_004, &
         unit_m_string_before_004, &
         unit_m_string_after_004
  public unit_m_string_test_005, &
         unit_m_string_before_005, &
         unit_m_string_after_005
  public unit_m_string_test_006, &
         unit_m_string_before_006, &
         unit_m_string_after_006
  public unit_m_string_test_007, &
         unit_m_string_before_007, &
         unit_m_string_after_007
  public unit_m_string_test_008, &
         unit_m_string_before_008, &
         unit_m_string_after_008

  public manager

!---End of public/private declarations------------------------------------------

  character(len=*), parameter :: package = 'utilities'
  character(len=*), parameter :: module = 'm_string'
  
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

subroutine unit_m_string_before_001( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut


end subroutine unit_m_string_before_001

! ############################################################################

subroutine unit_m_string_test_001( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! Local variables
  type(t_string) :: s, t
  character(len=32) :: char1 = 'Hello world !'
  character(len=32) :: char2 = '   Hello world !   '
  character, dimension(32) :: arraychar

! String initialisation
  s = ''
  call ut%assert_equal( 'string_0', s%character(), '' )
  s = char1
  call ut%assert_equal( 'string_char1', s%character(), char1 )
  s = char2
  call ut%assert_equal( 'string_char2', s%character(), char2 )
  arraychar = ' '
  arraychar(:len_trim(char1)) = transfer( char1, arraychar(:len_trim(char1)) )

! String length
  s = ''
  call ut%assert_equal( 'len_trim_0', s%len_trim(), 0 )
  s = char1
  call ut%assert_equal( 'len_trim_n', s%len_trim(), len_trim(char1) )
  s = char2
  call ut%assert_equal( 'len_strip', len_strip(s), len_trim(adjustl(char2)) )

! Adjusting
  s = '    Test string      '
  call ut%assert_equal( 'adjustl', character(adjustl(s)), adjustl('    Test string      ') )
  call ut%assert_equal( 'adjustr', character(adjustr(s)), adjustr('    Test string      ') )
  
! Replacing
  s = '    Test string      '
  t = replace( s, 's', 'x' )
  call ut%assert_equal( 'replace', character(t), '    Text xtring      ' )

! Indexing
  s = char1
  t = 'world'
  call ut%assert_equal( 'index_string', index(s,t), index(char1,'world') )
  call ut%assert_equal( 'index_char', index(s,'rld'), index(char1,'rld') )

! Upper and lower case
  s = char1
  t = s%lowercase()
  call ut%assert_equal( 'lowercase', character(t), 'hello world !' )
  t = s%uppercase()
  call ut%assert_equal( 'uppercase', character(t), 'HELLO WORLD !' )

end subroutine unit_m_string_test_001

! ############################################################################

subroutine unit_m_string_after_001( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut


end subroutine unit_m_string_after_001

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_string_before_002( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut


end subroutine unit_m_string_before_002

! ############################################################################

subroutine unit_m_string_test_002( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! Local variables
  type(t_string) :: s, t
  type(t_String) :: actual, expected

! Initialise the handlers
  expected = trim(manager%get_unit_ref_dir()) // '/unit_m_string_002.ref'
  actual = 'unit_m_string_002.out'

! Initialise string
  s = 'Hello world!'

! Open the output file
  open( 99, file=actual%character(), status='unknown' )

! Dump ascii
  call s%write( 99, advance='NO' )
  write(99,'(a)') '<-- Synthetic newline'
  call s%write( 99 )
  call s%write( 99, advance='YES' )

! Close the output file
  flush( 99 )
  close( 99 )
  
! Implement the assertion
  call ut%assert_compare_files( 'Write', actual, expected )

end subroutine unit_m_string_test_002

! ############################################################################

subroutine unit_m_string_after_002( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut


end subroutine unit_m_string_after_002

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_string_before_003( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut


end subroutine unit_m_string_before_003

! ############################################################################

subroutine unit_m_string_test_003( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! Local variables
  type(t_string) :: s, t
  character(len=256) :: actual, expected

! Input file
  integer,           parameter :: unit    = 13
  character(len=64), parameter :: datfile = 'unit_m_string_003.dat'
  character(len=:), allocatable :: input_path
  integer :: ios


! Open the input file
  input_path = trim(manager%get_unit_data_dir()) // '/' // datfile
  open( unit, file=input_path, status='OLD' )

! Read a string from open unit
  call s%read( unit, ios )
  call ut%assert_equal( 'read_1', s%character(), 'Line 1 for test unit_m_string_001' ) 
  call s%read( unit, ios )
  call ut%assert_equal( 'read_2', s%character(), 'Line 2 for test unit_m_string_001' ) 
  call s%read( unit, ios, format='(a5)' )
  call ut%assert_equal( 'read_fmt_5', trim(s%character()), 'Line' ) 
  call s%read( unit, ios, format='(a8)' )
  call ut%assert_equal( 'read_fmt_8', trim(s%character()), 'Line 4 f' ) 
  call s%read( unit, ios )
  call ut%assert_equal( 'read_5', s%character(), 'Line 5 for test unit_m_string_001' ) 
  call s%read( unit, ios, format='(i3)' )
  call ut%assert_true( 'read_error', ios > 0 )
  close( unit )

end subroutine unit_m_string_test_003

! ############################################################################

subroutine unit_m_string_after_003( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut


end subroutine unit_m_string_after_003

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_string_before_004( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut


end subroutine unit_m_string_before_004

! ############################################################################

subroutine unit_m_string_test_004( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! Local variables
  type(t_string) :: s, t, u, v
  type(t_string) :: trim_s, trim_t
  character(len=32) :: char1 = 'Hello world !'
  character(len=32) :: char2 = '   Hello world !   '
  character(len=:), allocatable :: char3
  character, dimension(32) :: arraychar
  type(t_string) :: unas

! Empty initialisation
  s = string( 10 )
  call ut%assert_equal( 'assign_empty', s%character(), '' )

! Assign from character
  s = char1
  call ut%assert_equal( 'assign_char', s%character(), char1 )
  s = string(char1)
  call ut%assert_equal( 'assign_string', s%character(), char1 )
  s = char2
  call ut%assert_equal( 'reassign', s%character(), char2 )
  arraychar = ' '
  arraychar(:len_trim(char1)) = transfer( char1, arraychar(:len_trim(char1)) )

  s = string(char1,3)
  call ut%assert_equal( 'assign_char_select_3', s%character(), char1(3:) )
  s = string(char1,3,9)
  call ut%assert_equal( 'assign_char_select_3_9', s%character(), char1(3:9) )
  s = char1
  s = string(s)
  call ut%assert_equal( 'assign_string_select_3', s%character(), char1 )
  s = char1
  s = string(s,3)
  call ut%assert_equal( 'assign_string_select_3', s%character(), char1(3:) )
  s = char1
  s = string(s,3,9)
  call ut%assert_equal( 'assign_string_select_3_9', s%character(), char1(3:9) )
  arraychar = ' '
  arraychar(:len_trim(char1)) = transfer( char1, arraychar(:len_trim(char1)) )
  s = string(arraychar)
  call ut%assert_equal( 'assign_array', s%character(), char1 )
  s = string(arraychar,3)
  call ut%assert_equal( 'assign_array_select_3', s%character(), char1(3:) )
  s = string(arraychar,3,9)
  call ut%assert_equal( 'assign_array_select_3_9', s%character(), char1(3:9) )  
  s = char1
  char3 = character(s,3)
!  call ut%assert_equal( 'character_select_3', char3, char1(3:) )
  char3 = character(s,3,9)
!  call ut%assert_equal( 'character_select_3_9', char3, char1(3:9) )
  arraychar = s%array()
  call ut%assert_equal( 'array', arraychar, &
                        transfer( char1, arraychar(:len_trim(char1)) ) )
  call ut%assert_equal( 'array_select_3', s%array(3), &
                        transfer( char1(3:), arraychar(3:len_trim(char1)) ) )
  call ut%assert_equal( 'array_select_3_9', s%array(3,9), &
                        transfer( char1(3:9), arraychar(3:9) ) )

! Assign from string
  s = char1
  t = s
  call ut%assert_equal( 'string_assign_string', s%character(), character(t) )
  char3 = s
  call ut%assert_equal( 'char_assign_string', char3, s%character() )

! Concatenate two strings
  s = char3
  t = char2
  u = s // t
  call ut%assert_equal( 'string_cat_string', u%character(), char1 // char2 )

! Concatenate string with char
  u = s // char2
  call ut%assert_equal( 'string_cat_char', u%character(), char1 // char2 )

! Conctatenation with the + operator
  u = s + t
  call ut%assert_equal( 'string_plux_string', u%character(), char1 // char2 )
  u = char1 + char2
  call ut%assert_equal( 'string_plus_char', u%character(), char1 // char2 )

! Concatenate with character strings
  v = '"' // s // '"'
  call ut%assert_equal( 'string_multi_cat', v%character(), '"' // char1 // '"' )

! Concatenate with trimmed string
  trim_s = trim(s)
  v = '"' // trim_s // '"'
  call ut%assert_equal( 'string_multi_cat_trim', v%character(), '"' // trim(char1) // '"' )

! Test of the length calculation functions
  s = char1
  trim_t = trim(t)
  call ut%assert_equal( 'len_1', len(s), len(char1) )
  call ut%assert_equal( 'len_2', len(t), len(char2) )

! Concatenation with character strings
  s = char1
  v = '"' + s + '"'
  call ut%assert_equal( 'string_multi_cat', v%character(), '"' // char1 // '"' )
  v = '"' + trim_s + '"'
  call ut%assert_equal( 'string_multi_cat', v%character(), '"' // trim(char1) // '"' )
  trim_s = strip(s)
  v = '"' + trim_s + '"'
  call ut%assert_equal( 'string_multi_cat', v%character(), '"' // trim(adjustl(char1)) // '"' )

! Try character from unallocated string
  call ut%assert_equal( 'unassigned_1', unas%character(), '' )
  call ut%assert_equal( 'unassigned_1', unas%character( 3 ), '' )

end subroutine unit_m_string_test_004

! ############################################################################

subroutine unit_m_string_after_004( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut


end subroutine unit_m_string_after_004

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_string_before_005( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut


end subroutine unit_m_string_before_005

! ############################################################################

subroutine unit_m_string_test_005( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! Local variables
  type(t_string) :: s, t, u, v
  type(t_string) :: trim_s, trim_t
  character(len=32) :: char1 = 'Hello world !'
  character(len=32) :: char2 = '   Hello world !   '
  

! Initialise
  char1 = 'Test string 1'
  char2 = 'Second test string'

! Comparisons
  s = char1
  t = char1
  call ut%assert_true( 'string_equal_string', s == t )
  call ut%assert_false( 'string_not_equal_string', s /= t )
  call ut%assert_true( 'string_equal_char', s == char1 )
  call ut%assert_false( 'string_not_equal_char', s /= char1 )
  call ut%assert_true( 'char_equal_string', char1 == s )
  call ut%assert_false( 'char_not_equal_string', char1 /= s )

! Comparisons
  s = char1
  t = char2
  trim_s = trim(s)
  trim_t = trim(t)
  call ut%assert_false( 'string_equal_string', s == t )
  call ut%assert_true( 'char_equal_char', char2 == t )
  call ut%assert_false( 'char_equal_string', char2 == s )

  call ut%assert_true( 'string_not_equal_string', s /= t )
  call ut%assert_false( 'char_not_equal_char', char2 /= t )
  call ut%assert_true( 'char_not_equal_string', char2 /= s )

  call ut%assert_true( 'string_gt_string', s > t )
  call ut%assert_true( 'string_gt_string', lgt(s,t) )
  call ut%assert_true( 'string_gt_string', lgt(s,char2) )
  call ut%assert_true( 'string_gt_string', lgt(char1,t) )

  call ut%assert_true( 'string_ge_string', s >= t )
  call ut%assert_true( 'string_ge_string', lge(s,t) )
  call ut%assert_true( 'string_ge_string', lge(s,char2) )
  call ut%assert_true( 'string_ge_string', lge(char1,t) )

  call ut%assert_false( 'string_lt_string', s < t )
  call ut%assert_false( 'string_lt_string', llt(s,t) )
  call ut%assert_false( 'string_lt_string', llt(s,char2) )
  call ut%assert_false( 'string_lt_string', llt(char1,t) )

  call ut%assert_false( 'string_le_string', s <= t )
  call ut%assert_false( 'string_le_string', lle(s,t) )
  call ut%assert_false( 'string_le_string', lle(s,char2) )
  call ut%assert_false( 'string_le_string', lle(char1,t) )

end subroutine unit_m_string_test_005

! ############################################################################

subroutine unit_m_string_after_005( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut


end subroutine unit_m_string_after_005

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_string_before_006( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut


end subroutine unit_m_string_before_006

! ############################################################################

subroutine unit_m_string_test_006( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! Local variables
  type(t_string) :: s1, s2
  character(len=32) :: char1 = 'Hello world !'
  character(len=32) :: char2 = 'H e l l o !'


! Verify
  s1 = char1
  s2 = char2
  call ut%assert_equal( 'string_verify_string', verify(s1,s2), verify(char1,char2) )
  call ut%assert_equal( 'string_verify_char', verify(s1,char2), verify(char1,char2) )
  call ut%assert_equal( 'string_verify_back_string', verify(s1,s2,.true.), verify(char1,char2,.true.) )
  call ut%assert_equal( 'string_verify_back_char', verify(s1,char2,.true.), verify(char1,char2,.true.) )

! Scan
  s1 = char1
  char2 = char2(5:)
  s2 = char2
  call ut%assert_equal( 'string_scan_string', scan(s1,s2), scan(char1,char2) )
  call ut%assert_equal( 'string_scan_char', scan(s1,char2), scan(char1,char2) )
  call ut%assert_equal( 'string_scan_back_string', scan(s1,s2,.true.), scan(char1,char2,.true.) )
  call ut%assert_equal( 'string_scan_back_char', scan(s1,char2,.true.), scan(char1,char2,.true.) )

end subroutine unit_m_string_test_006

! ############################################################################

subroutine unit_m_string_after_006( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut


end subroutine unit_m_string_after_006

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_string_before_007( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut


end subroutine unit_m_string_before_007

! ############################################################################

subroutine unit_m_string_test_007( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! Local variables
  type(t_string) :: input, pattern
  character(len=:), allocatable :: cinput
  character(len=:), allocatable :: cpattern

! Initialise
  cinput = 'Hello world !'
  cpattern = '*'
  
! Easy match to *
  input = cinput
  pattern = cpattern
  call ut%assert_true( 'Match to '//trim(cpattern), match( input, pattern ) )

! Easy match with simple *
  cpattern = 'Hello *'
  pattern = cpattern
  call ut%assert_true( 'Match to '//trim(cpattern), match( input, pattern ) )

! Easy match with simple *
  cpattern = '* world !'
  pattern = cpattern
  call ut%assert_true( 'Match to '//trim(cpattern), match( input, pattern ) )

! Easy match with simple *
  cpattern = 'Hel*world !'
  pattern = cpattern
  call ut%assert_true( 'Match to '//trim(cpattern), match( input, pattern ) )

! Easy failed match with simple *
  cpattern = 'Hal*world !'
  pattern = cpattern
  call ut%assert_false( 'Match fail to '//trim(cpattern), match( input, pattern ) )

! Match with multiple *
  cpattern = 'Hel*wor* !'
  pattern = cpattern
  call ut%assert_true( 'Match to '//trim(cpattern), match( input, pattern ) )

! Match with simple * and vaious ?
  cpattern = 'Hel*wor?? !'
  pattern = cpattern
  call ut%assert_true( 'Match to '//trim(cpattern), match( input, pattern ) )

! Match fail with simple * and vaious ?
  cpattern = 'Hel*world ???'
  pattern = cpattern
  call ut%assert_false( 'Match to '//trim(cpattern), match( input, pattern ) )

end subroutine unit_m_string_test_007

! ############################################################################

subroutine unit_m_string_after_007( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut


end subroutine unit_m_string_after_007

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_string_before_008( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut


end subroutine unit_m_string_before_008

! ############################################################################

subroutine unit_m_string_test_008( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! Local variables
  type(t_string) :: s, t
  character(len=128) :: char0 = 'ccsds:mois:navgw:opm'
  character(len=128) :: char1 = '/home/user/documents/atlantis/src/ccsds/utest/data/test_mandatory.dat'
  character(len=128) :: char2 = '/home////user/documents/atlantis//src/ccsds///utest/data/test_mandatory.dat'
  type(t_string), allocatable, dimension(:) :: tokens0, tokens1, tokens2, tokens

! Initialise
  allocate( tokens0(4), source=(/ string('ccsds'), string('mois'), string('navgw'), string('opm') /) )
  allocate( tokens1(10), source=(/ string(''), string('home'), string('user'), string('documents'), &
                                   string('atlantis'), string('src'), string('ccsds'), string('utest'), &
                                   string('data'), string('test_mandatory.dat') /) )
  allocate( tokens2(16), source=(/ string(''), string('home'), string(''), string(''), string(''), &
                                   string('user'), string('documents'), string('atlantis'), string(''), &
                                   string('src'), string('ccsds'), string(''), string(''), string('utest'), &
                                   string('data'), string('test_mandatory.dat') /) )

! Split
  s = char0
  call s%split( ':', tokens )
  call ut%assert_equal( 'Split (:)', tokens, tokens0 )

! Join
  call t%join( tokens, ':' )
  call ut%assert_equal( 'Join (:)', t, s )
  
! Split
  s = char1
  call s%split( '/', tokens )
  call ut%assert_equal( 'Split (/)', tokens, tokens1 )
  
! Join
  call t%join( tokens, string('/') )
  call ut%assert_equal( 'Join (/)', t, s )

! Simple split with multiple separators flag
  s = char0
  call s%split( ':', tokens, .true. )
  call ut%assert_equal( 'Split (:, multiple flag)', tokens, tokens0 )
  
! Simple split with multiple separators flag
  s = char1
  call s%split( '/', tokens, .true. )
  call ut%assert_equal( 'Split (/, mutliple flag)', tokens, tokens1 )
  
! Simple split with actual mutltiple separators
  s = char2
  call s%split( '/', tokens )
  call ut%assert_equal( 'Split (///)', tokens, tokens2 )
  
! Simple split with actual mutltiple separators
  s = char2
  call s%split( '/', tokens, .true. )
  call ut%assert_equal( 'Split (///, multiple flag)', tokens, tokens1 )
  
end subroutine unit_m_string_test_008

! ############################################################################

subroutine unit_m_string_after_008( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut


end subroutine unit_m_string_after_008

! ############################################################################
! # Suite before procedure ###################################################
! ############################################################################

subroutine unit_m_string_suite_before( suite )

! Reference suite
  class(t_xfunit_suite), intent(inout) :: suite


end subroutine unit_m_string_suite_before

! ############################################################################
! # Suite after procedure ####################################################
! ############################################################################

subroutine unit_m_string_suite_after( suite )

! Reference suite
  class(t_xfunit_suite), intent(inout) :: suite


end subroutine unit_m_string_suite_after

end module unit_m_string_tests
