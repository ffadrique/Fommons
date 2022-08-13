module unit_m_util_convert_tests

!-------------------------------------------------------------------------------
! Copyright : 2022, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
! Synopsis  : Unit tests for m_util_convert
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

!---End of use statements-------------------------------------------------------

  implicit none

!---Public/Private declarations-------------------------------------------------

  private


  public unit_m_util_convert_test_001
  public unit_m_util_convert_test_002
  public unit_m_util_convert_test_003
  public unit_m_util_convert_test_004
  public unit_m_util_convert_test_005
  public unit_m_util_convert_test_006
  public unit_m_util_convert_test_007
  public unit_m_util_convert_test_008
  public unit_m_util_convert_test_009

  public manager

!---End of public/private declarations------------------------------------------

  character(len=*), parameter :: package = 'utilities'
  character(len=*), parameter :: module = 'm_util_convert'
  
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

subroutine unit_m_util_convert_test_001( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! Local variables
  complex(kind=8),   dimension(5) :: dcmp
  complex(kind=4),   dimension(5) :: cmp
  real(kind=8),      dimension(5) :: dbl
  real(kind=4),      dimension(5) :: real
  character(len=64), dimension(5) :: chr
  integer(kind=1),   dimension(5) :: integ1
  integer(kind=2),   dimension(5) :: integ2
  integer,           dimension(5) :: integ
  logical(kind=1),   dimension(5) :: bool1
  logical(kind=2),   dimension(5) :: bool2
  logical,           dimension(5) :: bool

! Counter
  integer :: i

! Generate inputs
  dbl = [ 1.000000_8, 0.025480_8, 0.352516_8, 0.666914_8, 0.963056_8 ]
  real = [ 1.000000_4, 0.025480_4, 0.352516_4, 0.666914_4, 0.963056_4 ]
  dcmp = cmplx( dbl, -dbl, kind=8 )
  dcmp(2) = conjg(dcmp(2))
  dcmp(4) = conjg(dcmp(4))
  cmp = cmplx( real, -real, kind=4 )
  cmp(2) = conjg(cmp(2))
  cmp(4) = conjg(cmp(4))
  integ = int( 1000_8 * dbl, kind=4 )
  integ2 = int( 100_8 * dbl, kind=2 )
  integ1 = int( 10_8 * dbl, kind=1 )
  bool1 = .true._1
  bool1(3) = .false._1
  bool2 = .true._2
  bool2(2) = .false._2
  bool = .true.
  bool(4) = .false.

! Convert logical to character string
  chr = character( bool )
  call ut%assert_equal( 'Logical(kind=4)', chr, [ 'T', 'T', 'T', 'F', 'T' ] )
  chr = character( bool2 )
  call ut%assert_equal( 'Logical(kind=2)', chr, [ 'T', 'F', 'T', 'T', 'T' ] )
  chr = character( bool1 )
  call ut%assert_equal( 'Logical(kind=1)', chr, [ 'T', 'T', 'F', 'T', 'T' ] )
  chr = character( bool, fmt='yesno' )
  call ut%assert_equal( 'Logical yesno', chr, [ 'yes', 'yes', 'yes', 'no ', 'yes' ] )
  chr = character( bool, fmt='YESNO' )
  call ut%assert_equal( 'Logical YESNO', chr, [ 'YES', 'YES', 'YES', 'NO ', 'YES' ]  )
  chr = character( bool, fmt='yn' )
  call ut%assert_equal( 'Logical yn', chr, [ 'y', 'y', 'y', 'n', 'y' ] )
  chr = character( bool, fmt='YN' )
  call ut%assert_equal( 'Logical YN', chr, [ 'Y', 'Y', 'Y', 'N', 'Y' ] )
  chr = character( bool, fmt='truefalse' )
  call ut%assert_equal( 'Logical truefalse', chr, [ 'true ', 'true ', 'true ', 'false', 'true ' ] )
  chr = character( bool, fmt='TRUEFALSE' )
  call ut%assert_equal( 'Logical TRUEFALSE', chr, [ 'TRUE ', 'TRUE ', 'TRUE ', 'FALSE', 'TRUE ' ] )
  chr = character( bool, fmt='tf' )
  call ut%assert_equal( 'Logical tf', chr, [ 't', 't', 't', 'f', 't' ] )
  chr = character( bool, fmt='TF' )
  call ut%assert_equal( 'Logical TF', chr, [ 'T', 'T', 'T', 'F', 'T' ] )
  chr = character( bool, fmt='01' )
  call ut%assert_equal( 'Logical 01', chr, [ '1', '1', '1', '0', '1' ] )
  chr = character( bool, fmt='any' )
  call ut%assert_equal( 'Logical invalid format', chr, [ 'T', 'T', 'T', 'F', 'T' ] )

! Convert integer to character string
  chr = character( integ )
  call ut%assert_equal( 'Integer(kind=4)', chr, [ '1000', '25  ', '352 ', '666 ', '963 ' ] )
  chr = character( integ2 )
  call ut%assert_equal( 'Integer(kind=2)', chr, [ '100', '2  ', '35 ', '66 ', '96 ' ] )
  chr = character( integ1 )
  call ut%assert_equal( 'Integer(kind=1)', chr, [ '10', '0 ', '3 ', '6 ', '9 ' ] )

! Convert integer to character string forcing '+' sign
  chr = character( integ, sp=.true. )
  call ut%assert_equal( 'Integer(kind=4)', chr, [ '+1000', '+25  ', '+352 ', '+666 ', '+963 ' ] )
  chr = character( integ2, sp=.true. )
  call ut%assert_equal( 'Integer(kind=2)', chr, [ '+100', '+2  ', '+35 ', '+66 ', '+96 ' ] )
  chr = character( integ1, sp=.true. )
  call ut%assert_equal( 'Integer(kind=1)', chr, [ '+10', '+0 ', '+3 ', '+6 ', '+9 ' ] )

! Convert integer to character string with format given
  chr = character( integ, fmt='(i7.7)' )
  call ut%assert_equal( 'Integer(kind=4)', chr, [ '0001000', '0000025', '0000352', '0000666', '0000963' ] )
  chr = character( integ2, fmt='(i7.7)' )
  call ut%assert_equal( 'Integer(kind=2)', chr, [ '0000100', '0000002', '0000035', '0000066', '0000096' ] )
  chr = character( integ1, fmt='(spi7.6)' )
  call ut%assert_equal( 'Integer(kind=1)', chr, [ '+000010', '+000000', '+000003', '+000006', '+000009' ] )

! Convert floating point numbers to character string
  chr = character( real )
  call ut%assert_equal( 'Real(kind=4)', chr, [ '1.000000      ', &
                                                '0.2548000e-001', &
                                                '0.3525160     ', &
                                                '0.6669140     ', &
                                                '0.9630560     ' ], ignorecase=.true. )
  chr = character( dbl )
  call ut%assert_equal( 'Real(kind=8)', chr, [ '1.00000000000000      ', &
                                                '0.254800000000000E-001', &
                                                '0.352516000000000     ', &
                                                '0.666914000000000     ', &
                                                '0.963056000000000     ' ], ignorecase=.true. )

! Convert double to character string forcing + sign
  chr = character( real, sp=.true. )
  call ut%assert_equal( 'Real(kind=4)', chr, [ '+1.000000      ', &
                                                '+0.2548000e-001', &
                                                '+0.3525160     ', &
                                                '+0.6669140     ', &
                                                '+0.9630560     ' ], ignorecase=.true. )
  chr = character( dbl, sp=.true. )
  call ut%assert_equal( 'Real(kind=8)', chr, [ '+1.00000000000000      ', &
                                                '+0.254800000000000E-001', &
                                                '+0.352516000000000     ', &
                                                '+0.666914000000000     ', &
                                                '+0.963056000000000     ' ], ignorecase=.true. )

! Convert double to character string with format given
  chr = character( real, fmt='(f20.3)' )
  call ut%assert_equal( 'Real(kind=4)', chr, [ '               1.000', &
                                                '               0.025', &
                                                '               0.353', &
                                                '               0.667', &
                                                '               0.963' ] )
  chr = character( dbl, fmt='(f20.3)' )
  call ut%assert_equal( 'Real(kind=8)', chr, [ '               1.000', &
                                                '               0.025', &
                                                '               0.353', &
                                                '               0.667', &
                                                '               0.963' ] )

! Convert complex to character string
  chr = character( cmp )
  call ut%assert_equal( 'Complex(kind=4)', chr, &
                        [ '1.000000-i1.000000            ', &
                           '0.2548000E-001+i0.2548000E-001', &
                           '0.3525160-i0.3525160          ', &
                           '0.6669140+i0.6669140          ', &
                           '0.9630560-i0.9630560          ' ], &
                        ignorecase=.true. )
  chr = character( dcmp )
  call ut%assert_equal( 'Complex(kind=8)', chr, &
                        [ '1.00000000000000-i1.00000000000000            ', &
                           '0.254800000000000E-001+i0.254800000000000E-001', &
                           '0.352516000000000-i0.352516000000000          ', &
                           '0.666914000000000+i0.666914000000000          ', &
                           '0.963056000000000-i0.963056000000000          ' ], &
                        ignorecase=.true. )
  chr = character( cmp, 'j' )
  call ut%assert_equal( 'Complex (kind=4,j symbol)', chr, &
                        [ '1.000000-j1.000000            ', &
                           '0.2548000E-001+j0.2548000E-001', &
                           '0.3525160-j0.3525160          ', &
                           '0.6669140+j0.6669140          ', &
                           '0.9630560-j0.9630560          ' ], &
                        ignorecase=.true. )
  chr = character( dcmp, 'j' )
  call ut%assert_equal( 'Complex (kind=8,j symbol)', chr, &
                        [ '1.00000000000000-j1.00000000000000            ', &
                           '0.254800000000000E-001+j0.254800000000000E-001', &
                           '0.352516000000000-j0.352516000000000          ', &
                           '0.666914000000000+j0.666914000000000          ', &
                           '0.963056000000000-j0.963056000000000          ' ], &
                        ignorecase=.true. )

end subroutine unit_m_util_convert_test_001

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_util_convert_test_002( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! Local variables
  character(len=32) :: chr


! Convert character string to logical
  chr = '.true.'
  call ut%assert_true( 'Character to logical: .true.', logical(chr) )
  chr = '.false.'
  call ut%assert_false( 'Character to logical: .false.', logical(chr) )
  chr = 'yes'
  call ut%assert_true( 'Character to logical: yes', logical(chr) )
  chr = 'no'
  call ut%assert_false( 'Character to logical: no', logical(chr) )
  chr = 'true'
  call ut%assert_true( 'Character to logical: true', logical(chr) )
  chr = 'false'
  call ut%assert_false( 'Character to logical: false', logical(chr) )
  chr = '1'
  call ut%assert_true( 'Character to logical: 1', logical(chr) )
  chr = '0'
  call ut%assert_false( 'Character to logical: 0', logical(chr) )

! Convert character string to integer
  chr = '12345'
  call ut%assert_equal( 'Character to integer: Integer positive', integer(chr), 12345 )
  chr = '-12345'
  call ut%assert_equal( 'Character to integer: Integer negative', integer(chr), -12345 )
  chr = '-123.45'
  call ut%assert_equal( 'Character to integer: Real negative', integer(chr), -123 )
  chr = '-12345d-3'
  call ut%assert_equal( 'Character to integer: Double negative', integer(chr), -12 )
  chr = '-1234.5d-3'
  call ut%assert_equal( 'Character to integer: Double negative', integer(chr), -1 )
  chr = '1234.5d-3'
  call ut%assert_equal( 'Character to integer: Double positive', integer(chr), 1 )
  chr = '   12345   '
  call ut%assert_equal( 'Character to integer: Integer with spaces', integer(chr), 12345 )

! Convert character string to real
  chr = '12345.34'
  call ut%assert_equal( 'Character to real: Real positive', real(chr), 12345.34_4 )
  chr = '-12345.34'
  call ut%assert_equal( 'Character to real: Real negative', real(chr), -12345.34_4 )
  chr = '12345.34d3'
  call ut%assert_equal( 'Character to real: real positive', real(chr), 12345340.0_4 )
  chr = '12345.34a4'
  call ut%assert_equal( 'Character to real: Stop at letter', real(chr), 12345.34_4 )
  chr(1:1) = char(0)
  call ut%assert_equal( 'Character to real: Invalid first character', real(chr), 0.0_4 )

! Convert character string to double
  chr = '12345.34'
  call ut%assert_equal( 'Character to double: Real positive', double(chr), 12345.34_8 )
  chr = '-12345.34'
  call ut%assert_equal( 'Character to double: Real negative', double(chr), -12345.34_8 )
  chr = '12345.34d3'
  call ut%assert_equal( 'Character to double: Double positive', double(chr), 12345340.0_8 )
  chr = '12345.34a4'
  call ut%assert_equal( 'Character to double: Stop at letter', double(chr), 12345.34_8 )
  chr(1:1) = char(0)
  call ut%assert_equal( 'Character to double: Invalid first character', double(chr), 0.0_8 )

! Convert character string to complex
  chr = '12345.34'
  call ut%assert_equal( 'Character to complex: Real part only', &
                        complex(chr), cmplx(12345.34_8,kind=4) )
  chr = 'i12345.34'
  call ut%assert_equal( 'Character to complex: Imaginary part only', &
                        complex(chr), cmplx(0.0_8,12345.34_8,kind=4) )
  chr = '12345.34+i23456.34'
  call ut%assert_equal( 'Character to complex: Whole with i', &
                        complex(chr), cmplx(12345.34_8,23456.34_8,kind=4) )
  chr = '12345.34+j23456.34'
  call ut%assert_equal( 'Character to complex: Whole with j', &
                        complex(chr), cmplx(12345.34_8,23456.34_8,kind=4) )
  chr = '12345.34d2+i23456.34e-2'
  call ut%assert_equal( 'Character to complex: Mixed types', &
                        complex(chr), cmplx(1234534.0_8,234.5634_8,kind=4) )
  chr(1:1) = 'a'
  call ut%assert_equal( 'Character to complex: Invalid first character', &
                        complex(chr), cmplx(0.0_8,0.0_8,kind=4) )

! Convert character string to double complex
  chr = '12345.34'
  call ut%assert_equal( 'Character to dobule complex: Real part only', &
                        double_complex(chr), cmplx(12345.34_8,kind=8) )
  chr = 'i12345.34'
  call ut%assert_equal( 'Character to dobule complex: Imaginary part only', &
                        double_complex(chr), cmplx(0.0_8,12345.34_8,kind=8) )
  chr = '12345.34+i23456.34'
  call ut%assert_equal( 'Character to dobule complex: Whole with i', &
                        double_complex(chr), cmplx(12345.34_8,23456.34_8,kind=8) )
  chr = '12345.34+j23456.34'
  call ut%assert_equal( 'Character to dobule complex: Whole with j', &
                        double_complex(chr), cmplx(12345.34_8,23456.34_8,kind=8) )
  chr = '12345.34d2+i23456.34e-2'
  call ut%assert_equal( 'Character to dobule complex: Mixed types', &
                        double_complex(chr), cmplx(1234534.0_8,234.5634_8,kind=8) )
  chr(1:1) = 'a'
  call ut%assert_equal( 'Character to double complex: Invalid first character', &
                        double_complex(chr), cmplx(0.0_8,0.0_8,kind=8) )

end subroutine unit_m_util_convert_test_002

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_util_convert_test_003( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! Local variables
  character(len=7) :: cbuffer, cbuffer2
  integer(kind=1), dimension(len(cbuffer)) :: ibuffer, check
  integer :: i

! Initialise characters
  cbuffer = 'abcde'

! Convert characters to buffer
  ibuffer = character_to_bytes( cbuffer )
  do i = 1, len(cbuffer)
    check(i) = ichar(cbuffer(i:i))
  end do
  call ut%assert_equal( 'Char to buffer', ibuffer, check )

! Convert trimmed character string
  ibuffer = 0_1
  ibuffer(:len_trim(cbuffer)) = character_to_bytes( trim(cbuffer) )
  do i = 1, len_trim(cbuffer)
    check(i) = ichar(cbuffer(i:i))
  end do
  call ut%assert_equal( 'Char to buffer (trimmed)', ibuffer(:len_trim(cbuffer)), check(:len_trim(cbuffer)) )

! Convert buffer to characters
  ibuffer = [ int(z'61'), int(z'62'), int(z'63'), int(z'64'), int(z'65'), int(z'20'), int(z'20') ]
  cbuffer2 = bytes_to_character( ibuffer )
  call ut%assert_equal( 'Buffer to char', cbuffer2, cbuffer )

end subroutine unit_m_util_convert_test_003

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_util_convert_test_004( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! Local variables
  character(len=14) :: hbuffer, hbuffer2
  integer(kind=1), dimension(len(hbuffer)/2) :: buffer, check
  integer :: i

! Initialise characters
  hbuffer = '6A6B6F64652020'

! Convert hexadecimal to buffer
  buffer = hex_to_bytes( hbuffer )
  do i = 1, size(check)
    read( hbuffer(2*i-1:2*i), '(z2)' ) check(i:i)
  end do
  call ut%assert_equal( 'Hex to buffer', buffer, check )

! Convert buffer to hexadecimal
  buffer = [ int(z'6A'), int(z'6B'), int(z'6F'), int(z'64'), int(z'65'), int(z'20'), int(z'20') ]
  hbuffer2 = bytes_to_hex( buffer )
  call ut%assert_equal( 'Buffer to hex', hbuffer2, hbuffer )

! Convert buffer to hexadecimal (lowercase)
  buffer = [ int(z'6A'), int(z'6B'), int(z'6F'), int(z'64'), int(z'65'), int(z'20'), int(z'20') ]
  hbuffer2 = bytes_to_hex( buffer, .true. )
  call ut%assert_equal( 'Buffer to hex (lowercase)', hbuffer2, lowercase(hbuffer) )

! Convert integer to hexadecimal
  call ut%assert_equal( 'Integer to hex (kind=1)', hex(125_1), '7D' )
  call ut%assert_equal( 'Integer to hex (kind=2)', hex(4787_2), '12B3' )
  call ut%assert_equal( 'Integer to hex (kind=4)', hex(341099169), '1454C2A1' )
  call ut%assert_equal( 'Integer to hex lowercase (kind=1)', hex(125_1,.true.), '7d' )
  call ut%assert_equal( 'Integer to hex lowercase (kind=2)', hex(4787_2,.true.), '12b3' )
  call ut%assert_equal( 'Integer to hex lowercase (kind=4)', hex(341099169,.true.), '1454c2a1' )

end subroutine unit_m_util_convert_test_004

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_util_convert_test_005( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! Local variables
  integer(kind=1), dimension(4) :: buffer, buffer0
  integer(kind=1), dimension(32) :: blist

! Convert byte buffer to list of bits
  buffer0 = [ int(z'a4',1), int(z'86',1), int(z'0F',1), int(z'01',1) ]
  blist = bytes_to_bits_list(buffer0)
  call ut%assert_equal( 'Buffer to bits list', &
                        blist, &
                        transfer('10100100100001100000111100000001',blist)-ichar('0',kind=1) )

! Convert list of bits to list of bytes
  buffer = bits_list_to_bytes( blist )
  call ut%assert_equal( 'Bits list to buffer', buffer, buffer0 )

end subroutine unit_m_util_convert_test_005

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_util_convert_test_006( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! Local variables
  character(len=50) :: cbuffer

! Initialise characters
  cbuffer = 'abcdefghijklmnopqrstuvwxyz0123456789:_?ABCDEFG()'

! Convert to uppercase
  call ut%assert_equal( 'Uppercase', uppercase(cbuffer), 'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789:_?ABCDEFG()' )

! Convert to lowercase
  call ut%assert_equal( 'Lowercase', lowercase(cbuffer), 'abcdefghijklmnopqrstuvwxyz0123456789:_?abcdefg()' )

end subroutine unit_m_util_convert_test_006

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_util_convert_test_007( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! Check odd numbers
  call ut%assert_true( 'Odd 35713', odd(35713) )
  call ut%assert_true( 'Odd 713_2', odd(713_2) )
  call ut%assert_true( 'Odd 13_1', odd(13_1) )
  call ut%assert_false( 'Not Odd 35714', odd(35714) )
  call ut%assert_false( 'Not Odd 714_2', odd(714_2) )
  call ut%assert_false( 'Not Odd 14_1', odd(14_1) )

! Check even numbers
  call ut%assert_true( 'Even 35714', even(35714) )
  call ut%assert_true( 'Even 714_2', even(714_2) )
  call ut%assert_true( 'Even 14_1', even(14_1) )
  call ut%assert_false( 'Not Even 35713', even(35713) )
  call ut%assert_false( 'Not Even 713_2', even(713_2) )
  call ut%assert_false( 'Not Even 13_1', even(13_1) )

end subroutine unit_m_util_convert_test_007

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_util_convert_test_008( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! Local variables
  character(len=:), allocatable :: s, t
  character(len=128) :: char0 = 'ccsds:mois:navwg:opm'
  character(len=128) :: char1 = '/home/user/documents/atlantis/src/ccsds/utest/data/test_mandatory.dat'
  character(len=128) :: char2 = '/home////user/documents/atlantis//src/ccsds///utest/data/test_mandatory.dat'
  character(len=:), allocatable, dimension(:) :: tokens0, tokens1, tokens2, tokens

! Initialise
  allocate( tokens0(4), source=[ 'ccsds', 'mois ', 'navwg', 'opm  ' ] )
  allocate( tokens1(10), source=[ '                  ', 'home              ', &
                                   'user              ', 'documents         ', &
                                   'atlantis          ', 'src               ', &
                                   'ccsds             ', 'utest             ', &
                                   'data              ', 'test_mandatory.dat' ] )
  allocate( tokens2(16), source=[ '                  ', 'home              ', &
                                   '                  ', '                  ', &
                                   '                  ', 'user              ', &
                                   'documents         ', 'atlantis          ', &
                                   '                  ', 'src               ', &
                                   'ccsds             ', '                  ', &
                                   '                  ', 'utest             ', &
                                   'data              ', 'test_mandatory.dat' ] )

! Split
  s = char0
  call split( s, ':', tokens )
  call ut%assert_equal( 'Split (:)', tokens, tokens0 )

! Join
  t = join( tokens, ':' )
  call ut%assert_equal( 'Join (:)', t, s )
  
! Split
  s = char1
  call split( s, '/', tokens )
  call ut%assert_equal( 'Split [)', tokens, tokens1 )
  
! Join
  t = join( tokens, '/' )
  call ut%assert_equal( 'Join [)', t, s )

! Simple split with multiple separators flag
  s = char0
  call split( s, ':', tokens, .true. )
  call ut%assert_equal( 'Split (:, multiple flag)', tokens, tokens0 )
  
! Simple split with multiple separators flag
  s = char1
  call split( s, '/', tokens, .true. )
  call ut%assert_equal( 'Split [, mutliple flag)', tokens, tokens1 )
  
! Simple split with actual mutltiple separators
  s = char2
  call split( s, '/', tokens )
  call ut%assert_equal( 'Split [/]', tokens, tokens2 )
  
! Simple split with actual mutltiple separators
  s = char2
  call split( s, '/', tokens, .true. )
  call ut%assert_equal( 'Split [//, multiple flag)', tokens, tokens1 )

end subroutine unit_m_util_convert_test_008

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_util_convert_test_009( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! Local variables
  integer(kind=4) :: i4_little0
  data i4_little0 / z'1a1b1c1d' /
  integer(kind=4) :: i4_little, i4_big
  integer(kind=2) :: i2_little0
  data i2_little0 / z'1a1b' /
  integer(kind=2) :: i2_little, i2_big

! Initialise
  i4_little = i4_little0
  i2_little = i2_little0

! Convert 4 byts to big endian
  i4_big = change_endianness( i4_little )
  call ut%assert_equal( 'Big endian d-word', hex(i4_big,.true.), '1d1c1b1a' )

! Convert 4 byts to little endian
  i4_little = 0
  i4_little = change_endianness( i4_big )
  call ut%assert_equal( 'Little endian d-word', hex(i4_little,.true.), '1a1b1c1d' )

! Convert 2 byts to big endian
  i2_big = change_endianness( i2_little )
  call ut%assert_equal( 'Big endian word', hex(i2_big,.true.), '1b1a' )

! Convert 2 byts to little endian
  i2_little = 0_2
  i2_little = change_endianness( i2_big )
  call ut%assert_equal( 'Little endian word', hex(i2_little,.true.), '1a1b' )

end subroutine unit_m_util_convert_test_009

end module unit_m_util_convert_tests

