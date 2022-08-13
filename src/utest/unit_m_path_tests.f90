module unit_m_path_tests

!-------------------------------------------------------------------------------
! Copyright : 2022, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
! Synopsis  : Unit tests for m_path
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

  use m_path

!---End of use statements-------------------------------------------------------

  implicit none

!---Public/Private declarations-------------------------------------------------

  private


  public unit_m_path_test_001

  public unit_m_path_test_002

  public unit_m_path_test_003

  public unit_m_path_test_004

  public manager

!---End of public/private declarations------------------------------------------

  character(len=*), parameter :: package = 'utilities'
  character(len=*), parameter :: module = 'm_path'

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

subroutine unit_m_path_test_001( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! The path structure
  type(t_path) :: handler

! Local variables
  character(len=32) :: file = 'xfunit_test.dat'
  character(len=256) :: filename
  type(t_string) :: tfilename

! Intialise handler
  filename = '/home/user/software/..' // '\' // file
  handler = path( filename )

! Get handler parts
  call ut%assert_equal( 'Full path from character', character(handler%to_string()), '/home/user/xfunit_test.dat' )
  call ut%assert_equal( 'Path from character', character(handler%directory()), '/home/user' )
  call ut%assert_equal( 'Name from character', character(handler%name()), 'xfunit_test' )
  call ut%assert_equal( 'File name from character', character(handler%file_name()), 'xfunit_test.dat' )
  call ut%assert_true( 'Has extension from character', handler%has_extension() )
  call ut%assert_equal( 'Extension from character', character(handler%extension()), 'dat' )
  call ut%assert_true( 'Is abaolute from character', handler%is_absolute() )
  call handler%change_extension( string('.apx') )
  call ut%assert_true( 'Has modified extension from string', handler%has_extension() )
  call ut%assert_equal( 'Modified xtension from string', character(handler%extension()), 'apx' )
  call ut%assert_equal( 'File name with modified extension from character', character(handler%file_name()), 'xfunit_test.apx' )

! Intialise handler
  tfilename = filename
  handler = path( tfilename )

! Get handler parts
  call ut%assert_equal( 'Full path from string', character(handler%to_string()), '/home/user/xfunit_test.dat' )
  call ut%assert_equal( 'Path from string', character(handler%directory()), '/home/user' )
  call ut%assert_equal( 'Name from string', character(handler%name()), 'xfunit_test' )
  call ut%assert_equal( 'File name from string', character(handler%file_name()), 'xfunit_test.dat' )
  call ut%assert_true( 'Has extension from string', handler%has_extension() )
  call ut%assert_equal( 'Extension from string', character(handler%extension()), 'dat' )
  call ut%assert_true( 'Is abaolute from string', handler%is_absolute() )
  call handler%change_extension( 'dot' )
  call ut%assert_true( 'Has modified extension from string', handler%has_extension() )
  call ut%assert_equal( 'Modified xtension from string', character(handler%extension()), 'dot' )
  call ut%assert_equal( 'File name with modified extension from string', character(handler%file_name()), 'xfunit_test.dot' )

end subroutine unit_m_path_test_001

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_path_test_002( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! The path structure
  type(t_path) :: handler

! Local variables
  character(len=32) :: file = 'xfunit_test'
  character(len=256) :: filename
  type(t_path) :: cwd

! Initialise
  cwd = path( manager%get_unit_root_dir() )
  call cwd%normalise()

! Intialise handler
  filename = '../.././dir/../local/' // file
  handler = path( filename )

! Get handler parts
  call ut%assert_equal( 'Full path', character(handler%to_string()), '../../local/xfunit_test' )
  call ut%assert_equal( 'Path', character(handler%directory()), '../../local' )
  call ut%assert_equal( 'Name', character(handler%name()), 'xfunit_test' )
  call ut%assert_equal( 'File name', character(handler%file_name()), 'xfunit_test' )
  call ut%assert_false( 'Has extension', handler%has_extension() )
  call ut%assert_equal( 'Extension', character(handler%extension()), '' )
  call ut%assert_false( 'Is abaolute', handler%is_absolute() )
  call handler%change_extension( string('.dot') )
  call ut%assert_true( 'Has modified extension', handler%has_extension() )
  call ut%assert_equal( 'Modified xtension', character(handler%extension()), 'dot' )
  call ut%assert_equal( 'File name with modified extension', character(handler%file_name()), 'xfunit_test.dot' )


end subroutine unit_m_path_test_002

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_path_test_003( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! The path structure
  type(t_path) :: handler

! Local variables
  character(len=32) :: file = 'xfunit_test'
  character(len=256) :: filename
  type(t_string) :: tfilename, tdir

! Initialise the reference directory and file
  tdir = manager%get_unit_ref_dir() // '/path'
  tfilename = manager%get_unit_ref_dir() // '/path/path_test_exists.txt'

! Check a file that exists
  handler = path( tfilename )
  call ut%assert_true( 'File exists', handler%exists() )

! Check a file that does not exist
  handler = path( tfilename // '.not' )
  call ut%assert_false( 'File does not exist', handler%exists() )

! Isolated file name without extension
  tfilename = file
  handler = path( tfilename )
  call ut%assert_equal( 'Isolated file directory', character(handler%directory()), '' )
  call ut%assert_equal( 'Isolated file name', character(handler%file_name()), file )
  call ut%assert_equal( 'Isolated file extension', character(handler%extension()), '' )

! Default constructor
  handler = path()
  call ut%assert_equal( 'Default constructor', character(handler%to_string()), '.' )

! Normalise with /./ pattern in path
  handler = path( '/home/fmmf/./Documents/./propio/private' )
  call handler%normalise()
  call ut%assert_equal( 'Path with /./ pattern', character(handler%to_string()), '/home/fmmf/Documents/propio/private' )

! Constructor from tokens
  handler = path( [ '/home    ', '/fmmf    ', 'Documents' ] )
  call ut%assert_equal( 'Contructor from tokens', character(handler%to_string()), '/home/fmmf/Documents' )

! Static character call to change extension
  tfilename = path_change_extension( file, '.exe' )
  call ut%assert_equal( 'Extension change static', tfilename%character(), 'xfunit_test.exe' )

end subroutine unit_m_path_test_003

! ############################################################################
! ############################################################################
! ############################################################################

subroutine unit_m_path_test_004( ut )

! The unit test
  class(t_xfunit_unit), intent(inout) :: ut

! The path structure
  type(t_path) :: handler

! Local variables
  type(t_string) :: tmpdir, tmpfile
  character(len=255) :: envvar

! Get path to temporary directory
  tmpdir = path_temp_path()
  call get_environment_variable( 'TMP', envvar )
  call ut%assert_equal( 'Path to temporary directory', tmpdir, string(trim(envvar)) )

! Get temporary file name
  tmpfile = path_temp_file_name()
  call ut%assert_equal( 'Path to temporary file', tmpfile, string(trim(envvar)//'*.tmp'), &
                        matching=xfunit_assertion_character_match_global )

end subroutine unit_m_path_test_004

end module unit_m_path_tests

