program unit_m_util_convert

!-------------------------------------------------------------------------------
! Copyright : 2022, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
! Project   : Atlantis
! Author    : Fran Martinez Fadrique
! Language  : Object Oriented Fortran 2018
! Synopsis  : Unit tests driver for m_util_convert
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

  use unit_m_util_convert_tests

!---End of use statements-------------------------------------------------------

  implicit none

  character(len=*), parameter :: package = 'utilities'
  character(len=*), parameter :: module = 'm_util_convert'
  
!---Declaration of local variables----------------------------------------------


!---End of declaration of local variables---------------------------------------

!---Executable Code-------------------------------------------------------------

! Local variables
  character(len=256) :: xfunit_root_dir
  logical :: junit_strict
  type(t_xfunit_suite), allocatable :: suite
  type(t_xfunit_unit), allocatable :: ut

! Initialise report generation flag
  junit_strict = .false.

! Initialise unit test infrastructure
  allocate( manager )
  allocate( suite )
  allocate( ut )

! Initialise the unit test manager
  call get_environment_variable( 'XFUNIT_ROOT_DIR', xfunit_root_dir )
  manager = xfunit_manager_eclipse( module, xfunit_root_dir, junit_strict )

! Initialise test suite
  suite = xfunit_suite( package=package, &
                        source='m_util_convert.f03', &
                        annotation='General basic type conversions' )

! Create test
  ut = xfunit_unit( name='unit_m_util_convert_test_001', &
                    annotation='Conversions to character', &
                    executer=unit_m_util_convert_test_001 )
  call suite%add_unit_test( ut )

! Create test
  ut = xfunit_unit( name='unit_m_util_convert_test_002', &
                    annotation='Conversions from character', &
                    executer=unit_m_util_convert_test_002 )
  call suite%add_unit_test( ut )

! Create test
  ut = xfunit_unit( name='unit_m_util_convert_test_003', &
                    annotation='Character bytes', &
                    executer=unit_m_util_convert_test_003 )
  call suite%add_unit_test( ut )

! Create test
  ut = xfunit_unit( name='unit_m_util_convert_test_004', &
                    annotation='Bytes hexadecimal', &
                    executer=unit_m_util_convert_test_004 )
  call suite%add_unit_test( ut )

! Create test
  ut = xfunit_unit( name='unit_m_util_convert_test_005', &
                    annotation='Bits and bytes', &
                    executer=unit_m_util_convert_test_005 )
  call suite%add_unit_test( ut )

! Create test
  ut = xfunit_unit( name='unit_m_util_convert_test_006', &
                    annotation='Uppercase and lowercase', &
                    executer=unit_m_util_convert_test_006 )
  call suite%add_unit_test( ut )

! Create test
  ut = xfunit_unit( name='unit_m_util_convert_test_007', &
                    annotation='Odd and even numbers', &
                    executer=unit_m_util_convert_test_007 )
  call suite%add_unit_test( ut )

! Create test
  ut = xfunit_unit( name='unit_m_util_convert_test_008', &
                    annotation='Character string split and join', &
                    executer=unit_m_util_convert_test_008 )
  call suite%add_unit_test( ut )

! Create test
  ut = xfunit_unit( name='unit_m_util_convert_test_009', &
                    annotation='Endianness change', &
                    executer=unit_m_util_convert_test_009 )
  call suite%add_unit_test( ut )

! Execute tests
  call manager%execute( suite )
  if( manager%is_error() ) then
    call manager%dump_error( 0 )
  end if

! Generate output
  call manager%write_xml( suite )

! Terminate unit test infrastructure
  deallocate( manager )
  deallocate( suite )
  deallocate( ut )

end program unit_m_util_convert

