# Fommons
Fommons (Fortran Commons) is a collection of common low level components to support XFunit, FTL and Frypto.

## The Fommons collection
Fommons encompasses the following Fortran modules:
- `m_object`: base building object providing low-level services to derived classes (hash, equality).
- `m_string`: dynamci character string, compatible with the Fortarn `character` type.
- `m_file_handler`: integrated management of file handling (open, close, access, read/write, ...).
- `m_iso8601_date`, `m_iso8601_time`, `m_iso8601_datetime`: management of date/time according to the ISO8601 convention.
- `m_msg`, `m_messages`: error handling
- `m_path`: Management of path (file and directory) strings.
- `m_util_convert`: Basic conversion amond different intrinsic data types.
- `m_xml`, `m_xml`,`m_xml_attribute`,`m_xml_encoder`,`m_xml_writer`, `m_xml_writer_settings`: support for the generation of XML documents

## Dependencies with XFunit

Like the other libraries in the collection, XFunit depends on Fommons. At the same time, Fommons depens on XFunit for its unit testing. The aparent circular dependency is not such taking into account.
- The Fommons library can be build indenpendently of any other library.
- The XFunit library depends on on Fommons, but only in the library, not on the unit tests.
- The Fommons unit tests are build from the Fommons unit test sources and the XFunit and Fommons libraries.
- The Xfunit unit tests are build from the XFunit unit test sources and the XFunit and Fommons libraries.

In both Windows and Linux deploy the Fommons and XFunit from the same root directory
- Windows: `Projects/Fommons` and `Projects/XFunit`. The `.sln` files are prepared for this configuration.
- Linux: `Projects/fommons` and `Projects/xfunit`. The `gmake` files are prepared for this configuration (mind the lowercase for the library folder names).
 
## Building and testing Fommons
Fommons has been tested with Intel Fortran 19 (or higher) and gfortran 9.4 (or higher).

The tests provided along with the Fommons librariy are written using XFunit.

### Windows
Fommons is provided with a Visual Studio 2019 configured solution that allows building and testing the entire suite. The solution provides a C# project that integrates the unit test in Fortran with the unit test detection feature of Visual Studio. This allows the execution of all unit tests from the Test Explorer menu.

Create the environment variable `XFUNIT_ROOT_DIR=$(ProjectDir)..` for each unit testing project to store the test output (.jxml file) in the `utest` directory. This configuration depends on the windows user in file `unit_m_xfunit_<functionname>.vfproj.<windows user>.user`

### Linux
Fommons is provided with `gmake` makefiles to build and test the entire suite. 
To build the Fommons library and use modules files execute the following command in the `src` directory
```make
gmake libs
```
To build the Fommons library, use modules files and unit tests execute the following command in the `src` directory
```make
gmake all
```
To execute the unit tests execute the following command in the `src` or `utest` directory
```make
gmake units
```
The default compiler is `gfortran` but can be overridden from the command line to use Intel Fortran
```make
gmake F90COMP=ifort
```
The ifort or gfortran commands must be in the execution path.
Alternatively, the makefile can be edited to provide the full path to the compiler.
Compiler options are automatically selected for the active compiler; only Debug configuration is provided in the make files.

## Documentation
This readme page is the main user documentation. In addition, documentation generated with FORD and Doxygen can be found in the `documentation` directory

## Licensing
Fommons is open-source software, licensed under the GNU Lesser General Public License (LGPL).

