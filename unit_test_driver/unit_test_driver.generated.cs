





using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace UnitTestDriver
{


		[TestClass]
		public class fommons : Unit
		{

				[TestMethod]
				public void unit_m_c_string()
				{
					// Initialise the test run
					Initialise("unit_m_c_string");

					// Execute the fortran test if required
					if(OutOfDate) RunFortranTest();

					// Assert the test completion
					Assert.AreEqual(ProcessJxmlFile("m_c_string"), 0);
				}

				[TestMethod]
				public void unit_m_file_handler()
				{
					// Initialise the test run
					Initialise("unit_m_file_handler");

					// Execute the fortran test if required
					if(OutOfDate) RunFortranTest();

					// Assert the test completion
					Assert.AreEqual(ProcessJxmlFile("m_file_handler"), 0);
				}

				[TestMethod]
				public void unit_m_iso8601_date()
				{
					// Initialise the test run
					Initialise("unit_m_iso8601_date");

					// Execute the fortran test if required
					if(OutOfDate) RunFortranTest();

					// Assert the test completion
					Assert.AreEqual(ProcessJxmlFile("m_iso8601_date"), 0);
				}

				[TestMethod]
				public void unit_m_iso8601_date_time()
				{
					// Initialise the test run
					Initialise("unit_m_iso8601_date_time");

					// Execute the fortran test if required
					if(OutOfDate) RunFortranTest();

					// Assert the test completion
					Assert.AreEqual(ProcessJxmlFile("m_iso8601_date_time"), 0);
				}

				[TestMethod]
				public void unit_m_iso8601_time()
				{
					// Initialise the test run
					Initialise("unit_m_iso8601_time");

					// Execute the fortran test if required
					if(OutOfDate) RunFortranTest();

					// Assert the test completion
					Assert.AreEqual(ProcessJxmlFile("m_iso8601_time"), 0);
				}

				[TestMethod]
				public void unit_m_messages()
				{
					// Initialise the test run
					Initialise("unit_m_messages");

					// Execute the fortran test if required
					if(OutOfDate) RunFortranTest();

					// Assert the test completion
					Assert.AreEqual(ProcessJxmlFile("m_messages"), 0);
				}

				[TestMethod]
				public void unit_m_object()
				{
					// Initialise the test run
					Initialise("unit_m_object");

					// Execute the fortran test if required
					if(OutOfDate) RunFortranTest();

					// Assert the test completion
					Assert.AreEqual(ProcessJxmlFile("m_object"), 0);
				}

				[TestMethod]
				public void unit_m_path()
				{
					// Initialise the test run
					Initialise("unit_m_path");

					// Execute the fortran test if required
					if(OutOfDate) RunFortranTest();

					// Assert the test completion
					Assert.AreEqual(ProcessJxmlFile("m_path"), 0);
				}

				[TestMethod]
				public void unit_m_string()
				{
					// Initialise the test run
					Initialise("unit_m_string");

					// Execute the fortran test if required
					if(OutOfDate) RunFortranTest();

					// Assert the test completion
					Assert.AreEqual(ProcessJxmlFile("m_string"), 0);
				}

				[TestMethod]
				public void unit_m_util_convert()
				{
					// Initialise the test run
					Initialise("unit_m_util_convert");

					// Execute the fortran test if required
					if(OutOfDate) RunFortranTest();

					// Assert the test completion
					Assert.AreEqual(ProcessJxmlFile("m_util_convert"), 0);
				}

		}

}
