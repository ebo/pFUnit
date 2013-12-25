cmake_minimum_required(VERSION 2.8)

include_directories (${pFUnit_SOURCE_DIR}/include)
include_directories (${pFUnit_BINARY_DIR}/source)

set(srcs Test_Assert.F90)
list(APPEND srcs MakeNaN.F90)
list(APPEND srcs MakeInfinity.F90)
list(APPEND srcs Test_AssertReal.F90)
list(APPEND srcs Test_AssertBasic.F90)
list(APPEND srcs BrokenSetUpCase.F90)
list(APPEND srcs BrokenTestCase.F90)
list(APPEND srcs FixtureTestCase.F90)
list(APPEND srcs MockListener.F90)
list(APPEND srcs SimpleTestCase.F90)
list(APPEND srcs Test_Assert.F90)
list(APPEND srcs Test_AssertComplex.F90)
list(APPEND srcs Test_AssertInteger.F90)
list(APPEND srcs Test_Exception.F90)
list(APPEND srcs Test_FixtureTestCase.F90)
list(APPEND srcs Test_MockCall.F90)
list(APPEND srcs Test_MockRepository.F90)
list(APPEND srcs Test_RobustRunner.F90)
list(APPEND srcs Test_SimpleTestCase.F90)
list(APPEND srcs Test_StringConversionUtilities.F90)
list(APPEND srcs Test_TestMethod.F90)
list(APPEND srcs Test_TestResult.F90)
list(APPEND srcs Test_TestSuite.F90)
list(APPEND srcs Test_UnixProcess.F90)
list(APPEND srcs robustTestSuite.F90)

if ( USEMPI )
  list(APPEND srcs Test_MpiContext.F90)
  list(APPEND srcs Test_MpiException.F90)
  list(APPEND srcs Test_MpiTestCase.F90)
endif()

add_library(testpfunit STATIC ${srcs})
target_link_libraries(testpfunit pfunit)

set(TEST_EXE tests.x)
set(REMOTE_EXE remote.x)

add_executable (${TEST_EXE} selfTests.F90)
add_executable (${REMOTE_EXE} RemoteProgram.F90)

add_dependencies(tests.x pfunit)

target_link_libraries(${TEST_EXE} pfunit testpfunit)
target_link_libraries(${REMOTE_EXE} pfunit testpfunit)

if ( USEMPI )
     target_link_libraries(${REMOTE_EXE} ${MPI_Fortran_LIBRARIES} )
     target_link_libraries(${REMOTE_EXE} ${MPI_Fortran_LIBRARIES} )
     add_custom_target(tests COMMAND mpirun -np 4 .${TEST_EXE} DEPENDS ${TEST_EXE} ${REMOTE_EXE})
else()
     add_custom_target(tests COMMAND ${TEST_EXE} DEPENDS ${TEST_EXE} ${REMOTE_EXE} WORKING_DIRECTORY ${CMAKE_BINARY_DIR})
endif ()