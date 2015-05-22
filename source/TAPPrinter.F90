!-------------------------------------------------------------------------------
! NASA/GSFC Advanced Software Technology Group
!-------------------------------------------------------------------------------
!  MODULE: TAPPrinter
!
!> @brief
!! This driver produces Testing Anything Protocol (TAP) output for the
!! pFUnit framework (See https://testanything.org/ for more details).
!! Using the Simple example provided with this package, we get the
!! following output:
!!
!! 1..31 - default_suite_name
!! # RUN_TIME:   0.000000
!! ok 1 - helloWorld_suite.testHelloWorld
!! ok 2 - helloWorld_suite.testBrokenHelloWorld
!! not ok 3 - helloWorld_suite.testBrokenHelloWorld
!! ...
!!
!! The first line contains the number of tests and the test name (ie
!! 31 tests, numbered 1 to 31, with the test suite name
!! "default_suite_name").  That is followed by a series of numbered
!! test results and optional comments.  The test results are either
!! "ok" or "not ok" followed by the test number and an optional test
!! descriptor. In addition to the basic pass/fail results for the
!! test, there is also a "bail out" which signifies that the test
!! suite crashed and not further tests were run.
!!
!! It is also important to note that the TAP protocol also allows for
!! designating skipped tests and test stubs (called todo), but pFUnit
!! currently does not include the appropriate functionality to
!! accommodate these primitives.
!!
!! @author
!! John (EBo) David, SSAI/NASA-GSFC 
!!
!! @date
!! 22 May 2015
!! 
!! @note <A note here.>
!! <Or starting here...>
!
! REVISION HISTORY:
!
! 22 May 2015. Initial commit.
!
!-------------------------------------------------------------------------------
module TAPPrinter_mod
   use Exception_mod
   use TestListener_mod, only : TestListener
   implicit none
   private

   public :: TAPPrinter
   public :: newTAPPrinter

   type, extends(TestListener) :: TAPPrinter
      integer :: unit
      integer :: column
   contains
      procedure :: addFailure
      procedure :: addError
      procedure :: startTest
      procedure :: endTest
      procedure :: endRun
      procedure :: print
   end type TAPPrinter

   integer, parameter :: MAX_COLUMN = 40
   logical, parameter :: DEBUG = .false.
!!$   logical, parameter :: DEBUG = .true.

   integer :: numMessages
   character(len=255), dimension (:), allocatable :: messages(:)
contains

  function newTAPPrinter(unit)
     type (TAPPrinter) :: newTAPPrinter
     integer, intent(in) :: unit

     newTAPPrinter%unit = unit
     newTAPPrinter%column = 0
     numMessages = 0
     allocate(messages(0))

  end function newTAPPrinter

  subroutine addFailure(this, testName, exceptions)
     use Exception_mod
     class (TAPPrinter), intent(inOut) :: this
     character(len=*), intent(in) :: testName
     type (Exception), intent(in) :: exceptions(:)
     character(len=255), dimension (:), allocatable :: tmp(:)
     character(len=255) :: str
     integer :: n

     n = numMessages
     numMessages = numMessages + 1

     write(str,'(a,i0,a,a)') "not ok ", numMessages, " - ",  trim(testName)

     allocate(tmp(n))
     tmp(1:n) = messages(1:n)
     deallocate(messages)
     allocate(messages(n+1))
     messages(1:n) = tmp
     messages(n+1) = trim(str)
     deallocate(tmp)
     
  end subroutine addFailure

  subroutine addError(this, testName, exceptions)
     use Exception_mod
     class (TAPPrinter), intent(inOut) :: this
     character(len=*), intent(in) :: testName
     type (Exception), intent(in) :: exceptions(:)
     character(len=255), dimension (:), allocatable :: tmp(:)
     character(len=255) :: str
     integer :: n

     n = numMessages
     numMessages = numMessages + 1

     write(str,'(a,i0,a,a)') "Bail out! ",  trim(testName)

     allocate(tmp(n))
     tmp(1:n) = messages(1:n)
     deallocate(messages)
     allocate(messages(n+1))
     messages(1:n) = tmp
     messages(n+1) = trim(str)
     deallocate(tmp)

  end subroutine addError

  subroutine startTest(this, testName)
     class (TAPPrinter), intent(inOut) :: this
     character(len=*), intent(in) :: testName
     character(len=255), dimension (:), allocatable :: tmp(:)
     character(len=255) :: str
     integer :: n

     n = numMessages
     numMessages = numMessages + 1

     write(str,'(a,i0,a,a)') "ok ", numMessages, " - ",  trim(testName)

     allocate(tmp(n))
     tmp(1:n) = messages(1:n)
     deallocate(messages)
     allocate(messages(n+1))
     messages(1:n) = tmp
     messages(n+1) = trim(str)
     deallocate(tmp)

   end subroutine startTest

  subroutine endTest(this, testName)
     class (TAPPrinter), intent(inOut) :: this
     character(len=*), intent(in) :: testName

     if (DEBUG) then
        write(this%unit,*)trim(testName)
        call flush(this%unit)
     end if

   end subroutine endTest

   subroutine endRun(this, result)
      use AbstractTestResult_mod, only : AbstractTestResult
      class (TAPPrinter), intent(inout) :: this
      class (AbstractTestResult), intent(in) :: result

      call this%print(result)

    end subroutine endRun

   subroutine print(this, result)
      use AbstractTestResult_mod, only : AbstractTestResult
      class (TAPPrinter), intent(in) :: this
      class (AbstractTestResult), intent(in) :: result
      integer :: i

      write(this%unit,'(a,i0,a, a)')"1..", numMessages, " - ", trim(result%getName())
      write(this%unit,'(a,f10.6)') "# RUN_TIME: ", result%getRunTime()
      write(*,*) "runtime: ", result%getRunTime()

      do i = 1, size(messages)
         write(this%unit,'(a)') trim(messages(i))
      end do
      flush(this%unit)

   end subroutine print

end module TAPPrinter_mod
