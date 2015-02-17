MODULE Program 0 0
ENDHDR
PROC Test.initialize 0 0 0
END

PROC Test.increment 0 0 0
END

PROC Test.allow 4 0 0
END

PROC Delegate.increment 0 0 0
END

PROC Program.%main 0 0 0
  CONST 4
  GLOBAL Lib.Print
  CALL 1
  RETURN
END

DEFINE Object

! Descriptor for Test
DEFINE Test
! size - 8
WORD Test.initialize
WORD Test.increment
WORD Test.allow
DEFINE Test.%super
WORD Object
WORD Test

! Descriptor for Delegate
DEFINE Delegate
! size - 8
WORD Test.initialize
WORD Delegate.increment
WORD Test.allow
DEFINE Delegate.%super
WORD Object
WORD Test
WORD Delegate
