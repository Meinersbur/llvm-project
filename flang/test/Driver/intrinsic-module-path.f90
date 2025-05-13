! Ensure argument -fintrinsic-modules-path works as expected.
! The driver will add the builtin search path to the -fc1 command, in addition to the -fintrinsic-modules-path provided by the user.
! For regression tests, the default -fintrinsic-modules-path is provided by %flang_fc1.


! WITHOUT the option, the default location for the module is checked and no error generated.
! With the option GIVEN, the module with the same name is PREPENDED, and considered over the
! default one, causing a CHECKSUM error.

!-----------------------------------------
! FRONTEND FLANG DRIVER (flang -fc1)
!-----------------------------------------
! RUN: not %flang -fc1 -fsyntax-only %s  2>&1 | FileCheck %s --allow-empty --check-prefix=NONE
! RUN: %flang_fc1  -fsyntax-only %s  2>&1 | FileCheck %s --allow-empty --check-prefix=WITHOUT
! RUN: %flang -fsyntax-only %s  2>&1 | FileCheck %s --allow-empty --check-prefix=WITHOUT

! RUN: not %flang -fc1 -fsyntax-only -fintrinsic-modules-path %S/Inputs/ %s  2>&1 | FileCheck %s --allow-empty --check-prefix=GIVEN
! RUN: %flang_fc1  -fsyntax-only -fintrinsic-modules-path %S/Inputs/ %s  2>&1 | FileCheck %s --allow-empty --check-prefix=WITHOUT
! RUN: not %flang -fsyntax-only -fintrinsic-modules-path %S/Inputs/ %s  2>&1 | FileCheck %s --allow-empty --check-prefix=GIVEN



! NONE: 'ieee_arithmetic.mod' was not found
! NONE: 'iso_fortran_env.mod' was not found

! WITHOUT-NOT: 'ieee_arithmetic.mod' was not found
! WITHOUT-NOT: 'iso_fortran_env.mod' was not found

! GIVEN: error: Cannot use module file for module 'ieee_arithmetic': File has invalid checksum
! GIVEN: error: Cannot use module file for module 'iso_fortran_env': File has invalid checksum


program test_intrinsic_module_path
   use ieee_arithmetic, only: ieee_round_type
   use iso_fortran_env, only: team_type, event_type, lock_type
end program
