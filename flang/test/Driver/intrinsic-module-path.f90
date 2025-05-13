! Ensure argument -fintrinsic-modules-path works as expected.
! The driver will add the builtin search path to the -fc1 command, in addition
! to the -fintrinsic-modules-path provided by the user. For regression tests,
! the default -fintrinsic-modules-path is provided already by %flang_fc1.

! Not providing any default -fintrinsic-modules-path
! RUN: not %flang -fc1 -fsyntax-only                                     %s 2>&1 | FileCheck %s --check-prefix=NONE
! RUN: not %flang -fc1 -fsyntax-only -fintrinsic-modules-path %S/Inputs/ %s 2>&1 | FileCheck %s --check-prefix=GIVEN

! Default -fintrinsic-modules-path provided by test infrastructure, appears before user-provided ones
! RUN: %flang_fc1  -fsyntax-only                                     %s 2>&1 | FileCheck %s --check-prefix=DEFAULT --allow-empty
! RUN: %flang_fc1  -fsyntax-only -fintrinsic-modules-path %S/Inputs/ %s 2>&1 | FileCheck %s --check-prefix=DEFAULT --allow-empty

! Default -fintrinsic-modules-path provided by driver, user-provided ones take priority
! RUN:     %flang -###  -fsyntax-only                                     %s 2>&1 | FileCheck %s --check-prefix=DRIVERARGS
! RUN:     %flang       -fsyntax-only                                     %s 2>&1 | FileCheck %s --check-prefix=DEFAULT --allow-empty
! RUN:     %flang -###  -fsyntax-only -fintrinsic-modules-path %S/Inputs/ %s 2>&1 | FileCheck %s --check-prefix=DRIVERARGS
! RUN: not %flang       -fsyntax-only -fintrinsic-modules-path %S/Inputs/ %s 2>&1 | FileCheck %s --check-prefix=GIVEN

! NONE: 'ieee_arithmetic.mod' was not found
! NONE: 'iso_fortran_env.mod' was not found

! DEFAULT-NOT: 'ieee_arithmetic.mod' was not found
! DEFAULT-NOT: 'iso_fortran_env.mod' was not found

! GIVEN: error: Cannot use module file for module 'ieee_arithmetic': File has invalid checksum
! GIVEN: error: Cannot use module file for module 'iso_fortran_env': File has invalid checksum

! DRIVERARGS: "-fc1"
! DRIVERARGS-SAME: "-fintrinsic-modules-path" "{{.*}}{{[/\\]}}finclude{{[/\\]}}{{.*}}"


program test_intrinsic_module_path
   use ieee_arithmetic, only: ieee_round_type
   use iso_fortran_env, only: team_type, event_type, lock_type
end program
