#===-- lib/runtime/CMakeLists.txt ------------------------------------------===#
#
# Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
# See https://llvm.org/LICENSE.txt for license information.
# SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
#
#===------------------------------------------------------------------------===#

include(AddFlangRTOffload)
# function checks
find_package(Backtrace)
set(HAVE_BACKTRACE ${Backtrace_FOUND})
set(BACKTRACE_HEADER ${Backtrace_HEADER})


# List of files that are buildable for all devices.
set(supported_sources
  ${FLANG_SOURCE_DIR}/lib/Decimal/binary-to-decimal.cpp
  ${FLANG_SOURCE_DIR}/lib/Decimal/decimal-to-binary.cpp
  ISO_Fortran_binding.cpp
  allocator-registry.cpp
  allocatable.cpp
  array-constructor.cpp
  assign.cpp
  buffer.cpp
  character.cpp
  connection.cpp
  copy.cpp
  derived-api.cpp
  derived.cpp
  descriptor-io.cpp
  descriptor.cpp
  dot-product.cpp
  edit-input.cpp
  edit-output.cpp
  environment.cpp
  external-unit.cpp
  extrema.cpp
  file.cpp
  findloc.cpp
  format.cpp
  inquiry.cpp
  internal-unit.cpp
  io-api.cpp
  io-api-minimal.cpp
  io-error.cpp
  io-stmt.cpp
  iostat.cpp
  matmul-transpose.cpp
  matmul.cpp
  memory.cpp
  misc-intrinsic.cpp
  namelist.cpp
  non-tbp-dio.cpp
  numeric.cpp
  pointer.cpp
  product.cpp
  pseudo-unit.cpp
  ragged.cpp
  stat.cpp
  sum.cpp
  support.cpp
  terminator.cpp
  tools.cpp
  transformational.cpp
  type-code.cpp
  type-info.cpp
  unit.cpp
  utf.cpp
)

# List of source not used for GPU offloading.
set(CMAKE_Fortran_MODULE_DIRECTORY "${LLVM_LIBRARY_OUTPUT_INTDIR}/../include/flang")
set(host_sources
  __cuda_builtins.f90
  __cuda_device.f90
  __fortran_builtins.f90
  __fortran_ieee_exceptions.f90
  __fortran_type_info.f90
  cudadevice.f90
  ieee_arithmetic.f90
  ieee_exceptions.f90
  ieee_features.f90
  iso_c_binding.f90
  iso_fortran_env_impl.f90
  iso_fortran_env.f90

  command.cpp
  complex-powi.cpp
  complex-reduction.c
  exceptions.cpp
  execute.cpp
  extensions.cpp
  main.cpp
  random.cpp
  reduce.cpp
  reduction.cpp
  stop.cpp
  temporary-stack.cpp
  time-intrinsic.cpp
  unit-map.cpp
)

set_source_files_properties(cudadevice.f90
  PROPERTIES COMPILE_OPTIONS "-xcuda"
)
set_source_files_properties(
  mma.f90
  cudadevice.f90
  PROPERTIES Fortran_PREPROCESS OFF
)

message("CMAKE_SYSTEM_PROCESSOR: ${CMAKE_SYSTEM_PROCESSOR}")
message("CMAKE_HOST_SYSTEM_PROCESSOR: ${CMAKE_HOST_SYSTEM_PROCESSOR}")
if  (CMAKE_SYSTEM_PROCESSOR STREQUAL "powerpc")
  list(APPEND host_source
    __ppc_types.f90
    __ppc_intrinsics.f90
    mma.f90
  )
endif ()

add_compile_options($<$<COMPILE_LANGUAGE:Fortran>:-mmlir> $<$<COMPILE_LANGUAGE:Fortran>:-ignore-missing-type-desc>)
#set(CMAKE_Fortran_PREPROCESS FALSE)
#add_compile_options(-mllvm -ignore-missing-type-desc)

file(GLOB_RECURSE public_headers
  "${FLANG_RT_SOURCE_DIR}/include/flang_rt/*.h"
  "${FLANG_SOURCE_DIR}/include/flang/Common/*.h"
  )

file(GLOB_RECURSE private_headers
  "${FLANG_RT_SOURCE_DIR}/lib/flang_rt/*.h"
  "${FLANG_SOURCE_DIR}/lib/Common/*.h"
  )


# Import changes from flang_rt.quadmath
get_target_property(f128_sources
  FortranFloat128MathILib INTERFACE_SOURCES
  )
if (f128_sources)
  # The interface may define special macros for Float128Math files,
  # so we need to propagate them.
  get_target_property(f128_defs
    FortranFloat128MathILib INTERFACE_COMPILE_DEFINITIONS
    )
  set_property(SOURCE ${f128_sources}
    APPEND PROPERTY COMPILE_DEFINITIONS
    ${f128_defs}
    )
  get_target_property(f128_include_dirs
    FortranFloat128MathILib INTERFACE_INCLUDE_DIRECTORIES
    )
  set_property(SOURCE ${f128_sources}
    APPEND PROPERTY INCLUDE_DIRECTORIES
    ${f128_include_dirs}
    )
else ()
  set(f128_sources "")
endif ()

set(sources ${supported_sources} ${host_sources} ${f128_sources})


if (NOT WIN32)
  add_flangrt_library(flang_rt.runtime STATIC SHARED
    ${sources}
    LINK_LIBRARIES ${Backtrace_LIBRARY}
    INSTALL_WITH_TOOLCHAIN
    ADDITIONAL_HEADERS ${public_headers} ${private_headers}
  )

  enable_cuda_compilation(flang_rt.runtime "${supported_sources}")
  enable_omp_offload_compilation(flang_rt.runtime "${supported_sources}")

  # Select a default runtime, which is used for unit and regression tests.
  get_target_property(default_target flang_rt.runtime.default ALIASED_TARGET)
  add_library(flang_rt.runtime.unittest ALIAS "${default_target}")
else()
  # Target for building all versions of the runtime
  add_custom_target(flang_rt.runtime)
  set_target_properties(flang_rt.runtime PROPERTIES FOLDER "Flang-RT/Meta")

  function (add_win_flangrt_runtime libtype suffix msvc_lib)
    set(name "flang_rt.runtime.${suffix}")
    add_flangrt_library(${name} ${libtype}
        ${sources}
        ${ARGN}
        LINK_LIBRARIES ${Backtrace_LIBRARY}
        ADDITIONAL_HEADERS ${public_headers} ${private_headers}
      )

    if (msvc_lib)
      set_target_properties(${name}
          PROPERTIES
            MSVC_RUNTIME_LIBRARY "${msvc_lib}"
        )
    endif ()

    # Setting an unique Fortran_MODULE_DIRECTORY is required for each variant to
    # write a different .mod file.
    set_target_properties(${name}
        PROPERTIES
          Fortran_MODULE_DIRECTORY "module.${suffix}"
      )

    enable_cuda_compilation(${name} "${supported_sources}")
    enable_omp_offload_compilation(${name} "${supported_sources}")
    add_dependencies(flang_rt.runtime ${name})
  endfunction ()

  # Variants of the static flang_rt for different versions of the msvc runtime.
  #
  # The dynamic/dynamic_dbg variants are not DLLs themselves, only require
  # linking to msvcrt(d).dll.
  # FIXME: Generating actual runtime DLLs is currently not possible. There are
  # two roadblocks:
  #
  #  * Flang emits /DEFAULTLIB:flang_rt.dynamic.lib into
  #    iso_fortran_env_impl.f90.obj. Because that file is itself part of
  #    flang_rt.dynamic, this results in a recursive dependency when invoking
  #    the linker.
  #
  #  * The externally-visible functions must either be annotated with
  #    __declspec(dllexport), or listed in an exports file. A possible workaround
  #    is CMAKE_WINDOWS_EXPORT_ALL_SYMBOLS which would also export the internal
  #    C++ symbols and still requires global data symbols to be annotated
  #    manually.
  add_win_flangrt_runtime(STATIC static      MultiThreaded         INSTALL_WITH_TOOLCHAIN)
  add_win_flangrt_runtime(STATIC static_dbg  MultiThreadedDebug    INSTALL_WITH_TOOLCHAIN)
  add_win_flangrt_runtime(STATIC dynamic     MultiThreadedDLL      INSTALL_WITH_TOOLCHAIN)
  add_win_flangrt_runtime(STATIC dynamic_dbg MultiThreadedDebugDLL INSTALL_WITH_TOOLCHAIN)

  # Unittests link against LLVMSupport which is using CMake's default runtime
  # library selection, which is either MultiThreadedDLL or MultiThreadedDebugDLL
  # depending on the configuration. They have to match or linking will fail.
  if (GENERATOR_IS_MULTI_CONFIG)
    # We cannot select an ALIAS library because it may be different
    # per configuration. Fallback to CMake's default.
    add_win_flangrt_runtime(STATIC unittest "" EXCLUDE_FROM_ALL)
  else ()
    string(TOLOWER ${CMAKE_BUILD_TYPE} build_type)
    if (build_type STREQUAL "debug")
      add_library(flang_rt.runtime.unittest ALIAS flang_rt.runtime.dynamic_dbg)
    else ()
      add_library(flang_rt.runtime.unittest ALIAS flang_rt.runtime.dynamic)
    endif ()
  endif ()
endif()
