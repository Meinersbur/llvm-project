

function(ensure_compile_test_target testdir testtargetname abstestpath)
  message("testdir ${testdir}")
  message("testtargetname ${testtargetname}")

  if (TARGET "${testtargetname}")
    # target already created, anly append to list of tests
    message("Adding test to ${testtargetname}: ${abstestpath}")
    target_sources("${testtargetname}" PUBLIC "${testfile}")
    return ()
  endif ()

  # The following is inspired by add_unittest

  if( NOT LLVM_BUILD_TESTS )
    set(EXCLUDE_FROM_ALL ON)
  endif()

  # Our current version of gtest does not properly recognize C++11 support
  # with MSVC, so it falls back to tr1 / experimental classes.  Since LLVM
  # itself requires C++11, we can safely force it on unconditionally so that
  # we don't have to fight with the buggy gtest check.
  add_definitions(-DGTEST_LANG_CXX11=1)
  add_definitions(-DGTEST_HAS_TR1_TUPLE=0)

  include_directories(${LLVM_MAIN_SRC_DIR}/utils/unittest/googletest/include)
  include_directories(${LLVM_MAIN_SRC_DIR}/utils/unittest/googlemock/include)
  if (NOT LLVM_ENABLE_THREADS)
    list(APPEND LLVM_COMPILE_DEFINITIONS GTEST_HAS_PTHREAD=0)
  endif ()

  if (SUPPORTS_VARIADIC_MACROS_FLAG)
    list(APPEND LLVM_COMPILE_FLAGS "-Wno-variadic-macros")
  endif ()
  # Some parts of gtest rely on this GNU extension, don't warn on it.
  if(SUPPORTS_GNU_ZERO_VARIADIC_MACRO_ARGUMENTS_FLAG)
    list(APPEND LLVM_COMPILE_FLAGS "-Wno-gnu-zero-variadic-macro-arguments")
  endif()

  set(LLVM_REQUIRES_RTTI OFF)
  list(APPEND LLVM_LINK_COMPONENTS Support) # gtest needs it for raw_ostream

  message("Creating compiled test ${testtargetname} using ${abstestpath}")
  add_llvm_executable("${testtargetname}" "${abstestpath}" NO_INSTALL_RPATH)
  
  set(outdir "${LLVM_COMPILED_TEST_BINARY_DIR}")
  message("outdir: ${LLVM_COMPILED_TEST_BINARY_DIR}")

  set_output_directory(${testtargetname} BINARY_DIR ${outdir} LIBRARY_DIR ${outdir})
  # libpthreads overrides some standard library symbols, so main
  # executable must be linked with it in order to provide consistent
  # API for all shared libaries loaded by this executable.
  target_link_libraries(${testtargetname} PRIVATE compiledtestboilerplate gtest_main gtest ${LLVM_PTHREAD_LIB})

  add_dependencies(llvm-compiled-tests ${testtargetname})
  set_property(TARGET ${testtargetname} PROPERTY FOLDER "compiled tests")
endfunction ()





function(add_compiled_test testfile abstestpath)
  get_filename_component(testdir "${testfile}" DIRECTORY)

  set(testtargetname "${testdir}")
  string(REPLACE "/" "_" testtargetname "${testdir}")
  string(REPLACE "\\" "_" testtargetname "${testtargetname}")

  ensure_compile_test_target("${testdir}" "${testtargetname}CompiledTests" "${abstestpath}")
endfunction()







function(find_compiled_tests testsrcdir)
  cmake_parse_arguments(ARG
    ""
    "GLOBBING"
    ""
    ${ARGN})
  message("testdir: ${testsrcdir}")
  message("GLOBBING ${ARG_GLOBBING}")

  if (ARG_GLOBBING)
    message("Globbing at compile-time")
    file(GLOB_RECURSE compiled_tests FOLLOW_SYMLINKS RELATIVE "${testsrcdir}" CONFIGURE_DEPENDS "${testsrcdir}/*.cxx")
  else ()
    message("Globbing at configure-time")
    file(GLOB_RECURSE compiled_tests FOLLOW_SYMLINKS RELATIVE "${testsrcdir}" "${testsrcdir}/*.cxx")
  endif ()

  
  message("COMPILED TESTS FOUND: ${compiled_tests}")

  # Group by folder
  foreach(compiled_test IN LISTS compiled_tests)
    message("TEST: ${compiled_test}")

    set(abstestpath "${testsrcdir}/${compiled_test}")

    add_compiled_test("${compiled_test}" "${abstestpath}")
  endforeach()
endfunction()






if( CMAKE_SOURCE_DIR STREQUAL CMAKE_CURRENT_SOURCE_DIR )
  # Compile regression tests as a separate build
  message("using externally compiled regression tests")

  project(llvm-regression CXX)
  cmake_minimum_required(VERSION 3.4.3)

  set(llvm_cmake_config_dir ${LLVM_BINARY_DIR})
  message("LLVM config expected at ${llvm_cmake_config_dir}")
  find_package(LLVM REQUIRED CONFIG 
    PATHS "${llvm_cmake_config_dir}"
    NO_DEFAULT_PATH
  )

  message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
  message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")
  list(APPEND CMAKE_MODULE_PATH "${LLVM_CMAKE_DIR}")
  include(AddLLVM)

  add_custom_target(llvm-compiled-tests)

  find_compiled_tests("${CMAKE_CURRENT_SOURCE_DIR}/.." GLOBBING ON)
else ()
  # Compile regression tests in the same build
  message("integrated regression test compilation")
  option(LLVM_TEST_GLOBBING "Research for new regression test before each compilation" ON)

  add_custom_target(llvm-compiled-tests)

  set(LLVM_COMPILED_TEST_BINARY_DIR "${LLVM_BINARY_DIR}/compiledtests/${CMAKE_CFG_INTDIR}")
  find_compiled_tests("${CMAKE_CURRENT_SOURCE_DIR}/.." GLOBBING ${LLVM_TEST_GLOBBING})
endif ()



