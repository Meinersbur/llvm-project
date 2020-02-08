

# copied from add_unittest
function(add_compiled_test testfile)
  

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

  set(test_name "${sourcefile}")
  string(REPLACE  "/" "_" test_name "${test_name}")
  string(REPLACE  "\\" "_" test_name "${test_name}")
  message("test_name: ${test_name} sourcefile: ${sourcefile}")
  add_llvm_executable(${test_name} ${sourcefile}  NO_INSTALL_RPATH DEPENDS regression)
  target_link_libraries(${test_name} PUBLIC regression)
  # IGNORE_EXTERNALIZE_DEBUGINFO

  # TODO: put into appropriate subdir
  set(outdir ${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_CFG_INTDIR})
  message("outdir: ${outdir}")
  set_output_directory(${test_name} BINARY_DIR ${outdir} LIBRARY_DIR ${outdir})
  # libpthreads overrides some standard library symbols, so main
  # executable must be linked with it in order to provide consistent
  # API for all shared libaries loaded by this executable.
  target_link_libraries(${test_name} PRIVATE gtest_main gtest ${LLVM_PTHREAD_LIB})

  set(LLVM_TEST_DEPENDS "${LLVM_TEST_DEPENDS};${test_name}" PARENT_SCOPE)
  message("depends ${LLVM_TEST_DEPENDS}")
  #list(APPEND LLVM_TEST_DEPENDS ${test_name} )
  #add_dependencies(llvm-test-depends ${test_name})
  #get_target_property(test_suite_folder ${test_suite} FOLDER)
  #if (NOT ${test_suite_folder} STREQUAL "NOTFOUND")
  #  set_property(TARGET ${test_name} PROPERTY FOLDER "${test_suite_folder}")
  #endif ()
endfunction()





file(GLOB_RECURSE compiled_tests FOLLOW_SYMLINKS RELATIVE "${CMAKE_CURRENT_SOURCE_DIR}" CONFIGURE_DEPENDS "${CMAKE_CURRENT_SOURCE_DIR}/*.cpp")
message("COMPILED TESTS FOUND: ${compiled_tests}")
foreach(compiled_test IN LISTS compiled_tests)
  message("TEST: ${compiled_test}")
  add_compiled_test(${compiled_test})
endforeach()
