llvm_canonicalize_cmake_booleans(
  BUILD_SHARED_LIBS
  HAVE_LIBXAR
  HAVE_LIBZ
  HAVE_OCAMLOPT
  HAVE_OCAML_OUNIT
  LLVM_ENABLE_DIA_SDK
  LLVM_ENABLE_FFI
  LLVM_ENABLE_THREADS
  LLVM_INCLUDE_GO_TESTS
  LLVM_LIBXML2_ENABLED
  LLVM_LINK_LLVM_DYLIB
  LLVM_TOOL_LTO_BUILD
  LLVM_USE_INTEL_JITEVENTS
  LLVM_BUILD_EXAMPLES
  LLVM_ENABLE_PLUGINS
  )

configure_lit_site_cfg(
  ${CMAKE_CURRENT_SOURCE_DIR}/lit.site.cfg.py.in
  ${CMAKE_CURRENT_BINARY_DIR}/lit.site.cfg.py
  MAIN_CONFIG
  ${CMAKE_CURRENT_SOURCE_DIR}/lit.cfg.py
  )
configure_lit_site_cfg(
  ${CMAKE_CURRENT_SOURCE_DIR}/Unit/lit.site.cfg.py.in
  ${CMAKE_CURRENT_BINARY_DIR}/Unit/lit.site.cfg.py
  MAIN_CONFIG
  ${CMAKE_CURRENT_SOURCE_DIR}/Unit/lit.cfg.py
  )

# Don't include check-llvm into check-all without LLVM_BUILD_TOOLS.
if(NOT LLVM_BUILD_TOOLS)
  set(EXCLUDE_FROM_ALL ON)
endif()

# Set the depends list as a variable so that it can grow conditionally.
# NOTE: Sync the substitutions in test/lit.cfg when adding to this list.
set(LLVM_TEST_DEPENDS
          BugpointPasses
          FileCheck
          LLVMHello
          UnitTests
          bugpoint
          count
          llc
          lli
          lli-child-target
          llvm-addr2line
          llvm-ar
          llvm-as
          llvm-bcanalyzer
          llvm-c-test
          llvm-cat
          llvm-cfi-verify
          llvm-config
          llvm-cov
          llvm-cvtres
          llvm-cxxdump
          llvm-cxxfilt
          llvm-cxxmap
          llvm-diff
          llvm-dis
          llvm-dlltool
          dsymutil
          llvm-dwarfdump
          llvm-dwp
          llvm-elfabi
          llvm-exegesis
          llvm-extract
          llvm-isel-fuzzer
          llvm-jitlink
          llvm-lib
          llvm-link
          llvm-lipo
          llvm-lto2
          llvm-mc
          llvm-mca
          llvm-modextract
          llvm-mt
          llvm-nm
          llvm-objcopy
          llvm-objdump
          llvm-opt-fuzzer
          llvm-opt-report
          llvm-pdbutil
          llvm-profdata
          llvm-ranlib
          llvm-rc
          llvm-readobj
          llvm-readelf
          llvm-rtdyld
          llvm-size
          llvm-split
          llvm-strings
          llvm-strip
          llvm-symbolizer
          llvm-tblgen
          llvm-undname
          llvm-xray
          not
          obj2yaml
          opt
          sancov
          sanstats
          verify-uselistorder
          yaml-bench
          yaml2obj
          regression
        )

if(TARGET llvm-lto)
  set(LLVM_TEST_DEPENDS ${LLVM_TEST_DEPENDS} llvm-lto)
endif()

# If Intel JIT events are supported, depend on a tool that tests the listener.
if( LLVM_USE_INTEL_JITEVENTS )
  set(LLVM_TEST_DEPENDS ${LLVM_TEST_DEPENDS} llvm-jitlistener)
endif( LLVM_USE_INTEL_JITEVENTS )

if(TARGET LLVMgold)
  set(LLVM_TEST_DEPENDS ${LLVM_TEST_DEPENDS} LLVMgold)
endif()

if(TARGET llvm-go)
  set(LLVM_TEST_DEPENDS ${LLVM_TEST_DEPENDS} llvm-go)
endif()

if(TARGET LTO)
  set(LLVM_TEST_DEPENDS ${LLVM_TEST_DEPENDS} LTO)
endif()

if(LLVM_BUILD_EXAMPLES)
  list(APPEND LLVM_TEST_DEPENDS
    Kaleidoscope-Ch3
    Kaleidoscope-Ch4
    Kaleidoscope-Ch5
    Kaleidoscope-Ch6
    Kaleidoscope-Ch7
    )
endif()

if(TARGET ocaml_llvm)
  # Clear all non-OCaml cross-target dependencies when building out-of-tree.
  if(LLVM_OCAML_OUT_OF_TREE)
    set(LLVM_TEST_DEPENDS)
  endif()

  set(LLVM_TEST_DEPENDS ${LLVM_TEST_DEPENDS}
          ocaml_llvm
          ocaml_llvm_all_backends
          ocaml_llvm_analysis
          ocaml_llvm_bitreader
          ocaml_llvm_bitwriter
          ocaml_llvm_executionengine
          ocaml_llvm_irreader
          ocaml_llvm_linker
          ocaml_llvm_target
          ocaml_llvm_ipo
          ocaml_llvm_passmgr_builder
          ocaml_llvm_scalar_opts
          ocaml_llvm_transform_utils
          ocaml_llvm_vectorize
        )
endif()

# copied from add_unittest
function(add_compiled_test sourcefile)
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

file(GLOB_RECURSE compiled_tests FOLLOW_SYMLINKS RELATIVE "${CMAKE_CURRENT_SOURCE_DIR}" CONFIGURE_DEPENDS  "${CMAKE_CURRENT_SOURCE_DIR}/*.cxx")
message("COMPILED TESTS FOUND: ${compiled_tests}")
foreach(compiled_test IN LISTS compiled_tests)
  message("TEST: ${compiled_test}")
  add_compiled_test(${compiled_test})
endforeach()






add_custom_target(llvm-test-depends DEPENDS ${LLVM_TEST_DEPENDS})
set_target_properties(llvm-test-depends PROPERTIES FOLDER "Tests")

add_lit_testsuite(check-llvm "Running the LLVM regression tests"
  ${CMAKE_CURRENT_BINARY_DIR}
  DEPENDS ${LLVM_TEST_DEPENDS}
  )
set_target_properties(check-llvm PROPERTIES FOLDER "Tests")

add_lit_testsuites(LLVM ${CMAKE_CURRENT_SOURCE_DIR}
  DEPENDS ${LLVM_TEST_DEPENDS}
  )

# Setup a legacy alias for 'check-llvm'. This will likely change to be an
# alias for 'check-all' at some point in the future.
add_custom_target(check)
add_dependencies(check check-llvm)
set_target_properties(check PROPERTIES FOLDER "Tests")



