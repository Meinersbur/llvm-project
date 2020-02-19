
file(GLOB_RECURSE compiled_tests FOLLOW_SYMLINKS RELATIVE "${CMAKE_CURRENT_SOURCE_DIR}" CONFIGURE_DEPENDS  "${CMAKE_CURRENT_SOURCE_DIR}/*.cxx")
message("COMPILED TESTS FOUND: ${compiled_tests}")
foreach(compiled_test IN LISTS compiled_tests)
  message("TEST: ${compiled_test}")
  add_compiled_test(${compiled_test})
endforeach()