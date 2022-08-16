cmake_minimum_required(VERSION 3.14)

project(conbor VERSION "1.0.0" DESCRIPTION "C++ SQLite3 wrapper" LANGUAGES CXX)

include(CMakePackageConfigHelpers)
include(GNUInstallDirs)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

add_library(
    conbor
    INTERFACE
    )

include(FetchContent)
FetchContent_Declare(
  googletest
  GIT_REPOSITORY https://github.com/google/googletest.git
  GIT_TAG release-1.12.1
)
# For Windows: Prevent overriding the parent project's compiler/linker settings
set(gtest_force_shared_crt ON CACHE BOOL "" FORCE)
FetchContent_MakeAvailable(googletest)

set_property(TARGET conbor PROPERTY POSITION_INDEPENDENT_CODE TRUE)

set(CMAKE_THREAD_PREFER_PTHREAD TRUE)
set(THREADS_PREFER_PTHREAD_FLAG TRUE)

install(FILES
    conbor/cbor.hxx
    DESTINATION "${CMAKE_INSTALL_INCLUDEDIR}/conbor"
    )

install(TARGETS conbor EXPORT conborTargets
    ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
    INCLUDES DESTINATION ${CMAKE_INSTALL_INCLUDEDIR})

install(EXPORT conborTargets NAMESPACE conbor:: DESTINATION "${CMAKE_INSTALL_LIBDIR}/cmake/conbor")



configure_package_config_file("conborConfig.cmake.in"
    "${CMAKE_CURRENT_BINARY_DIR}/conborConfig.cmake"
    INSTALL_DESTINATION "${CMAKE_INSTALL_LIBDIR}/cmake/conbor"
    )

write_basic_package_version_file(
    "${CMAKE_CURRENT_BINARY_DIR}/conborConfigVersion.cmake"
    VERSION ${PACKAGE_VERSION}
    COMPATIBILITY SameMajorVersion
    )

install(FILES ${CMAKE_CURRENT_BINARY_DIR}/conborConfig.cmake
              ${CMAKE_CURRENT_BINARY_DIR}/conborConfigVersion.cmake
        DESTINATION "${CMAKE_INSTALL_LIBDIR}/cmake/conbor"
        )

enable_testing()

add_executable(
  conbor_test
  conbor_test.cxx
)
target_link_libraries(
  conbor_test
  GTest::gtest_main
  conbor
)

include(GoogleTest)
gtest_discover_tests(conbor_test)
