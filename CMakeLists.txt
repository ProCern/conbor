cmake_minimum_required(VERSION 3.14)

project(conbor VERSION "1.0.0" DESCRIPTION "C++ SQLite3 wrapper" LANGUAGES CXX)

include(CMakePackageConfigHelpers)
include(GNUInstallDirs)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

add_library(
    conbor
    conbor/value.cxx
    )

set_property(TARGET conbor PROPERTY POSITION_INDEPENDENT_CODE TRUE)

set(CMAKE_THREAD_PREFER_PTHREAD TRUE)
set(THREADS_PREFER_PTHREAD_FLAG TRUE)

find_package(SQLite3 REQUIRED)
find_package(Threads REQUIRED)

target_compile_options(conbor PRIVATE -Wall -Wextra -Werror)
target_compile_definitions(conbor PRIVATE -D_POSIX_C_SOURCE=200809L -D_XOPEN_SOURCE=700 -D_XOPEN_SOURCE_EXTENDED=1 -D_ALL_SOURCE)

target_link_libraries(conbor PUBLIC SQLite::SQLite3 Threads::Threads)

install(FILES
    conbor/value.hxx
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

