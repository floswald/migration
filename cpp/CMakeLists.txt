cmake_minimum_required (VERSION 2.8) # We are using the cmake build tool, with features from v2.8 and later
project (Migration)
include_directories(src) # This location "src" is added to the list of locations 
                         # which are searched when resolving #include statements in C/C++
add_subdirectory(src) # Look in the CMakeLists.txt file for explanation on how to build our library code
add_executable(migration migration.cpp) # The executable to make is 
									 # called "migration" and depends on the listed source files and libraries
target_link_libraries(migration migration_library) # Add our library of reaction system code to the executable

# Boilerplate to enable the Google testing framework
INCLUDE(ExternalProject)
set(EXTERNAL_ROOT ${CMAKE_BINARY_DIR}/external)
set(CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/cmake_modules)
find_package(GTest)
enable_testing()

add_subdirectory(test)