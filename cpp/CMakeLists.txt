cmake_minimum_required (VERSION 2.8) # We are using the cmake build tool, with features from v2.8 and later
project (Migration)
include_directories(src) # This location "src" is added to the list of locations 
                         # which are searched when resolving #include statements in C/C++

# this includes the Rcpp library ?
#include_directories(/Library/Frameworks/R.framework/Versions/3.0/Resources/library/Rcpp/include)
#link_directories(/Library/Frameworks/R.framework/Versions/3.0/Resources/library/Rcpp/lib)
#include_directories(/usr/local/include)

## and this the standard R header files
#include_directories(/Library/Frameworks/R.framework/Resources/include)
#link_directories(/Library/Frameworks/R.framework/Resources/lib)

set(CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/cmake_modules)

# add boost
#find_package(Boost 1.53.0 )
#if(Boost_FOUND)
	#include_directories(${Boost_INCLUDE_DIRS})
#endif()

# add GSL
find_package(GSL REQUIRED)
include_directories(${GSL_INCLUDE_DIRS} ${GSLCBLAS_INCLUDE_DIRS})
set(LIBS ${LIBS} ${GSL_LIBRARIES} ${GSLCBLAS_LIBRARIES})

# add my cpp source files
add_subdirectory(src) 
add_executable(migration migration.cpp) 
#add_executable(migration2 migration2.cpp) 
#add_executable(migration3 migration3.cpp) 
									
target_link_libraries(migration migration_library ${LIBS}) # Add our library of reaction system code to the executable
#target_link_libraries(migration2 migration_library ${LIBS}) # Add our library of reaction system code to the executable
#target_link_libraries(migration3 migration_library ${LIBS}) # Add our library of reaction system code to the executable
#link_libraries( ${LIBS} )

# Boilerplate to enable the Google testing framework
INCLUDE(ExternalProject)
set(EXTERNAL_ROOT ${CMAKE_BINARY_DIR}/external)
find_package(GTest)
enable_testing()

add_subdirectory(test)
