cmake_minimum_required (VERSION 2.8.4) # We are using the cmake build tool, with features from v2.8 and later
project (migration)
include_directories(src) # This location "src" is added to the list of locations 
                         # which are searched when resolving #include statements in C/C++

# this includes the Rcpp library ?
#include_directories(/Library/Frameworks/R.framework/Versions/3.0/Resources/library/Rcpp/include)
#include_directories(/Library/Frameworks/R.framework/Versions/3.0/Resources/include)
#link_directories(/Library/Frameworks/R.framework/Versions/3.0/Resources/library/Rcpp/lib)
#link_directories(/Library/Frameworks/R.framework/Resources/lib)

set(CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/cmake_modules)

# enable c++11
#set(CMAKE_CXX_FLAGS "-std=c++11 -stdlib=libc++")


# add boost
#find_package(Boost 1.53.0 )
#if(Boost_FOUND)
	#include_directories(${Boost_INCLUDE_DIRS})
#endif()

# add GSL
#find_package(GSL REQUIRED)
#include_directories(${GSL_INCLUDE_DIRS} ${GSLCBLAS_INCLUDE_DIRS})

# add interp2d
#include_directories(${GSL_INCLUDE_DIRS} ${GSLCBLAS_INCLUDE_DIRS} /usr/local/include)
include_directories(/usr/local/include)

# add armadillo
include_directories(/usr/local/include/armadillo)

# put all third party libraries in list "LIBS"
#set(LIBS ${GSL_LIBRARIES} ${GSLCBLAS_LIBRARIES} interp2d gtest armadillo)
set(LIBS gtest armadillo)


# add my cpp source files
add_subdirectory(src) 
add_executable(simulation sim.cpp) 

#add_executable(simulation sim.cpp) 
#add_executable(migration3 migration3.cpp) 
									
target_link_libraries(simulation BBL_library ${LIBS} ) # Add all libraries to the executable
#target_link_libraries(simulation bankruptcy_library ${LIBS} ) # Add all libraries to the executable


# Boilerplate to enable the Google testing framework
# uncomment if want to use UCL auto generation of tests.
#INCLUDE(ExternalProject)
#set(EXTERNAL_ROOT ${CMAKE_BINARY_DIR}/external)
#find_package(GTest)
enable_testing()

add_subdirectory(test)
