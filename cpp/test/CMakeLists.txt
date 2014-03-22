
# note: if in the root CMakeLists.txt you enable the UCL google test setting,
# you can use the syntax as below:

#cxx_test(BKTest BKTest.cpp bankruptcy_library ${LIBS}) #specify a new test, and which files it depends on
#cxx_test(TTest TTest.cpp bankruptcy_library ${LIBS}) #specify a new test, and which files it depends on

# downside is that you have to svn update in each compilation run. unbearable.

# here's how you manually add a test.

set(TNAMES "SimTest" "SolTest" "SolInterpTest" "ComputeOwnTest" "DimTest" "ComputeFunsOwnBKTest" "ComputeRenterTest" "RunAllPeriods_SLOW")

foreach(TNAME ${TNAMES})

	add_executable(test_${TNAME} ${TNAME}.cpp)
	target_link_libraries(test_${TNAME} /usr/local/lib/libgtest.a bankruptcy_library ${LIBS})

	add_dependencies(test_${TNAME} googletest)

	add_test( cxx_${TNAME} ${EXECUTABLE_OUTPUT_PATH}/test_${TNAME} --gtest_output=xml:${CMAKE_BINARY_DIR}/test-results/test_${TNAME}.xml)
	set_tests_properties( cxx_${TNAME} PROPERTIES LABELS "gtest")

endforeach()


