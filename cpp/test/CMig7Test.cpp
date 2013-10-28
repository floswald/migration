
#include "CMig7.h" // Include the code that we are testing
#include <vector>
#include <gtest/gtest.h> // Include the google test framework
#include <random/uniform.h>
#include <typeinfo>		// do some type testing



using namespace blitz;


// test can construct the default object
TEST(Mig7Test, TestDefaultConstructor) {

	// create an instance of owner class
	CMig7 myMig(1);

	// is a CMig6 object?
	EXPECT_EQ("CMig7", myMig.version());

	//// dimension of biggest array is ... ?
	EXPECT_EQ(3 , myMig.MaxDim() );
}




int main(int argc, char **argv) { 
  ::testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}
