
#include "cowner.h" // Include the code that we are testing
#include <gtest/gtest.h> // Include the google test framework

using namespace blitz;


TEST(cowner4Test, VmaxDims) {
	double data[] = {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16};
	TinyVector<int,4> dims = shape(2,2,2,2);
	TinyVector<int,3> dims2 = shape(2,2,2);
	COwner4 t(dims,dims2,data);

	// check wether allocated res object has 3 dimensions
	EXPECT_EQ(3,t.getVmax().dimensions());	
}


TEST(cowner4Test, ResDims) {
	double data[] = {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16};
	TinyVector<int,4> dims = shape(2,2,2,2);
	TinyVector<int,3> dims2 = shape(2,2,2);
	COwner4 t(dims,dims2,data);

	// check wether allocated res object has 4 dimensions
	EXPECT_EQ(4,t.getRes().dimensions());	// expect that blitz array "res" has 4 dims
}

TEST(cowner4Test, resHasCorrectData) {
	double data[] = {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16};
	TinyVector<int,4> dims = shape(2,2,2,2);
	TinyVector<int,3> dims2 = shape(2,2,2);
	COwner4 t(dims,dims2,data);
	std::cout << "in my test" << std::endl;

	// check wether allocated res object has 4 dimensions
	EXPECT_EQ(data,t.getRes().data());	// expect that data fed to class is ok
}

TEST(cowner4Test, resIsFortranArray) {
	double data[] = {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16};
	TinyVector<int,4> dims = shape(2,2,2,2);
	TinyVector<int,3> dims2 = shape(2,2,2);
	COwner4 t(dims,dims2,data);

	// fortranArray has blitz::ordering() vector 0,1,...,blitz::rank()-1
	// check last entry of that with
	int storage[] = {0,1,2,3};	// fortran order
	//int storage[] = {3,2,1,0};	// C order

	TinyVector<int,4> blitzStorage = t.getRes().ordering();

	for (int i=0;i<4;i++) {
		EXPECT_EQ(storage[i],blitzStorage(i)) << "Vectors x and y differ at index " << i;
	}
	
}

TEST(cowner4Test, VmaxIsFortranArray) {
	double data[] = {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16};
	TinyVector<int,4> dims = shape(2,2,2,2);
	TinyVector<int,3> dims2 = shape(2,2,2);
	COwner4 t(dims,dims2,data);

	// fortranArray has blitz::ordering() vector 0,1,...,blitz::rank()-1
	// check last entry of that with
	int storage[] = {0,1,2};	// fortran order
	//int storage[] = {2,1,0};	// C order

	TinyVector<int,3> blitzStorage = t.getVmax().ordering();

	for (int i=0;i<3;i++) {
		EXPECT_EQ(storage[i],blitzStorage(i)) << "Vectors x and y differ at index " << i;
	}
	
}


int main(int argc, char **argv) { // A main function scaffold to call the tests
  ::testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}
