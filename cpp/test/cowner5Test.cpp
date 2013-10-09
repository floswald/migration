
#include "cowner.h" // Include the code that we are testing
#include <vector>
#include <gtest/gtest.h> // Include the google test framework

using namespace blitz;



TEST(cowner5Test, ResPointersCorrect) {

	double data2[] = {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,
	                 17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32};
	double data3[] = {17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,
	                 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16};

	double fake[] = {2};

	TinyVector<int,5> dims3 = shape(2,2,2,2,2);

	// create an instance of owner class
	COwner5 t5(dims3,data2,data3);
	
	EXPECT_EQ(data2,t5.getResStay().data());	// expect that data fed to class is ok
	EXPECT_EQ(data3,t5.getResSell().data());	// expect that data fed to class is ok
    EXPECT_NE(fake,t5.getResSell().data());		// expect not equal

}

TEST(cowner5Test, ResAreFortranArrays) {
	double data2[] = {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,
	                 17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32};
	double data3[] = {17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,
	                 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16};
	TinyVector<int,5> dims3 = shape(2,2,2,2,2);

	// create an instance of owner class
	COwner5 t5(dims3,data2,data3);

	// fortranArray has blitz::ordering() vector 0,1,...,blitz::rank()-1
	// check last entry of that with
	int storage[] = {0,1,2,3,4};	// fortran order
	//int storage[] = {2,1,0};	// C order

	TinyVector<int,5> stay = t5.getResStay().ordering();
	TinyVector<int,5> sell = t5.getResSell().ordering();

	for (int i=0;i<3;i++) {
		EXPECT_EQ(storage[i],stay(i)) << "stay has wrong ordering at dim " << i;
		EXPECT_EQ(storage[i],sell(i)) << "sell has wrong ordering at dim " << i;
	}
	
}

TEST(cowner5Test, ResContents) {
	double data2[] = {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,
	                 17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32};
	double data3[] = {17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,
	                 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16};
	TinyVector<int,5> dims3 = shape(2,2,2,2,2);

	// create an instance of owner class
	COwner5 t5(dims3,data2,data3);

	// get data
	Array<double,5> dStay = t5.getResStay();
	Array<double,5> dSell = t5.getResSell();

	// define an iterator for a blitz array
	Array<double,5>::iterator iter;

	// define two std::vectors to take the values
	std::vector<double> stayvec;
	std::vector<double> sellvec;

	// iterate over blitz and fill vector with values
	for (iter = dStay.begin() ; iter!=dStay.end();++iter){
		stayvec.push_back(*iter);
	}

	for (iter = dSell.begin() ; iter!=dSell.end();++iter){
		sellvec.push_back(*iter);
	}


	for (int i=0; i<sellvec.size(); i++){
		EXPECT_EQ(data2[i],stayvec.at(i)) << "ResStay has wrong value at idx " << i;
		EXPECT_EQ(data3[i],sellvec.at(i)) << "ResSell has wrong value at idx " << i;
	}
}





int main(int argc, char **argv) { // A main function scaffold to call the tests
  ::testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}
