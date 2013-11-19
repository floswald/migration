

#include "CMig_dev1.h" // Include the code that we are testing
#include <vector>
#include <gtest/gtest.h> // Include the google test framework

using namespace blitz;


// test whether integration function is correct
TEST(MigTestIndivid, checkIntegration) {

	// create an instance of owner class
	CMig myMig;

	// get data
	Array<double,3> test(myMig.GetDimAYP(),FortranArray<3>());
	Array<double,3> bout(myMig.GetDimAYP(),FortranArray<3>());
	test = 1;
	bout = myMig.integrate(test);

	std::vector<double> out;
	// define an iterator for a blitz array
	Array<double,3>::iterator iter;

	// iterate over blitz and fill vector with values
	for (iter = bout.begin() ; iter!=bout.end();++iter){
		out.push_back(*iter);
	}

	for (int i=0; i<out.size(); i++){
		EXPECT_EQ(1,out.at(i)) << "integration not 1 at index " << i;
	}
}


// check whether discrete choice function 
// works correctly
TEST(MigTestIndivid,TestDchoice3d){

	CMig myMig;

	// setup test for dchoice3d
	Array<double,3> one(myMig.GetDimAYP(),FortranArray<3>());
	Array<double,3> two(myMig.GetDimAYP(),FortranArray<3>());
	Array<double,3> ret(myMig.GetDimAYP(),FortranArray<3>());
	one = 1;
	two = 0,0,2,0,2,0,2,2;

	std::vector<double> dchoice;
	std::vector<double> vec;
	dchoice.push_back(1);
	dchoice.push_back(1);
	dchoice.push_back(2);
	dchoice.push_back(1);
	dchoice.push_back(2);
	dchoice.push_back(1);
	dchoice.push_back(2);
	dchoice.push_back(2);

	ret = myMig.dchoice3d(one,two);
	// define two std::vectors to take the values
	//std::vector<double> vec;
	Array<double,3>::iterator iter;

	// iterate over blitz and fill vector with values
	for (iter = ret.begin() ; iter!=ret.end();++iter){
		vec.push_back(*iter);
	}
	for (int i=0; i<dchoice.size(); i++){
		EXPECT_EQ(dchoice.at(i),vec.at(i)) << "at" << i << "dchoice is "<<dchoice.at(i)<<"and blitzchoice is " << vec.at(i);
	}

	
}


TEST(MigTestIndivid, TensorAddition ) {

	Array<double,3> A(shape(2,2,2),FortranArray<3>());
	A = 0.25; 

	Array<double,2> B(shape(2,2),FortranArray<2>());
	B = 0,0,1,1; 

	firstIndex i;
	secondIndex j;
	thirdIndex k;

	Array<double,3> C(A(i,j,k) + B(k,j));
	Array<double,2> c;
	Array<double,2>::iterator it;
	
	c = C(Range::all(),1,Range::all());

	for (it = c.begin(); it != c.end(); it++){
		EXPECT_EQ(0.25, *it);
	}
	c = C(Range::all(),2,Range::all());

	for (it = c.begin(); it != c.end(); it++){
		EXPECT_EQ(1.25, *it);
	}
}


// test whether tensor arithmetic in integration
// is correct
TEST(MigTestIndivid, CheckIntegration){
     firstIndex i;
     secondIndex j;
     thirdIndex k;
     fourthIndex l;

     Array<double,3> A(2,2,2,FortranArray<3>());
     Array<double,2> G(2,2,FortranArray<2>());
     Array<double,3> R(2,2,2,FortranArray<3>());

     A=1;
     G = 0.3,0.4,0.7,0.6;
     Array<double,4> C(2,2,2,2,FortranArray<4>());

     C = A(i,j,k) * G(l,j);   // C(i,j,k,l)

     R = sum(C(i,l,k,j),l);
	
	 Array<double,3>::iterator iter;
	 for (iter = R.begin(); iter != R.end(); iter++){

		 EXPECT_EQ( 1.0, *iter ) << "integrates not to one";

	 }
}	


int main(int argc, char **argv) { // A main function scaffold to call the tests
  ::testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}
