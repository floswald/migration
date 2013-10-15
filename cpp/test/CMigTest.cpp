
#include "CMig.h" // Include the code that we are testing
#include <vector>
#include <gtest/gtest.h> // Include the google test framework

using namespace blitz;


class MigrationTest: public ::testing::Test {
protected:
	Array<double,5> tstay,tsell,trent,tbuy;	
	Array<double,2> trans;
	Parstruc pars;
	TinyVector<int,5> aypta;
	TinyVector<int,4> aypt ;
	TinyVector<int,4> aypa ;
	TinyVector<int,4> aypy ;
	TinyVector<int,3> ayp  ;
	TinyVector<int,2> y    ;

	CMig myMig;

	MigrationTest() {
		pars.beta = 0.9;
		pars.myNA = -99;

		Array<double,2> trans(shape(2,2),FortranArray<2>());
		trans = 0.9,0.3,0.1,0.7;

		// create dimension vectors	                 
		aypta = shape(2,2,2,2,2);
		aypt  = shape(2,2,2,2);
		aypa  = shape(2,2,2,2);
		aypy  = shape(2,2,2,2);
		ayp   = shape(2,2,2);
		y     = shape(2,2);
		
		Array<double,5> tstay(2,2,2,2,2,FortranArray<5>());	
		Array<double,5> tsell(2,2,2,2,2,FortranArray<5>());	
		Array<double,5> trent(2,2,2,2,2,FortranArray<5>());	
		Array<double,5> tbuy( 2,2,2,2,2,FortranArray<5>());	
		tstay = 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,
			   17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32;
		tsell = 17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,
						 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16;
		trent = 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,
				17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32;
		tbuy  = 17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,
						 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16;

	}
};


// test whether the default constructor allocates
// a default array for DimAYP = {2,2,2}
TEST_F(MigrationTest, DefaultDimAYP){

	std::vector<int> vec;
	vec.push_back(2);
	vec.push_back(2);
	vec.push_back(2);

	TinyVector<int,3> blitz;
	blitz = myMig.GetDimAYP();
	TinyVector<int,3>::iterator iter;

	for (iter = blitz.begin(); iter!= blitz.end(); iter ++){

		EXPECT_EQ(vec.at(iter-blitz.begin()), *iter);
	}
}

// test whether pointers point to correct data in memory
TEST_F(MigrationTest, TestDataPointers){
	
	// set some data
	myMig.ReferenceStay( tstay );
	myMig.ReferenceSell( tsell );
	myMig.ReferenceRent( trent );
	myMig.ReferenceBuy(  tbuy );
	myMig.ReferenceG( trans );
	myMig.SetP( &pars );

	// expect same data address
	EXPECT_EQ( tstay.data() , myMig.GetResStay().data() ) << "ResStay pointer has wrong value " << endl;
	EXPECT_EQ( tsell.data() , myMig.GetResSell().data() ) << "ResSell pointer has wrong value " << endl ;
	EXPECT_EQ( trent.data() , myMig.GetResRent().data() ) << "ResRent poiter has wrong value "  << endl;
	EXPECT_EQ( tbuy.data()  , myMig.GetResBuy().data() )  << "ResBuy pointer has wrong value "  << endl;
	EXPECT_EQ( trans.data()  , myMig.GetG().data() )  << "G pointer has wrong value "  << endl;
}

// test whether allocate data contains same values 
// as data to be allocated
TEST_F(MigrationTest, CheckDataContents){
	
		Array<double,5> tstay(2,2,2,2,2,FortranArray<5>());	
		Array<double,5> tsell(2,2,2,2,2,FortranArray<5>());	
		Array<double,5> trent(2,2,2,2,2,FortranArray<5>());	
		Array<double,5> tbuy( 2,2,2,2,2,FortranArray<5>());	
		tstay = 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,
			   17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32;
		tsell = 17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,
						 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16;
		trent = 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,
				17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32;
		tbuy  = 17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,
						 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16;
	// set some data
	myMig.ReferenceStay( tstay );
	myMig.ReferenceSell( tsell );
	myMig.ReferenceRent( trent );
	myMig.ReferenceBuy(  tbuy );

	// now check it's the right data
	std::vector<double> stayvec;
	std::vector<double> sellvec;
	std::vector<double> rentvec;
	std::vector<double> buyvec;
	std::vector<double> stayblitz;
	std::vector<double> sellblitz;
	std::vector<double> rentblitz;
	std::vector<double> buyblitz;

	Array<double,5>::iterator iter;
	for (iter = tstay.begin() ; iter!=tstay.end();++iter){
		stayvec.push_back(*iter);
	}
	for (iter = tsell.begin() ; iter!=tsell.end();++iter){
		sellvec.push_back(*iter);
	}
	for (iter = trent.begin() ; iter!=trent.end();++iter){
		rentvec.push_back(*iter);
	}
	for (iter = tbuy.begin() ; iter!=tbuy.end();++iter){
		buyvec.push_back(*iter);
	}
 
	stayblitz = myMig.GetResStayNumeric();
	sellblitz = myMig.GetResSellNumeric();
	rentblitz = myMig.GetResRentNumeric();
	buyblitz = myMig.GetResBuyNumeric();

	for (int i=0; i<sellvec.size(); i++){
		EXPECT_EQ(stayvec.at(i),stayblitz.at(i)) << "ResStay has wrong value at idx " << i;
		EXPECT_EQ(sellvec.at(i),sellblitz.at(i)) << "ResSell has wrong value at idx " << i;
		EXPECT_EQ(rentvec.at(i),rentblitz.at(i)) << "ResRent has wrong value at idx " << i;
		EXPECT_EQ(buyvec.at(i),buyblitz.at(i))   << "ResBuy has wrong value at idx "  << i;
	}
}



// test whether referenced data is indeed 
// stored in FortranArray mode
TEST_F(MigrationTest, ResArraysAreFortranOrder) {
	
	Array<double,5> tstay(2,2,2,2,2,FortranArray<5>());	
	Array<double,5> tsell(2,2,2,2,2,FortranArray<5>());	
	Array<double,5> trent(2,2,2,2,2,FortranArray<5>());	
	Array<double,5> tbuy( 2,2,2,2,2,FortranArray<5>());	
	tstay = 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,
		   17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32;
	tsell = 17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,
					 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16;
	trent = 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,
			17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32;
	tbuy  = 17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,
					 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16;

	// set some data
	myMig.ReferenceStay( tstay );
	myMig.ReferenceSell( tsell );
	myMig.ReferenceRent( trent );
	myMig.ReferenceBuy(  tbuy );
	 
	// fortranArray has blitz::ordering() vector 0,1,...,blitz::rank()-1
	 // check last entry of that with
	 TinyVector<int,5> fortran;
	 fortran = 0,1,2,3,4;
	 //int storage[] = {2,1,0};	// C order

	 TinyVector<int,5> stay = myMig.GetResStayOrder();
	 TinyVector<int,5> sell = myMig.GetResSellOrder();
	 TinyVector<int,5> rent = myMig.GetResRentOrder();
	 TinyVector<int,5> buy  = myMig.GetResBuyOrder();

	 for (int i=0;i<5;i++) {
		 EXPECT_EQ(fortran(i),stay(i)) << "stay has wrong ordering at dim " << i;
		 EXPECT_EQ(fortran(i),sell(i)) << "sell has wrong ordering at dim " << i;
		 EXPECT_EQ(fortran(i),rent(i)) << "rent has wrong ordering at dim " << i;
		 EXPECT_EQ(fortran(i),buy(i)) << "buy has wrong ordering at dim " << i;
	 }
}



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
