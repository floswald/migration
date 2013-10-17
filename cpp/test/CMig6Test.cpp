
#include "CMig6.h" // Include the code that we are testing
#include <vector>
#include <gtest/gtest.h> // Include the google test framework
#include <random/uniform.h>



using namespace blitz;


// test can construct the default object
TEST(Mig6Test, TestDefaultConstructor) {

	// create an instance of owner class
	CMig6 myMig;

	// is a CMig6 object?
	EXPECT_EQ("CMig6", myMig.version());

	// dimension of biggest array is ... ?
	EXPECT_EQ(7 , myMig.MaxDim() );
}


// test can construct the object with reference to data
TEST(Mig6Test, TestReferenceConstructor) {

	// dim vectors
    TinyVector<int,7> dim_ayp_here_there_ta;
	TinyVector<int,6> dim_ayp_here_there_t; 
	TinyVector<int,6> dim_ayp_here_there_a; 
	TinyVector<int,5> dim_ayp_here_there; 
	TinyVector<int,5> dim_ayp_here_t; 
	TinyVector<int,4> dim_ayp_here;      
	TinyVector<int,4> D_aypy; 
	TinyVector<int,3> D_ayp; 
	TinyVector<int,2> D_y; 

    Array<double,2> trans(shape(2,2),FortranArray<2>());
	trans = 0.9,0.3,0.1,0.7;

	Parstruc pars;
	pars.beta = 0.9;
	pars.myNA = -99;
	Parstruc* pp;
	pp = &pars;

    dim_ayp_here_there_ta = 2,2,2,2,2,2,2;
	dim_ayp_here_there_t  = 2,2,2,2,2,2;
	dim_ayp_here_there_a  = 2,2,2,2,2,2; 
	dim_ayp_here_there    = 2,2,2,2,2;
	dim_ayp_here_t        = 2,2,2,2,2;
	dim_ayp_here          = 2,2,2,2;   
	D_aypy                = 2,2,2,2;
	D_ayp                 = 2,2,2;
	D_y                   = 2,2;


	// get some data
	double dstay[128], dsell[128], drent[128], dbuy[128];
	// that's the C++11 way
	//std::default_random_engine generator;
	//std::uniform_real_distribution<double> distribution(1,128);

	//double x;

	//for (int i=0;i<128;i++){
		//x = distribution( generator );
		//dstay[i] = x;
		//x = distribution( generator );
		//dsell[i] = x;
		//x = distribution( generator );
		//drent[i] = x;
		//x = distribution( generator );
		//dbuy[i] = x;
	//}
	


	for (int i=0;i<128;i++){
		dstay[i] = rand(); 
		dsell[i] = rand();
		drent[i] = rand();
		dbuy[i] = rand();
	}

	//pointers
	


	Array<double,7> tstay(2,2,2,2,2,2,2,neverDeleteData,FortranArray<7>());	
	Array<double,7> tsell(2,2,2,2,2,2,2,neverDeleteData,FortranArray<7>());	
	Array<double,7> trent(2,2,2,2,2,2,2,neverDeleteData,FortranArray<7>());	
	Array<double,7> tbuy( 2,2,2,2,2,2,2,neverDeleteData,FortranArray<7>());	

	//fill with random numbers
	ranlib::Uniform<double> uniGen;
	Array<double,7>::iterator it;

	for (it = tstay.begin(); it!=tstay.end(); it++) {
		*it = uniGen.random() + 1;
	}
	for (it = tsell.begin(); it!=tsell.end(); it++) {
		*it = uniGen.random() + 1;
	}
	for (it = trent.begin(); it!=trent.end(); it++) {
		*it = uniGen.random() + 1;
	}
	for (it = tbuy.begin(); it!=tbuy.end(); it++) {
		*it = uniGen.random() + 1;
	}
	


	// create an instance of owner class
	CMig6 myMig(dim_ayp_here_there_ta,          
			    dim_ayp_here_there_t,
				dim_ayp_here_there_a, 
				dim_ayp_here_t, 
				dim_ayp_here,      
				D_aypy, 
				D_ayp, 
				D_y, 
				pp,
				tstay,tsell,trent,tbuy,trans);

	// is a CMig6 object?
	EXPECT_EQ("CMig6", myMig.version());

	// dimension of biggest array is ... ?
	EXPECT_EQ(7 , myMig.MaxDim() );
}



int main(int argc, char **argv) { // A main function scaffold to call the tests
  ::testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}
