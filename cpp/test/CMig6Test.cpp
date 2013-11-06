
#include "CMig6.h" // Include the code that we are testing
#include <vector>
#include <gtest/gtest.h> // Include the google test framework
#include <random/uniform.h>
#include <typeinfo>		// do some type testing



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
	TinyVector<int,5> dim_ayp_here_y; 
	TinyVector<int,4> dim_ayp_here;      
	TinyVector<int,3> D_ayp; 
	TinyVector<int,2> D_y; 

    Array<double,2> trans(shape(2,2),FortranArray<2>());
	trans = 0.9,0.3,0.1,0.7;
	Array<double,2> transP(shape(2,2),FortranArray<2>());
	transP = 0.8,0.4,0.2,0.6;

    Array<double,2> MoveCost(shape(2,2),FortranArray<2>());
	MoveCost = 0,1,1,0;

    Array<double,1> Amenity(2,FortranArray<1>());
	Amenity = 1,2;

	PStruct pars;
	pars.beta = 0.9;
	pars.myNA = -99;
	PStruct* pp;
	pp = &pars;

    dim_ayp_here_there_ta = 2,2,2,2,2,2,2;
	dim_ayp_here_there_t  = 2,2,2,2,2,2;
	dim_ayp_here_there_a  = 2,2,2,2,2,2; 
	dim_ayp_here_there    = 2,2,2,2,2;
	dim_ayp_here_t        = 2,2,2,2,2;
	dim_ayp_here_y        = 2,2,2,2,2;
	dim_ayp_here          = 2,2,2,2;   
	D_ayp                 = 2,2,2;
	D_y                   = 2,2;


	// get some data

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
	
	int verbose = 1;


	// create an instance of owner class
	CMig6 myMig(dim_ayp_here_there_ta,          
			    dim_ayp_here_there_t,
				dim_ayp_here_there_a, 
				dim_ayp_here_there, 
				dim_ayp_here_t, 
				dim_ayp_here_y, 
				dim_ayp_here,      
				D_ayp, 
				D_y, 
				pp,
				tstay,tsell,trent,tbuy,trans,transP,MoveCost,Amenity,verbose);

	// is a CMig6 object?
	EXPECT_EQ("CMig6", myMig.version());

	// dimension of biggest array is ... ?
	EXPECT_EQ(7 , myMig.MaxDim() );
}

// Test whether can GET various objects
TEST(Mig6Test, TestGetters) {

	CMig6 myMig;
	Array<double,7> t_ResStay(2,2,2,2,2,2,2,FortranArray<7>());
	Array<double,7> t_ResSell(2,2,2,2,2,2,2,FortranArray<7>());
	Array<double,7> t_ResRent(2,2,2,2,2,2,2,FortranArray<7>());
	Array<double,7> t_ResBuy (2,2,2,2,2,2,2,FortranArray<7>());


	Array<double,6> c_loc_stay(2,2,2,2,2,2,FortranArray<6>());	
	Array<double,6> c_loc_sell(2,2,2,2,2,2,FortranArray<6>());  
	Array<double,6> c_loc_rent(2,2,2,2,2,2,FortranArray<6>());	
	Array<double,6> c_loc_buy ( 2,2,2,2,2,2,FortranArray<6>());   

	Array<int   ,5> Down(  2,2,2,2,2,FortranArray<5>());  
	Array<int   ,5> Drent( 2,2,2,2,2,FortranArray<5>());

	EXPECT_EQ( typeid( t_ResStay ), typeid( myMig.GetResStay() ) );
	EXPECT_EQ( typeid( t_ResSell ), typeid( myMig.GetResSell() ) );
	EXPECT_EQ( typeid( t_ResRent ), typeid( myMig.GetResRent() ) );
	EXPECT_EQ( typeid( t_ResBuy  ), typeid( myMig.GetResBuy () ) );
	
	EXPECT_EQ( typeid( c_loc_stay ), typeid( myMig.Getc_loc_stay() ) );
	EXPECT_EQ( typeid( c_loc_sell ), typeid( myMig.Getc_loc_sell() ) );
	EXPECT_EQ( typeid( c_loc_rent ), typeid( myMig.Getc_loc_rent() ) );
	EXPECT_EQ( typeid( c_loc_buy  ), typeid( myMig.Getc_loc_buy () ) );

	// ordering() returns a TinyVector<int,7>. ckeck if last elt is equal
	EXPECT_EQ( t_ResStay.ordering()(6),  myMig.GetResStay().ordering()(6) );
	EXPECT_EQ( t_ResSell.ordering()(6) , myMig.GetResSell().ordering()(6) );
	EXPECT_EQ( t_ResRent.ordering()(6) , myMig.GetResRent().ordering()(6) );
	EXPECT_EQ(  t_ResBuy.ordering()(6) , myMig.GetResBuy ().ordering()(6) );

}	


// test computePeriod
TEST(Mig6Test, TestComputePeriod) {

	// dim vectors
    TinyVector<int,7> dim_ayp_here_there_ta;
	TinyVector<int,6> dim_ayp_here_there_t; 
	TinyVector<int,6> dim_ayp_here_there_a; 
	TinyVector<int,5> dim_ayp_here_there; 
	TinyVector<int,5> dim_ayp_here_t; 
	TinyVector<int,5> dim_ayp_here_y; 
	TinyVector<int,4> dim_ayp_here;      
	TinyVector<int,3> D_ayp; 
	TinyVector<int,2> D_y; 

    Array<double,2> trans(shape(2,2),FortranArray<2>());
	trans = 0.9,0.3,0.1,0.7;
	Array<double,2> transP(shape(2,2),FortranArray<2>());
	transP = 0.8,0.4,0.2,0.6;

    Array<double,2> MoveCost(shape(2,2),FortranArray<2>());
	MoveCost = 0,1,1,0;

    Array<double,1> Amenity(2,FortranArray<1>());
	Amenity = 1,2;

	PStruct pars;
	pars.beta = 0.9;
	pars.myNA = -99;
	PStruct* pp;
	pp = &pars;

    dim_ayp_here_there_ta = 2,2,2,2,2,2,2;
	dim_ayp_here_there_t  = 2,2,2,2,2,2;
	dim_ayp_here_there_a  = 2,2,2,2,2,2; 
	dim_ayp_here_there    = 2,2,2,2,2;
	dim_ayp_here_t        = 2,2,2,2,2;
	dim_ayp_here_y        = 2,2,2,2,2;
	dim_ayp_here          = 2,2,2,2;   
	D_ayp                 = 2,2,2;
	D_y                   = 2,2;


	// get some data

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
	

	int verbose = 1;

	// create an instance of owner class
	CMig6 myMig(dim_ayp_here_there_ta,          
			    dim_ayp_here_there_t,
				dim_ayp_here_there_a, 
				dim_ayp_here_there, 
				dim_ayp_here_t, 
				dim_ayp_here_y, 
				dim_ayp_here,      
				D_ayp, 
				D_y, 
				pp,
				tstay,tsell,trent,tbuy,trans,transP,MoveCost,Amenity,verbose);

	// is a CMig6 object?
	EXPECT_EQ("CMig6", myMig.version());

	// dimension of biggest array is ... ?
	EXPECT_EQ(7 , myMig.MaxDim() );
}


// test whether integration function is correct
TEST(Mig6Test, checkIntegration) {

	// create an instance of owner class
	CMig6 myMig;

	// get data
	Array<double,4> test(myMig.GetDim_ayp_here(),FortranArray<4>());
	Array<double,4> bout(myMig.GetDim_ayp_here(),FortranArray<4>());
	test = 1;
	bout = myMig.integrate(test);

	std::vector<double> out;
	// define an iterator for a blitz array
	Array<double,4>::iterator iter;

	// iterate over blitz and fill vector with values
	for (iter = bout.begin() ; iter!=bout.end();++iter){
		out.push_back(*iter);
	}

	for (int i=0; i<out.size(); i++){
		EXPECT_DOUBLE_EQ(1,out.at(i)) << "integration not 1 at index " << i;
	}
}




/*// check whether discrete choice function */
//// works correctly
//TEST(Mig6Test,TestDchoice3d){

	//CMig myMig;

	//// setup test for dchoice3d
	//Array<double,3> one(myMig.GetDimAYP(),FortranArray<3>());
	//Array<double,3> two(myMig.GetDimAYP(),FortranArray<3>());
	//Array<double,3> ret(myMig.GetDimAYP(),FortranArray<3>());
	//one = 1;
	//two = 0,0,2,0,2,0,2,2;

	//std::vector<double> dchoice;
	//std::vector<double> vec;
	//dchoice.push_back(1);
	//dchoice.push_back(1);
	//dchoice.push_back(2);
	//dchoice.push_back(1);
	//dchoice.push_back(2);
	//dchoice.push_back(1);
	//dchoice.push_back(2);
	//dchoice.push_back(2);

	//ret = myMig.dchoice3d(one,two);
	//// define two std::vectors to take the values
	////std::vector<double> vec;
	//Array<double,3>::iterator iter;

	//// iterate over blitz and fill vector with values
	//for (iter = ret.begin() ; iter!=ret.end();++iter){
		//vec.push_back(*iter);
	//}
	//for (int i=0; i<dchoice.size(); i++){
		//EXPECT_EQ(dchoice.at(i),vec.at(i)) << "at" << i << "dchoice is "<<dchoice.at(i)<<"and blitzchoice is " << vec.at(i);
	//}

	
/*}*/


int main(int argc, char **argv) { 
  ::testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}
