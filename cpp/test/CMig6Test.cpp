
#include "CMig6.h" // Include the code that we are testing
#include <vector>
#include <gtest/gtest.h> // Include the google test framework
#include <random/uniform.h>
#include <random/discrete-uniform.h>
#include <typeinfo>		// do some type testing



using namespace blitz;


TEST(MigrationTest, GetNameAndDimDefault){
	CMig6 myMig;
	EXPECT_EQ("CMig6_default", myMig.version());
	EXPECT_EQ(6, myMig.MaxDim());
}

TEST(MigrationTest, GetNameAndDim_2){
	CMig6 myMig(10, 3, 3, 4, 5);
	EXPECT_EQ("CMig6_dims_given", myMig.version());
	EXPECT_EQ(6, myMig.MaxDim());
}

TEST(MigrationTest, CanSetGetCtmp){
	int nA = 5;
	int nY = 2;
	int nP = 3;
	int nL = 4;
	int nT = 3;
	CMig6 myMig(nA, nY, nP, nL, nT);
	Array<double,6> myctmp(nA, nY, nP, nL, nL ,nA, FortranArray<6>());

	myctmp = 1.3;
	myMig.SetCtmp( myctmp );
	
	Array<double,6>::iterator it;
	Array<double,6> out;
	out = myMig.GetCtmp();

	// traverse out and check all equal to set value
	for (it = out.begin();
		 it != out.end();
		 it ++ ){

		EXPECT_EQ( myctmp(1,1,1,1,1,1), *it );

	}
}


TEST(MigrationTest, Blimit_ownWorks){
	
	int nA = 30;
	int nY = 4;
	int nP = 5;
	int nL = 9;
	int nT = 4;

	Array<double,2> trans(nY,nY,FortranArray<2>());
	trans = 0.9,0.3,0.1,0.7;

	Array<double,2> transP(nP,nP,FortranArray<2>());
	transP = 0.9025,0.0475,0.0025,
			 0.095,0.905,0.095,
			 0.0025,0.0475,0.9025;

	// get some data
	TinyVector<int,6> aypHTt = shape(nA,nY,nP,nL,nL,nY);

	Array<double,6> tstay(aypHTt,FortranArray<6>());	
	Array<double,6> tsell(aypHTt,FortranArray<6>());	
	Array<double,6> trent(aypHTt,FortranArray<6>());	
	Array<double,6> tbuy( aypHTt,FortranArray<6>());	

	//fill with random numbers
	tstay = 1;
	tsell = 2;
	trent = 4;
	tbuy = 5;

	ranlib::Uniform<double> uniGen;
	Array<double,6>::iterator it;

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

	// savings grid
	Array<double,1> agrid(nA,FortranArray<1>());
	agrid = 1;
	agrid(1) = -2;
	agrid(nA) = 3;

	double step_own = (agrid(nA)-agrid(1)) / nA;
	for (int i=2;i<nA+1;i++) agrid(i) = agrid(i-1) + step_own;

	Array<double,2> MoveCost(nL,nL,FortranArray<2>());
	MoveCost = 100;

	Array<double,1> Amenity(nL,FortranArray<1>());
	Amenity = 12;


	Array<int,3> blim_own(nL,nL,nP,FortranArray<3>());
	Array<int,2> blim_buy(nL,nP,FortranArray<2>());

	// create borrowing limits are random indices
	ranlib::DiscreteUniform<int> Disc( nA );
	Array<int,3>::iterator it3;
	for (it3 = blim_own.begin();
	     it3 != blim_own.end();
		 it3 ++){
		*it3 = Disc.random() + 1;
	}

	Array<int,2>::iterator it2;
	for (it2 = blim_buy.begin();
	     it2 != blim_buy.end();
		 it2 ++){
		*it2 = Disc.random() + 1;
	}

	int blim_rent = nA-1;

	int verbose = 1;
	
	PStruct pars2;
	pars2.beta = 0.9;
	pars2.myNA = -99;
	pars2.gamma   = 1.4;
	pars2.mgamma  = 1 - pars2.gamma;
	pars2.imgamma = 1/pars2.mgamma;

	CMig6 myMig_ref(nA, nY, nP, nL, nT,
			    &pars2, tstay, tsell, trent, tbuy, trans,
				transP, MoveCost, Amenity, agrid, blim_own, blim_buy,
				blim_rent, verbose);

	// TEST
	// * upon construction, ctmp has a positive value
	// * calling LimitOwn() will set all savings indices greater than blim_own(at,that,index) to pars2.myNA
	// * go through blim_own and see if that is true.
	

	// remember owner is only limited if here != there !!!!!
	myMig_ref.LimitOwner();
	Array<double,6> myctmp(nA, nY, nP, nL, nL ,nA, FortranArray<6>());
	myctmp = myMig_ref.GetCtmp() ;

	for (int ia=1;ia<nA+1; ia++){
		for (int iy=1;iy<nY+1; iy++){
			for (int ip=1;ip<nP+1; ip++){
				for (int ih=1;ih<nL+1; ih++){
					for (int it=1;it<nL+1; it++){
							
						for (int is=1;is<nA+1; is++){

							// at all savings indices greater than the blimit index, 
							// ctmp should be a positive number
							if (is > blim_own(ih,it,ip) ) {

								EXPECT_TRUE( (myctmp(ia,iy,ip,ih,it,is) > 0) ) << "FALSE at " << ia << iy << ip << ih << it << is << "ctmp is " << myctmp(ia,iy,ip,ih,it,is)  << ", blim is " << blim_own(ih,it,ip) << endl;

							// otherwise ctmp should be a positive number
							// at all savings indeces less than or equal the blimit index, (and you are moving)
							// ctmp should be myNA 
							} else if (is <= blim_own(ih,it,ip) && ih != it ){

								EXPECT_DOUBLE_EQ( pars2.myNA ,  myctmp(ia,iy,ip,ih,it,is) ) << "FALSE at " << ia << iy << ip << ih << it << is << endl;

							}
						}
					}
				}
			}
		}
	}

}


TEST(MigrationTest, Blimit_buyWorks){
	
	int nA = 2;
	int nY = 2;
	int nP = 3;
	int nL = 2;
	int nT = 3;

	Array<double,2> trans(nY,nY,FortranArray<2>());
	trans = 0.9,0.3,0.1,0.7;

	Array<double,2> transP(nP,nP,FortranArray<2>());
	transP = 0.9025,0.0475,0.0025,
			 0.095,0.905,0.095,
			 0.0025,0.0475,0.9025;

	// get some data
	TinyVector<int,6> aypHTt = shape(nA,nY,nP,nL,nL,nY);

	Array<double,6> tstay(aypHTt,FortranArray<6>());	
	Array<double,6> tsell(aypHTt,FortranArray<6>());	
	Array<double,6> trent(aypHTt,FortranArray<6>());	
	Array<double,6> tbuy( aypHTt,FortranArray<6>());	

	//fill with random numbers
	tstay = 1;
	tsell = 2;
	trent = 4;
	tbuy = 5;

	ranlib::Uniform<double> uniGen;
	Array<double,6>::iterator it;

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

	// savings grid
	Array<double,1> agrid(nA,FortranArray<1>());
	agrid = 1;
	agrid(1) = -2;
	agrid(nA) = 3;

	double step_own = (agrid(nA)-agrid(1)) / nA;
	for (int i=2;i<nA+1;i++) agrid(i) = agrid(i-1) + step_own;

	Array<double,2> MoveCost(nL,nL,FortranArray<2>());
	MoveCost = 100;

	Array<double,1> Amenity(nL,FortranArray<1>());
	Amenity = 12;


	Array<int,3> blim_own(nL,nL,nP,FortranArray<3>());
	Array<int,2> blim_buy(nL,nP,FortranArray<2>());

	// create borrowing limits are random indices
	ranlib::DiscreteUniform<int> Disc( nA );
	Array<int,3>::iterator it3;
	for (it3 = blim_own.begin();
	     it3 != blim_own.end();
		 it3 ++){
		*it3 = Disc.random() + 1;
	}

	Array<int,2>::iterator it2;
	for (it2 = blim_buy.begin();
	     it2 != blim_buy.end();
		 it2 ++){
		*it2 = Disc.random() + 1;
	}

	int blim_rent = nA-1;

	int verbose = 1;
	
	PStruct pars2;
	pars2.beta = 0.9;
	pars2.myNA = -99;
	pars2.gamma   = 1.4;
	pars2.mgamma  = 1 - pars2.gamma;
	pars2.imgamma = 1/pars2.mgamma;

	CMig6 myMig_ref(nA, nY, nP, nL, nT,
			    &pars2, tstay, tsell, trent, tbuy, trans,
				transP, MoveCost, Amenity, agrid, blim_own, blim_buy,
				blim_rent, verbose);

	// TEST
	// * upon construction, ctmp has a positive value
	// * calling LimitOwn() will set all savings indices greater than blim_own(at,that,index) to pars2.myNA
	// * go through blim_own and see if that is true.
	

	// remember owner is only limited if here != there !!!!!
	myMig_ref.LimitBuyer();
	Array<double,6> myctmp(nA, nY, nP, nL, nL ,nA, FortranArray<6>());
	myctmp = myMig_ref.GetCtmp() ;

	for (int ia=1;ia<nA+1; ia++){
		for (int iy=1;iy<nY+1; iy++){
			for (int ip=1;ip<nP+1; ip++){
				for (int ih=1;ih<nL+1; ih++){
					for (int it=1;it<nL+1; it++){
							
						for (int is=1;is<nA+1; is++){

							// at all savings indices greater than the blimit index, 
							// ctmp should be a positive number
							if (is > blim_buy(it,ip) ) {

								EXPECT_TRUE( (myctmp(ia,iy,ip,ih,it,is) > 0) ) << "FALSE at " << ia << iy << ip << ih << it << is << "ctmp is " << myctmp(ia,iy,ip,ih,it,is)  << ", blim is " << blim_buy(it,ip) << endl;

							// otherwise ctmp should be a positive number
							// at all savings indeces less than or equal the blimit index	
							// ctmp should be myNA 
							} else if (is <= blim_buy(it,ip)){

								EXPECT_DOUBLE_EQ( pars2.myNA ,  myctmp(ia,iy,ip,ih,it,is) ) << "FALSE at " << ia << iy << ip << ih << it << is << endl;

							}
						}
					}
				}
			}
		}
	}

}









TEST(MigrationTest, CanGetNameAndDimReferenced){

	int nA = 5;
	int nY = 2;
	int nP = 3;
	int nL = 4;
	int nT = 3;

	Array<double,2> trans(nY,nY,FortranArray<2>());
	trans = 0.9,0.3,0.1,0.7;

	Array<double,2> transP(nP,nP,FortranArray<2>());
	transP = 0.9025,0.0475,0.0025,
			 0.095,0.905,0.095,
			 0.0025,0.0475,0.9025;

	// get some data
	TinyVector<int,6> aypHTt = shape(nA,nY,nP,nL,nL,nY);

	Array<double,6> tstay(aypHTt,FortranArray<6>());	
	Array<double,6> tsell(aypHTt,FortranArray<6>());	
	Array<double,6> trent(aypHTt,FortranArray<6>());	
	Array<double,6> tbuy( aypHTt,FortranArray<6>());	

	//fill with random numbers
	ranlib::Uniform<double> uniGen;
	Array<double,6>::iterator it;

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

	// savings grid
	Array<double,1> agrid(nA,FortranArray<1>());
	agrid(1) = -2;
	agrid(nA) = 3;

	double step_own = (agrid(nA)-agrid(1)) / nA;
	for (int i=2;i<nA+1;i++) agrid(i) = agrid(i-1) + step_own;

	Array<double,2> MoveCost(nL,nL,FortranArray<2>());
	MoveCost = 100;

	Array<double,1> Amenity(nL,FortranArray<1>());
	Amenity = 12;

	Array<int,3> blim_own(nL,nL,nP,FortranArray<3>());
	Array<int,2> blim_buy(nL,nP,FortranArray<2>());
	int blim_rent = nA-1;
	blim_own = 3;
	blim_buy = 4;

	int verbose = 1;
	
	PStruct pars2;
	pars2.beta = 0.9;
	pars2.myNA = -99;
	pars2.gamma   = 1.4;
	pars2.mgamma  = 1 - pars2.gamma;
	pars2.imgamma = 1/pars2.mgamma;

	CMig6 myMig_ref(nA, nY, nP, nL, nT,
			    &pars2, tstay, tsell, trent, tbuy, trans,
				transP, MoveCost, Amenity, agrid, blim_own, blim_buy,
				blim_rent, verbose);

	EXPECT_EQ("CMig6_data_referenced", myMig_ref.version() );
	EXPECT_EQ( tstay.dimensions(), myMig_ref.MaxDim() );

}


// test whether integration function is correct
// NOTE: this test only checks whether integration 
// give a one array if given a one array. it cannot
// check the ordering of the tensor operation.
TEST(Mig6Test, checkIntegration) {

	int nA = 5;
	int nY = 2;
	int nP = 3;
	int nL = 4;
	int nT = 3;

	// create an instance of owner class
	CMig6 myMig(nA, nY, nP, nL, nT);

	// get data
	Array<double,4> test(nA,nY,nP,nL,FortranArray<4>());
	Array<double,4> bout(nA,nY,nP,nL,FortranArray<4>());
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



// check whether discrete choice function 
// works correctly
TEST(Mig6Test,TestDchoice5d){

	CMig6 myMig;

	// setup test for dchoice3d
	Array<double,5> one(2,2,2,2,2,FortranArray<5>());
	Array<double,5> two(2,2,2,2,2,FortranArray<5>());
	Array<double,5> max12(2,2,2,2,2,FortranArray<5>());
	Array<double,5> ret(2,2,2,2,2,FortranArray<5>());

	one = 1;
	two = 0,0,2,0,2,0,2,2,
		  0,0,2,0,2,0,2,2,
		  0,0,2,0,2,0,2,2,
          0,0,2,0,2,0,2,2;

	max12 = where(one > two, one, two );

	ret = myMig.dchoice5d(one,two);
	// define two std::vectors to take the values
	//std::vector<double> vec;
	Array<double,5>::iterator it1;
	Array<double,5>::iterator it2;
	double idx;

	// iterate over blitz and fill vector with values
	for (it1 = ret.begin(), it2 = max12.begin() ; it1!=ret.end() && it2!=max12.end() ; ++it1, ++it2){

		EXPECT_EQ( *it2, *it1 );
	}

}






//// Test whether can GET various objects
//TEST(Mig6Test, TestGetters) {

	//CMig6 myMig;
	//Array<double,7> t_ResStay(2,2,2,2,2,2,2,FortranArray<7>());
	//Array<double,7> t_ResSell(2,2,2,2,2,2,2,FortranArray<7>());
	//Array<double,7> t_ResRent(2,2,2,2,2,2,2,FortranArray<7>());
	//Array<double,7> t_ResBuy (2,2,2,2,2,2,2,FortranArray<7>());


	//Array<double,6> c_loc_stay(2,2,2,2,2,2,FortranArray<6>());	
	//Array<double,6> c_loc_sell(2,2,2,2,2,2,FortranArray<6>());  
	//Array<double,6> c_loc_rent(2,2,2,2,2,2,FortranArray<6>());	
	//Array<double,6> c_loc_buy ( 2,2,2,2,2,2,FortranArray<6>());   

	//Array<int   ,5> Down(  2,2,2,2,2,FortranArray<5>());  
	//Array<int   ,5> Drent( 2,2,2,2,2,FortranArray<5>());

	//EXPECT_EQ( typeid( t_ResStay ), typeid( myMig.GetResStay() ) );
	//EXPECT_EQ( typeid( t_ResSell ), typeid( myMig.GetResSell() ) );
	//EXPECT_EQ( typeid( t_ResRent ), typeid( myMig.GetResRent() ) );
	//EXPECT_EQ( typeid( t_ResBuy  ), typeid( myMig.GetResBuy () ) );
	
	//EXPECT_EQ( typeid( c_loc_stay ), typeid( myMig.Getc_loc_stay() ) );
	//EXPECT_EQ( typeid( c_loc_sell ), typeid( myMig.Getc_loc_sell() ) );
	//EXPECT_EQ( typeid( c_loc_rent ), typeid( myMig.Getc_loc_rent() ) );
	//EXPECT_EQ( typeid( c_loc_buy  ), typeid( myMig.Getc_loc_buy () ) );

	//// ordering() returns a TinyVector<int,7>. ckeck if last elt is equal
	//EXPECT_EQ( t_ResStay.ordering()(6),  myMig.GetResStay().ordering()(6) );
	//EXPECT_EQ( t_ResSell.ordering()(6) , myMig.GetResSell().ordering()(6) );
	//EXPECT_EQ( t_ResRent.ordering()(6) , myMig.GetResRent().ordering()(6) );
	//EXPECT_EQ(  t_ResBuy.ordering()(6) , myMig.GetResBuy ().ordering()(6) );

//}	





//class MigrationFTest: public ::testing::Test {

//protected:
	//int nA = 2;
	//int nY = 2;
	//int nP = 3;
	//int nL = 2;
	//int nT = 3;

	//Array<double,2> trans(nY,nY,FortranArray<2>());
	//trans = 0.9,0.3,0.1,0.7;

	//Array<double,2> transP(nP,nP,FortranArray<2>());
	//transP = 0.9025,0.0475,0.0025,
			 //0.095,0.905,0.095,
			 //0.0025,0.0475,0.9025;

	//// get some data
	//TinyVector<int,6> aypHTt = shape(nA,nY,nP,nL,nL,nY);

	//Array<double,6> tstay(aypHTt,FortranArray<6>());	
	//Array<double,6> tsell(aypHTt,FortranArray<6>());	
	//Array<double,6> trent(aypHTt,FortranArray<6>());	
	//Array<double,6> tbuy( aypHTt,FortranArray<6>());	

	////fill with random numbers
	//tstay = 1;
	//tsell = 2;
	//trent = 4;
	//tbuy = 5;

	//ranlib::Uniform<double> uniGen;
	//Array<double,6>::iterator it;

	//for (it = tstay.begin(); it!=tstay.end(); it++) {
		//*it = uniGen.random() + 1;
	//}
	//for (it = tsell.begin(); it!=tsell.end(); it++) {
		//*it = uniGen.random() + 1;
	//}
	//for (it = trent.begin(); it!=trent.end(); it++) {
		//*it = uniGen.random() + 1;
	//}
	//for (it = tbuy.begin(); it!=tbuy.end(); it++) {
		//*it = uniGen.random() + 1;
	//}

	//// savings grid
	//Array<double,1> agrid(nA,FortranArray<1>());
	//agrid = 1;
	//agrid(1) = -2;
	//agrid(nA) = 3;

	//double step_own = (agrid(nA)-agrid(1)) / nA;
	//for (int i=2;i<nA+1;i++) agrid(i) = agrid(i-1) + step_own;

	//Array<double,2> MoveCost(nL,nL,FortranArray<2>());
	//MoveCost = 100;

	//Array<double,1> Amenity(nL,FortranArray<1>());
	//Amenity = 12;


	//Array<int,3> blim_own(nL,nL,nP,FortranArray<3>());
	//Array<int,2> blim_buy(nL,nP,FortranArray<2>());

	//// create borrowing limits are random indices
	//ranlib::DiscreteUniform<int> Disc( nA );
	//Array<int,3>::iterator it3;
	//for (it3 = blim_own.begin();
		 //it3 != blim_own.end();
		 //it3 ++){
		//*it3 = Disc.random() + 1;
	//}

	//Array<int,2>::iterator it2;
	//for (it2 = blim_buy.begin();
		 //it2 != blim_buy.end();
		 //it2 ++){
		//*it2 = Disc.random() + 1;
	//}

	//int blim_rent = nA-1;

	//int verbose = 1;
	
	//PStruct pars2;
	//pars2.beta = 0.9;
	//pars2.myNA = -99;
	//pars2.gamma   = 1.4;
	//pars2.mgamma  = 1 - pars2.gamma;
	//pars2.imgamma = 1/pars2.mgamma;

	//CMig6 myMig_ref(nA, nY, nP, nL, nT,
				//&pars2, tstay, tsell, trent, tbuy, trans,
				//transP, MoveCost, Amenity, agrid, blim_own, blim_buy,
				//blim_rent, verbose);


	//MigrationTest() {
	//}
/*};*/


int main(int argc, char **argv) { 
  ::testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}
