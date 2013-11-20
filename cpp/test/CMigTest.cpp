
#include "CMig.h" // Include the code that we are testing
#include <vector>
#include <gtest/gtest.h> // Include the google test framework
#include <random/uniform.h>
#include <random/discrete-uniform.h>
#include <typeinfo>		// do some type testing



using namespace blitz;


TEST(MigrationTest, GetNameAndDimDefault){
	CMig myMig;
	EXPECT_EQ("CMig_default", myMig.version());
	EXPECT_EQ(7, myMig.MaxDim());
}

TEST(MigrationTest, GetNameAndDim_2){
	CMig myMig(10, 3, 3, 2, 4, 5);
	EXPECT_EQ("CMig_dims_given", myMig.version());
	EXPECT_EQ(7, myMig.MaxDim());
}


// this tests the price grid interpolator
// defines a function f(x) = 3*x and 
// interpolates off the xgrid
TEST(MigrationTest, Test1DInterpolator) {

	int nA = 8;
	int nY = 2;
	int nP = 5;
	int nL = 5;
	int nZ = 3;
	int nT = 3;

	CMig myMig(nA,nY,nP,nZ,nL,nT);

	Array<double,1> x(nP);
	x = 1,2,3,4,5;
	Array<double,1> y(nP);
    y	= 3*x;

	Array<double,1> newx(nP+1,FortranArray<1>());
	newx = 1.1,2.5,3,4.5,4.6,5;
	Array<double,1> ret(nP+1,FortranArray<1>());

    ret = myMig.interp1D( x.data(), y.data(), newx );

	for (unsigned int i=1; i<newx.size() + 1; i++){
		EXPECT_DOUBLE_EQ( ret(i) ,3*newx(i));
	}
}


// Test ComputeExpectation function
// return Vbar_own and Vbar_rent and check if 
// they where altered in the expected way
TEST(MigrationTest, VbarOwnIsChangedCorrectly){
	int nA = 8;
	int nY = 2;
	int nP = 5;
	int nL = 5;
	int nZ = 2;
	int nT = 3;

	CMig myMig(nA,nY,nP,nZ,nL,nT);

	// from this constructor, Vbar_own
	// is zero.
	// calling this should change it:
	
	myMig.ComputeExpectations( nT );
	Array<double,5> res(nA,nY,nP,nZ,nL,FortranArray<5>());

	res = myMig.GetVbarOwn(nT);

	double truth = log(nL) + euler_mascheroni;

	Array<double,5>::iterator it;
	for (it =res.begin();
		 it!=res.end();
		 ++it ){
		EXPECT_DOUBLE_EQ( truth, *it );
	}
}



// Test Test version of interpolate function. is given a 
// 5D array and interpolates on some price grids
TEST(MigrationTest, CheckTestInterpolate){

	int nA = 8;
	int nY = 2;
	int nP = 5;
	int nL = 5;
	int nZ = 2;
	int nT = 3;

	CMig myMig(nA,nY,nP,nZ,nL,nT);
	// create a tensor for vbar
	Array<double,5> vbar(nA,nY,nP,nZ,nL,FortranArray<5>());

	// create evaluation grid and integration nodes
	Array<double,1> pval(nP,FortranArray<1>()); 
	Array<double,2> pint(nP,nZ,FortranArray<2>());

	pval = 1,2,3,4,5;
	pint = 1,1.9,2.9,3.9,4.9,
	       1.1,2.1,3.1,4.1,5;

	// fill the Grid arrays with those values
	Array<double,3> Geval(nL,nT,nP,FortranArray<3>());
	Array<double,4> GInt(nL,nZ,nT,nP,FortranArray<4>());

	for (int iL=1; iL<nL+1 ; iL++){
		for (int iT=1; iT<nT+1; iT++){
			Geval(iL,iT,Range::all()) = pval;
			for (int iZ=1; iZ<nZ+1; iZ++){
				GInt(iL,iZ,iT,Range::all()) = pint(Range::all(),iZ);
			}
		}
	}

	// fill vbar with increasing values in p-dimension
	// the function to interpolate is vbar.
	// it is such that f(ia,iy,x,here,Z) = x
	// i.e. for any indices (ia,iy,here,Z), the function value is x
	// in other words a straight line with slope 1.
	thirdIndex ip;
	vbar = pval(ip);

	Array<double,5> ret(nA,nY,nP,nZ,nL,FortranArray<5>());
    ret = myMig.TestInterpolate( vbar, Geval, GInt, 1 );
	
	
	for (int ia=1; ia<nA+1 ; ia++){
	for (int iy=1; iy<nY+1 ; iy++){
	for (int ip=1; ip<nP+1 ; ip++){
	for (int iL=1; iL<nL+1 ; iL++){
		for (int iZ=1; iZ<nZ+1; iZ++){
			// check if vbar(ia,iy,ip,iL,iZ) == pint(ip,iZ)
			EXPECT_DOUBLE_EQ( pint(ip,iZ), ret(ia,iy,ip,iZ,iL)  );	// expected, actual
		}
	}
	}
	}
	}
}

// Test interpolate function. is given a 
// 5D array and interpolates on some price grids
// needs referenced constructor
TEST(MigrationTest, CheckInterpolate){


	int nA = 8;
	int nY = 2;
	int nP = 5;
	int nL = 5;
	int nZ = 2;
	int nT = 3;

	Array<double,2> trans(nY,nY,FortranArray<2>());
	trans = 0.9,0.3,0.1,0.7;

	Array<double,2> transZ(nZ,nZ,FortranArray<2>());
	//transZ = 0.9025,0.0475,0.0025,
			 //0.095,0.905,0.095,
			 //0.0025,0.0475,0.9025;
	transZ = 1;

	// get some data
	TinyVector<int,7> aypZHTt = shape(nA,nY,nP,nZ,nL,nL,nT);

	Array<double,7> tstay(aypZHTt,FortranArray<7>());	
	Array<double,7> tsell(aypZHTt,FortranArray<7>());	
	Array<double,7> trent(aypZHTt,FortranArray<7>());	
	Array<double,7> tbuy( aypZHTt,FortranArray<7>());	


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
	blim_buy = nA-1;


	// create borrowing limits are random indices
	ranlib::DiscreteUniform<int> Disc( nA );
	Array<int,3>::iterator it3;
	for (it3 = blim_own.begin();
		 it3 != blim_own.end();
		 it3 ++){
		*it3 = Disc.random() + 1;
	}
	int verbose = 1;
	
	PStruct pars2;
	pars2.beta = 0.9;
	pars2.myNA = -99;
	pars2.gamma   = 1.4;
	pars2.mgamma  = 1 - pars2.gamma;
	pars2.imgamma = 1/pars2.mgamma;
	pars2.R       = 1/(1+0.04);
	pars2.type   = gsl_interp_linear;	// change interpolation type here.
	pars2.acc    = gsl_interp_accel_alloc ();
	pars2.spline = gsl_spline_alloc (pars2.type, nP );

	// make savings tensors
	Array<double,4> save_own(nL,nL,nP,nA,FortranArray<4>());
	for (int here=1; here<nL+1; here++){
	for (int there=1;there<nL+1;there++){
	for (int pr=1;   pr<nP+1;   pr++){
		save_own(here,there,pr,Range::all() ) = agrid;
		if (blim_own(here,there,pr)>0) save_own(here,there,pr,Range(fromStart,blim_own(here,there,pr)) ) = (-1) * pars2.myNA;
	}}}

	Array<double,3> save_buy(nL,nP,nA,FortranArray<3>());
	save_buy=2;

	Array<double,1> save_rent(agrid);

	// Constructor needs GpEval and GpInt!
	// fill with ascending numbers
	// create evaluation grid and integration nodes
	Array<double,1> pval(nP,FortranArray<1>()); 
	Array<double,2> pint(nP,nZ,FortranArray<2>());

	pval = 1,2,3,4,5;
	pint = 1,1.9,2.9,3.9,4.9,
	       1.1,2.1,3.1,4.1,5;

	// fill the Grid arrays with those values
	Array<double,3> GpEval(nL,nT,nP,FortranArray<3>());
	Array<double,4> GpInt(nL,nZ,nT,nP,FortranArray<4>());

	for (int iL=1; iL<nL+1 ; iL++){
		for (int iT=1; iT<nT+1; iT++){
			GpEval(iL,iT,Range::all()) = pval;
			for (int iZ=1; iZ<nZ+1; iZ++){
				GpInt(iL,iZ,iT,Range::all()) = pint(Range::all(),iZ);
			}
		}
	}


	CMig myMig_ref(nA,nY,nP,nZ,nL,nT,
				&pars2, tstay, tsell, trent, tbuy, trans,
				transZ, MoveCost, Amenity, save_own, save_buy, save_rent, GpEval, GpInt, verbose);
		
	Array<double,5> vbar(nA,nY,nP,nZ,nL,FortranArray<5>());
	thirdIndex ip;
	vbar = pval(ip);
	Array<double,5> ret(nA,nY,nP,nZ,nL,FortranArray<5>());
	ret = myMig_ref.Interpolate( vbar, 1 );

	for (int ia=1; ia<nA+1 ; ia++){
	for (int iy=1; iy<nY+1 ; iy++){
	for (int ip=1; ip<nP+1 ; ip++){
	for (int iL=1; iL<nL+1 ; iL++){
		for (int iZ=1; iZ<nZ+1; iZ++){
			// check if vbar(ia,iy,ip,iL,iZ) == pint(ip,iZ)
			EXPECT_DOUBLE_EQ( pint(ip,iZ), ret(ia,iy,ip,iZ,iL)  );	// expected, actual
		}
	}
	}
	}
	}
}



	

TEST(MigrationTest, Blimit_ownCorrect){
	
	int nA = 8;
	int nY = 2;
	int nP = 5;
	int nL = 5;
	int nZ = 2;
	int nT = 3;

	Array<double,2> trans(nY,nY,FortranArray<2>());
	trans = 0.9,0.3,0.1,0.7;

	Array<double,2> transZ(nZ,nZ,FortranArray<2>());
	//transZ = 0.9025,0.0475,0.0025,
			 //0.095,0.905,0.095,
			 //0.0025,0.0475,0.9025;
	transZ = 1;

	// get some data
	TinyVector<int,7> aypZHTt = shape(nA,nY,nP,nZ,nL,nL,nT);

	Array<double,7> tstay(aypZHTt,FortranArray<7>());	
	Array<double,7> tsell(aypZHTt,FortranArray<7>());	
	Array<double,7> trent(aypZHTt,FortranArray<7>());	
	Array<double,7> tbuy( aypZHTt,FortranArray<7>());	


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
	blim_buy = nA-1;


	// create borrowing limits are random indices
	ranlib::DiscreteUniform<int> Disc( nA );
	Array<int,3>::iterator it3;
	for (it3 = blim_own.begin();
		 it3 != blim_own.end();
		 it3 ++){
		*it3 = Disc.random() + 1;
	}
	int verbose = 1;
	
	PStruct pars2;
	pars2.beta = 0.9;
	pars2.myNA = -99;
	pars2.gamma   = 1.4;
	pars2.mgamma  = 1 - pars2.gamma;
	pars2.imgamma = 1/pars2.mgamma;
	pars2.R       = 1/(1+0.04);
	pars2.type   = gsl_interp_linear;	// change interpolation type here.
	pars2.acc    = gsl_interp_accel_alloc ();
	pars2.spline = gsl_spline_alloc (pars2.type, nP );

	// make savings tensors
	Array<double,4> save_own(nL,nL,nP,nA,FortranArray<4>());
		save_own = 0;
	for (int here=1; here<nL+1; here++){
	for (int there=1;there<nL+1;there++){
	for (int pr=1;   pr<nP+1;   pr++){
		if (blim_own(here,there,pr)>0) save_own(here,there,pr,Range(fromStart,blim_own(here,there,pr)) ) = (-1) * pars2.myNA;
	}}}

	Array<double,3> save_buy(nL,nP,nA,FortranArray<3>());
	save_buy=2;

	Array<double,1> save_rent(agrid);

	// Constructor needs GpEval and GpInt!
	// fill with ascending numbers
	// create evaluation grid and integration nodes
	Array<double,1> pval(nP,FortranArray<1>()); 
	Array<double,2> pint(nP,nZ,FortranArray<2>());

	pval = 1,2,3,4,5;
	pint = 1,1.9,2.9,3.9,4.9,
		   1.1,2.1,3.1,4.1,5;

	// fill the Grid arrays with those values
	Array<double,3> GpEval(nL,nT,nP,FortranArray<3>());
	Array<double,4> GpInt(nL,nZ,nT,nP,FortranArray<4>());

	for (int iL=1; iL<nL+1 ; iL++){
		for (int iT=1; iT<nT+1; iT++){
			GpEval(iL,iT,Range::all()) = pval;
			for (int iZ=1; iZ<nZ+1; iZ++){
				GpInt(iL,iZ,iT,Range::all()) = pint(Range::all(),iZ);
			}
		}
	}

	double resources = 2.0;
	tstay = resources;

	CMig myMig_ref(nA,nY,nP,nZ,nL,nT,
				&pars2, tstay, tsell, trent, tbuy, trans,
				transZ, MoveCost, Amenity, save_own, save_buy, save_rent, GpEval, GpInt, verbose);
		

	//// TEST
	//// * upon construction, ctmp has a positive value
	//// * calling TestCtmpSubset() will set all savings indices greater than blim_own(at,that,index) to pars2.myNA
	//// * go through blim_own and see if that is true.
	
	myMig_ref.TestCtmpSubset();
	Array<double,7> myctmp(nA, nY, nP, nZ, nL, nL , nA, FortranArray<7>());
	myctmp = myMig_ref.GetCtmp() ;

	// remember owner is only limited if here != there !!!!!

	for (int ia=1;ia<nA+1; ia++){
		for (int iy=1;iy<nY+1; iy++){
			for (int ip=1;ip<nP+1; ip++){
				for (int iZ=1;iZ<nZ+1; iZ++){
					for (int ih=1;ih<nL+1; ih++){
						for (int it=1;it<nL+1; it++){
								
							for (int is=1;is<nA+1; is++){

								// at all savings indices greater than the blimit index, 
								// we computed res - save = 2 - 0 = 2
								if (is > blim_own(ih,it,ip) ) {

									EXPECT_DOUBLE_EQ( resources, myctmp(ia,iy,ip,iZ,ih,it,is) ) << "FALSE at " << ia << iy << ip<< iZ << ih << it  << is << " ctmp is " << myctmp(ia,iy,ip,iZ,ih,it,is)  << ", blim is " << blim_own(ih,it,ip) << endl;

								// otherwise ctmp should be a positive number
								// at all savings indeces less than or equal the blimit index, (and you are moving)
								// ctmp should be -myNA 
								} else if (is <= blim_own(ih,it,ip) && ih != it ){

									EXPECT_DOUBLE_EQ( resources + pars2.R * pars2.myNA , myctmp(ia,iy,ip,iZ,ih,it,is) ) << "FALSE at " << ia << iy << ip << ih << it << iZ << is << endl;

								}
							}
						}
					}
				}
			}
		}
	}

}


TEST(MigrationTest, Blimit_buyCorrect){
	
	int nA = 8;
	int nY = 2;
	int nP = 5;
	int nL = 5;
	int nZ = 2;
	int nT = 3;
	
	Array<double,2> trans(nY,nY,FortranArray<2>());
	trans = 0.9,0.3,0.1,0.7;

	Array<double,2> transZ(nZ,nZ,FortranArray<2>());
	transZ = 0.9,0.3,0.1,0.7;

	// get some data
	TinyVector<int,7> aypZHTt = shape(nA,nY,nP,nZ,nL,nL,nT);

	Array<double,7> tstay(aypZHTt,FortranArray<7>());	
	Array<double,7> tsell(aypZHTt,FortranArray<7>());	
	Array<double,7> trent(aypZHTt,FortranArray<7>());	
	Array<double,7> tbuy( aypZHTt,FortranArray<7>());	


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


	// create borrowing limits are random indices
	ranlib::DiscreteUniform<int> Disc( nA );
	Array<int,2>::iterator it3;
	for (it3 = blim_buy.begin();
		 it3 != blim_buy.end();
		 it3 ++){
		*it3 = Disc.random() + 1;
	}
	int verbose = 1;
	
	PStruct pars2;
	pars2.beta = 0.9;
	pars2.myNA = -99;
	pars2.gamma   = 1.4;
	pars2.mgamma  = 1 - pars2.gamma;
	pars2.imgamma = 1/pars2.mgamma;
	pars2.R       = 1/(1+0.04);
	pars2.type   = gsl_interp_linear;	// change interpolation type here.
	pars2.acc    = gsl_interp_accel_alloc ();
	pars2.spline = gsl_spline_alloc (pars2.type, nP );

	// make savings tensors
	Array<double,4> save_own(nL,nL,nP,nA,FortranArray<4>());

	Array<double,3> save_buy(nL,nP,nA,FortranArray<3>());
	save_buy=0;
	for (int here=1; here<nL+1; here++){
	for (int pr=1;   pr<nP+1;   pr++){
		if (blim_buy(here,pr)>0) save_buy(here,pr,Range(fromStart,blim_buy(here,pr)) ) = (-1) * pars2.myNA;
	}}

	Array<double,1> save_rent(agrid);

	// Constructor needs GpEval and GpInt!
	// fill with ascending numbers
	// create evaluation grid and integration nodes
	Array<double,1> pval(nP,FortranArray<1>()); 
	Array<double,2> pint(nP,nZ,FortranArray<2>());

	pval = 1,2,3,4,5;
	pint = 1,1.9,2.9,3.9,4.9,
		   1.1,2.1,3.1,4.1,5;

	// fill the Grid arrays with those values
	Array<double,3> GpEval(nL,nT,nP,FortranArray<3>());
	Array<double,4> GpInt(nL,nZ,nT,nP,FortranArray<4>());

	for (int iL=1; iL<nL+1 ; iL++){
		for (int iT=1; iT<nT+1; iT++){
			GpEval(iL,iT,Range::all()) = pval;
			for (int iZ=1; iZ<nZ+1; iZ++){
				GpInt(iL,iZ,iT,Range::all()) = pint(Range::all(),iZ);
			}
		}
	}

	double resources = 2.0;
	tstay = resources;

	CMig myMig_ref(nA,nY,nP,nZ,nL,nT,
				&pars2, tstay, tsell, trent, tbuy, trans,
				transZ, MoveCost, Amenity, save_own, save_buy, save_rent, GpEval, GpInt, verbose);
		
	myMig_ref.TestCtmpSubset_Buy();
	Array<double,7> myctmp(nA, nY, nP, nZ, nL, nL , nA, FortranArray<7>());
	myctmp = myMig_ref.GetCtmp() ;


	for (int ia=1;ia<nA+1; ia++){
		for (int iy=1;iy<nY+1; iy++){
			for (int ip=1;ip<nP+1; ip++){
				for (int iZ=1;iZ<nZ+1; iZ++){
					for (int ih=1;ih<nL+1; ih++){
						for (int it=1;it<nL+1; it++){
							
							for (int is=1;is<nA+1; is++){

							//// at all savings indices greater than the blimit index, 
							//// ctmp should be a positive number
								if (is > blim_buy(it,ip) ) {

									EXPECT_DOUBLE_EQ( resources, myctmp(ia,iy,ip,iZ,ih,it,is) ) << "FALSE at " << ia << iy << ip << ih << it << iZ << is << " ctmp is " << myctmp(ia,iy,ip,iZ,ih,it,is)  << ", blim is " <<blim_buy(it,ip) << endl;

								} else if (is <= blim_buy(it,ip)){

									EXPECT_DOUBLE_EQ( resources + pars2.R * pars2.myNA , myctmp(ia,iy,ip,iZ,ih,it,is) ) << "FALSE at " << ia << iy << ip << ih << it << iZ << is << endl;
								}

							}
						}
					}
				}
			}
		}
	}

}



// test whether integration function is correct
// NOTE: this test only checks whether integration 
// give a one array if given a one array. it cannot
// check the ordering of the tensor operation.
TEST(MigTest, checkIntegration) {

	int nA = 8;
	int nY = 2;
	int nP = 3;
	int nL = 5;
	int nZ = 3;
	int nT = 3;
	
	Array<double,2> trans(nY,nY,FortranArray<2>());
	trans = 0.9,0.3,0.1,0.7;

	Array<double,2> transZ(nZ,nZ,FortranArray<2>());
	transZ = 0.9025,0.0475,0.0025,
			 0.095,0.905,0.095,
			 0.0025,0.0475,0.9025;

	// get some data
	TinyVector<int,7> aypZHTt = shape(nA,nY,nP,nZ,nL,nL,nT);

	Array<double,7> tstay(aypZHTt,FortranArray<7>());	
	Array<double,7> tsell(aypZHTt,FortranArray<7>());	
	Array<double,7> trent(aypZHTt,FortranArray<7>());	
	Array<double,7> tbuy( aypZHTt,FortranArray<7>());	


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
	Array<int,2> blim_buy(nL,nP,FortranArray<2>());

	// create borrowing limits are random indices
	ranlib::DiscreteUniform<int> Disc( nA );

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
	pars2.R       = 1/(1+0.04);

	// make savings tensors
	// this is to be done in R when producing stuff
	// ============================================
	
	Array<double,3> save_buy(nL,nP,nA,FortranArray<3>());
	save_buy= 0;

	Array<double,4> save_own(nL,nL,nP,nA,FortranArray<4>());
	save_own=2;

	Array<double,1> save_rent(agrid);
	
	Array<double,3> GpEval(nL,nT,nP,FortranArray<3>());
	Array<double,4> GpInt(nL,nZ,nT,nP,FortranArray<4>());

	CMig myMig_ref(nA,nY,nP,nZ,nL,nT,
				&pars2, tstay, tsell, trent, tbuy, trans,
				transZ, MoveCost, Amenity, save_own, save_buy, save_rent, GpEval, GpInt, verbose);

	// get data
	Array<double,5> test(nA,nY,nP,nZ,nL,FortranArray<5>());
	Array<double,5> bout(nA,nY,nP,nZ,nL,FortranArray<5>());
	test = 1;
	bout = myMig_ref.integrate(test);

	// define an iterator for a blitz array
	Array<double,5>::iterator iter;

	// iterate over blitz and fill vector with values
	for (iter = bout.begin() ; iter!=bout.end();++iter){
		EXPECT_DOUBLE_EQ( 1.0, *iter );
	}

}



//// check whether discrete choice function 
//// works correctly
//TEST(Mig6Test,TestDchoice5d){

	//CMig myMig;

	//// setup test for dchoice3d
	//Array<double,5> one(2,2,2,2,2,FortranArray<5>());
	//Array<double,5> two(2,2,2,2,2,FortranArray<5>());
	//Array<double,5> max12(2,2,2,2,2,FortranArray<5>());
	//Array<double,5> ret(2,2,2,2,2,FortranArray<5>());

	//one = 1;
	//two = 0,0,2,0,2,0,2,2,
		  //0,0,2,0,2,0,2,2,
		  //0,0,2,0,2,0,2,2,
          //0,0,2,0,2,0,2,2;

	//max12 = where(one > two, one, two );

	//ret = myMig.dchoice5d(one,two);
	//// define two std::vectors to take the values
	////std::vector<double> vec;
	//Array<double,5>::iterator it1;
	//Array<double,5>::iterator it2;
	//double idx;

	//// iterate over blitz and fill vector with values
	//for (it1 = ret.begin(), it2 = max12.begin() ; it1!=ret.end() && it2!=max12.end() ; ++it1, ++it2){

		//EXPECT_EQ( *it2, *it1 );
	//}

//}






//// Test whether can GET various objects
//TEST(Mig6Test, TestGetters) {

	//CMig myMig;
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
	//TinyVector<int,6> aypHTt = shape(nA,nY,nP,nL,nL,nT);

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

	//CMig myMig_ref(nA, nY, nP, nL, nT,
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
