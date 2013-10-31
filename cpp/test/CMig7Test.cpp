
#include "CMig7.h" // Include the code that we are testing
#include <vector>
#include <gtest/gtest.h> // Include the google test framework
#include <random/uniform.h>
#include <typeinfo>		// do some type testing



using namespace blitz;


// test can construct the default object
TEST(Mig7Test, TestDefaultConstructor) {

	// create an instance of owner class
	CMig7 myMig;

	// is a CMig6 object?
	EXPECT_EQ("CMig7", myMig.version());

	//// dimension of biggest array is ... ?
	EXPECT_EQ(3 , myMig.GetMaxDim() );

	EXPECT_EQ(2, myMig.GetMaxage() );
}


// test whether integration function is correct
TEST(Mig7Test, checkIntegration) {

	// create an instance of owner class
	CMig7 myMig;

	TinyVector<int,2> dim;
	dim = myMig.GetDim();
	// get data
	Array<double,2> test(dim(0),dim(1),FortranArray<2>());
	Array<double,2> bout(dim(0),dim(1),FortranArray<2>());
	test = 1;
	bout = myMig.integrate(test);

	std::vector<double> out;
	// define an iterator for a blitz array
	Array<double,2>::iterator iter;

	// iterate over blitz and fill vector with values
	for (iter = bout.begin() ; iter!=bout.end();++iter){
		out.push_back(*iter);
	}

	for (int i=0; i<out.size(); i++){
		EXPECT_EQ(1,out.at(i)) << "integration not 1 at index " << i;
	}
}





// test can construct the referenced object
TEST(Mig7Test, TestReferencedConstructor) {

	// dim vectors
    TinyVector<int,3> dim_ay_t;

	int na = 20;
    Array<double,2> trans(shape(2,2),FortranArray<2>());
	trans = 0.9,0.3,0.1,0.7;

    dim_ay_t = na,2,2;

	// get some data

	Array<double,3> tstay(na,2,2,neverDeleteData,FortranArray<3>());	
	Array<double,3> tsell(na,2,2,neverDeleteData,FortranArray<3>());	

	//fill with random numbers
	ranlib::Uniform<double> uniGen;
	Array<double,3>::iterator it;

	for (it = tstay.begin(); it!=tstay.end(); it++) {
		*it = uniGen.random() + 1;
	}
	for (it = tsell.begin(); it!=tsell.end(); it++) {
		*it = uniGen.random();
	}

	// get two savings spaces
	Array<double,1> a_own(na);
	Array<double,1> a_rent(na);
	a_own(0) = -2;
	a_rent(0) = 0;
	a_own(na-1) = 3;
	a_rent(na-1) = 3;

	double step_own = (a_own(na-1)-a_own(0)) / na;
	double step_rent = (a_rent(na-1)-a_rent(0)) / na;
	for (int i=1;i<na;i++) a_own(i) = a_own(i-1) + step_own;
	for (int i=1;i<na;i++) a_rent(i) = a_rent(i-1) + step_rent;

	double cutoff, gamma, theta;
	cutoff = 0.1;
	gamma = 1.5;
	theta = 0.3;

	gsl_f_pars p;
	p.res    = 0;
	//p.type   = gsl_interp_cspline;
	p.type   = gsl_interp_linear;	// change interpolation type here.
	p.acc    = gsl_interp_accel_alloc ();
	p.spline = gsl_spline_alloc (p.type, a_own.size());
	p.T      = gsl_root_fsolver_brent;
	p.sroot  = gsl_root_fsolver_alloc (p.T);

	// create an instance of owner class
	CMig7 myMig(dim_ay_t,          
				tstay,tsell,trans,a_own,a_rent,0,cutoff,gamma,theta, &p);

	// is a CMig6 object?
	EXPECT_EQ("CMig7", myMig.version());

	//// dimension of biggest array is ... ?
	EXPECT_EQ(3 , myMig.GetMaxDim() );
}


// test can construct the referenced object with param data
// and compute a period
TEST(Mig7Test, TestComputePeriod) {

	// dim vectors
    TinyVector<int,3> dim_ay_t;

	int na = 20;
    Array<double,2> trans(shape(2,2),FortranArray<2>());
	trans = 0.9,0.3,0.1,0.7;

    dim_ay_t = na,2,2;

	// get some data

	Array<double,3> tstay(na,2,2,neverDeleteData,FortranArray<3>());	
	Array<double,3> tsell(na,2,2,neverDeleteData,FortranArray<3>());	

	//fill with random numbers
	ranlib::Uniform<double> uniGen;
	Array<double,3>::iterator it;

	for (it = tstay.begin(); it!=tstay.end(); it++) {
		*it = uniGen.random() + 1;
	}
	for (it = tsell.begin(); it!=tsell.end(); it++) {
		*it = uniGen.random();
	}

	// get two savings spaces
	Array<double,1> a_own(na);
	Array<double,1> a_rent(na);
	a_own(0) = -2;
	a_rent(0) = 0;
	a_own(na-1) = 3;
	a_rent(na-1) = 3;

	double step_own = (a_own(na-1)-a_own(0)) / na;
	double step_rent = (a_rent(na-1)-a_rent(0)) / na;
	for (int i=1;i<na;i++) a_own(i) = a_own(i-1) + step_own;
	for (int i=1;i<na;i++) a_rent(i) = a_rent(i-1) + step_rent;

	double cutoff, gamma, theta;
	cutoff = 0.1;
	gamma = 1.5;
	theta = 0.3;

	gsl_f_pars p;
	p.res    = 0;
	//p.type   = gsl_interp_cspline;
	p.type   = gsl_interp_linear;	// change interpolation type here.
	p.acc    = gsl_interp_accel_alloc ();
	p.spline = gsl_spline_alloc (p.type, a_own.size());
	p.T      = gsl_root_fsolver_brent;
	p.sroot  = gsl_root_fsolver_alloc (p.T);

	// create an instance of owner class
	CMig7 myMig(dim_ay_t,          
				tstay,tsell,trans,a_own,a_rent,0,cutoff,gamma,theta,&p);

 
	EXPECT_DOUBLE_EQ(1.5, myMig.GetGamma());

	// can compute a period?
	myMig.ComputePeriod(2);
	myMig.ComputePeriod(1);

	// is a CMig6 object?
	EXPECT_EQ("CMig7", myMig.version());

	//// dimension of biggest array is ... ?
	EXPECT_EQ(3 , myMig.GetMaxDim() );
}


// test can construct the referenced object with param data
// and compute a many periods 
TEST(Mig7Test, TestReferencedWithComputatoin) {

	// dim vectors
    TinyVector<int,3> dim_ay_t;

	int na = 20;
	int nT = 20;
    Array<double,2> trans(shape(2,2),FortranArray<2>());
	trans = 0.9,0.3,0.1,0.7;

    dim_ay_t = na,2,nT;

	// get some data

	Array<double,3> tstay(na,2,nT,neverDeleteData,FortranArray<3>());	
	Array<double,3> tsell(na,2,nT,neverDeleteData,FortranArray<3>());	

	//fill with random numbers
	ranlib::Uniform<double> uniGen;
	Array<double,3>::iterator it;

	for (it = tstay.begin(); it!=tstay.end(); it++) {
		*it = uniGen.random() + 1;
	}
	for (it = tsell.begin(); it!=tsell.end(); it++) {
		*it = uniGen.random();
	}

	// get two savings spaces
	Array<double,1> a_own(na);
	Array<double,1> a_rent(na);
	a_own(0) = -2;
	a_rent(0) = 0;
	a_own(na-1) = 3;
	a_rent(na-1) = 3;

	double step_own = (a_own(na-1)-a_own(0)) / na;
	double step_rent = (a_rent(na-1)-a_rent(0)) / na;
	for (int i=1;i<na;i++) a_own(i) = a_own(i-1) + step_own;
	for (int i=1;i<na;i++) a_rent(i) = a_rent(i-1) + step_rent;

	double cutoff, gamma, theta;
	cutoff = 0.1;
	gamma = 1.5;
	theta = 0.3;

	gsl_f_pars p;
	p.res    = 0;
	//p.type   = gsl_interp_cspline;
	p.type   = gsl_interp_linear;	// change interpolation type here.
	p.acc    = gsl_interp_accel_alloc ();
	p.spline = gsl_spline_alloc (p.type, a_own.size());
	p.T      = gsl_root_fsolver_brent;
	p.sroot  = gsl_root_fsolver_alloc (p.T);

	// create an instance of owner class
	CMig7 myMig(dim_ay_t,tstay,tsell,trans,a_own,a_rent,0,cutoff,gamma,theta,&p);

 
	EXPECT_DOUBLE_EQ(1.5, myMig.GetGamma());

	// can compute a period?
	for (int ti=myMig.GetMaxage(); ti>0; ti--){
		myMig.ComputePeriod( ti );
	}
	//myMig.ComputePeriod(nT-1);

	// is a CMig6 object?
	EXPECT_EQ("CMig7", myMig.version());

	//// dimension of biggest array is ... ?
	EXPECT_EQ(3 , myMig.GetMaxDim() );
}
TEST(Mig7Test, TestUtilityParameters){

	CMig7 myMig;

	EXPECT_EQ( 1.4, myMig.GetGamma() );
	EXPECT_EQ( 1-1.4, myMig.GetMgamma() );
	EXPECT_EQ( 0.05 , myMig.GetCutoff() );
	EXPECT_DOUBLE_EQ( pow(0.05,1-1.4), myMig.GetTmpu() );
	EXPECT_DOUBLE_EQ( pow(0.05,1-1.4) / 0.05, myMig.GetDtmpu() );
	EXPECT_DOUBLE_EQ( -1.4 * (pow(0.05,1-1.4) / 0.05) / 0.05, myMig.GetDdtmpu() );

}


TEST(Mig7Test, TestUtilityFunction) {

	CMig7 myMig;

	EXPECT_DOUBLE_EQ( -2.5, myMig.utility( 1 ) ) << "utility function at c=1 is wrong";

	// compute approximation to c=-1 by hand:
	double cut = myMig.GetCutoff() ;
    double diff = -1 - cut;
	double tmp = pow(cut,1-1.4);
	double dtmp_dc = tmp / cut;
	double ddtmp_dcc = -1.4 * (dtmp_dc / cut);
	double util = (1/(1-1.4)) * tmp + dtmp_dc*diff + 0.5 * ddtmp_dcc * pow(diff,2);

	EXPECT_EQ( util, myMig.utility( -1 ) ) << "utility function at c=-1 is wrong";

}

TEST(Mig7Test, TestMarginalUtilityFunction) {

	CMig7 myMig;

	EXPECT_EQ( 1, myMig.mutility( 1 ) ) << "marginal utility function at c=1 is wrong";
	EXPECT_DOUBLE_EQ( 0.37892914162759955, myMig.mutility( 2 ) ) << "marginal utility function at c=2 is wrong";

	// compute approximation to du(-1) by hand:
	double cut = myMig.GetCutoff() ;
    double diff = -1 - cut;
	double dtmp_dc = pow(cut,-1.4);
	double ddtmp_dcc = -1.4 * dtmp_dc / cut;
	double grad = dtmp_dc + ddtmp_dcc * diff;

	EXPECT_DOUBLE_EQ( grad , myMig.mutility( -1 ) ) << "marginal utility function at c=-1 is wrong";

}


//TEST(Mig7Test, TestSpline) {

	//CMig7 myMig;
	
	//gsl_f_pars p;
	//p.res    = 0;
	//p.type   = gsl_interp_cspline;
	//p.acc    = gsl_interp_accel_alloc ();
	//p.spline = gsl_spline_alloc (p.type, myMig.GetAgrid_own().size());
	//p.T      = gsl_root_fsolver_brent;
	//p.sroot  = gsl_root_fsolver_alloc (p.T);


 
    //myMig.setPars( &p );	

	//double data[13] = {0.1,1,2,3,4,5,6,7,8,9,10,11,12};
	//for (int i=0; i<13; i++) data[i] = log(data[i]);
	//Array<double,13> ret;


	//ret = myMig.SplineTester( data, 13 );
	//for (int i=0; i<13; i++){
		//EXPECT_EQ( ret(i), data[i] ) ;
	//}
/*}*/

int main(int argc, char **argv) { 
  ::testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}
