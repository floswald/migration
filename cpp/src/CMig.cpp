


#ifdef RcppCompile
#include "../../cpp/src/CMig.h"
#else
#include "CMig.h"		// include class declaration of owner class
#endif

using namespace blitz;
using namespace std;


// test function for list

//Rcpp::List myfun( void ){
	//Rcpp::List mylist = Rcpp::List::create( Rcpp::_["elt1"] = 1 );
	//return mylist;
//}


///////////////////////
// class implementation
// of Migration class
///////////////////////

CMig::CMig() :

	// the default constructor initiates all arrays with an extent of 2 in each dimension

	// Arrays of size (a,y,p,here,there,age)
	ResStay(        2,2,2,2,2,2,2,FortranArray<7>()),
    ResSell(        2,2,2,2,2,2,2,FortranArray<7>()),
	ResRent(        2,2,2,2,2,2,2,FortranArray<7>()),
    ResBuy(         2,2,2,2,2,2,2,FortranArray<7>()),
                                  
    v_loc_stay(     2,2,2,2,2,2,2,FortranArray<7>()),	
    v_loc_sell(     2,2,2,2,2,2,2,FortranArray<7>()),  
    v_loc_rent(     2,2,2,2,2,2,2,FortranArray<7>()),	
    v_loc_buy(      2,2,2,2,2,2,2,FortranArray<7>()),   
	c_loc_stay(     2,2,2,2,2,2,2,FortranArray<7>()),	
	c_loc_sell(     2,2,2,2,2,2,2,FortranArray<7>()),  
	c_loc_rent(     2,2,2,2,2,2,2,FortranArray<7>()),	
	c_loc_buy(      2,2,2,2,2,2,2,FortranArray<7>()),   
	s_loc_stay(     2,2,2,2,2,2,2,FortranArray<7>()),  
	s_loc_sell(     2,2,2,2,2,2,2,FortranArray<7>()), 
	s_loc_rent(     2,2,2,2,2,2,2,FortranArray<7>()), 
	s_loc_buy(      2,2,2,2,2,2,2,FortranArray<7>()), 
	
	W_loc_rent(     2,2,2,2,2,2,2,FortranArray<7>()), 
	W_loc_own(      2,2,2,2,2,2,2,FortranArray<7>()), 
	rho_rent(       2,2,2,2,2,2,2,FortranArray<7>()), 
	rho_own(        2,2,2,2,2,2,2,FortranArray<7>()), 
	Tenure_loc_rent(2,2,2,2,2,2,2,FortranArray<7>()), 
	Tenure_loc_own( 2,2,2,2,2,2,2,FortranArray<7>()), 

	// Arrays of size (a,y,p,here,there,a)
	ctmp(           2,2,2,2,2,2,2,FortranArray<7>()),  	
	xtmp(           2,2,2,2,2,2,2,FortranArray<7>()),  	

	// Arrays of size (a,y,p,here,there)
	restmp(         2,2,2,2,2,2,  FortranArray<6>()),  	

	// Arrays of size (a,y,p,here,age)
	Vown(           2,2,2,2,2,2,   FortranArray<6>()),
	Vrent(          2,2,2,2,2,2,   FortranArray<6>()),
	EVown(          2,2,2,2,2,2,   FortranArray<6>()),
	EVrent(         2,2,2,2,2,2,   FortranArray<6>()),
	Vbar_own(       2,2,2,2,2,2,   FortranArray<6>()),
	Vbar_rent(      2,2,2,2,2,2,   FortranArray<6>()),
	Location_own(   2,2,2,2,2,2,   FortranArray<6>()),   
	Location_rent(  2,2,2,2,2,2,   FortranArray<6>()),   

	v_loc_tmp(      2,2,2,2,2,2,   FortranArray<6>()),	
	vbar_tmp(       2,2,2,2,2,     FortranArray<5>()),	

	vplustmp(       2,2,2,2,2,     FortranArray<5>()), 	// (a,y,p,here,Z)

	save_own(       2,2,2,2,       FortranArray<4>()), 	// (here,there,p,save) 	
	save_buy(       2,2,2,         FortranArray<3>()),  // (     there,p,save)	
	save_rent(      2,             FortranArray<1>()),  // (             save)

	MoveCost(       2,2,           FortranArray<2>()) ,	// (here,there)
	G(              2,2,           FortranArray<2>()) ,	// (y,y')
	Gz(             2,2,           FortranArray<2>()) ,	// (Z,Z')
	pgrid(          2,             FortranArray<1>()),	
	Amenity(        2,             FortranArray<1>()) ,
	tmp1(           2,             FortranArray<1>()),

	verbose(1),
	nLoc(2),
	nPrice(2),
	nIncome(2),
	nAsset(2),
	nAgg(2),
	name("CMig_default"){	

		// set array values
		ResStay         = 0.1;
		ResSell         = 0.2;
		ResRent         = 0.3;
		ResBuy          = 0.4;
		v_loc_stay      = 1;
		v_loc_sell      = 2;
		v_loc_rent      = 3;
		v_loc_buy       = 4;
		s_loc_stay      = 1;
		s_loc_sell      = 2;
		s_loc_rent      = 3;
		s_loc_buy       = 4;
		xtmp            = 1;
		ctmp            = 2;
		restmp          = 3;
		W_loc_rent      = 0;
		W_loc_own       = 0;
		rho_rent        = 0;
		rho_own         = 0;
		Tenure_loc_rent = 0;
		Tenure_loc_own  = 0;
		Location_own    = 0;
		Location_rent   = 0;
		// get the dimension of the problem
		dim_ayp_here_there_Z_t = ResStay.extent();
		// Set transition matrices and grids
		G        = 1.0/dim_ayp_here_there_Z_t(1);
		Gz       = 1.0/dim_ayp_here_there_Z_t(5);
		MoveCost = 1.0/nLoc;
	    Amenity  = 2;
		pgrid    = 1;
		// set parameter values
		p.myNA    = -99;
		p.beta    = 0.9;
		p.gamma   = 1.4;
		p.mgamma  = -0.4;
		p.imgamma = -1/0.4;
		p.theta   = 0.1;
		p.R       = 1/(1+0.4);
		maxage    = dim_ayp_here_there_Z_t(6);
}

// constructor 1
CMig::CMig(int nA, int nY, int nP, int nL, int nZ, int nT):

	// this is syntactic sugar. an initialiser list. 

	// Arrays of size (a,y,p,here,there,Z,age)
	ResStay(        nA,nY,nP,nL,nL,nZ,nT,FortranArray<7>()),
    ResSell(        nA,nY,nP,nL,nL,nZ,nT,FortranArray<7>()),
	ResRent(        nA,nY,nP,nL,nL,nZ,nT,FortranArray<7>()),
    ResBuy(         nA,nY,nP,nL,nL,nZ,nT,FortranArray<7>()),
                                      
    v_loc_stay(     nA,nY,nP,nL,nL,nZ,nT,FortranArray<7>()),	
    v_loc_sell(     nA,nY,nP,nL,nL,nZ,nT,FortranArray<7>()),  
    v_loc_rent(     nA,nY,nP,nL,nL,nZ,nT,FortranArray<7>()),	
    v_loc_buy(      nA,nY,nP,nL,nL,nZ,nT,FortranArray<7>()),   
	c_loc_stay(     nA,nY,nP,nL,nL,nZ,nT,FortranArray<7>()),	
	c_loc_sell(     nA,nY,nP,nL,nL,nZ,nT,FortranArray<7>()),  
	c_loc_rent(     nA,nY,nP,nL,nL,nZ,nT,FortranArray<7>()),	
	c_loc_buy(      nA,nY,nP,nL,nL,nZ,nT,FortranArray<7>()),   
	s_loc_stay(     nA,nY,nP,nL,nL,nZ,nT,FortranArray<7>()),  
	s_loc_sell(     nA,nY,nP,nL,nL,nZ,nT,FortranArray<7>()), 
	s_loc_rent(     nA,nY,nP,nL,nL,nZ,nT,FortranArray<7>()), 
	s_loc_buy(      nA,nY,nP,nL,nL,nZ,nT,FortranArray<7>()), 
	
	W_loc_rent(     nA,nY,nP,nL,nL,nZ,nT,FortranArray<7>()), 
	W_loc_own(      nA,nY,nP,nL,nL,nZ,nT,FortranArray<7>()), 
	rho_rent(       nA,nY,nP,nL,nL,nZ,nT,FortranArray<7>()), 
	rho_own(        nA,nY,nP,nL,nL,nZ,nT,FortranArray<7>()), 
	Tenure_loc_rent(nA,nY,nP,nL,nL,nZ,nT,FortranArray<7>()), 
	Tenure_loc_own( nA,nY,nP,nL,nL,nZ,nT,FortranArray<7>()), 

	// Arrays of size (a,y,p,here,there,Z,a)
	ctmp(           nA,nY,nP,nL,nL,nZ,nA,FortranArray<7>()),  	
	xtmp(           nA,nY,nP,nL,nL,nZ,nA,FortranArray<7>()),  	
	
	// Arrays of size (a,y,p,here,there,Z)
	restmp(    		nA,nY,nP,nL,nL,nZ,   FortranArray<6>()),  	

	// Arrays of size (a,y,p,here,Z,age)
	Vown(  	        nA,nY,nP,nL,nZ,nT,   FortranArray<6>()),
	Vrent( 	        nA,nY,nP,nL,nZ,nT,   FortranArray<6>()),
	EVown( 	        nA,nY,nP,nL,nZ,nT,   FortranArray<6>()),
	EVrent(	        nA,nY,nP,nL,nZ,nT,   FortranArray<6>()),
	Vbar_own( 	    nA,nY,nP,nL,nZ,nT,   FortranArray<6>()),
	Vbar_rent(	    nA,nY,nP,nL,nZ,nT,   FortranArray<6>()),
	Location_own(   nA,nY,nP,nL,nZ,nT,   FortranArray<6>()),
	Location_rent(  nA,nY,nP,nL,nZ,nT,   FortranArray<6>()),

	v_loc_tmp(      nA,nY,nP,nL,nL,nZ,   FortranArray<6>()),
	vbar_tmp(       nA,nY,nP,nL,nZ,      FortranArray<5>()),

	vplustmp(       nA,nY,nP,nL,nZ,      FortranArray<5>()), 	// (a,y,p,here)
	
	save_own(       nL,nL,nP,nA,         FortranArray<4>()), 	// (here,there,p,save) 	
	save_buy(       nL,nP,nA,            FortranArray<3>()), 	// (     there,p,save) 	
	save_rent(      nA,                  FortranArray<1>()),    // (             save)

	MoveCost(       nL,nL,               FortranArray<2>()) ,	// (here,there)
	G(              nY,nY,               FortranArray<2>()) ,	// (y,y')
	Gz(             nZ,nZ,               FortranArray<2>()) ,	// (z,z')
	pgrid(          nP,                  FortranArray<1>()),	
	Amenity(        nL,                  FortranArray<1>()) ,
	tmp1(           nP,                  FortranArray<1>()),

	verbose(1),
	nLoc(nL),
	nPrice(nP),
	nIncome(nY),
	nAsset(nA),
	nAgg(nZ),
	name("CMig_dims_given"){	

		// set array values
		ResStay         = 0.1;
		ResSell         = 0.2;
		ResRent         = 0.3;
		ResBuy          = 0.4;
		v_loc_stay      = 1;
		v_loc_sell      = 2;
		v_loc_rent      = 3;
		v_loc_buy       = 4;
		s_loc_stay      = 1;
		s_loc_sell      = 2;
		s_loc_rent      = 3;
		s_loc_buy       = 4;
		xtmp            = 1;
		ctmp            = 2;
		restmp          = 3;
		W_loc_rent      = 0;
		W_loc_own       = 0;
		rho_rent        = 0;
		rho_own         = 0;
		Tenure_loc_rent = 0;
		Tenure_loc_own  = 0;
		Location_own    = 0;
		Location_own    = 0;
		// get the dimension of the problem
		dim_ayp_here_there_Z_t = ResStay.extent();
		// Set transition matrices and grids
		G        = 1.0/dim_ayp_here_there_Z_t(1);
		Gz       = 1.0/dim_ayp_here_there_Z_t(5);
		MoveCost = 1.0/nLoc;
	    Amenity  = 2;
		pgrid    = 1;
		// set parameter values
		p.myNA    = -99;
		p.beta    = 0.9;
		p.gamma   = 1.4;
		p.mgamma  = -0.4;
		p.imgamma = -1/0.4;
		p.theta   = 0.1;
		p.R       = 1/(1+0.4);
		maxage    = dim_ayp_here_there_Z_t(6);
		save_own   = 1;
		save_buy   = 2;
		save_rent  = 3;
}

CMig::CMig(int nA, int nY, int nP, int nL, int nZ, int nT,
				PStruct * data_pars,
				Array<double,7> data_stay,
				Array<double,7> data_sell,
				Array<double,7> data_rent,
				Array<double,7> data_buy,
				Array<double,2> data_G,	
				Array<double,2> data_Gz,	
				Array<double,2> data_MoveCost,
				Array<double,1> data_Amenity,
				Array<double,4> data_save_own,
				Array<double,3> data_save_buy,
				Array<double,1> data_save_rent,
				Array<double,3> data_GpEval,
				Array<double,4> data_GpInt,
			 int data_verbose)  :              

	// this is syntactic sugar. an initialiser list. 

	// Arrays of size (a,y,p,here,there,Z,age)
	ResStay(        nA,nY,nP,nL,nL,nZ,nT,FortranArray<7>()),
    ResSell(        nA,nY,nP,nL,nL,nZ,nT,FortranArray<7>()),
	ResRent(        nA,nY,nP,nL,nL,nZ,nT,FortranArray<7>()),
    ResBuy(         nA,nY,nP,nL,nL,nZ,nT,FortranArray<7>()),
                                      
    v_loc_stay(     nA,nY,nP,nL,nL,nZ,nT,FortranArray<7>()),	
    v_loc_sell(     nA,nY,nP,nL,nL,nZ,nT,FortranArray<7>()),  
    v_loc_rent(     nA,nY,nP,nL,nL,nZ,nT,FortranArray<7>()),	
    v_loc_buy(      nA,nY,nP,nL,nL,nZ,nT,FortranArray<7>()),   
	c_loc_stay(     nA,nY,nP,nL,nL,nZ,nT,FortranArray<7>()),	
	c_loc_sell(     nA,nY,nP,nL,nL,nZ,nT,FortranArray<7>()),  
	c_loc_rent(     nA,nY,nP,nL,nL,nZ,nT,FortranArray<7>()),	
	c_loc_buy(      nA,nY,nP,nL,nL,nZ,nT,FortranArray<7>()),   
	s_loc_stay(     nA,nY,nP,nL,nL,nZ,nT,FortranArray<7>()),  
	s_loc_sell(     nA,nY,nP,nL,nL,nZ,nT,FortranArray<7>()), 
	s_loc_rent(     nA,nY,nP,nL,nL,nZ,nT,FortranArray<7>()), 
	s_loc_buy(      nA,nY,nP,nL,nL,nZ,nT,FortranArray<7>()), 
	
	W_loc_rent(     nA,nY,nP,nL,nL,nZ,nT,FortranArray<7>()), 
	W_loc_own(      nA,nY,nP,nL,nL,nZ,nT,FortranArray<7>()), 
	rho_rent(       nA,nY,nP,nL,nL,nZ,nT,FortranArray<7>()), 
	rho_own(        nA,nY,nP,nL,nL,nZ,nT,FortranArray<7>()), 
	Tenure_loc_rent(nA,nY,nP,nL,nL,nZ,nT,FortranArray<7>()), 
	Tenure_loc_own( nA,nY,nP,nL,nL,nZ,nT,FortranArray<7>()), 

	// Arrays of size (a,y,p,here,there,Z,a)
	ctmp(           nA,nY,nP,nL,nL,nZ,nA,FortranArray<7>()),  	
	xtmp(           nA,nY,nP,nL,nL,nZ,nA,FortranArray<7>()),  	
	
	// Arrays of size (a,y,p,here,there,Z)
	restmp(    		nA,nY,nP,nL,nL,nZ,   FortranArray<6>()),  	

	// Arrays of size (a,y,p,here,Z,age)
	Vown(  	        nA,nY,nP,nL,nZ,nT,   FortranArray<6>()),
	Vrent( 	        nA,nY,nP,nL,nZ,nT,   FortranArray<6>()),
	EVown( 	        nA,nY,nP,nL,nZ,nT,   FortranArray<6>()),
	EVrent(	        nA,nY,nP,nL,nZ,nT,   FortranArray<6>()),
	Vbar_own( 	    nA,nY,nP,nL,nZ,nT,   FortranArray<6>()),
	Vbar_rent(	    nA,nY,nP,nL,nZ,nT,   FortranArray<6>()),
	Location_own(   nA,nY,nP,nL,nZ,nT,   FortranArray<6>()),
	Location_rent(  nA,nY,nP,nL,nZ,nT,   FortranArray<6>()),

	v_loc_tmp(      nA,nY,nP,nL,nL,nZ,   FortranArray<6>()),
	vbar_tmp(       nA,nY,nP,nL,nZ,      FortranArray<5>()),

	vplustmp(       nA,nY,nP,nL,nZ,      FortranArray<5>()), 	// (a,y,p,here)
	
	save_own(       nL,nL,nP,nA,         FortranArray<4>()), 	// (here,there,p,save) 	
	save_buy(       nL,nP,nA,            FortranArray<3>()), 	// (     there,p,save) 	
	save_rent(      nA,                  FortranArray<1>()),    // (             save)

	MoveCost(       nL,nL,               FortranArray<2>()) ,	// (here,there)
	G(              nY,nY,               FortranArray<2>()) ,	// (y,y')
	Gz(             nZ,nZ,               FortranArray<2>()) ,	// (z,z')
	Amenity(        nL,                  FortranArray<1>()) ,
	tmp1(           nP,                  FortranArray<1>()),

	// price grids
	GpEval(         nL,nT,nP,            FortranArray<3>()),
	GpInt(          nL,nZ,nT,nP,         FortranArray<4>()),
	pgrid(          nP,                  FortranArray<1>()),

	verbose(data_verbose),
	nLoc(nL),
	nPrice(nP),
	nIncome(nY),
	nAsset(nA),
	nAgg(nZ),
	name("CMig_data_referenced"),	
	p(*data_pars) {						  

		// set array values
		ResStay.reference(data_stay);
		ResSell.reference(data_sell);
		ResRent.reference(data_rent);
		ResBuy.reference(data_buy );
		G.reference(data_G);
		Gz.reference(data_Gz);
		MoveCost.reference(data_MoveCost);
		Amenity.reference(data_Amenity);
		save_own.reference(data_save_own);
		save_buy.reference(data_save_buy);
		save_rent.reference(data_save_rent);
		GpEval.reference(data_GpEval);
		GpInt.reference(data_GpInt);
		W_loc_rent      = 0;
		W_loc_own       = 0;
		rho_rent        = 0;
		rho_own         = 0;
		Vbar_rent       = 0;
		Vbar_own        = 0;
		Tenure_loc_rent = 0;
		Tenure_loc_own  = 0;
		Location_own    = 0;
		Location_rent   = 0;
		Vown       = 1;
		Vrent      = 2;
		v_loc_stay = 1;
		v_loc_sell = 2;
		v_loc_rent = 3;
		v_loc_buy  = 4;
		s_loc_stay = 1;
		s_loc_sell = 2;
		s_loc_rent = 3;
		s_loc_buy  = 4;
		xtmp       = 2;
		ctmp       = 3;
		restmp     = 1;
		// get the dimension of the problem
		dim_ayp_here_there_Z_t = ResStay.extent();
		maxage = dim_ayp_here_there_Z_t(6);
}


// Define Getters



// this printing stuff should be done with Rprintf. Rcout looks as terrible as cout in console.

//#ifdef RcppCompile   // conditions for printing the show method: if you are working from R, print with Rcpp::Rcout

//// Define show method
//// TODO ask how to compile with R libraries
/*void CMig::show(){*/
	//int ma = 10;
	//int my = 10;
	//int mL = 10;
	//ma = min(ma,dim_ayp_here_there_t(0));
	//my = min(my,dim_ayp_here_there_t(1));
	//mL = min(mL,dim_ayp_here_there_t(3));

	//Rcpp::Rcout << "This is the show method of class " << name << endl;
	//Rcpp::Rcout << "we have this dimension vector: " << endl;
	//Rcpp::Rcout <<  dim_ayp_here_there_t << endl;
	//Rcpp::Rcout << "we have beta: " << endl;
	//Rcpp::Rcout <<  p.beta << endl;
	//Rcpp::Rcout << "we have myNA: " << endl;
	//Rcpp::Rcout <<  p.myNA << endl;
	//Rcpp::Rcout << "we have G: " << endl;
	//Rcpp::Rcout <<  G << endl;
	//Rcpp::Rcout << "we have Gp: " << endl;
	//Rcpp::Rcout <<  Gp << endl;
	//Rcpp::Rcout << "we have Amenity: " << endl;
	//Rcpp::Rcout <<  Amenity << endl;
	//Rcpp::Rcout << "showing the first " << mL << " rows of location dim" << endl;
	//Rcpp::Rcout << "=======================" << endl;
	//Rcpp::Rcout <<  endl;
	//Rcpp::Rcout << "we have MoveCost(here,there): " << endl;
	//Rcpp::Rcout <<  MoveCost(Range(fromStart,mL),Range(fromStart,mL)) << endl;
	//Rcpp::Rcout << "we have blimit_own(here,there,price): " << endl;
	//Rcpp::Rcout <<  blimit_own(Range(fromStart,mL),Range(fromStart,mL),Range::all()) << endl;
	//Rcpp::Rcout << "we have blimit_buy(there,price): " << endl;
	//Rcpp::Rcout <<  blimit_buy(Range(fromStart,mL),Range::all()) << endl;
	//Rcpp::Rcout <<  endl;
	//Rcpp::Rcout << "showing the first " << ma << " rows of asset dim" << endl;
	//Rcpp::Rcout << "=======================" << endl;
	//Rcpp::Rcout << "ResStay(:,:,1,1,1,1) = " << endl;
	//Rcpp::Rcout << ResStay(Range(fromStart,ma),Range(fromStart,my),1,1,1,1) << endl;
	//Rcpp::Rcout << "ResSell(:,:,1,1,1,1) = " << endl;
	//Rcpp::Rcout << ResSell(Range(fromStart,ma),Range(fromStart,my),1,1,1,1) << endl;
	//Rcpp::Rcout << "ResRent(:,:,1,1,1,1) = " << endl;
	//Rcpp::Rcout << ResRent(Range(fromStart,ma),Range(fromStart,my),1,1,1,1) << endl;
	//Rcpp::Rcout << "ResBuy(:,:,1,1,1,1) = " << endl;
	//Rcpp::Rcout << ResBuy(Range(fromStart,ma),Range(fromStart,my),1,1,1,1) << endl;
	//Rcpp::Rcout << "end of show method: " << endl;
	//Rcpp::Rcout << "===================" << endl;
//}


/*#else   // if you are not in R and must print to stdout*/

// Define show method
void CMig::show(){
	int ma = 10;
	int my = 10;
	int mL = 10;
	ma = min(ma,dim_ayp_here_there_Z_t(0));
	my = min(my,dim_ayp_here_there_Z_t(1));
	mL = min(mL,dim_ayp_here_there_Z_t(3));

	cout << "This is the show method of class " << name << endl;
	cout << "this class has maximally " << dim_ayp_here_there_Z_t.length() << " dimensions." << endl << endl;
	cout << "we have this dimension vector: " << endl;
	cout <<  dim_ayp_here_there_Z_t << endl;
	cout << "we have beta: " << endl;
	cout <<  p.beta << endl;
	cout << "we have myNA: " << endl;
	cout <<  p.myNA << endl;
	cout << "we have G: " << endl;
	cout <<  G << endl;
	cout << "we have Gz: " << endl;
	cout <<  Gz << endl;
	cout << "we have Amenity: " << endl;
	cout <<  Amenity << endl;
	cout << "showing the first " << mL << " rows of location dim" << endl;
	cout << "=======================" << endl;
	cout <<  endl;
	cout << "we have MoveCost(here,there): " << endl;
	cout <<  MoveCost(Range(fromStart,mL),Range(fromStart,mL)) << endl;
	cout <<  endl;
	cout << "showing the first " << ma << " rows of asset dim" << endl;
	cout << "=======================" << endl;
	cout << "ResStay(:,:,1,1,1,1,1) = " << endl;
	cout << ResStay(Range(fromStart,ma),Range(fromStart,my),1,1,1,1,1) << endl;
	cout << "ResSell(:,:,1,1,1,1,1) = " << endl;
	cout << ResSell(Range(fromStart,ma),Range(fromStart,my),1,1,1,1,1) << endl;
	cout << "ResRent(:,:,1,1,1,1,1) = " << endl;
	cout << ResRent(Range(fromStart,ma),Range(fromStart,my),1,1,1,1,1) << endl;
	cout << "ResBuy(:,:,1,1,1,1,1) = " << endl;
	cout << ResBuy(Range(fromStart,ma),Range(fromStart,my),1,1,1,1,1) << endl;
	cout << "end of show method: " << endl;
	cout << "===================" << endl;
}

//#endif // printing conditions


//std::vector<double> CMig::GetResStayNumeric( void ) {
	//Array<double,5>::iterator iter;
	//std::vector<double> out;
	//for (iter = ResStay.begin() ; iter!=ResStay.end();++iter){
		//out.push_back(*iter);
	//}
	//return out;
//}

//std::vector<double> CMig::GetResSellNumeric( void ) {
	//Array<double,5>::iterator iter;
	//std::vector<double> out;
	//for (iter = ResSell.begin() ; iter!=ResSell.end();++iter){
		//out.push_back(*iter);
	//}
	//return out;
//}

//std::vector<double> CMig::GetResRentNumeric( void ) {
	//Array<double,5>::iterator iter;
	//std::vector<double> out;
	//for (iter = ResRent.begin() ; iter!=ResRent.end();++iter){
		//out.push_back(*iter);
	//}
	//return out;
//}

//std::vector<double> CMig::GetResBuyNumeric( void ) {
	//Array<double,5>::iterator iter;
	//std::vector<double> out;
	//for (iter = ResBuy.begin() ; iter!=ResBuy.end();++iter){
		//out.push_back(*iter);
	//}
	//return out;
//}


//// =====================================
//// Computation of period value functions
//// =====================================


/**
 * function computes a hypothetical consumption tensor
 * used for testing only
 */
void CMig::TestCtmpSubset( void ){

	firstIndex   a;
	secondIndex  y;
	thirdIndex   pr;
	fourthIndex  here;
	fifthIndex   there;
	sixthIndex   Z;
	seventhIndex save;
	Range all = Range::all();
	
	restmp   = ResStay(all,all,all,all,all,all,1); // EV(a,y,p,here,there,Z)
	ctmp     = restmp(a,y,pr,here,there,Z) - p.R * save_own(here,there,pr,save);

}

void CMig::TestCtmpSubset_Buy( void ){

	firstIndex   a;
	secondIndex  y;
	thirdIndex   pr;
	fourthIndex  here;
	fifthIndex   there;
	sixthIndex   Z;
	seventhIndex save;
	Range all = Range::all();
	
	restmp   = ResStay(all,all,all,all,all,all,1); // EV(a,y,p,here,there,Z)
	ctmp     = restmp(a,y,pr,here,there,Z) - p.R * save_buy(there,pr,save);

}

// TODO
//void CMig::ComputeStay(int age) {

	//firstIndex   a;
	//secondIndex  y;
	//thirdIndex   pr;
	//fourthIndex  here;
	//fifthIndex   there;
	//sixthIndex   Z;
	//seventhIndex   save;
	//Range all = Range::all();

	//vplustmp = EVown(all,all,all,all,age+1);	// EV(a,y,p,here,age)
	//// get consumption at all states,savings combinations
	
	//// build consumption tensor here. that is c = Res - save
	//// complication: different borrowing constraints
	//restmp   = ResStay(all,all,all,all,all,age); // EV(a,y,p,here,there)

	//// TODO: ALTERNATIVE to this formulation
	//// is to compute the entire tensor for savings once for buyer
	//// owner and renter (a.k.a. seller), and set restricted borrowing
	//// values to +99. They never change over the lifecycle.
	//ctmp     = restmp(a,y,pr,here,there,Z) - p.R * save_own(pr,here,there,save);
	
	//// enforce the borrowing limit for moving owners.
	//LimitOwner() ;
	
	//// taking care of infeasible consumption values here

	//xtmp     = where(ctmp > 0, p.imgamma*( pow(ctmp(a,y,pr,here,there,save),p.mgamma) ) + p.theta + p.beta * vplustmp(save,y,pr,there) - MoveCost(here,there) + Amenity(there), p.myNA);	//vplustmp(save,y,p,there)
	//// get value of being an owner in all locations (here,there)
	//v_loc_stay(all,all,all,all,all,age) = max(xtmp, save);
	//s_loc_stay(all,all,all,all,all,age) = maxIndex(xtmp, save);

	//FindStayCons( age );	
//}



// TODO
//void CMig::ComputeBuy(int age) {

	//firstIndex   a;
	//secondIndex  y;
	//thirdIndex   pr;
	//fourthIndex  here;
	//fifthIndex   there;
	//sixthIndex   save;
	//Range all = Range::all();

	//vplustmp = EVown(all,all,all,all,age+1);	// EV(a,y,p,here,age)
	//// get consumption at all states,savings combinations
	//restmp   = ResBuy(all,all,all,all,all,age);
	
	//ctmp     = restmp(a,y,pr,here,there) - p.R * agrid(save);
	
	//LimitBuyer();
	//xtmp     = where(ctmp > 0, p.imgamma*(pow(ctmp(a,y,pr,here,there,save),p.mgamma)) + p.theta + p.beta * vplustmp(save,y,pr,there) - MoveCost(here,there) + Amenity(there), p.myNA);	//vplustmp(save,y,p,there)
	//// get value of staying
	//v_loc_buy(all,all,all,all,all,age) = max(xtmp, save);
	//s_loc_buy(all,all,all,all,all,age) = maxIndex(xtmp, save);
	//FindBuyCons( age );	
//}



// TODO
//void CMig::ComputeSell(int age) {

	//firstIndex   a;
	//secondIndex  y;
	//thirdIndex   pr;
	//fourthIndex  here;
	//fifthIndex   there;
	//sixthIndex   save;
	//Range all = Range::all();

	//vplustmp = EVrent(all,all,all,all,age+1);	// EV(a,y,p,here,age)
	//restmp   = ResSell(all,all,all,all,all,age);
	
	//ctmp     = restmp(a,y,pr,here,there) - p.R * agrid(save);

	//// no borrowing for seller
	//ctmp(all,all,all,all,all,Range(fromStart,blimit_rent) ) = p.myNA;
	
	//xtmp     = where(ctmp > 0, p.imgamma*(pow(ctmp(a,y,pr,here,there,save),p.mgamma)) +    0    + p.beta * vplustmp(save,y,pr,there) - MoveCost(here,there) + Amenity(there), p.myNA);	//vplustmp(save,y,p,there)
	//// get value of selling at combo (here,there)
	//v_loc_sell(all,all,all,all,all,age) = max(xtmp, save);
	//s_loc_sell(all,all,all,all,all,age) = maxIndex(xtmp, save);
	//FindSellCons( age );	

//}

// TODO
//void CMig::ComputeRent(int age) {

	//firstIndex   a;
	//secondIndex  y;
	//thirdIndex   pr;
	//fourthIndex  here;
	//fifthIndex   there;
	//sixthIndex   save;
	//Range all = Range::all();

	//vplustmp = EVrent(all,all,all,all,age+1);	// EV(a,y,p,here,age)
	//// get consumption at all states,savings combinations
	//restmp   = ResRent(all,all,all,all,all,age);
	
	//ctmp     = restmp(a,y,pr,here,there) - p.R * agrid(save);

	//// no borrowing for renter
	//ctmp(all,all,all,all,all,Range(fromStart,blimit_rent) ) = p.myNA;


	//xtmp     = where(ctmp > 0, p.imgamma*(pow(ctmp(a,y,pr,here,there,save),p.mgamma)) +    0    + p.beta * vplustmp(save,y,pr,there) - MoveCost(here,there) + Amenity(there), p.myNA);	//vplustmp(save,y,p,there)
	//// get value of staying
	//v_loc_rent(all,all,all,all,all,age) = max(xtmp, save);
	//s_loc_rent(all,all,all,all,all,age) = maxIndex(xtmp, save);
	//FindRentCons( age );	

//}




//// Consumption finder functions
//// ============================

// TODO
//void CMig::FindStayCons( int age ){
	//int idx;
	//TinyVector<int,6> ext;
	//ext = s_loc_stay.extent() + 1; // add + 1 here because we have fortran arrays, which are indexed 1,2,...,extent(). So the upper bound of the loop must is extent()+1
	//for (int i1=1;i1<ext(0);++i1){		// a
		//for (int i2=1;i2<ext(1);++i2){	// y
			//for (int i3=1; i3<ext(2); ++i3){	//p
				//for (int i4=1; i4<ext(3); ++i4){	//here
					//for (int i5=1; i5<ext(4); ++i5){	//there

						//idx                            = s_loc_stay(i1,i2,i3,i4,i5,age);	// savings choice at that index
						//c_loc_stay(i1,i2,i3,i4,i5,age) = ctmp(i1,i2,i3,i4,i5,idx);			// consumption at that index
					//}
				//}
			//}
		//}
	//}
//}

// TODO
//void CMig::FindSellCons( int age ){
	//int idx;
	//TinyVector<int,6> ext;
	//ext = s_loc_sell.extent() + 1; // add + 1 here because we have fortran arrays, which are indexed 1,2,...,extent(). So the upper bound of the loop must is extent()+1
	//for (int i1=1;i1<ext(0);++i1){		// a
		//for (int i2=1;i2<ext(1);++i2){	// y
			//for (int i3=1; i3<ext(2); ++i3){	//p
				//for (int i4=1; i4<ext(3); ++i4){	//here
					//for (int i5=1; i5<ext(4); ++i5){	//there

						//idx                            = s_loc_sell(i1,i2,i3,i4,i5,age);	// savings choice at that index
						//c_loc_sell(i1,i2,i3,i4,i5,age) = ctmp(i1,i2,i3,i4,i5,idx);			// consumption at that index
					//}
				//}
			//}
		//}
	//}
//}

// TODO
//void CMig::FindRentCons( int age ){
	//int idx;
	//TinyVector<int,6> ext;
	//ext = s_loc_rent.extent() + 1; // add + 1 here because we have fortran arrays, which are indexed 1,2,...,extent(). So the upper bound of the loop must is extent()+1
	//for (int i1=1;i1<ext(0);++i1){		// a
		//for (int i2=1;i2<ext(1);++i2){	// y
			//for (int i3=1; i3<ext(2); ++i3){	//p
				//for (int i4=1; i4<ext(3); ++i4){	//here
					//for (int i5=1; i5<ext(4); ++i5){	//there

						//idx                            = s_loc_rent(i1,i2,i3,i4,i5,age);	// savings choice at that index
						//c_loc_rent(i1,i2,i3,i4,i5,age) = ctmp(i1,i2,i3,i4,i5,idx);			// consumption at that index
					//}
				//}
			//}
		//}
	//}
//}


// TODO
//void CMig::FindBuyCons( int age ){
	//int idx;
	//TinyVector<int,6> ext;
	//ext = s_loc_buy.extent() + 1; // add + 1 here because we have fortran arrays, which are indexed 1,2,...,extent(). So the upper bound of the loop must is extent()+1
	//for (int i1=1;i1<ext(0);++i1){		// a
		//for (int i2=1;i2<ext(1);++i2){	// y
			//for (int i3=1; i3<ext(2); ++i3){	//p
				//for (int i4=1; i4<ext(3); ++i4){	//here
					//for (int i5=1; i5<ext(4); ++i5){	//there

						//idx                            = s_loc_buy(i1,i2,i3,i4,i5,age);	// savings choice at that index
						//c_loc_buy(i1,i2,i3,i4,i5,age) = ctmp(i1,i2,i3,i4,i5,idx);			// consumption at that index
					//}
				//}
			//}
		//}
	//}
//}


// TODO
//void CMig::ComputePeriod(int age){

	//Range all = Range::all();
	
	//if (verbose>0){
		//cout << "age is " << age << std::endl;
	//}

	//// if final operiod, then preComputed resources are utility
	//// unfortunately TinyVector dim_ayp_here_t only available as C++ array, so different indexing for those.
	//if (age==maxage) {
		//// EV(a,y,p,here,age)
		//EVown( all,all,all,all,age) = ResStay(all,all,all,all,1,age);	// get final utility precomputed from resources
		//EVrent(all,all,all,all,age) = ResRent(all,all,all,all,1,age);

	//} else {

		//ComputeStay(age);		
		//ComputeSell(age);		
		//ComputeRent(age);		
		//ComputeBuy(age);		
		//ComputeTenureChoice(age);		 
		//ComputeLocationChoice(age);		 
		//ComputeExpectations(age);	// get EVown and EVrent
	
	//}

//}


// TODO
/** Binary Discrete Choice Function for Arrays
 * @param blitz::Array<double,5> one
 * @param blitz::Array<double,5> two
 *
 * @return blitz::Array<double,5> that has values max(one,two) index by index
 */
//Array<double,5> CMig::dchoice5d(Array<double,5> one, Array<double,5> two){

	//Array<double,5> ret(nAsset,nIncome,nPrice,nLoc,nLoc,FortranArray<5>());

	//ret = where(one > two, one, two);

	//return(ret);
//}

// TODO
/** Binary Discrete Choice Indicator for Arrays
 * @param blitz::Array<double,5> one
 * @param blitz::Array<double,5> two
 *
 * @return blitz::Array<double,5> that the indicator of which array element is larger (i.e. 1 or 2) index by index
 */
//Array<int,5> CMig::dchoiceID5d(Array<double,5> one, Array<double,5> two){

	//Array<int,5> ret(nAsset,nIncome,nPrice,nLoc,nLoc,FortranArray<5>());

	//ret = where(one > two, 1, 2);

	//return(ret);
//}


// TODO
/** Location Choice given everything else
 * Given values conditional on location (
 * i.e. having maxed over tenure at each location)
 * compute the upper envelope of all location-specific
 * value funcitons and their indicators
 * NOTE: if I do logit shocks, this function is 
 * irrelevant for the model solution, as Vown and Vrent
 * are not used anywhere. the Vbar formulation gets
 * around that. This should be used in SIMULATION,
 * where it depends on a parameter \sigma which is the
 * variance of the logit shock. 
 */
//void CMig::ComputeLocationChoice( int age ){
	//// source format: (a,y,p,here,there,age)
	//// target format: (a,y,p,here,age)
   
	//fifthIndex   there;	
	//Range all = Range::all();

	//// v_loc_tmp is (a,y,p,here,there,age)!!
	//v_loc_tmp = W_loc_own(all,all,all,all,all,age);

	//// NOTE: simulation. when using this in simulation, here add
	//// a vector \xi of dimension (there,1) to each slice of W_loc_own
	//// in the "there" dimension before taking the discrete choice.
	//Vown(        all,all,all,all,age) = max(      v_loc_tmp, there );
	//Location_own(all,all,all,all,age) = maxIndex( v_loc_tmp, there );

	//v_loc_tmp = W_loc_rent(all,all,all,all,all,age);
	//Vrent(        all,all,all,all,age) = max(      v_loc_tmp, there );
	//Location_rent(all,all,all,all,age) = maxIndex( v_loc_tmp, there );
//}




// TODO
/** Tenure Choice at each (here,there) combo
 * choose between (stay,sell) for owner and (rent,buy) for renter
 * states.
 * @return nothing but alter members W_loc_own and W_loc_rent
 */
//void CMig::ComputeTenureChoice( int age ){

	//// (a,y,p,here,there,age)
	//Range all = Range::all();

	//W_loc_own(  all,all,all,all,all,age)  = dchoice5d(  v_loc_stay(all,all,all,all,all,age), v_loc_sell(all,all,all,all,all,age));
	//W_loc_rent( all,all,all,all,all,age)  = dchoice5d(  v_loc_rent(all,all,all,all,all,age), v_loc_buy( all,all,all,all,all,age));

	//Tenure_loc_own(  all,all,all,all,all,age)  = dchoiceID5d(  v_loc_stay(all,all,all,all,all,age), v_loc_sell(all,all,all,all,all,age));
	//Tenure_loc_rent( all,all,all,all,all,age)  = dchoiceID5d(  v_loc_rent(all,all,all,all,all,age), v_loc_buy( all,all,all,all,all,age));

//}


// TODO
/** Expectations and Conditional Choice Probabilities
 * compute vbar, EV and rho
 */
//void CMig::ComputeExpectations( int age ){

	//Range all = Range::all();
	//firstIndex   a;
	//secondIndex  y;
	//thirdIndex   pr;
	//fourthIndex  here;
	//fifthIndex   there;
	//// compute Vbar first
	
	//// owner
	//// =====
	
	//v_loc_tmp = W_loc_own(all,all,all,all,all,age);  // a,y,p,here,there
	//Vbar_own( all,all,all,all,age) = euler_mascheroni + log( sum( exp( v_loc_tmp ), there ) ) ; 

	//// compute CCP aka rho
	//vbar_tmp = Vbar_own(all,all,all,all,age);  // a,y,p,here
	//rho_own( all,all,all,all,all,age) = exp( euler_mascheroni + v_loc_tmp(a,y,pr,here,there) - vbar_tmp(a,y,pr,here) );
	

	////renter  
	//// =====
	
	//v_loc_tmp = W_loc_rent(all,all,all,all,all,age);  // a,y,p,here,there
	//Vbar_rent( all,all,all,all,age) = euler_mascheroni + log( sum( exp( v_loc_tmp ), there ) ) ; 

	//// compute CCP aka rho
	//vbar_tmp = Vbar_rent(all,all,all,all,age);  // a,y,p,here
	//rho_rent( all,all,all,all,all,age) = exp( euler_mascheroni + v_loc_tmp(a,y,pr,here,there) - vbar_tmp(a,y,pr,here) );

	//// then integrate over Vbar to account for laws of motion of y and p.
	
// first here must interpolate into vtilde
//
	//vtilde = interpolate(Vbar_own( all,all,all,all,all,age));
	////EVown( all,all,all,all,age) = integrate(vtilde);
	//vtilde = interpolate(Vbar_rent( all,all,all,all,all,age));
	//EVrent(all,all,all,all,age) = integrate(vtilde);

//}


// TODO
/**
 * tens is defined on the period price grid that spans
 * all possible price values
 * must evaluate it from perspective of period (t-1), where
 * each price level p(t-1) has nZ possible future values.
 * need to interpolate vbar and evaluate at those vales.
 * could declare void once tested.
 */
/*Array<double,5> CMig::interpolate(Array<double,5> tens, int age) {*/

	//Range all = Range::all();
	//Array<double,5> ret(nAsset,nIncome,nPrice,nLoc,nAgg,FortranArray<5>());

	//for (int ih=1; ih<nLoc+1; ih++){
		//pgrid = GpEval(ih,age,all);		// this is the current period price grid, invariant with Z
		//for (int ia=1;ia<nAsset+1; ia++){
			//for (int iy=1;iy<nIncome+1; iy++){
				//for (int iz=1; iz<nAgg+1; iz++){

					//tmp1  = tens(ia,iy,all,ih,iz);	// Array<double,1> tmp1(nP,FortranArray<1>());
					//// set the spline on this data
					//gsl_spline_init(p->spline, pgrid.data(), tmp1.data(), nPrice );

					//// find the value at each integration node.
					//for (int ip=1; ip<nPrice+1; ip++) ret(ia,iy,ip,ih,iz) = gsp_spline_eval(p->spline,GpInt(ih,iz,age,ip)) ;
					////GpInt is the grid of integration nodes from a (t-1) perspective. there are nPrice such nodes
					////for each aggregate state Z.
				//}
			//}
		//}
	//}
	//return( ret );

/*}*/



Array<double,5> CMig::integrate(Array<double,5> tens){


	firstIndex   a;	
	secondIndex  y; 
	thirdIndex   pr;	
	fourthIndex  here;	
	fifthIndex   Z;	
	sixthIndex   yp;	
	seventhIndex   Zp;	
	
	Array<double,5> ret(nAsset,nIncome,nPrice,nLoc,nAgg,FortranArray<5>());

	// this is my version:
	//
	//Array<double,6> tmpyp(dim_ayp_here_yp,FortranArray<6>());	// tmp(i1,i2,i3,i4,i5,i6)
	//Array<double,5> tmpy(dim_ayp_here_y,FortranArray<5>());	// tmp(i1,i2,i3,i4,i5)
	
	//tmpy = tens(i1,i2,i3,i4) * G(i5,i2);
	//tmpyp = tmpy(i1,i2,i3,i4,i5) * Gp(i6,i3);
	
	//tmpy = sum( tmpyp(i1,i2,i6,i4,i5,i3), i6 ) ;// integrate out p'

	//ret = sum( tmpy(i1,i5,i3,i4,i2), i5);	// integrate out y
		  
	//ret = sum( sum(   tens(i1,i5,i6,i4) * G(i2,i5) * Gp(i3,i6) , i6) ,i5);
	//
	//
	//and this is tibo's version:
	//
	ret = sum( sum(   tens(a,yp,pr,here,Zp) * G(y,yp) * Gz(Z,Zp) , Zp) ,yp );
	// uncomment to have a failing test
	//ret = sum( sum(   tens(a,yp,pr,here,Zp) * G(y,yp) * Gz(Zp,Z) , Zp) ,yp );

	if (verbose>1) {
		cout << endl;
		cout << "in integrate now. ret is :" << endl;
		cout << ret << endl;
	}
	//tmp = tens(i1,i2,i3,i4) * G(i5,i2);
	//ret = sum( tmp(i1,i5,i3,i4,i2), i5);
	return(ret);

}

   
   



















