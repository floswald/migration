


#ifdef RcppCompile
#include "../../cpp/src/CMig6.h"
#else
#include "CMig6.h"		// include class declaration of owner class
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

CMig6::CMig6() :

	// this is syntactic sugar. an initialiser list. 

	// Arrays of size (a,y,p,here,there,age)
	ResStay(   2,2,2,2,2,2,FortranArray<6>()),
    ResSell(   2,2,2,2,2,2,FortranArray<6>()),
	ResRent(   2,2,2,2,2,2,FortranArray<6>()),
    ResBuy(    2,2,2,2,2,2,FortranArray<6>()),

    v_loc_stay(2,2,2,2,2,2,FortranArray<6>()),	
    v_loc_sell(2,2,2,2,2,2,FortranArray<6>()),  
    v_loc_rent(2,2,2,2,2,2,FortranArray<6>()),	
    v_loc_buy( 2,2,2,2,2,2,FortranArray<6>()),   
	c_loc_stay(2,2,2,2,2,2,FortranArray<6>()),	
	c_loc_sell(2,2,2,2,2,2,FortranArray<6>()),  
	c_loc_rent(2,2,2,2,2,2,FortranArray<6>()),	
	c_loc_buy( 2,2,2,2,2,2,FortranArray<6>()),   
	s_loc_stay(2,2,2,2,2,2,FortranArray<6>()),  
	s_loc_sell(2,2,2,2,2,2,FortranArray<6>()), 
	s_loc_rent(2,2,2,2,2,2,FortranArray<6>()), 
	s_loc_buy( 2,2,2,2,2,2,FortranArray<6>()), 
	
	W_loc_rent(     2,2,2,2,2,2,FortranArray<6>()), 
	W_loc_own(      2,2,2,2,2,2,FortranArray<6>()), 
	rho_rent(       2,2,2,2,2,2,FortranArray<6>()), 
	rho_own(        2,2,2,2,2,2,FortranArray<6>()), 
	Tenure_loc_rent(2,2,2,2,2,2,FortranArray<6>()), 
	Tenure_loc_own( 2,2,2,2,2,2,FortranArray<6>()), 

	// Arrays of size (a,y,p,here,there,a)
	ctmp(      2,2,2,2,2,2,FortranArray<6>()),  	
	xtmp(      2,2,2,2,2,2,FortranArray<6>()),  	
	
	// Arrays of size (a,y,p,here,there)
	restmp(    2,2,2,2,2,   FortranArray<5>()),  	

	// Arrays of size (a,y,p,here,age)
	Vown(      2,2,2,2,2,   FortranArray<5>()),
	Vrent(     2,2,2,2,2,   FortranArray<5>()),
	EVown(     2,2,2,2,2,   FortranArray<5>()),
	EVrent(    2,2,2,2,2,   FortranArray<5>()),
	Vbar_own(     2,2,2,2,2,   FortranArray<5>()),
	Vbar_rent(    2,2,2,2,2,   FortranArray<5>()),
	Location_own(      2,2,2,2,2,   FortranArray<5>()),
	Location_rent(     2,2,2,2,2,   FortranArray<5>()),

	v_loc_tmp( 2,2,2,2,2,   FortranArray<5>()),	
	vbar_tmp(  2,2,2,2,   FortranArray<4>()),	

	
	vplustmp(  2,2,2,2,      FortranArray<4>()), 	// (a,y,p,here)
	blimit_own(2,2,2,         FortranArray<3>()) ,	// (here,there,p)
	blimit_buy(2,2,            FortranArray<2>()) ,	// (there,p)
	MoveCost(  2,2,            FortranArray<2>()) ,	// (here,there)
	G(         2,2,            FortranArray<2>()) ,	// (y,y')
	Gp(        2,2,            FortranArray<2>()) ,	// (p,p')
	agrid(     2,               FortranArray<1>()),	
	Amenity(   2,               FortranArray<1>()) ,

	blimit_rent(1),
	verbose(1),
	nLoc(2),
	nPrice(2),
	nIncome(2),
	nAsset(2),
	name("CMig6_default"){	

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
		W_loc_rent = 0;
		W_loc_own  = 0;
		rho_rent = 0;
		rho_own  = 0;
		Tenure_loc_rent = 0;
		Tenure_loc_own  = 0;
		Location_own    = 0;
		Location_rent    = 0;
		// get the dimension of the problem
		dim_ayp_here_there_t = ResStay.extent();
		G          = 1.0/dim_ayp_here_there_t(1);
		Gp         = 1.0/dim_ayp_here_there_t(2);
		MoveCost   = 1.0/nLoc;
	    Amenity    = 2;
		agrid = 1;
		// set parameter values
		p.myNA     = -99;
		p.beta     = 0.9;
		p.gamma    = 1.4;
		p.mgamma   = -0.4;
		p.imgamma  = -1/0.4;
		p.theta    = 0.1;
		p.R        = 1/(1+0.4);
		maxage = dim_ayp_here_there_t(5);
		// set borrowing limits
		blimit_own = 2;
		blimit_buy = 1;
		blimit_rent = 1;
}

// constructor 1
CMig6::CMig6(int nA, int nY, int nP, int nL, int nT):

	// this is syntactic sugar. an initialiser list. 

	// Arrays of size (a,y,p,here,there,age)
	ResStay(   nA,nY,nP,nL,nL,nT,FortranArray<6>()),
    ResSell(   nA,nY,nP,nL,nL,nT,FortranArray<6>()),
	ResRent(   nA,nY,nP,nL,nL,nT,FortranArray<6>()),
    ResBuy(    nA,nY,nP,nL,nL,nT,FortranArray<6>()),

    v_loc_stay(nA,nY,nP,nL,nL,nT,FortranArray<6>()),	
    v_loc_sell(nA,nY,nP,nL,nL,nT,FortranArray<6>()),  
    v_loc_rent(nA,nY,nP,nL,nL,nT,FortranArray<6>()),	
    v_loc_buy( nA,nY,nP,nL,nL,nT,FortranArray<6>()),   
	c_loc_stay(nA,nY,nP,nL,nL,nT,FortranArray<6>()),	
	c_loc_sell(nA,nY,nP,nL,nL,nT,FortranArray<6>()),  
	c_loc_rent(nA,nY,nP,nL,nL,nT,FortranArray<6>()),	
	c_loc_buy( nA,nY,nP,nL,nL,nT,FortranArray<6>()),   
	s_loc_stay(nA,nY,nP,nL,nL,nT,FortranArray<6>()),  
	s_loc_sell(nA,nY,nP,nL,nL,nT,FortranArray<6>()), 
	s_loc_rent(nA,nY,nP,nL,nL,nT,FortranArray<6>()), 
	s_loc_buy( nA,nY,nP,nL,nL,nT,FortranArray<6>()), 
	
	W_loc_rent(     nA,nY,nP,nL,nL,nT,FortranArray<6>()), 
	W_loc_own(      nA,nY,nP,nL,nL,nT,FortranArray<6>()), 
	rho_rent(     nA,nY,nP,nL,nL,nT,FortranArray<6>()), 
	rho_own(      nA,nY,nP,nL,nL,nT,FortranArray<6>()), 
	Tenure_loc_rent(nA,nY,nP,nL,nL,nT,FortranArray<6>()), 
	Tenure_loc_own( nA,nY,nP,nL,nL,nT,FortranArray<6>()), 

	// Arrays of size (a,y,p,here,there,a)
	ctmp(      nA,nY,nP,nL,nL,nA,FortranArray<6>()),  	
	xtmp(      nA,nY,nP,nL,nL,nA,FortranArray<6>()),  	
	
	// Arrays of size (a,y,p,here,there)
	restmp(    nA,nY,nP,nL,nL,   FortranArray<5>()),  	

	// Arrays of size (a,y,p,here,age)
	Vown(  	   nA,nY,nP,nL,nT,   FortranArray<5>()),
	Vrent( 	   nA,nY,nP,nL,nT,   FortranArray<5>()),
	EVown( 	   nA,nY,nP,nL,nT,   FortranArray<5>()),
	EVrent(	   nA,nY,nP,nL,nT,   FortranArray<5>()),
	Vbar_own( 	   nA,nY,nP,nL,nT,   FortranArray<5>()),
	Vbar_rent(	   nA,nY,nP,nL,nT,   FortranArray<5>()),
	Location_own( nA,nY,nP,nL,nT,   FortranArray<5>()),
	Location_rent(nA,nY,nP,nL,nT,   FortranArray<5>()),

	v_loc_tmp( nA,nY,nP,nL,nL,   FortranArray<5>()),
	vbar_tmp( nA,nY,nP,nL,       FortranArray<4>()),

	vplustmp(  nA,nY,nP,nL,      FortranArray<4>()), 	// (a,y,p,here)
	blimit_own(nL,nL,nP,         FortranArray<3>()) ,	// (here,there,p)
	blimit_buy(nL,nP,            FortranArray<2>()) ,	// (there,p)
	MoveCost(  nL,nL,            FortranArray<2>()) ,	// (here,there)
	G(         nY,nY,            FortranArray<2>()) ,	// (y,y')
	Gp(        nP,nP,            FortranArray<2>()) ,	// (p,p')
	agrid(     nA,               FortranArray<1>()),	
	Amenity(   nL,               FortranArray<1>()) ,

	blimit_rent(1),
	verbose(1),
	nLoc(nL),
	nPrice(nP),
	nIncome(nY),
	nAsset(nA),
	name("CMig6_dims_given"){	

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
		W_loc_rent = 0;
		W_loc_own  = 0;
		rho_rent = 0;
		rho_own  = 0;
		Tenure_loc_rent = 0;
		Tenure_loc_own  = 0;
		Location_own    = 0;
		Location_own    = 0;
		// get the dimension of the problem
		dim_ayp_here_there_t = ResStay.extent();
		G          = 1.0/dim_ayp_here_there_t(1);
		Gp         = 1.0/dim_ayp_here_there_t(2);
		MoveCost   = 1.0/nLoc;
	    Amenity    = 2;
		agrid = 1;
		// set parameter values
		p.myNA     = -99;
		p.beta     = 0.9;
		p.gamma    = 1.4;
		p.mgamma   = -0.4;
		p.imgamma  = -1/0.4;
		p.theta    = 0.1;
		p.R        = 1/(1+0.4);
		maxage = dim_ayp_here_there_t(5);
		// set borrowing limits
		blimit_own = 2;
		blimit_buy = 1;
		blimit_rent = 1;
}

CMig6::CMig6(int nA, int nY, int nP, int nL, int nT,
	   	     PStruct * data_pars,
	   	     Array<double,6> data_stay,
	   	     Array<double,6> data_sell,
	   	     Array<double,6> data_rent,
	   	     Array<double,6> data_buy,
	   	     Array<double,2> data_G,	
	   	     Array<double,2> data_Gp,	
	   	     Array<double,2> data_MoveCost,
	   	     Array<double,1> data_Amenity,
	   	     Array<double,1> data_agrid,
	   	     Array<int   ,3> data_blimit_own,
	   	     Array<int   ,2> data_blimit_buy,
			 int data_blimit_rent,
			 int data_verbose)  :              

	// this is syntactic sugar. an initialiser list. 

	// Arrays of size (a,y,p,here,there,age)
	ResStay(   nA,nY,nP,nL,nL,nT,FortranArray<6>()),
    ResSell(   nA,nY,nP,nL,nL,nT,FortranArray<6>()),
	ResRent(   nA,nY,nP,nL,nL,nT,FortranArray<6>()),
    ResBuy(    nA,nY,nP,nL,nL,nT,FortranArray<6>()),

    v_loc_stay(nA,nY,nP,nL,nL,nT,FortranArray<6>()),	
    v_loc_sell(nA,nY,nP,nL,nL,nT,FortranArray<6>()),  
    v_loc_rent(nA,nY,nP,nL,nL,nT,FortranArray<6>()),	
    v_loc_buy( nA,nY,nP,nL,nL,nT,FortranArray<6>()),   
	c_loc_stay(nA,nY,nP,nL,nL,nT,FortranArray<6>()),	
	c_loc_sell(nA,nY,nP,nL,nL,nT,FortranArray<6>()),  
	c_loc_rent(nA,nY,nP,nL,nL,nT,FortranArray<6>()),	
	c_loc_buy( nA,nY,nP,nL,nL,nT,FortranArray<6>()),   
	s_loc_stay(nA,nY,nP,nL,nL,nT,FortranArray<6>()),  
	s_loc_sell(nA,nY,nP,nL,nL,nT,FortranArray<6>()), 
	s_loc_rent(nA,nY,nP,nL,nL,nT,FortranArray<6>()), 
	s_loc_buy( nA,nY,nP,nL,nL,nT,FortranArray<6>()), 
	
	W_loc_rent(     nA,nY,nP,nL,nL,nT,FortranArray<6>()), 
	W_loc_own(      nA,nY,nP,nL,nL,nT,FortranArray<6>()), 
	rho_rent(     nA,nY,nP,nL,nL,nT,FortranArray<6>()), 
	rho_own(      nA,nY,nP,nL,nL,nT,FortranArray<6>()), 
	Tenure_loc_rent(nA,nY,nP,nL,nL,nT,FortranArray<6>()), 
	Tenure_loc_own( nA,nY,nP,nL,nL,nT,FortranArray<6>()), 

	// Arrays of size (a,y,p,here,there,a)
	ctmp(      nA,nY,nP,nL,nL,nA,FortranArray<6>()),  	
	xtmp(      nA,nY,nP,nL,nL,nA,FortranArray<6>()),  	
	
	// Arrays of size (a,y,p,here,there)
	restmp(    nA,nY,nP,nL,nL,   FortranArray<5>()),  	

	// Arrays of size (a,y,p,here,age)
	Vown(  	   nA,nY,nP,nL,nT,   FortranArray<5>()),
	Vrent( 	   nA,nY,nP,nL,nT,   FortranArray<5>()),
	EVown( 	   nA,nY,nP,nL,nT,   FortranArray<5>()),
	EVrent(	   nA,nY,nP,nL,nT,   FortranArray<5>()),
	Vbar_own(  nA,nY,nP,nL,nT,   FortranArray<5>()),
	Vbar_rent( nA,nY,nP,nL,nT,   FortranArray<5>()),
	Location_own( nA,nY,nP,nL,nT,   FortranArray<5>()),
	Location_rent(nA,nY,nP,nL,nT,   FortranArray<5>()),

	v_loc_tmp( nA,nY,nP,nL,nL,   FortranArray<5>()),
	vbar_tmp(  nA,nY,nP,nL,      FortranArray<4>()),
	
	vplustmp(  nA,nY,nP,nL,      FortranArray<4>()), 	// (a,y,p,here)
	blimit_own(nL,nL,nP,         FortranArray<3>()) ,	// (here,there,p)
	blimit_buy(nL,nP,            FortranArray<2>()) ,	// (there,p)
	MoveCost(  nL,nL,            FortranArray<2>()) ,	// (here,there)
	G(         nY,nY,            FortranArray<2>()) ,	// (y,y')
	Gp(        nP,nP,            FortranArray<2>()) ,	// (p,p')
	agrid(     nA,               FortranArray<1>()),	
	Amenity(   nL,               FortranArray<1>()) ,

	blimit_rent(data_blimit_rent),
	verbose(data_verbose),
	nLoc(nL),
	nPrice(nP),
	nIncome(nY),
	nAsset(nA),
	name("CMig6_data_referenced"),	
	p(*data_pars) {						  

		// set array values
		ResStay.reference(data_stay);
		ResSell.reference(data_sell);
		ResRent.reference(data_rent);
		ResBuy.reference(data_buy );
		G.reference(data_G);
		Gp.reference(data_Gp);
		MoveCost.reference(data_MoveCost);
		blimit_own.reference(data_blimit_own);
		blimit_buy.reference(data_blimit_buy);
		Amenity.reference(data_Amenity);
		agrid.reference(data_agrid);
		W_loc_rent = 0;
		W_loc_own  = 0;
		rho_rent = 0;
		rho_own  = 0;
		Vbar_rent = 0;
		Vbar_own  = 0;
		Tenure_loc_rent = 0;
		Tenure_loc_own  = 0;
		Location_own    = 0;
		Location_rent    = 0;
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
		dim_ayp_here_there_t = ResStay.extent();
		maxage = dim_ayp_here_there_t(5);
}


// Define Getters




#ifdef RcppCompile   // conditions for printing the show method: if you are working from R, print with Rcpp::Rcout

//// Define show method
//// TODO ask how to compile with R libraries
void CMig6::show(){
	int ma = 10;
	int my = 10;
	int mL = 10;
	ma = min(ma,dim_ayp_here_there_t(0));
	my = min(my,dim_ayp_here_there_t(1));
	mL = min(mL,dim_ayp_here_there_t(3));

	Rcpp::Rcout << "This is the show method of class " << name << endl;
	Rcpp::Rcout << "we have this dimension vector: " << endl;
	Rcpp::Rcout <<  dim_ayp_here_there_t << endl;
	Rcpp::Rcout << "we have beta: " << endl;
	Rcpp::Rcout <<  p.beta << endl;
	Rcpp::Rcout << "we have myNA: " << endl;
	Rcpp::Rcout <<  p.myNA << endl;
	Rcpp::Rcout << "we have G: " << endl;
	Rcpp::Rcout <<  G << endl;
	Rcpp::Rcout << "we have Gp: " << endl;
	Rcpp::Rcout <<  Gp << endl;
	Rcpp::Rcout << "we have Amenity: " << endl;
	Rcpp::Rcout <<  Amenity << endl;
	Rcpp::Rcout << "showing the first " << mL << " rows of location dim" << endl;
	Rcpp::Rcout << "=======================" << endl;
	Rcpp::Rcout <<  endl;
	Rcpp::Rcout << "we have MoveCost(here,there): " << endl;
	Rcpp::Rcout <<  MoveCost(Range(fromStart,mL),Range(fromStart,mL)) << endl;
	Rcpp::Rcout << "we have blimit_own(here,there,price): " << endl;
	Rcpp::Rcout <<  blimit_own(Range(fromStart,mL),Range(fromStart,mL),Range::all()) << endl;
	Rcpp::Rcout << "we have blimit_buy(there,price): " << endl;
	Rcpp::Rcout <<  blimit_buy(Range(fromStart,mL),Range::all()) << endl;
	Rcpp::Rcout <<  endl;
	Rcpp::Rcout << "showing the first " << ma << " rows of asset dim" << endl;
	Rcpp::Rcout << "=======================" << endl;
	Rcpp::Rcout << "ResStay(:,:,1,1,1,1) = " << endl;
	Rcpp::Rcout << ResStay(Range(fromStart,ma),Range(fromStart,my),1,1,1,1) << endl;
	Rcpp::Rcout << "ResSell(:,:,1,1,1,1) = " << endl;
	Rcpp::Rcout << ResSell(Range(fromStart,ma),Range(fromStart,my),1,1,1,1) << endl;
	Rcpp::Rcout << "ResRent(:,:,1,1,1,1) = " << endl;
	Rcpp::Rcout << ResRent(Range(fromStart,ma),Range(fromStart,my),1,1,1,1) << endl;
	Rcpp::Rcout << "ResBuy(:,:,1,1,1,1) = " << endl;
	Rcpp::Rcout << ResBuy(Range(fromStart,ma),Range(fromStart,my),1,1,1,1) << endl;
	Rcpp::Rcout << "end of show method: " << endl;
	Rcpp::Rcout << "===================" << endl;
}


#else   // if you are not in R and must print to stdout

// Define show method
void CMig6::show(){
	int ma = 10;
	int my = 10;
	int mL = 10;
	ma = min(ma,dim_ayp_here_there_t(0));
	my = min(my,dim_ayp_here_there_t(1));
	mL = min(mL,dim_ayp_here_there_t(3));

	cout << "This is the show method of class " << name << endl;
	cout << "we have this dimension vector: " << endl;
	cout <<  dim_ayp_here_there_t << endl;
	cout << "we have beta: " << endl;
	cout <<  p.beta << endl;
	cout << "we have myNA: " << endl;
	cout <<  p.myNA << endl;
	cout << "we have G: " << endl;
	cout <<  G << endl;
	cout << "we have Gp: " << endl;
	cout <<  Gp << endl;
	cout << "we have Amenity: " << endl;
	cout <<  Amenity << endl;
	cout << "showing the first " << mL << " rows of location dim" << endl;
	cout << "=======================" << endl;
	cout <<  endl;
	cout << "we have MoveCost(here,there): " << endl;
	cout <<  MoveCost(Range(fromStart,mL),Range(fromStart,mL)) << endl;
	cout << "we have blimit_own(here,there,price): " << endl;
	cout <<  blimit_own(Range(fromStart,mL),Range(fromStart,mL),Range::all()) << endl;
	cout << "we have blimit_buy(there,price): " << endl;
	cout <<  blimit_buy(Range(fromStart,mL),Range::all()) << endl;
	cout <<  endl;
	cout << "showing the first " << ma << " rows of asset dim" << endl;
	cout << "=======================" << endl;
	cout << "ResStay(:,:,1,1,1,1) = " << endl;
	cout << ResStay(Range(fromStart,ma),Range(fromStart,my),1,1,1,1) << endl;
	cout << "ResSell(:,:,1,1,1,1) = " << endl;
	cout << ResSell(Range(fromStart,ma),Range(fromStart,my),1,1,1,1) << endl;
	cout << "ResRent(:,:,1,1,1,1) = " << endl;
	cout << ResRent(Range(fromStart,ma),Range(fromStart,my),1,1,1,1) << endl;
	cout << "ResBuy(:,:,1,1,1,1) = " << endl;
	cout << ResBuy(Range(fromStart,ma),Range(fromStart,my),1,1,1,1) << endl;
	cout << "end of show method: " << endl;
	cout << "===================" << endl;
}

#endif // printing conditions


//std::vector<double> CMig6::GetResStayNumeric( void ) {
	//Array<double,5>::iterator iter;
	//std::vector<double> out;
	//for (iter = ResStay.begin() ; iter!=ResStay.end();++iter){
		//out.push_back(*iter);
	//}
	//return out;
//}

//std::vector<double> CMig6::GetResSellNumeric( void ) {
	//Array<double,5>::iterator iter;
	//std::vector<double> out;
	//for (iter = ResSell.begin() ; iter!=ResSell.end();++iter){
		//out.push_back(*iter);
	//}
	//return out;
//}

//std::vector<double> CMig6::GetResRentNumeric( void ) {
	//Array<double,5>::iterator iter;
	//std::vector<double> out;
	//for (iter = ResRent.begin() ; iter!=ResRent.end();++iter){
		//out.push_back(*iter);
	//}
	//return out;
//}

//std::vector<double> CMig6::GetResBuyNumeric( void ) {
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



void CMig6::LimitOwner( void ) {

	for (int ihere=1;ihere<nLoc+1; ihere++){

		for (int ithere=1;ithere<nLoc+1; ithere++){

			if (ihere != ithere ){
				// enforce the borrowing limit: here's an owner who is moving to buy.

				for (int ip=1; ip<nPrice+1; ip++) {
					
					// if there was a limit specified for this combination (i.e. pos index)
					// set all consumption indices up to THAT one to NA
					if (blimit_own(ihere,ithere,ip) > 0) ctmp(Range::all(),Range::all(),ip,ihere,ithere,Range(fromStart,blimit_own(ihere,ithere,ip)) ) = p.myNA;
				}
			}
		}
	}
}

void CMig6::LimitBuyer( void ) {

	for (int ithere=1;ithere<nLoc+1; ithere++){

		for (int ip=1; ip<nPrice+1; ip++) {
			
			// if there was a limit specified for this combination (i.e. pos index)
			// set all consumption indices up to THAT one to NA
			if (blimit_buy(ithere,ip) > 0) ctmp(Range::all(),Range::all(),ip,Range::all(),ithere,Range(fromStart,blimit_buy(ithere,ip)) ) = p.myNA;
		}
	}
}



void CMig6::ComputeStay(int age) {

	firstIndex   a;
	secondIndex  y;
	thirdIndex   pr;
	fourthIndex  here;
	fifthIndex   there;
	sixthIndex   save;
	Range all = Range::all();

	vplustmp = EVown(all,all,all,all,age+1);	// EV(a,y,p,here,age)
	// get consumption at all states,savings combinations
	
	// build consumption tensor here. that is c = Res - save
	// complication: different borrowing constraints
	restmp   = ResStay(all,all,all,all,all,age); // EV(a,y,p,here,there)

	// TODO: ALTERNATIVE to this formulation
	// is to compute the entire tensor for savings once for buyer
	// owner and renter (a.k.a. seller), and set restricted borrowing
	// values to +99. They never change over the lifecycle.
	ctmp     = restmp(a,y,pr,here,there) - p.R * agrid(save);
	
	// enforce the borrowing limit for moving owners.
	LimitOwner() ;
	
	// taking care of infeasible consumption values here

	xtmp     = where(ctmp > 0, p.imgamma*( pow(ctmp(a,y,pr,here,there,save),p.mgamma) ) + p.theta + p.beta * vplustmp(save,y,pr,there) - MoveCost(here,there) + Amenity(there), p.myNA);	//vplustmp(save,y,p,there)
	// get value of being an owner in all locations (here,there)
	v_loc_stay(all,all,all,all,all,age) = max(xtmp, save);
	s_loc_stay(all,all,all,all,all,age) = maxIndex(xtmp, save);

	FindStayCons( age );	
}



void CMig6::ComputeBuy(int age) {

	firstIndex   a;
	secondIndex  y;
	thirdIndex   pr;
	fourthIndex  here;
	fifthIndex   there;
	sixthIndex   save;
	Range all = Range::all();

	vplustmp = EVown(all,all,all,all,age+1);	// EV(a,y,p,here,age)
	// get consumption at all states,savings combinations
	restmp   = ResBuy(all,all,all,all,all,age);
	
	ctmp     = restmp(a,y,pr,here,there) - p.R * agrid(save);
	
	LimitBuyer();
	xtmp     = where(ctmp > 0, p.imgamma*(pow(ctmp(a,y,pr,here,there,save),p.mgamma)) + p.theta + p.beta * vplustmp(save,y,pr,there) - MoveCost(here,there) + Amenity(there), p.myNA);	//vplustmp(save,y,p,there)
	// get value of staying
	v_loc_buy(all,all,all,all,all,age) = max(xtmp, save);
	s_loc_buy(all,all,all,all,all,age) = maxIndex(xtmp, save);
	FindBuyCons( age );	
}



void CMig6::ComputeSell(int age) {

	firstIndex   a;
	secondIndex  y;
	thirdIndex   pr;
	fourthIndex  here;
	fifthIndex   there;
	sixthIndex   save;
	Range all = Range::all();

	vplustmp = EVrent(all,all,all,all,age+1);	// EV(a,y,p,here,age)
	restmp   = ResSell(all,all,all,all,all,age);
	
	ctmp     = restmp(a,y,pr,here,there) - p.R * agrid(save);

	// no borrowing for seller
	ctmp(all,all,all,all,all,Range(fromStart,blimit_rent) ) = p.myNA;
	
	xtmp     = where(ctmp > 0, p.imgamma*(pow(ctmp(a,y,pr,here,there,save),p.mgamma)) +    0    + p.beta * vplustmp(save,y,pr,there) - MoveCost(here,there) + Amenity(there), p.myNA);	//vplustmp(save,y,p,there)
	// get value of selling at combo (here,there)
	v_loc_sell(all,all,all,all,all,age) = max(xtmp, save);
	s_loc_sell(all,all,all,all,all,age) = maxIndex(xtmp, save);
	FindSellCons( age );	

}

void CMig6::ComputeRent(int age) {

	firstIndex   a;
	secondIndex  y;
	thirdIndex   pr;
	fourthIndex  here;
	fifthIndex   there;
	sixthIndex   save;
	Range all = Range::all();

	vplustmp = EVrent(all,all,all,all,age+1);	// EV(a,y,p,here,age)
	// get consumption at all states,savings combinations
	restmp   = ResRent(all,all,all,all,all,age);
	
	ctmp     = restmp(a,y,pr,here,there) - p.R * agrid(save);

	// no borrowing for renter
	ctmp(all,all,all,all,all,Range(fromStart,blimit_rent) ) = p.myNA;


	xtmp     = where(ctmp > 0, p.imgamma*(pow(ctmp(a,y,pr,here,there,save),p.mgamma)) +    0    + p.beta * vplustmp(save,y,pr,there) - MoveCost(here,there) + Amenity(there), p.myNA);	//vplustmp(save,y,p,there)
	// get value of staying
	v_loc_rent(all,all,all,all,all,age) = max(xtmp, save);
	s_loc_rent(all,all,all,all,all,age) = maxIndex(xtmp, save);
	FindRentCons( age );	

}




// Consumption finder functions
// ============================

void CMig6::FindStayCons( int age ){
	int idx;
	TinyVector<int,6> ext;
	ext = s_loc_stay.extent() + 1; // add + 1 here because we have fortran arrays, which are indexed 1,2,...,extent(). So the upper bound of the loop must is extent()+1
	for (int i1=1;i1<ext(0);++i1){		// a
		for (int i2=1;i2<ext(1);++i2){	// y
			for (int i3=1; i3<ext(2); ++i3){	//p
				for (int i4=1; i4<ext(3); ++i4){	//here
					for (int i5=1; i5<ext(4); ++i5){	//there

						idx                            = s_loc_stay(i1,i2,i3,i4,i5,age);	// savings choice at that index
						c_loc_stay(i1,i2,i3,i4,i5,age) = ctmp(i1,i2,i3,i4,i5,idx);			// consumption at that index
					}
				}
			}
		}
	}
}

void CMig6::FindSellCons( int age ){
	int idx;
	TinyVector<int,6> ext;
	ext = s_loc_sell.extent() + 1; // add + 1 here because we have fortran arrays, which are indexed 1,2,...,extent(). So the upper bound of the loop must is extent()+1
	for (int i1=1;i1<ext(0);++i1){		// a
		for (int i2=1;i2<ext(1);++i2){	// y
			for (int i3=1; i3<ext(2); ++i3){	//p
				for (int i4=1; i4<ext(3); ++i4){	//here
					for (int i5=1; i5<ext(4); ++i5){	//there

						idx                            = s_loc_sell(i1,i2,i3,i4,i5,age);	// savings choice at that index
						c_loc_sell(i1,i2,i3,i4,i5,age) = ctmp(i1,i2,i3,i4,i5,idx);			// consumption at that index
					}
				}
			}
		}
	}
}

void CMig6::FindRentCons( int age ){
	int idx;
	TinyVector<int,6> ext;
	ext = s_loc_rent.extent() + 1; // add + 1 here because we have fortran arrays, which are indexed 1,2,...,extent(). So the upper bound of the loop must is extent()+1
	for (int i1=1;i1<ext(0);++i1){		// a
		for (int i2=1;i2<ext(1);++i2){	// y
			for (int i3=1; i3<ext(2); ++i3){	//p
				for (int i4=1; i4<ext(3); ++i4){	//here
					for (int i5=1; i5<ext(4); ++i5){	//there

						idx                            = s_loc_rent(i1,i2,i3,i4,i5,age);	// savings choice at that index
						c_loc_rent(i1,i2,i3,i4,i5,age) = ctmp(i1,i2,i3,i4,i5,idx);			// consumption at that index
					}
				}
			}
		}
	}
}


void CMig6::FindBuyCons( int age ){
	int idx;
	TinyVector<int,6> ext;
	ext = s_loc_buy.extent() + 1; // add + 1 here because we have fortran arrays, which are indexed 1,2,...,extent(). So the upper bound of the loop must is extent()+1
	for (int i1=1;i1<ext(0);++i1){		// a
		for (int i2=1;i2<ext(1);++i2){	// y
			for (int i3=1; i3<ext(2); ++i3){	//p
				for (int i4=1; i4<ext(3); ++i4){	//here
					for (int i5=1; i5<ext(4); ++i5){	//there

						idx                            = s_loc_buy(i1,i2,i3,i4,i5,age);	// savings choice at that index
						c_loc_buy(i1,i2,i3,i4,i5,age) = ctmp(i1,i2,i3,i4,i5,idx);			// consumption at that index
					}
				}
			}
		}
	}
}


void CMig6::ComputePeriod(int age){

	Range all = Range::all();
	
	if (verbose>0){
		cout << "age is " << age << std::endl;
	}

	// if final operiod, then preComputed resources are utility
	// unfortunately TinyVector dim_ayp_here_t only available as C++ array, so different indexing for those.
	if (age==maxage) {
		// EV(a,y,p,here,age)
		EVown( all,all,all,all,age) = ResStay(all,all,all,all,1,age);	// get final utility precomputed from resources
		EVrent(all,all,all,all,age) = ResRent(all,all,all,all,1,age);

	} else {

		ComputeStay(age);		
		ComputeSell(age);		
		ComputeRent(age);		
		ComputeBuy(age);		
		ComputeTenureChoice(age);		 
		ComputeLocationChoice(age);		 
		ComputeExpectations(age);	// get EVown and EVrent
	
	}

}


/** Binary Discrete Choice Function for Arrays
 * @param blitz::Array<double,5> one
 * @param blitz::Array<double,5> two
 *
 * @return blitz::Array<double,5> that has values max(one,two) index by index
 */
Array<double,5> CMig6::dchoice5d(Array<double,5> one, Array<double,5> two){

	Array<double,5> ret(nAsset,nIncome,nPrice,nLoc,nLoc,FortranArray<5>());

	ret = where(one > two, one, two);

	return(ret);
}

/** Binary Discrete Choice Indicator for Arrays
 * @param blitz::Array<double,5> one
 * @param blitz::Array<double,5> two
 *
 * @return blitz::Array<double,5> that the indicator of which array element is larger (i.e. 1 or 2) index by index
 */
Array<int,5> CMig6::dchoiceID5d(Array<double,5> one, Array<double,5> two){

	Array<int,5> ret(nAsset,nIncome,nPrice,nLoc,nLoc,FortranArray<5>());

	ret = where(one > two, 1, 2);

	return(ret);
}


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
void CMig6::ComputeLocationChoice( int age ){
	// source format: (a,y,p,here,there,age)
	// target format: (a,y,p,here,age)
   
	fifthIndex   there;	
	Range all = Range::all();

	// v_loc_tmp is (a,y,p,here,there,age)!!
	v_loc_tmp = W_loc_own(all,all,all,all,all,age);

	// NOTE: simulation. when using this in simulation, here add
	// a vector \xi of dimension (there,1) to each slice of W_loc_own
	// in the "there" dimension before taking the discrete choice.
	Vown(        all,all,all,all,age) = max(      v_loc_tmp, there );
	Location_own(all,all,all,all,age) = maxIndex( v_loc_tmp, there );

	v_loc_tmp = W_loc_rent(all,all,all,all,all,age);
	Vrent(        all,all,all,all,age) = max(      v_loc_tmp, there );
	Location_rent(all,all,all,all,age) = maxIndex( v_loc_tmp, there );
}




/** Tenure Choice at each (here,there) combo
 * choose between (stay,sell) for owner and (rent,buy) for renter
 * states.
 * @return nothing but alter members W_loc_own and W_loc_rent
 */
void CMig6::ComputeTenureChoice( int age ){

	// (a,y,p,here,there,age)
	Range all = Range::all();

	W_loc_own(  all,all,all,all,all,age)  = dchoice5d(  v_loc_stay(all,all,all,all,all,age), v_loc_sell(all,all,all,all,all,age));
	W_loc_rent( all,all,all,all,all,age)  = dchoice5d(  v_loc_rent(all,all,all,all,all,age), v_loc_buy( all,all,all,all,all,age));

	Tenure_loc_own(  all,all,all,all,all,age)  = dchoiceID5d(  v_loc_stay(all,all,all,all,all,age), v_loc_sell(all,all,all,all,all,age));
	Tenure_loc_rent( all,all,all,all,all,age)  = dchoiceID5d(  v_loc_rent(all,all,all,all,all,age), v_loc_buy( all,all,all,all,all,age));

}


/** Expectations and Conditional Choice Probabilities
 * compute vbar, EV and rho
 */
void CMig6::ComputeExpectations( int age ){

	Range all = Range::all();
	firstIndex   a;
	secondIndex  y;
	thirdIndex   pr;
	fourthIndex  here;
	fifthIndex   there;
	// compute Vbar first
	
	// owner
	// =====
	
	v_loc_tmp = W_loc_own(all,all,all,all,all,age);  // a,y,p,here,there
	Vbar_own( all,all,all,all,age) = euler_mascheroni + log( sum( exp( v_loc_tmp ), there ) ) ; 

	// compute CCP aka rho
	vbar_tmp = Vbar_own(all,all,all,all,age);  // a,y,p,here
	rho_own( all,all,all,all,all,age) = exp( euler_mascheroni + v_loc_tmp(a,y,pr,here,there) - vbar_tmp(a,y,pr,here) );
	

	//renter  
	// =====
	
	v_loc_tmp = W_loc_rent(all,all,all,all,all,age);  // a,y,p,here,there
	Vbar_rent( all,all,all,all,age) = euler_mascheroni + log( sum( exp( v_loc_tmp ), there ) ) ; 

	// compute CCP aka rho
	vbar_tmp = Vbar_rent(all,all,all,all,age);  // a,y,p,here
	rho_rent( all,all,all,all,all,age) = exp( euler_mascheroni + v_loc_tmp(a,y,pr,here,there) - vbar_tmp(a,y,pr,here) );

	// then integrate over Vbar to account for laws of motion of y and p.
	
	EVown( all,all,all,all,age) = integrate(Vbar_own( all,all,all,all,age));
	EVrent(all,all,all,all,age) = integrate(Vbar_rent(all,all,all,all,age));

}

Array<double,4> CMig6::integrate(Array<double,4> tens){


	firstIndex   a;	
	secondIndex  y; 
	thirdIndex   p;	
	fourthIndex  here;	
	fifthIndex   yp;	
	sixthIndex   pp;	
	
	Array<double,4> ret(nAsset,nIncome,nPrice,nLoc,FortranArray<4>());

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
	ret = sum( sum(   tens(a,yp,pp,here) * G(y,yp) * Gp(p,pp) , pp) ,yp );

	if (verbose>1) {
		cout << endl;
		cout << "in integrate now. ret is :" << endl;
		cout << ret << endl;
	}
	//tmp = tens(i1,i2,i3,i4) * G(i5,i2);
	//ret = sum( tmp(i1,i5,i3,i4,i2), i5);
	return(ret);

}

   
   
/*// constructor 4: expose the class directly to R*/
//// TODO can you do 
//// int m = dims(dims.length())
//// Array<double,m>
//CMig6::CMig6(Rcpp::IntegerVector dims,
		   //Rcpp::List pars,
		   //Rcpp::List arrays) :

	//// this is syntactic sugar. an initialiser list. 

	//ResStay(FortranArray<5>()),
    //ResSell(FortranArray<5>()),
	//ResRent(FortranArray<5>()),
    //ResBuy( FortranArray<5>()),

    //v_stay(FortranArray<4>()),	
    //v_sell(FortranArray<4>()),  
    //v_rent(FortranArray<4>()),	
    //v_buy( FortranArray<4>()),   
	//c_stay(FortranArray<4>()),	
	//c_sell(FortranArray<4>()),  
	//c_rent(FortranArray<4>()),	
	//c_buy( FortranArray<4>()),   
	//Vown(  FortranArray<4>()),	
	//Vrent( FortranArray<4>()),	
	//EVown( FortranArray<4>()),	
	//EVrent(FortranArray<4>()),	
	//ctmp(  FortranArray<4>()),  	
	//xtmp(  FortranArray<4>()),  	

	//s_stay(FortranArray<4>()),  
	//s_sell(FortranArray<4>()),  
	//s_rent(FortranArray<4>()),  
	//s_buy( FortranArray<4>()),  
	//Down(  FortranArray<4>()),  
	//Drent( FortranArray<4>()),  
	
	//vplustmp(FortranArray<3>()),
	//G(FortranArray<2>())         
    //{						  
		//// get data out of R lists
		//// 
		//Rcpp::NumericVector R_CO = Rcpp::as<Rcpp::NumericVector>(arrays["consO"]);
		//Rcpp::NumericVector R_CR = Rcpp::as<Rcpp::NumericVector>(arrays["consR"]);
		//Rcpp::NumericVector R_CB = Rcpp::as<Rcpp::NumericVector>(arrays["consB"]);
		//Rcpp::NumericVector R_CS = Rcpp::as<Rcpp::NumericVector>(arrays["consS"]);
		//Rcpp::NumericVector R_G  = Rcpp::as<Rcpp::NumericVector>(arrays["G"]);

		//p.beta = Rcpp::as<double>(data["beta"]);
		//p.myNA = Rcpp::as<double>(data["myNA"]);

		//// map to blitz arrays
		//// ===================
		
		//TinyVector<int,5> D_aypta(dims(0),dims(1),dims(2),dims(3),dims(0));
		//TinyVector<int,4> D_aypt(dims(0),dims(1),dims(2),dims(3));
		//TinyVector<int,4> D_aypa(dims(0),dims(1),dims(2),dims(0));
		//TinyVector<int,4> D_aypy(dims(0),dims(1),dims(2),dims(1));
		//TinyVector<int,3> D_ayp(dims(0),dims(1),dims(2));
		//TinyVector<int,2> D_y(dims(1),dims(1));
		//Array<double,2> G(R_G.begin(),shape(d(1),d(1)),neverDeleteData,FortranArray<2>());
		//Array<double,5> stay(R_CO.begin(),D_aypta,neverDeleteData,FortranArray<5>());
		//Array<double,5> rent(R_CR.begin(),D_aypta,neverDeleteData,FortranArray<5>());
		//Array<double,5> buy( R_CB.begin(),D_aypta,neverDeleteData,FortranArray<5>());
		//Array<double,5> sell(R_CS.begin(),D_aypta,neverDeleteData,FortranArray<5>());

		//// reference the data
		//ResStay.reference(data_stay);
		//ResSell.reference(data_sell);
		//ResRent.reference(data_rent);
		//ResBuy.reference(data_buy );
		//G.reference(data_G);
		//v_stay = 1;
		//v_sell = 2;
		//v_rent = 3;
		//v_buy = 4;
/*}*/


//#endif

























