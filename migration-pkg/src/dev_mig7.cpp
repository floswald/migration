
// define Rcpp compiling switch
#define RcppCompile TRUE
#include <blitz/array.h>
#include <Rcpp/Benchmark/Timer.h>
#include <Rcpp.h>
#include <RcppGSL.h>
#include "../../cpp/src/CMig7.cpp"

// [[Rcpp::depends(RcppGSL)]]

using namespace blitz;
 

//' dev9: continuous optimization, utility function
//' 
//' @example examples/example10.r
// [[Rcpp::export]]
Rcpp::List dev9( Rcpp::List data ) {

// start timer
	Rcpp::Timer timer;

	// R array data
	Rcpp::NumericVector R_CO   = Rcpp::as<Rcpp::NumericVector>(data["ResO"]);
	Rcpp::NumericVector R_CS   = Rcpp::as<Rcpp::NumericVector>(data["ResS"]);
	Rcpp::IntegerVector d      = Rcpp::as<Rcpp::IntegerVector>(data["dims"]);
	Rcpp::NumericVector R_G    = Rcpp::as<Rcpp::NumericVector>(data["G"]);
	Rcpp::NumericVector a_own  = Rcpp::as<Rcpp::NumericVector>(data["agrid_own"]);
	Rcpp::NumericVector a_rent = Rcpp::as<Rcpp::NumericVector>(data["agrid_rent"]);

	double cutoff = Rcpp::as<double>(data["cutoff"]);
	double gamma  = Rcpp::as<double>(data["gamma"]);
	double theta  = Rcpp::as<double>(data["theta"]);

	// map to blitz arrays
	// ===================
	
	TinyVector<int,3> D_ay_t(d(0),d(1),d(2));
	Array<double,2> G(R_G.begin(),d(1),d(1),neverDeleteData,FortranArray<2>());
	Array<double,3> stay(R_CO.begin(),D_ay_t,neverDeleteData,FortranArray<3>());
	Array<double,3> sell(R_CS.begin(),D_ay_t,neverDeleteData,FortranArray<3>());
	Array<double,1> aown(a_own.begin(),d(0),neverDeleteData);
	Array<double,1> arent(a_rent.begin(),d(0),neverDeleteData);

	Rcpp::Rcout << "length of aown " << aown.size() << endl;
	Rcpp::Rcout << "aown " << aown << endl;
	Rcpp::Rcout << "a_own " << a_own.begin() << endl;

	Rcpp::Rcout << "stay" << stay << endl;
	Rcpp::Rcout << "sell" << sell << endl;



	// make a gsl parameter struct
	// ===========================
	
	gsl_f_pars p;
	p.res    = 0;
	//p.type   = gsl_interp_cspline;
	p.type   = gsl_interp_linear;	// change interpolation type here.
	p.acc    = gsl_interp_accel_alloc ();
	p.spline = gsl_spline_alloc (p.type, aown.size());
	p.T      = gsl_root_fsolver_brent;
	p.sroot  = gsl_root_fsolver_alloc (p.T);


	// create an instance of the migration class
	// ===================================
	CMig7 myMig(D_ay_t,stay,sell,G,aown,arent,0,cutoff,gamma,theta,&p);

	// time loop to compute backwards
	// ===================================
	
	for (int it = myMig.GetMaxage(); it>0; it--) {
		myMig.ComputePeriod( it );
	}

	cout << "Computation done!" << endl;

	//timer.step("end blitz");

	//// output results to R
	//// ===================
	//// TODO want to do
	////Rcpp::List list = mig.RcppExport();
	
	//Rcpp::IntegerVector dAYPHereT = Rcpp::IntegerVector::create(d(0),d(1),d(2),d(3),d(5));
	//Rcpp::IntegerVector dAYPHereThereT = Rcpp::IntegerVector::create(d(0),d(1),d(2),d(3),d(4),d(5));

	//Rcpp::NumericVector Vown_out   ( mig6.GetVown().size());
	//Rcpp::NumericVector Vrent_out  ( Vown_out.length());
	//Rcpp::NumericVector EVown_out  ( Vown_out.length());
	//Rcpp::NumericVector EVrent_out ( Vown_out.length());
	//Rcpp::NumericVector Down_out  ( Vown_out.length());
	//Rcpp::NumericVector Drent_out ( Vown_out.length());
	//Rcpp::NumericVector vstay_out  ( Vown_out.length());
	//Rcpp::NumericVector vsell_out  ( Vown_out.length());
	//Rcpp::NumericVector vrent_out  ( Vown_out.length());
	//Rcpp::NumericVector vbuy_out   ( Vown_out.length());
	//Rcpp::NumericVector move_stay_out  ( Vown_out.length());
	//Rcpp::NumericVector move_sell_out  ( Vown_out.length());
	//Rcpp::NumericVector move_rent_out  ( Vown_out.length());
	//Rcpp::NumericVector move_buy_out   ( Vown_out.length());
	//Rcpp::NumericVector s_loc_stay_out  ( mig6.Getv_loc_stay().size());
	//Rcpp::NumericVector s_loc_sell_out  ( s_loc_stay_out.length());
	//Rcpp::NumericVector s_loc_rent_out  ( s_loc_stay_out.length());
	//Rcpp::NumericVector s_loc_buy_out   ( s_loc_stay_out.length());
	//Rcpp::NumericVector v_loc_stay_out  ( mig6.Getv_loc_stay().size());
	//Rcpp::NumericVector v_loc_sell_out  ( s_loc_stay_out.length());
	//Rcpp::NumericVector v_loc_rent_out  ( s_loc_stay_out.length());
	//Rcpp::NumericVector v_loc_buy_out   ( s_loc_stay_out.length());
	//Rcpp::NumericVector c_loc_stay_out  ( s_loc_stay_out.length());
	//Rcpp::NumericVector c_loc_sell_out  ( s_loc_stay_out.length());
	//Rcpp::NumericVector c_loc_rent_out  ( s_loc_stay_out.length());
	//Rcpp::NumericVector c_loc_buy_out   ( s_loc_stay_out.length());

	////Rcpp::NumericVector Vown_out2( mig.GetVown().data(), mig.GetVown().size() );
	
	//// copy blitz to Rcpp::Numeri[>c<]
	//Vown_out       = mig6.GetVown();
	//Vrent_out      = mig6.GetVrent();
	//EVown_out      = mig6.GetEVown();
	//EVrent_out     = mig6.GetEVrent();
	//Down_out       = mig6.GetDown();
	//Drent_out      = mig6.GetDrent();

	//vstay_out      = mig6.Getv_stay();
	//vsell_out      = mig6.Getv_sell();
	//vbuy_out       = mig6.Getv_buy();
	//vrent_out      = mig6.Getv_rent();
	
	//v_loc_stay_out      = mig6.Getv_loc_stay();
	//v_loc_sell_out      = mig6.Getv_loc_sell();
	//v_loc_buy_out       = mig6.Getv_loc_buy();
	//v_loc_rent_out      = mig6.Getv_loc_rent();

	//move_stay_out  = mig6.Getmove_stay();
	//move_sell_out  = mig6.Getmove_sell();
	//move_buy_out   = mig6.Getmove_buy();
	//move_rent_out  = mig6.Getmove_rent();

	//s_loc_stay_out = mig6.Gets_loc_stay();
	//s_loc_sell_out = mig6.Gets_loc_sell();
	//s_loc_buy_out  = mig6.Gets_loc_buy();
	//s_loc_rent_out = mig6.Gets_loc_rent();
	//c_loc_stay_out = mig6.Getc_loc_stay();
	//c_loc_sell_out = mig6.Getc_loc_sell();
	//c_loc_buy_out  = mig6.Getc_loc_buy();
	//c_loc_rent_out = mig6.Getc_loc_rent();
	
	//// attach dimension argument
	//Vown_out.attr("dim")       = dAYPHereT;
	//Vrent_out.attr("dim")      = dAYPHereT;
	//EVown_out.attr("dim")      = dAYPHereT;
	//EVrent_out.attr("dim")     = dAYPHereT;
	//Down_out.attr("dim")       = dAYPHereT;
	//Drent_out.attr("dim")      = dAYPHereT;
	//vstay_out.attr("dim")      = dAYPHereT;
	//vsell_out.attr("dim")      = dAYPHereT;
	//vbuy_out.attr("dim")       = dAYPHereT;
	//vrent_out.attr("dim")      = dAYPHereT;
	//move_stay_out.attr("dim")  = dAYPHereT;
	//move_sell_out.attr("dim")  = dAYPHereT;
	//move_buy_out.attr("dim")   = dAYPHereT;
	//move_rent_out.attr("dim")  = dAYPHereT;
	//v_loc_stay_out.attr("dim") = dAYPHereThereT;
	//v_loc_sell_out.attr("dim") = dAYPHereThereT;
	//v_loc_buy_out.attr("dim")  = dAYPHereThereT;
	//v_loc_rent_out.attr("dim") = dAYPHereThereT;
	//s_loc_stay_out.attr("dim") = dAYPHereThereT;
	//s_loc_sell_out.attr("dim") = dAYPHereThereT;
	//s_loc_buy_out.attr("dim")  = dAYPHereThereT;
	//s_loc_rent_out.attr("dim") = dAYPHereThereT;
	//c_loc_stay_out.attr("dim") = dAYPHereThereT;
	//c_loc_sell_out.attr("dim") = dAYPHereThereT;
	//c_loc_buy_out.attr("dim")  = dAYPHereThereT;
	/*c_loc_rent_out.attr("dim") = dAYPHereThereT;*/
	
	// create output list
	Rcpp::List list = Rcpp::List::create( Rcpp::_["Vown"]   = 1);
	//Rcpp::List list = Rcpp::List::create( Rcpp::_["Vown"]   = Vown_out,
										  //Rcpp::_["Vrent"]  = Vrent_out,
										  //Rcpp::_["EVown"]  = EVown_out,
										  //Rcpp::_["EVrent"] = EVrent_out,
										  //Rcpp::_["Down"]   = Down_out,
										  //Rcpp::_["Drent"]  = Drent_out,
										  //Rcpp::_["vstay"]  = vstay_out,
										  //Rcpp::_["vsell"]  = vsell_out,
										  //Rcpp::_["vbuy"]   = vbuy_out ,
										  //Rcpp::_["vrent"]  = vrent_out,
										  //Rcpp::_["move_stay"]  = move_stay_out,
										  //Rcpp::_["move_sell"]  = move_sell_out,
										  //Rcpp::_["move_buy"]   = move_buy_out ,
										  //Rcpp::_["move_rent"]  = move_rent_out,
										  //Rcpp::_["s_loc_stay"]  = s_loc_stay_out,
										  //Rcpp::_["s_loc_sell"]  = s_loc_sell_out,
										  //Rcpp::_["s_loc_buy"]   = s_loc_buy_out ,
										  //Rcpp::_["s_loc_rent"]  = s_loc_rent_out,
										  ////Rcpp::_["v_loc_stay"]  = v_loc_stay_out,
										  ////Rcpp::_["v_loc_sell"]  = v_loc_sell_out,
										  ////Rcpp::_["v_loc_buy"]   = v_loc_buy_out ,
										  ////Rcpp::_["v_loc_rent"]  = v_loc_rent_out,
										  ////Rcpp::_["c_loc_stay"]  = c_loc_stay_out,
										  ////Rcpp::_["c_loc_sell"]  = c_loc_sell_out,
										  ////Rcpp::_["c_loc_buy"]   = c_loc_buy_out ,
										  ////Rcpp::_["c_loc_rent"]  = c_loc_rent_out,
										  //Rcpp::_["time"]   = timer);

										   
	return list;
}   
