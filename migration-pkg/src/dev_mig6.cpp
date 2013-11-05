// define Rcpp compiling switch
#define RcppCompile TRUE
#include <blitz/array.h>
#include <Rcpp/Benchmark/Timer.h>
#include <Rcpp.h>
#include "../../cpp/src/CMig6.cpp"

using namespace blitz;
 

//' dev8: discrete optimization, housing choice, utility function, location choice
//' 
//' @example examples/example-dev8.r
// [[Rcpp::export]]
Rcpp::List dev8( Rcpp::List data ) {

// start timer
	Rcpp::Timer timer;

	// R array data
	Rcpp::NumericVector R_CO = Rcpp::as<Rcpp::NumericVector>(data["consO"]);
	Rcpp::NumericVector R_CR = Rcpp::as<Rcpp::NumericVector>(data["consR"]);
	Rcpp::NumericVector R_CB = Rcpp::as<Rcpp::NumericVector>(data["consB"]);
	Rcpp::NumericVector R_CS = Rcpp::as<Rcpp::NumericVector>(data["consS"]);
	Rcpp::IntegerVector d    = Rcpp::as<Rcpp::IntegerVector>(data["dims"]);
	Rcpp::NumericVector R_G  = Rcpp::as<Rcpp::NumericVector>(data["G"]);
	Rcpp::NumericVector R_M  = Rcpp::as<Rcpp::NumericVector>(data["MoveCost"]);
	Rcpp::NumericVector R_A  = Rcpp::as<Rcpp::NumericVector>(data["Amenity"]);



	// R parameter data
	Parstruc p;
	p.beta    = Rcpp::as<double>(data["beta"]);
	p.myNA    = Rcpp::as<double>(data["myNA"]);
	p.theta   = Rcpp::as<double>(data["theta"]);
	p.gamma   = Rcpp::as<double>(data["gamma"]);
	p.mgamma  = 1 - p.gamma;
	p.imgamma = 1/p.mgamma;

	// stop if gamma==1. no provision in code for log utilty.
	if (p.gamma==1.0) {
		throw std::range_error("CRRA gamma==1 is not implemented");
	}

	// map to blitz arrays
	// ===================
	
	TinyVector<int,7> D_ayp_here_there_ta(d(0),d(1),d(2),d(3),d(4),d(5),d(0));
	TinyVector<int,6> D_ayp_here_there_t(d(0),d(1),d(2),d(3),d(4),d(5));
	TinyVector<int,6> D_ayp_here_there_a(d(0),d(1),d(2),d(3),d(4),d(0));
	TinyVector<int,5> D_ayp_here_there(d(0),d(1),d(2),d(3),d(4));
	TinyVector<int,5> D_ayp_here_t(d(0),d(1),d(2),d(3),d(5));
	TinyVector<int,5> D_ayp_here_y(d(0),d(1),d(2),d(3),d(1));
	TinyVector<int,4> D_ayp_here(d(0),d(1),d(2),d(3));
	TinyVector<int,3> D_ayp(d(0),d(1),d(2));
	TinyVector<int,2> D_y(d(1),d(1));
	TinyVector<int,2> D_M(d(3),d(3));

	Array<double,2> G(R_G.begin(),D_y,neverDeleteData,FortranArray<2>());
	Array<double,2> MoveCost(R_M.begin(),D_M,neverDeleteData,FortranArray<2>());
	Array<double,1> Amenity(R_A.begin(),d(3),neverDeleteData,FortranArray<1>());
	Array<double,7> stay(R_CO.begin(),D_ayp_here_there_ta,neverDeleteData,FortranArray<7>());
	Array<double,7> rent(R_CR.begin(),D_ayp_here_there_ta,neverDeleteData,FortranArray<7>());
	Array<double,7> buy( R_CB.begin(),D_ayp_here_there_ta,neverDeleteData,FortranArray<7>());
	Array<double,7> sell(R_CS.begin(),D_ayp_here_there_ta,neverDeleteData,FortranArray<7>());

	// create an instance of the migration class
	// ===================================
    CMig6 mig6(D_ayp_here_there_ta,		
	         D_ayp_here_there_t,   
	         D_ayp_here_there_a,   
	         D_ayp_here_there,   
	   	     D_ayp_here_t,      
	   	     D_ayp_here_y,      
	   	     D_ayp_here,      
	   	     D_ayp,               
	   	     D_y,                 
	   	     &p ,                       
	   	     stay,             
	   	     sell,             
	   	     rent,             
	   	     buy,              
	   	     G,
			 MoveCost,
			 Amenity	) ;             
	

	// call the show method
	mig6.show();

	// time loop to compute backwards
	// ===================================
	
	for (int it = d(5); it>0; it--) {
		mig6.ComputePeriod( it );
	}

	timer.step("end blitz");

	// output results to R
	// ===================
	// TODO want to do
	//Rcpp::List list = mig.RcppExport();
	
	Rcpp::IntegerVector dAYPHereT = Rcpp::IntegerVector::create(d(0),d(1),d(2),d(3),d(5));
	Rcpp::IntegerVector dAYPHereThereT = Rcpp::IntegerVector::create(d(0),d(1),d(2),d(3),d(4),d(5));

	Rcpp::NumericVector Vown_out   ( mig6.GetVown().size());
	Rcpp::NumericVector Vrent_out  ( Vown_out.length());
	Rcpp::NumericVector EVown_out  ( Vown_out.length());
	Rcpp::NumericVector EVrent_out ( Vown_out.length());
	Rcpp::NumericVector Down_out  ( Vown_out.length());
	Rcpp::NumericVector Drent_out ( Vown_out.length());
	Rcpp::NumericVector vstay_out  ( Vown_out.length());
	Rcpp::NumericVector vsell_out  ( Vown_out.length());
	Rcpp::NumericVector vrent_out  ( Vown_out.length());
	Rcpp::NumericVector vbuy_out   ( Vown_out.length());
	Rcpp::NumericVector move_stay_out  ( Vown_out.length());
	Rcpp::NumericVector move_sell_out  ( Vown_out.length());
	Rcpp::NumericVector move_rent_out  ( Vown_out.length());
	Rcpp::NumericVector move_buy_out   ( Vown_out.length());
	Rcpp::NumericVector s_loc_stay_out  ( mig6.Getv_loc_stay().size());
	Rcpp::NumericVector s_loc_sell_out  ( s_loc_stay_out.length());
	Rcpp::NumericVector s_loc_rent_out  ( s_loc_stay_out.length());
	Rcpp::NumericVector s_loc_buy_out   ( s_loc_stay_out.length());
	Rcpp::NumericVector v_loc_stay_out  ( mig6.Getv_loc_stay().size());
	Rcpp::NumericVector v_loc_sell_out  ( s_loc_stay_out.length());
	Rcpp::NumericVector v_loc_rent_out  ( s_loc_stay_out.length());
	Rcpp::NumericVector v_loc_buy_out   ( s_loc_stay_out.length());
	Rcpp::NumericVector c_loc_stay_out  ( s_loc_stay_out.length());
	Rcpp::NumericVector c_loc_sell_out  ( s_loc_stay_out.length());
	Rcpp::NumericVector c_loc_rent_out  ( s_loc_stay_out.length());
	Rcpp::NumericVector c_loc_buy_out   ( s_loc_stay_out.length());

	//Rcpp::NumericVector Vown_out2( mig.GetVown().data(), mig.GetVown().size() );
	
	// copy blitz to Rcpp::Numeri/*c*/
	Vown_out       = mig6.GetVown();
	Vrent_out      = mig6.GetVrent();
	EVown_out      = mig6.GetEVown();
	EVrent_out     = mig6.GetEVrent();
	Down_out       = mig6.GetDown();
	Drent_out      = mig6.GetDrent();

	vstay_out      = mig6.Getv_stay();
	vsell_out      = mig6.Getv_sell();
	vbuy_out       = mig6.Getv_buy();
	vrent_out      = mig6.Getv_rent();
	
	v_loc_stay_out      = mig6.Getv_loc_stay();
	v_loc_sell_out      = mig6.Getv_loc_sell();
	v_loc_buy_out       = mig6.Getv_loc_buy();
	v_loc_rent_out      = mig6.Getv_loc_rent();

	move_stay_out  = mig6.Getmove_stay();
	move_sell_out  = mig6.Getmove_sell();
	move_buy_out   = mig6.Getmove_buy();
	move_rent_out  = mig6.Getmove_rent();

	s_loc_stay_out = mig6.Gets_loc_stay();
	s_loc_sell_out = mig6.Gets_loc_sell();
	s_loc_buy_out  = mig6.Gets_loc_buy();
	s_loc_rent_out = mig6.Gets_loc_rent();
	c_loc_stay_out = mig6.Getc_loc_stay();
	c_loc_sell_out = mig6.Getc_loc_sell();
	c_loc_buy_out  = mig6.Getc_loc_buy();
	c_loc_rent_out = mig6.Getc_loc_rent();
	
	// attach dimension argument
	Vown_out.attr("dim")       = dAYPHereT;
	Vrent_out.attr("dim")      = dAYPHereT;
	EVown_out.attr("dim")      = dAYPHereT;
	EVrent_out.attr("dim")     = dAYPHereT;
	Down_out.attr("dim")       = dAYPHereT;
	Drent_out.attr("dim")      = dAYPHereT;
	vstay_out.attr("dim")      = dAYPHereT;
	vsell_out.attr("dim")      = dAYPHereT;
	vbuy_out.attr("dim")       = dAYPHereT;
	vrent_out.attr("dim")      = dAYPHereT;
	move_stay_out.attr("dim")  = dAYPHereT;
	move_sell_out.attr("dim")  = dAYPHereT;
	move_buy_out.attr("dim")   = dAYPHereT;
	move_rent_out.attr("dim")  = dAYPHereT;
	v_loc_stay_out.attr("dim") = dAYPHereThereT;
	v_loc_sell_out.attr("dim") = dAYPHereThereT;
	v_loc_buy_out.attr("dim")  = dAYPHereThereT;
	v_loc_rent_out.attr("dim") = dAYPHereThereT;
	s_loc_stay_out.attr("dim") = dAYPHereThereT;
	s_loc_sell_out.attr("dim") = dAYPHereThereT;
	s_loc_buy_out.attr("dim")  = dAYPHereThereT;
	s_loc_rent_out.attr("dim") = dAYPHereThereT;
	c_loc_stay_out.attr("dim") = dAYPHereThereT;
	c_loc_sell_out.attr("dim") = dAYPHereThereT;
	c_loc_buy_out.attr("dim")  = dAYPHereThereT;
	c_loc_rent_out.attr("dim") = dAYPHereThereT;
	
	// create output list
	Rcpp::List bigL;
	Rcpp::List Values = Rcpp::List::create( Rcpp::_["Vown"]   = Vown_out,
										  Rcpp::_["Vrent"]  = Vrent_out,
	                                      Rcpp::_["EVown"]  = EVown_out,
										  Rcpp::_["EVrent"] = EVrent_out,
										  Rcpp::_["vstay"]  = vstay_out,
										  Rcpp::_["vsell"]  = vsell_out,
										  Rcpp::_["vbuy"]   = vbuy_out ,
										  Rcpp::_["vrent"]  = vrent_out,
										  Rcpp::_["v_loc_stay"]  = v_loc_stay_out,
										  Rcpp::_["v_loc_sell"]  = v_loc_sell_out,
										  Rcpp::_["v_loc_buy"]   = v_loc_buy_out ,
										  Rcpp::_["v_loc_rent"]  = v_loc_rent_out);
	Rcpp::List policies = Rcpp::List::create( Rcpp::_["Down"]   = Down_out,
										  Rcpp::_["Drent"]  = Drent_out,
										  Rcpp::_["move_stay"]  = move_stay_out,
										  Rcpp::_["move_sell"]  = move_sell_out,
										  Rcpp::_["move_buy"]   = move_buy_out ,
										  Rcpp::_["move_rent"]  = move_rent_out,
										  Rcpp::_["s_loc_stay"]  = s_loc_stay_out,
										  Rcpp::_["s_loc_sell"]  = s_loc_sell_out,
										  Rcpp::_["s_loc_buy"]   = s_loc_buy_out ,
										  Rcpp::_["s_loc_rent"]  = s_loc_rent_out,
										  Rcpp::_["c_loc_stay"]  = c_loc_stay_out,
										  Rcpp::_["c_loc_sell"]  = c_loc_sell_out,
										  Rcpp::_["c_loc_buy"]   = c_loc_buy_out ,
										  Rcpp::_["c_loc_rent"]  = c_loc_rent_out,
										  Rcpp::_["time"]   = timer);


	bigL = Rcpp::List::create( Rcpp::_["Values"] = Values,Rcpp::_["policies"] = policies);

										   
	return bigL;
}   
