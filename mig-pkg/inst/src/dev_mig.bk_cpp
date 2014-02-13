// define Rcpp compiling switch
#define RcppCompile TRUE
#include <blitz/array.h>
#include <Rcpp/Benchmark/Timer.h>
#include <Rcpp.h>
#include "../../cpp/src/CMig.cpp"

using namespace blitz;


// how to use this interface
// =========================
//
// the user must pass a list with data. list contains
// * admissible savings functions. these are arrays that specify how much can
//   be borrowed on each state. If borrowing is illegal on a certain state (and
//   after a certain amount of borrowing) then the array must hold NA at that position.
// * Evaluation grids for prices in each period and location, independent of aggregate state
// * Integration grids for each period, location and aggregate state. these are tomorrow's prices
//   if today's price is p, you are in locatino j, and tomorrows state can be Z=1 or Z=0.
// * parameter data
// * Resource arrays. cash on hand at each state and here-there choice
// * transition matrices
// * amenity values
// * moving costs
// * 




//' dev8: discrete optimization, housing choice, utility function, location choice
//' 
//' @example examples/example11.r
// [[Rcpp::export]]
Rcpp::List dev8( Rcpp::List data ) {

// start timer
	Rcpp::Timer timer;

	// R array data
	Rcpp::NumericVector R_CO        = Rcpp::as<Rcpp::NumericVector>(data["resO"]);
	Rcpp::NumericVector R_CR        = Rcpp::as<Rcpp::NumericVector>(data["resR"]);
	Rcpp::NumericVector R_CB        = Rcpp::as<Rcpp::NumericVector>(data["resB"]);
	Rcpp::NumericVector R_CS        = Rcpp::as<Rcpp::NumericVector>(data["resS"]);
	Rcpp::IntegerVector d           = Rcpp::as<Rcpp::IntegerVector>(data["dimshere"]);
	Rcpp::NumericVector R_G         = Rcpp::as<Rcpp::NumericVector>(data["G"]);
	Rcpp::NumericVector R_Gp        = Rcpp::as<Rcpp::NumericVector>(data["Gp"]);
	Rcpp::NumericVector R_M         = Rcpp::as<Rcpp::NumericVector>(data["MoveCost"]);
	Rcpp::IntegerVector R_blim_own  = Rcpp::as<Rcpp::IntegerVector>(data["blim_own"]);
	Rcpp::IntegerVector R_blim_buy  = Rcpp::as<Rcpp::IntegerVector>(data["blim_buy"]);
	int                 R_blim_rent = Rcpp::as<int>(data["blim_rent"]);
	int                 verbose     = Rcpp::as<int>(data["verbose"]);
	Rcpp::NumericVector R_Amenity   = Rcpp::as<Rcpp::NumericVector>(data["Amenity"]);
	Rcpp::NumericVector R_agrid     = Rcpp::as<Rcpp::NumericVector>(data["agrid"]);

	// R parameter data
	PStruct p;
	p.beta    = Rcpp::as<double>(data["beta"]);
	p.myNA    = Rcpp::as<double>(data["myNA"]);
	p.theta   = Rcpp::as<double>(data["theta"]);
	p.gamma   = Rcpp::as<double>(data["gamma"]);
	p.R       = Rcpp::as<double>(data["R"]);
	p.mgamma  = 1 - p.gamma;
	p.imgamma = 1/p.mgamma;

	// stop if gamma==1. no provision in code for log utilty.
	if (p.gamma==1.0) {
		throw std::range_error("CRRA gamma==1 is not implemented");
	}

	// map to blitz arrays
	// ===================
	
	TinyVector<int,6> D_ayp_here_there_t(d(0),d(1),d(2),d(3),d(3),d(4));
	TinyVector<int,2> D_y(d(1),d(1));
	TinyVector<int,2> D_M(d(3),d(3));
	TinyVector<int,2> D_P(d(2),d(2));
	TinyVector<int,1> D_A(d(0));
	TinyVector<int,1> D_L(d(3));
	TinyVector<int,3> D_LLP(d(3),d(3),d(2));
	TinyVector<int,2> D_LP(d(3),d(2));

	Array<double,2> G(R_G.begin(),D_y,neverDeleteData,FortranArray<2>());
	Array<double,2> Gp(R_Gp.begin(),D_P,neverDeleteData,FortranArray<2>());

	Array<double,2> MoveCost(R_M.begin(),D_M,neverDeleteData,FortranArray<2>());
	Array<int   ,3> blim_own(R_blim_own.begin(),D_LLP,neverDeleteData,FortranArray<3>());
	Array<int   ,2> blim_buy(R_blim_buy.begin(),D_LP,neverDeleteData,FortranArray<2>());
	Array<double,1> Amenity(R_Amenity.begin(),D_L,neverDeleteData,FortranArray<1>());

	Array<double,1> agrid(R_agrid.begin(),D_A,neverDeleteData,FortranArray<1>());
	Array<double,6> stay(R_CO.begin(),D_ayp_here_there_t,neverDeleteData,FortranArray<6>());
	Array<double,6> rent(R_CR.begin(),D_ayp_here_there_t,neverDeleteData,FortranArray<6>());
	Array<double,6> buy( R_CB.begin(),D_ayp_here_there_t,neverDeleteData,FortranArray<6>());
	Array<double,6> sell(R_CS.begin(),D_ayp_here_there_t,neverDeleteData,FortranArray<6>());

	// create an mapped data instance of the migration class
	// =====================================================
	
	CMig myMig_ref(d(0), d(1), d(2), d(3), d(4),
			    &p, stay, sell, rent, buy, G, Gp,
				MoveCost, Amenity, agrid, blim_own, blim_buy,
				R_blim_rent, verbose);

	// if verbose call the show method
	if (verbose > 0) myMig_ref.show();
	

	// time loop to compute backwards
	// ===================================
	
	for (int it = myMig_ref.GetMaxage(); it>0; it--) {
		myMig_ref.ComputePeriod( it );
	}

	timer.step("end blitz");

	 //output results to R
	 //===================
	 //TODO want to do
	//Rcpp::List list = mig.RcppExport();
	
	Rcpp::IntegerVector dAYPHereT = Rcpp::IntegerVector::create(d(0),d(1),d(2),d(3),d(4));
	Rcpp::IntegerVector dAYPHereThereT = Rcpp::IntegerVector::create(d(0),d(1),d(2),d(3),d(3),d(4));

	// length (a,y,p,here,age)
	Rcpp::NumericVector Vown_out         ( myMig_ref.GetVown().size());
	Rcpp::NumericVector Vrent_out        ( Vown_out.length());
	Rcpp::NumericVector EVown_out        ( Vown_out.length());
	Rcpp::NumericVector EVrent_out       ( Vown_out.length());
	Rcpp::NumericVector Vbar_own_out      ( Vown_out.length());
	Rcpp::NumericVector Vbar_rent_out     ( Vown_out.length());
	Rcpp::NumericVector LocationOwn_out  ( Vown_out.length());
	Rcpp::NumericVector LocationRent_out ( Vown_out.length());

	// length (a,y,p,here,there,age)
	Rcpp::NumericVector TenureOwn_out    ( myMig_ref.GetTenure_loc_own().size() );
	Rcpp::NumericVector TenureRent_out   ( TenureOwn_out.length());
	Rcpp::NumericVector W_loc_own_out    ( TenureOwn_out.length());
	Rcpp::NumericVector W_loc_rent_out   ( TenureOwn_out.length());
	Rcpp::NumericVector rho_own_out    ( TenureOwn_out.length());
	Rcpp::NumericVector rho_rent_out   ( TenureOwn_out.length());

	Rcpp::NumericVector s_loc_stay_out  ( TenureOwn_out.length());
	Rcpp::NumericVector s_loc_sell_out  ( TenureOwn_out.length());
	Rcpp::NumericVector s_loc_rent_out  ( TenureOwn_out.length());
	Rcpp::NumericVector s_loc_buy_out   ( TenureOwn_out.length());
	Rcpp::NumericVector v_loc_stay_out  ( TenureOwn_out.length());
	Rcpp::NumericVector v_loc_sell_out  ( TenureOwn_out.length());
	Rcpp::NumericVector v_loc_rent_out  ( TenureOwn_out.length());
	Rcpp::NumericVector v_loc_buy_out   ( TenureOwn_out.length());
	Rcpp::NumericVector c_loc_stay_out  ( TenureOwn_out.length());
	Rcpp::NumericVector c_loc_sell_out  ( TenureOwn_out.length());
	Rcpp::NumericVector c_loc_rent_out  ( TenureOwn_out.length());
	Rcpp::NumericVector c_loc_buy_out   ( TenureOwn_out.length());

	//Rcpp::NumericVector Vown_out2( mig.GetVown().data(), mig.GetVown().size() );
	
	// copy blitz to Rcpp::Numeri[>c<]
	Vown_out         = myMig_ref.GetVown();
	Vrent_out        = myMig_ref.GetVrent();
	EVown_out        = myMig_ref.GetEVown();
	EVrent_out       = myMig_ref.GetEVrent();
	Vbar_own_out     = myMig_ref.GetVbarOwn();
	Vbar_rent_out    = myMig_ref.GetVbarRent();
	LocationOwn_out  = myMig_ref.GetLocationOwn();
	LocationRent_out = myMig_ref.GetLocationRent();
                    
	TenureOwn_out    = myMig_ref.GetTenure_loc_own();
	TenureRent_out   = myMig_ref.GetTenure_loc_rent();
	W_loc_own_out    = myMig_ref.GetW_loc_own();
	W_loc_rent_out   = myMig_ref.GetW_loc_rent();
	rho_own_out    = myMig_ref.Getrho_own();
	rho_rent_out   = myMig_ref.Getrho_rent();
	                
	s_loc_stay_out   = myMig_ref.Gets_loc_stay();
	s_loc_sell_out   = myMig_ref.Gets_loc_sell();
	s_loc_rent_out   = myMig_ref.Gets_loc_rent();
	s_loc_buy_out    = myMig_ref.Gets_loc_buy();
    v_loc_stay_out   = myMig_ref.Getv_loc_stay();
	v_loc_sell_out   = myMig_ref.Getv_loc_sell();
	v_loc_rent_out   = myMig_ref.Getv_loc_rent();
	v_loc_buy_out    = myMig_ref.Getv_loc_buy();
	c_loc_stay_out   = myMig_ref.Getc_loc_stay();
    c_loc_sell_out   = myMig_ref.Getc_loc_sell();
	c_loc_rent_out   = myMig_ref.Getc_loc_rent();
	c_loc_buy_out    = myMig_ref.Getc_loc_buy();
	
	// attach dimension argument
	Vown_out.attr("dim")         = dAYPHereT;
	Vrent_out.attr("dim")        = dAYPHereT;
	EVown_out.attr("dim")        = dAYPHereT;
	EVrent_out.attr("dim")       = dAYPHereT;
	Vbar_own_out.attr("dim")     = dAYPHereT;
	Vbar_rent_out.attr("dim")    = dAYPHereT;
	LocationOwn_out.attr("dim")  = dAYPHereT;
	LocationRent_out.attr("dim") = dAYPHereT;

	W_loc_own_out.attr("dim")  = dAYPHereThereT;
	W_loc_rent_out.attr("dim") = dAYPHereThereT;
	rho_own_out.attr("dim")  = dAYPHereThereT;
	rho_rent_out.attr("dim") = dAYPHereThereT;
	TenureOwn_out.attr("dim")  = dAYPHereThereT;
	TenureRent_out.attr("dim") = dAYPHereThereT;

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
	Rcpp::List Values = Rcpp::List::create( Rcpp::_["Vown"]       = Vown_out,
										    Rcpp::_["Vrent"]      = Vrent_out,
										    Rcpp::_["EVown"]      = EVown_out,
										    Rcpp::_["EVrent"]     = EVrent_out,
										    Rcpp::_["Vbar_own"]   = Vbar_own_out,
										    Rcpp::_["Vbar_rent"]  = Vbar_rent_out,
										    Rcpp::_["Wown"]      = W_loc_own_out,
										    Rcpp::_["Wrent"]     = W_loc_rent_out,
										    Rcpp::_["v_loc_stay"] = v_loc_stay_out,
										    Rcpp::_["v_loc_sell"] = v_loc_sell_out,
										    Rcpp::_["v_loc_buy"]  = v_loc_buy_out ,
										    Rcpp::_["v_loc_rent"] = v_loc_rent_out);

	Rcpp::List policies = Rcpp::List::create( Rcpp::_["TenureOwn"]     = TenureOwn_out,
											  Rcpp::_["TenureRent"]    = TenureRent_out,
											  Rcpp::_["LocationOwn"]   = LocationOwn_out,
											  Rcpp::_["LocationRent"] = LocationRent_out,
											  Rcpp::_["rho_own"]   = rho_own_out,
											  Rcpp::_["rho_rent"] =  rho_rent_out,
											  Rcpp::_["s_loc_stay"]    = s_loc_stay_out,
											  Rcpp::_["s_loc_sell"]    = s_loc_sell_out,
											  Rcpp::_["s_loc_buy"]     = s_loc_buy_out ,
											  Rcpp::_["s_loc_rent"]    = s_loc_rent_out,
											  Rcpp::_["c_loc_stay"]    = c_loc_stay_out,
											  Rcpp::_["c_loc_sell"]    = c_loc_sell_out,
											  Rcpp::_["c_loc_buy"]     = c_loc_buy_out ,
											  Rcpp::_["c_loc_rent"]    = c_loc_rent_out,
											  Rcpp::_["time"]          = timer);


	bigL = Rcpp::List::create( Rcpp::_["Values"] = Values,Rcpp::_["policies"] = policies);

										   
	return bigL;
}   
