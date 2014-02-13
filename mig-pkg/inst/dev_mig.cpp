// define Rcpp compiling switch
#define RcppCompile TRUE
#include <blitz/array.h>
#include <Rcpp/Benchmark/Timer.h>
#include <Rcpp.h>
#include "../../cpp/src/CMig.cpp"

using namespace blitz;
 

//' dev7: discrete optimization, housing choice, utility function
//' 
//' @example examples/example-dev7.r
// [[Rcpp::export]]
Rcpp::List dev7( Rcpp::List data ) {

// start timer
	Rcpp::Timer timer;

	// R array data
	Rcpp::NumericVector R_CO = Rcpp::as<Rcpp::NumericVector>(data["consO"]);
	Rcpp::NumericVector R_CR = Rcpp::as<Rcpp::NumericVector>(data["consR"]);
	Rcpp::NumericVector R_CB = Rcpp::as<Rcpp::NumericVector>(data["consB"]);
	Rcpp::NumericVector R_CS = Rcpp::as<Rcpp::NumericVector>(data["consS"]);
	Rcpp::IntegerVector d    = Rcpp::as<Rcpp::IntegerVector>(data["dims"]);
	Rcpp::NumericVector R_G  = Rcpp::as<Rcpp::NumericVector>(data["G"]);

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
	
	TinyVector<int,5> D_aypta(d(0),d(1),d(2),d(3),d(0));
	TinyVector<int,4> D_aypt(d(0),d(1),d(2),d(3));
	TinyVector<int,4> D_aypa(d(0),d(1),d(2),d(0));
	TinyVector<int,4> D_aypy(d(0),d(1),d(2),d(1));
	TinyVector<int,3> D_ayp(d(0),d(1),d(2));
	TinyVector<int,2> D_y(d(1),d(1));
	Array<double,2> G(R_G.begin(),shape(d(1),d(1)),neverDeleteData,FortranArray<2>());
	Array<double,5> stay(R_CO.begin(),D_aypta,neverDeleteData,FortranArray<5>());
	Array<double,5> rent(R_CR.begin(),D_aypta,neverDeleteData,FortranArray<5>());
	Array<double,5> buy( R_CB.begin(),D_aypta,neverDeleteData,FortranArray<5>());
	Array<double,5> sell(R_CS.begin(),D_aypta,neverDeleteData,FortranArray<5>());

	// create an instance of the migration class
	// ===================================
	
	CMig mig(D_aypta,D_aypt,D_aypa,D_aypy,D_ayp,D_y, &p, stay,sell,rent,buy,G);

	// call the show method
	mig.show();

	// time loop to compute backwards
	// ===================================
	
	for (int it = d(3); it>0; it--) {
		mig.ComputePeriod( it );
	}

	timer.step("end blitz");

	// output results to R
	// ===================
	// TODO want to do
	//Rcpp::List list = mig.RcppExport();
	
	Rcpp::IntegerVector dAYPT = Rcpp::IntegerVector::create(d(0),d(1),d(2),d(3));
	Rcpp::IntegerVector dAYPTA = Rcpp::IntegerVector::create(d(0),d(1),d(2),d(3),d(0));

	Rcpp::NumericVector Vown_out   ( mig.GetVown().size());
	Rcpp::NumericVector Vrent_out  ( Vown_out.length());
	Rcpp::NumericVector EVown_out  ( Vown_out.length());
	Rcpp::NumericVector EVrent_out ( Vown_out.length());
	Rcpp::NumericVector Down_out  ( Vown_out.length());
	Rcpp::NumericVector Drent_out ( Vown_out.length());
	Rcpp::NumericVector vstay_out  ( Vown_out.length());
	Rcpp::NumericVector vsell_out  ( Vown_out.length());
	Rcpp::NumericVector vrent_out  ( Vown_out.length());
	Rcpp::NumericVector vbuy_out   ( Vown_out.length());
	Rcpp::NumericVector sstay_out  ( Vown_out.length());
	Rcpp::NumericVector ssell_out  ( Vown_out.length());
	Rcpp::NumericVector srent_out  ( Vown_out.length());
	Rcpp::NumericVector sbuy_out   ( Vown_out.length());
	Rcpp::NumericVector cstay_out  ( Vown_out.length());
	Rcpp::NumericVector csell_out  ( Vown_out.length());
	Rcpp::NumericVector crent_out  ( Vown_out.length());
	Rcpp::NumericVector cbuy_out   ( Vown_out.length());

	//Rcpp::NumericVector Vown_out2( mig.GetVown().data(), mig.GetVown().size() );
	
	// copy blitz to Rcpp::Numeric
	Vown_out   = mig.GetVown();
	Vrent_out  = mig.GetVrent();
	EVown_out  = mig.GetEVown();
	EVrent_out = mig.GetEVrent();
	Down_out   = mig.GetDown();
	Drent_out  = mig.GetDrent();
	vstay_out  = mig.Getv_stay();
	vsell_out  = mig.Getv_sell();
	vbuy_out   = mig.Getv_buy();
	vrent_out  = mig.Getv_rent();
	sstay_out  = mig.Gets_stay();
	ssell_out  = mig.Gets_sell();
	sbuy_out   = mig.Gets_buy();
	srent_out  = mig.Gets_rent();
	cstay_out  = mig.Getc_stay();
	csell_out  = mig.Getc_sell();
	cbuy_out   = mig.Getc_buy();
	crent_out  = mig.Getc_rent();
	
	// attach dimension argument
	Vown_out.attr("dim")   = dAYPT;
	Vrent_out.attr("dim")  = dAYPT;
	EVown_out.attr("dim")  = dAYPT;
	EVrent_out.attr("dim") = dAYPT;
	Down_out.attr("dim")   = dAYPT;
	Drent_out.attr("dim")  = dAYPT;
	vstay_out.attr("dim")  = dAYPT;
	vsell_out.attr("dim")  = dAYPT;
	vbuy_out.attr("dim")   = dAYPT;
	vrent_out.attr("dim")  = dAYPT;
	sstay_out.attr("dim")  = dAYPT;
	ssell_out.attr("dim")  = dAYPT;
	sbuy_out.attr("dim")   = dAYPT;
	srent_out.attr("dim")  = dAYPT;
	cstay_out.attr("dim")  = dAYPT;
	csell_out.attr("dim")  = dAYPT;
	cbuy_out.attr("dim")   = dAYPT;
	crent_out.attr("dim")  = dAYPT;
	
	// create output list
	Rcpp::List list = Rcpp::List::create( Rcpp::_["Vown"]   = Vown_out,
										  Rcpp::_["Vrent"]  = Vrent_out,
	                                      Rcpp::_["EVown"]  = EVown_out,
										  Rcpp::_["EVrent"] = EVrent_out,
	                                      Rcpp::_["Down"]   = Down_out,
										  Rcpp::_["Drent"]  = Drent_out,
										  Rcpp::_["vstay"]  = vstay_out,
										  Rcpp::_["vsell"]  = vsell_out,
										  Rcpp::_["vbuy"]   = vbuy_out ,
										  Rcpp::_["vrent"]  = vrent_out,
										  Rcpp::_["sstay"]  = sstay_out,
										  Rcpp::_["ssell"]  = ssell_out,
										  Rcpp::_["sbuy"]   = sbuy_out ,
										  Rcpp::_["srent"]  = srent_out,
										  Rcpp::_["cstay"]  = cstay_out,
										  Rcpp::_["csell"]  = csell_out,
										  Rcpp::_["cbuy"]   = cbuy_out ,
										  Rcpp::_["crent"]  = crent_out,
										  Rcpp::_["time"]   = timer);

										   
	return list;
}   



//' dev6: discrete optimization, housing choice
//' 
//' @example examples/example-dev6.r
// [[Rcpp::export]]
Rcpp::List dev6( Rcpp::List data ) {

// start timer
	Rcpp::Timer timer;

	// R array data
	Rcpp::NumericVector R_CO = Rcpp::as<Rcpp::NumericVector>(data["consO"]);
	Rcpp::NumericVector R_CR = Rcpp::as<Rcpp::NumericVector>(data["consR"]);
	Rcpp::NumericVector R_CB = Rcpp::as<Rcpp::NumericVector>(data["consB"]);
	Rcpp::NumericVector R_CS = Rcpp::as<Rcpp::NumericVector>(data["consS"]);
	Rcpp::IntegerVector d    = Rcpp::as<Rcpp::IntegerVector>(data["dims"]);
	Rcpp::NumericVector R_G  = Rcpp::as<Rcpp::NumericVector>(data["G"]);

	// R parameter data
	Parstruc p;
	p.beta = Rcpp::as<double>(data["beta"]);
	p.myNA = Rcpp::as<double>(data["myNA"]);

	// map to blitz arrays
	// ===================
	
	TinyVector<int,5> D_aypta(d(0),d(1),d(2),d(3),d(0));
	TinyVector<int,4> D_aypt(d(0),d(1),d(2),d(3));
	TinyVector<int,4> D_aypa(d(0),d(1),d(2),d(0));
	TinyVector<int,4> D_aypy(d(0),d(1),d(2),d(1));
	TinyVector<int,3> D_ayp(d(0),d(1),d(2));
	TinyVector<int,2> D_y(d(1),d(1));
	Array<double,2> G(R_G.begin(),shape(d(1),d(1)),neverDeleteData,FortranArray<2>());
	Array<double,5> stay(R_CO.begin(),D_aypta,neverDeleteData,FortranArray<5>());
	Array<double,5> rent(R_CR.begin(),D_aypta,neverDeleteData,FortranArray<5>());
	Array<double,5> buy( R_CB.begin(),D_aypta,neverDeleteData,FortranArray<5>());
	Array<double,5> sell(R_CS.begin(),D_aypta,neverDeleteData,FortranArray<5>());

	// create an instance of the migration class
	// ===================================
	
	CMig mig(D_aypta,D_aypt,D_aypa,D_aypy,D_ayp,D_y, &p, stay,sell,rent,buy,G);

	// call the show method
	mig.show();

	// time loop to compute backwards
	// ===================================
	
	for (int it = d(3); it>0; it--) {
		mig.ComputePeriod( it );
	}

	timer.step("end blitz");

	// output results to R
	// ===================
	
	Rcpp::IntegerVector dAYPT = Rcpp::IntegerVector::create(d(0),d(1),d(2),d(3));
	Rcpp::IntegerVector dAYPTA = Rcpp::IntegerVector::create(d(0),d(1),d(2),d(3),d(0));

	Rcpp::NumericVector Vown_out   ( mig.GetVown().size());
	Rcpp::NumericVector Vrent_out  ( Vown_out.length());
	Rcpp::NumericVector EVown_out  ( Vown_out.length());
	Rcpp::NumericVector EVrent_out ( Vown_out.length());
	Rcpp::NumericVector vstay_out  ( Vown_out.length());
	Rcpp::NumericVector vsell_out  ( Vown_out.length());
	Rcpp::NumericVector vrent_out  ( Vown_out.length());
	Rcpp::NumericVector vbuy_out   ( Vown_out.length());
	Rcpp::NumericVector sstay_out  ( Vown_out.length());
	Rcpp::NumericVector ssell_out  ( Vown_out.length());
	Rcpp::NumericVector srent_out  ( Vown_out.length());
	Rcpp::NumericVector sbuy_out   ( Vown_out.length());

	//Rcpp::NumericVector Vown_out2( mig.GetVown().data(), mig.GetVown().size() );
	
	// copy blitz to Rcpp::Numeric
	Vown_out   = mig.GetVown();
	Vrent_out  = mig.GetVrent();
	EVown_out  = mig.GetEVown();
	EVrent_out = mig.GetEVrent();
	vstay_out  = mig.Getv_stay();
	vsell_out  = mig.Getv_sell();
	vbuy_out   = mig.Getv_buy();
	vrent_out  = mig.Getv_rent();
	
	// attach dimension argument
	Vown_out.attr("dim")   = dAYPT;
	Vrent_out.attr("dim")  = dAYPT;
	EVown_out.attr("dim")  = dAYPT;
	EVrent_out.attr("dim") = dAYPT;
	vstay_out.attr("dim")  = dAYPT;
	vsell_out.attr("dim")  = dAYPT;
	vbuy_out.attr("dim")   = dAYPT;
	vrent_out.attr("dim")  = dAYPT;
	
	// create output list
	Rcpp::List list = Rcpp::List::create( Rcpp::_["Vown"]   = Vown_out,
										  Rcpp::_["Vrent"]  = Vrent_out,
	                                      Rcpp::_["EVown"]  = EVown_out,
										  Rcpp::_["EVrent"] = EVrent_out,
										  Rcpp::_["vstay"]  = vstay_out,
										  Rcpp::_["vsell"]  = vsell_out,
										  Rcpp::_["vbuy"]   = vbuy_out ,
										  Rcpp::_["vrent"]  = vrent_out,
										  Rcpp::_["time"]   = timer);

	// TODO want to do
	//Rcpp::List list = mig.RcppExport();
										   
	return list;
}   

//' dev5: discrete optimization, housing choice
//' 
//' @example examples/example-dev5.r
// [[Rcpp::export]]
Rcpp::List dev5( Rcpp::List data ) {

// start timer
	Rcpp::Timer timer;

	// R array data
	Rcpp::NumericVector R_CO = Rcpp::as<Rcpp::NumericVector>(data["consO"]);
	Rcpp::NumericVector R_CR = Rcpp::as<Rcpp::NumericVector>(data["consR"]);
	Rcpp::NumericVector R_CB = Rcpp::as<Rcpp::NumericVector>(data["consB"]);
	Rcpp::NumericVector R_CS = Rcpp::as<Rcpp::NumericVector>(data["consS"]);
	Rcpp::IntegerVector d    = Rcpp::as<Rcpp::IntegerVector>(data["dims"]);

	// R parameter data
	double theta = Rcpp::as<double>(data["theta"]);
	double beta  = Rcpp::as<double>(data["beta"]);
	double myNA  = Rcpp::as<double>(data["myNA"]);

	int ns = d(0);
	int nState = d(0) * d(1) * d(2) * d(3);


	// (conditional) value functions, savings policies and discrete choice
	Array<double,4> VO(d(0),d(1),d(2),d(3),FortranArray<4>());
	Array<double,4> VR(d(0),d(1),d(2),d(3),FortranArray<4>());
	Array<double,4> VB(d(0),d(1),d(2),d(3),FortranArray<4>());
	Array<double,4> VS(d(0),d(1),d(2),d(3),FortranArray<4>());
	VO = 0; VR = 0; VB = 0; VS = 0;

	Array<int,4> saveO(d(0),d(1),d(2),d(3),FortranArray<4>());
	Array<int,4> saveR(d(0),d(1),d(2),d(3),FortranArray<4>());
	Array<int,4> saveB(d(0),d(1),d(2),d(3),FortranArray<4>());
	Array<int,4> saveS(d(0),d(1),d(2),d(3),FortranArray<4>());
	saveO = 0; saveR = 0; saveB = 0; saveS = 0;

	// envelops and Dchoices for renter and owner
	Array<double,4> WO(d(0),d(1),d(2),d(3),FortranArray<4>());
	Array<double,4> WR(d(0),d(1),d(2),d(3),FortranArray<4>());
	Array<int,4> DO(d(0),d(1),d(2),d(3),FortranArray<4>());
	Array<int,4> DR(d(0),d(1),d(2),d(3),FortranArray<4>());
	WO = 0; WR = 0; DO = 0; DR = 0;

	// temopary full tensors x = U + beta V at all (states,savings) combinations by period
	Array<double,4> xO(d(0),d(1),d(2),ns,FortranArray<4>());
	Array<double,4> xR(d(0),d(1),d(2),ns,FortranArray<4>());
	Array<double,4> xB(d(0),d(1),d(2),ns,FortranArray<4>());
	Array<double,4> xS(d(0),d(1),d(2),ns,FortranArray<4>());
	
	// cons tensors: cash - savings at each state
	Array<double,5> CO(R_CO.begin(),shape(d(0),d(1),d(2),d(3),ns),neverDeleteData,FortranArray<5>());
	Array<double,5> CR(R_CR.begin(),shape(d(0),d(1),d(2),d(3),ns),neverDeleteData,FortranArray<5>());
	Array<double,5> CB(R_CB.begin(),shape(d(0),d(1),d(2),d(3),ns),neverDeleteData,FortranArray<5>());
	Array<double,5> CS(R_CS.begin(),shape(d(0),d(1),d(2),d(3),ns),neverDeleteData,FortranArray<5>());

	// ctmp
	Array<double,4> ctmp(d(0),d(1),d(2),ns,FortranArray<4>());

	// vplustmp
	Array<double,3> vplustmp(d(0),d(1),d(2),FortranArray<3>());

	// tensor index descriptors
	firstIndex  i;	
	secondIndex j;  
	thirdIndex  k;	
	fourthIndex l;	

	timer.step("allocate");

	// loop over time
	// final period: where cons is positive, say utility is log(cash)
	
	for (int t=d(3); t>0; t--){

		// if final period
		if (t==d(3)){

			WR(Range::all(),Range::all(),Range::all(),t) =  CR(Range::all(),Range::all(),Range::all(),t,ns) ; 
			WO(Range::all(),Range::all(),Range::all(),t) =  CO(Range::all(),Range::all(),Range::all(),t,ns) ; 

		} else {
			
			// ////////////////////////////
			// future value is owner state:
			// ////////////////////////////

			vplustmp = WO(Range::all(),Range::all(),Range::all(),t+1);
			
			// owner stay
			// ----------
     
			ctmp = CO(Range::all(),Range::all(),Range::all(),t,Range::all());		// consumption at all savings states in period t
			xO   = where(ctmp > 0, log( ctmp(i,j,k,l) ) + beta * vplustmp(l,j,k), myNA );

			VO(Range::all(),Range::all(),Range::all(),t)    = max( xO, l);
			saveO(Range::all(),Range::all(),Range::all(),t) = maxIndex( xO, l);

			// renter buy
			// ----------
     
			ctmp = CB(Range::all(),Range::all(),Range::all(),t,Range::all());		// consumption at all savings states in period t
			xB   = where(ctmp > 0, log( ctmp(i,j,k,l) ) + beta * vplustmp(l,j,k), myNA );

			VB(Range::all(),Range::all(),Range::all(),t)    = max( xB, l);
			saveB(Range::all(),Range::all(),Range::all(),t) = maxIndex( xB, l);

			// ////////////////////////////
			// future value is renter state:
			// ////////////////////////////

			vplustmp = WR(Range::all(),Range::all(),Range::all(),t+1);
		
			// owner sell
			// ----------
     
			ctmp = CS(Range::all(),Range::all(),Range::all(),t,Range::all());		// consumption at all savings states in period t
			xS   = where(ctmp > 0, log( ctmp(i,j,k,l) ) + beta * vplustmp(l,j,k), myNA );

			VS(Range::all(),Range::all(),Range::all(),t)    = max( xS, l);
			saveS(Range::all(),Range::all(),Range::all(),t) = maxIndex( xS, l);

			// renter rent
			// ----------

			ctmp = CR(Range::all(),Range::all(),Range::all(),t,Range::all());		// consumption at all savings states in period t
			xR   = where(ctmp > 0, log( ctmp(i,j,k,l) ) + beta * vplustmp(l,j,k), myNA );

			VR(Range::all(),Range::all(),Range::all(),t)    = max( xR, l);
			saveR(Range::all(),Range::all(),Range::all(),t) = maxIndex( xR, l);
			
			
			// ///////////////////////////////////////////////////
			// find envelopes and discrete choices for both states
			// ///////////////////////////////////////////////////
			
			// find WO
			WO(Range::all(),Range::all(),Range::all(),t) = where(VO(Range::all(),Range::all(),Range::all(),t) > VS(Range::all(),Range::all(),Range::all(),t),
				                                                VO(Range::all(),Range::all(),Range::all(),t), 
																VS(Range::all(),Range::all(),Range::all(),t));
			// find DO
			DO(Range::all(),Range::all(),Range::all(),t) = where(VO(Range::all(),Range::all(),Range::all(),t) > VS(Range::all(),Range::all(),Range::all(),t),
				                                                1, 
																2);
			
			// find WR
			WR(Range::all(),Range::all(),Range::all(),t) = where(VR(Range::all(),Range::all(),Range::all(),t) > VB(Range::all(),Range::all(),Range::all(),t),
				                                                VR(Range::all(),Range::all(),Range::all(),t), 
																VB(Range::all(),Range::all(),Range::all(),t));
			// find DR
			DR(Range::all(),Range::all(),Range::all(),t) = where(VR(Range::all(),Range::all(),Range::all(),t) > VB(Range::all(),Range::all(),Range::all(),t),
				                                                1, 
																2);

		}

	}
	timer.step("maximize");

	// map to output objects
	Rcpp::IntegerVector dAYPT = Rcpp::IntegerVector::create(d(0),d(1),d(2),d(3));
	Rcpp::IntegerVector dAYPTA = Rcpp::IntegerVector::create(d(0),d(1),d(2),d(3),ns);

	//
	// envelopes and discrete choice functions
	Rcpp::NumericVector WOout(WO.size());
	Rcpp::NumericVector DOout(WO.size());
	Rcpp::NumericVector WRout(WR.size());
	Rcpp::NumericVector DRout(WR.size());

	WOout = WO;
	DOout = DO;
	WRout = WR;
	DRout = DR;

	WOout.attr("dim") = dAYPT;
	DOout.attr("dim") = dAYPT;
	WRout.attr("dim") = dAYPT;
	DRout.attr("dim") = dAYPT;

	// consumption values at each state
	Rcpp::NumericVector CO_out(CO.size());
	Rcpp::NumericVector CR_out(CR.size());
	Rcpp::NumericVector CB_out(CB.size());
	Rcpp::NumericVector CS_out(CS.size());

	CO_out = CO;
	CR_out = CR;
	CB_out = CB;
	CS_out = CS;

	CO_out.attr("dim") = dAYPTA;
	CR_out.attr("dim") = dAYPTA;
	CB_out.attr("dim") = dAYPTA;
	CS_out.attr("dim") = dAYPTA;

	// conditional value functions
	Rcpp::NumericVector VOout(VO.size());
	Rcpp::NumericVector VRout(VR.size());
	Rcpp::NumericVector VBout(VB.size());
	Rcpp::NumericVector VSout(VS.size());

	VOout = VO;
	VRout = VR;
	VBout = VB;
	VSout = VS;

	VOout.attr("dim") = dAYPT;
	VRout.attr("dim") = dAYPT;
	VBout.attr("dim") = dAYPT;
	VSout.attr("dim") = dAYPT;

	// savings function
	// conditional savings functions
	Rcpp::NumericVector saveOout(saveO.size());
	Rcpp::NumericVector saveRout(saveR.size());
	Rcpp::NumericVector saveBout(saveB.size());
	Rcpp::NumericVector saveSout(saveS.size());

	saveOout = saveO;
	saveRout = saveR;
	saveBout = saveB;
	saveSout = saveS;

	saveOout.attr("dim") = dAYPT;
	saveRout.attr("dim") = dAYPT;
	saveBout.attr("dim") = dAYPT;
	saveSout.attr("dim") = dAYPT;

	timer.step("out map");
	Rcpp::NumericVector time(timer);

	// create output list
	Rcpp::List list = Rcpp::List::create( Rcpp::_["WO"]    = WOout, 
			                              Rcpp::_["DO"]    = DOout,
			                              Rcpp::_["WR"]    = WRout,
			                              Rcpp::_["DR"]    = DRout,
										  Rcpp::_["CO"]    = CO_out,
										  Rcpp::_["CR"]    = CR_out,
										  Rcpp::_["CB"]    = CB_out,
										  Rcpp::_["CS"]    = CS_out,
										  Rcpp::_["saveO"] = saveOout,
										  Rcpp::_["saveR"] = saveRout,
										  Rcpp::_["saveB"] = saveBout,
										  Rcpp::_["saveS"] = saveSout,
										  Rcpp::_["VO"]    = VOout,
										  Rcpp::_["VR"]    = VRout,
										  Rcpp::_["VB"]    = VBout,
										  Rcpp::_["VS"]    = VSout,
										  Rcpp::_["time"]  = time);
    return list;
}   




//' dev4
//'
//' @example examples/example-dev4.r
// [[Rcpp::export]]
Rcpp::List dev4( Rcpp::List data ) {

// current example: v(a,y,t) = max(v1(a,y,t),v2(a,y,t))
// build consumption, evaluate utility


	// R array data
	Rcpp::NumericVector R_C1 = Rcpp::as<Rcpp::NumericVector>(data["cash1"]);
	Rcpp::NumericVector R_C2 = Rcpp::as<Rcpp::NumericVector>(data["cash2"]);
	Rcpp::NumericVector R_S  = Rcpp::as<Rcpp::NumericVector>(data["savings"]);
	Rcpp::IntegerVector d    = Rcpp::as<Rcpp::IntegerVector>(data["dims"]);

	// R parameter data
	double theta = Rcpp::as<double>(data["theta"]);
	double beta  = Rcpp::as<double>(data["beta"]);
	double myNA  = Rcpp::as<double>(data["myNA"]);

	int ns = R_S.size();
	int nState = d(0) * d(1) * d(2);


	// (conditional) value functions, savings policies and discrete choice
	Array<double,3> V(d(0),d(1),d(2),FortranArray<3>());
	Array<double,3> D(d(0),d(1),d(2),FortranArray<3>());
	Array<double,3> v1(d(0),d(1),d(2),FortranArray<3>());
	Array<double,3> v2(d(0),d(1),d(2),FortranArray<3>());
	Array<double,3> s1(d(0),d(1),d(2),FortranArray<3>());
	Array<double,3> s2(d(0),d(1),d(2),FortranArray<3>());
	V  = 0;
	D  = 0;
	v1 = 0;
	v2 = 0;
	s1 = 0;
	s2 = 0;

	// cash tensors
	Array<double,3> Cash1(R_C1.begin(), shape(d(0),d(1),d(2)),neverDeleteData,FortranArray<3>());
	Array<double,3> Cash2(R_C2.begin(), shape(d(0),d(1),d(2)),neverDeleteData,FortranArray<3>());

	// savings tensor (just a vector)
	Array<double,1> S( R_S.begin(),R_S.length(),neverDeleteData,FortranArray<1>() );

	// consumption tensor and ctmp
	Array<double,4> Cons1(d(0),d(1),d(2),ns,FortranArray<4>());
	Array<double,4> Cons2(d(0),d(1),d(2),ns,FortranArray<4>());
	Array<double,3> ctmp(d(0),d(1),ns,FortranArray<3>());
	Cons1 = 0;
	Cons2 = 0;

	// full value tensor and tmp
	Array<double,3> w(d(0),d(1),ns,FortranArray<3>());
	Array<double,2> vtmp1(d(0),d(1),FortranArray<2>());
	Array<double,2> vtmp2(d(0),d(1),FortranArray<2>());
	Array<double,2> stmp(d(0),d(1),FortranArray<2>());
	Array<double,2> vplustmp(d(0),d(1),FortranArray<2>());
	w        = 0;
	vtmp1    = 0;
	vtmp2    = 0;
	vplustmp = 0;


	// tensor index descriptors
	firstIndex  i;	
	secondIndex j;  
	thirdIndex  k;	
	fourthIndex l;	

	Cons1 = Cash1(i,j,k) - S(l);
	Cons2 = Cash2(i,j,k) - S(l);

	// loop over time
	// final period: where cons is positive, say utility is log(cash)
	
	for (int t=d(2); t>0; t--){

		// if final period
		if (t==d(2)){

			V(Range::all(),Range::all(),t) = log( Cash1(Range::all(),Range::all(),t) ); 

		} else {
			
			vplustmp = V(Range::all(),Range::all(),t+1);
			
			// option 1
			ctmp     = Cons1(Range::all(),Range::all(),t,Range::all());		// consumption at all savings states in period t

			w        = where(ctmp > 0, log( ctmp(i,j,k) ) + beta * vplustmp(k,j), myNA );
			vtmp1    = max( w, k);
			stmp     = maxIndex( w, k);
			// unnecessary
			v1(Range::all(),Range::all(),t) = vtmp1;
			s1(Range::all(),Range::all(),t) = stmp;
			
			// option 2
			ctmp     = Cons2(Range::all(),Range::all(),t,Range::all());		// consumption at all savings states in period t

			w        = where(ctmp > 0, log( ctmp(i,j,k) ) + beta * vplustmp(k,j), myNA );
			vtmp2    = max( w, k);
			stmp     = maxIndex( w, k);
			// unnecessary
			v2(Range::all(),Range::all(),t) = vtmp2;
			s2(Range::all(),Range::all(),t) = stmp;
		
			// find vmax
			V(Range::all(),Range::all(),t) = where(vtmp1 > vtmp2, vtmp1, vtmp2);
			// find dchoice
			D(Range::all(),Range::all(),t) = where(vtmp1 > vtmp2, 1, 2);

		}

	}

	// map to output objects
	//
	// Maximal value function and discrete choice function
	Rcpp::NumericVector Vout(nState);
	Rcpp::NumericVector Dout(nState);

	Vout = V;
	Dout = D;
	Vout.attr("dim") = Rcpp::IntegerVector::create(d(0),d(1),d(2));
	Dout.attr("dim") = Rcpp::IntegerVector::create(d(0),d(1),d(2));

	// consumption functions
	Rcpp::NumericVector C1_out(Cons1.size());
	Rcpp::NumericVector C2_out(Cons2.size());

	C1_out = Cons1;
	C2_out = Cons2;
	C1_out.attr("dim") = Rcpp::IntegerVector::create(d(0),d(1),d(2),ns);
	C2_out.attr("dim") = Rcpp::IntegerVector::create(d(0),d(1),d(2),ns);
	
	// conditional value functions
	Rcpp::NumericVector v1_out(v1.size());
	Rcpp::NumericVector v2_out(v2.size());

	v1_out = v1;
	v2_out = v2;
	v1_out.attr("dim") = Rcpp::IntegerVector::create(d(0),d(1),d(2));
	v2_out.attr("dim") = Rcpp::IntegerVector::create(d(0),d(1),d(2));

	// savings function
	Rcpp::NumericVector s1_out(s1.size());
	Rcpp::NumericVector s2_out(s2.size());

	s1_out = s1;
	s2_out = s2;
	s1_out.attr("dim") = Rcpp::IntegerVector::create(d(0),d(1),d(2));
	s2_out.attr("dim") = Rcpp::IntegerVector::create(d(0),d(1),d(2));

	// create output list
	Rcpp::List list = Rcpp::List::create( Rcpp::_["V"] = Vout, 
			                              Rcpp::_["D"] = Dout, 
										  Rcpp::_["cons1"] = C1_out,
										  Rcpp::_["cons2"] = C2_out,
										  Rcpp::_["save1"] = s1_out,
										  Rcpp::_["save2"] = s2_out,
										  Rcpp::_["v1"] = v1_out,
										  Rcpp::_["v2"] = v2_out);
    return list;
}   



//' dev3H
//'
//' @example examples/example-dev3H.r
// [[Rcpp::export]]
Rcpp::List dev3H( Rcpp::List data ) {

// current example: v(a,y,t) = max(v1(a,y,t),v2(a,y,t))
// build consumption, evaluate utility


	// R array data
	Rcpp::NumericVector R_C1 = Rcpp::as<Rcpp::NumericVector>(data["cash1"]);
	Rcpp::NumericVector R_S  = Rcpp::as<Rcpp::NumericVector>(data["savings"]);
	Rcpp::IntegerVector d    = Rcpp::as<Rcpp::IntegerVector>(data["dims"]);

	// R parameter data
	double theta = Rcpp::as<double>(data["theta"]);
	double beta  = Rcpp::as<double>(data["beta"]);
	double myNA  = Rcpp::as<double>(data["myNA"]);

	int ns = R_S.size();
	int nState = d(0) * d(1) * d(2);

	// out objects 
	Rcpp::NumericVector Vout(nState);
	Rcpp::NumericVector Dout(nState);

	// conditional value functions
	Array<double,3> V1(d(0),d(1),d(2),FortranArray<3>());
	V1 = 0;

	// cash tensors
	Array<double,3> Cash1(R_C1.begin(), shape(d(0),d(1),d(2)),neverDeleteData,FortranArray<3>());

	// savings tensor (just a vector)
	Array<double,1> S( R_S.begin(),R_S.length(),neverDeleteData,FortranArray<1>() );

	// consumption tensor and ctmp
	Array<double,4> Cons1(d(0),d(1),d(2),ns,FortranArray<4>());
	Array<double,3> ctmp(d(0),d(1),ns,FortranArray<3>());
	Cons1 = 0;

	// full value tensor and tmp
	Array<double,3> w(d(0),d(1),ns,FortranArray<3>());
	Array<double,2> vtmp(d(0),d(1),FortranArray<2>());
	Array<double,2> vplustmp(d(0),d(1),FortranArray<2>());
	w        = 0;
	vtmp     = 0;
	vplustmp = 0;


	// tensor index descriptors
	firstIndex  i;	
	secondIndex j;  
	thirdIndex  k;	
	fourthIndex l;	

	Cons1 = Cash1(i,j,k) - S(l);

	// loop over time
	// final period: where cons is positive, say utility is log(cash)
	
	for (int t=d(2); t>0; t--){

		// if final period
		if (t==d(2)){

			V1(Range::all(),Range::all(),t) = log( Cash1(Range::all(),Range::all(),t) ); 

		} else {

			ctmp     = Cons1(Range::all(),Range::all(),t,Range::all());		// consumption at all savings states in period t
			vplustmp = V1(Range::all(),Range::all(),t+1);

			// where consumption is positive, assign value, 
			// where not, assign NA
			w        = where(ctmp > 0, log( ctmp(i,j,k) ) + beta * vplustmp(k,j), myNA );

			// take max over third dimension of w
			V1(Range::all(),Range::all(),t) = max( w, k);

		}

	}

	// map to output objects
	//
	Rcpp::NumericVector C_out(Cons1.size());

	C_out = Cons1;
	C_out.attr("dim") = Rcpp::IntegerVector::create(d(0),d(1),d(2),ns);
	
	Rcpp::NumericVector Out2(w.size());
	Rcpp::NumericVector Out(V1.size());
	Out = V1;
	Out2 = w;
    
	Out.attr("dim") = Rcpp::IntegerVector::create(d(0),d(1),d(2));
	Out2.attr("dim") = Rcpp::IntegerVector::create(d(0),d(1),ns);

	// create output list
	Rcpp::List list = Rcpp::List::create( Rcpp::_["V1"] = Out, Rcpp::_["w"] = Out2, Rcpp::_["cons"] = C_out);
    return list;
}   




//' dev3
//'
//' @example examples/example-dev3.r
// [[Rcpp::export]]
Rcpp::List dev3( Rcpp::List data ) {

// current example: v(a,y,t) = max(v1(a,y,t),v2(a,y,t))
// build consumption, evaluate utility


	// R array data
	Rcpp::NumericVector R_C1 = Rcpp::as<Rcpp::NumericVector>(data["cash1"]);
	Rcpp::NumericVector R_S  = Rcpp::as<Rcpp::NumericVector>(data["savings"]);
	Rcpp::IntegerVector d    = Rcpp::as<Rcpp::IntegerVector>(data["dims"]);

	// R parameter data
	double theta = Rcpp::as<double>(data["theta"]);
	double beta  = Rcpp::as<double>(data["beta"]);
	double myNA  = Rcpp::as<double>(data["myNA"]);

	int ns = R_S.size();
	int nState = d(0) * d(1) * d(2);

	// out objects 
	Rcpp::NumericVector Vout(nState);
	Rcpp::NumericVector Dout(nState);

	// conditional value functions
	Array<double,3> V1(d(0),d(1),d(2),FortranArray<3>());
	V1 = 0;

	// cash tensors
	Array<double,3> Cash1(R_C1.begin(), shape(d(0),d(1),d(2)),neverDeleteData,FortranArray<3>());

	// savings tensor (just a vector)
	Array<double,1> S( R_S.begin(),R_S.length(),neverDeleteData,FortranArray<1>() );

	// consumption tensor and ctmp
	Array<double,4> Cons1(d(0),d(1),d(2),ns,FortranArray<4>());
	Array<double,3> ctmp(d(0),d(1),ns,FortranArray<3>());
	Cons1 = 0;

	// full value tensor and tmp
	Array<double,3> w(d(0),d(1),ns,FortranArray<3>());
	Array<double,2> vtmp(d(0),d(1),FortranArray<2>());
	Array<double,2> vplustmp(d(0),d(1),FortranArray<2>());
	w        = 0;
	vtmp     = 0;
	vplustmp = 0;


	// tensor index descriptors
	firstIndex  i;	
	secondIndex j;  
	thirdIndex  k;	
	fourthIndex l;	

	Cons1 = Cash1(i,j,k) - S(l);

	// loop over time
	// final period: where cons is positive, say utility is log(cash)
	
	for (int t=d(2); t>0; t--){

		// if final period
		if (t==d(2)){

			V1(Range::all(),Range::all(),t) = log( Cash1(Range::all(),Range::all(),t) ); 

		} else {

			ctmp     = Cons1(Range::all(),Range::all(),t,Range::all());		// consumption at all savings states in period t
			vplustmp = V1(Range::all(),Range::all(),t+1);

			// where consumption is positive, assign value, 
			// where not, assign NA
			w        = where(ctmp > 0, log( ctmp(i,j,k) ) + beta * vplustmp(k,j), myNA );

			// take max over third dimension of w
			V1(Range::all(),Range::all(),t) = max( w, k);

		}

	}

	// map to output objects
	//
	Rcpp::NumericVector C_out(Cons1.size());

	C_out = Cons1;
	C_out.attr("dim") = Rcpp::IntegerVector::create(d(0),d(1),d(2),ns);
	
	Rcpp::NumericVector Out2(w.size());
	Rcpp::NumericVector Out(V1.size());
	Out = V1;
	Out2 = w;
    
	Out.attr("dim") = Rcpp::IntegerVector::create(d(0),d(1),d(2));
	Out2.attr("dim") = Rcpp::IntegerVector::create(d(0),d(1),ns);

	// create output list
	Rcpp::List list = Rcpp::List::create( Rcpp::_["V1"] = Out, Rcpp::_["w"] = Out2, Rcpp::_["cons"] = C_out);
    return list;
}   



//' dev2
//'
//' @example examples/example-dev2.r
// [[Rcpp::export]]
Rcpp::List dev2( Rcpp::List data ) {

// current example: v(a,y,t) = max(v1(a,y,t),v2(a,y,t))
// build consumption, evaluate utility


	// R array data
	Rcpp::NumericVector R_C1 = Rcpp::as<Rcpp::NumericVector>(data["cash1"]);
	Rcpp::NumericVector R_C2 = Rcpp::as<Rcpp::NumericVector>(data["cash2"]);
	Rcpp::NumericVector R_S  = Rcpp::as<Rcpp::NumericVector>(data["savings"]);
	Rcpp::IntegerVector d    = Rcpp::as<Rcpp::IntegerVector>(data["dims"]);

	// R parameter data
	double theta = Rcpp::as<double>(data["theta"]);
	double beta  = Rcpp::as<double>(data["beta"]);
	double myNA  = Rcpp::as<double>(data["myNA"]);

	int ns = R_S.size();
	int nState = d(0) * d(1) * d(2);

	// out objects 
	Rcpp::NumericVector Vout(nState);
	Rcpp::NumericVector Dout(nState);

	// conditional value functions
	Array<double,3> V1(d(0),d(1),d(2),FortranArray<3>());
	Array<double,3> V2(d(0),d(1),d(2),FortranArray<3>());
	V1 = 0;
	V2 = 0;

	// cash tensors
	Array<double,3> Cash1(R_C1.begin(), shape(d(0),d(1),d(2)),neverDeleteData,FortranArray<3>());
	Array<double,3> Cash2(R_C2.begin(), shape(d(0),d(1),d(2)),neverDeleteData,FortranArray<3>());

	// savings tensor (just a vector)
	Array<double,1> S( R_S.begin(),R_S.length(),neverDeleteData,FortranArray<1>() );

	// consumption tensor
	Array<double,4> Cons1(d(0),d(1),d(2),ns,FortranArray<4>());
	Array<double,4> Cons2(d(0),d(1),d(2),ns,FortranArray<4>());
	Cons1 = 0;
	Cons2 = 0;


	// tensor index descriptors
	firstIndex  i;
	secondIndex j;
	thirdIndex  k;
	fourthIndex l;

	Cons1 = Cash1(i,j,k) - S(l);
	Cons2 = Cash2(i,j,k) - S(l);

	Rcpp::NumericVector Out(Cons1.size());

	// loop over time
	// final period: where cons is positive, say utility is log(cash)
	
	for (int t=d(2); t>0; t--){

		// if final period
		// V(a,y,T) = log( cash(a,y,T) )
		if (t==d(2)){

			V1(Range::all(),Range::all(),t) = log(Cash1(Range::all(),Range::all(),t));

		// else
		// if cash(a,y,t) > 0: V(a,y,t) = log( cash(a,y,t) ) + beta * V(a,y,t+1)
		// if cash(a,y,t) < 0: V(a,y,t) = myNA               + beta * V(a,y,t+1)
		} else {

			V1(Range::all(),Range::all(),t) = where( Cash1(Range::all(),Range::all(),t) > 0, 
													 log(Cash1(Range::all(),Range::all(),t)), 
													 myNA ) 
				                              + beta * V1(Range::all(),Range::all(),t+1);

		}

	}

	Rcpp::NumericVector Out2(V1.size());
	Out2 = V1;

	Out2.attr("dim") = Rcpp::IntegerVector::create(d(0),d(1),d(2));
	Rcpp::List list = Rcpp::List::create( Rcpp::_["V1"] = Out2);
	return list;
}



//' dev2a
//'
//' @example examples/example-dev2a.r
// [[Rcpp::export]]
Rcpp::List dev2a( Rcpp::List data ) {

// current example: v(a,y,t) = max(v1(a,y,t),v2(a,y,t))
// build consumption, evaluate utility


	// R array data
	Rcpp::NumericVector R_C1 = Rcpp::as<Rcpp::NumericVector>(data["cash1"]);
	Rcpp::NumericVector R_C2 = Rcpp::as<Rcpp::NumericVector>(data["cash2"]);
	Rcpp::NumericVector R_S  = Rcpp::as<Rcpp::NumericVector>(data["savings"]);
	Rcpp::IntegerVector d    = Rcpp::as<Rcpp::IntegerVector>(data["dims"]);

	// R parameter data
	double theta = Rcpp::as<double>(data["theta"]);
	double beta  = Rcpp::as<double>(data["beta"]);
	double myNA  = Rcpp::as<double>(data["myNA"]);

	int ns = R_S.size();
	int nState = d(0) * d(1) * d(2);

	// out objects 
	Rcpp::NumericVector Vout(nState);
	Rcpp::NumericVector Dout(nState);

	// conditional value functions
	Array<double,3> V1(d(0),d(1),d(2));
	Array<double,3> V2(d(0),d(1),d(2));
	V1 = 0;
	V2 = 0;

	// cash tensors
	Array<double,3> Cash1(R_C1.begin(), shape(d(0),d(1),d(2)),neverDeleteData);
	Array<double,3> Cash2(R_C2.begin(), shape(d(0),d(1),d(2)),neverDeleteData);

	// savings tensor (just a vector)
	Array<double,1> S( R_S.begin(),R_S.length(),neverDeleteData);

	// consumption tensor
	Array<double,4> Cons1(d(0),d(1),d(2),ns);
	Array<double,4> Cons2(d(0),d(1),d(2),ns);
	Cons1 = 0;
	Cons2 = 0;


	// tensor index descriptors
	firstIndex  i;
	secondIndex j;
	thirdIndex  k;
	fourthIndex l;

	Cons1 = Cash1(i,j,k) - S(l);
	Cons2 = Cash2(i,j,k) - S(l);

	Rcpp::NumericVector Out(Cons1.size());

	// loop over time
	// final period: where cons is positive, say utility is log(cash)
	
	for (int t=d(2); t>0; t--){

		// if final period
		// V(a,y,T) = log( cash(a,y,T) )
		if (t==d(2)){

			V1(Range::all(),Range::all(),t) = log(Cash1(Range::all(),Range::all(),t));

		// else
		// if cash(a,y,t) > 0: V(a,y,t) = log( cash(a,y,t) ) + beta * V(a,y,t+1)
		// if cash(a,y,t) < 0: V(a,y,t) = myNA               + beta * V(a,y,t+1)
		} else {

			V1(Range::all(),Range::all(),t) = where( Cash1(Range::all(),Range::all(),t) > 0, 
													 log(Cash1(Range::all(),Range::all(),t)), 
													 myNA ) 
				                              + beta * V1(Range::all(),Range::all(),t+1);

		}

	}

	Rcpp::NumericVector Out2(V1.size());
	Out2 = V1;

	Rcpp::List list = Rcpp::List::create( Rcpp::_["V1"] = Out2);
	return list;
}



//' dev1
//'
//' @example examples/example-dev1.r
// [[Rcpp::export]]
Rcpp::List dev1( Rcpp::List data ) {

// current example: v(a,y,t) = max(v1(a,y,t),v2(a,y,t))
// build consumption, evaluate utility, maximize w.r.t. savings


	// R array data
	Rcpp::NumericVector R_C1 = Rcpp::as<Rcpp::NumericVector>(data["cash1"]);
	Rcpp::NumericVector R_C2 = Rcpp::as<Rcpp::NumericVector>(data["cash2"]);
	Rcpp::NumericVector R_S  = Rcpp::as<Rcpp::NumericVector>(data["savings"]);
	Rcpp::IntegerVector d    = Rcpp::as<Rcpp::IntegerVector>(data["dims"]);

	// R parameter data
	double theta = Rcpp::as<double>(data["theta"]);

	int ns = R_S.size();
	int nState = d(0) * d(1) * d(2);

	// out objects 
	Rcpp::NumericVector Vout(nState);
	Rcpp::NumericVector Dout(nState);

	// conditional value functions
	Array<double,3> V1(d(0),d(1),d(2),FortranArray<3>());
	Array<double,3> V2(d(0),d(1),d(2),FortranArray<3>());
	V1 = 0;
	V2 = 0;

	// temporary value functions (no time index)
	Array<double,2> vtmp(d(0),d(1),FortranArray<2>());
	vtmp = 0;
	

	// cash tensors
	Array<double,3> Cash1(R_C1.begin(), shape(d(0),d(1),d(2)),neverDeleteData,FortranArray<3>());
	Array<double,3> Cash2(R_C2.begin(), shape(d(0),d(1),d(2)),neverDeleteData,FortranArray<3>());

	// savings tensor (just a vector)
	Array<double,1> S( R_S.begin(),R_S.length(),neverDeleteData,FortranArray<1>() );

	// consumption tensor
	Array<double,4> Cons1(d(0),d(1),d(2),ns,FortranArray<4>());
	Array<double,4> Cons2(d(0),d(1),d(2),ns,FortranArray<4>());
	Cons1 = 0;
	Cons2 = 0;


	firstIndex  i;
	secondIndex j;
	thirdIndex  k;
	fourthIndex l;

	Cons1 = Cash1(i,j,k) - S(l);
	Cons2 = Cash2(i,j,k) - S(l);

	Rcpp::NumericVector Out(Cons1.size());

	// loop over time
	// final period: where cons is positive, say utility is log(cash)
	int t = 3;
	vtmp = Cash1(Range::all(),Range::all(),t);

	//V1(Range::all(),Range::all(),t) = where(vtmp > 0, log(vtmp), -99);
	V1(Range::all(),Range::all(),t) = log(vtmp);
	
	// Notice that Rcout prints the elements in col order, 
	// that's why this is wrong.
//	Rcpp::Rcout << "cash( , ,3) :" << endl;
//	Rcpp::Rcout << vtmp  << endl;

//	Rcpp::Rcout << "V1 :" << endl;
//	Rcpp::Rcout << V1  << endl;

	// if we map it to a numeric vector with appropriate dim args, its correct.
	Rcpp::NumericVector Out2(V1.size());
	Out2 = V1;

	Out = Cons1;

	Out.attr("dim") = Rcpp::IntegerVector::create(d(0),d(1),d(2),ns);
	Out2.attr("dim") = Rcpp::IntegerVector::create(d(0),d(1),d(2));
	Rcpp::List list = Rcpp::List::create( Rcpp::_["vmax"] = Out,Rcpp::_["V1"] = Out2);
	return list;
}





//' devC
//'
//' @example examples/example-devC.r
// [[Rcpp::export]]
Rcpp::List devC( Rcpp::List data ) {

// current example: v(a,y,t) = max(v1(a,y,t),v2(a,y,t))


	Rcpp::NumericVector R_C1 = Rcpp::as<Rcpp::NumericVector>(data["cash1"]);
	Rcpp::NumericVector R_C2 = Rcpp::as<Rcpp::NumericVector>(data["cash2"]);
	Rcpp::NumericVector R_S  = Rcpp::as<Rcpp::NumericVector>(data["savings"]);
	Rcpp::IntegerVector d    = Rcpp::as<Rcpp::IntegerVector>(data["dims"]);

	int ns = R_S.size();

	// out objects 
	Rcpp::NumericVector Vout(d(0) * d(1) * d(2));
	Rcpp::NumericVector Dout(d(0) * d(1) * d(2));

	// conditional value functions
	Array<double,3> V1(d(0),d(1),d(2),FortranArray<3>());
	Array<double,3> V2(d(0),d(1),d(2),FortranArray<3>());

	// cash tensors
	Array<double,3> Cash1(R_C1.begin(), shape(d(0),d(1),d(2)),neverDeleteData,FortranArray<3>());
	Array<double,3> Cash2(R_C2.begin(), shape(d(0),d(1),d(2)),neverDeleteData,FortranArray<3>());

	// savings tensor (just a vector)
	Array<double,1> S( R_S.begin(),R_S.length(),neverDeleteData,FortranArray<1>() );

	// consumption tensor
	Array<double,4> Cons1(d(0),d(1),d(2),ns,FortranArray<4>());
	Array<double,4> Cons2(d(0),d(1),d(2),ns,FortranArray<4>());
	Cons1 = 0;
	Cons2 = 0;

	firstIndex  i;
	secondIndex j;
	thirdIndex  k;
	fourthIndex l;

	Cons1 = Cash1(i,j,k) - S(l);
	Cons2 = Cash2(i,j,k) - S(l);

	Rcpp::NumericVector Out(Cons1.size());

	Out = Cons1;

	Out.attr("dim") = Rcpp::IntegerVector::create(d(0),d(1),d(2),ns);
	Rcpp::List list = Rcpp::List::create( Rcpp::_["vmax"] = Out);
	return list;
}
