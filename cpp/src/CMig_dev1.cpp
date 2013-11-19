

#ifdef RcppCompile
#include "../../cpp/src/CMig_dev1.h"
#else
#include "CMig_dev1.h"		// include class declaration of owner class
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

// default constructor
CMig::CMig():

	// this is syntactic sugar. an initialiser list. 

	ResStay(2,2,2,2,2,FortranArray<5>()),
    ResSell(2,2,2,2,2,FortranArray<5>()),
	ResRent(2,2,2,2,2,FortranArray<5>()),
    ResBuy( 2,2,2,2,2,FortranArray<5>()),

    v_stay(2,2,2,2,FortranArray<4>()),	
    v_sell(2,2,2,2,FortranArray<4>()),  
    v_rent(2,2,2,2,FortranArray<4>()),	
    v_buy( 2,2,2,2,FortranArray<4>()),   
	c_stay(2,2,2,2,FortranArray<4>()),	
	c_sell(2,2,2,2,FortranArray<4>()),  
	c_rent(2,2,2,2,FortranArray<4>()),	
	c_buy( 2,2,2,2,FortranArray<4>()),   
	Vown(  2,2,2,2,FortranArray<4>()),	
	Vrent( 2,2,2,2,FortranArray<4>()),	
	EVown( 2,2,2,2,FortranArray<4>()),	
	EVrent(2,2,2,2,FortranArray<4>()),	
	ctmp(  2,2,2,2,FortranArray<4>()),  	
	xtmp(  2,2,2,2,FortranArray<4>()),  	

	s_stay(2,2,2,2,FortranArray<4>()),  
	s_sell(2,2,2,2,FortranArray<4>()),  
	s_rent(2,2,2,2,FortranArray<4>()),  
	s_buy( 2,2,2,2,FortranArray<4>()),  
	Down(  2,2,2,2,FortranArray<4>()),  
	Drent( 2,2,2,2,FortranArray<4>()),  
	
	vplustmp(2,2,2,FortranArray<3>()), 
    dim_aypt(2,2,2,2) ,				   
    dim_aypy(2,2,2,2) ,				   
    dim_ayp(2,2,2), 				    
	G(2,2,FortranArray<2>()) {
		ResStay = 0.1;
		ResSell = 0.2;
		ResRent = 0.3;
		ResBuy = 0.4;
		v_stay = 1;
		v_sell = 2;
		v_rent = 3;
		v_buy = 4;
	    G = 0.9,0.3,0.1,0.7;
		p.myNA = -99;
		p.beta = 0.9;
}

CMig::CMig(int x1,int x2, int x3,int x4, int x5):

	// this is syntactic sugar. an initialiser list. 

	ResStay(x1,x2,x3,x4,x5,FortranArray<5>()),
    ResSell(x1,x2,x3,x4,x5,FortranArray<5>()),
	ResRent(x1,x2,x3,x4,x5,FortranArray<5>()),
    ResBuy( x1,x2,x3,x4,x5,FortranArray<5>()),

    v_stay(x1,x2,x3,x4,FortranArray<4>()),	
    v_sell(x1,x2,x3,x4,FortranArray<4>()),  
    v_rent(x1,x2,x3,x4,FortranArray<4>()),	
    v_buy( x1,x2,x3,x4,FortranArray<4>()),   
	c_stay(x1,x2,x3,x4,FortranArray<4>()),	
	c_sell(x1,x2,x3,x4,FortranArray<4>()),  
	c_rent(x1,x2,x3,x4,FortranArray<4>()),	
	c_buy( x1,x2,x3,x4,FortranArray<4>()),   
	Vown(  x1,x2,x3,x4,FortranArray<4>()),	
	Vrent( x1,x2,x3,x4,FortranArray<4>()),	
	EVown( x1,x2,x3,x4,FortranArray<4>()),	
	EVrent(x1,x2,x3,x4,FortranArray<4>()),	
	ctmp(  x1,x2,x3,x4,FortranArray<4>()),  	
	xtmp(  x1,x2,x3,x4,FortranArray<4>()),  	

	s_stay(x1,x2,x3,x4,FortranArray<4>()),  
	s_sell(x1,x2,x3,x4,FortranArray<4>()),  
	s_rent(x1,x2,x3,x4,FortranArray<4>()),  
	s_buy( x1,x2,x3,x4,FortranArray<4>()),  
	Down(  x1,x2,x3,x4,FortranArray<4>()),  
	Drent( x1,x2,x3,x4,FortranArray<4>()),  
	
	vplustmp(x1,x2,x3,FortranArray<3>()), 
    dim_aypt(x1,x2,x3,x4) ,				   
    dim_aypy(x1,x2,x3,x4) ,				   
    dim_ayp(x1,x2,x3), 				    
	G(x2,x2,FortranArray<2>()) {
		ResStay   = 0.1;
		ResSell   = 0.2;
		ResRent   = 0.3;
		ResBuy    = 0.4;
		v_stay    = 1;
		v_sell    = 2;
		v_rent    = 3;
		v_buy     = 4;
	    G         = 0.9,0.3,0.1,0.7;
		p.myNA    = -99;
		p.beta    = 0.9;
		p.theta   = 0.1;
		p.gamma   = 1.4;
		p.mgamma  = 1-1.4;
		p.imgamma = 1/(1-1.4);
}


// constructor 3: data referenced 
CMig::CMig(TinyVector<int,5> D_aypta,
				 TinyVector<int,4> D_aypt, 
	             TinyVector<int,4> D_aypa, 
	             TinyVector<int,4> D_aypy, 
	             TinyVector<int,3> D_ayp, 
   				 TinyVector<int,2> D_y, 
				 Parstruc* pars,
				 Array<double,5> data_stay,
				 Array<double,5> data_sell,
				 Array<double,5> data_rent,
				 Array<double,5> data_buy,
	             Array<double,2> data_G	) :

	// this is syntactic sugar. an initialiser list. 

	ResStay(D_aypta,FortranArray<5>()),
    ResSell(D_aypta,FortranArray<5>()),
	ResRent(D_aypta,FortranArray<5>()),
    ResBuy( D_aypta,FortranArray<5>()),

    v_stay(D_aypt,FortranArray<4>()),	
    v_sell(D_aypt,FortranArray<4>()),  
    v_rent(D_aypt,FortranArray<4>()),	
    v_buy( D_aypt,FortranArray<4>()),   
	c_stay(D_aypt,FortranArray<4>()),	
	c_sell(D_aypt,FortranArray<4>()),  
	c_rent(D_aypt,FortranArray<4>()),	
	c_buy( D_aypt,FortranArray<4>()),   
	Vown(  D_aypt,FortranArray<4>()),	
	Vrent( D_aypt,FortranArray<4>()),	
	EVown( D_aypt,FortranArray<4>()),	
	EVrent(D_aypt,FortranArray<4>()),	
	ctmp(  D_aypa,FortranArray<4>()),  	
	xtmp(  D_aypa,FortranArray<4>()),  	

	s_stay(D_aypt,FortranArray<4>()),  
	s_sell(D_aypt,FortranArray<4>()),  
	s_rent(D_aypt,FortranArray<4>()),  
	s_buy( D_aypt,FortranArray<4>()),  
	Down(  D_aypt,FortranArray<4>()),  
	Drent( D_aypt,FortranArray<4>()),  
	
	vplustmp(D_ayp,FortranArray<3>()),
	G(D_y,FortranArray<2>()),         
    dim_aypt(D_aypt) ,				  
    dim_aypy(D_aypy) ,				  
    dim_ayp(D_ayp) ,				  
    p(*pars) {						  
		// reference the data
		ResStay.reference(data_stay);
		ResSell.reference(data_sell);
		ResRent.reference(data_rent);
		ResBuy.reference(data_buy );
		G.reference(data_G);
		v_stay = 1;
		v_sell = 2;
		v_rent = 3;
		v_buy = 4;
}

/*// constructor 4: expose the class directly to R*/
//// TODO can you do 
//// int m = dims(dims.length())
//// Array<double,m>
//CMig::CMig(Rcpp::IntegerVector dims,
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

// Define Getters
Array<double,5> CMig::GetResStay( void ){ return(ResStay); };
Array<double,5> CMig::GetResSell( void ){ return(ResSell); };
Array<double,5> CMig::GetResBuy( void ){ return(ResStay); };
Array<double,5> CMig::GetResRent( void ){ return(ResSell); };
Array<double,4> CMig::GetVown( void ){ return(Vown); };
Array<double,4> CMig::GetVrent( void ){ return(Vrent); };
Array<int   ,4> CMig::GetDown( void ){ return(Down); };
Array<int   ,4> CMig::GetDrent( void ){ return(Drent); };
Array<double,4> CMig::GetEVown( void ){ return(EVown); };
Array<double,4> CMig::GetEVrent( void ){ return(EVrent); };
Array<double,4> CMig::Getv_stay( void ){ return(v_stay); };
Array<double,4> CMig::Getv_rent( void ){ return(v_rent); };
Array<double,4> CMig::Getv_sell( void ){ return(v_sell); };
Array<double,4> CMig::Getv_buy( void ){ return(v_buy); };
Array<double,4> CMig::Getc_stay( void ){ return(c_stay); };
Array<double,4> CMig::Getc_rent( void ){ return(c_rent); };
Array<double,4> CMig::Getc_sell( void ){ return(c_sell); };
Array<double,4> CMig::Getc_buy( void ){  return(c_buy ); };
Array<int,   4> CMig::Gets_stay( void ){ return(s_stay); };
Array<int,   4> CMig::Gets_rent( void ){ return(s_rent); };
Array<int,   4> CMig::Gets_sell( void ){ return(s_sell); };
Array<int,   4> CMig::Gets_buy( void ){  return(s_buy ); };
Array<double,2> CMig::GetG( void ){ return(G); };


// if you are working from R
#ifdef RcppCompile

// Define show method
// TODO should be Rprintf
// TODO ask how to compile with R libraries
void CMig::show(){
	int ma = 10;
	int my = 10;
	ma = min(ma,dim_aypt(0));
	my = min(my,dim_aypt(1));

	Rcpp::Rcout << "CMig show() method: " << endl;
	Rcpp::Rcout << "we have this dimension vector: " << endl;
	Rcpp::Rcout <<  dim_aypt << endl;
	Rcpp::Rcout << "we have beta: " << endl;
	Rcpp::Rcout <<  p.beta << endl;
	Rcpp::Rcout << "we have myNA: " << endl;
	Rcpp::Rcout <<  p.myNA << endl;
	Rcpp::Rcout << "we have G: " << endl;
	Rcpp::Rcout <<  G << endl;
	Rcpp::Rcout << "showing the first " << ma << " rows" << endl;
	Rcpp::Rcout << "=======================" << endl;
	Rcpp::Rcout <<  endl;
	Rcpp::Rcout << "ResStay(:,:,1,1,1) = " << endl;
	Rcpp::Rcout << ResStay(Range(fromStart,ma),Range(fromStart,my),1,1,1) << endl;
	Rcpp::Rcout << "ResSell(:,:,1,nT,nA) = " << endl;
	Rcpp::Rcout << ResSell(Range(fromStart,ma),Range(fromStart,my),1,dim_aypt(3),dim_aypt(0)) << endl;
	Rcpp::Rcout << "ResRent(:,:,1,nT,nA) = " << endl;
	Rcpp::Rcout << ResRent(Range(fromStart,ma),Range(fromStart,my),1,dim_aypt(3),dim_aypt(0)) << endl;
	Rcpp::Rcout << "ResBuy(:,:,1,nT,nA) = " << endl;
	Rcpp::Rcout << ResBuy(Range(fromStart,ma),Range(fromStart,my),1,dim_aypt(3),dim_aypt(0)) << endl;
	Rcpp::Rcout << "end of show method: " << endl;
	Rcpp::Rcout << "===================" << endl;
}


#else   // if you are not in R and must print to stdout

// Define show method
void CMig::show(){
	int ma = 10;
	int my = 10;
	ma = min(ma,dim_aypt(0));
	my = min(my,dim_aypt(1));

	cout << "CMig show() method: " << endl;
	cout << "we have this dimension vector: " << endl;
	cout <<  dim_aypt << endl;
	cout << "we have beta: " << endl;
	cout <<  p.beta << endl;
	cout << "we have myNA: " << endl;
	cout <<  p.myNA << endl;
	cout << "we have G: " << endl;
	cout <<  G << endl;
	cout << "showing the first " << ma << " rows" << endl;
	cout << "=======================" << endl;
	cout <<  endl;
	cout << "ResStay(:,:,1,1,1) = " << endl;
	cout << ResStay(Range(fromStart,ma),Range(fromStart,my),1,1,1) << endl;
	cout << "ResSell(:,:,1,nT,nA) = " << endl;
	cout << ResSell(Range(fromStart,ma),Range(fromStart,my),1,dim_aypt(3),dim_aypt(0)) << endl;
	cout << "ResRent(:,:,1,nT,nA) = " << endl;
	cout << ResRent(Range(fromStart,ma),Range(fromStart,my),1,dim_aypt(3),dim_aypt(0)) << endl;
	cout << "ResBuy(:,:,1,nT,nA) = " << endl;
	cout << ResBuy(Range(fromStart,ma),Range(fromStart,my),1,dim_aypt(3),dim_aypt(0)) << endl;
	cout << "end of show method: " << endl;
	cout << "===================" << endl;
}

#endif // printing conditions



std::vector<double> CMig::GetResStayNumeric( void ) {
	Array<double,5>::iterator iter;
	std::vector<double> out;
	for (iter = ResStay.begin() ; iter!=ResStay.end();++iter){
		out.push_back(*iter);
	}
	return out;
}

std::vector<double> CMig::GetResSellNumeric( void ) {
	Array<double,5>::iterator iter;
	std::vector<double> out;
	for (iter = ResSell.begin() ; iter!=ResSell.end();++iter){
		out.push_back(*iter);
	}
	return out;
}

std::vector<double> CMig::GetResRentNumeric( void ) {
	Array<double,5>::iterator iter;
	std::vector<double> out;
	for (iter = ResRent.begin() ; iter!=ResRent.end();++iter){
		out.push_back(*iter);
	}
	return out;
}

std::vector<double> CMig::GetResBuyNumeric( void ) {
	Array<double,5>::iterator iter;
	std::vector<double> out;
	for (iter = ResBuy.begin() ; iter!=ResBuy.end();++iter){
		out.push_back(*iter);
	}
	return out;
}


// =====================================
// Computation of period value functions
// =====================================


void CMig::ComputeStay(int age) {

	firstIndex  i;	
	secondIndex j;  
	thirdIndex  k;	
	fourthIndex l;	

	vplustmp = EVown(Range::all(),Range::all(),Range::all(),age+1);
	// get consumption at all states,savings combinations
	ctmp     = ResStay(Range::all(),Range::all(),Range::all(),age,Range::all());
	xtmp     = where(ctmp > 0, p.imgamma*(pow(ctmp(i,j,k,l),p.mgamma)) + p.theta + p.beta * vplustmp(l,j,k), p.myNA);
	// get value of staying
	v_stay(Range::all(),Range::all(),Range::all(),age) = max(xtmp, l);
	s_stay(Range::all(),Range::all(),Range::all(),age) = maxIndex(xtmp, l);
	FindStayCons( age );	// uses values currently in ctmp. TODO how safe is that?

}

void CMig::ComputeSell(int age) {

	firstIndex  i;	
	secondIndex j;  
	thirdIndex  k;	
	fourthIndex l;	

	vplustmp = EVrent(Range::all(),Range::all(),Range::all(),age+1);
	// get consumption at all states,savings combinations
	ctmp     = ResSell(Range::all(),Range::all(),Range::all(),age,Range::all());
	xtmp     = where(ctmp > 0, p.imgamma*(pow(ctmp(i,j,k,l),p.mgamma)) +     0    + p.beta * vplustmp(l,j,k), p.myNA);
	// get value of staying
	v_sell(Range::all(),Range::all(),Range::all(),age) = max(xtmp, l);
	s_sell(Range::all(),Range::all(),Range::all(),age) = maxIndex(xtmp, l);
	FindSellCons( age );	// uses values currently in ctmp. TODO how safe is that?

}

void CMig::ComputeRent(int age) {

	firstIndex  i;	
	secondIndex j;  
	thirdIndex  k;	
	fourthIndex l;	

	vplustmp = EVrent(Range::all(),Range::all(),Range::all(),age+1);
	// get consumption at all states,savings combinations
	ctmp     = ResRent(Range::all(),Range::all(),Range::all(),age,Range::all());
	xtmp     = where(ctmp > 0, p.imgamma*(pow(ctmp(i,j,k,l),p.mgamma)) +     0    + p.beta * vplustmp(l,j,k), p.myNA);
	// get value of staying
	v_rent(Range::all(),Range::all(),Range::all(),age) = max(xtmp, l);
	s_rent(Range::all(),Range::all(),Range::all(),age) = maxIndex(xtmp, l);
	FindRentCons( age );	

}


void CMig::ComputeBuy(int age) {

	firstIndex  i;	
	secondIndex j;  
	thirdIndex  k;	
	fourthIndex l;	

	vplustmp = EVown(Range::all(),Range::all(),Range::all(),age+1);
	// get consumption at all states,savings combinations
	ctmp     = ResBuy(Range::all(),Range::all(),Range::all(),age,Range::all());
	xtmp     = where(ctmp > 0, p.imgamma*(pow(ctmp(i,j,k,l),p.mgamma)) + p.theta + p.beta * vplustmp(l,j,k), p.myNA);
	// get value of staying
	v_buy(Range::all(),Range::all(),Range::all(),age) = max(xtmp, l);
	s_buy(Range::all(),Range::all(),Range::all(),age) = maxIndex(xtmp, l);
	FindBuyCons( age );	
}



// Consumption finder functions
// ============================

void CMig::FindStayCons( int age ){
	int idx;
	for (int i=1;i<s_stay.extent(firstDim)+1;++i){	// CAUTION: fortran arrays are indexed 1,2,...,rank
		for (int j=1;j<s_stay.extent(secondDim)+1;++j){
			for (int k=1; k<s_stay.extent(thirdDim)+1; ++k){

				idx               = s_stay(i,j,k,age); 
				c_stay(i,j,k,age) = ctmp(i,j,k,idx);
			}
		}
	}
}

void CMig::FindSellCons( int age ){
	int idx;
	for (int i=1;i<s_sell.extent(firstDim)+1;++i){	// CAUTION: fortran arrays are indexed 1,2,...,rank
		for (int j=1;j<s_sell.extent(secondDim)+1;++j){
			for (int k=1; k<s_sell.extent(thirdDim)+1; ++k){

				idx               = s_sell(i,j,k,age); 
				c_sell(i,j,k,age) = ctmp(i,j,k,idx);
			}
		}
	}
}

void CMig::FindRentCons( int age ){
	int idx;
	for (int i=1;i<s_rent.extent(firstDim)+1;++i){	// CAUTION: fortran arrays are indexed 1,2,...,rank
		for (int j=1;j<s_rent.extent(secondDim)+1;++j){
			for (int k=1; k<s_rent.extent(thirdDim)+1; ++k){

				idx               = s_rent(i,j,k,age); 
				c_rent(i,j,k,age) = ctmp(i,j,k,idx);
			}
		}
	}
}

void CMig::FindBuyCons( int age ){
	int idx;
	for (int i=1;i<s_buy.extent(firstDim)+1;++i){	// CAUTION: fortran arrays are indexed 1,2,...,rank
		for (int j=1;j<s_buy.extent(secondDim)+1;++j){
			for (int k=1; k<s_buy.extent(thirdDim)+1; ++k){

				idx               = s_buy(i,j,k,age); 
				c_buy(i,j,k,age) = ctmp(i,j,k,idx);
			}
		}
	}
}



void CMig::ComputePeriod(int age){

	// if final operiod, then preComputed resources are utility
	// unfortunately TinyVector dim_aypt only available as C++ array, so different indexing for those.
	if (age==dim_aypt(dim_aypt.length()-1)) {

		EVown(Range::all(),Range::all(),Range::all(),age) = ResStay(Range::all(),Range::all(),Range::all(),age,dim_aypt(0));	//dim_aypt(0) is index of last element in savings vector.
		EVrent(Range::all(),Range::all(),Range::all(),age) = ResRent(Range::all(),Range::all(),Range::all(),age,dim_aypt(0));

	} else {

		ComputeStay(age);		// get v_stay 
		ComputeSell(age);		// get v_sell 	
		ComputeRent(age);		// get v_rent 
		ComputeBuy(age);		// get v_buy
		ComputeDchoice(age);		// get Vmax = max(stay,sell,rent,buy)	
		ComputeExpectations(age);	// get EVown and EVrent
	
	}

}


Array<double,3> CMig::dchoice3d(Array<double,3> one, Array<double,3> two){

	Array<double,3> ret(dim_ayp,FortranArray<3>());

	ret = where(one > two, one, two);

	return(ret);
}

Array<int,3> CMig::dchoiceID3d(Array<double,3> one, Array<double,3> two){

	Array<int,3> ret(dim_ayp,FortranArray<3>());

	ret = where(one > two,1,2);
		  
	return(ret);
}

void CMig::ComputeDchoice( int age ){

	Vown(Range::all(),Range::all(),Range::all(),age)  = dchoice3d( v_stay(Range::all(),Range::all(),Range::all(),age), v_sell(Range::all(),Range::all(),Range::all(),age));
	Down(Range::all(),Range::all(),Range::all(),age)  = dchoiceID3d( v_stay(Range::all(),Range::all(),Range::all(),age), v_sell(Range::all(),Range::all(),Range::all(),age));
	Vrent(Range::all(),Range::all(),Range::all(),age) = dchoice3d( v_rent(Range::all(),Range::all(),Range::all(),age), v_buy(Range::all(),Range::all(),Range::all(),age));
	Drent(Range::all(),Range::all(),Range::all(),age) = dchoiceID3d( v_rent(Range::all(),Range::all(),Range::all(),age), v_buy(Range::all(),Range::all(),Range::all(),age));
}

void CMig::ComputeExpectations( int age ){

	EVown(Range::all(),Range::all(),Range::all(),age) = integrate(Vown(Range::all(),Range::all(),Range::all(),age));
	EVrent(Range::all(),Range::all(),Range::all(),age) = integrate(Vrent(Range::all(),Range::all(),Range::all(),age));

}

Array<double,3> CMig::integrate(Array<double,3> tens){
	
	firstIndex  i;	
	secondIndex j;  
	thirdIndex  k;
	fourthIndex l;	
	
	Array<double,3> ret(dim_ayp,FortranArray<3>());

	Array<double,4> tmp(dim_aypy,FortranArray<4>());	// tmp(i,j,k,l)
	tmp = tens(i,j,k) * G(l,j);
	ret = sum( tmp(i,l,k,j), l);
		  
	return(ret);

}

//#ifdef RcppCompile

	//Rcpp::List CMig::RcppExport( void ) {

		//Rcpp::IntegerVector dAYPT = Rcpp::IntegerVector::create(d(0),d(1),d(2),d(3));
		//Rcpp::IntegerVector dAYPTA = Rcpp::IntegerVector::create(d(0),d(1),d(2),d(3),d(0));

		//// TODO Rcpp::List out = mig.RcppExporter();
		//Rcpp::NumericVector Vown_out  ( Vown.size ( ));
		//Rcpp::NumericVector Vrent_out ( Vown_out.length ( ));
		//Rcpp::NumericVector vstay_out ( Vown_out.length ( ));
		//Rcpp::NumericVector vsell_out ( Vown_out.length ( ));
		//Rcpp::NumericVector vrent_out ( Vown_out.length ( ));
		//Rcpp::NumericVector vbuy_out  ( Vown_out.length ( ));
		//Rcpp::NumericVector sstay_out ( Vown_out.length ( ));
		//Rcpp::NumericVector ssell_out ( Vown_out.length ( ));
		//Rcpp::NumericVector srent_out ( Vown_out.length ( ));
		//Rcpp::NumericVector sbuy_out  ( Vown_out.length ( ));

		////Rcpp::NumericVector Vown_out2( mig.GetVown().data(), mig.GetVown().size() );
		
		//// copy blitz to Rcpp::Numeric
		//Vown_out  = Vown;
		//Vrent_out = Vrent;
		//vstay_out = v_stay;
		//vsell_out = v_sell;
		//vbuy_out  = v_buy;
		//vrent_out = v_rent;
		
		//// create output list
		//Rcpp::List list = Rcpp::List::create( Rcpp::_["Vown"]    = Vown_out,
											  //Rcpp::_["Vrent"]   = Vrent_out,
											  //Rcpp::_["vstay"]   = vstay_out,
											  //Rcpp::_["vsell"]   = vsell_out,
											  //Rcpp::_["vbuy"]    = vbuy_out ,
											  //Rcpp::_["vrent"]   = vrent_out);
		//return list;
	//}



//#endif

























