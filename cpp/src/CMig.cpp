

#ifdef RcppCompile
#include "../../cpp/src/CMig.h"
#else
#include "CMig.h"		// include class declaration of owner class
#endif

using namespace blitz;
using namespace std;


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
	Dmax(  2,2,2,2,FortranArray<4>()),  
	
	vplustmp(2,2,2,FortranArray<3>()), 
    dim_aypt(2,2,2,2) ,				   
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
	Dmax(  x1,x2,x3,x4,FortranArray<4>()),  
	
	vplustmp(x1,x2,x3,FortranArray<3>()), 
    dim_aypt(x1,x2,x3,x4) ,				   
    dim_ayp(x1,x2,x3), 				    
	G(x2,x2,FortranArray<2>()) {
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


// constructor 3: data referenced 
CMig::CMig(TinyVector<int,5> D_aypta,
				 TinyVector<int,4> D_aypt, 
	             TinyVector<int,4> D_aypa, 
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
	Dmax(  D_aypt,FortranArray<4>()),  
	
	vplustmp(D_ayp,FortranArray<3>()), //d
	G(D_y,FortranArray<2>()),         //d2
    dim_aypt(D_aypt) ,				    //e
    dim_ayp(D_ayp) ,				    //e2
    p(*pars) {							//f				
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

// Define Getters
Array<double,5> CMig::GetResStay( void ){ return(ResStay); };
Array<double,5> CMig::GetResSell( void ){ return(ResSell); };
Array<double,5> CMig::GetResBuy( void ){ return(ResStay); };
Array<double,5> CMig::GetResRent( void ){ return(ResSell); };
Array<double,4> CMig::GetVown( void ){ return(Vown); };
Array<double,4> CMig::GetVrent( void ){ return(Vrent); };
Array<double,4> CMig::GetEVown( void ){ return(EVown); };
Array<double,4> CMig::GetEVrent( void ){ return(EVrent); };
Array<double,4> CMig::Getv_stay( void ){ return(v_stay); };
Array<double,4> CMig::Getv_rent( void ){ return(v_rent); };
Array<double,4> CMig::Getv_sell( void ){ return(v_sell); };
Array<double,4> CMig::Getv_buy( void ){ return(v_buy); };
Array<double,2> CMig::GetG( void ){ return(G); };

// Define show method
void CMig::show(){
	cout << "Start show method: " << endl;
	cout << "we have this dimension vector: " << endl;
	cout <<  dim_aypt << endl;
	cout << "we have beta: " << endl;
	cout <<  p.beta << endl;
	cout << "we have myNA: " << endl;
	cout <<  p.myNA << endl;
	cout << "we have G: " << endl;
	cout <<  G << endl;
	cout << "ResStay(:,:,1,1,1) = " << endl;
	cout << ResStay(Range::all(),Range::all(),1,1,1) << endl;
	cout << "ResSell(:,:,1,1,1) = " << endl;
	cout << ResSell(Range::all(),Range::all(),1,1,1) << endl;
	cout << "ResRent(:,:,1,1,1) = " << endl;
	cout << ResRent(Range::all(),Range::all(),1,1,1) << endl;
	cout << "ResBuy(:,:,1,1,1) = " << endl;
	cout << ResBuy(Range::all(),Range::all(),1,1,1) << endl;
	cout << "end of show method: " << endl;
	cout << "===================" << endl;
}

void CMig::version(){
	cout << "This is an object of type CMig" << endl;
	cout << "The largest array in here has" << ResStay.dimensions() << "dimensions" << endl;
}

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


void CMig::computeStay(int age) {

	firstIndex  i;	
	secondIndex j;  
	thirdIndex  k;	
	fourthIndex l;	

	vplustmp = EVown(Range::all(),Range::all(),Range::all(),age+1);
	// get consumption at all states,savings combinations
	ctmp     = ResStay(Range::all(),Range::all(),Range::all(),age,Range::all());
	xtmp     = where(ctmp > 0, log(ctmp(i,j,k,l)) + p.beta * vplustmp(l,j,k), p.myNA);
	// get value of staying
	v_stay(Range::all(),Range::all(),Range::all(),age) = max(xtmp, l);
	s_stay(Range::all(),Range::all(),Range::all(),age) = maxIndex(xtmp, l);

}

void CMig::computeSell(int age) {

	firstIndex  i;	
	secondIndex j;  
	thirdIndex  k;	
	fourthIndex l;	

	vplustmp = EVrent(Range::all(),Range::all(),Range::all(),age+1);
	// get consumption at all states,savings combinations
	ctmp     = ResSell(Range::all(),Range::all(),Range::all(),age,Range::all());
	xtmp     = where(ctmp > 0, log(ctmp(i,j,k,l)) + p.beta * vplustmp(l,j,k), p.myNA);
	// get value of staying
	v_sell(Range::all(),Range::all(),Range::all(),age) = max(xtmp, l);
	s_sell(Range::all(),Range::all(),Range::all(),age) = maxIndex(xtmp, l);

}

void CMig::computeRent(int age) {

	firstIndex  i;	
	secondIndex j;  
	thirdIndex  k;	
	fourthIndex l;	

	vplustmp = EVrent(Range::all(),Range::all(),Range::all(),age+1);
	// get consumption at all states,savings combinations
	ctmp     = ResRent(Range::all(),Range::all(),Range::all(),age,Range::all());
	xtmp     = where(ctmp > 0, log(ctmp(i,j,k,l)) + p.beta * vplustmp(l,j,k), p.myNA);
	// get value of staying
	v_rent(Range::all(),Range::all(),Range::all(),age) = max(xtmp, l);
	s_rent(Range::all(),Range::all(),Range::all(),age) = maxIndex(xtmp, l);

}

void CMig::computeBuy(int age) {

	firstIndex  i;	
	secondIndex j;  
	thirdIndex  k;	
	fourthIndex l;	

	vplustmp = EVown(Range::all(),Range::all(),Range::all(),age+1);
	// get consumption at all states,savings combinations
	ctmp     = ResBuy(Range::all(),Range::all(),Range::all(),age,Range::all());
	xtmp     = where(ctmp > 0, log(ctmp(i,j,k,l)) + p.beta * vplustmp(l,j,k), p.myNA);
	// get value of staying
	v_buy(Range::all(),Range::all(),Range::all(),age) = max(xtmp, l);
	s_buy(Range::all(),Range::all(),Range::all(),age) = maxIndex(xtmp, l);
}




void CMig::computePeriod(int age){

	// if final operiod, then precomputed resources are utility
	if (age==dim_aypt(dim_aypt.length())) {

		EVown(Range::all(),Range::all(),Range::all(),age) = ResStay(Range::all(),Range::all(),Range::all(),age,1);
		EVrent(Range::all(),Range::all(),Range::all(),age) = ResRent(Range::all(),Range::all(),Range::all(),age,1);

	} else {

		computeStay(age);		// get v_stay 
		computeSell(age);		// get v_sell 	
		computeRent(age);		// get v_rent 
		computeBuy(age);		// get v_buy
		computeDchoice(age);		// get Vmax = max(stay,sell,rent,buy)	
		computeExpectations(age);	// get EVown and EVrent
	
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

void CMig::computeDchoice( int age ){

	Vown(Range::all(),Range::all(),Range::all(),age)  = dchoice3d( v_stay(Range::all(),Range::all(),Range::all(),age), v_sell(Range::all(),Range::all(),Range::all(),age));
	Down(Range::all(),Range::all(),Range::all(),age)  = dchoiceID3d( v_stay(Range::all(),Range::all(),Range::all(),age), v_sell(Range::all(),Range::all(),Range::all(),age));
	Vrent(Range::all(),Range::all(),Range::all(),age) = dchoice3d( v_rent(Range::all(),Range::all(),Range::all(),age), v_buy(Range::all(),Range::all(),Range::all(),age));
	Drent(Range::all(),Range::all(),Range::all(),age) = dchoiceID3d( v_rent(Range::all(),Range::all(),Range::all(),age), v_buy(Range::all(),Range::all(),Range::all(),age));
}

void CMig::computeExpectations( int age ){

	EVown(Range::all(),Range::all(),Range::all(),age) = integrate(Vown(Range::all(),Range::all(),Range::all(),age));
	EVrent(Range::all(),Range::all(),Range::all(),age) = integrate(Vrent(Range::all(),Range::all(),Range::all(),age));

}

Array<double,3> CMig::integrate(Array<double,3> tens){
	
	Array<double,3> ret(dim_ayp,FortranArray<3>());
	firstIndex  i;	
	secondIndex j;  
	thirdIndex  k;
	fourthIndex l;	

	ret = sum(tens(i,k,j) * G(l,k),k);
		  
	return(ret);

}


























