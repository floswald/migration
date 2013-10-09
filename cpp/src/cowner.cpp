

#include "cowner.h"		// include class declaration of owner class

using namespace blitz;
using namespace std;

// define constructor
COwner4::COwner4(TinyVector<int,4> dims, TinyVector<int,3> dims2, double * data) : 
	Res(data,dims,neverDeleteData,FortranArray<4>()), 
    v_L1(dims2,FortranArray<3>()),
    v_L2(dims2,FortranArray<3>()),  
	c_L1(dims2,FortranArray<3>()),
	c_L2(dims2,FortranArray<3>()),  
	Vmax(dims2,FortranArray<3>()),
	s_L1(dims2,FortranArray<3>()),  
	s_L2(dims2,FortranArray<3>()),
	Dmax(dims2,FortranArray<3>()),  
	save(16,FortranArray<1>()) {

					   Array<double,4> a;
					   Array<double,3> b, c, d, e, f;
					   Array<int,3> g, h, i;
					   Array<double,1> k;
}
				   
// Define Setters

void COwner4::setsave( Array<double,1> dat ){ save = dat ; };
void COwner4::setRes( Array<double,4> dat ){ Res = dat ; };

// Define Getters
Array<double,4> COwner4::getRes( void ){ return(Res); };
Array<double,3> COwner4::getVmax( void ){ return(Vmax); };

// Define show method
void COwner4::show(){
	cout << "Start show method: " << endl;
	cout << "Res = " << endl;
	cout << Res << endl;
	cout << "Res(0,0,0,0) = " << endl;
	cout << Res(1,1,1,1) << endl;
	cout << "Dmax = " << endl;
	cout << Dmax << endl;
	cout << "save = " << endl;
	cout << save << endl;
	cout << "end of show method: " << endl;
	cout << "===================" << endl;
}

void COwner4::version(){
	cout << "This is an object of type COwner4" << endl;
	cout << "The largest array in here has 4 dimensions" << endl;
}


//////////////////////////////////////////
// 5 dimensions
//////////////////////////////////////////

// define constructor
COwner5::COwner5(TinyVector<int,5> dims, double* Stay, double* Sell) : 

	ResStay(Stay,dims,neverDeleteData,FortranArray<5>()), 	//a
    ResSell(Sell,dims,neverDeleteData,FortranArray<5>()), 	//b

    v_stay(dims(0),dims(1),dims(2),dims(3),FortranArray<4>()),	//c
    v_sell(dims(0),dims(1),dims(2),dims(3),FortranArray<4>()),  //d
	c_stay(dims(0),dims(1),dims(2),dims(3),FortranArray<4>()),	//e
	c_sell(dims(0),dims(1),dims(2),dims(3),FortranArray<4>()),  //f
	Vmax(dims(0),dims(1),dims(2),dims(3),FortranArray<4>()),	//g
	ctmp(dims(0),dims(1),dims(2),dims(4),FortranArray<4>()),  	//h
	xtmp(dims(0),dims(1),dims(2),dims(0),FortranArray<4>()),  	//i
	
	vplustmp(dims(0),dims(1),dims(2),FortranArray<3>()),  		//j

	s_stay(dims(0),dims(1),dims(2),dims(3),FortranArray<4>()),  //k
	s_sell(dims(0),dims(1),dims(2),dims(3),FortranArray<4>()),	//l
	Dmax(dims(0),dims(1),dims(2),dims(3),FortranArray<4>()) {  	//m

   // idims(dims) {												//n

					   Array<double,5> a, b;
					   Array<double,4> c, d, e, f, g, h, i;
					   Array<double,3> j;
					   Array<int,4> k, l, m;
					   TinyVector<int,5> n;
}
				   

// Define Getters
Array<double,5> COwner5::getResStay( void ){ return(ResStay); };
Array<double,5> COwner5::getResSell( void ){ return(ResSell); };
Array<double,4> COwner5::getVmax( void ){ return(Vmax); };

// Define show method
void COwner5::show(){
	cout << "Start show method: " << endl;
	cout << "ResStay(0,0,0,0,0) = " << endl;
	cout << ResStay(Range::all(),Range::all(),1,1,1) << endl;
	cout << "ResSell(0,0,0,0,0) = " << endl;
	cout << ResSell(Range::all(),Range::all(),1,1,1) << endl;
	cout << "Dmax = " << endl;
	cout << Dmax << endl;
	cout << "end of show method: " << endl;
	cout << "===================" << endl;
}

void COwner5::version(){
	cout << "This is an object of type COwner5" << endl;
	cout << "The largest array in here has" << ResStay.dimensions() << "dimensions" << endl;
}


// void COwner5::computePeriod(int age, EVstruc* EV){

// 	// if final operiod, then precomputed resources are utility
// 	if (age==idims(Res.rank()-1)){

// 		Vmax(Range::all(),Range::all(),Range::all(),age) = ResStay(Range::all(),Range::all(),Range::all(),age,0)

// 	} else {

// 		// stay
// 		vplustmp = EV->EVOwn(Range::all(),Range::all(),Range::all(),t+1);
// 		ctmp     = ResStay(Range::all(),Range::all(),Range::all(),age,Range::all());




// 	}

// }




















