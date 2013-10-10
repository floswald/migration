
#include "CMig.h"		// include class declaration of owner class

using namespace blitz;
using namespace std;


///////////////////////
// class implementation
// of Migration class
///////////////////////

// define constructor
// this is a bit complicated, because I create the objects inside the class
// and because they have to be allocated as FortranArrays
// must pass dims and a pointer to dims
CMig::CMig(TinyVector<int,5>* D_aypta,
				 TinyVector<int,4>* D_aypt, 
	             TinyVector<int,4>* D_aypa, 
	             TinyVector<int,3>* D_ayp, 
   				 TinyVector<int,2>* D_y, 
	             double* Stay, 
	             double* Sell,
	             double* Rent, 
	             double* Buy,
	             double* Gdat,
	             Parstruc* pars) : 

	ResStay(Stay,*D_aypta,neverDeleteData,FortranArray<5>()), 	//a1
    ResSell(Sell,*D_aypta,neverDeleteData,FortranArray<5>()), 	//a2
	ResRent(Rent,*D_aypta,neverDeleteData,FortranArray<5>()), 	//a3
    ResBuy(Buy ,*D_aypta,neverDeleteData,FortranArray<5>()), 	//a4

    v_stay(*D_aypt,FortranArray<4>()),	//b1
    v_sell(*D_aypt,FortranArray<4>()),  //b2
    v_rent(*D_aypt,FortranArray<4>()),	//b3
    v_buy(*D_aypt,FortranArray<4>()),   //b4
	c_stay(*D_aypt,FortranArray<4>()),	//b5
	c_sell(*D_aypt,FortranArray<4>()),  //b6
	c_rent(*D_aypt,FortranArray<4>()),	//b7
	c_buy(*D_aypt,FortranArray<4>()),   //b8
	Vown(*D_aypt,FortranArray<4>()),	//b9
	Vrent(*D_aypt,FortranArray<4>()),	//b10
	EVown(*D_aypt,FortranArray<4>()),	//b11
	EVrent(*D_aypt,FortranArray<4>()),	//b12
	ctmp(*D_aypa,FortranArray<4>()),  	//b13
	xtmp(*D_aypa,FortranArray<4>()),  	//b14

	s_stay(*D_aypt,FortranArray<4>()),  //c1
	s_sell(*D_aypt,FortranArray<4>()),  //c2
	s_rent(*D_aypt,FortranArray<4>()),  //c3
	s_buy(*D_aypt,FortranArray<4>()),   //c4
	Down(*D_aypt,FortranArray<4>()),    //c5
	Drent(*D_aypt,FortranArray<4>()),   //c6
	Dmax(*D_aypt,FortranArray<4>()),    //c7
	
	vplustmp(*D_ayp,FortranArray<3>()), //d
	G(Gdat,shape(2,2),neverDeleteData,FortranArray<2>()),         //d2
    dim_aypt(*D_aypt) ,				    //e
    dim_ayp(*D_ayp) ,				    //e2
    p(*pars) {							//f				

			   Array<double,5> a1, a2, a3, a4;
			   Array<double,4> b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11,b12,b13,b14;
			   Array<int   ,4> c1,c2,c3,c4,c5,c6,c7;
			   Array<double,3> d;
			   Array<double,2> d2;
			   TinyVector<int,4> e;
			   TinyVector<int,3> e2;
			   Parstruc f;
}


// Define Getters
Array<double,5> CMig::getResStay( void ){ return(ResStay); };
Array<double,5> CMig::getResSell( void ){ return(ResSell); };
Array<double,4> CMig::getVown( void ){ return(Vown); };
Array<double,4> CMig::getVrent( void ){ return(Vrent); };
Array<double,4> CMig::getEVown( void ){ return(EVown); };
Array<double,4> CMig::getEVrent( void ){ return(EVrent); };
Array<double,4> CMig::getv_stay( void ){ return(v_stay); };
Array<double,4> CMig::getv_rent( void ){ return(v_rent); };
Array<double,4> CMig::getv_sell( void ){ return(v_sell); };
Array<double,4> CMig::getv_buy( void ){ return(v_buy); };

// Define show method
void CMig::show(){
	cout << "Start show method: " << endl;
	cout << "we have this dimension vector: " << endl;
	cout <<  dim_aypt << endl;
	cout << "we have beta: " << endl;
	cout <<  p.beta << endl;
	cout << "we have myNA: " << endl;
	cout <<  p.myNA << endl;
	cout << "ResStay(0,0,0,0,0) = " << endl;
	cout << ResStay(Range::all(),Range::all(),1,1,1) << endl;
	cout << "ResSell(0,0,0,0,0) = " << endl;
	cout << ResSell(Range::all(),Range::all(),1,1,1) << endl;
	cout << "Dmax = " << endl;
	cout << Dmax << endl;
	cout << "end of show method: " << endl;
	cout << "===================" << endl;
}

void CMig::version(){
	cout << "This is an object of type CMig" << endl;
	cout << "The largest array in here has" << ResStay.dimensions() << "dimensions" << endl;
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
	if (age==2) {

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


























