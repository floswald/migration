#include <iostream>
#include <blitz/array.h>

using namespace std;
using namespace blitz;
             
#ifndef ONCE_COWNER_H
#define ONCE_COWNER_H

// struct to hold parameters
struct Parstruc{
	double beta;
	double myNA;
};

// struct to hold expected value functions
struct EVstruc{
    Array<double,4> EVown;
    Array<double,4> EVrent;
    Array<double,4> Vmax;
};

// Expectations computing function
// takes the EV structure and computes
// Array<double,3> computeExpectation(int age, EVstruc* EV, Array<double,2> G);


class CMig {
	private:
		// private data objects
		Array<double,5> ResStay, ResSell, ResRent, ResBuy;	
		Array<double,4> EVown,EVrent,Vown,Vrent,v_stay,v_sell,v_rent,v_buy,c_stay,c_sell,c_rent,c_buy,Vmax,ctmp,xtmp;
		Array<double,3> vplustmp;
		Array<double,2> G;
		Array<int,4>    s_stay,s_sell,s_rent,s_buy,Dmax,Down,Drent;
		TinyVector<int,4> dim_aypt;
		TinyVector<int,3> dim_ayp;
		Parstruc p;	
		// private member functions

	public: 
		// constructor
		CMig(TinyVector<int,5>* D_aypta,
				TinyVector<int,4>* D_aypt, 
				TinyVector<int,4>* D_aypa, 
				TinyVector<int,3>* D_ayp, 
				TinyVector<int,2>* D_y, 
				double* Stay, 
				double* Sell,
				double* Rent, 
				double* Buy,
				double* Gdat,
				Parstruc* pars);

		// getters
		Array<double,5> getResStay( void );
		Array<double,5> getResSell( void );
		Array<double,4> getVown( void );
		Array<double,4> getVrent( void );
		Array<double,4> getEVown( void );
		Array<double,4> getEVrent( void );
		Array<double,4> getv_stay( void );
		Array<double,4> getv_rent( void );
		Array<double,4> getv_sell( void );
		Array<double,4> getv_buy( void );

		// other member functions		
		void show ( void );
		void version ( void );
		void computePeriod(int age);
		void computeExpectations( int age );

		void computeStay( int age );
		void computeSell( int age );
		void computeRent( int age );
		void computeBuy( int age );
		void computeDchoice( int age );
		
		Array<double,3> dchoice3d(Array<double,3> one, Array<double,3> two);
		Array<int   ,3> dchoiceID3d(Array<double,3> one, Array<double,3> two);
		Array<double,3> integrate(Array<double,3> tens);

};

#endif //ONCE_COWNER_H
