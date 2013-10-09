#include <iostream>
#include <blitz/array.h>

using namespace std;
using namespace blitz;
             
#ifndef ONCE_COWNER_H
#define ONCE_COWNER_H



                                    
class COwner4 {
	private:
		// private data objects

		Array<double,4> Res;
		Array<double,3> v_L1,v_L2,c_L1,c_L2,Vmax;
		Array<int,3>    s_L1,s_L2,Dmax;
		Array<double,1> save;	// savings grid
		// private member functions
	public: 
		// constructor
		COwner4(TinyVector<int,4> dims, TinyVector<int,3> dims2, double * data);	
		void show ( void );
		void version ( void );
		// setters
		void setsave( Array<double,1> dat );
		void setRes( Array<double,4> dat );
		// getters
		Array<double,4> getRes( void );
		Array<double,3> getVmax( void );
		// other member functions
		//int computePeriod( int age, Array<double,3> EV, Array<double,3> BK);	// will compute all conditional values and set them into the private objects
		                  							 							// EV.case, BK.case
};


// same for 5 dimensions
class COwner5 {
	private:
		// private data objects
		Array<double,5> ResStay, ResSell;	
		Array<double,4> v_stay,v_sell,c_stay,c_sell,Vmax,ctmp,xtmp;
		Array<double,3> vplustmp;
		Array<int,4>    s_stay,s_sell,Dmax;
		//TinyVector<int,5> idims;	
		// private member functions

	public: 
		// constructor
		COwner5(TinyVector<int,5> dims, double * dataStay, double * dataSell);	
		void show ( void );
		void version ( void );
		// setters
		// getters
		Array<double,5> getResStay( void );
		Array<double,5> getResSell( void );
		Array<double,4> getVmax( void );
		// other member functions
		//void computePeriod( int age, EVstruc* EV);	// will compute all conditional values and set them into the private objects
		                  							 							// EV.case, BK.case
};

#endif //ONCE_COWNER_H
