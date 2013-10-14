

#include <blitz/array.h>
#include <iostream>

using namespace blitz;
using namespace std;


class COwner {
	private:
		// private data objects
		Array<double,4> Res;
		Array<double,3> v_L1,v_L2,c_L1,c_L2,Vmax;
		Array<int,3>    s_L1,s_L2,Dmax;
		Array<double,1> save;	// savings grid
		// private member functions
	public: 
		// constructor
		COwner(int d1, int d2, int d3, int d4);	
		void show ( void );
		// setters
		//void setRes( Array<double,1> dat ){ Res = dat ; };
		void setRes( Rcpp::NumericVector dat ){ Res = dat ; };
		void setsave( Array<double,1> dat ){ save = dat ; };
		// other member functions
		//int computePeriod( int age, Array<double,3> EV, Array<double,3> BK);	// will compute all conditional values and set them into the private objects
		                  							 							// EV.case, BK.case
};

COwner::COwner(int d1, int d2, int d3, int d4) : Res(d1,d2,d3,d4,FortranArray<4>()), v_L1(d1,d2,d3,FortranArray<3>()),
			       v_L2(d1,d2,d3,FortranArray<3>()),  c_L1(d1,d2,d3,FortranArray<3>()),
			       c_L2(d1,d2,d3,FortranArray<3>()),  Vmax(d1,d2,d3,FortranArray<3>()),
			       s_L1(d1,d2,d3,FortranArray<3>()),  s_L2(d1,d2,d3,FortranArray<3>()),
			       Dmax(d1,d2,d3,FortranArray<3>()),  save(16,FortranArray<1>()) {

					   Array<double,4> a;
					   Array<double,3> b, c, d, e, f;
					   Array<int,3> g, h, i;
					   Array<double,1> k;
				   }

void COwner::show(){
	cout << "Start show method: " << endl;
	cout << "Res = " << endl;
	cout << Res << endl;
	cout << "Dmax = " << endl;
	cout << Dmax << endl;
	cout << "save = " << endl;
	cout << save << endl;
	cout << "end of show method: " << endl;
	cout << "===================" << endl;
}

