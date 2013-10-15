#include <iostream>
#include <vector>
#include <algorithm>	// using min in show method
#include <blitz/array.h>
//#include <Rcpp.h>
//#include "/Library/Frameworks/R.framework/Versions/Current/Resources/include/R.h"
#include <R.h>	// Rprintf 
#include <Rcpp.h>	// Rprintf 

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
// takes the EV structure and Computes
// Array<double,3> ComputeExpectation(int age, EVstruc* EV, Array<double,2> G);


//#ifdef RcppCompile

class CMig {
	private:
		// private data objects
		Array<double,5> ResStay, ResSell, ResRent, ResBuy;	
		Array<double,4> EVown,EVrent,Vown,Vrent,v_stay,v_sell,v_rent,v_buy,c_stay,c_sell,c_rent,c_buy,Vmax,ctmp,xtmp;
		Array<double,3> vplustmp;
		Array<double,2> G;
		Array<int,4>    s_stay,s_sell,s_rent,s_buy,Dmax,Down,Drent;
		TinyVector<int,4> dim_aypt;
		TinyVector<int,4> dim_aypy;
		TinyVector<int,3> dim_ayp;
		Parstruc p;	
		// private member functions

	public: 
		// 3 constructors
		CMig();
		CMig(int x1,int x2, int x3, int x4, int x5);

		CMig(TinyVector<int,5> D_aypta,
				 TinyVector<int,4> D_aypt, 
	             TinyVector<int,4> D_aypa, 
	             TinyVector<int,4> D_aypy, 
	             TinyVector<int,3> D_ayp, 
   				 TinyVector<int,2> D_y, 
	             Parstruc* pars ,
				 Array<double,5> data_stay,
				 Array<double,5> data_sell,
				 Array<double,5> data_rent,
				 Array<double,5> data_buy,
		         Array<double,2> G	);

		// getters
		Array<double,5> GetResStay( void );
		Array<double,5> GetResSell( void );
		Array<double,5> GetResBuy( void );
		Array<double,5> GetResRent( void );
		Array<double,4> GetVown( void );
		Array<double,4> GetVrent( void );
		Array<double,4> GetEVown( void );
		Array<double,4> GetEVrent( void );
		Array<double,4> Getv_stay( void );
		Array<double,4> Getv_rent( void );
		Array<double,4> Getv_sell( void );
		Array<double,4> Getv_buy( void );
		Array<double,2> GetG( void );
		const TinyVector<int,3> GetDimAYP( void ) const { return dim_ayp;};
		const TinyVector<int,5> GetResStayOrder( void ) const { return(ResStay.ordering()); };
		const TinyVector<int,5> GetResSellOrder( void ) const { return(ResSell.ordering()); };
		const TinyVector<int,5> GetResBuyOrder( void ) const { return(ResStay.ordering()); };
		const TinyVector<int,5> GetResRentOrder( void ) const { return(ResSell.ordering()); };
		const TinyVector<int,4> GetVownOrder( void ) const { return(Vown.ordering()); };
		const TinyVector<int,4> GetVrentOrder( void ) const { return(Vrent.ordering()); };
		const TinyVector<int,4> GetEVownOrder( void ) const { return(EVown.ordering()); };
		const TinyVector<int,4> GetEVrentOrder( void ) const { return(EVrent.ordering()); };
		const TinyVector<int,4> Getv_stayOrder( void ) const { return(v_stay.ordering()); };
		const TinyVector<int,4> Getv_rentOrder( void ) const { return(v_rent.ordering()); };
		const TinyVector<int,4> Getv_sellOrder( void ) const { return(v_sell.ordering()); };
		const TinyVector<int,4> Getv_buyOrder( void ) const { return(v_buy.ordering()); };
		const TinyVector<int,2> GetGOrder( void ) const { return(G.ordering()); };

		//setters
		void ReferenceStay( Array<double,5> x ) { ResStay.reference( x ) ; }
		void ReferenceSell( Array<double,5> x ) { ResSell.reference( x ) ; }
		void ReferenceRent( Array<double,5> x ) { ResRent.reference( x ) ; }
		void ReferenceBuy( Array<double,5> x ) { ResBuy.reference( x ) ; }
		
		void ReferenceG( Array<double,2> x ) { G.reference( x ) ; }
		void SetP( Parstruc* par ) { p = *par ; }

		// other member functions		
		void show ( void );
		void version ( void );
		void ComputePeriod(int age);
		void ComputeExpectations( int age );

		void ComputeStay( int age );
		void ComputeSell( int age );
		void ComputeRent( int age );
		void ComputeBuy( int age );
		void ComputeDchoice( int age );

		std::vector<double> GetResStayNumeric( void );
		std::vector<double> GetResSellNumeric( void );
		std::vector<double> GetResRentNumeric( void );
		std::vector<double> GetResBuyNumeric( void );
		
		Array<double,3> dchoice3d(Array<double,3> one, Array<double,3> two);
		Array<int   ,3> dchoiceID3d(Array<double,3> one, Array<double,3> two);
		Array<double,3> integrate(Array<double,3> tens);


		//Rcpp::List RcppExport( void );	


};

/*#else	// if NOT Rcpp compiled, leave RcppExport out*/

//class CMig {
	//private:
		//// private data objects
		//Array<double,5> ResStay, ResSell, ResRent, ResBuy;	
		//Array<double,4> EVown,EVrent,Vown,Vrent,v_stay,v_sell,v_rent,v_buy,c_stay,c_sell,c_rent,c_buy,Vmax,ctmp,xtmp;
		//Array<double,3> vplustmp;
		//Array<double,2> G;
		//Array<int,4>    s_stay,s_sell,s_rent,s_buy,Dmax,Down,Drent;
		//TinyVector<int,4> dim_aypt;
		//TinyVector<int,3> dim_ayp;
		//Parstruc p;	
		//// private member functions

	//public: 
		//// 3 constructors
		//CMig();
		//CMig(int x1,int x2, int x3, int x4, int x5);

		//CMig(TinyVector<int,5> D_aypta,
				 //TinyVector<int,4> D_aypt, 
				 //TinyVector<int,4> D_aypa, 
				 //TinyVector<int,3> D_ayp, 
                    //TinyVector<int,2> D_y, 
				 //Parstruc* pars ,
				 //Array<double,5> data_stay,
				 //Array<double,5> data_sell,
				 //Array<double,5> data_rent,
				 //Array<double,5> data_buy,
				 //Array<double,2> G	);

		//// getters
		//Array<double,5> GetResStay( void );
		//Array<double,5> GetResSell( void );
		//Array<double,5> GetResBuy( void );
		//Array<double,5> GetResRent( void );
		//Array<double,4> GetVown( void );
		//Array<double,4> GetVrent( void );
		//Array<double,4> GetEVown( void );
		//Array<double,4> GetEVrent( void );
		//Array<double,4> Getv_stay( void );
		//Array<double,4> Getv_rent( void );
		//Array<double,4> Getv_sell( void );
		//Array<double,4> Getv_buy( void );
		//Array<double,2> GetG( void );
		//const TinyVector<int,3> GetDimAYP( void ) const { return dim_ayp;};

		////setters
		//void ReferenceStay( Array<double,5> x ) { ResStay.reference( x ) ; }
		//void ReferenceSell( Array<double,5> x ) { ResSell.reference( x ) ; }
		//void ReferenceRent( Array<double,5> x ) { ResRent.reference( x ) ; }
		//void ReferenceBuy( Array<double,5> x ) { ResBuy.reference( x ) ; }
		
		//void ReferenceG( Array<double,2> x ) { G.reference( x ) ; }
		//void SetP( Parstruc* par ) { p = *par ; }

		//// other member functions		
		//void show ( void );
		//void version ( void );
		//void ComputePeriod(int age);
		//void ComputeExpectations( int age );

		//void ComputeStay( int age );
		//void ComputeSell( int age );
		//void ComputeRent( int age );
		//void ComputeBuy( int age );
		//void ComputeDchoice( int age );

		//std::vector<double> GetResStayNumeric( void );
		//std::vector<double> GetResSellNumeric( void );
		//std::vector<double> GetResRentNumeric( void );
		//std::vector<double> GetResBuyNumeric( void );
		
		//Array<double,3> dchoice3d(Array<double,3> one, Array<double,3> two);
		//Array<int   ,3> dchoiceID3d(Array<double,3> one, Array<double,3> two);
		//Array<double,3> integrate(Array<double,3> tens);


//};

//#endif	// RcppCompile

#endif //ONCE_COWNER_H
