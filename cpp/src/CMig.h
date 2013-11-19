

#include <iostream>
#include <vector>
#include <algorithm>	// using min in show method
#include <blitz/array.h>
#include <gsl/gsl_interp.h>
#include <gsl/gsl_spline.h>
#include <gsl/gsl_errno.h>
#include <string>

// defined the euler mascheroni constant
// http://en.wikipedia.org/wiki/Gumbel_distribution
#define euler_mascheroni 0.5772

using namespace blitz;

// Migration model class for N locations
// =====================================
//
// version 3.0
//
// with aggregate shock Z
//
// pass borrowing limits and built consumption in C
//
// this version has location as a state variable "here" and "there", both can
// take N values. 
// If here =  there, no migration occurs. 
// if here != there, there is a moving cost in terms of utility. 
// does have integration over house prices as well.
//
// notes:
// 1) arrays are fortranArrays throughout. that means they are indexed 1,2,3,...,Rank in each dimension
// 2) this is done purely for compatibility reasons with R. An array in R is in fortran order. 
//    I want compatibility with the same computation in R in order to check the results. THIS MAY CHANGE IN THE FUTURE.
// 3) unfortunately TinyVectors are not available as FortranArrays. Therefore, they are indexed as 0,1,...,Rank-1 .



#ifndef ONCE_COWNER_H
#define ONCE_COWNER_H


// struct to hold parameters
struct PStruct{
	double beta;
	double myNA;
	double gamma;
	double mgamma;
	double imgamma;
	double theta;
	double R;
	gsl_interp_accel *acc;
	gsl_spline *spline;
	const gsl_interp_type *type; 
};


class CMig {
	private:
		// private data objects
		Array<double,7> ResStay, ResSell, ResRent, ResBuy;	//(a,y,p,here,there,Z,age)

		// (I) values/policies conditional on (here,there) AND tenure choice
		// v_loc_x = max_{savings} u(resources - savings) + EV(savings,x), x = stay,sell,rent,buy
		Array<double,7> v_loc_stay, v_loc_sell, v_loc_rent, v_loc_buy, c_loc_stay, c_loc_sell, c_loc_rent, c_loc_buy;	//(a,y,p,here,there,Z,age)
		Array<int   ,7> s_loc_stay,s_loc_sell,s_loc_rent,s_loc_buy;	//(a,y,p,here,there,Z,age)

		// (II) envelope and indicators over (I)
		// W_loc_own  = max( v_loc_stay, v_loc_sell )
		// W_loc_rent = max( v_loc_rent, v_loc_buy  )
		Array<double,7> W_loc_own, W_loc_rent, rho_own, rho_rent;	 //(a,y,p,here,there,Z,age)
		Array<int   ,7> Tenure_loc_own, Tenure_loc_rent;	 //(a,y,p,here,there,Z,age)

		// (III) values/indicators conditional on here only
		// i.e. envelopes over (II): 
		// Vown  = max( W_loc_own,  there)
		// Vrent = max( W_loc_rent, there)
		// expectations thereof
		Array<double,6> EVown, EVrent, Vown, Vrent, Vbar_own, Vbar_rent;   //(a,y,p,here,Z,age)
		// indicators
		Array<int   ,6> Location_own, Location_rent;   //(a,y,p,here,Z,age)

		// (IV) auxiliary member arrays
		Array<double,6> v_loc_tmp;   //(a,y,p,here,there,Z)
		Array<double,5> vbar_tmp;   //(a,y,p,here,Z)

		Array<double,7> ctmp,xtmp;	//(a,y,p,here,there,Z,a')
		Array<double,6> restmp;	//(a,y,p,here,there,Z)
		Array<double,5> vplustmp;
		Array<double,4> save_own;	//(here,there,price,save)
		Array<double,3> save_buy;	//(    ,there,price,save)
		Array<double,1> save_rent;	//(                ,save)
		Array<double,2> G, Gz;
		Array<double,2> MoveCost;
		Array<double,1> Amenity, tmp1;
		Array<double,4> GpInt;
		Array<double,3> GpEval;
		Array<double,1> pgrid;
		TinyVector<int,7> dim_ayp_here_there_Z_t;
		PStruct p;	
		int maxage, verbose, nLoc, nPrice, nAsset, nIncome, nAgg;
	    const std::string name; // A member variable for the class to store the version 
		
		// private member functions

	public: 
		// 2 constructors
		// you build the arrays before you initiate
		// the class. the class is referenced to those
		// data arrays
		CMig();
		CMig(int nA, int nY, int nP, int nL, int nZ, int nT);
		CMig(int nA, int nY, int nP, int nL, int nZ, int nT,
				PStruct * data_pars,
				Array<double,7> data_stay,
				Array<double,7> data_sell,
				Array<double,7> data_rent,
				Array<double,7> data_buy,
				Array<double,2> data_G,	
				Array<double,2> data_Gz,	
				Array<double,2> data_MoveCost,
				Array<double,1> data_Amenity,
				Array<double,4> data_save_own,
				Array<double,3> data_save_buy,
				Array<double,1> data_save_rent,
				Array<double,3> data_GpEval,
				Array<double,4> data_GpInt,
			 int data_verbose)  ;              
                  
			

		// getters
		Array<double,7> GetResStay(    void ) const {return(ResStay);};
		Array<double,7> GetResSell(    void ) const {return(ResSell);};
		Array<double,7> GetResBuy(     void ) const {return(ResBuy );};
		Array<double,7> GetResRent(    void ) const {return(ResRent);};
		Array<double,7> GetCtmp(       void ) const {return(ctmp);};
		Array<double,4> GetSaveOwn(    void ) const {return(save_own);};
		Array<double,3> GetSaveBuy(    void ) const {return(save_buy);};
		Array<double,1> GetSaveRent(   void ) const {return(save_rent);};

		Array<double,6> GetVown(       void ) const {return(Vown)   ;};
		Array<double,6> GetVrent(      void ) const {return(Vrent)  ;};
		Array<double,6> GetEVown(      void ) const {return(EVown)  ;};
		Array<double,6> GetVbarRent(   void ) const {return(Vbar_rent) ;};
		Array<double,6> GetVbarOwn(    void ) const {return(Vbar_own) ;};
		Array<double,6> GetEVrent(     void ) const {return(EVrent) ;};
		Array<int   ,6> GetLocationOwn( void ) const {return(Location_own)   ;};
		Array<int   ,6> GetLocationRent(void ) const {return(Location_rent)   ;};

		Array<double,7> GetW_loc_own( void ) const {return(W_loc_own);};
		Array<double,7> GetW_loc_rent(void ) const {return(W_loc_rent);};
		
		Array<double,7> Getrho_own( void ) const {return(rho_own);};
		Array<double,7> Getrho_rent(void ) const {return(rho_rent);};

		Array<int   ,7> GetTenure_loc_own(  void ) const {return(Tenure_loc_own);};
		Array<int   ,7> GetTenure_loc_rent( void ) const {return(Tenure_loc_rent);};

		Array<double,7> Getv_loc_stay( void ) const {return(v_loc_stay);};
		Array<double,7> Getv_loc_rent( void ) const {return(v_loc_rent);};
		Array<double,7> Getv_loc_sell( void ) const {return(v_loc_sell);};
		Array<double,7> Getv_loc_buy(  void ) const {return(v_loc_buy );};

		Array<double,7> Getc_loc_stay( void ) const {return(c_loc_stay);};
		Array<double,7> Getc_loc_rent( void ) const {return(c_loc_rent);};
		Array<double,7> Getc_loc_sell( void ) const {return(c_loc_sell);};
		Array<double,7> Getc_loc_buy(  void ) const {return(c_loc_buy );};

		Array<int   ,7> Gets_loc_stay( void ) const {return(s_loc_stay);};
		Array<int   ,7> Gets_loc_rent( void ) const {return(s_loc_rent);};
		Array<int   ,7> Gets_loc_sell( void ) const {return(s_loc_sell);};
		Array<int   ,7> Gets_loc_buy(  void ) const {return(s_loc_buy );};

		Array<double,2> GetG(          void ) const {return(G);};
		Array<double,2> GetGz(         void ) const {return(Gz);};
		int GetMaxage( void ) const {return(maxage);};
		const std::string version(){ return( name ); };
		int MaxDim(){ return(ResStay.dimensions()); };

	
		// setter
		//void SetCtmp ( Array<double,6> x ) { ctmp.reference( x ) ;}

		//// other member functions		
		void show ( void );
		void BuildSaveOwn( void ) ;
		void TestCtmpSubset( void );
		void TestCtmpSubset_Buy( void );
		//void LimitBuyer( void ) ;
		//void ComputePeriod(int age);
		//void ComputeExpectations( int age );

		//void ComputeStay( int age );
		//void ComputeSell( int age );
		//void ComputeRent( int age );
		//void ComputeBuy( int age );
		//void ComputeLocationChoice( int age );
		//void ComputeTenureChoice( int age );
		//void FindStayCons( int age );
		//void FindSellCons( int age );
		//void FindRentCons( int age );
		//void FindBuyCons( int age );

		////std::vector<double> GetResStayNumeric( void );
		////std::vector<double> GetResSellNumeric( void );
		////std::vector<double> GetResRentNumeric( void );
		////std::vector<double> GetResBuyNumeric( void );
		
		//Array<double,6> dchoice5d(Array<double,6> one, Array<double,6> two);
		//Array<int   ,6> dchoiceID5d(Array<double,6> one, Array<double,6> two);
		Array<double,5> integrate(Array<double,5> tens);




		//Rcpp::List RcppExport( void );	


};
#endif //ONCE_COWNER_H
