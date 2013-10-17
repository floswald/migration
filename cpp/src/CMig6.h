

#include "CMig.h"	// include here to get Parstruc 
#include <string>


// Migration model class for N locations
// =====================================
//
// this version has location as a state variable "here" and "there", both can
// take N values. 
// If here =  there, no migration occurs. 
// if here != there, there is a moving cost in terms of utility. There is NO difference
// in prices (y,p) between locations as of yet.
//
//
// notes:
// 1) arrays are fortranArrays throughout. that means they are indexed 1,2,3,...,Rank in each dimension
// 2) this is done purely for compatibility reasons with R. An array in R is in fortran order. 
//    I want compatibility with the same computation in R in order to check the results. THIS MAY CHANGE IN THE FUTURE.
// 3) unfortunately TinyVectors are not available as FortranArrays. Therefore, they are indexed as 0,1,...,Rank-1 .

class CMig6 {
	private:
		// private data objects
		Array<double,7> ResStay, ResSell, ResRent, ResBuy;	//(a,y,p,here,there,age,a')
		Array<double,6> v_loc_stay,v_loc_sell,v_loc_rent,v_loc_buy,c_loc_stay,c_loc_sell,c_loc_rent,c_loc_buy;	//(a,y,p,here,there,age)
		Array<double,6> ctmp,xtmp;	//(a,y,p,here,there,a')
		Array<int   ,6> s_loc_stay,s_loc_sell,s_loc_rent,s_loc_buy;	//(a,y,p,here,there,age)
		Array<int   ,5> move_stay,move_sell,move_rent,move_buy;	//(a,y,p,here,age) array where an entry is the index of the place one is moving TO.
		Array<double,5> EVown,EVrent,Vown,Vrent;   //(a,y,p,here,age)
		Array<int   ,5> Down,Drent;   //(a,y,p,here,age) array where an entry is 1 or 2. 1 is "stay" (stay owner or renter), 2 is "change" (seller or buyer)
									  // then, conditional on 1 or 2 in this array, go to appropriate move_* array
									  // for owner:
									  // Down = 1: go to move_stay
									  // Down = 2: go to move_sell
									  // for renter:
									  // Drent = 1: go to move_rent
									  // Drent = 2: go to move_buy
		Array<double,4> vplustmp;
		Array<double,2> G;
		TinyVector<int,6> dim_ayp_here_there_t;
		TinyVector<int,5> dim_ayp_here_there;
		TinyVector<int,5> dim_ayp_here_t;
		TinyVector<int,4> dim_aypy;
		TinyVector<int,3> dim_ayp;
		Parstruc p;	
	    const std::string name; // A member variable for the class to store the version 
		
		// private member functions

	public: 
		// 3 constructors
		CMig6();
		//CMig6(int x1,int x2, int x3, int x4, int x5);

		CMig6(TinyVector<int,7> dim_ayp_here_there_ta,
			  TinyVector<int,6> dim_ayp_here_there_t, 
			  TinyVector<int,6> dim_ayp_here_there_a, 
			  TinyVector<int,5> dim_ayp_here_t, 
	   	      TinyVector<int,4> dim_ayp_here,      
			  TinyVector<int,4> dim_aypy, 
			  TinyVector<int,3> dim_ayp, 
			  TinyVector<int,2> dim_y, 
			  Parstruc* pars ,
			  Array<double,7> data_stay,
			  Array<double,7> data_sell,
			  Array<double,7> data_rent,
			  Array<double,7> data_buy,
			  Array<double,2> G	);
                  
		const std::string version(){ return( name ); };
		int MaxDim(){ return(ResStay.dimensions()); };
			

		// getters
	   /* Array<double,5> GetResStay( void );*/
		//Array<double,5> GetResSell( void );
		//Array<double,5> GetResBuy( void );
		//Array<double,5> GetResRent( void );
		//Array<double,4> GetVown( void );
		//Array<double,4> GetVrent( void );
		//Array<int   ,4> GetDown( void );
		//Array<int   ,4> GetDrent( void );
		//Array<double,4> GetEVown( void );
		//Array<double,4> GetEVrent( void );
		//Array<double,4> Getv_stay( void );
		//Array<double,4> Getv_rent( void );
		//Array<double,4> Getv_sell( void );
		//Array<double,4> Getv_buy( void );
		//Array<int   ,4> Gets_stay( void );
		//Array<int   ,4> Gets_rent( void );
		//Array<int   ,4> Gets_sell( void );
		//Array<int   ,4> Gets_buy( void );
		//Array<double,4> Getc_stay( void );
		//Array<double,4> Getc_rent( void );
		//Array<double,4> Getc_sell( void );
		//Array<double,4> Getc_buy( void );
		//Array<double,2> GetG( void );
		//const TinyVector<int,3> GetDimAYP( void ) const { return dim_ayp;};
		//const TinyVector<int,5> GetResStayOrder( void ) const { return(ResStay.ordering()); };
		//const TinyVector<int,5> GetResSellOrder( void ) const { return(ResSell.ordering()); };
		//const TinyVector<int,5> GetResBuyOrder( void ) const { return(ResStay.ordering()); };
		//const TinyVector<int,5> GetResRentOrder( void ) const { return(ResSell.ordering()); };
		//const TinyVector<int,4> GetVownOrder( void ) const { return(Vown.ordering()); };
		//const TinyVector<int,4> GetVrentOrder( void ) const { return(Vrent.ordering()); };
		//const TinyVector<int,4> GetEVownOrder( void ) const { return(EVown.ordering()); };
		//const TinyVector<int,4> GetEVrentOrder( void ) const { return(EVrent.ordering()); };
		//const TinyVector<int,4> Getv_stayOrder( void ) const { return(v_stay.ordering()); };
		//const TinyVector<int,4> Getv_rentOrder( void ) const { return(v_rent.ordering()); };
		//const TinyVector<int,4> Getv_sellOrder( void ) const { return(v_sell.ordering()); };
		//const TinyVector<int,4> Getv_buyOrder( void ) const { return(v_buy.ordering()); };
		//const TinyVector<int,2> GetGOrder( void ) const { return(G.ordering()); };

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
		//void FindStayCons( int age );
		//void FindSellCons( int age );
		//void FindRentCons( int age );
		//void FindBuyCons( int age );

		//std::vector<double> GetResStayNumeric( void );
		//std::vector<double> GetResSellNumeric( void );
		//std::vector<double> GetResRentNumeric( void );
		//std::vector<double> GetResBuyNumeric( void );
		
		//Array<double,3> dchoice3d(Array<double,3> one, Array<double,3> two);
		//Array<int   ,3> dchoiceID3d(Array<double,3> one, Array<double,3> two);
		//Array<double,3> integrate(Array<double,3> tens);




		//Rcpp::List RcppExport( void );	


};
