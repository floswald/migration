

#include "CMig.h"	// include here to get Parstruc 
#include <string>


// Migration model class for N locations
// =====================================
//
// version 1.1
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

class CMig6 {
	private:
		// private data objects
		Array<double,7> ResStay, ResSell, ResRent, ResBuy;	//(a,y,p,here,there,age,a')
		Array<double,6> v_loc_stay,v_loc_sell,v_loc_rent,v_loc_buy,c_loc_stay,c_loc_sell,c_loc_rent,c_loc_buy;	//(a,y,p,here,there,age)
		Array<double,6> ctmp,xtmp;	//(a,y,p,here,there,a')
		Array<int   ,6> s_loc_stay,s_loc_sell,s_loc_rent,s_loc_buy;	//(a,y,p,here,there,age)
		Array<int   ,5> move_stay,move_sell,move_rent,move_buy;	//(a,y,p,here,age) array where an entry is the index of the place one is moving TO.
		Array<double,5> EVown,EVrent,Vown,Vrent,v_loc_tmp,v_stay,v_sell,v_rent,v_buy;   //(a,y,p,here,age)
		Array<int   ,5> Down,Drent;   //(a,y,p,here,age) array where an entry is 1 or 2. 1 is "stay" (stay owner or renter), 2 is "change" (seller or buyer)
									  // then, conditional on 1 or 2 in this array, go to appropriate move_* array
									  // for owner:
									  // Down = 1: go to move_stay
									  // Down = 2: go to move_sell
									  // for renter:
									  // Drent = 1: go to move_rent
									  // Drent = 2: go to move_buy
		Array<double,4> vplustmp;
		Array<double,2> G, Gp;
		Array<double,2> MoveCost;
		Array<double,1> Amenity;
		TinyVector<int,6> dim_ayp_here_there_t;
		TinyVector<int,5> dim_ayp_here_there;
		TinyVector<int,5> dim_ayp_here_t;
		TinyVector<int,5> dim_ayp_here_y;
		TinyVector<int,6> dim_ayp_here_yp;
		TinyVector<int,4> dim_ayp_here;
		TinyVector<int,3> dim_ayp;
		Parstruc p;	
		int maxage, verbose;
	    const std::string name; // A member variable for the class to store the version 
		
		// private member functions

	public: 
		// 2 constructors
		CMig6();
		CMig6(TinyVector<int,7> dim_ayp_here_there_ta,
			  TinyVector<int,6> dim_ayp_here_there_t, 
			  TinyVector<int,6> dim_ayp_here_there_a, 
			  TinyVector<int,5> dim_ayp_here_there, 
			  TinyVector<int,5> dim_ayp_here_t, 
			  TinyVector<int,5> dim_ayp_here_y, 
	   	      TinyVector<int,4> dim_ayp_here,      
			  TinyVector<int,3> dim_ayp, 
			  TinyVector<int,2> dim_y, 
			  Parstruc* pars,
			  Array<double,7> data_stay,
			  Array<double,7> data_sell,
			  Array<double,7> data_rent,
			  Array<double,7> data_buy,
			  Array<double,2> G,	
			  Array<double,2> Gp,	
			  Array<double,2> MoveCost,
			  Array<double,1> Amenity,
			  int verbose);
                  
		const std::string version(){ return( name ); };
		int MaxDim(){ return(ResStay.dimensions()); };
			

		// getters
		Array<double,7> GetResStay(    void ) const {return(ResStay);};
		Array<double,7> GetResSell(    void ) const {return(ResSell);};
		Array<double,7> GetResBuy(     void ) const {return(ResBuy );};
		Array<double,7> GetResRent(    void ) const {return(ResRent);};
		Array<double,5> GetVown(       void ) const {return(Vown)   ;};
		Array<double,5> GetVrent(      void ) const {return(Vrent)  ;};
		Array<double,5> GetEVown(      void ) const {return(EVown)  ;};
		Array<double,5> GetEVrent(     void ) const {return(EVrent) ;};
		Array<int   ,5> GetDown(       void ) const {return(Down)   ;};
		Array<int   ,5> GetDrent(      void ) const {return(Drent)   ;};
		Array<double,5> Getv_stay(  void ) const {return(v_stay) ;};
		Array<double,5> Getv_sell(  void ) const {return(v_sell) ;};
		Array<double,5> Getv_rent(  void ) const {return(v_rent) ;};
		Array<double,5> Getv_buy(   void ) const {return(v_buy)  ;};
		//Array<double,2> GetmoveCost(  void ) const {return(MoveCost) ;};
		Array<int   ,5> Getmove_stay(  void ) const {return(move_stay) ;};
		Array<int   ,5> Getmove_sell(  void ) const {return(move_sell) ;};
		Array<int   ,5> Getmove_rent(  void ) const {return(move_rent) ;};
		Array<int   ,5> Getmove_buy(   void ) const {return(move_buy)  ;};
		Array<double,6> Getv_loc_stay( void ) const {return(v_loc_stay);};
		Array<double,6> Getv_loc_rent( void ) const {return(v_loc_rent);};
		Array<double,6> Getv_loc_sell( void ) const {return(v_loc_sell);};
		Array<double,6> Getv_loc_buy(  void ) const {return(v_loc_buy );};
		Array<double,6> Getc_loc_stay( void ) const {return(c_loc_stay);};
		Array<double,6> Getc_loc_rent( void ) const {return(c_loc_rent);};
		Array<double,6> Getc_loc_sell( void ) const {return(c_loc_sell);};
		Array<double,6> Getc_loc_buy(  void ) const {return(c_loc_buy );};
		Array<int   ,6> Gets_loc_stay( void ) const {return(s_loc_stay);};
		Array<int   ,6> Gets_loc_rent( void ) const {return(s_loc_rent);};
		Array<int   ,6> Gets_loc_sell( void ) const {return(s_loc_sell);};
		Array<int   ,6> Gets_loc_buy(  void ) const {return(s_loc_buy );};
		Array<double,2> GetG(          void ) const {return(G);};
		Array<double,2> GetGp(          void ) const {return(Gp);};
		TinyVector<int,4> GetDim_ayp_here(    void ) const {return(dim_ayp_here);};
		int GetMaxage( void ) const {return(maxage);};


		//setters
		void ReferenceStay( Array<double,7> x ) { ResStay.reference( x ) ; }
		void ReferenceSell( Array<double,7> x ) { ResSell.reference( x ) ; }
		void ReferenceRent( Array<double,7> x ) { ResRent.reference( x ) ; }
		void ReferenceBuy( Array<double,7> x ) { ResBuy.reference( x ) ; }
		
		void ReferenceG( Array<double,2> x ) { G.reference( x ) ; }
		void ReferenceGp( Array<double,2> x ) { Gp.reference( x ) ; }
		void SetP( Parstruc* par ) { p = *par ; }

		//// other member functions		
		void show ( void );
		void ComputePeriod(int age);
		void ComputeExpectations( int age );

		void ComputeStay( int age );
		void ComputeSell( int age );
		void ComputeRent( int age );
		void ComputeBuy( int age );
		void ComputeLocationChoice( int age );
		void ComputeDchoice( int age );
		void FindStayCons( int age );
		void FindSellCons( int age );
		void FindRentCons( int age );
		void FindBuyCons( int age );

		//std::vector<double> GetResStayNumeric( void );
		//std::vector<double> GetResSellNumeric( void );
		//std::vector<double> GetResRentNumeric( void );
		//std::vector<double> GetResBuyNumeric( void );
		
		Array<double,4> dchoice4d(Array<double,4> one, Array<double,4> two);
		Array<int   ,4> dchoiceID4d(Array<double,4> one, Array<double,4> two);
		Array<double,4> integrate(Array<double,4> tens);




		//Rcpp::List RcppExport( void );	


};
