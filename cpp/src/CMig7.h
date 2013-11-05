

#include <string>
#include <gsl/gsl_roots.h>
#include <gsl/gsl_interp.h>
#include <gsl/gsl_spline.h>
#include <gsl/gsl_errno.h>
#include <blitz/array.h>
#include <vector>
#include <stdio.h>

using namespace blitz;

// Migration model class for 1 location and continuous optimization on 3 state variables (a,y,t)
// =============================================================================================
//

// define gsl parameter struct
struct gsl_f_pars ;

///////////////////////////////////////////////////////////////////////////////////
// \brief Class for N locations and continuous optimization of migration model
//
// This class holds the entire solution to a migration model. it is initiated
// on a default set of parameters, the data needs to be set manually. The 
// use of this class is geared towards passing data from R to c++, then mapping
// it to the class. the class uses Blitz++ quite extensively. Also GSL optimization
// and spline interpolation is used.
// this version has location as a state variable "here" and "there", both can
// take N values. 
// - If here =  there, no migration occurs. 
// - if here != there, there is a moving cost in terms of utility. There is NO difference
// in prices (y,p) between locations as of yet.
//
//
// ## notes ##
// 1. arrays are fortranArrays throughout. that means they are indexed 1,2,3,...,Rank in each dimension
// 2. this is done purely for compatibility reasons with R. An array in R is in fortran order. 
//    I want compatibility with the same computation in R in order to check the results. THIS MAY CHANGE IN THE FUTURE.
// 3. unfortunately TinyVectors are not available as FortranArrays. Therefore, they are indexed as 0,1,...,Rank-1 .
///////////////////////////////////////////////////////////////////////////////////
class CMig7 {
	private:
		/**
		 * Resource arrays. 
		 * Contain available resources (cash on hand)
		 * at each point in the state space. in final period
		 * contains net wealth
		 */
		Array<double,3> ResStay, ResSell;	//(a,y,age)

		/**
		 * Conditional (expected) Value Functions
		 * for each discrete choice, there is a value function
		 */
		Array<double,3> VStay, VSell;	    //(a,y,age)
		Array<double,3> EVown, EVrent;	    //(a,y,age)
		
		/**
		 * Conditional Cons and Save Functions
		 * for each discrete choice, there is a policy function
		 */
		Array<double,3> CStay, CSell;	    //(a,y,age)
		Array<double,3> SStay, SSell;	    //(a,y,age)

		Array<double,1> evtmp;	    //(a)
		TinyVector<int,3> bounds;	
		TinyVector<int,3> dim;	
		gsl_f_pars *p;
		Array<double,1> agrid_own, agrid_rent;	    // asset grids to approximate EVown and EVrent
		double root, hi, low;
		double c_cutoff, gamma, mgamma, theta, diff, tmpu, dtmpu_dc, ddtmpu_dcc, myNA, R, beta;
		int verbose;
		Array<double,2> G;
		int maxage;


		// GSL related members
		//std::vector<double>::iterator eviter;
		
		// private member functions

	    const std::string name; // A member variable for the class to store the version 
	public: 
		/**
		 * default constructor
		 */
		CMig7();

		/**
		 * constructor to set up production
		 * use this constructor to do some actual computation
		 */
		CMig7(TinyVector<int,3> dim_ay_t, 
			  Array<double,3> data_stay,
			  Array<double,3> data_sell,
			  Array<double,2> G,
			  Array<double,1> data_a_own,
			  Array<double,1> data_a_rent,
			  int verb,
			  double d_cutoff,
			  double d_gamma,
			  double d_theta,
			  gsl_f_pars *gslpar);
			  

		const std::string version(){ return( name ); };
		int GetMaxDim(){ return(ResStay.dimensions()); };
		TinyVector<int,3> GetDim(){ return(dim); };
			
		double utility( double cons ) ;
		double mutility( double cons );

		double dummyDV( double save ) { return( 1/save ); }

		double obj(double x, void * par) ;
		void setPars( gsl_f_pars * xp ) { p = xp; };

		// getters
		Array<double,3> GetResStay( void ) const {return(ResStay);};
		Array<double,3> GetVStay( void ) const {return(VStay);};
		Array<double,3> GetVSell( void ) const {return(VSell);};
		Array<double,3> GetSStay( void ) const {return(SStay);};
		Array<double,3> GetSSell( void ) const {return(SSell);};
		Array<double,3> GetCStay( void ) const {return(CStay);};
		Array<double,3> GetCSell( void ) const {return(CSell);};
		Array<double,3> GetEVown( void ) const {return(EVown);};
		Array<double,3> GetEVrent( void ) const {return(EVrent);};
		Array<double,1> GetAgrid_own( void ) const {return(agrid_own);};
		Array<double,1> GetAgrid_rent( void ) const {return(agrid_rent);};
		double GetCutoff( void ) const { return(c_cutoff); };
		double GetGamma( void ) const { return(gamma); };
		double GetMgamma( void ) const { return(mgamma); };
		double GetTmpu( void ) const { return(tmpu); };
		double GetDtmpu( void ) const { return(dtmpu_dc); };
		double GetDdtmpu( void ) const { return(ddtmpu_dcc); };

		void ComputePeriod( int age );
		void ComputeExpectations( int age );
		void ComputeStay( int age );
		void ComputeSell( int age );
		Array<double,2> integrate(Array<double,2> tens);
		double Owner_blimit( int ix1, int ix2, int age);
		double bequest( double money, double beq ){ return( pow(money, beq)) ;};
		int GetMaxage( void ) const {return(maxage);};
		//Array<double,13> SplineTester( double data[] , int size );
		//double SplineTesterDeriv( double data[] );
};


struct gsl_f_pars {
	double res;
	gsl_interp_accel *acc;
	gsl_spline *spline;
	gsl_function F;
	const gsl_interp_type *type; 
    CMig7 *pt_Class;	
	const gsl_root_fsolver_type *T;
	gsl_root_fsolver *sroot;
};


double gslClassWrapper(double x, void * pp) ; 

double find_root(gsl_root_fsolver *RootFinder,  gsl_function  F, double x_lo, double x_hi, int verb) ;

// declare utility func as a Blitz function for final period
//BZ_DECLARE_FUNCTION(CMig7::utility)
