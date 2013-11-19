


#include <string>
#include <gsl/gsl_roots.h>
#include <gsl/gsl_interp.h>
#include <gsl/gsl_spline.h>
#include <gsl/gsl_errno.h>
#include <blitz/array.h>
#include <vector>
#include <stdio.h>

using namespace blitz;

// Migration model class for N locations and continuous optimization
// =================================================================
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

#ifndef ONCE_COWNER_D3_H
#define ONCE_COWNER_D3_H

// define gsl parameter struct
struct gsl_f_pars ;


class CMig8 {
	private:
		// private data objects
		Array<double,3> Res;	//(a,y,age)
		Array<double,3> V;	    //(a,y,age)
		Array<double,3> C;	    //(a,y,age)
		Array<double,3> S;	    //(a,y,age)
		Array<double,1> evtmp;	    //(a)
		gsl_f_pars *p;
		Array<double,1> agrid;	    //(a)
		double root;
		double verbose;


		// GSL related members
		//std::vector<double>::iterator eviter;
		
		// private member functions

	    const std::string name; // A member variable for the class to store the version 
	public: 
		// constructor
		CMig7(double verb);

		const std::string version(){ return( name ); };
		int MaxDim(){ return(Res.dimensions()); };
			
		double utility( double cons, double mgam ) { return( (1/mgam) * pow(cons,mgam) ) ;};
		double mutility( double cons, double gam ) { return(  1 / pow(cons,gam) ) ;};

		double dummyDV( double save ) { return( 1/save ); }

		double obj(double x, void * par) ;
		void setPars( gsl_f_pars * xp ) { p = xp; };

		// getters
		Array<double,3> GetResStay( void ) const {return(Res);};
		Array<double,3> GetV( void ) const {return(V);};
		Array<double,1> GetAgrid( void ) const {return(agrid);};

		void ComputeSolution( int age );
};


struct gsl_f_pars {
	double gamma;
	double mgamma;
	double beta;
	double R;
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

double find_root(gsl_root_fsolver *RootFinder,  gsl_function  F, double x_lo, double x_hi, double verbose) ;

#endif
