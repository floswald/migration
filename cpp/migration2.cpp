

#include "CMig7.h"

int main(int argument_count, char ** command_line_arguments)
{ 
	std::cout << "Boilerplate program 2 is running" << std::endl;


	CMig7 myMig7;

	// allocate parameter struct


	gsl_f_pars p;
	p.res    = 0;
	//p.type   = gsl_interp_cspline;
	p.type   = gsl_interp_linear;	// change interpolation type here.
	p.acc    = gsl_interp_accel_alloc ();
	p.spline = gsl_spline_alloc (p.type, myMig7.GetAgrid_own().size());
	p.T      = gsl_root_fsolver_brent;
	p.sroot  = gsl_root_fsolver_alloc (p.T);


 
    myMig7.setPars( &p );	
      //std::cout << "current V " <<  myMig7.GetV() << std::endl;	
	  //

	myMig7.utility( -1 );

	//double data[13] = {0.1,1,2,3,4,5,6,7,8,9,10,11,12};

	//myMig7.SplineTester( data, 13 );


	 

	for (int ti=2; ti>0; ti--){
		myMig7.ComputePeriod( ti );
	}
	Array<double,3> out(myMig7.GetVStay());
	  std::cout << "current Vstay " << out  << std::endl;	
	  std::cout << "current Vstay(:,1,1) " << out(Range::all(),1,1)  << std::endl;	
	  std::cout << "current Vstay(:,2,1) " << out(Range::all(),2,1)  << std::endl;	
	
	return 0;
}
