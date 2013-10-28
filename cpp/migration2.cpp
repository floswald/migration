

#include "CMig7.h"

int main(int argument_count, char ** command_line_arguments)
{ 
	std::cout << "Boilerplate program 2 is running" << std::endl;


	CMig7 myMig7(0);

	// allocate parameter struct
	gsl_f_pars p;
	p.gamma  = 1.4;
	p.mgamma = 1-1.4;
	p.beta   = 0.9;
	p.R      = 1/1.04;
	p.res    = 0;
	p.type   = gsl_interp_cspline;
	p.acc    = gsl_interp_accel_alloc ();
	p.spline = gsl_spline_alloc (p.type, myMig7.GetAgrid().size());
	p.T      = gsl_root_fsolver_brent;
	p.sroot  = gsl_root_fsolver_alloc (p.T);


 
    myMig7.setPars( &p );	
      //std::cout << "current V " <<  myMig7.GetV() << std::endl;	
	myMig7.ComputeSolution(2);
	myMig7.ComputeSolution(1);
	  std::cout << "current V " <<  myMig7.GetV() << std::endl;	
	
	return 0;
}
