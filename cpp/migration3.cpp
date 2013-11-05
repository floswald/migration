


#include "CMig7.h"
#include <random/uniform.h>

int main(int argument_count, char ** command_line_arguments)
{ 
	std::cout <<  std::endl;
	std::cout <<  std::endl;
	std::cout << "Boilerplate program 3 is running" << std::endl;
	std::cout <<  std::endl;
	std::cout <<  std::endl;
	std::cout <<  std::endl;
	std::cout << "Don't expect any sensible output, as resources are random." << std::endl;



	// dim vectors
    TinyVector<int,3> dim_ay_t;

	int na = 20;
	int nT = 20;
    Array<double,2> trans(shape(2,2),FortranArray<2>());
	trans = 0.9,0.3,0.1,0.7;

    dim_ay_t = na,2,nT;

	// get some data

	Array<double,3> tstay(na,2,nT,neverDeleteData,FortranArray<3>());	
	Array<double,3> tsell(na,2,nT,neverDeleteData,FortranArray<3>());	

	//fill with random numbers
	ranlib::Uniform<double> uniGen;
	Array<double,3>::iterator it;

	for (it = tstay.begin(); it!=tstay.end(); it++) {
		*it = uniGen.random() + 1;
	}
	for (it = tsell.begin(); it!=tsell.end(); it++) {
		*it = uniGen.random();
	}

	// get two savings spaces
	Array<double,1> a_own(na);
	Array<double,1> a_rent(na);
	a_own(0) = -2;
	a_rent(0) = 0;
	a_own(na-1) = 3;
	a_rent(na-1) = 3;

	double step_own = (a_own(na-1)-a_own(0)) / na;
	double step_rent = (a_rent(na-1)-a_rent(0)) / na;
	for (int i=1;i<na;i++) a_own(i) = a_own(i-1) + step_own;
	for (int i=1;i<na;i++) a_rent(i) = a_rent(i-1) + step_rent;

	double cutoff, gamma, theta;
	cutoff = 0.1;
	gamma = 1.5;
	theta = 0.3;

	gsl_f_pars p;
	p.res    = 0;
	//p.type   = gsl_interp_cspline;
	p.type   = gsl_interp_linear;	// change interpolation type here.
	p.acc    = gsl_interp_accel_alloc ();
	p.spline = gsl_spline_alloc (p.type, a_own.size());
	p.T      = gsl_root_fsolver_brent;
	p.sroot  = gsl_root_fsolver_alloc (p.T);

	int verbose = 2;

	// create an instance of owner class
	CMig7 myMig(dim_ay_t,tstay,tsell,trans,a_own,a_rent,verbose,cutoff,gamma,theta,&p);

 
	// can compute a period?
	for (int ti=myMig.GetMaxage(); ti>0; ti--){
		myMig.ComputePeriod( ti );
	}
	Array<double,3> out(myMig.GetSStay());
	  std::cout << "current Sstay " << out  << std::endl;	
	  std::cout << "current Sstay(:,:,1) " << out(Range::all(),Range::all(),1)  << std::endl;	
	  std::cout << "current Sstay(:,:,2) " << out(Range::all(),Range::all(),2)  << std::endl;	
	  std::cout << "current Sstay(:,:,myMig.GetMaxage()-1) " << out(Range::all(),Range::all(),myMig.GetMaxage()-1)  << std::endl;	
	
	return 0;
}
