

#include "CMig7.h"

void CMig7::ComputeSolution( int age ){

	if (age==2) {

	} else {

		// you do the spline/solver allocation once per period.
		// probaly better to create those on the stack with new?

		const gsl_interp_type *t = gsl_interp_cspline; 
		p->acc            = gsl_interp_accel_alloc ();
		p->spline         = gsl_spline_alloc (t, 10);
		p->pt_Class       = this;

		gsl_function F;
		F.params   = p;
		F.function = &gslClassWrapper;

		const gsl_root_fsolver_type *T;
		gsl_root_fsolver *sroot;
		T     = gsl_root_fsolver_brent;
		sroot = gsl_root_fsolver_alloc (T);

		int low = 0;
		int hi  = 10;

		TinyVector<int,3> ext;
		ext = Res.extent(); 
		for (int i1=0;i1<ext(1);++i1){		// y
     
			// prepare the interpolation
			evtmp = V(Range::all(),i1,age + 1);
														   
			gsl_spline_init( p->spline, agrid.data(), evtmp.data(), 10 );

			for (int i2=0;i2<ext(0);++i2){	// a
				
				p->res = Res(i1,i2,age);

				// find the root here!
				root         = find_root(sroot,&F, low , hi );
				C(i1,i2,age) = p->res - p->R*root;
				S(i1,i2,age) = root;
				V(i1,i2,age) = utility(C(i1,i2,age), p->mgamma) + p->beta * gsl_spline_eval(p->spline, root , p->acc);

			}

		}
	}

}

double CMig7::obj(double x, void * par) {

	gsl_f_pars *p = (gsl_f_pars *)par;
	// obtain interpolation
	double ev = gsl_spline_eval(p->spline, x, p->acc); // or so

	double cons = p->res - p->R * x;
	double mu   = mutility( cons, p->gamma );
	double out;

	out = mu - p->beta * ev;

	return out;
}


CMig7::CMig7( ) :
	Res(2,2,2),
	V(2,2,2),
	agrid(2),
	name("CMig7"),
	evtmp(2) {
		Res = 1,2,3,4,
			  5,6,7,8;
		V   = -10,-9,-8,-7,-6,
			  -5,-4,-3,-2,-1;
		agrid = 1,2;
		//p.gamma = 1.4;
		//p.mgamma = 1-1.4;
		//p.beta = 0.9;
		//p.R = 1.04;
		//p.res = 0;
	}

// Wrapper that points to member function
// Trick: MyClass is an element of the gsl_f_pars struct
// so we can tease the value of the objective function out
// of there.
double gslClassWrapper(double x, void * pp) {
	gsl_f_pars *p = (gsl_f_pars *)pp;
	return p->pt_Class->obj(x,p);
}

// helper function for GSL root finder
// ===================================
  
double find_root(gsl_root_fsolver *RootFinder,  gsl_function * F, double x_lo, double x_hi) {
  gsl_root_fsolver_set (RootFinder, F, x_lo , x_hi );

	//std::cout << "objective function value is " << GSL_FN_EVAL(F,2.0) << std::endl;
	
  int iter = 0,status;
  int maxiter = 100;
  double r;
		printf ("%5s [%9s, %9s] %9s %9s\n",
          "iter", "lower", "upper", "root", 
           "err(est)");
  do {
        iter++;
        status = gsl_root_fsolver_iterate (RootFinder);
        r      = gsl_root_fsolver_root    (RootFinder);
        x_lo   = gsl_root_fsolver_x_lower (RootFinder);
        x_hi   = gsl_root_fsolver_x_upper (RootFinder);
        status = gsl_root_test_interval (x_lo, x_hi,0, 0.0001);


		      printf ("%5d [%.7f, %.7f] %.7f %.7f\n",
              iter, x_lo, x_hi,
              r,  
              x_hi - x_lo);
  } while (status == GSL_CONTINUE && iter < maxiter);
  return(r);
}

