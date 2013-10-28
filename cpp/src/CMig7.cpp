

#include "CMig7.h"


//void CMig6::ComputePeriod(int age){

	////const gsl_interp_type *t = gsl_interp_cspline; 
	////p->acc            = gsl_interp_accel_alloc ();
	////p->spline         = gsl_spline_alloc (t, agrid.size());
	//p->pt_Class       = this;

	//gsl_function F;
	//F.params   = p;
	//F.function = &gslClassWrapper;

	////const gsl_root_fsolver_type *T;
	////gsl_root_fsolver *sroot;
	////T     = gsl_root_fsolver_brent;
	////sroot = gsl_root_fsolver_alloc (T);

	//// if final operiod, then preComputed resources are utility
	//// unfortunately TinyVector dim_ayp_here_t only available as C++ array, so different indexing for those.
	//if (age==dim_ayp_here_t(4)) {
		//// EV(a,y,p,here,age)
		//EVown( Range::all(),Range::all(),Range::all(),Range::all(),age) = ResStay(Range::all(),Range::all(),Range::all(),Range::all(),1,age,dim_ayp_here_t(0));	//dim_ayp_here_t(0) is index of last element in savings vector.
		//EVrent(Range::all(),Range::all(),Range::all(),Range::all(),age) = ResRent(Range::all(),Range::all(),Range::all(),Range::all(),1,age,dim_ayp_here_t(0));

	//} else {

		//ComputeStay(age);		// get v_stay 
		//ComputeSell(age);		// get v_sell 	
		//ComputeRent(age);		// get v_rent 
		//ComputeBuy(age);		// get v_buy
		//ComputeLocationChoice(age);		// 
		//ComputeDchoice(age);		// 
		//ComputeExpectations(age);	// get EVown and EVrent
	
	//}

/*}*/

void CMig7::ComputeSolution( int age ){

	if (age==2) {


	} else {

		// you do the spline/solver allocation once per period.
		// probaly better to create those on the stack with new?

		//const gsl_interp_type *t = gsl_interp_cspline; 
		//p->acc            = gsl_interp_accel_alloc ();
		//p->spline         = gsl_spline_alloc (t, agrid.size());
		p->pt_Class       = this;

		gsl_function F;
		F.params   = p;
		F.function = &gslClassWrapper;

		//const gsl_root_fsolver_type *T;
		//gsl_root_fsolver *sroot;
		//T     = gsl_root_fsolver_brent;
		//sroot = gsl_root_fsolver_alloc (T);

		double low = 0.01;
		double hi = 10;

		// TODO check low-high interval is within agrid!

		TinyVector<int,3> ext;
		ext = Res.extent(); 
		for (int i1=0;i1<ext(1);++i1){		// y
     
			// prepare the interpolation
			//evtmp = V(Range::all(),i1,age + 1);

			evtmp = log(agrid); 

			gsl_spline_init( p->spline, agrid.data(), evtmp.data(), agrid.size() );
														   

			for (int i2=0;i2<ext(0);++i2){	// a
				
				p->res = Res(i1,i2,age);
				hi = p->res - 0.01;

				// find the root here!
				root         = find_root(p->sroot,&F, low , hi, verbose );
                                     
				C(i1,i2,age) = p->res - p->R * root;
				S(i1,i2,age) = root;
				V(i1,i2,age) = utility(C(i1,i2,age), p->mgamma) + p->beta * gsl_spline_eval(p->spline, root , p->acc);

			}

		}
	}

}

double CMig7::obj(double x, void * par) {

	gsl_f_pars *p = (gsl_f_pars *)par;
	// obtain interpolation

	double ev = gsl_spline_eval_deriv(p->spline, x, p->acc); 

	double cons = p->res - p->R * x;
	double mu   = mutility( cons, p->gamma );
	double out;

	out = mu - p->beta * ev;

	if (verbose>1){
		cout << "in objective function: x = " << x << endl;
		cout << "objective " << out << endl;
	}

	return out;
}


CMig7::CMig7( double verb ) :
	Res(5,2,2),
	V(5,2,2),
	C(5,2,2),
	S(5,2,2),
	agrid(11),
	name("CMig7"),
	evtmp(11),
	verbose(verb) {
		Res = 1,2,3,4,5,
			  6,7,8,9,10,
		      1,2,3,4,5,
			  6,7,8,9,10;
		V   = -100,-9,-8,-7,-6,
			  -100,-9,-8,-7,-6,
		      -100,-9,-8,-7,-6,
			  -100,-9,-8,-7,-6;
		C = 0;
		S = 0;
		agrid = 0.01,1,2,3,4,5,6,7,8,9,10;
		evtmp = 0;
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
  
double find_root(gsl_root_fsolver *RootFinder,  gsl_function * F, double x_lo, double x_hi,double verbose) {
  gsl_root_fsolver_set (RootFinder, F, x_lo , x_hi );

	//std::cout << "objective function value is " << GSL_FN_EVAL(F,2.0) << std::endl;
	
  int iter = 0,status;
  int maxiter = 100;
  double r;
  if (verbose>0){
		cout << "in root finder." << endl;
		printf ("%5s [%9s, %9s] %9s %9s\n",
          "iter", "lower", "upper", "root", 
           "err(est)");
  }

  do {
        iter++;
        status = gsl_root_fsolver_iterate (RootFinder);
        r      = gsl_root_fsolver_root    (RootFinder);
        x_lo   = gsl_root_fsolver_x_lower (RootFinder);
        x_hi   = gsl_root_fsolver_x_upper (RootFinder);
        status = gsl_root_test_interval (x_lo, x_hi,0, 0.0001);


		if (verbose>0) {
		      printf ("%5d [%.7f, %.7f] %.7f %.7f\n",
              iter, x_lo, x_hi,
              r,  
              x_hi - x_lo);
		}
  } while (status == GSL_CONTINUE && iter < maxiter);
  return(r);
}

