

#ifdef RcppCompile
#include "../../cpp/src/CMig_dev2.h"
#else
#include "CMig_dev2.h"		// include class declaration of owner class
#endif

#define _EPS 1.0e-9

// Constructor
CMig7::CMig7() :
	ResStay(5,2,2,FortranArray<3>()),
	ResSell(5,2,2,FortranArray<3>()),
	VStay(5,2,2,FortranArray<3>()),
	VSell(5,2,2,FortranArray<3>()),
	EVown(5,2,2,FortranArray<3>()),
	EVrent(5,2,2,FortranArray<3>()),
	CStay(5,2,2,FortranArray<3>()),
	CSell(5,2,2,FortranArray<3>()),
	SStay(5,2,2,FortranArray<3>()),
	SSell(5,2,2,FortranArray<3>()),
	c_cutoff(0.05),
	gamma(1.4),
	agrid_own(5),
	agrid_rent(5),
	myNA(-99),
	R(1.04),
	beta(0.9),
	name("CMig7"),
	G(2,2,FortranArray<2>()) ,
	evtmp(5),
	verbose(2) {
		ResStay = 0.1,2,3,4,5,
			  6,7,8,9,10,
		      0.1,2,3,4,5,
			  6,7,8,9,10;
		ResSell = 0.1,2,3,4,5,
			  6,7,8,9,10,
		      0.1,2,3,4,5,
			  6,7,8,9,10;
		VStay   = 0;
		VSell   = 0;
		agrid_own = -2,-1.5,4,8,10;
		agrid_rent = 0,0.5,5,8,10;
		EVown   = 0;
		EVrent  = 0;
		CStay = 0;
		CSell = 0;
		SStay = 0;
		SSell = 0;
		mgamma = 1-gamma;
		tmpu       = pow(c_cutoff,mgamma);
		dtmpu_dc   = tmpu / c_cutoff;
		ddtmpu_dcc = -gamma * (dtmpu_dc / c_cutoff );
	    G     = 0.9,0.3,0.1,0.7;
		evtmp = 0;
		dim = ResStay.extent();	
		bounds = ResStay.extent() + 1;	// shift this by one to get proper upper array bounds for fortran arrays
		maxage = dim(2);
		//p.gamma = 1.4;
		//p.mgamma = 1-1.4;
		//p.beta = 0.9;
		//p.R = 1.04;
		//p.res = 0;
	}


CMig7::CMig7( TinyVector<int,3> Dim_ay_t,
		      Array<double,3> data_stay,
			  Array<double,3> data_sell,
			  Array<double,2> data_G,
			  Array<double,1> data_a_own,
			  Array<double,1> data_a_rent,
			  int verb,
			  double d_cutoff, double d_gamma, double d_theta, gsl_f_pars *gslpar) :

	ResStay(Dim_ay_t,FortranArray<3>()),
	ResSell(Dim_ay_t,FortranArray<3>()),
	VStay(Dim_ay_t,FortranArray<3>()),
	VSell(Dim_ay_t,FortranArray<3>()),
	EVown(Dim_ay_t,FortranArray<3>()),
	EVrent(Dim_ay_t,FortranArray<3>()),
	CStay(Dim_ay_t,FortranArray<3>()),
	CSell(Dim_ay_t,FortranArray<3>()),
	SStay(Dim_ay_t,FortranArray<3>()),
	SSell(Dim_ay_t,FortranArray<3>()),
	c_cutoff(d_cutoff),
	gamma(d_gamma),
	theta(d_theta),
	agrid_own(Dim_ay_t(0)),
	agrid_rent(Dim_ay_t(0)),
	myNA(-99),
	R(1.04),
	beta(0.9),
	name("CMig7"),
	dim(Dim_ay_t),
	maxage(Dim_ay_t(2)),
	G(Dim_ay_t(1),Dim_ay_t(1),FortranArray<2>()) ,
	evtmp(Dim_ay_t(0)),
	verbose(verb) {
		// reference the data
		ResStay.reference(data_stay);
		ResSell.reference(data_sell);
		G.reference(data_G);
		agrid_own.reference(data_a_own);
		agrid_rent.reference(data_a_rent);
		VStay   = 0;
		VSell   = 0;
		EVown   = 0;
		EVrent  = 0;
		CStay = 0;
		CSell = 0;
		SStay = 0;
		SSell = 0;
		mgamma = 1-gamma;
		tmpu       = pow(c_cutoff,mgamma);
		dtmpu_dc   = tmpu / c_cutoff;
		ddtmpu_dcc = -gamma * (dtmpu_dc / c_cutoff );
		evtmp = 0;
		bounds = ResStay.extent() + 1;
		p = gslpar;
		//p.gamma = 1.4;
		//p.mgamma = 1-1.4;
		//p.beta = 0.9;
		//p.R = 1.04;
		//p.res = 0;
	}

//void CMig6::ComputePeriod(int age){

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

//}

void CMig7::ComputeStay(int age) {

	// initiate GSL objects

	p->pt_Class   = this;
	p->F.params   = p;
	p->F.function = &gslClassWrapper;

	// loop over states
	for (int i2=1;i2<bounds(1);++i2){		// y

		evtmp = EVown(Range::all(),i2,age + 1);

		if (age==(maxage-1)){
			// next period's EV is defined only over positive assets
			gsl_spline_init( p->spline, agrid_rent.data(), evtmp.data(), agrid_rent.size() );
			if (verbose>0) {
				cout << "evtmp owner in period " << age << " at state " << i2 << endl;
				cout << evtmp;
				cout << "approximation to evtmp owner in period " << age << " at state " << i2 << endl;
				for (int i=0;i<agrid_own.size();i++) evtmp(i) = gsl_spline_eval( p->spline, agrid_rent(i), p->acc);
				cout << evtmp;
				cout << "approximation to deriv evtmp owner in period " << age << " at state " << i2 << endl;
				for (int i=0;i<agrid_own.size();i++) evtmp(i) = gsl_spline_eval_deriv( p->spline, agrid_rent(i), p->acc);
				cout << evtmp;
			}
		} else {
			gsl_spline_init( p->spline, agrid_own.data(), evtmp.data(), agrid_own.size() );
			if (verbose>0) {
				cout << "evtmp owner in period " << age << " at state " << i2 << endl;
				cout << evtmp;
				cout << "approximation to evtmp owner in period " << age << " at state " << i2 << endl;
				for (int i=0;i<agrid_own.size();i++) evtmp(i) = gsl_spline_eval( p->spline, agrid_own(i), p->acc);
				cout << evtmp;
				cout << "approximation to deriv evtmp owner in period " << age << " at state " << i2 << endl;
				for (int i=0;i<agrid_own.size();i++) evtmp(i) = gsl_spline_eval_deriv( p->spline, agrid_own(i), p->acc);
				cout << evtmp;
			}
		}



		for (int i1=1;i1<bounds(0);++i1){	// a
		
			if (verbose>2) cout << "asset idx = " << i1 << endl;

			// feasible if Res(i1,i2,age) + 1/R max.borrow > 0
			// ie if max.borrow > -income(i2) * R/(R-1)
			// ie if -2 > -1 * 26 = -26
			// myfun = function(x){-1 * (1+x)/x}
			// curve(myfun)
			//
			//TODO
			//if (min(agrid_own) < -1 * (R/(R-1))) {
				//cout << "infeasible state at " << i1 << endl;
			
				//CStay(i1,i2,age) = myNA;
				//SStay(i1,i2,age) = myNA;
				//VStay(i1,i2,age) = myNA;

			//} else {
				
				p->res = ResStay(i1,i2,age);
				low    = Owner_blimit(i1,i2,age);	

				if ((age==(maxage-1)) && (hi>max(agrid_rent) || low < min(agrid_rent))  ){
					cout << "hi or low are outside asset grid at ia = " << i1 << ", iy = " << i2 << endl;
					hi     = max(agrid_rent) ;
				}
				if ((age<(maxage-1)) && (hi>max(agrid_own) || low < min(agrid_own))  ){
					cout << "hi or low are outside asset grid at ia = " << i1 << ", iy = " << i2 << endl;
					hi     = max(agrid_own) ;
				}


				// find root of euler equation
				root         = find_root(p->sroot,p->F, low , hi, verbose );
				
				if ((age==(maxage-1)) && (root>max(agrid_rent) || root < min(agrid_rent))  ){
					cout << "hi or low are outside asset grid at ia = " << i1 << ", iy = " << i2 << endl;
				}
				if ((age<(maxage-1)) && (root>max(agrid_own) || root < min(agrid_own))  ){
					cout << "hi or low are outside asset grid at ia = " << i1 << ", iy = " << i2 << endl;
				}
									 
				CStay(i1,i2,age) = p->res - R * root;
				SStay(i1,i2,age) = root;
				VStay(i1,i2,age) = utility(CStay(i1,i2,age)) + theta + beta * gsl_spline_eval(p->spline, root , p->acc);
				//V(i1,i2,age) = utility(C(i1,i2,age), p->mgamma) + p.theta - MoveCost(i4,i5) + Amenity(i5) + beta * gsl_spline_eval(p->spline, root , p->acc);
			//}
		}
		//gsl_interp_accel_reset( p->acc );
	}
}


double CMig7::Owner_blimit( int ix1, int ix2, int age){

	double lim;

	if (age==(maxage-1)){

		lim = 0.0;

	} else {

		lim = agrid_own(0);

	}
	return(lim);
}
		
	






void CMig7::ComputeSell(int age) {

	// initiate gsl objects

	p->pt_Class   = this;
	p->F.params   = p;
	p->F.function = &gslClassWrapper;


	// loop over states
	for (int i2=1;i2<bounds(1);++i2){		// y

		evtmp = EVrent(Range::all(),i2,age + 1);
		gsl_spline_init( p->spline, agrid_rent.data(), evtmp.data(), agrid_rent.size() );
		if (verbose>1) cout << "evtmp seller = " << evtmp << endl;

		for (int i1=1;i1<bounds(0);++i1){	// a

			// feasible?
			//if (ResSell(i1,i2,age) < 0) {
			
				//CSell(i1,i2,age) = myNA;
				//SSell(i1,i2,age) = myNA;
				//SSell(i1,i2,age) = myNA;

			//} else {

				p->res = ResSell(i1,i2,age);
				hi     = min(p->res - _EPS, max(agrid_rent) );
				low    = agrid_rent(0);	// seller cannot borrow

				if (hi>max(agrid_rent) || low < min(agrid_rent) ){
					cout << "hi or low are outside asset grid at ia = " << i1 << ", iy = " << i2 << endl;
				}

				// find root of euler equation
				root         = find_root(p->sroot,p->F, low , hi, verbose );

				if (root>max(agrid_rent) || root < min(agrid_rent) ){

					cout << "root outside asset grid at ia = " << i1 << ", iy = " << i2 << endl;

				}

				CSell(i1,i2,age) = p->res - R * root;
				SSell(i1,i2,age) = root;
				VSell(i1,i2,age) = utility(CSell(i1,i2,age)) +    0    + beta * gsl_spline_eval(p->spline, root , p->acc);
				//V(i1,i2,age) = utility(C(i1,i2,age), p->mgamma) + p.theta - MoveCost(i4,i5) + Amenity(i5) + beta * gsl_spline_eval(p->spline, root , p->acc);
			//}
		}
		//gsl_interp_accel_reset( p->acc );
	}
}



/*double CMig7::SplineTesterDeriv( double data[] ) {*/

	//gsl_spline_init( p->spline, agrid_rent.data(), data, agrid_rent.size() );

	//double vals[data.size()];

	//for (int i=0;i<data.size();i++){

		//grad[i] = gsl_spline_eval_deriv(p->spline, data[i], p->acc);
	//}
	//return(grad);
/*}*/



void CMig7::ComputePeriod( int age ){
			
	if (verbose>1) cout << endl<< endl<< endl<< endl;
	if (verbose>1) cout << "----------------------------" << endl;
	if (verbose>1) cout << "in period = " << age << endl;

	if (age==maxage) {

		// TODO make sure ResStay(Range::all(),Range::all(),age=T) = a + p + y.


		for (int i1=1;i1<bounds(0);++i1){		// a
			for (int i2=1;i2<bounds(1);++i2){	// y
			if (verbose>2) cout << "ResStay(:,iy,it) = " << ResStay(Range::all(),i2,age) << endl;
			if (verbose>2) cout << "ResSell(:,iy,it) = " << ResSell(Range::all(),i2,age) << endl;
				//EVown(  i1, i2,age) = bequest(ResStay(i1,i2,age) ,-3);
				//EVrent( i1, i2,age) = bequest(ResSell(i1,i2,age) ,-3);
				EVown(  i1, i2,age) = utility(ResStay(i1,i2,age) );
				EVrent( i1, i2,age) = utility(ResSell(i1,i2,age) );
			}
		}


	} else {

		ComputeStay( age );
		ComputeSell( age );
		if (verbose>2) cout << "VStay(:,:,it) = " << VStay(Range::all(),Range::all(),age) << endl;
		if (verbose>2) cout << "VSell(:,:,it) = " << VSell(Range::all(),Range::all(),age) << endl;
		//ComputeDchoice( age );
		ComputeExpectations( age );
		if (verbose>2) cout << "EVown(:,:,it) = " << EVown(Range::all(),Range::all(),age) << endl;
		if (verbose>2) cout << "EVrent(:,:,it) = " << EVrent(Range::all(),Range::all(),age) << endl;

		if (age==1) {
			// clean up memory
			gsl_interp_accel_free( p->acc );
			gsl_spline_free( p->spline );
			gsl_root_fsolver_free( p->sroot );
		}

	}

}


void CMig7::ComputeExpectations( int age ){

	EVown( Range::all(),Range::all(),age) = integrate(VStay( Range::all(),Range::all(),age));
	EVrent(Range::all(),Range::all(),age) = integrate(VSell(Range::all(),Range::all(),age));

}


Array<double,2> CMig7::integrate(Array<double,2> tens){
	
	firstIndex   i1;	// a
	secondIndex  i2;    // y
	thirdIndex   i3;	// y'
	
	Array<double,2> ret(dim(0),dim(1),FortranArray<2>());

	Array<double,3> tmp(dim(0),dim(1),dim(1),FortranArray<3>());	// tmp(i1,i2,i3)
	tmp = tens(i1,i2) * G(i3,i2);
	ret = sum( tmp(i1,i3,i2), i3);
		  
	return(ret);

}




double CMig7::obj(double x, void * par) {

	gsl_f_pars *p = (gsl_f_pars *)par;
	// obtain interpolation

	double dev = gsl_spline_eval_deriv(p->spline, x, p->acc); 

	double cons = p->res - 1/R * x;
	double mu   = mutility( cons );
	const double out = mu - beta * dev;

	if (verbose>2){
		cout << "in objective function:" << endl;
		cout << "======================" << endl;
		cout << "x = " << x << endl;
		cout << "d.ev = " << dev << endl;
		cout << "marginal u = " << mu << endl;
		cout << "objective " << out << endl;
	}

	return out;
}


//double CMig7::obj_min(double x, void * par) {

	//gsl_f_pars *p = (gsl_f_pars *)par;
	//// obtain interpolation

	//double ev = gsl_spline_eval(p->spline, x, p->acc); 

	//double cons = p->res - R * x;
	//double u   = utility( cons );
	//const double out = u + beta * ev;

	//if (verbose>2){
		//cout << "in objective function:" << endl;
		//cout << "======================" << endl;
		//cout << "x = " << x << endl;
		//cout << "ev = " << ev << endl;
		//cout << "u = " << u << endl;
		//cout << "objective " << out << endl;
	//}

	//return (-1) * out;
//}





// Wrapper that points to member function
// Trick: MyClass is an element of the gsl_f_pars struct
// so we can tease the value of the objective function out
// of there.
double gslClassWrapper(double x, void * pp) {
	gsl_f_pars *p = (gsl_f_pars *)pp;
	return p->pt_Class->obj(x,p);
}

//double gslClassWrapperMin(double x, void * pp) {
	//gsl_f_pars *p = (gsl_f_pars *)pp;
	//return p->pt_Class->obj_min(x,p);
//}



// helper function for GSL root finder
// ===================================
  
double find_root(gsl_root_fsolver *RootFinder,  gsl_function F, double x_lo, double x_hi,int verb) {
	
  	int iter = 0,status;
  	int maxiter = 100;
  	double r;

	// check if straddle zero
	bool straddle = false;

	double up   = GSL_FN_EVAL( &F, x_hi );
	double down = GSL_FN_EVAL( &F, x_lo );

	straddle = (up * down < 0);

	if (!straddle) { 
	  
		if (verb>2){
			cout << "obj does not straddle zero. assign bound closest to zero." << endl;
	 	 }

		// if distance from zero is smaller 
		// at upper bound, pick upper bound
		if (abs(up) < abs(down)) {

			r = x_hi;

		} else {

			r = x_lo;

		}
		return(r) ;

	} else {

	  gsl_root_fsolver_set (RootFinder, &F, x_lo , x_hi );

		//std::cout << "objective function value is " << GSL_FN_EVAL(F,2.0) << std::endl;
		
	  if (verb>2){
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


			if (verb>2) {
				  printf ("%5d [%.7f, %.7f] %.7f %.7f\n",
				  iter, x_lo, x_hi,
				  r,  
				  x_hi - x_lo);
			}
	  } while (status == GSL_CONTINUE && iter < maxiter);
	  return(r);
	}
}

// helper function for GSL max finder
// ===================================
  
/*double find_max(gsl_min_fminimizer *MinFinder,  gsl_function F, double x_lo, double x_hi,int verb) {*/
	
      //int iter = 0,status;
      //int maxiter = 100;
      //double r;

	//// check if straddle zero
	//bool straddle = false;

	//double up   = GSL_FN_EVAL( &F, x_hi );
	//double down = GSL_FN_EVAL( &F, x_lo );

	//straddle = (up * down < 0);

	//if (!straddle) { 
	  
		//if (verb>2){
			//cout << "obj does not straddle zero. assign bound closest to zero." << endl;
		  //}

		//// if distance from zero is smaller 
		//// at upper bound, pick upper bound
		//if (abs(up) < abs(down)) {

			//r = x_hi;

		//} else {

			//r = x_lo;

		//}
		//return(r) ;

	//} else {

	  //gsl_root_fsolver_set (RootFinder, &F, x_lo , x_hi );

		////std::cout << "objective function value is " << GSL_FN_EVAL(F,2.0) << std::endl;
		
	  //if (verb>2){
			//cout << "in root finder." << endl;
			//printf ("%5s [%9s, %9s] %9s %9s\n",
			  //"iter", "lower", "upper", "root", 
			   //"err(est)");
	  //}

	  //do {
			//iter++;
			//status = gsl_root_fsolver_iterate (RootFinder);
			//r      = gsl_root_fsolver_root    (RootFinder);
			//x_lo   = gsl_root_fsolver_x_lower (RootFinder);
			//x_hi   = gsl_root_fsolver_x_upper (RootFinder);
			//status = gsl_root_test_interval (x_lo, x_hi,0, 0.0001);


			//if (verb>2) {
				  //printf ("%5d [%.7f, %.7f] %.7f %.7f\n",
				  //iter, x_lo, x_hi,
				  //r,  
				  //x_hi - x_lo);
			//}
	  //} while (status == GSL_CONTINUE && iter < maxiter);
	  //return(r);
	//}
/*}*/


// TODO make log utility switch for gamma==1
double CMig7::utility( double cons ) { 

	double util;

	if (cons < c_cutoff) {
		
		diff = cons - c_cutoff;

		util = (1/mgamma) * tmpu + dtmpu_dc * diff + 0.5 * ddtmpu_dcc * pow(diff,2);

	} else {

		util = (1/mgamma) * pow(cons,mgamma);

	}
	return util;
}
	
double CMig7::mutility( double cons ) { 

	double grad;

	if (cons < c_cutoff) {
		
		diff = cons - c_cutoff;

		grad = dtmpu_dc + ddtmpu_dcc * diff;

	} else {

		grad = 1 / pow( cons, gamma );
	}
	return grad;
}



