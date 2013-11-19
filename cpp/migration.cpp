
#include <iostream>
#include "CMig.h"
#include <vector>
#include <random/uniform.h>
#include <random/discrete-uniform.h>

int main(int argument_count, char ** command_line_arguments)
{ 
	std::cout <<  std::endl;
	std::cout <<  std::endl;
	cout << "Boilerplate for CMig " << endl;
	cout << "======================" << endl;
	cout << endl;
	
	cout << endl;
	cout << "Default Constructor for CMig " << endl;
	cout << "======================" << endl;
	cout << endl;
	CMig mig1;
	mig1.show();

	cout << endl;
	cout << "Constructor for CMig with dimensions" << endl;
	cout << "======================" << endl;
	cout << endl;

	int nA = 8;
	int nY = 2;
	int nP = 3;
	int nL = 5;
	int nZ = 3;
	int nT = 3;

	CMig defmig(nA,nY,nP,nL,nZ,nT);
	defmig.show();

	cout << endl;
	cout << "Referenced Constructor for CMig " << endl;
	cout << "======================" << endl;
	cout << endl;

	Array<double,2> trans(nY,nY,FortranArray<2>());
	trans = 0.9,0.3,0.1,0.7;

	Array<double,2> transP(nP,nP,FortranArray<2>());
	transP = 0.9025,0.0475,0.0025,
			 0.095,0.905,0.095,
			 0.0025,0.0475,0.9025;

	// get some data
	TinyVector<int,7> aypHTZt = shape(nA,nY,nP,nL,nL,nZ,nY);

	Array<double,7> tstay(aypHTZt,FortranArray<7>());	
	Array<double,7> tsell(aypHTZt,FortranArray<7>());	
	Array<double,7> trent(aypHTZt,FortranArray<7>());	
	Array<double,7> tbuy( aypHTZt,FortranArray<7>());	


	//fill with random numbers
	ranlib::Uniform<double> uniGen;
	Array<double,7>::iterator it;

	for (it = tstay.begin(); it!=tstay.end(); it++) {
		*it = uniGen.random() + 1;
	}
	for (it = tsell.begin(); it!=tsell.end(); it++) {
		*it = uniGen.random() + 1;
	}
	for (it = trent.begin(); it!=trent.end(); it++) {
		*it = uniGen.random() + 1;
	}
	for (it = tbuy.begin(); it!=tbuy.end(); it++) {
		*it = uniGen.random() + 1;
	}

	// savings grid
	Array<double,1> agrid(nA,FortranArray<1>());
	agrid(1) = -2;
	agrid(nA) = 3;

	double step_own = (agrid(nA)-agrid(1)) / nA;
	for (int i=2;i<nA+1;i++) agrid(i) = agrid(i-1) + step_own;


	Array<double,2> MoveCost(nL,nL,FortranArray<2>());
	MoveCost = 100;

	Array<double,1> Amenity(nL,FortranArray<1>());
	Amenity = 12;

	Array<int,3> blim_own(nL,nL,nP,FortranArray<3>());
	Array<int,2> blim_buy(nL,nP,FortranArray<2>());
	int blim_rent = nA-1;
	blim_buy = nA-1;


	// create borrowing limits are random indices
	ranlib::DiscreteUniform<int> Disc( nA );
	Array<int,3>::iterator it3;
	for (it3 = blim_own.begin();
		 it3 != blim_own.end();
		 it3 ++){
		*it3 = Disc.random() + 1;
	}
	int verbose = 1;
	
	PStruct pars2;
	pars2.beta = 0.9;
	pars2.myNA = -99;
	pars2.gamma   = 1.4;
	pars2.mgamma  = 1 - pars2.gamma;
	pars2.imgamma = 1/pars2.mgamma;
	pars2.R       = 1/(1+0.04);
	pars2.type   = gsl_interp_linear;	// change interpolation type here.
	pars2.acc    = gsl_interp_accel_alloc ();
	pars2.spline = gsl_spline_alloc (pars2.type, nP );

	// make savings tensors
	Array<double,4> save_own(nL,nL,nP,nA,FortranArray<4>());
	for (int here=1; here<nL+1; here++){
	for (int there=1;there<nL+1;there++){
	for (int pr=1;   pr<nP+1;   pr++){
		save_own(here,there,pr,Range::all() ) = agrid;
		if (blim_own(here,there,pr)>0) save_own(here,there,pr,Range(fromStart,blim_own(here,there,pr)) ) = (-1) * pars2.myNA;
	}}}

	Array<double,3> save_buy(nL,nP,nA,FortranArray<3>());
	save_buy=2;

	Array<double,1> save_rent(agrid);

	// Constructor needs GpEval and GpInt!
	// fill with ascending numbers
	Array<double,3> GpEval(nL,nT,nP,FortranArray<3>());
	Array<double,4> GpInt( nL,nZ,nT,nP,FortranArray<4>());

	for (std::pair<Array<double,3>::iterator, int> i(GpEval.begin(),1); 
			i.first != GpEval.end();
			++i.first, ++i.second){
		*i.first = i.second;
	}
	
	for (std::pair<Array<double,4>::iterator, int> i(GpInt.begin(),1); 
			i.first != GpInt.end();
			++i.first, ++i.second){
		*i.first = i.second;
	}


	CMig myMig_ref(nA,nY,nP,nL,nZ,nT,
				&pars2, tstay, tsell, trent, tbuy, trans,
				transP, MoveCost, Amenity, save_own, save_buy, save_rent, GpEval, GpInt, verbose);
		
	myMig_ref.show();
	
	return 0;
}




