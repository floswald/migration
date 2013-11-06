
#include <iostream>
#include "CMig.h"
#include "CMig6.h"
#include <vector>
#include <random/uniform.h>

int main(int argument_count, char ** command_line_arguments)
{ 
	std::cout <<  std::endl;
	std::cout <<  std::endl;
	std::cout << "Boilerplate program is running" << std::endl;
	std::cout <<  std::endl;
	std::cout <<  std::endl;
	std::cout <<  std::endl;
	std::cout << "Don't expect any sensible output, as resources are random." << std::endl;

	Array<double,5> stay(shape(2,2,2,2,2),FortranArray<5>());
	Array<double,5> sell(shape(2,2,2,2,2),FortranArray<5>());
	Array<double,5> rent(shape(2,2,2,2,2),FortranArray<5>());
	Array<double,5> buy( shape(2,2,2,2,2),FortranArray<5>());

	stay = 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,
		   17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32;
    sell = 17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,
						 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16;
    rent = 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,
				17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32;
	buy  = 17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,
						 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16;

	Array<double,2> trans(shape(2,2),FortranArray<2>());
	trans = 0.9,0.3,0.1,0.7;

	Parstruc pars;
	pars.beta = 0.9;
	pars.myNA = -99;
	pars.gamma   = 1.4;
	pars.mgamma  = 1 - pars.gamma;
	pars.imgamma = 1/pars.mgamma;

    // create dimension vectors	                 
	TinyVector<int,5> aypta = shape(2,2,2,2,2);
	TinyVector<int,4> aypt  = shape(2,2,2,2);
	TinyVector<int,4> aypa  = shape(2,2,2,2);
	TinyVector<int,4> aypy  = shape(2,2,2,2);
	TinyVector<int,3> ayp   = shape(2,2,2);
	TinyVector<int,2> y     = shape(2,2);

	// create an instance of owner class
	CMig myMig( aypta,aypt, aypa, aypy,ayp, y, &pars, stay, sell, rent, buy, trans);
	//myMig.show();
	myMig.ComputePeriod(2);
	myMig.ComputePeriod(1);

	cout << "c_stay " << endl;
	cout << myMig.Getc_stay()  << endl;

	cout << "s_stay " << endl;
	cout << myMig.Gets_stay()  << endl;

	cout << endl;
	cout << endl;
	cout << "Boilerplate for CMig6 " << endl;
	cout << "======================" << endl;
	cout << endl;

	// dim vectors
	TinyVector<int,6> dim_ayp_here_there_t; 
	TinyVector<int,6> dim_ayp_here_there_a; 
	TinyVector<int,5> dim_ayp_here_there; 
	TinyVector<int,5> dim_ayp_here_t; 
	TinyVector<int,5> dim_ayp_here_y; 
	TinyVector<int,4> dim_ayp_here;      
	TinyVector<int,3> D_ayp; 
	TinyVector<int,2> D_y; 

	int nT = 3;
	int nL = 4;
	int nP = 3;
	int nA = 11;

	dim_ayp_here_there_t  = nA,2,nP,nL,nL,nT;
	dim_ayp_here_there_a  = nA,2,nP,nL,nL,2; 
	dim_ayp_here_there    = nA,2,nP,nL,nL;
	dim_ayp_here_t        = nA,2,nP,nL,nT;
	dim_ayp_here_y        = nA,2,nP,nL,2;
	dim_ayp_here          = nA,2,nP,nL;   
	D_ayp                 = nA,2,nP;
	D_y                   = 2,2;

	Array<double,2> transP(nP,nP,FortranArray<2>());
	transP = 0.9025,0.0475,0.0025,
		     0.095,0.905,0.095,
			 0.0025,0.0475,0.9025;

	// get some data

	Array<double,6> tstay(dim_ayp_here_there_t,FortranArray<6>());	
	Array<double,6> tsell(dim_ayp_here_there_t,FortranArray<6>());	
	Array<double,6> trent(dim_ayp_here_there_t,FortranArray<6>());	
	Array<double,6> tbuy( dim_ayp_here_there_t,FortranArray<6>());	


	//fill with random numbers
	ranlib::Uniform<double> uniGen;
	Array<double,6>::iterator it;

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

    Array<double,2> MoveCost(shape(2,2),FortranArray<2>());
	MoveCost = 0,1,1,0;

    Array<double,1> Amenity(2,FortranArray<1>());
	Amenity = 1,2;

	Array<int,3> blim_own(nL,nL,nP,FortranArray<3>());
	Array<int,2> blim_buy(nL,nP,FortranArray<2>());
	int blim_rent = nA-1;
	blim_own = 3;
	blim_buy = 4;

	int verbose = 1;
	
	PStruct pars2;
	pars2.beta = 0.9;
	pars2.myNA = -99;
	pars2.gamma   = 1.4;
	pars2.mgamma  = 1 - pars2.gamma;
	pars2.imgamma = 1/pars2.mgamma;

	CMig6 myMig6(dim_ayp_here_there_t,                                          
				dim_ayp_here_there_a, 
				dim_ayp_here_there, 
				dim_ayp_here_t, 
				dim_ayp_here_y, 
				dim_ayp_here,      
				D_ayp, 
				D_y, 
				&pars2,
				tstay,tsell,trent,tbuy,trans, transP, MoveCost, Amenity, agrid, blim_own, blim_buy, blim_rent, verbose);
        
	myMig6.show();

	// can compute a period?
	for (int ti=myMig6.GetMaxage(); ti>0; ti--){
		myMig6.ComputePeriod( ti );
	}

	Array<double,5> ret = myMig6.GetEVown();
	cout << "EVown(:,:,:,:,nT-1) is = " << endl;
	cout << ret(Range::all(),Range::all(),Range::all(),Range::all(),myMig6.GetMaxage()-1) << endl;
	cout << "EVown(:,:,:,:,2) is = " << endl;
	cout << ret(Range::all(),Range::all(),Range::all(),Range::all(),2) << endl;
	cout << "EVown(:,:,:,:,1) is = " << endl;
	cout << ret(Range::all(),Range::all(),Range::all(),Range::all(),1) << endl;
	cout << "END OF Boilerplates " << endl;

	//CMig migdef;
	//migdef.show();

	//CMig migc1(100,10,5,30,100);
	
	return 0;
}
