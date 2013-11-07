
#include <iostream>
#include "CMig.h"
#include "CMig6.h"
#include <vector>
#include <random/uniform.h>
#include <random/discrete-uniform.h>

int main(int argument_count, char ** command_line_arguments)
{ 
	std::cout <<  std::endl;
	std::cout <<  std::endl;
	cout << "Boilerplate for CMig6 " << endl;
	cout << "======================" << endl;
	cout << endl;
	
	cout << endl;
	cout << "Default Constructor for CMig6 " << endl;
	cout << "======================" << endl;
	cout << endl;
	CMig6 mig1;
	mig1.show();

	cout << endl;
	cout << "Constructor for CMig6 with dimensions" << endl;
	cout << "======================" << endl;
	cout << endl;

	int nA = 8;
	int nY = 2;
	int nP = 3;
	int nL = 5;
	int nT = 3;

	CMig6 defmig(nA,nY,nP,nL,nT);
	defmig.show();

	cout << endl;
	cout << "Referenced Constructor for CMig6 " << endl;
	cout << "======================" << endl;
	cout << endl;

	Array<double,2> trans(nY,nY,FortranArray<2>());
	trans = 0.9,0.3,0.1,0.7;

	Array<double,2> transP(nP,nP,FortranArray<2>());
	transP = 0.9025,0.0475,0.0025,
			 0.095,0.905,0.095,
			 0.0025,0.0475,0.9025;

	// get some data
	TinyVector<int,6> aypHTt = shape(nA,nY,nP,nL,nL,nY);

	Array<double,6> tstay(aypHTt,FortranArray<6>());	
	Array<double,6> tsell(aypHTt,FortranArray<6>());	
	Array<double,6> trent(aypHTt,FortranArray<6>());	
	Array<double,6> tbuy( aypHTt,FortranArray<6>());	


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

	CMig6 myMig_ref(nA,nY,nP,nL,nT,
			    &pars2, tstay, tsell, trent, tbuy, trans,
				transP, MoveCost, Amenity, agrid, blim_own, blim_buy,
				blim_rent, verbose);
        
	myMig_ref.show();
	
	// can compute a period?
	for (int ti=myMig_ref.GetMaxage(); ti>0; ti--){
		myMig_ref.ComputePeriod( ti );
	}
   
	//myMig_ref.LimitOwner();
	//Array<double,6> myctmp(nA, nY, nP, nL, nL ,nA, FortranArray<6>());
	//myctmp = myMig_ref.GetCtmp() ;

	//for (int ia=1;ia<nA+1; ia++){
		//for (int iy=1;iy<nY+1; iy++){
			//for (int ip=1;ip<nP+1; ip++){
				//for (int ih=1;ih<nL+1; ih++){
					//for (int it=1;it<nL+1; it++){
							
						//for (int is=1;is<nA+1; is++){

							//// at all savings indices greater than the blimit index, 
							//// ctmp should be a positive number

								//cout << "ctmp = " << myctmp(ia,iy,ip,ih,it,is) << " at " << ia << iy << ip << ih << it << is << ", blim is " << blim_own(ih,it,ip) << endl;

						//}
					//}
				//}
			//}
		//}
	//}
	
	return 0;
}


	// boilerplate for CMig class.

	//std::cout <<  std::endl;
	//std::cout <<  std::endl;
	//std::cout << "Don't expect any sensible output, as resources are random." << std::endl;

	//Array<double,5> stay(shape(2,2,2,2,2),FortranArray<5>());
	//Array<double,5> sell(shape(2,2,2,2,2),FortranArray<5>());
	//Array<double,5> rent(shape(2,2,2,2,2),FortranArray<5>());
	//Array<double,5> buy( shape(2,2,2,2,2),FortranArray<5>());

	//stay = 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,
		   //17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32;
    //sell = 17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,
						 //1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16;
    //rent = 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,
				//17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32;
	//buy  = 17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,
						 //1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16;

	//Array<double,2> trans(shape(2,2),FortranArray<2>());
	//trans = 0.9,0.3,0.1,0.7;

	//Parstruc pars;
	//pars.beta = 0.9;
	//pars.myNA = -99;
	//pars.gamma   = 1.4;
	//pars.mgamma  = 1 - pars.gamma;
	//pars.imgamma = 1/pars.mgamma;

    //// create dimension vectors	                 
	//TinyVector<int,5> aypta = shape(2,2,2,2,2);
	//TinyVector<int,4> aypt  = shape(2,2,2,2);
	//TinyVector<int,4> aypa  = shape(2,2,2,2);
	//TinyVector<int,4> aypy  = shape(2,2,2,2);
	//TinyVector<int,3> ayp   = shape(2,2,2);
	//TinyVector<int,2> y     = shape(2,2);

	//// create an instance of owner class
	//CMig myMig( aypta,aypt, aypa, aypy,ayp, y, &pars, stay, sell, rent, buy, trans);
	////myMig.show();
	//myMig.ComputePeriod(2);
	//myMig.ComputePeriod(1);

	//cout << "c_stay " << endl;
	//cout << myMig.Getc_stay()  << endl;

	//cout << "s_stay " << endl;
	//cout << myMig.Gets_stay()  << endl;

