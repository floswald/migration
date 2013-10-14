
#include <iostream>
#include "CMig.h"
#include <vector>

int main(int argument_count, char ** command_line_arguments)
{ 
	std::cout << "The main program is running" << std::endl;

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
	Parstruc* pp;
	pp = &pars;

    // create dimension vectors	                 
	TinyVector<int,5> aypta = shape(2,2,2,2,2);
	TinyVector<int,4> aypt  = shape(2,2,2,2);
	TinyVector<int,4> aypa  = shape(2,2,2,2);
	TinyVector<int,3> ayp   = shape(2,2,2);
	TinyVector<int,2> y     = shape(2,2);

	// create an instance of owner class
	CMig myMig( aypta,aypt, aypa, ayp, y, pp, stay, sell, rent, buy, trans);
	myMig.show();

	CMig migdef;
	migdef.show();

	CMig migc1(100,10,5,30,100);
	
    //Array<double,3> test(ayp,FortranArray<3>());
	//Array<double,3> bout(ayp,FortranArray<3>());
	//test = 1;
	//bout = t5.integrate(test);
	//cout << bout << endl;

	//t5.computePeriod( 2 );
	//t5.computePeriod( 1 );
	
	
	//std::cout << std::endl;
	//std::cout << "class type:" << std::endl;
	//t5.version();
	
	return 0;
}
