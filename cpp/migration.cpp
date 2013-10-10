
#include <iostream>
#include "CMig.h"
#include <vector>

int main(int argument_count, char ** command_line_arguments)
{ 
	std::cout << "The main program is running" << std::endl;

	double dstay[] = {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,
	                 17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32};
	double dsell[] = {17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,
	                 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16};
	double drent[] = {17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,
	                 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16};
	double dbuy[] = {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,
					 17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32};


	double trans[] = {0.9,0.1,0.1,0.9};

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

	// create pointers to them
	TinyVector<int,5> *paypta;
	TinyVector<int,4> *paypt ;
	TinyVector<int,4> *paypa ;
	TinyVector<int,3> *payp  ;
	TinyVector<int,2> *py  ;


	// store address of dim vecs in pointers
	paypta = &aypta;
	paypt  = &aypt ;
	paypa  = &aypa ;
	payp   = &ayp  ;
	py     = &y;


	// create an instance of owner class
	CMig t5(paypta,paypt,paypa,payp,py,dstay,dsell,drent,dbuy,trans,pp);
	
	//t5.show();


	t5.computePeriod( 2 );
	t5.computePeriod( 1 );
	
	
	std::cout << std::endl;
	std::cout << "class type:" << std::endl;
	t5.version();
	
	return 0;
}
