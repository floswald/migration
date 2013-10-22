

#include "CMig7.h"

int main(int argument_count, char ** command_line_arguments)
{ 
	std::cout << "Boilerplate program 2 is running" << std::endl;


	CMig7 myMig7;

	gsl_f_pars p;
	p.gamma=1.4;
	p.mgamma=1-1.4;
	p.beta=0.9;
	p.R=1.04;
	p.res=0;
 
    myMig7.setPars( &p );	
  	std::cout << "current V " <<  myMig7.GetV() << std::endl;	
	myMig7.ComputeSolution(2);
	myMig7.ComputeSolution(1);
    myMig7.GetV();	
	
	return 0;
}
