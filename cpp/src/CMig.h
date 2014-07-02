

// Migration Class Header File
// ===========================

#include <iostream>
#include <vector>
#include <algorithm>	// using min in show method
#include <armadillo>
#include <string.h>
#include <map>

// defined the euler mascheroni constant
// http://en.wikipedia.org/wiki/Gumbel_distribution
#define euler_mascheroni 0.5772


#ifndef ONCE_CMIG_H
#define ONCE_CMIG_H



// struct to hold parameters
struct PStruct{
	double beta;
	double myNA;
	double gamma;
	double mgamma;
	double imgamma;
	double theta;
	double R;
};



class CMig {
private: 
	std::vector<double> v, c, rho, EV, vbar, EVfinal;
	std::vector<int> s, dh;

	int nJ,ny,np,nP,nz,na,nh,ntau,nT,aone;

	std::map<std::string, std::vector<double> > grids;


public:
	// Constructor
	CMig( PStruct p_, std::vector<int>dimvec, std::map<std::string, std::vector<double> > grids_ );

};


// endif ONCE_CMIG_H
#endif	