

#include "BBLsim.h"


// default constructor
BBLsim::BBLsim( void ){
	name = "BBLsim";

}

// constructor 2
BBLsim::BBLsim( int N_, int T_, int R_, int nS_, arma::mat initCond) :
	name("BBLsim"),
	N(N_),
	T(T_),
	R(R_),
	nS(nS_) {
		dcube.zeros( N, T, R);
	}


