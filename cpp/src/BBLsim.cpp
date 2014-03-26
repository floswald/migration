

#include "BBLsim.h"


// default constructor
CBBLsim::CBBLsim( void ){
	name = "BBLsim";

}

// constructor 2
CBBLsim::CBBLsim( int N_, int T_, int R_, int nS_, int K_, arma::mat initCond) :
	name("BBLsim"),
	N(N_),
	T(T_),
	R(R_),
	K(K_),
	nS(nS_) {
		dcube.zeros( N, T, R);
		logitmat.zeros(nS,K);
	}



// CBBLsim::simulate(){

// 	// Initiate an instance of Individual
// 	// constructor takes reduced form models
// 	Ind ind(RFmodels,aggPrices);

// 	for (i in 1:N){

// 		// setup i on first datavalue
// 		ind.register(i);
// 		nT = ind.getLifetime();	// how many years left

// 		for (int it=1; it<nT+1; it++){

// 			// find location in logitmat
// 			ind.newLoc();

// 			// predict housing choice
// 			ind.newHouse();

// 			// savings choice
// 			ind.saving();

// 			// record actions and states
// 			ind.report(it);

// 			// transition
// 			// update age, assets, house, state, duration,
// 			// debt, equity,
// 			// fill in new house prices and average incmoes
// 			// update incomes in all locations in logitmat
// 			// after drawing z-shock

// 			ind.transition(it);	// define Ind as inheriting from BBLsim so that it
// 			                    // can write into results matrix here.



// 		}
// 	}

// }

