



// BBL Simulation Class
//
// This class simulates data for the BBL estimator
// based on 
// 1) a set of initial conditions, 
// 2) a set of reduced form policy functions, and
// 3) several sets of paths for exogenous stochastic state variables
//
// The main output of the class is an estimate for the
// value function of the problem. This estimate is obtained
// by averaging over many simulated lifecycle paths.
//
// The class will be used to simulate 
// *) The true value function, i.e. the one based on
//    the actual policies recovered from the data; as well as
// *) Fake policy functions
//
// The distance between true and fake value functions is an 
// input to the BBL estimator.


#ifndef BBL_SIM_H
#define BBL_SIM_H


struct parStruct{
	double tau;
	double beta;
};

#include <armadillo>

using namespace arma;

class CBBLsim {

	protected: 

		// data objects
		cube dcube;		// data cube. (id+period,variables,replication)
		cube aggPrices;	// aggregate prices. (period,priceType,replication)
		std::vector< std::vector<double> > RFcoefs; // coefficients of reduced form policies

		mat logitmat;	// nS by K matrix to predict i's income

		// RF objects
		std::vector<std::vector< int > > idx;	// indices of data


		// info objects
		std::string name;
		int N,T,R,K,nS;	// number of individuals, max periods, number of replications and num of states

		parStruct par;


	public:

		CBBLsim();	// default constructor
		CBBLsim(int N_, int T_, int R_, int nS_, int K_, arma::mat initCond);	// default constructor

		// member functions
		void show(void);

		// getters
		std::string GetName( void ) const { return(name); };
		cube  Getdcube( void ) const { return(dcube); };
		mat  Getlogitmat( void ) const { return(logitmat); };

};


class CIndividual: public CBBLsim {


};


#endif // endif once guard