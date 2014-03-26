

#include "BBLsim.h" // Include the code that we are testing
#include <gtest/gtest.h> // Include the google test framework

using namespace arma;

class CBBLSimTest : public ::testing::Test {
	protected:

		// data members of the fixture
		int N;	// individuals
		int T;	// periods
		int R;	// replications
		int K;	// number of vars in datamatrix (id, age, state, h, a, m, eq, pay)
		int nS;	// number of states

		arma::mat iCond;	// initial conditions 

		void SetUp() {

			N=10;	// individuals
			T=20;	// periods
			R=2;	// replications
			K=8;	// number of vars in datamatrix (id, age, state, h, a, m, eq, pay)
			nS=9;	// number of states

			iCond.randu(N,K);
			
			// create a class on the heap
			BBL = new CBBLsim(N,T,R,nS,K,iCond);
		}


		virtual void TearDown(){ 
		
			delete BBL;
		}

		CBBLsim *BBL;
};




// test if can construct an instance
TEST_F(CBBLSimTest, CanGetName){

	EXPECT_EQ( "BBLsim", BBL->GetName() );
}

TEST_F(CBBLSimTest, DimsAreCorrect){

	cube myc = zeros<cube>(N,T,R);
	cube test = BBL->Getdcube();

	EXPECT_EQ( myc.n_cols, test.n_cols );
	EXPECT_EQ( myc.n_rows, test.n_rows );
	EXPECT_EQ( myc.n_slices, test.n_slices );

	mat mym = zeros<mat>(N,K);
	mat testm = BBL->Getlogitmat();

	EXPECT_EQ( myc.n_cols, test.n_cols );
	EXPECT_EQ( myc.n_rows, test.n_rows );
	EXPECT_EQ( myc.n_slices, test.n_slices );
}





int main(int argc, char **argv) { 
  ::testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}



