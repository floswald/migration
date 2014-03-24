

#include "BBLsim.h" // Include the code that we are testing
#include <gtest/gtest.h> // Include the google test framework



class BBLSimTest : public ::testing::Test {
	protected:

		// data members of the fixture
		int N;	// individuals
		int T;	// periods
		int R;	// replications
		int K;	// number of vars in datamatrix (id, age, state, h, a, m, eq, pay)
		int nS;	// number of states

		arma::mat iCond;

		void SetUp() {

			N=10;	// individuals
			T=20;	// periods
			R=2;	// replications
			K=8;	// number of vars in datamatrix (id, age, state, h, a, m, eq, pay)
			nS=9;	// number of states

			iCond.randu(N,K);
			
			// create a class on the heap
			BBL = new BBLsim(N,T,R,nS,iCond);
		}


		virtual void TearDown(){ 
		
			delete BBL;
		}

		BBLsim *BBL;
};




// test if can construct an instance
TEST_F(BBLSimTest, CanGetName){

	EXPECT_EQ( "BBLsim", BBL->GetName() );

}

TEST_F(BBLSimTest, DimsOfdcubeAreCorrect){

	arma::cube myc = arma::zeros<arma::cube>(N,T,R);
	arma::cube test = BBL->Getdcube();

	EXPECT_EQ( myc.n_cols, test.n_cols );
	EXPECT_EQ( myc.n_rows, test.n_rows );
	EXPECT_EQ( myc.n_slices, test.n_slices );
}





int main(int argc, char **argv) { 
  ::testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}



