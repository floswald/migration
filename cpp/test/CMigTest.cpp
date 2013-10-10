
#include "CMig.h" // Include the code that we are testing
#include <vector>
#include <gtest/gtest.h> // Include the google test framework

using namespace blitz;



// TEST(CMigTest, ResPointersCorrect) {

// 	double data2[] = {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,
// 	                 17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32};
// 	double data3[] = {17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,
// 	                 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16};
// 	double fake[] = {2};

// 	Parstruc pars;
// 	pars.beta = 0.9;
// 	pars.myNA = -99;
// 	Parstruc* pp;
// 	pp = &pars;

//     // create dimension vectors	                 
// 	TinyVector<int,5> aypta = shape(2,2,2,2,2);
// 	TinyVector<int,4> aypt  = shape(2,2,2,2);
// 	TinyVector<int,4> aypa  = shape(2,2,2,2);
// 	TinyVector<int,3> ayp   = shape(2,2,2);

// 	// create pointers to them
// 	TinyVector<int,5> *paypta;  paypta
// 	TinyVector<int,4> *paypt ;  paypt 
// 	TinyVector<int,4> *paypa ;  paypa 
// 	TinyVector<int,3> *payp  ;  payp  

// 	// store address of dim vecs in pointers
// 	paypta = &aypta;
// 	paypt  = &aypt ;
// 	paypa  = &aypa ;
// 	payp   = &ayp  ;


// 	// create an instance of owner class
// 	CMig t5(paypta,paypt,paypa,payp,data2,data3,pp);

	
// 	EXPECT_EQ(data2,t5.getResStay().data());	// expect that data fed to class is ok
// 	EXPECT_EQ(data3,t5.getResSell().data());	// expect that data fed to class is ok
//     EXPECT_NE(fake,t5.getResSell().data());		// expect not equal

// }


// TEST(CMigTest, ResAreFortranArrays) {
// 	double data2[] = {1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,
// 	                 17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32};
// 	double data3[] = {17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,
// 	                 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16};
// 	Parstruc pars;
// 	pars.beta = 0.9;
// 	pars.myNA = -99;
// 	Parstruc* pp;
// 	pp = &pars;

//     // create dimension vectors	                 
// 	TinyVector<int,5> aypta = shape(2,2,2,2,2);
// 	TinyVector<int,4> aypt  = shape(2,2,2,2);
// 	TinyVector<int,4> aypa  = shape(2,2,2,2);
// 	TinyVector<int,3> ayp   = shape(2,2,2);

// 	// create pointers to them
// 	TinyVector<int,5> *paypta;
// 	TinyVector<int,4> *paypt ;
// 	TinyVector<int,4> *paypa ;
// 	TinyVector<int,3> *payp  ;

// 	// store address of dim vecs in pointers
// 	paypta = &aypta;
// 	paypt  = &aypt ;
// 	paypa  = &aypa ;
// 	payp   = &ayp  ;


// 	// create an instance of owner class
// 	CMig t5(paypta,paypt,paypa,payp,data2,data3,pp);


// 	// fortranArray has blitz::ordering() vector 0,1,...,blitz::rank()-1
// 	// check last entry of that with
// 	int storage[] = {0,1,2,3,4};	// fortran order
// 	//int storage[] = {2,1,0};	// C order

// 	TinyVector<int,5> stay = t5.getResStay().ordering();
// 	TinyVector<int,5> sell = t5.getResSell().ordering();

// 	for (int i=0;i<3;i++) {
// 		EXPECT_EQ(storage[i],stay(i)) << "stay has wrong ordering at dim " << i;
// 		EXPECT_EQ(storage[i],sell(i)) << "sell has wrong ordering at dim " << i;
// 	}
	
// }

TEST(CMigTest, ResContents) {
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

	// get data
	Array<double,5> dStay = t5.getResStay();
	Array<double,5> dSell = t5.getResSell();

	// define an iterator for a blitz array
	Array<double,5>::iterator iter;

	// define two std::vectors to take the values
	std::vector<double> stayvec;
	std::vector<double> sellvec;

	// iterate over blitz and fill vector with values
	for (iter = dStay.begin() ; iter!=dStay.end();++iter){
		stayvec.push_back(*iter);
	}

	for (iter = dSell.begin() ; iter!=dSell.end();++iter){
		sellvec.push_back(*iter);
	}


	for (int i=0; i<sellvec.size(); i++){
		EXPECT_EQ(dstay[i],stayvec.at(i)) << "ResStay has wrong value at idx " << i;
		EXPECT_EQ(dsell[i],sellvec.at(i)) << "ResSell has wrong value at idx " << i;
	}
}


TEST(CMigTest, checkIntegration) {
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

	// get data
	Array<double,3> test(ayp,FortranArray<3>());
	Array<double,3> bout(ayp,FortranArray<3>());
	test = 1;
	bout = t5.integrate(test);

	std::vector<double> out;
	// define an iterator for a blitz array
	Array<double,3>::iterator iter;

	// iterate over blitz and fill vector with values
	for (iter = bout.begin() ; iter!=bout.end();++iter){
		out.push_back(*iter);
	}

	for (int i=0; i<out.size(); i++){
		EXPECT_EQ(1,out.at(i)) << "integration not 1 " << i;
	}
}




int main(int argc, char **argv) { // A main function scaffold to call the tests
  ::testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}
