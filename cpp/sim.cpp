
#include "simclass.h"

using namespace arma;

int main(){

	std::cout<< "boilerplate running." << std::endl;
	std::cout<< "This will NOT give sensible results, just checking if program runs." << std::endl;
	std::cout << std::endl;


	int na = 50; int ny=7; int np=5; int nm=5; int nh=2; int nap;int nT = 30;

	int verbose = 0;


	// asset grids
	double agrid[na];
			//for (int i=0;i<na;i++) agrid[i] = -3 + i* (10 - (-3)) / (floor(na)-1) ;
			//does not work without centering on zero!
	for (int i=0;i<na;i++) agrid[i] = -3 + i;
			
	// find aposgrid as positive portion of agrid
	int	azero = 0;
	int i=0;
	while (azero==0){
		if (agrid[i]>=0) {
			azero = i;
		}
		i++;
	}

	nap = na - azero;
	double aposgrid[nap];
	for (int i=0;i<nap;i++) aposgrid[i] = agrid[i + azero ];

	// mortgage grid
	arma::mat mgrid(nm,nh);
	for (int i=0;i<nm;i++){
	for (int ih=0;ih<nh;ih++){
		mgrid(i,ih) = i ;
	}}

	// price grid
	arma::mat pgrid(np,nh);
	for (int i=0;i<np;i++){
	for (int ih=0;ih<nh;ih++){
		pgrid(i,ih) = i+ih;
	}}
	// income grid
	arma::mat ygrid(ny,nT);
	for (int i=0;i<ny;i++){
	for (int it=0;it<nT;it++){
		ygrid(i,it) = i+it;
	}}

	double Gy [ny*ny];
	double Gp [np*np];
	for (int ip=0;ip<np;ip++) {
		for (int jp=0;jp<np;jp++) {
	  Gp[ ip + np * jp ] = (ip + np*jp)/(np*np);
	}}
	for (int iy=0;iy<ny;iy++) {
		for (int jy=0;jy<ny;jy++) {
	  Gy[ iy + ny * jy ] = (iy + ny*jy)/(ny*ny);
	}}

	pStruct par;
	par.delta = 0.5;
	par.R = 1.04;
	par.down=0.2;
	par.phi_s = 0.98;
	par.myNA = -99;
	par.hex = 1;
	par.nSim = 10000;	// number of inds to simulate
	par.verbose = verbose;

	int a_y_p_m_h_T    = na *  ny * np * nm * nh * nT;
	int apos_y_p_m_h_T = nap * ny * np * nm * nh * nT;
	int a_y_p_h_T      = na * ny * np * nh * nT;
	int y_p_m_h_T      =       ny * np * nm * nh * nT;
	int a_y_p_T        = na *  ny * np           * nT;
	int apos_y_p_T     = nap *  ny * np           * nT;
	int y_p_T          =       ny * np           * nT;

	arma::imat s_stay_L = arma::randi<arma::imat>(a_y_p_m_h_T, 2 , arma::distr_param(0,(na-1)) ); 
	arma::mat v_stay_L = arma::randu<arma::mat>(a_y_p_m_h_T, 2 );
	arma::mat c_stay_L = arma::randu<arma::mat>(a_y_p_m_h_T, 2 );
	arma::mat v_sell_L = arma::randu<arma::mat>(a_y_p_m_h_T, 2 );
	arma::imat s_sell_L = arma::randi<arma::imat>(a_y_p_m_h_T, 2 , arma::distr_param(0,(na-1)) );
	arma::mat c_sell_L = arma::randu<arma::mat>(a_y_p_m_h_T, 2 );
	arma::mat v_def_L  = arma::randu<arma::mat>(a_y_p_m_h_T, 2 );
	arma::imat s_def_L  = arma::randi<arma::imat>(a_y_p_m_h_T, 2 , arma::distr_param(0,(na-1)) );
	arma::mat c_def_L  = arma::randu<arma::mat>(a_y_p_m_h_T, 2 );

	arma::mat v_file7_L  = arma::randu<arma::mat>(y_p_m_h_T, 2 );
	arma::mat c_file7_L  = arma::randu<arma::mat>(y_p_m_h_T, 2 );
	arma::mat v_file13_L = arma::randu<arma::mat>(y_p_m_h_T, 2 );
	arma::mat c_file13_L = arma::randu<arma::mat>(y_p_m_h_T, 2 );
	arma::mat v_filedef_L = arma::randu<arma::mat>(y_p_m_h_T, 2 );
	arma::mat c_filedef_L = arma::randu<arma::mat>(y_p_m_h_T, 2 );
		
	arma::mat vBK7_stay_L = arma::randu<arma::mat>(apos_y_p_m_h_T, 2 );
	arma::imat sBK7_stay_L = arma::randi<arma::imat>(apos_y_p_m_h_T, 2 , arma::distr_param(0,(nap-1)) );
	arma::mat cBK7_stay_L = arma::randu<arma::mat>(apos_y_p_m_h_T, 2 );
	arma::mat vBK7_sell_L = arma::randu<arma::mat>(apos_y_p_m_h_T, 2 );
	arma::imat sBK7_sell_L = arma::randi<arma::imat>(apos_y_p_m_h_T, 2 , arma::distr_param(0,(nap-1)) );
	arma::mat cBK7_sell_L = arma::randu<arma::mat>(apos_y_p_m_h_T, 2 );
	arma::mat vBK7_def_L  = arma::randu<arma::mat>(apos_y_p_m_h_T, 2 );
	arma::imat sBK7_def_L  = arma::randi<arma::imat>(apos_y_p_m_h_T, 2 , arma::distr_param(0,(nap-1)) );
	arma::mat cBK7_def_L  = arma::randu<arma::mat>(apos_y_p_m_h_T, 2 );
											   
	arma::mat vBK13_stay_L = arma::randu<arma::mat>(apos_y_p_m_h_T, 2 );
	arma::imat sBK13_stay_L = arma::randi<arma::imat>(apos_y_p_m_h_T, 2 , arma::distr_param(0,(nap-1)) );
	arma::mat cBK13_stay_L = arma::randu<arma::mat>(apos_y_p_m_h_T, 2 );
	arma::mat vBK13_sell_L = arma::randu<arma::mat>(apos_y_p_m_h_T, 2 );
	arma::imat sBK13_sell_L = arma::randi<arma::imat>(apos_y_p_m_h_T, 2 , arma::distr_param(0,(nap-1)) );
	arma::mat cBK13_sell_L = arma::randu<arma::mat>(apos_y_p_m_h_T, 2 );
	arma::mat vBK13_def_L  = arma::randu<arma::mat>(apos_y_p_m_h_T, 2 );
	arma::imat sBK13_def_L  = arma::randi<arma::imat>(apos_y_p_m_h_T, 2 , arma::distr_param(0,(nap-1)) );
	arma::mat cBK13_def_L  = arma::randu<arma::mat>(apos_y_p_m_h_T, 2 );

	//Renter	
	arma::mat Rv_rent_L  = arma::randu<arma::mat>(a_y_p_T, 2 );
	arma::imat Rs_rent_L  = arma::randi<arma::imat>(a_y_p_T, 2 , arma::distr_param(0,(na-1)) );
	arma::mat Rc_rent_L  = arma::randu<arma::mat>(a_y_p_T, 2 );

	// renter file
	arma::mat Rv_file7_L  = arma::randu<arma::mat>(y_p_T, 2 );
	arma::mat Rc_file7_L  = arma::randu<arma::mat>(y_p_T, 2 );
	arma::mat Rv_file13_L = arma::randu<arma::mat>(y_p_T, 2 );
	arma::mat Rc_file13_L = arma::randu<arma::mat>(y_p_T, 2 );
	arma::mat RDL_file7   = arma::randu<arma::mat>(y_p_T, 2 );
	arma::mat RDL_file13  = arma::randu<arma::mat>(y_p_T, 2 );

	//// renter bk
	arma::mat Rv_BK7_L = arma::randu<arma::mat>(apos_y_p_T, 2 );
	arma::imat Rs_BK7_L = arma::randi<arma::imat>(apos_y_p_T, 2 , arma::distr_param(0,(nap-1)) );
	arma::mat Rc_BK7_L = arma::randu<arma::mat>(apos_y_p_T, 2 );
	arma::mat Rv_BK13_L  = arma::randu<arma::mat>(apos_y_p_T, 2 );
	arma::imat Rs_BK13_L  = arma::randi<arma::imat>(apos_y_p_T, 2 , arma::distr_param(0,(nap-1)) );
	arma::mat Rc_BK13_L  = arma::randu<arma::mat>(apos_y_p_T, 2 );

	////buyer
	
	arma::mat Rv_buy1_L = arma::randu<arma::mat>(a_y_p_h_T, 2 );
	arma::mat Rv_buy2_L = arma::randu<arma::mat>(a_y_p_h_T, 2 );
	arma::imat Rs_buy1_L = arma::randi<arma::imat>(a_y_p_T, 2 , arma::distr_param(0,(na-1)) );
	arma::imat Rs_buy2_L = arma::randi<arma::imat>(a_y_p_T, 2 , arma::distr_param(0,(na-1)) );
	arma::mat Rc_buy1_L = arma::randu<arma::mat>(a_y_p_h_T, 2 );
	arma::mat Rc_buy2_L = arma::randu<arma::mat>(a_y_p_h_T, 2 );

	// prob of bk functions
	arma::vec Pr7_own   = arma::randu<arma::vec>( a_y_p_m_h_T );
	arma::vec Pr13_own  = arma::randu<arma::vec>( a_y_p_m_h_T );

	arma::vec Pr7_buy1= arma::randu<arma::vec>( a_y_p_T );
	arma::vec Pr7_buy2= arma::randu<arma::vec>( a_y_p_T );
	arma::vec Pr13_buy1= arma::randu<arma::vec>( a_y_p_T );
	arma::vec Pr13_buy2= arma::randu<arma::vec>( a_y_p_T );
	arma::vec Pr13_rent= arma::randu<arma::vec>( a_y_p_T );
	arma::vec Pr7_rent = arma::randu<arma::vec>( a_y_p_T );
	
	// interest rate functions
	arma::vec interest_rent= arma::randu<arma::vec>( a_y_p_T );
	arma::vec interest_buy1= arma::randu<arma::vec>( a_y_p_T );
	arma::vec interest_buy2= arma::randu<arma::vec>( a_y_p_T );
	arma::vec interest_own = arma::randu<arma::vec>( a_y_p_m_h_T );

	std::vector<int> dimvec;
	dimvec.push_back( na );
	dimvec.push_back( ny );
	dimvec.push_back( np );
	dimvec.push_back( nm );
	dimvec.push_back( nh );
	dimvec.push_back( nap );
	dimvec.push_back( nT );
	int nall = na*ny*np*nm*nh*nT;

	std::vector<arma::mat *> Vals_RENT_BK0;
		Vals_RENT_BK0.push_back( &Rv_rent_L );
		Vals_RENT_BK0.push_back( &Rv_buy1_L   );
		Vals_RENT_BK0.push_back( &Rv_buy2_L   );
		Vals_RENT_BK0.push_back( &Rv_file7_L  );
		Vals_RENT_BK0.push_back( &Rv_file13_L );
		std::vector<arma::mat *> Labor_RENT_FILE;
		Labor_RENT_FILE.push_back( &RDL_file7  );
		Labor_RENT_FILE.push_back( &RDL_file13 );
		
		std::vector<arma::mat *> Vals_RENT_BK1;
		Vals_RENT_BK1.push_back( &Rv_BK7_L );
		std::vector<arma::mat *> Vals_RENT_BK2;
		Vals_RENT_BK2.push_back( &Rv_BK13_L);

		std::vector<std::vector<arma::mat *> > Vals_RENT;
		Vals_RENT.push_back( Vals_RENT_BK0 );
		Vals_RENT.push_back( Vals_RENT_BK1 );
		Vals_RENT.push_back( Vals_RENT_BK2 );
	
		// consumption function
		
		std::vector<arma::mat *> C_RENT_BK0;
		C_RENT_BK0.push_back( &Rc_rent_L );
		C_RENT_BK0.push_back( &Rc_buy1_L   );
		C_RENT_BK0.push_back( &Rc_buy2_L   );
		C_RENT_BK0.push_back( &Rc_file7_L  );
		C_RENT_BK0.push_back( &Rc_file13_L );
		
		std::vector<arma::mat *> C_RENT_BK1;
		C_RENT_BK1.push_back( &Rc_BK7_L );
		std::vector<arma::mat *> C_RENT_BK2;
		C_RENT_BK2.push_back( &Rc_BK13_L);

		std::vector<std::vector<arma::mat *> > C_RENT;
		C_RENT.push_back( C_RENT_BK0 );
		C_RENT.push_back( C_RENT_BK1 );
		C_RENT.push_back( C_RENT_BK2 );
		
		// savings function

		// S_RENT -> S_RENT_BK0
		//        -> s_rentBK7
		//        -> S_Rent_BK13
		// S_RENT_BK0  -> s_rent
		//             -> s_buy1    
		//             -> s_buy2    
		//             -> s_file7   
		//             -> s_file13  
		std::vector<arma::imat *> S_RENT_BK0;
		S_RENT_BK0.push_back( &Rs_rent_L );
		S_RENT_BK0.push_back( &Rs_buy1_L   );
		S_RENT_BK0.push_back( &Rs_buy2_L   );
		
		std::vector<arma::imat *> S_RENT_BK7;
		S_RENT_BK7.push_back( &Rs_BK7_L );
		std::vector<arma::imat *> S_RENT_BK13;
		S_RENT_BK13.push_back( &Rs_BK13_L );

		std::vector<std::vector<arma::imat *> > S_RENT;
		S_RENT.push_back(S_RENT_BK0);
		S_RENT.push_back( S_RENT_BK7);
		S_RENT.push_back( S_RENT_BK13);

		// probability of bk functions
		std::vector<arma::vec *> Pr7_RENT;		// [rent, buy1, buy2]
		Pr7_RENT.push_back( &Pr7_rent );
		Pr7_RENT.push_back( &Pr7_buy1 );
		Pr7_RENT.push_back( &Pr7_buy2 );

		std::vector<arma::vec *> Pr13_RENT;		// [rent, buy1, buy2]
		Pr13_RENT.push_back( &Pr13_rent );
		Pr13_RENT.push_back( &Pr13_buy1 );
		Pr13_RENT.push_back( &Pr13_buy2 );

		// Vals_OWN  -> Vals_OWN_BK0
		//           		-> w_stay
		//           		-> w_sell
		//           		-> w_def
		//           		-> w_file7 
		//           		-> w_file13
		//           		-> w_filedef
		//           -> Vals_OWN_BK7
		//           		-> w_stayBK7
		//           		-> w_sellBK7
		//           		-> w_defBK7
		//           -> Vals_OWN_BK13
		//           		-> w_stayBK13
		//           		-> w_sellBK13
		//           		-> w_defBK13
		std::vector<arma::mat *> Vals_OWN_BK0;
		Vals_OWN_BK0.push_back( &v_stay_L  );
		Vals_OWN_BK0.push_back( &v_sell_L );
		Vals_OWN_BK0.push_back( &v_def_L );
		Vals_OWN_BK0.push_back( &v_file7_L);
		Vals_OWN_BK0.push_back( &v_file13_L );
		Vals_OWN_BK0.push_back( &v_filedef_L );

		std::vector<arma::mat *> Vals_OWN_BK7;
		Vals_OWN_BK7.push_back( &vBK7_stay_L      );
		Vals_OWN_BK7.push_back( &vBK7_sell_L       );
		Vals_OWN_BK7.push_back( &vBK7_def_L   );

		std::vector<arma::mat *> Vals_OWN_BK13;
		Vals_OWN_BK13.push_back( &vBK13_stay_L   );
		Vals_OWN_BK13.push_back( &vBK13_sell_L    );
		Vals_OWN_BK13.push_back( &vBK13_def_L    );
		
		// All together now!
		std::vector<std::vector<arma::mat *> > Vals_OWN;
		Vals_OWN.push_back( Vals_OWN_BK0 );
		Vals_OWN.push_back( Vals_OWN_BK7 );
		Vals_OWN.push_back( Vals_OWN_BK13 );

		// consumption 
		std::vector<arma::mat *> C_OWN_BK0;
		C_OWN_BK0.push_back( &c_stay_L  );
		C_OWN_BK0.push_back( &c_sell_L );
		C_OWN_BK0.push_back( &c_def_L );
		C_OWN_BK0.push_back( &c_file7_L);
		C_OWN_BK0.push_back( &c_file13_L );
		C_OWN_BK0.push_back( &c_filedef_L );

		std::vector<arma::mat *> C_OWN_BK7;
		C_OWN_BK7.push_back( &cBK7_stay_L      );
		C_OWN_BK7.push_back( &cBK7_sell_L       );
		C_OWN_BK7.push_back( &cBK7_def_L   );

		std::vector<arma::mat *> C_OWN_BK13;
		C_OWN_BK13.push_back( &cBK13_stay_L   );
		C_OWN_BK13.push_back( &cBK13_sell_L    );
		C_OWN_BK13.push_back( &cBK13_def_L    );
		
		// All together now!
		std::vector<std::vector<arma::mat *> > C_OWN;
		C_OWN.push_back( C_OWN_BK0 );
		C_OWN.push_back( C_OWN_BK7 );
		C_OWN.push_back( C_OWN_BK13 );


		//savings functions
		std::vector<arma::imat *> S_OWN_BK0;
		S_OWN_BK0.push_back( &s_stay_L);
		S_OWN_BK0.push_back( &s_sell_L );
		S_OWN_BK0.push_back( &s_def_L  );

		std::vector<arma::imat *> S_OWN_BK7;
		S_OWN_BK7.push_back( &sBK7_stay_L   );
		S_OWN_BK7.push_back( &sBK7_sell_L    );
		S_OWN_BK7.push_back( &sBK7_def_L    );

		std::vector<arma::imat *> S_OWN_BK13;
		S_OWN_BK13.push_back( &sBK13_stay_L   );
		S_OWN_BK13.push_back( &sBK13_sell_L    );
		S_OWN_BK13.push_back( &sBK13_def_L    );
		
		// All together now!
		std::vector<std::vector<arma::imat *> > S_OWN;
		S_OWN.push_back( S_OWN_BK0 );
		S_OWN.push_back( S_OWN_BK7 );
		S_OWN.push_back( S_OWN_BK13 );
		
		// Probability of bankrupty
		std::vector<arma::vec * > PrBK_OWN;
		PrBK_OWN.push_back( &Pr7_own );
		PrBK_OWN.push_back( &Pr13_own );

		// Interest Rate Functions
		std::vector<arma::vec * > INTEREST;	// [ rent, buy1, buy2, own ]
		INTEREST.push_back( &interest_rent ); // 0
		INTEREST.push_back( &interest_buy1 ); // 1
		INTEREST.push_back( &interest_buy2 ); // 2
		INTEREST.push_back( &interest_own  ); // 3

	// create class instance
	SimClass S(dimvec,Vals_OWN,S_OWN,C_OWN,Vals_RENT,S_RENT,C_RENT,Labor_RENT_FILE,PrBK_OWN,Pr7_RENT,Pr13_RENT,INTEREST,agrid,aposgrid,&mgrid,&ygrid,&pgrid,par,Gy,Gp);

	arma::mat Res;
	//S.show();
	//std::cout << "azero " << azero << std::endl;
	//std::cout << "nap " << nap<< std::endl;
	//std::cout << sBK13_stay_L << std::endl;
	Res = S.Simulate();
	//std::cout<< Res << std::endl;


	
	std::cout << std::endl;
	std::cout<< "END OF boilerplate" << std::endl;
	std::cout<< "Goodbye." << std::endl;

	return 0;
}

