

#include "BK_sol.h" // Include the code that we are testing
#include <gtest/gtest.h> // Include the google test framework
#include "misc.h"	//linspace




class SolTest : public ::testing::Test {
	protected:

		// data members of the fixture
		int na;
		int ny;
		int np;
		int nm;
		int nh;
		int nap;
		int nL ;
		int maxage;
		int azero;

		bool randomResources;

		std::vector<int> dimvec;
		pStruct pars;
		std::vector< Array<double, 1> * > grids1D;	// agrid(a),aposgrid(apos),Lgrid(nL),Hgrid(nh)
		std::vector< Array<double, 2> * > grids2D;	// ygrid(y,age),pgrid(p,h),currdebt(m,h),initdebt(m,h)
		std::vector< Array<double, 3> * > grids3D;	// nextdebt(m,h,age),nextdebtbuyer(p,h,age),equity,secequitybuyer
		std::vector< Array<double, 4> * > grids4D; 	// secequity(a,p,m,h), repay(a,y,L,age)


		// owner resource containers
		std::vector< blitz::Array<double,6> * > tmp1;	// ResOwner(stay)(L1)
		std::vector< blitz::Array<double,6> * > tmp2;	// ResOwner(sell)(L2)
		std::vector< blitz::Array<double,6> * > tmp3;	// ResOwner(def)(L2)
		std::vector< blitz::Array<double,5> * > tmp4;	// ResOwnerFile(file7)(L2)
		std::vector< blitz::Array<double,5> * > tmp5;	// ResOwnerFile(file13)(L2)
		std::vector< blitz::Array<double,5> * > tmp6;	// ResOwnerFile(filedef)(L2)

		std::vector< blitz::Array<double,4> * > tmp7;	// ResRenter(Rent)(L1)
		std::vector< blitz::Array<double,4> * > tmp8;	// ResBuyer(buy1)(L1)
		std::vector< blitz::Array<double,4> * > tmp9;	// ResBuyer(buy2)(L1)
		std::vector< blitz::Array<double,3> * > tmp10;	// ResRenterFile(Fil7)(L1)
		std::vector< blitz::Array<double,3> * > tmp11;	// ResRenterFile(Fil13)(L1)
		std::vector< blitz::Array<double,4> * > tmp12;	// ResRenterBK7
		std::vector< blitz::Array<double,4> * > tmp13;	// ResRenterBK13

		std::vector< blitz::Array<double,6> * > tmp14;	// ResOwnerBK7(stay)(L1)
		std::vector< blitz::Array<double,6> * > tmp15;	// ResOwnerBK7(sell)(L1)
		std::vector< blitz::Array<double,6> * > tmp16;	// ResOwnerBK7(def)(L1)
		std::vector< blitz::Array<double,6> * > tmp17;	// ResOwnerBK13(stay)(L1)
		std::vector< blitz::Array<double,6> * > tmp18;	// ResOwnerBK13(sell)(L1)
		std::vector< blitz::Array<double,6> * > tmp19;	// ResOwnerBK13(def)(L1)


		std::vector< std::vector< blitz::Array<double,6> * > > ResOwner;
		std::vector< std::vector< blitz::Array<double,6> * > > ResOwnerBK7;
		std::vector< std::vector< blitz::Array<double,6> * > > ResOwnerBK13;
		std::vector< std::vector< Array<double,5> * > > ResOwnerFile;
		std::vector< std::vector< Array<double,4> * > > ResBuyer;
		std::vector< std::vector< Array<double,4> * > > ResRenter;
		std::vector< std::vector< Array<double,4> * > > ResRenterBK7;
		std::vector< std::vector< Array<double,4> * > > ResRenterBK13;
		std::vector< std::vector< Array<double,3> * > > ResRenterFile;

		ranlib::Uniform<double> uniGen;
		Range all;

		void SetUp() {
			na     = 15;
			ny     = 5;
			np     = 10;
			nm     = 4;
			nh     = 2;
			nL     = 2;	// watch out! before increasing nL, make sure you create additional corresponding resource arrays (at the moment there are just two of course!)
			maxage = 5;
			
			randomResources = false;		// random resources are more demanding a test on the discrete choice functions.

			

			all = Range::all();
			
			pars.tau         = 0.7;
			pars.beta        = 0.9;
			pars.R           = 1.04;
			pars.R_inverse   = 1/pars.R;
			pars.hex         = 0.2;
			pars.phi_s       = 0.94;
			pars.gamma       = 2;
			pars.mgamma      = 1-pars.gamma;
			pars.imgamma     = 1/pars.mgamma;
			pars.rollover    = 0.2;
			pars.theta       = 0.1;
			pars.myNA        = -99;
			pars.mu          = 1;
			pars.alpha       = -0.3;
			pars.DefCost     = 1;
			pars.FileCost    = 1;
			pars.FileDefCost = 2;
			pars.meanstest   = 0.3;
			pars.delta       = 0.15;
			pars.down       = 0.2;
			pars.omega      = 0.1;
			pars.omegaH     = 0.4;
			pars.omegaBK     = 10;
			pars.tau_c      = 0.05;
		
			grids1D.push_back( new Array<double,1>(na,F(1)) );
			
		    (*grids1D.at(0)) = linspace(-1,10,na);

		    // find aposgrid as positive portion of agrid
			azero = 0;
			int i=1;
			while (azero==0){
				if ((*grids1D.at(0))(i)>=0) {
					azero = i;
				}
				i++;
			}

			nap = na - azero + 1;

			dimvec.push_back( na );
			dimvec.push_back( ny     );
			dimvec.push_back( np     );
			dimvec.push_back( nm     );
			dimvec.push_back( nh     );
			dimvec.push_back( nap    );
			dimvec.push_back( nL     );
			dimvec.push_back( maxage );

			// now you can allocate aposgrid.

			grids1D.push_back( new Array<double,1>(nap,F(1)) );

			(*grids1D.at(1)) = (*grids1D.at(0))(Range(azero,toEnd));

			// Lgrid and Hgrid
			grids1D.push_back( new Array<double,1>(nL,F(1)) );
			grids1D.push_back( new Array<double,1>(nh,F(1)) );

			(*grids1D.at(2)) = linspace(1,nL,nL);
			(*grids1D.at(3)) = linspace(1,nh,nh);

			grids2D.push_back( new Array<double,2>(ny,maxage,F(2)) );	//ygrid
			grids2D.push_back( new Array<double,2>(np,nh,F(2)) );	// pgrid
			grids2D.push_back( new Array<double,2>(nm,nh,F(2)));	// currdebt
			grids2D.push_back( new Array<double,2>(np,nh,F(2)));	// initdebt
			grids2D.push_back( new Array<double,2>(ny,ny,F(2)));	// y transition
			grids2D.push_back( new Array<double,2>(np,np,F(2)));	// p transition

			//ygrid
			for (int iy=1;iy<ny+1;iy++){
				for (int it=1;it<maxage+1;it++){
					(*grids2D.at(0))(iy,it) = iy + 0.3*it;
				}
			}

		    for (int ip=1;ip<np+1;ip++){
			for (int ih=1;ih<nh+1;ih++){
				(*grids2D.at(1))(ip,ih) = (ip + ih);	// pgrid 
				(*grids2D.at(3))(ip,ih) = (*grids2D.at(1))(ip,ih)*(1-pars.down);	// initdebt
			}}

			for (int ih=1;ih<nh+1;ih++){	// currdebt
			(*grids2D.at(2))(Range::all(),ih) = linspace(0,(*grids2D.at(3))(np,ih)*(1-pars.down),nm) ;
			}

			// G
			for (int iy=1;iy<ny+1;iy++){
			for (int jy=1;jy<ny+1;jy++){
				(*grids2D.at(4))(iy,jy) = 1.0/ny;
			}
			}

			//Gp
			double tmp;
			for (int ip=1;ip<np+1;ip++){
			for (int jp=1;jp<np+1;jp++){
				//(*grids2D.at(5))(ip,jp) = 1.0/np;
				(*grids2D.at(5))(ip,jp) = (ip + jp)/ 2.0;
			}
			 
			tmp = 0;
			for (int jp=1;jp<np+1;jp++) tmp += (*grids2D.at(5))(ip,jp);	// sum over row
			(*grids2D.at(5))(ip,all) = (*grids2D.at(5))(ip,all) / tmp;	// normalize by row

			}

			grids3D.push_back( new Array<double,3>(nm,nh,maxage,F(3)) );	//nextdebt
			grids3D.push_back( new Array<double,3>(np,nh,maxage,F(3)) );	//nextdebtbuyer
			grids3D.push_back( new Array<double,3>(np,nh,nm,F(3)) );    	//equity
			grids3D.push_back( new Array<double,3>(na,np,nh,F(3)) );    	//secequitybuyer

			// nextdebt
		    for (int im=1;im<nm+1;im++){
		    for (int ih=1;ih<nh+1;ih++){
			for (int it=1;it<maxage+1;it++){
						(*grids3D.at(0))(im,ih,it) = (*grids2D.at(2))(im,ih) * 0.9 ;	// should be a function of age, really.
					}
				}
			}

		   // nextdebtbuyer
			for (int ip=1;ip<np+1;ip++){
			for (int ih=1;ih<nh+1;ih++){
			for (int it=1;it<maxage+1;it++){
						(*grids3D.at(1))(ip,ih,it) = (*grids2D.at(3))(ip,ih) * 0.79 ;
					}
				}
			}

			// negequity
			for (int ip=1;ip<np+1;ip++){
			for (int ih=1;ih<nh+1;ih++){
			for (int im=1;im<nm+1;im++){
						(*grids3D.at(2))(ip,ih,im) = (*grids2D.at(1))(ip,ih) * pars.phi_s - (*grids2D.at(2))(im,ih)  ;
					}
				}
			}
			
			// secequity of a buyer at state (ia,ip,ih)
			(*grids3D.at(3)) = 0;
			for (int ih=1;ih<nh+1;ih++){
			for (int ip=1;ip<np+1;ip++){
			for (int ia=1;ia<na+1;ia++){
				if ((*grids1D.at(0))(ia)<0){							// price * phi - init.debt
							(*grids3D.at(3))(ia,ip,ih) = std::min( std::max( (*grids2D.at(1))(ip,ih) * pars.phi_s - (*grids2D.at(3))(ip,ih) - pars.hex, 0.0 ), -(*grids1D.at(0))(ia) );
						}
					}
				}
			}

			grids4D.push_back( new Array<double,4>(na,np,nm,nh,F(4)) );	//secequity
			grids4D.push_back( new Array<double,4>(na,ny,nL,maxage,F(4)) ); //repay

			// secequity of a owner at state (ia,ip,im,ih)
			(*grids4D.at(0)) = 0;
			for (int im=1;im<nm+1;++im){
			for (int ih=1;ih<nh+1;ih++){
			for (int ip=1;ip<np+1;ip++){
			for (int ia=1;ia<na+1;ia++){
				if ((*grids1D.at(0))(ia)<0){
					(*grids4D.at(0))(ia,ip,im,ih) = std::min( std::max( (*grids2D.at(1))(ip,ih) * pars.phi_s - (*grids2D.at(2))(im,ih) - pars.hex, 0.0 ), -(*grids1D.at(0))(ia) );
				}
			}}}}

			// repay array
			// this is (na,ny,nL,nt): repayment of debt a
			// depends on wages at age t, and labor L supplied
			(*grids4D.at(1)) = 0;
			for (int ia=1;ia<na+1;ia++){
			for (int iy=1;iy<ny+1;iy++){
			for (int iL=1;iL<nL+1;iL++){
			for (int it=1;it<maxage+1;it++){
				if ((*grids1D.at(0))(ia)<0){
					(*grids4D.at(1))(ia,iy,iL,it) = std::min( (*grids1D.at(2))(iL) * (*grids2D.at(0))(iy,it), -(*grids1D.at(0))(ia) );
				}
			}}}}
			

			// create resource arrays
			tmp1.push_back( new blitz::Array<double,6>(na,ny,np,nm,nh,maxage,F(6)) );	// ResOwner(stay)(L1)
			tmp1.push_back( new blitz::Array<double,6>(na,ny,np,nm,nh,maxage,F(6)) );	// ResOwner(stay)(L2)
			tmp2.push_back( new blitz::Array<double,6>(na,ny,np,nm,nh,maxage,F(6)) );	// ResOwner(sell)(L1)
			tmp2.push_back( new blitz::Array<double,6>(na,ny,np,nm,nh,maxage,F(6)) );	// ResOwner(sell)(L2)
			tmp3.push_back( new blitz::Array<double,6>(na,ny,np,nm,nh,maxage,F(6)) );	// ResOwner(def)(L1)
			tmp3.push_back( new blitz::Array<double,6>(na,ny,np,nm,nh,maxage,F(6)) );	// ResOwner(def)(L2)

			tmp4.push_back( new blitz::Array<double,5>(   ny,np,nm,nh,maxage,F(5)) );	// ResOwnerFile(file7)(L1)
			tmp4.push_back( new blitz::Array<double,5>(   ny,np,nm,nh,maxage,F(5)) );	// ResOwnerFile(file7)(L2)
			tmp5.push_back( new blitz::Array<double,5>(   ny,np,nm,nh,maxage,F(5)) );	// ResOwnerFile(file13)(L1)
			tmp5.push_back( new blitz::Array<double,5>(   ny,np,nm,nh,maxage,F(5)) );	// ResOwnerFile(file13)(L2)
			tmp6.push_back( new blitz::Array<double,5>(   ny,np,nm,nh,maxage,F(5)) );	// ResOwnerFile(filedef)(L1)
			tmp6.push_back( new blitz::Array<double,5>(   ny,np,nm,nh,maxage,F(5)) );	// ResOwnerFile(filedef)(L2)

			tmp7.push_back( new blitz::Array<double,4>(na,ny,np,maxage,F(4)) );   // ResRenter(Rent)(L1)
			tmp7.push_back( new blitz::Array<double,4>(na,ny,np,maxage,F(4)) );   // ResRenter(Rent)(L2)
			tmp8.push_back( new blitz::Array<double,4>(na,ny,np,maxage,F(4)) );   // ResBuyer(buy1)(L1)
			tmp8.push_back( new blitz::Array<double,4>(na,ny,np,maxage,F(4)) );   // ResBuyer(buy1)(L2)
			tmp9.push_back( new blitz::Array<double,4>(na,ny,np,maxage,F(4)) );   // ResBuyer(buy2)(L1)
			tmp9.push_back( new blitz::Array<double,4>(na,ny,np,maxage,F(4)) );   // ResBuyer(buy2)(L2)

			tmp10.push_back( new blitz::Array<double,3>(   ny,np,maxage,F(3)) );  // ResRenterFile(Fil7)(L1)
			tmp10.push_back( new blitz::Array<double,3>(   ny,np,maxage,F(3)) );  // ResRenterFile(Fil7)(L2)
			tmp11.push_back( new blitz::Array<double,3>(   ny,np,maxage,F(3)) );  // ResRenterFile(Fil13)(L1)
			tmp11.push_back( new blitz::Array<double,3>(   ny,np,maxage,F(3)) );  // ResRenterFile(Fil13)(L2)

			tmp12.push_back( new blitz::Array<double,4>(nap,ny,np,maxage,F(4)) ); // ResRenterBK7(L1)
			tmp12.push_back( new blitz::Array<double,4>(nap,ny,np,maxage,F(4)) ); // ResRenterBK7(L2)
			tmp13.push_back( new blitz::Array<double,4>(nap,ny,np,maxage,F(4)) ); // ResRenterBK13(L1)
			tmp13.push_back( new blitz::Array<double,4>(nap,ny,np,maxage,F(4)) ); // ResRenterBK13(L2)

			tmp14.push_back( new blitz::Array<double,6>(nap,ny,np,nm,nh,maxage,F(6)) );	// ResOwnerBK7(stay)(L1)
			tmp14.push_back( new blitz::Array<double,6>(nap,ny,np,nm,nh,maxage,F(6)) );	// ResOwnerBK7(stay)(L2)
			tmp15.push_back( new blitz::Array<double,6>(nap,ny,np,nm,nh,maxage,F(6)) );	// ResOwnerBK7(sell)(L1)
			tmp15.push_back( new blitz::Array<double,6>(nap,ny,np,nm,nh,maxage,F(6)) );	// ResOwnerBK7(sell)(L2)
			tmp16.push_back( new blitz::Array<double,6>(nap,ny,np,nm,nh,maxage,F(6)) );	// ResOwnerBK7(def)(L1)
			tmp16.push_back( new blitz::Array<double,6>(nap,ny,np,nm,nh,maxage,F(6)) );	// ResOwnerBK7(def)(L2)
                                                           
			tmp17.push_back( new blitz::Array<double,6>(nap,ny,np,nm,nh,maxage,F(6)) );	// ResOwnerBK13(stay)(L1)
			tmp17.push_back( new blitz::Array<double,6>(nap,ny,np,nm,nh,maxage,F(6)) );	// ResOwnerBK13(stay)(L2)
			tmp18.push_back( new blitz::Array<double,6>(nap,ny,np,nm,nh,maxage,F(6)) );	// ResOwnerBK13(sell)(L1)
			tmp18.push_back( new blitz::Array<double,6>(nap,ny,np,nm,nh,maxage,F(6)) );	// ResOwnerBK13(sell)(L2)
			tmp19.push_back( new blitz::Array<double,6>(nap,ny,np,nm,nh,maxage,F(6)) );	// ResOwnerBK13(def)(L1)
			tmp19.push_back( new blitz::Array<double,6>(nap,ny,np,nm,nh,maxage,F(6)) );	// ResOwnerBK13(def)(L2)
			
			
			// add others here when testing them...
     

			if (randomResources){

				// fill with random numbers
				for (int itmp=0;itmp<nL;itmp++) { 
					for (Array<double,6>::iterator it=tmp1.at(itmp)->begin();it!=tmp1.at(itmp)->end();it ++) { *it = uniGen.random(); } 
					for (Array<double,6>::iterator it=tmp2.at(itmp)->begin();it!=tmp2.at(itmp)->end();it ++) { *it = uniGen.random(); } 
					for (Array<double,6>::iterator it=tmp3.at(itmp)->begin();it!=tmp3.at(itmp)->end();it ++) { *it = uniGen.random(); } 
					for (Array<double,5>::iterator it=tmp4.at(itmp)->begin();it!=tmp4.at(itmp)->end();it ++) { *it = uniGen.random(); } 
					for (Array<double,5>::iterator it=tmp5.at(itmp)->begin();it!=tmp5.at(itmp)->end();it ++) { *it = uniGen.random(); } 
					for (Array<double,5>::iterator it=tmp6.at(itmp)->begin();it!=tmp6.at(itmp)->end();it ++) { *it = uniGen.random(); } 

					for (Array<double,4>::iterator it=tmp7.at(itmp)->begin();it!=tmp7.at(itmp)->end();it ++) { *it = uniGen.random(); } 
					for (Array<double,4>::iterator it=tmp8.at(itmp)->begin();it!=tmp8.at(itmp)->end();it ++) { *it = uniGen.random(); } 
					for (Array<double,4>::iterator it=tmp9 .at(itmp)->begin();it!=tmp9 .at(itmp)->end();it ++) { *it = uniGen.random(); } 
					
					for (Array<double,3>::iterator it=tmp10.at(itmp)->begin();it!=tmp10.at(itmp)->end();it ++) { *it = uniGen.random(); } 
					for (Array<double,3>::iterator it=tmp11.at(itmp)->begin();it!=tmp11.at(itmp)->end();it ++) { *it = uniGen.random(); } 
					
					for (Array<double,4>::iterator it=tmp12.at(itmp)->begin();it!=tmp12.at(itmp)->end();it ++) { *it = uniGen.random(); } 
					for (Array<double,4>::iterator it=tmp13.at(itmp)->begin();it!=tmp13.at(itmp)->end();it ++) { *it = uniGen.random(); } 

					for (Array<double,6>::iterator it=tmp14.at(itmp)->begin();it!=tmp14.at(itmp)->end();it ++) { *it = uniGen.random(); } 
					for (Array<double,6>::iterator it=tmp15.at(itmp)->begin();it!=tmp15.at(itmp)->end();it ++) { *it = uniGen.random(); } 
					for (Array<double,6>::iterator it=tmp16.at(itmp)->begin();it!=tmp16.at(itmp)->end();it ++) { *it = uniGen.random(); } 
					for (Array<double,6>::iterator it=tmp17.at(itmp)->begin();it!=tmp17.at(itmp)->end();it ++) { *it = uniGen.random(); } 
					for (Array<double,6>::iterator it=tmp18.at(itmp)->begin();it!=tmp18.at(itmp)->end();it ++) { *it = uniGen.random(); } 
					for (Array<double,6>::iterator it=tmp19.at(itmp)->begin();it!=tmp19.at(itmp)->end();it ++) { *it = uniGen.random(); } 
				}

			} else {

				// fill with something that is increasing in a and y always.

				for (int itmp=0;itmp<nL;itmp++) { // labor loop
				for (int iy = 1; iy<ny+1 ; iy++){
				for (int ip = 1; ip<np+1 ; ip++){
				for (int im = 1; im<nm+1 ; im++){
				for (int ih = 1; ih<nh+1 ; ih++){
				for (int it = 1; it<maxage+1 ; it++){
				for (int ia = 1; ia<na+1 ; ia++){


														// a + y  * labor                    + p - m + h + age
					(*tmp1.at(itmp))(ia,iy,ip,im,ih,it) = ia + iy * (*grids1D.at(3))(itmp+1) + ip - im + ih + it ;// ResOwner(stay)(L1)
					(*tmp2.at(itmp))(ia,iy,ip,im,ih,it) = ia + iy * (*grids1D.at(3))(itmp+1) + ip - im + ih + it ;// ResOwner(sell)(L2)
					(*tmp3.at(itmp))(ia,iy,ip,im,ih,it) = ia + iy * (*grids1D.at(3))(itmp+1) + ip - im + ih + it ;// ResOwner(def)(L2)
                                                                                                                  
					(*tmp4.at(itmp))(   iy,ip,im,ih,it) =   iy * (*grids1D.at(3))(itmp+1) + ip - im + ih + it ;   // ResOwnerFile(file7)(L2)
					(*tmp5.at(itmp))(   iy,ip,im,ih,it) =   iy * (*grids1D.at(3))(itmp+1) + ip - im + ih + it ;   // ResOwnerFile(file13)(L2)
					(*tmp6.at(itmp))(   iy,ip,im,ih,it) =   iy * (*grids1D.at(3))(itmp+1) + ip - im + ih + it ;   // ResOwnerFile(filedef)(L2)

					(*tmp7.at(itmp))(ia,iy,ip,      it) = ia + iy * (*grids1D.at(3))(itmp+1) + ip   + ih    + it ;// ResRenter(Rent)(L1)
					(*tmp8.at(itmp))(ia,iy,ip,      it) = ia + iy * (*grids1D.at(3))(itmp+1) + ip   + ih    + it ;// ResBuyer(buy1)(L1)
					(*tmp9.at(itmp))(ia,iy,ip,      it) = ia + iy * (*grids1D.at(3))(itmp+1) + ip           + it ;// ResBuyer(buy2)(L1)
					(*tmp10.at(itmp))(  iy,ip,      it) =      iy * (*grids1D.at(3))(itmp+1) + ip           + it ;// ResRenterFile(Fil7)(L1)
					(*tmp11.at(itmp))(  iy,ip,      it) =      iy * (*grids1D.at(3))(itmp+1) + ip           + it ;// ResRenterFile(Fil13)(L1)

					if (ia >=azero ){

						(*tmp12.at(itmp))(ia,iy,ip,      it) =  ia + iy * (*grids1D.at(3))(itmp+1) + ip + it ;// ResRenterBK7
						(*tmp13.at(itmp))(ia,iy,ip,      it) =  ia + iy * (*grids1D.at(3))(itmp+1) + ip + it ;// ResRenterBK13

						(*tmp14.at(itmp))(ia,iy,ip,im,ih,it) = ia + iy * (*grids1D.at(3))(itmp+1) + ip - im + ih + it ;// ResOwnerBK7(stay)(L1)
						(*tmp15.at(itmp))(ia,iy,ip,im,ih,it) = ia + iy * (*grids1D.at(3))(itmp+1) + ip - im + ih + it ;// ResOwnerBK7(sell)(L1)
						(*tmp16.at(itmp))(ia,iy,ip,im,ih,it) = ia + iy * (*grids1D.at(3))(itmp+1) + ip - im + ih + it ;// ResOwnerBK7(def)(L1)
						(*tmp17.at(itmp))(ia,iy,ip,im,ih,it) = ia + iy * (*grids1D.at(3))(itmp+1) + ip - im + ih + it ;// ResOwnerBK13(stay)(L1)
						(*tmp18.at(itmp))(ia,iy,ip,im,ih,it) = ia + iy * (*grids1D.at(3))(itmp+1) + ip - im + ih + it ;// ResOwnerBK13(sell)(L1)
						(*tmp19.at(itmp))(ia,iy,ip,im,ih,it) = ia + iy * (*grids1D.at(3))(itmp+1) + ip - im + ih + it ;// ResOwnerBK13(def)(L1)


					}

				}}}}}}}

			}


			ResOwner.push_back( tmp1 );
			ResOwner.push_back( tmp2 );
			ResOwner.push_back( tmp3 );
			ResOwnerFile.push_back( tmp4 );
			ResOwnerFile.push_back( tmp5 );
			ResOwnerFile.push_back( tmp6 );

			ResOwnerBK7.push_back( tmp14 );
			ResOwnerBK7.push_back( tmp15 );
			ResOwnerBK7.push_back( tmp16 );
			ResOwnerBK13.push_back( tmp17 );
			ResOwnerBK13.push_back( tmp18 );
			ResOwnerBK13.push_back( tmp19 );

			ResRenter.push_back( tmp7 );
			ResBuyer.push_back( tmp8 );
			ResBuyer.push_back( tmp9 );
			ResRenterFile.push_back( tmp10 );
			ResRenterFile.push_back( tmp11 );
			ResRenterBK7.push_back( tmp12 );
			ResRenterBK13.push_back( tmp13 );

			// for other test cases, i.e. where renter values are need
			// must creat more resource arrays here.

			// create a class on the heap
			Sol = new Solution(pars,
					   dimvec,
					   grids1D,
					   grids2D,
					   grids3D,
					   grids4D,
					   ResOwner,
					   ResOwnerBK7,
					   ResOwnerBK13,
					   ResOwnerFile,
					   ResRenter,
					   ResBuyer,
					   ResRenterBK7,
					   ResRenterBK13,
					   ResRenterFile );
			}

		virtual void TearDown(){ 
			for (std::vector<Array<double,1> * >::iterator it1=grids1D.begin();it1!=grids1D.end();it1++) { delete *it1; };
			for (std::vector<Array<double,2> * >::iterator it2=grids2D.begin();it2!=grids2D.end();it2++) { delete *it2; };
			for (std::vector<Array<double,3> * >::iterator it3=grids3D.begin();it3!=grids3D.end();it3++) { delete *it3; };
			for (std::vector<Array<double,4> * >::iterator it4=grids4D.begin();it4!=grids4D.end();it4++) { delete *it4; };
			for (std::vector< blitz::Array<double,6> * >::iterator it=tmp1.begin();it!=tmp1.end();it++) { delete *it; };
			for (std::vector< blitz::Array<double,6> * >::iterator it=tmp2.begin();it!=tmp2.end();it++) { delete *it; };
			for (std::vector< blitz::Array<double,6> * >::iterator it=tmp3.begin();it!=tmp3.end();it++) { delete *it; };
			for (std::vector< blitz::Array<double,5> * >::iterator it=tmp4.begin();it!=tmp4.end();it++) { delete *it; };
			for (std::vector< blitz::Array<double,5> * >::iterator it=tmp5.begin();it!=tmp5.end();it++) { delete *it; };
			for (std::vector< blitz::Array<double,5> * >::iterator it=tmp6.begin();it!=tmp6.end();it++) { delete *it; };


			for (std::vector< blitz::Array<double,4> * >::iterator it=tmp7.begin();it!=tmp7.end();it++) { delete *it; };
			for (std::vector< blitz::Array<double,4> * >::iterator it=tmp8.begin();it!=tmp8.end();it++) { delete *it; };
			for (std::vector< blitz::Array<double,4> * >::iterator it=tmp9.begin();it!=tmp9.end();it++) { delete *it; };

			for (std::vector< blitz::Array<double,3> * >::iterator it=tmp10.begin();it!=tmp10.end();it++) { delete *it; };
			for (std::vector< blitz::Array<double,3> * >::iterator it=tmp11.begin();it!=tmp11.end();it++) { delete *it; };

			for (std::vector< blitz::Array<double,4> * >::iterator it=tmp12.begin();it!=tmp12.end();it++) { delete *it; };
			for (std::vector< blitz::Array<double,4> * >::iterator it=tmp13.begin();it!=tmp13.end();it++) { delete *it; };

			for (std::vector< blitz::Array<double,6> * >::iterator it=tmp14.begin();it!=tmp14.end();it++) { delete *it; };
			for (std::vector< blitz::Array<double,6> * >::iterator it=tmp15.begin();it!=tmp15.end();it++) { delete *it; };
			for (std::vector< blitz::Array<double,6> * >::iterator it=tmp16.begin();it!=tmp16.end();it++) { delete *it; };
			for (std::vector< blitz::Array<double,6> * >::iterator it=tmp17.begin();it!=tmp17.end();it++) { delete *it; };
			for (std::vector< blitz::Array<double,6> * >::iterator it=tmp18.begin();it!=tmp18.end();it++) { delete *it; };
			for (std::vector< blitz::Array<double,6> * >::iterator it=tmp19.begin();it!=tmp19.end();it++) { delete *it; };
			
			delete Sol;
		}

		Solution *Sol;
};




// test if can construct an instance
TEST_F(SolTest, CanGetName){

	EXPECT_EQ( "SolutionClass", Sol->GetName() );
}

// =====================================
// Compute OwnNetBorrow, OwnNetBorrow and RentNetBorrw
// =====================================


TEST_F(SolTest, CanComputeNetBorrowFunctions){

	// get Ownnetborrow before 
	Array<double,6> ob(na,ny,np,nm,nh,maxage,F(6));
	ob = Sol->Get_netb_own();
	// expect netb_own to be equal to value set 
	// in constructor
	for (Array<double,6>::iterator iter=ob.begin();iter!=ob.end();iter++){
		EXPECT_EQ( 14, *iter );
	}
	
	// get BuyNetborrow before 
	Array<double,5> bb(na,ny,np,nh,maxage,F(5));
	bb = Sol->Get_netb_buy();
	for (Array<double,5>::iterator iter=bb.begin();iter!=bb.end();iter++){
		EXPECT_EQ( 11.5, *iter );
	}

	// get RentNetBorrow before 
	Array<double,4> rb(na,ny,np,maxage,F(4));
	rb = Sol->Get_netb_rent();
	// expect netb_own to be equal to value set 
	// in constructor
	for (Array<double,4>::iterator it4=rb.begin();it4!=rb.end();it4++){
		EXPECT_EQ( 18, *it4 );
	}

	// call NetBorrow functions
	int age = 3;
	Sol->ComputeOwnNetBorrow(  age );
	Sol->ComputeRentNetBorrow(  age );
	Sol->ComputeBuyNetBorrow(  age );
	// get again
	ob = Sol->Get_netb_own();
	rb = Sol->Get_netb_rent();
	bb = Sol->Get_netb_buy();

	// expect OwnNetBorrow to be 
	// (1 - 0.1 - 0.1) * agrid - 0.1 * secequity - 0.1 * 1;
	//
	// expect BuyNetBorrow to be 
	// (1 - 0.1 - 0.1) * agrid - 0.1 * secequitybuyer - 0.1 * 2;
	// (but on smaller dimension: no mortgage)
	//
	// expect RentNetBorrow to be 
	// (1 - 0.1 - 0.1) * agrid -                   0.1 * 1;
	Array<double,1> anew(na,F(1));
	anew = 0.8 * (*grids1D.at(0));

	for (int ia=1; ia<na+1; ia++){
		for (int iy=1;iy<ny+1; iy++){
			for (int ip=1;ip<np+1; ip++){
				for (int im=1; im<nm+1; im++){
					for (int ih=1; ih<nh+1; ih++){
						if ((*grids1D.at(0))(ia)<0){
							EXPECT_NEAR( (anew(ia) - 0.1 - 0.1*(*grids4D.at(0))(ia,ip,im,ih))*pars.R_inverse, ob(ia,iy,ip,im,ih,age), 0.0001 );
							EXPECT_NEAR( (anew(ia) - 0.2 - 0.1*(*grids3D.at(3))(ia,ip,   ih))*pars.R_inverse, bb(ia,iy,ip,   ih,age), 0.0001 );
							EXPECT_NEAR( (anew(ia) - 0.1                                    )*pars.R_inverse, rb(ia,iy,ip,      age), 0.0001 );
						}
					}
				}
			}
		}
	}
}







//===================
// test maxmin_equity
//===================

TEST_F(SolTest, MaxMinEquityIsCorrect){

	// Test whether maxmin_equity gives correct result.
	double eq = -2.0;
	double eqr = eq * pars.rollover;

	Array<double,1> newa(na,F(1));
	newa = (*grids1D.at(0)) + eqr;
	for (int i=1;i<na+1;i++) newa(i) = max((*grids1D.at(0))(1),newa(i));

	//test against result from class:
	for (int i=1;i<na+1;i++) {
		EXPECT_DOUBLE_EQ( newa(i), Sol->maxmin_equity( eq )(i) );
	}
	std::cout << "original asset grid = " << std::endl;
	std::cout << (*grids1D.at(0))  << std::endl;
	std::cout << "neg.equity adjusted grid = " << std::endl;
	std::cout << Sol->maxmin_equity(eq)  << std::endl;
}

//===================
// test Set_vtmp_dchoice_file
//===================

TEST_F(SolTest, CheckSet_vtmp_dchoice_file ){

	int age = 3;
	Sol->Set_v_rand()   ;
	Array<double,5> v_file7(Sol->Get_v_file7());
	Array<double,5> v_file13(Sol->Get_v_file13());
	Array<double,5> v_filedef(Sol->Get_v_filedef());
	Array<double,6> vtmp_dchoice(na,ny,np,nm,nh,6,F(6));
	Array<double,6> sol_vtmp_dchoice(na,ny,np,nm,nh,6,F(6));
	vtmp_dchoice = pars.myNA;

	v_file7 = Sol->Get_v_file7();
	v_file13 = Sol->Get_v_file13();
	v_filedef = Sol->Get_v_filedef();

	int azero;
	azero = na-nap+1;	// na-nap+1. the +1 because of fortran indexing
	
	for (int ia=1; ia < azero ; ia++){

		// enforce equity restriction on file7
		for (int ip=1;ip<np+1;ip++){
			for (int ih=1; ih<nh+1;ih++){
				for (int im=1;im<nm+1;im++){

					// file7 is admissible if equity is less than
					// homestead exemption.
					if ((*grids3D.at(2))(ip,ih,im) < pars.hex) {
						vtmp_dchoice(ia,all,ip,im,ih,4) = v_file7(all,ip,im,ih,age);
					}
					if ((*grids3D.at(2))(ip,ih,im) < 0) {
						vtmp_dchoice(ia,all,ip,im,ih,6) = v_filedef(all,ip,im,ih,age);
					}
				}
			}
		}

		// no restrictions on other choices
		vtmp_dchoice(ia,all,all,all,all,5) = v_file13(all,all,all,all,age);
	}

	// check if values are the same on the class
	Sol->Set_vtmp_dchoice_file(age);

	sol_vtmp_dchoice = Sol->Get_vtmp_dchoice();
	for (int ia=1;ia<na+1;ia++){
		for (int iy=1;iy<ny+1;iy++){
			for (int ip=1;ip<np+1; ip++){
				for (int im=1;im<nm+1; im++){
					for (int ih=1;ih<nh+1; ih++){
						for (int i=1;i<7;i++){
							EXPECT_EQ( sol_vtmp_dchoice(ia,iy,ip,im,ih,i),vtmp_dchoice(ia,iy,ip,im,ih,i));
						}
					}
				}
			}
		}
	}
}



//===================
// test Set_vtmp_dchoice_negequity
//===================

TEST_F(SolTest, CheckSet_vtmp_dchoice_negequity ){

	int age = 2;

	double x;
	x = uniGen.random();

	Sol->Set_v_def( x )    ;
	Sol->Set_vtmp_dchoice_negequity( age );



	Array<double,6> vtmp_dchoice(Sol->Get_vtmp_dchoice());
	vtmp_dchoice = Sol->Get_vtmp_dchoice();


	for (int ip=1;ip<np+1;ip++){
		for (int ih=1; ih<nh+1;ih++){
			for (int im=1;im<nm+1;im++){
				for (int ia=1;ia<na+1;ia++){
					for (int iy=1;iy<ny+1;iy++){

						if ((*grids3D.at(2))(ip,ih,im) < 0){	// if there is negative equity on that state

							EXPECT_EQ( vtmp_dchoice(ia,iy,ip,im,ih,3), x) ;	// the value should be equal to the one i set on v_def

						} else {

							EXPECT_EQ(vtmp_dchoice(ia,iy,ip,im,ih,3), pars.myNA) ;	// the value should be equal to NA
						}
					}
				}
			}
		}
	}
}

//===================
// test Set_vtmpBK7_dchoice_negequity
//===================

TEST_F(SolTest, CheckSet_vtmpBK7_dchoice_negequity ){

	int age = 2;

	double x;
	x = uniGen.random();

	Sol->Set_vBK7_def( x )    ;
	Sol->Set_vtmpBK7_dchoice_negequity( age );



	Array<double,6> vtmp_dchoice(Sol->Get_vtmpBK7_dchoice());
	//vtmp_dchoice = Sol->Get_vtmpBK7_dchoice();


	for (int ip=1;ip<np+1;ip++){
		for (int ih=1; ih<nh+1;ih++){
			for (int im=1;im<nm+1;im++){
				for (int ia=1;ia<nap+1;ia++){
					for (int iy=1;iy<ny+1;iy++){

						if ((*grids3D.at(2))(ip,ih,im) < 0){	// if there is negative equity on that state

							EXPECT_EQ( vtmp_dchoice(ia,iy,ip,im,ih,3), x) ;	// the value should be equal to the one i set on v_def

						} else {

							EXPECT_EQ(vtmp_dchoice(ia,iy,ip,im,ih,3), pars.myNA) ;	// the value should be equal to NA
						}
					}
				}
			}
		}
	}
}

//===================
// test Set_vtmpBK13_dchoice_negequity
//===================

TEST_F(SolTest, CheckSet_vtmpBK13_dchoice_negequity ){

	int age = 2;

	double x;
	x = uniGen.random();

	Sol->Set_vBK13_def( x )    ;
	Sol->Set_vtmpBK13_dchoice_negequity( age );



	Array<double,6> vtmp_dchoice(Sol->Get_vtmpBK13_dchoice());
	//vtmp_dchoice = Sol->Get_vtmpBK13_dchoice();


	for (int ip=1;ip<np+1;ip++){
		for (int ih=1; ih<nh+1;ih++){
			for (int im=1;im<nm+1;im++){
				for (int ia=1;ia<nap+1;ia++){
					for (int iy=1;iy<ny+1;iy++){

						if ((*grids3D.at(2))(ip,ih,im) < 0){	// if there is negative equity on that state

							EXPECT_EQ( vtmp_dchoice(ia,iy,ip,im,ih,3), x) ;	// the value should be equal to the one i set on v_def

						} else {

							EXPECT_EQ(vtmp_dchoice(ia,iy,ip,im,ih,3), pars.myNA) ;	// the value should be equal to NA
						}
					}
				}
			}
		}
	}
}


// Owner Discrete Choice Fun
// =========================

// test: 
// * set conditional value functions
//   inside the class equal to random values
// * compute discrete choice on those fucntions in the class
// * replicate the discrete choice outside the class,
//   taking care to enforce all embedded restrictions
TEST_F(SolTest, ComputeOwnDiscreteChoice){


	int age = 1;

	int azero;
	azero = na-nap+1;	// na-nap+1. the +1 because of fortran indexing

	// set random values on class
	Sol->Set_v_rand()   ;

	// then get those values back
	Array<double,6> v_stay(Sol->Get_v_stay());
	Array<double,6> v_sell(Sol->Get_v_sell());
	Array<double,6> v_def (Sol->Get_v_def());
	Array<double,6> v_file7_L(Sol->Get_v_file7_L());	// must use L-conditional for file7
	Array<int   ,5> L_file7(Sol->Get_L_file7());
	Array<double,5> v_file13(Sol->Get_v_file13());
	Array<double,5> v_filedef(Sol->Get_v_filedef());

	// recompute v_file7
	Sol->ComputeOwnFile7(age);

	// now v_file7_L has the income restriction embedded!

	v_file7_L = Sol->Get_v_file7_L();
	L_file7 = Sol->Get_L_file7();
	
	
	// compute owner dchoice in class
	// this takes into account the equity restriction
	Sol->ComputeOwnerDchoice( age ) ;

	// check if dchoice and vown correspond
	// my discrete choice
	//Array<double,5> vmax(na,ny,np,nm,nh,F(5));
	//Array<int   ,5> dmax(na,ny,np,nm,nh,F(5));
	double vmax;
	std::vector<double>::iterator v_iter;
	int dmax;


	Array<double,6> sol_v(na,ny,np,nm,nh,maxage,F(6));
	Array<int   ,6> sol_d(na,ny,np,nm,nh,maxage,F(6));
	
	sol_v = Sol->GetVown();
	sol_d = Sol->GetOwnDchoice();

	// vector to stack discrete choice options into
	std::vector<double> v;
	// always make sure this has 6 members.
	// push NA up the vector if non-admissible chocie
							
	for (int ip=1;ip<np+1;ip++){
		for (int ih=1; ih<nh+1;ih++){
			for (int im=1;im<nm+1;im++){
				for (int iy=1;iy<ny+1;iy++){
					for (int ia=1;ia<na+1;ia++){

						if (ia < azero){

							if ((*grids3D.at(2))(ip,ih,im) < 0){	// if there is negative equity
							
								std::cout << "case a < 0 and eq < 0" << std::endl;

								v.push_back(v_stay   (ia,iy,ip,im,ih,age));
								v.push_back(v_sell   (ia,iy,ip,im,ih,age));
								v.push_back(v_def    (ia,iy,ip,im,ih,age));

								v.push_back(v_file7_L(   iy,ip,im,ih,L_file7(iy,ip,im,ih,age),age));	// this makes sure that i use the appropriate labor supply value: 
								                                                                        // if income>meanstest, not possible to work.
								                                                                        // in case there is no admissible labor supply level,
																										// that's ok because i just push NA onto the v vector.
								v.push_back(v_file13 (   iy,ip,im,ih,age));
								v.push_back(v_filedef(   iy,ip,im,ih,age));

								v_iter= std::max_element(v.begin(),v.end());
								dmax = std::distance(v.begin(),v_iter) + 1;
								vmax = *v_iter;

								EXPECT_EQ( sol_v(ia,iy,ip,im,ih,age), vmax );
								EXPECT_EQ( sol_d(ia,iy,ip,im,ih,age), dmax );

								v.clear();

							} else {
								//pos equity
								if ((*grids3D.at(2))(ip,ih,im) >= pars.hex ) {
								std::cout << "case a < 0 and eq > hex " << std::endl;

									v.push_back(v_stay   (ia,iy,ip,im,ih,age));
									v.push_back(v_sell   (ia,iy,ip,im,ih,age));
									v.push_back(pars.myNA);
									v.push_back(pars.myNA);
									v.push_back(v_file13 (   iy,ip,im,ih,age));
									v.push_back(pars.myNA);

									v_iter= std::max_element(v.begin(),v.end());
									dmax = std::distance(v.begin(),v_iter) + 1;
									vmax = *v_iter;

									EXPECT_EQ( sol_v(ia,iy,ip,im,ih,age), vmax );
									EXPECT_EQ( sol_d(ia,iy,ip,im,ih,age), dmax );

									v.clear();
								} else {
								std::cout << "case a < 0 and eq > 0 < hex" << std::endl;

									v.push_back(v_stay   (ia,iy,ip,im,ih,age));
									v.push_back(v_sell   (ia,iy,ip,im,ih,age));
									v.push_back(pars.myNA);
									v.push_back(v_file7_L(   iy,ip,im,ih,L_file7(iy,ip,im,ih,age),age));
									v.push_back(v_file13 (   iy,ip,im,ih,age));
									v.push_back(pars.myNA);

									v_iter= std::max_element(v.begin(),v.end());
									dmax = std::distance(v.begin(),v_iter) + 1;
									vmax = *v_iter;

									EXPECT_EQ( sol_v(ia,iy,ip,im,ih,age), vmax );
									EXPECT_EQ( sol_d(ia,iy,ip,im,ih,age), dmax );

									v.clear();
								}
							}
						} else {
							if ((*grids3D.at(2))(ip,ih,im) < 0){	// if there is negative equity

								std::cout << "case a > 0 and eq < 0" << std::endl;
									v.push_back(v_stay   (ia,iy,ip,im,ih,age));
									v.push_back(v_sell   (ia,iy,ip,im,ih,age));
									v.push_back(v_def    (ia,iy,ip,im,ih,age));
									v.push_back(pars.myNA);
									v.push_back(pars.myNA);
									v.push_back(pars.myNA);

									v_iter= std::max_element(v.begin(),v.end());
									dmax = std::distance(v.begin(),v_iter) + 1;
									vmax = *v_iter;

									EXPECT_EQ( sol_v(ia,iy,ip,im,ih,age), vmax );
									EXPECT_EQ( sol_d(ia,iy,ip,im,ih,age), dmax );

									v.clear();
							} else {
								std::cout << "case a > 0 and eq > 0" << std::endl;
									v.push_back(v_stay   (ia,iy,ip,im,ih,age));
									v.push_back(v_sell   (ia,iy,ip,im,ih,age));
									v.push_back(pars.myNA);
									v.push_back(pars.myNA);
									v.push_back(pars.myNA);
									v.push_back(pars.myNA);

									v_iter= std::max_element(v.begin(),v.end());
									dmax = std::distance(v.begin(),v_iter) + 1;
									vmax = *v_iter;

									EXPECT_EQ( sol_v(ia,iy,ip,im,ih,age), vmax );
									EXPECT_EQ( sol_d(ia,iy,ip,im,ih,age), dmax );

									v.clear();
							}
						}
					}
				}
			}
		}
	}
}

// test: 
// * set conditional value functions
//   inside the class equal to random values
// * compute discrete choice on those fucntions in the class
// * check that Indi7 and Indi13 are one in the right places
TEST_F(SolTest, CheckOwnDchoiceBKIndicators){


	int age = 1;

	int azero;
	azero = na-nap+1;	// na-nap+1. the +1 because of fortran indexing

	// set random values on class
	Sol->Set_v_rand()   ;

	// then get those values back
	Array<double,6> v_stay(Sol->Get_v_stay());
	Array<double,6> v_sell(Sol->Get_v_sell());
	Array<double,6> v_def (Sol->Get_v_def());
	Array<double,6> v_file7_L(Sol->Get_v_file7_L());	// must use L-conditional for file7
	Array<int   ,5> L_file7(Sol->Get_L_file7());
	Array<double,5> v_file13(Sol->Get_v_file13());
	Array<double,5> v_filedef(Sol->Get_v_filedef());

	// recompute v_file7
	Sol->ComputeOwnFile7(age);

	// now v_file7_L has the income restriction embedded!

	v_file7_L = Sol->Get_v_file7_L();
	L_file7 = Sol->Get_L_file7();
	
	
	// compute owner dchoice in class
	// this takes into account the equity restriction
	Sol->ComputeOwnerDchoice( age ) ;
	Array<int,6> Dchoice(Sol->GetOwnDchoice());
	Array<double,6> OwnIndi7(Sol->GetOwnIndi7());
	Array<double,6> OwnIndi13(Sol->GetOwnIndi13());

	// check that bk indicators are set in the right way
					
	for (int ip=1;ip<np+1;ip++){
		for (int ih=1; ih<nh+1;ih++){
			for (int im=1;im<nm+1;im++){
				for (int iy=1;iy<ny+1;iy++){
					for (int ia=1;ia<na+1;ia++){
							
						std::cout << "Dchoice(ia,iy,ip,im,ih,age) = " << Dchoice(ia,iy,ip,im,ih,age) << std::endl;

						switch(Dchoice(ia,iy,ip,im,ih,age)){


							case 1: EXPECT_EQ( OwnIndi7(ia,iy,ip,im,ih,age), 0 );
							        EXPECT_EQ( OwnIndi13(ia,iy,ip,im,ih,age), 0 );
									break;
							case 2: EXPECT_EQ( OwnIndi7(ia,iy,ip,im,ih,age), 0 );
							        EXPECT_EQ( OwnIndi13(ia,iy,ip,im,ih,age), 0 );
									break;
							case 3: EXPECT_EQ( OwnIndi7(ia,iy,ip,im,ih,age), 0 );
							        EXPECT_EQ( OwnIndi13(ia,iy,ip,im,ih,age), 0 );
									break;
							case 4: EXPECT_EQ( OwnIndi7(ia,iy,ip,im,ih,age), 1 );
							        EXPECT_EQ( OwnIndi13(ia,iy,ip,im,ih,age), 0 );
									break;
							case 5: EXPECT_EQ( OwnIndi7(ia,iy,ip,im,ih,age), 0 );
							        EXPECT_EQ( OwnIndi13(ia,iy,ip,im,ih,age), 1 );
									break;
							case 6: EXPECT_EQ( OwnIndi7(ia,iy,ip,im,ih,age), 1 );
							        EXPECT_EQ( OwnIndi13(ia,iy,ip,im,ih,age), 0 );
									break;
							default: FAIL() << "no valid Dchoice found. " ;
						}
					}
				}
			}
		}
	}
}

/*TEST_F(SolTest, Export_arma) {*/

	//// call ComputeOwnNetBorrow
	//int age = 1;
		//Sol->ComputeRentNetBorrow(  age );
	
	//// Get solution
	//Sol->ComputeOwnSell( age );
	//Sol->ComputeOwnSell( age );
	//Array<double,7> sol_val(Sol->Get_v_sell_L());


	//arma::mat A(Sol->v_sell_L_arma2D());

	////A = (Sol->blitz7D_arma2D) (Sol->Get_v_sell_L());

	//EXPECT_EQ(A.n_rows, na*ny*np*nm*nh*maxage);
	//EXPECT_EQ(A.n_cols, nL);

	//double a,b;

	//for (int ia=0;ia<na;ia++){

		//EXPECT_EQ( A(ia,0) , sol_val(ia+1,1,1,1,1,1,1) );
		//EXPECT_EQ( A(ia,1) , sol_val(ia+1,1,1,1,1,2,1) );

	//}
	
/*}*/










TEST_F(SolTest, Export_arma7D) {

	// call ComputeOwnNetBorrow
	int age = 1;
	Sol->ComputeOwnNetBorrow(  age );
	Sol->ComputeOwnStay( age );

	// Get solution
	Array<double,7> sol_val(Sol->Get_v_stay_L());
	Array<double,7> sol_val2(sol_val.extent(),F(7));

	sol_val2 = 2.1 * sol_val;	// create a false array
	
	arma::mat A(Sol->blitz7D_arma2D(Sol->Get_v_stay_L()));

	//A = (Sol->blitz7D_arma2D) (Sol->Get_v_sell_L());

	EXPECT_EQ(A.n_rows, na*ny*np*nm*nh*maxage);
	EXPECT_EQ(A.n_cols, nL);

	for (int ia=0;ia<na;ia++){
	for (int iy=0; iy<ny ;iy++){
	for (int ip=0; ip<np ;ip++){
	for (int im=0; im<nm ;im++){
	for (int ih=0; ih<nh ;ih++){
	for (int iL=0; iL<nL ;iL++){

		EXPECT_EQ( A(Sol->Get6DIdx(ia,iy,ip,im,ih,age-1),iL), sol_val(ia+1,iy+1,ip+1,im+1,ih+1,iL+1,age) ) ;
		EXPECT_NE( A(Sol->Get6DIdx(ia,iy,ip,im,ih,age-1),iL), sol_val2(ia+1,iy+1,ip+1,im+1,ih+1,iL+1,age)) ;

	}}}}}}
	
}




TEST_F(SolTest, TestBlitz6D_arma1D) {

	int age = 3;
	Sol->Set_PrBK_rand();

	// Get Pr7_own
	Array<double,6> sol_Pr7(Sol->GetPr7_own());

	// check they are not equal zero
	EXPECT_NE( 0.0 , sol_Pr7(1,1,1,1,1,1) );

	
	arma::vec A(Sol->blitz6D_arma1D(Sol->GetPr7_own()));

	//A = (Sol->blitz7D_arma2D) (Sol->Get_v_sell_L());

	EXPECT_EQ(A.n_elem, na*ny*np*nm*nh*maxage);

	for (int ia=0;ia<na;ia++){
	for (int iy=0; iy<ny ;iy++){
	for (int ip=0; ip<np ;ip++){
	for (int im=0; im<nm ;im++){
	for (int ih=0; ih<nh ;ih++){

		EXPECT_EQ( A(Sol->Get6DIdx(ia,iy,ip,im,ih,age-1) ) , sol_Pr7(ia+1,iy+1,ip+1,im+1,ih+1,age) ) ;

	}}}}}
	
}

TEST_F(SolTest, TestBlitz4D_arma1D) {

	int age = 3;
	Sol->Set_PrBK_rand();

	// Get Pr13_rent
	Array<double,4> sol_Pr13(Sol->GetPr13_buy1());

	// check they are not equal zero
	EXPECT_NE( 0.0 , sol_Pr13(1,1,1,1) );
	
	arma::vec A(Sol->blitz4D_arma1D(Sol->GetPr13_buy1()));

	//A = (Sol->blitz7D_arma2D) (Sol->Get_v_sell_L());

	EXPECT_EQ(A.n_elem, na*ny*np*maxage);

	for (int ia=0;ia<na;ia++){
	for (int iy=0; iy<ny ;iy++){
	for (int ip=0; ip<np ;ip++){

		EXPECT_EQ( A(Sol->Get4DIdx(ia,iy,ip,age-1) ) , sol_Pr13(ia+1,iy+1,ip+1,age) ) ;

	}}}
	
}





TEST_F(SolTest, Export_arma7D_int) {

	// call ComputeOwnNetBorrow
	int age = 1;
	Sol->ComputeOwnNetBorrow(  age );
	Sol->ComputeOwnStay( age );

	// Get savings
	Array<double,7> sol_val(Sol->Get_s_stay_L());

	// get savings mapped to armamatrix
	// should adjust index - 1
	arma::imat A(Sol->blitz7D_arma2D_int(Sol->Get_s_stay_L()));

	EXPECT_EQ(A.n_rows, na*ny*np*nm*nh*maxage);
	EXPECT_EQ(A.n_cols, nL);

	for (int ia=0;ia<na;ia++){
	for (int iy=0; iy<ny ;iy++){
	for (int ip=0; ip<np ;ip++){
	for (int im=0; im<nm ;im++){
	for (int ih=0; ih<nh ;ih++){
	for (int iL=0; iL<nL ;iL++){

		EXPECT_EQ( A(Sol->Get6DIdx(ia,iy,ip,im,ih,age-1),iL), std::max( sol_val(ia+1,iy+1,ip+1,im+1,ih+1,iL+1,age) - 1, 0.0) ) ;
		EXPECT_NE( A(Sol->Get6DIdx(ia,iy,ip,im,ih,age-1),iL), sol_val(ia+1,iy+1,ip+1,im+1,ih+1,iL+1,age)) ;

	}}}}}}

	EXPECT_EQ( A.min(), std::max( min(sol_val) - 1, 0.0 ) );
	EXPECT_EQ( A.max(), max(sol_val) - 1 );
	
}

TEST_F(SolTest, Export_arma6D) {

	// call ComputeOwnNetBorrow
	int age = 1;
	Sol->ComputeOwnFile7( age );

	// Get solution
	Array<double,6> sol_val(Sol->Get_v_file7_L());

	arma::mat A(Sol->blitz6D_arma2D(Sol->Get_v_file7_L()));

	EXPECT_EQ(A.n_rows, ny*np*nm*nh*maxage);
	EXPECT_EQ(A.n_cols, nL);

	for (int iy=0; iy<ny ;iy++){
	for (int ip=0; ip<np ;ip++){
	for (int im=0; im<nm ;im++){
	for (int ih=0; ih<nh ;ih++){
	for (int iL=0; iL<nL ;iL++){

		EXPECT_EQ( A(Sol->Get5DIdx(iy,ip,im,ih,age-1),iL), sol_val(iy+1,ip+1,im+1,ih+1,iL+1,age) ) ;

	}}}}}
	
}


TEST_F(SolTest, Export_arma5D) {

	// call ComputeOwnNetBorrow
	int age = 1;
	Sol->ComputeRenter( age );

	// Get solution
	Array<double,5> sol_val(Sol->Get_Rv_rent_L());

	arma::mat A(Sol->blitz5D_arma2D(Sol->Get_Rv_rent_L()));

	EXPECT_EQ(A.n_rows, na*ny*np*maxage);
	EXPECT_EQ(A.n_cols, nL);

	for (int ia=0; ia<na ;ia++){
	for (int iy=0; iy<ny ;iy++){
	for (int ip=0; ip<np ;ip++){
	for (int iL=0; iL<nL ;iL++){

		EXPECT_EQ( A(Sol->Get4DIdx(ia,iy,ip,age-1),iL), sol_val(ia+1,iy+1,ip+1,iL+1,age) ) ;

	}}}}
	
}


TEST_F(SolTest, Export_arma5D_int) {

	// call ComputeOwnNetBorrow
	int age = 1;
	Sol->ComputeRenter( age );

	// Get solution
	Array<double,5> sol_val(Sol->Get_Rs_rent_L());

	arma::imat A(Sol->blitz5D_arma2D_int(Sol->Get_Rs_rent_L()));

	EXPECT_EQ(A.n_rows, na*ny*np*maxage);
	EXPECT_EQ(A.n_cols, nL);

	for (int ia=0; ia<na ;ia++){
	for (int iy=0; iy<ny ;iy++){
	for (int ip=0; ip<np ;ip++){
	for (int iL=0; iL<nL ;iL++){

		EXPECT_EQ( A(Sol->Get4DIdx(ia,iy,ip,age-1),iL), std::max( sol_val(ia+1,iy+1,ip+1,iL+1,age) -1, 0.0) ) ;

	}}}}
	
	EXPECT_EQ( A.min(), std::max( min(sol_val) - 1, 0.0 ) );
	EXPECT_EQ( A.max(), max(sol_val) - 1 );
}


TEST_F(SolTest, Export_arma4D) {

	// call ComputeOwnNetBorrow
	int age = 1;
	Sol->ComputeRenterFile7( age );

	// Get solution
	Array<double,4> sol_val(Sol->Get_Rv_file7_L());

	arma::mat A(Sol->blitz4D_arma2D(Sol->Get_Rv_file7_L()));

	EXPECT_EQ(A.n_rows, ny*np*maxage);
	EXPECT_EQ(A.n_cols, nL);

	for (int iy=0; iy<ny ;iy++){
	for (int ip=0; ip<np ;ip++){
	for (int iL=0; iL<nL ;iL++){

		EXPECT_EQ( A(Sol->Get3DIdx(iy,ip,age-1),iL), sol_val(iy+1,ip+1,iL+1,age) ) ;

	}}}
	
}




TEST_F(SolTest, Export_arma5DBK) {

	// call ComputeOwnNetBorrow
	int age = 1;
	Sol->ComputeRenterBK7( age );

	// Get solution
	Array<double,5> sol_val(Sol->Get_Rv_BK7_L());

	arma::mat A(Sol->blitzRBK_arma2D(Sol->Get_Rv_BK7_L()));

	EXPECT_EQ(A.n_rows, nap*ny*np*maxage);
	EXPECT_EQ(A.n_cols, nL);

	for (int ia=0; ia<nap; ia++){
	for (int iy=0; iy<ny ;iy++){
	for (int ip=0; ip<np ;ip++){
	for (int iL=0; iL<nL ;iL++){

		EXPECT_EQ( A(Sol->Get4DIdx_BK(ia,iy,ip,age-1),iL), sol_val(ia+1,iy+1,ip+1,iL+1,age) ) ;

	}}}}
	
}




TEST_F(SolTest, Export_arma5DBK_int) {

	// call ComputeOwnNetBorrow
	int age = 1;
	Sol->ComputeRenterBK7( age );

	// Get solution
	Array<double,5> sol_val(Sol->Get_Rs_BK7_L());

	arma::imat A(Sol->blitzRBK_arma2D_int(Sol->Get_Rs_BK7_L()));

	EXPECT_EQ(A.n_rows, nap*ny*np*maxage);
	EXPECT_EQ(A.n_cols, nL);

	for (int ia=0; ia<nap; ia++){
	for (int iy=0; iy<ny ;iy++){
	for (int ip=0; ip<np ;ip++){
	for (int iL=0; iL<nL ;iL++){

		EXPECT_EQ( A(Sol->Get4DIdx_BK(ia,iy,ip,age-1),iL), std::max( sol_val(ia+1,iy+1,ip+1,iL+1,age) -1, 0.0 ) ) ;

	}}}}
	
	EXPECT_EQ( A.min(), std::max( min(sol_val) - 1, 0.0 ) );
	EXPECT_EQ( A.max(), max(sol_val) - 1 );
}








TEST_F(SolTest, CheckFindOwnLabor){

	int age;

	ranlib::DiscreteUniform<int> randage(maxage);
	age = randage.random() + 1;

	int azero;
	azero = na-nap+1;	// na-nap+1. the +1 because of fortran indexing

	Sol->Set_OwnDchoice_rand();

	// load labor supply fucntions witn randome numbers
	Sol->Set_OwnL_rand();

	//
	// get those numbers
	Array<int,6> Lstay(    Sol-> Get_L_stay());
	Array<int,6> Lsell(    Sol-> Get_L_sell());
	Array<int,6> Ldef    (    Sol-> Get_L_def ());
	Array<int,5> Lfile7  (   Sol-> Get_L_file7());
	Array<int,5> Lfile13 (  Sol-> Get_L_file13());
	Array<int,5> Lfiledef( Sol-> Get_L_filedef());


	Sol->FindOwnLabor(age);

	// get OwnLabor
	Array<int,6> OwnLabor(Sol->GetOwnLabor());
	Array<int,6> OwnDchoice(Sol->GetOwnDchoice());

	
	for (int ia=1;ia<na+1;ia++){
	for (int iy=1;iy<ny+1;iy++){
	for (int ip=1;ip<np+1;ip++){
	for (int im=1;im<nm+1;im++){
	for (int ih=1;ih<nh+1;ih++){

		switch( OwnDchoice(ia,iy,ip,im,ih,age) ){

			case 1: EXPECT_EQ( OwnLabor(ia,iy,ip,im,ih,age), Lstay(ia,iy,ip,im,ih,age));
					break;
			case 2: EXPECT_EQ( OwnLabor(ia,iy,ip,im,ih,age), Lsell(ia,iy,ip,im,ih,age));
					break;
			case 3: EXPECT_EQ( OwnLabor(ia,iy,ip,im,ih,age), Ldef(ia,iy,ip,im,ih,age));
					break;
			case 4: EXPECT_EQ( OwnLabor(ia,iy,ip,im,ih,age), Lfile7(  iy,ip,im,ih,age));
					break;
			case 5: EXPECT_EQ( OwnLabor(ia,iy,ip,im,ih,age), Lfile13(   iy,ip,im,ih,age));
					break;
			case 6: EXPECT_EQ( OwnLabor(ia,iy,ip,im,ih,age), Lfiledef(    iy,ip,im,ih,age));
					break;
			default: FAIL();
					break;

		}

	}}}}}
}




TEST_F(SolTest, CheckComputeOwnRepay13){

	int age;

	ranlib::DiscreteUniform<int> randage(maxage);
	age = randage.random() + 1;

	int azero;
	azero = na-nap+1;	// na-nap+1. the +1 because of fortran indexing

	// load labor supply fucntions witn randome numbers
	Sol->Set_OwnL_rand();

	//
	// get those numbers
	Array<int,6> Lstay(    Sol-> Get_L_stay());
	Array<int,6> Lsell(    Sol-> Get_L_sell());
	Array<int,6> Ldef    (    Sol-> Get_L_def ());
	Array<int,5> Lfile7  (   Sol-> Get_L_file7());
	Array<int,5> Lfile13 (  Sol-> Get_L_file13());
	Array<int,5> Lfiledef( Sol-> Get_L_filedef());

	// load random discrete choice
	Sol->Set_OwnDchoice_rand();
	// apply findOwnLabor inside class to populate OwnLabor
	Sol->FindOwnLabor(age);
	// do ComputeOwnRepay13
	Sol->ComputeOwnRepay13(age);

	// get OwnLabor
	Array<int,6> OwnLabor(Sol->GetOwnLabor());
	Array<int,6> OwnDchoice(Sol->GetOwnDchoice());

	// compare results
	Array<double,6> sol_Repay(Sol->GetOwnRepay13());

	Array<double,6> my_repay(na,ny,np,nm,nh,maxage,F(6));

	for (int ia=1;ia<na+1;ia++){
	for (int iy=1;iy<ny+1;iy++){
	for (int ip=1;ip<np+1;ip++){
	for (int im=1;im<nm+1;im++){
	for (int ih=1;ih<nh+1;ih++){

		if (ia < azero){
			my_repay(ia,iy,ip,im,ih,age) = (*grids4D.at(1))(ia,iy,OwnLabor(ia,iy,ip,im,ih,age),age) * pars.tau * 5;
			EXPECT_EQ(my_repay(ia,iy,ip,im,ih,age), sol_Repay(ia,iy,ip,im,ih,age) );

		}
	}}}}}
}



TEST_F( SolTest, CheckIntegration){

	// get random data
	Array<double,5> t1(na,ny,np,nm,nh,F(5));
	Array<double,5> t2(nap,ny,np,nm,nh,F(5));

	for (Array<double,5>::iterator it=t1.begin();it!=t1.end();it++){
		*it = uniGen.random();
	}
	for (Array<double,5>::iterator it=t2.begin();it!=t2.end();it++){
		*it = uniGen.random();
	}
	
	// compute integration in class
	Array<double,5> b1(Sol->integrateOwn(t1));
	Array<double,5> b2(Sol->integrateOwn(t2));

	// compute integration here

	Array<double,5> r1(t1.extent(),F(5));
	Array<double,5> r2(t2.extent(),F(5));

	Array<double,2> tmp(ny,np,F(2));

	// Get maximum value of t1: average of 
	// some values must be equal or below that value
	double m = max(t1);

	//check with full a dimension
	for (int ia=1;ia<na+1;ia++){
	for (int iy=1;iy<ny+1;iy++){
	for (int ip=1;ip<np+1;ip++){
	for (int im=1;im<nm+1;im++){
	for (int ih=1;ih<nh+1;ih++){

		tmp = 0;

		for (int jy=1;jy<ny+1;jy++){
		for (int jp=1;jp<np+1;jp++){

			tmp(jy,jp) = t1(ia,jy,jp,im,ih) * (*grids2D.at(4))(iy,jy) * (*grids2D.at(5))(ip,jp);

		}}

		// EXPECT_DOUBLE_EQ( sum(tmp), sum( sum( tmp , j ) ));
		r1(ia,iy,ip,im,ih) = sum( tmp );
		
		EXPECT_NEAR(r1(ia,iy,ip,im,ih), b1(ia,iy,ip,im,ih), 0.000001) << printf("ia=%d,iy=%d,ip=%d,im=%d,ih=%d\n",ia,iy,ip,im,ih);
		EXPECT_TRUE(r1(ia,iy,ip,im,ih) <= m );
	}}}}}


	// check with apos dimension
	for (int ia=1;ia<nap+1;ia++){
	for (int iy=1;iy<ny+1;iy++){
	for (int ip=1;ip<np+1;ip++){
	for (int im=1;im<nm+1;im++){
	for (int ih=1;ih<nh+1;ih++){

		tmp = 0;

		for (int jy=1;jy<ny+1;jy++){
		for (int jp=1;jp<np+1;jp++){

			tmp(jy,jp) = t2(ia,jy,jp,im,ih) * (*grids2D.at(4))(iy,jy) * (*grids2D.at(5))(ip,jp);

		}}

		// EXPECT_DOUBLE_EQ( sum(tmp), sum( sum( tmp , j ) ));
		r2(ia,iy,ip,im,ih) = sum( tmp );
		
		EXPECT_NEAR(r2(ia,iy,ip,im,ih), b2(ia,iy,ip,im,ih), 0.00001) << printf("ia=%d,iy=%d,ip=%d,im=%d,ih=%d\n",ia,iy,ip,im,ih);
	}}}}}
}

TEST_F( SolTest, CheckIntegration2){

	// get structured data
	Array<double,5> t1(na,ny,np,nm,nh,F(5));
	Array<double,5> t2(nap,ny,np,nm,nh,F(5));

	for (int ia=1;ia<na+1;ia++){
	for (int iy=1;iy<ny+1;iy++){
	for (int ip=1;ip<np+1;ip++){
	for (int im=1;im<nm+1;im++){
	for (int ih=1;ih<nh+1;ih++){

		t1(ia,iy,ip,im,ih) = ia+iy+ip-im+ih;

	}}}}}

	
	// compute integration in class
	Array<double,5> b1(Sol->integrateOwn(t1));

	// compute integration here

	Array<double,5> r1(t1.extent(),F(5));

	Array<double,2> tmp(ny,np,F(2));

	// Get maximum value of t1: average of 
	// some values must be equal or below that value
	double m = max(t1);

	//check with full a dimension
	for (int ia=1;ia<na+1;ia++){
	for (int iy=1;iy<ny+1;iy++){
	for (int ip=1;ip<np+1;ip++){
	for (int im=1;im<nm+1;im++){
	for (int ih=1;ih<nh+1;ih++){

		tmp = 0;

		for (int jy=1;jy<ny+1;jy++){
		for (int jp=1;jp<np+1;jp++){

			tmp(jy,jp) = t1(ia,jy,jp,im,ih) * (*grids2D.at(4))(iy,jy) * (*grids2D.at(5))(ip,jp);

		}}

		// EXPECT_DOUBLE_EQ( sum(tmp), sum( sum( tmp , j ) ));
		r1(ia,iy,ip,im,ih) = sum( tmp );
		
		EXPECT_DOUBLE_EQ(r1(ia,iy,ip,im,ih), b1(ia,iy,ip,im,ih)) << printf("ia=%d,iy=%d,ip=%d,im=%d,ih=%d\n",ia,iy,ip,im,ih);
		EXPECT_TRUE(r1(ia,iy,ip,im,ih) <= m );
	}}}}}


}



TEST_F( SolTest, TestFinalPeriod ){

	int age = maxage;

	Sol->ComputeFinal( age );

	Array<double,6> EVown(Sol->GetEVown());
	Array<double,6> EVown7(Sol->Get_EVown7());
	Array<double,6> EVown13(Sol->Get_EVown13());
	Array<double,6> Pr7_own(Sol->GetPr7_own());
	Array<double,6> Pr13_own(Sol->GetPr13_own());
	Array<double,6> OwnRepay13(Sol->GetOwnRepay13());

	Array<double,5> EVbuy(Sol->Get_EVbuy());
	Array<double,6> EVrent_def(Sol->GetEVrent_def());
	
	Array<double,4> EVrent(Sol->GetEVrent());
	Array<double,4> EVrent7(Sol->Get_EVrent7());
	Array<double,4> EVrent13(Sol->Get_EVrent13());
	Array<double,4> Pr7_rent(Sol->GetPr7_rent());
	Array<double,4> Pr13_rent(Sol->GetPr13_rent());
	Array<double,4> RentRepay13(Sol->GetRentRepay13());


	for (int iy=1;iy<ny+1; iy++){
	for (int ip=1;ip<np+1; ip++){
	for (int im=1;im<nm+1; im++){
	for (int ih=1;ih<nh+1; ih++){

	for (int ia=1;ia<na+1; ia++){


		if (ia<azero){
			
			EXPECT_EQ( EVown(ia,iy,ip,im,ih,age), pars.myNA );
			EXPECT_EQ( EVrent(ia,iy,ip,age) , pars.myNA );

			EXPECT_EQ( EVbuy(ia,iy,ip,ih,age),pars.myNA  ) ;
			EXPECT_EQ( EVrent_def(ia,iy,ip,im,ih,age),pars.myNA  ) ;

		} else if (ia > azero) {

			EXPECT_EQ(EVown( ia,iy,ip,im,ih,age), pars.omega * ( pars.imgamma * pow( (*grids1D.at(0))(ia), pars.mgamma ) + pars.omegaH * (*grids2D.at(1))(ip,ih)) );
			EXPECT_EQ(EVrent(ia,iy,ip,age)      , pars.omega * (pars.imgamma * pow( (*grids1D.at(0))(ia), pars.mgamma ) ) );

			EXPECT_EQ( EVbuy(ia,iy,ip,ih,age), EVown(ia,iy,ip,1,ih,age) ) ;
			EXPECT_EQ( EVrent_def(ia,iy,ip,im,ih,age), EVrent(ia,iy,ip,age) ) ;

		} else if (ia==azero){

			EXPECT_EQ(EVown( ia,iy,ip,im,ih,age), EVown( ia+1,iy,ip,im,ih,age) - (EVown( ia+2,iy,ip,im,ih,age) - EVown( ia+1,iy,ip,im,ih,age)));
			EXPECT_EQ(EVrent(ia,iy,ip,age)      , EVrent( ia+1,iy,ip,age) - (EVrent( ia+2,iy,ip,age) - EVrent( ia+1,iy,ip,age)) );

			EXPECT_EQ( EVbuy(ia,iy,ip,ih,age), EVown(ia,iy,ip,1,ih,age) ) ;
			EXPECT_EQ( EVrent_def(ia,iy,ip,im,ih,age), EVrent(ia,iy,ip,age) ) ;

		}

	}

	for (int ia=1;ia<nap+1; ia++){

		EXPECT_EQ( EVown(ia+azero-1,iy,ip,im,ih,age) - pars.omegaBK, EVown7( ia,iy,ip,im,ih,age) ) << printf("index: %d %d %d %d %d\n",ia,iy,ip,im,ih);
		EXPECT_EQ( EVown(ia+azero-1,iy,ip,im,ih,age) - pars.omegaBK, EVown13(ia,iy,ip,im,ih,age) ) << printf("index: %d %d %d %d %d\n",ia,iy,ip,im,ih);

		EXPECT_EQ( EVrent(ia+azero-1,iy,ip,age) - pars.omegaBK, EVrent7(ia,iy,ip,age) );
		EXPECT_EQ( EVrent(ia+azero-1,iy,ip,age) - pars.omegaBK , EVrent13(ia,iy,ip,age) );

	}
		

	}}}}

	// check that EVown(a=0) != NA:
	
	for (int iy=1;iy<ny+1; iy++){
	for (int ip=1;ip<np+1; ip++){
	for (int im=1;im<nm+1; im++){
	for (int ih=1;ih<nh+1; ih++){
	
		EXPECT_TRUE( EVown( azero,iy,ip,im,ih,age) > pars.myNA ) ;
		EXPECT_TRUE( EVrent(azero,iy,ip,      age) > pars.myNA ) ;
		EXPECT_TRUE( EVbuy( azero,iy,ip,   ih,age) > pars.myNA ) ;
		//EXPECT_TRUE( EVown7(   1 ,iy,ip,im,ih,age) > pars.myNA ) ;
		//EXPECT_TRUE( EVown13(  1 ,iy,ip,im,ih,age) > pars.myNA ) ;
		EXPECT_TRUE( EVrent(azero,iy,ip,      age) > pars.myNA ) ;
		//EXPECT_TRUE( EVrent7(  1 ,iy,ip,      age) > pars.myNA ) ;
		//EXPECT_TRUE( EVrent13( 1 ,iy,ip,      age) > pars.myNA ) ;

	}}}}

	// check that EVown(a) != NA for a>=0
	
	for (int iy=1;iy<ny+1; iy++){
	for (int ip=1;ip<np+1; ip++){
	for (int im=1;im<nm+1; im++){
	for (int ih=1;ih<nh+1; ih++){

		for (int ia=azero;ia<na+1;ia++){
	
		EXPECT_TRUE( EVown( ia,iy,ip,im,ih,age) > pars.myNA ) ;
		EXPECT_TRUE( EVrent(ia,iy,ip,      age) > pars.myNA ) ;
		EXPECT_TRUE( EVbuy( ia,iy,ip,   ih,age) > pars.myNA ) ;
		EXPECT_TRUE( EVrent(ia,iy,ip,      age) > pars.myNA ) ;

	}}}}}

   /* for (int iy=1;iy<ny+1; iy++){*/
	//for (int ip=1;ip<np+1; ip++){
	//for (int im=1;im<nm+1; im++){
	//for (int ih=1;ih<nh+1; ih++){

		//for (int ia=1;ia<nap+1;ia++){

			//EXPECT_TRUE( EVown7(   ia,iy,ip,im,ih,age) > pars.myNA ) ;
			//EXPECT_TRUE( EVown13(  ia,iy,ip,im,ih,age) > pars.myNA ) ;
			//EXPECT_TRUE( EVrent7(  ia,iy,ip,      age) > pars.myNA ) ;
			//EXPECT_TRUE( EVrent13( ia,iy,ip,      age) > pars.myNA ) << EVrent13( ia,iy,ip,      age) ;

		/*}}}}}*/
}

TEST_F( SolTest, TestHowRangeWorks ){

	// check how to get the right indices with Range()
	Array<double,2> tarr(na,ny,F(2));
	for (Array<double,2>::iterator ite=tarr.begin();ite!=tarr.end();ite++){  *ite = uniGen.random(); }
	
	Array<double,1> slice(na,F(1));
	slice = tarr(all,1);
	Array<double,1> range(nap,F(1));
	range = tarr(Range(azero,toEnd),1);

	for (int ia=1;ia<nap+1; ia++){

		EXPECT_EQ(slice(ia+azero-1), range(ia)) << printf("index %d",ia);
	}



}





/*TEST_F( SolTest, CanRunComputePeriod) {*/
	//int age = maxage-1;
	//// Sol->ComputeOwnStay(age);	// Sol->Computes {v1,s1,c1,v2,s2,c2,L=which.max(v1,v2),vmax=max(v1,v2)}
	//// Sol->ComputeOwnSell(age);
	//// Sol->ComputeOwnDef(age);
	//// Sol->ComputeOwnFile7(age);
	//// Sol->ComputeOwnFile13(age);
	//// Sol->ComputeOwnFileDef(age);

	//// Sol->ComputeOwnerDchoice(age);
	
	//// Sol->ComputeOwnBK7Stay(age);
	//// Sol->ComputeOwnBK7Sell(age);
	//// Sol->ComputeOwnBK7Def(age);

	//// Sol->ComputeOwnerBK7Dchoice(age);
	
	//// Sol->ComputeOwnBK13Stay(age);
	//// Sol->ComputeOwnBK13Sell(age);
	//// Sol->ComputeOwnBK13Def(age);

	//// Sol->ComputeOwnerBK13Dchoice(age);

	//// Sol->ComputeRenter( age );
	//Sol->ComputeBuyer( age );
	//// Sol->ComputeRenterFile7( age );
	//// Sol->ComputeRenterFile13( age );
	
	//// Sol->ComputeRenterDchoice(age);

	//// Sol->ComputeRenterBK7( age );
	//// Sol->ComputeRenterBK13( age );

	//// // 5a) Sol->Compute OwnRepay13 and RentRepay13 by looking at labor supplied in each state
	//// Sol->ComputeOwnRepay13( age );
	//// Sol->ComputeRentRepay13( age );
	
	//// // 6) compute expectations for all relevant objects
	//// // Probabilities of BK: Pr7_own, Pr13_own, Pr7_rent, Pr13_rent, 
	//// // OwnRepay13, RentRepay13, EVown, EVown7, EVown13, EVrent, EVrent7, EVrent13

	//// Sol->Integrate_all( age );


/*}*/
	





int main(int argc, char **argv) { 
  ::testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}

