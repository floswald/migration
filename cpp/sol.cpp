
#include "BK_sol.h"
#include "misc.h"	//linspace


int main(){

	std::cout<< "solution boilerplate running." << std::endl;

	// make currdebt
	int na     = 15;
	int ny     = 5;
	int np     = 4;
	int nm     = 5;
	int nh     = 2;
	int nap    ;	// you cannot choose this number you dork!!!!!!!!!
	int nL     = 2;
	int maxage = 5;
	

	pStruct pars;
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
	
	ranlib::Uniform<double> uniGen;

	
	// Data Objects we need to supply to 
	// the class
	
	Array<double,1> agrid(na,F(1)),Hgrid(nh,F(1)),Lgrid(nL,F(1));
	agrid = linspace(-1,1,na);

	int azero = 0;
	int i=1;
	while (azero==0){
		if (agrid(i)>=0) {
			azero = i;
		}
		i++;
	}

	nap = na - azero + 1;
	Array<double,1> aposgrid(nap,F(1));
	aposgrid = agrid(Range(azero,toEnd));


	std::vector<int> dimvec;
	dimvec.push_back( na );
	dimvec.push_back( ny     );
	dimvec.push_back( np     );
	dimvec.push_back( nm     );
	dimvec.push_back( nh     );
	dimvec.push_back( nap    );
	dimvec.push_back( nL     );
	dimvec.push_back( maxage );


	Hgrid = 0.55, 1;
	Lgrid =linspace(1,nL,nL);

	std::vector< Array<double, 1> * > grids1D;	// agrid(a),aposgrid(apos)
	grids1D.push_back( &agrid );
	grids1D.push_back( &aposgrid );
	grids1D.push_back( &Lgrid );
	grids1D.push_back( &Hgrid );

	Array<double,2> ygrid(ny,maxage,F(2));
	Array<double,2> pgrid(np,nh,F(2));
	Array<double,2> initdebt(np,nh,F(2));
	Array<double,2> currdebt(nm,nh,F(2));
	Array<double,2> G(ny,ny,F(2));
	Array<double,2> Gp(np,np,F(2));

	for (int iy=1;iy<ny+1;iy++){
		for (int it=1;it<maxage+1;it++){
			ygrid(iy,it) = iy + 0.3*it;
		}
	}
	for (int ip=1;ip<np+1;ip++){
	for (int ih=1;ih<nh+1;ih++){
		pgrid(ip,ih) = ip + ih;
		initdebt(ip,ih) = pgrid(ip,ih)*(1-pars.down);
	}}

	for (int ih=1;ih<nh+1;ih++){
	currdebt(Range::all(),ih) = linspace(0,initdebt(np,ih)*(1-pars.down),nm) ;
	}

	for (int iy=1;iy<ny+1;iy++){
	for (int jy=1;jy<ny+1;jy++){
		G(iy,jy) = 1.0/ny;
	}}

	//Gp
	for (int ip=1;ip<np+1;ip++){
	for (int jp=1;jp<np+1;jp++){
		Gp(ip,jp) = 1.0/np;
	}}
	

	std::vector< Array<double, 2> * > grids2D;	// ygrid(y,age),pgrid(p,h),currdebt(m,h),initdebt(m,h)
	grids2D.push_back( &ygrid );
	grids2D.push_back( &pgrid );
	grids2D.push_back( &currdebt);
	grids2D.push_back( &initdebt);
	grids2D.push_back( &G);	// y transition
	grids2D.push_back( &Gp);	// p transition


	Array<double,3> nextdebt(nm,nh,maxage,F(3));
	for (int im=1;im<nm+1;im++){
		for (int ih=1;ih<nh+1;ih++){
			for (int it=1;it<maxage+1;it++){
				nextdebt(im,ih,it) = currdebt(im,ih) * 0.9 ;	// should be a function of age, really.
			}
		}
	}

	Array<double,3> nextdebtbuyer(np,nh,maxage,F(3));
	for (int ip=1;ip<np+1;ip++){
		for (int ih=1;ih<nh+1;ih++){
			for (int it=1;it<maxage+1;it++){
				nextdebtbuyer(ip,ih,it) = initdebt(ip,ih) * 0.79 ;
			}
		}
	}
	Array<double,3> equity(np,nh,nm,F(3));
	for (int ip=1;ip<np+1;ip++){
		for (int ih=1;ih<nh+1;ih++){
			for (int im=1;im<nm+1;im++){
				equity(ip,ih,im) = pgrid(ip,ih) * pars.phi_s - currdebt(im,ih)  ;
			}
		}
	}
	
	Array<double,3> secequitybuy(na,np,nh,F(3));
	secequitybuy = 0;
	for (int ih=1;ih<nh+1;ih++){
		for (int ip=1;ip<np+1;ip++){
			for (int ia=1;ia<na+1;ia++){
				if (agrid(ia)<0){							// price * phi - init.debt
					secequitybuy(ia,ip,ih) = std::min( std::max( pgrid(ip,ih) * pars.phi_s - initdebt(ip,ih) - pars.hex, 0.0 ), -agrid(ia) );
				}
			}
		}
	}

	std::vector< Array<double, 3> * > grids3D;	// nextdebt(m,h,age),nextdebtbuyer(p,h,age)
	grids3D.push_back( &nextdebt );
	grids3D.push_back( &nextdebtbuyer );
	grids3D.push_back( &equity );
	grids3D.push_back( &secequitybuy );

	Array<double,4> secequity(na,np,nm,nh,F(4));
	secequity = 0;
	for (int im=1;im<nm+1;++im){
		for (int ih=1;ih<nh+1;ih++){
			for (int ip=1;ip<np+1;ip++){
				for (int ia=1;ia<na+1;ia++){
					if (agrid(ia)<0){
						secequity(ia,ip,im,ih) = std::min( std::max( pgrid(ip,ih) * pars.phi_s - currdebt(im,ih) - pars.hex, 0.0 ), -agrid(ia) );
					}
				}
			}
		}
	}
	Array<double,4> repay(na,ny,nL,maxage,F(4));
	repay = 0;
	for (int ia=1;ia<na+1;ia++){
		for (int iy=1;iy<ny+1;iy++){
			for (int iL=1;iL<nL+1;iL++){
				for (int it=1;it<maxage+1;it++){
					if (agrid(ia)<0){
						repay(ia,iy,iL,it) = std::min( Lgrid(iL) * ygrid(iy,it), -agrid(ia) );
					}
				}
			}
		}
	}

	std::vector< Array<double, 4> * > grids4D; 	// secequity(a,p,m,h), repay(a,y,L,age)
	grids4D.push_back( &secequity );
	grids4D.push_back( &repay );
	// construct instance Sol
	// construct linear function of mortgage dimension
	// assess interpolation at 0.9*mgrid

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
	

	Solution Sol(pars,
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

	for (int age=maxage; age>0; age--){

		printf("period %d of %d\n",age,maxage);

		if (age==maxage){

			Sol.ComputeFinal( age );
		} else {

			Sol.ComputePeriod( age );
		}
	}

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



	Sol.Show();

	std::cout<< "end of solution." << std::endl;

 
 return( 0 );

}








