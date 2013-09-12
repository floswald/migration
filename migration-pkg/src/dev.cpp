



// developments

#include <blitz/array.h>
#include <Rcpp.h>

using namespace blitz;

//' dev5: adding house housing tenure choice
//'
//' @examples
//' nA <- 5L; nY <- 3L; nT <- 3L; nH <- 2
//' s <- data.table(expand.grid(a=seq(0,10,length=nA),y=1:nY,it=1:nT))
//' s[,cash1 := a + y + 0.3*it]
//' s[,cash2 := a + y + 0.2*it + 0.09*it^2]
//' save <- seq(0,10,length=nA)
//' ss <- data.table(expand.grid(a=seq(0,10,length=nA),y=1:nY,it=1:nT,save=save))
//' ss[,cons1 := a + y + 0.3*it - save]
//' ss[,cons2 := a + y + 0.2*it + 0.09*it^2 - save]
//' dataR <- list( cash1 = s[["cash1"]],
//'                cash2 = s[["cash2"]],
//'                dims = c(nA,nY,nT),
//'                savings = save,
//'                theta = 1.2,beta=0.95,myNA=-99)
//' blitz <- dev4(data=dataR)
//' 
//' ######################################################
//' # Calculating an R solution to this 3-period lifecycle
//' # with discrete choice in periods 1 and 2.
//' ######################################################
//' 
//'	V = array(0,c(nA,nY,nT))  # max( v1, v2 )
//'	D = array(0,c(nA,nY,nT))  # which.max( v1, v2 )
//'	v1 = array(0,c(nA,nY,nT))
//'	v2 = array(0,c(nA,nY,nT))
//'	S1 = array(0,c(nA,nY,nT))
//'	S2 = array(0,c(nA,nY,nT))
//'	V[ , ,3] = s[it==3,log(cash1)]
//' C1 = ss[,array(cons1,c(nA,nY,nT,nA))]
//' C2 = ss[,array(cons2,c(nA,nY,nT,nA))]
//' 
//' W1 = array(0,c(nA,nY,nT,nA))
//' W2 = array(0,c(nA,nY,nT,nA))
//' for (ti in 2:1) {
//'     for (ia in 1:nA) {
//'          for(iy in 1:nY) {
//'              for (ja in 1:nA){
//' 				 # optimal cons option 1
//'                  if (C1[ia,iy,ti,ja] > 0){
//' 		     		W1[ia,iy,ti,ja] = log(C1[ia,iy,ti,ja]) + dataR$beta*V[ja,iy,ti+1]
//' 			     } else {
//' 			        W1[ia,iy,ti,ja] = dataR$myNA
//' 			     }
//' 				 # optimal cons option 2
//'                  if (C2[ia,iy,ti,ja] > 0){
//' 		     		W2[ia,iy,ti,ja] = log(C2[ia,iy,ti,ja]) + dataR$beta*V[ja,iy,ti+1]
//' 			     } else {
//' 			        W2[ia,iy,ti,ja] = dataR$myNA
//' 			     }
//' 		     }
//' 			v1[ia,iy,ti] = max(W1[ia,iy,ti, ])
//' 			S1[ia,iy,ti] = which.max(W1[ia,iy,ti, ])
//' 			v2[ia,iy,ti] = max(W2[ia,iy,ti, ])
//' 			S2[ia,iy,ti] = which.max(W2[ia,iy,ti, ])
//'				V[ia,iy,ti] = max(v1[ia,iy,ti], v2[ia,iy,ti])
//'				D[ia,iy,ti] = which.max(c(v1[ia,iy,ti], v2[ia,iy,ti]))
//'	         }
//'	     }
//'	}
//' Cstar1 = array( C1[ cbind(1:(nA*nY*nT), as.numeric(S1) ) ], c(nA,nY,nT))
//' Cstar2 = array( C2[ cbind(1:(nA*nY*nT), as.numeric(S2) ) ], c(nA,nY,nT))
//' CstarBlitz1 = array( C1[ cbind(1:(nA*nY*nT), as.numeric(blitz$save1) ) ], c(nA,nY,nT))
//' CstarBlitz2 = array( C2[ cbind(1:(nA*nY*nT), as.numeric(blitz$save2) ) ], c(nA,nY,nT))
//' 
//' ############################################
//' # comparing R and blitz++ solutions
//' ############################################
//' 
//' print(all.equal(V,blitz$V))
//' print(all.equal(C1,blitz$cons1))
//' print(all.equal(C2,blitz$cons2))
//' print(all.equal(v1,blitz$v1))
//' print(all.equal(v2,blitz$v2))
//' print(all.equal(S1,blitz$save1))
//' print(all.equal(S2,blitz$save2))
//' print(all.equal(Cstar1,CstarBlitz1))
//' print(all.equal(Cstar2,CstarBlitz2))
// [[Rcpp::export]]
Rcpp::List dev4( Rcpp::List data ) {

// current example: v(a,y,t) = max(v1(a,y,t),v2(a,y,t))
// build consumption, evaluate utility


	// R array data
	Rcpp::NumericVector R_C1 = Rcpp::as<Rcpp::NumericVector>(data["cash1"]);
	Rcpp::NumericVector R_C2 = Rcpp::as<Rcpp::NumericVector>(data["cash2"]);
	Rcpp::NumericVector R_S  = Rcpp::as<Rcpp::NumericVector>(data["savings"]);
	Rcpp::IntegerVector d    = Rcpp::as<Rcpp::IntegerVector>(data["dims"]);

	// R parameter data
	double theta = Rcpp::as<double>(data["theta"]);
	double beta  = Rcpp::as<double>(data["beta"]);
	double myNA  = Rcpp::as<double>(data["myNA"]);

	int ns = R_S.size();
	int nState = d(0) * d(1) * d(2);


	// (conditional) value functions, savings policies and discrete choice
	Array<double,3> V(d(0),d(1),d(2),FortranArray<3>());
	Array<double,3> D(d(0),d(1),d(2),FortranArray<3>());
	Array<double,3> v1(d(0),d(1),d(2),FortranArray<3>());
	Array<double,3> v2(d(0),d(1),d(2),FortranArray<3>());
	Array<double,3> s1(d(0),d(1),d(2),FortranArray<3>());
	Array<double,3> s2(d(0),d(1),d(2),FortranArray<3>());
	V  = 0;
	D  = 0;
	v1 = 0;
	v2 = 0;
	s1 = 0;
	s2 = 0;

	// cash tensors
	Array<double,3> Cash1(R_C1.begin(), shape(d(0),d(1),d(2)),neverDeleteData,FortranArray<3>());
	Array<double,3> Cash2(R_C2.begin(), shape(d(0),d(1),d(2)),neverDeleteData,FortranArray<3>());

	// savings tensor (just a vector)
	Array<double,1> S( R_S.begin(),R_S.length(),neverDeleteData,FortranArray<1>() );

	// consumption tensor and ctmp
	Array<double,4> Cons1(d(0),d(1),d(2),ns,FortranArray<4>());
	Array<double,4> Cons2(d(0),d(1),d(2),ns,FortranArray<4>());
	Array<double,3> ctmp(d(0),d(1),ns,FortranArray<3>());
	Cons1 = 0;
	Cons2 = 0;

	// full value tensor and tmp
	Array<double,3> w(d(0),d(1),ns,FortranArray<3>());
	Array<double,2> vtmp1(d(0),d(1),FortranArray<2>());
	Array<double,2> vtmp2(d(0),d(1),FortranArray<2>());
	Array<double,2> stmp(d(0),d(1),FortranArray<2>());
	Array<double,2> vplustmp(d(0),d(1),FortranArray<2>());
	w        = 0;
	vtmp1    = 0;
	vtmp2    = 0;
	vplustmp = 0;


	// tensor index descriptors
	firstIndex  i;	
	secondIndex j;  
	thirdIndex  k;	
	fourthIndex l;	

	Cons1 = Cash1(i,j,k) - S(l);
	Cons2 = Cash2(i,j,k) - S(l);

	// loop over time
	// final period: where cons is positive, say utility is log(cash)
	
	for (int t=d(2); t>0; t--){

		// if final period
		if (t==d(2)){

			V(Range::all(),Range::all(),t) = log( Cash1(Range::all(),Range::all(),t) ); 

		} else {
			
			vplustmp = V(Range::all(),Range::all(),t+1);
			
			// option 1
			ctmp     = Cons1(Range::all(),Range::all(),t,Range::all());		// consumption at all savings states in period t

			w        = where(ctmp > 0, log( ctmp(i,j,k) ) + beta * vplustmp(k,j), myNA );
			vtmp1    = max( w, k);
			stmp     = maxIndex( w, k);
			// unnecessary
			v1(Range::all(),Range::all(),t) = vtmp1;
			s1(Range::all(),Range::all(),t) = stmp;
			
			// option 2
			ctmp     = Cons2(Range::all(),Range::all(),t,Range::all());		// consumption at all savings states in period t

			w        = where(ctmp > 0, log( ctmp(i,j,k) ) + beta * vplustmp(k,j), myNA );
			vtmp2    = max( w, k);
			stmp     = maxIndex( w, k);
			// unnecessary
			v2(Range::all(),Range::all(),t) = vtmp2;
			s2(Range::all(),Range::all(),t) = stmp;
		
			// find vmax
			V(Range::all(),Range::all(),t) = where(vtmp1 > vtmp2, vtmp1, vtmp2);
			// find dchoice
			D(Range::all(),Range::all(),t) = where(vtmp1 > vtmp2, 1, 2);

		}

	}

	// map to output objects
	//
	// Maximal value function and discrete choice function
	Rcpp::NumericVector Vout(nState);
	Rcpp::NumericVector Dout(nState);

	Vout = V;
	Dout = D;
	Vout.attr("dim") = Rcpp::IntegerVector::create(d(0),d(1),d(2));
	Dout.attr("dim") = Rcpp::IntegerVector::create(d(0),d(1),d(2));

	// consumption functions
	Rcpp::NumericVector C1_out(Cons1.size());
	Rcpp::NumericVector C2_out(Cons2.size());

	C1_out = Cons1;
	C2_out = Cons2;
	C1_out.attr("dim") = Rcpp::IntegerVector::create(d(0),d(1),d(2),ns);
	C2_out.attr("dim") = Rcpp::IntegerVector::create(d(0),d(1),d(2),ns);
	
	// conditional value functions
	Rcpp::NumericVector v1_out(v1.size());
	Rcpp::NumericVector v2_out(v2.size());

	v1_out = v1;
	v2_out = v2;
	v1_out.attr("dim") = Rcpp::IntegerVector::create(d(0),d(1),d(2));
	v2_out.attr("dim") = Rcpp::IntegerVector::create(d(0),d(1),d(2));

	// savings function
	Rcpp::NumericVector s1_out(s1.size());
	Rcpp::NumericVector s2_out(s2.size());

	s1_out = s1;
	s2_out = s2;
	s1_out.attr("dim") = Rcpp::IntegerVector::create(d(0),d(1),d(2));
	s2_out.attr("dim") = Rcpp::IntegerVector::create(d(0),d(1),d(2));

	// create output list
	Rcpp::List list = Rcpp::List::create( Rcpp::_["V"] = Vout, 
			                              Rcpp::_["D"] = Dout, 
										  Rcpp::_["cons1"] = C1_out,
										  Rcpp::_["cons2"] = C2_out,
										  Rcpp::_["save1"] = s1_out,
										  Rcpp::_["save2"] = s2_out,
										  Rcpp::_["v1"] = v1_out,
										  Rcpp::_["v2"] = v2_out);
    return list;
}   




//' dev4
//'
//' @examples
//' nA <- 5L; nY <- 3L; nT <- 3L
//' s <- data.table(expand.grid(a=seq(0,10,length=nA),y=1:nY,it=1:nT))
//' s[,cash1 := a + y + 0.3*it]
//' s[,cash2 := a + y + 0.2*it + 0.09*it^2]
//' save <- seq(0,10,length=nA)
//' ss <- data.table(expand.grid(a=seq(0,10,length=nA),y=1:nY,it=1:nT,save=save))
//' ss[,cons1 := a + y + 0.3*it - save]
//' ss[,cons2 := a + y + 0.2*it + 0.09*it^2 - save]
//' dataR <- list( cash1 = s[["cash1"]],
//'                cash2 = s[["cash2"]],
//'                dims = c(nA,nY,nT),
//'                savings = save,
//'                theta = 1.2,beta=0.95,myNA=-99)
//' blitz <- dev4(data=dataR)
//' 
//' ######################################################
//' # Calculating an R solution to this 3-period lifecycle
//' # with discrete choice in periods 1 and 2.
//' ######################################################
//' 
//'	V = array(0,c(nA,nY,nT))  # max( v1, v2 )
//'	D = array(0,c(nA,nY,nT))  # which.max( v1, v2 )
//'	v1 = array(0,c(nA,nY,nT))
//'	v2 = array(0,c(nA,nY,nT))
//'	S1 = array(0,c(nA,nY,nT))
//'	S2 = array(0,c(nA,nY,nT))
//'	V[ , ,3] = s[it==3,log(cash1)]
//' C1 = ss[,array(cons1,c(nA,nY,nT,nA))]
//' C2 = ss[,array(cons2,c(nA,nY,nT,nA))]
//' 
//' W1 = array(0,c(nA,nY,nT,nA))
//' W2 = array(0,c(nA,nY,nT,nA))
//' for (ti in 2:1) {
//'     for (ia in 1:nA) {
//'          for(iy in 1:nY) {
//'              for (ja in 1:nA){
//' 				 # optimal cons option 1
//'                  if (C1[ia,iy,ti,ja] > 0){
//' 		     		W1[ia,iy,ti,ja] = log(C1[ia,iy,ti,ja]) + dataR$beta*V[ja,iy,ti+1]
//' 			     } else {
//' 			        W1[ia,iy,ti,ja] = dataR$myNA
//' 			     }
//' 				 # optimal cons option 2
//'                  if (C2[ia,iy,ti,ja] > 0){
//' 		     		W2[ia,iy,ti,ja] = log(C2[ia,iy,ti,ja]) + dataR$beta*V[ja,iy,ti+1]
//' 			     } else {
//' 			        W2[ia,iy,ti,ja] = dataR$myNA
//' 			     }
//' 		     }
//' 			v1[ia,iy,ti] = max(W1[ia,iy,ti, ])
//' 			S1[ia,iy,ti] = which.max(W1[ia,iy,ti, ])
//' 			v2[ia,iy,ti] = max(W2[ia,iy,ti, ])
//' 			S2[ia,iy,ti] = which.max(W2[ia,iy,ti, ])
//'				V[ia,iy,ti] = max(v1[ia,iy,ti], v2[ia,iy,ti])
//'				D[ia,iy,ti] = which.max(c(v1[ia,iy,ti], v2[ia,iy,ti]))
//'	         }
//'	     }
//'	}
//' Cstar1 = array( C1[ cbind(1:(nA*nY*nT), as.numeric(S1) ) ], c(nA,nY,nT))
//' Cstar2 = array( C2[ cbind(1:(nA*nY*nT), as.numeric(S2) ) ], c(nA,nY,nT))
//' CstarBlitz1 = array( C1[ cbind(1:(nA*nY*nT), as.numeric(blitz$save1) ) ], c(nA,nY,nT))
//' CstarBlitz2 = array( C2[ cbind(1:(nA*nY*nT), as.numeric(blitz$save2) ) ], c(nA,nY,nT))
//' 
//' ############################################
//' # comparing R and blitz++ solutions
//' ############################################
//' 
//' print(all.equal(V,blitz$V))
//' print(all.equal(C1,blitz$cons1))
//' print(all.equal(C2,blitz$cons2))
//' print(all.equal(v1,blitz$v1))
//' print(all.equal(v2,blitz$v2))
//' print(all.equal(S1,blitz$save1))
//' print(all.equal(S2,blitz$save2))
//' print(all.equal(Cstar1,CstarBlitz1))
//' print(all.equal(Cstar2,CstarBlitz2))
// [[Rcpp::export]]
Rcpp::List dev4( Rcpp::List data ) {

// current example: v(a,y,t) = max(v1(a,y,t),v2(a,y,t))
// build consumption, evaluate utility


	// R array data
	Rcpp::NumericVector R_C1 = Rcpp::as<Rcpp::NumericVector>(data["cash1"]);
	Rcpp::NumericVector R_C2 = Rcpp::as<Rcpp::NumericVector>(data["cash2"]);
	Rcpp::NumericVector R_S  = Rcpp::as<Rcpp::NumericVector>(data["savings"]);
	Rcpp::IntegerVector d    = Rcpp::as<Rcpp::IntegerVector>(data["dims"]);

	// R parameter data
	double theta = Rcpp::as<double>(data["theta"]);
	double beta  = Rcpp::as<double>(data["beta"]);
	double myNA  = Rcpp::as<double>(data["myNA"]);

	int ns = R_S.size();
	int nState = d(0) * d(1) * d(2);


	// (conditional) value functions, savings policies and discrete choice
	Array<double,3> V(d(0),d(1),d(2),FortranArray<3>());
	Array<double,3> D(d(0),d(1),d(2),FortranArray<3>());
	Array<double,3> v1(d(0),d(1),d(2),FortranArray<3>());
	Array<double,3> v2(d(0),d(1),d(2),FortranArray<3>());
	Array<double,3> s1(d(0),d(1),d(2),FortranArray<3>());
	Array<double,3> s2(d(0),d(1),d(2),FortranArray<3>());
	V  = 0;
	D  = 0;
	v1 = 0;
	v2 = 0;
	s1 = 0;
	s2 = 0;

	// cash tensors
	Array<double,3> Cash1(R_C1.begin(), shape(d(0),d(1),d(2)),neverDeleteData,FortranArray<3>());
	Array<double,3> Cash2(R_C2.begin(), shape(d(0),d(1),d(2)),neverDeleteData,FortranArray<3>());

	// savings tensor (just a vector)
	Array<double,1> S( R_S.begin(),R_S.length(),neverDeleteData,FortranArray<1>() );

	// consumption tensor and ctmp
	Array<double,4> Cons1(d(0),d(1),d(2),ns,FortranArray<4>());
	Array<double,4> Cons2(d(0),d(1),d(2),ns,FortranArray<4>());
	Array<double,3> ctmp(d(0),d(1),ns,FortranArray<3>());
	Cons1 = 0;
	Cons2 = 0;

	// full value tensor and tmp
	Array<double,3> w(d(0),d(1),ns,FortranArray<3>());
	Array<double,2> vtmp1(d(0),d(1),FortranArray<2>());
	Array<double,2> vtmp2(d(0),d(1),FortranArray<2>());
	Array<double,2> stmp(d(0),d(1),FortranArray<2>());
	Array<double,2> vplustmp(d(0),d(1),FortranArray<2>());
	w        = 0;
	vtmp1    = 0;
	vtmp2    = 0;
	vplustmp = 0;


	// tensor index descriptors
	firstIndex  i;	
	secondIndex j;  
	thirdIndex  k;	
	fourthIndex l;	

	Cons1 = Cash1(i,j,k) - S(l);
	Cons2 = Cash2(i,j,k) - S(l);

	// loop over time
	// final period: where cons is positive, say utility is log(cash)
	
	for (int t=d(2); t>0; t--){

		// if final period
		if (t==d(2)){

			V(Range::all(),Range::all(),t) = log( Cash1(Range::all(),Range::all(),t) ); 

		} else {
			
			vplustmp = V(Range::all(),Range::all(),t+1);
			
			// option 1
			ctmp     = Cons1(Range::all(),Range::all(),t,Range::all());		// consumption at all savings states in period t

			w        = where(ctmp > 0, log( ctmp(i,j,k) ) + beta * vplustmp(k,j), myNA );
			vtmp1    = max( w, k);
			stmp     = maxIndex( w, k);
			// unnecessary
			v1(Range::all(),Range::all(),t) = vtmp1;
			s1(Range::all(),Range::all(),t) = stmp;
			
			// option 2
			ctmp     = Cons2(Range::all(),Range::all(),t,Range::all());		// consumption at all savings states in period t

			w        = where(ctmp > 0, log( ctmp(i,j,k) ) + beta * vplustmp(k,j), myNA );
			vtmp2    = max( w, k);
			stmp     = maxIndex( w, k);
			// unnecessary
			v2(Range::all(),Range::all(),t) = vtmp2;
			s2(Range::all(),Range::all(),t) = stmp;
		
			// find vmax
			V(Range::all(),Range::all(),t) = where(vtmp1 > vtmp2, vtmp1, vtmp2);
			// find dchoice
			D(Range::all(),Range::all(),t) = where(vtmp1 > vtmp2, 1, 2);

		}

	}

	// map to output objects
	//
	// Maximal value function and discrete choice function
	Rcpp::NumericVector Vout(nState);
	Rcpp::NumericVector Dout(nState);

	Vout = V;
	Dout = D;
	Vout.attr("dim") = Rcpp::IntegerVector::create(d(0),d(1),d(2));
	Dout.attr("dim") = Rcpp::IntegerVector::create(d(0),d(1),d(2));

	// consumption functions
	Rcpp::NumericVector C1_out(Cons1.size());
	Rcpp::NumericVector C2_out(Cons2.size());

	C1_out = Cons1;
	C2_out = Cons2;
	C1_out.attr("dim") = Rcpp::IntegerVector::create(d(0),d(1),d(2),ns);
	C2_out.attr("dim") = Rcpp::IntegerVector::create(d(0),d(1),d(2),ns);
	
	// conditional value functions
	Rcpp::NumericVector v1_out(v1.size());
	Rcpp::NumericVector v2_out(v2.size());

	v1_out = v1;
	v2_out = v2;
	v1_out.attr("dim") = Rcpp::IntegerVector::create(d(0),d(1),d(2));
	v2_out.attr("dim") = Rcpp::IntegerVector::create(d(0),d(1),d(2));

	// savings function
	Rcpp::NumericVector s1_out(s1.size());
	Rcpp::NumericVector s2_out(s2.size());

	s1_out = s1;
	s2_out = s2;
	s1_out.attr("dim") = Rcpp::IntegerVector::create(d(0),d(1),d(2));
	s2_out.attr("dim") = Rcpp::IntegerVector::create(d(0),d(1),d(2));

	// create output list
	Rcpp::List list = Rcpp::List::create( Rcpp::_["V"] = Vout, 
			                              Rcpp::_["D"] = Dout, 
										  Rcpp::_["cons1"] = C1_out,
										  Rcpp::_["cons2"] = C2_out,
										  Rcpp::_["save1"] = s1_out,
										  Rcpp::_["save2"] = s2_out,
										  Rcpp::_["v1"] = v1_out,
										  Rcpp::_["v2"] = v2_out);
    return list;
}   



//' dev3H
//'
//' @examples
//' nA <- 5L; nY <- 3L; nT <- 3L
//' s1 <- data.table(expand.grid(a=seq(0,10,length=nA),y=1:nY,it=1:nT))
//' s1[,cash := a + y + 0.3*it]
//' save <- seq(0,10,length=nA)
//' s3 <- data.table(expand.grid(a=seq(0,10,length=nA),y=1:nY,it=1:nT,save=save))
//' s3[,cons := a + y + 0.3*it - save]
//' dataR <- list( cash1 = s1[["cash"]],
//'                dims = c(nA,nY,nT),
//'                savings = save,
//'                theta = 1.2,beta=0.95,myNA=-99)
//' blitz <- dev3(data=dataR)
//' 
//' ######################################################
//' # Calculating an R solution to this 3-period lifecycle
//' ######################################################
//' 
//' V = array(0,c(nA,nY,nT))
//' V[ , ,3] = s1[it==3,log(cash)]
//' C = s3[,array(cons,c(nA,nY,nT,nA))]
//' W = array(0,c(nA,nY,nT,nA))
//' for (ti in 2:1) {
//'     for (ia in 1:nA) {
//'          for(iy in 1:nY) {
//'              for (ja in 1:nA){
//'                  if (C[ia,iy,ti,ja] > 0){
//' 		     		W[ia,iy,ti,ja] =  log(C[ia,iy,ti,ja])  + dataR$beta*V[ja,iy,ti+1]
//' 			     } else {
//' 			        W[ia,iy,ti,ja] = dataR$myNA
//' 			     }
//' 		     }
//' 			V[ia,iy,ti] = max(W[ia,iy,ti, ])
//'          }
//'      }
//'	}
//' ############################################
//' # comparing R and blitz++ solutions
//' ############################################
//' 
//' print(all.equal(V,blitz$V1))
//' print(all.equal(W[ , ,1, ],blitz$w))
//' print(all.equal(C,blitz$cons))
// [[Rcpp::export]]
Rcpp::List dev3( Rcpp::List data ) {

// current example: v(a,y,t) = max(v1(a,y,t),v2(a,y,t))
// build consumption, evaluate utility


	// R array data
	Rcpp::NumericVector R_C1 = Rcpp::as<Rcpp::NumericVector>(data["cash1"]);
	Rcpp::NumericVector R_S  = Rcpp::as<Rcpp::NumericVector>(data["savings"]);
	Rcpp::IntegerVector d    = Rcpp::as<Rcpp::IntegerVector>(data["dims"]);

	// R parameter data
	double theta = Rcpp::as<double>(data["theta"]);
	double beta  = Rcpp::as<double>(data["beta"]);
	double myNA  = Rcpp::as<double>(data["myNA"]);

	int ns = R_S.size();
	int nState = d(0) * d(1) * d(2);

	// out objects 
	Rcpp::NumericVector Vout(nState);
	Rcpp::NumericVector Dout(nState);

	// conditional value functions
	Array<double,3> V1(d(0),d(1),d(2),FortranArray<3>());
	V1 = 0;

	// cash tensors
	Array<double,3> Cash1(R_C1.begin(), shape(d(0),d(1),d(2)),neverDeleteData,FortranArray<3>());

	// savings tensor (just a vector)
	Array<double,1> S( R_S.begin(),R_S.length(),neverDeleteData,FortranArray<1>() );

	// consumption tensor and ctmp
	Array<double,4> Cons1(d(0),d(1),d(2),ns,FortranArray<4>());
	Array<double,3> ctmp(d(0),d(1),ns,FortranArray<3>());
	Cons1 = 0;

	// full value tensor and tmp
	Array<double,3> w(d(0),d(1),ns,FortranArray<3>());
	Array<double,2> vtmp(d(0),d(1),FortranArray<2>());
	Array<double,2> vplustmp(d(0),d(1),FortranArray<2>());
	w        = 0;
	vtmp     = 0;
	vplustmp = 0;


	// tensor index descriptors
	firstIndex  i;	
	secondIndex j;  
	thirdIndex  k;	
	fourthIndex l;	

	Cons1 = Cash1(i,j,k) - S(l);

	// loop over time
	// final period: where cons is positive, say utility is log(cash)
	
	for (int t=d(2); t>0; t--){

		// if final period
		if (t==d(2)){

			V1(Range::all(),Range::all(),t) = log( Cash1(Range::all(),Range::all(),t) ); 

		} else {

			ctmp     = Cons1(Range::all(),Range::all(),t,Range::all());		// consumption at all savings states in period t
			vplustmp = V1(Range::all(),Range::all(),t+1);

			// where consumption is positive, assign value, 
			// where not, assign NA
			w        = where(ctmp > 0, log( ctmp(i,j,k) ) + beta * vplustmp(k,j), myNA );

			// take max over third dimension of w
			V1(Range::all(),Range::all(),t) = max( w, k);

		}

	}

	// map to output objects
	//
	Rcpp::NumericVector C_out(Cons1.size());

	C_out = Cons1;
	C_out.attr("dim") = Rcpp::IntegerVector::create(d(0),d(1),d(2),ns);
	
	Rcpp::NumericVector Out2(w.size());
	Rcpp::NumericVector Out(V1.size());
	Out = V1;
	Out2 = w;
    
	Out.attr("dim") = Rcpp::IntegerVector::create(d(0),d(1),d(2));
	Out2.attr("dim") = Rcpp::IntegerVector::create(d(0),d(1),ns);

	// create output list
	Rcpp::List list = Rcpp::List::create( Rcpp::_["V1"] = Out, Rcpp::_["w"] = Out2, Rcpp::_["cons"] = C_out);
    return list;
}   




//' dev3
//'
//' @examples
//' nA <- 5L; nY <- 3L; nT <- 3L
//' s1 <- data.table(expand.grid(a=seq(0,10,length=nA),y=1:nY,it=1:nT))
//' s1[,cash := a + y + 0.3*it]
//' save <- seq(0,10,length=nA)
//' s3 <- data.table(expand.grid(a=seq(0,10,length=nA),y=1:nY,it=1:nT,save=save))
//' s3[,cons := a + y + 0.3*it - save]
//' dataR <- list( cash1 = s1[["cash"]],
//'                dims = c(nA,nY,nT),
//'                savings = save,
//'                theta = 1.2,beta=0.95,myNA=-99)
//' blitz <- dev3(data=dataR)
//' 
//' ######################################################
//' # Calculating an R solution to this 3-period lifecycle
//' ######################################################
//' 
//' V = array(0,c(nA,nY,nT))
//' V[ , ,3] = s1[it==3,log(cash)]
//' C = s3[,array(cons,c(nA,nY,nT,nA))]
//' W = array(0,c(nA,nY,nT,nA))
//' for (ti in 2:1) {
//'     for (ia in 1:nA) {
//'          for(iy in 1:nY) {
//'              for (ja in 1:nA){
//'                  if (C[ia,iy,ti,ja] > 0){
//' 		     		W[ia,iy,ti,ja] =  log(C[ia,iy,ti,ja])  + dataR$beta*V[ja,iy,ti+1]
//' 			     } else {
//' 			        W[ia,iy,ti,ja] = dataR$myNA
//' 			     }
//' 		     }
//' 			V[ia,iy,ti] = max(W[ia,iy,ti, ])
//'          }
//'      }
//'	}
//' ############################################
//' # comparing R and blitz++ solutions
//' ############################################
//' 
//' print(all.equal(V,blitz$V1))
//' print(all.equal(W[ , ,1, ],blitz$w))
//' print(all.equal(C,blitz$cons))
// [[Rcpp::export]]
Rcpp::List dev3( Rcpp::List data ) {

// current example: v(a,y,t) = max(v1(a,y,t),v2(a,y,t))
// build consumption, evaluate utility


	// R array data
	Rcpp::NumericVector R_C1 = Rcpp::as<Rcpp::NumericVector>(data["cash1"]);
	Rcpp::NumericVector R_S  = Rcpp::as<Rcpp::NumericVector>(data["savings"]);
	Rcpp::IntegerVector d    = Rcpp::as<Rcpp::IntegerVector>(data["dims"]);

	// R parameter data
	double theta = Rcpp::as<double>(data["theta"]);
	double beta  = Rcpp::as<double>(data["beta"]);
	double myNA  = Rcpp::as<double>(data["myNA"]);

	int ns = R_S.size();
	int nState = d(0) * d(1) * d(2);

	// out objects 
	Rcpp::NumericVector Vout(nState);
	Rcpp::NumericVector Dout(nState);

	// conditional value functions
	Array<double,3> V1(d(0),d(1),d(2),FortranArray<3>());
	V1 = 0;

	// cash tensors
	Array<double,3> Cash1(R_C1.begin(), shape(d(0),d(1),d(2)),neverDeleteData,FortranArray<3>());

	// savings tensor (just a vector)
	Array<double,1> S( R_S.begin(),R_S.length(),neverDeleteData,FortranArray<1>() );

	// consumption tensor and ctmp
	Array<double,4> Cons1(d(0),d(1),d(2),ns,FortranArray<4>());
	Array<double,3> ctmp(d(0),d(1),ns,FortranArray<3>());
	Cons1 = 0;

	// full value tensor and tmp
	Array<double,3> w(d(0),d(1),ns,FortranArray<3>());
	Array<double,2> vtmp(d(0),d(1),FortranArray<2>());
	Array<double,2> vplustmp(d(0),d(1),FortranArray<2>());
	w        = 0;
	vtmp     = 0;
	vplustmp = 0;


	// tensor index descriptors
	firstIndex  i;	
	secondIndex j;  
	thirdIndex  k;	
	fourthIndex l;	

	Cons1 = Cash1(i,j,k) - S(l);

	// loop over time
	// final period: where cons is positive, say utility is log(cash)
	
	for (int t=d(2); t>0; t--){

		// if final period
		if (t==d(2)){

			V1(Range::all(),Range::all(),t) = log( Cash1(Range::all(),Range::all(),t) ); 

		} else {

			ctmp     = Cons1(Range::all(),Range::all(),t,Range::all());		// consumption at all savings states in period t
			vplustmp = V1(Range::all(),Range::all(),t+1);

			// where consumption is positive, assign value, 
			// where not, assign NA
			w        = where(ctmp > 0, log( ctmp(i,j,k) ) + beta * vplustmp(k,j), myNA );

			// take max over third dimension of w
			V1(Range::all(),Range::all(),t) = max( w, k);

		}

	}

	// map to output objects
	//
	Rcpp::NumericVector C_out(Cons1.size());

	C_out = Cons1;
	C_out.attr("dim") = Rcpp::IntegerVector::create(d(0),d(1),d(2),ns);
	
	Rcpp::NumericVector Out2(w.size());
	Rcpp::NumericVector Out(V1.size());
	Out = V1;
	Out2 = w;
    
	Out.attr("dim") = Rcpp::IntegerVector::create(d(0),d(1),d(2));
	Out2.attr("dim") = Rcpp::IntegerVector::create(d(0),d(1),ns);

	// create output list
	Rcpp::List list = Rcpp::List::create( Rcpp::_["V1"] = Out, Rcpp::_["w"] = Out2, Rcpp::_["cons"] = C_out);
    return list;
}   





//' dev2
//'
//' @examples
//' nA <- 5L; nY <- 3L; nT <- 3L
//' s1 <- data.table(expand.grid(a=seq(0,10,length=nA),y=1:nY,it=1:nT))
//' s1[,cash := a + y + 0.3*it]
//' s2 <- copy(s1)
//' s2[,cash := a + y + 0.2*it + 0.09*it^2]
//' save <- seq(0,10,length=nA)
//' dataR <- list( cash1 = s1[["cash"]],
//'                cash2 = s2[["cash"]],
//'                dims = c(nA,nY,nT),
//'                savings = save,
//'                theta = 1.2,beta=0.95,myNA=-99)
//' blitz <- dev2(data=dataR)
//' V1_R = s1[,array(cash,c(nA,nY,nT))]
//' V1_R[ , ,3] = log(V1_R[ , ,3])
//' for (ti in 2:1) {
//'     for (ia in 1:nA) {
//'          for(iy in 1:nY) {
//'				if (V1_R[ia,iy,ti]<0) {
//'					V1_R[ia,iy,ti] <- dataR$myNA
//'				} else {
//'					V1_R[ia,iy,ti] = log(V1_R[ia,iy,ti]) + dataR$beta*V1_R[ia,iy,ti+1]
//'				}
//'          }
//'      }
//' }
//' print(all.equal(V1_R,blitz$V1))
// [[Rcpp::export]]
Rcpp::List dev2( Rcpp::List data ) {

// current example: v(a,y,t) = max(v1(a,y,t),v2(a,y,t))
// build consumption, evaluate utility


	// R array data
	Rcpp::NumericVector R_C1 = Rcpp::as<Rcpp::NumericVector>(data["cash1"]);
	Rcpp::NumericVector R_C2 = Rcpp::as<Rcpp::NumericVector>(data["cash2"]);
	Rcpp::NumericVector R_S  = Rcpp::as<Rcpp::NumericVector>(data["savings"]);
	Rcpp::IntegerVector d    = Rcpp::as<Rcpp::IntegerVector>(data["dims"]);

	// R parameter data
	double theta = Rcpp::as<double>(data["theta"]);
	double beta  = Rcpp::as<double>(data["beta"]);
	double myNA  = Rcpp::as<double>(data["myNA"]);

	int ns = R_S.size();
	int nState = d(0) * d(1) * d(2);

	// out objects 
	Rcpp::NumericVector Vout(nState);
	Rcpp::NumericVector Dout(nState);

	// conditional value functions
	Array<double,3> V1(d(0),d(1),d(2),FortranArray<3>());
	Array<double,3> V2(d(0),d(1),d(2),FortranArray<3>());
	V1 = 0;
	V2 = 0;

	// cash tensors
	Array<double,3> Cash1(R_C1.begin(), shape(d(0),d(1),d(2)),neverDeleteData,FortranArray<3>());
	Array<double,3> Cash2(R_C2.begin(), shape(d(0),d(1),d(2)),neverDeleteData,FortranArray<3>());

	// savings tensor (just a vector)
	Array<double,1> S( R_S.begin(),R_S.length(),neverDeleteData,FortranArray<1>() );

	// consumption tensor
	Array<double,4> Cons1(d(0),d(1),d(2),ns,FortranArray<4>());
	Array<double,4> Cons2(d(0),d(1),d(2),ns,FortranArray<4>());
	Cons1 = 0;
	Cons2 = 0;


	// tensor index descriptors
	firstIndex  i;
	secondIndex j;
	thirdIndex  k;
	fourthIndex l;

	Cons1 = Cash1(i,j,k) - S(l);
	Cons2 = Cash2(i,j,k) - S(l);

	Rcpp::NumericVector Out(Cons1.size());

	// loop over time
	// final period: where cons is positive, say utility is log(cash)
	
	for (int t=d(2); t>0; t--){

		// if final period
		// V(a,y,T) = log( cash(a,y,T) )
		if (t==d(2)){

			V1(Range::all(),Range::all(),t) = log(Cash1(Range::all(),Range::all(),t));

		// else
		// if cash(a,y,t) > 0: V(a,y,t) = log( cash(a,y,t) ) + beta * V(a,y,t+1)
		// if cash(a,y,t) < 0: V(a,y,t) = myNA               + beta * V(a,y,t+1)
		} else {

			V1(Range::all(),Range::all(),t) = where( Cash1(Range::all(),Range::all(),t) > 0, 
													 log(Cash1(Range::all(),Range::all(),t)), 
													 myNA ) 
				                              + beta * V1(Range::all(),Range::all(),t+1);

		}

	}

	Rcpp::NumericVector Out2(V1.size());
	Out2 = V1;

	Out2.attr("dim") = Rcpp::IntegerVector::create(d(0),d(1),d(2));
	Rcpp::List list = Rcpp::List::create( Rcpp::_["V1"] = Out2);
	return list;
}



//' dev1
//'
//' @examples
//' nA <- 5L; nY <- 3L; nT <- 3L
//' s1 <- data.table(expand.grid(a=seq(0,10,length=nA),y=1:nY,it=1:nT))
//' s1[,cash := a + y + 0.3*it]
//' s2 <- copy(s1)
//' s2[,cash := a + y + 0.2*it + 0.09*it^2]
//' save <- seq(0,10,length=nA)
//' dataR <- list( cash1 = s1[["cash"]],
//'                cash2 = s2[["cash"]],
//'                dims = c(nA,nY,nT),
//'                savings = save,
//'                theta = 1.2)
//' blitz <- dev1(data=dataR)
//' print(all.equal(s1[it==3,array(log(cash),c(nA,nY))],blitz$V1[ , ,3]))
// [[Rcpp::export]]
Rcpp::List dev1( Rcpp::List data ) {

// current example: v(a,y,t) = max(v1(a,y,t),v2(a,y,t))
// build consumption, evaluate utility, maximize w.r.t. savings


	// R array data
	Rcpp::NumericVector R_C1 = Rcpp::as<Rcpp::NumericVector>(data["cash1"]);
	Rcpp::NumericVector R_C2 = Rcpp::as<Rcpp::NumericVector>(data["cash2"]);
	Rcpp::NumericVector R_S  = Rcpp::as<Rcpp::NumericVector>(data["savings"]);
	Rcpp::IntegerVector d    = Rcpp::as<Rcpp::IntegerVector>(data["dims"]);

	// R parameter data
	double theta = Rcpp::as<double>(data["theta"]);

	int ns = R_S.size();
	int nState = d(0) * d(1) * d(2);

	// out objects 
	Rcpp::NumericVector Vout(nState);
	Rcpp::NumericVector Dout(nState);

	// conditional value functions
	Array<double,3> V1(d(0),d(1),d(2),FortranArray<3>());
	Array<double,3> V2(d(0),d(1),d(2),FortranArray<3>());
	V1 = 0;
	V2 = 0;

	// temporary value functions (no time index)
	Array<double,2> vtmp(d(0),d(1),FortranArray<2>());
	vtmp = 0;
	

	// cash tensors
	Array<double,3> Cash1(R_C1.begin(), shape(d(0),d(1),d(2)),neverDeleteData,FortranArray<3>());
	Array<double,3> Cash2(R_C2.begin(), shape(d(0),d(1),d(2)),neverDeleteData,FortranArray<3>());

	// savings tensor (just a vector)
	Array<double,1> S( R_S.begin(),R_S.length(),neverDeleteData,FortranArray<1>() );

	// consumption tensor
	Array<double,4> Cons1(d(0),d(1),d(2),ns,FortranArray<4>());
	Array<double,4> Cons2(d(0),d(1),d(2),ns,FortranArray<4>());
	Cons1 = 0;
	Cons2 = 0;


	firstIndex  i;
	secondIndex j;
	thirdIndex  k;
	fourthIndex l;

	Cons1 = Cash1(i,j,k) - S(l);
	Cons2 = Cash2(i,j,k) - S(l);

	Rcpp::NumericVector Out(Cons1.size());

	// loop over time
	// final period: where cons is positive, say utility is log(cash)
	int t = 3;
	vtmp = Cash1(Range::all(),Range::all(),t);

	//V1(Range::all(),Range::all(),t) = where(vtmp > 0, log(vtmp), -99);
	V1(Range::all(),Range::all(),t) = log(vtmp);
	
	// Notice that Rcout prints the elements in col order, 
	// that's why this is wrong.
//	Rcpp::Rcout << "cash( , ,3) :" << endl;
//	Rcpp::Rcout << vtmp  << endl;

//	Rcpp::Rcout << "V1 :" << endl;
//	Rcpp::Rcout << V1  << endl;

	// if we map it to a numeric vector with appropriate dim args, its correct.
	Rcpp::NumericVector Out2(V1.size());
	Out2 = V1;

	Out = Cons1;

	Out.attr("dim") = Rcpp::IntegerVector::create(d(0),d(1),d(2),ns);
	Out2.attr("dim") = Rcpp::IntegerVector::create(d(0),d(1),d(2));
	Rcpp::List list = Rcpp::List::create( Rcpp::_["vmax"] = Out,Rcpp::_["V1"] = Out2);
	return list;
}





//' devC
//'
//' @examples
//' nA <- 5L; nY <- 3L; nT <- 3L
//' s1 <- data.table(expand.grid(a=seq(0,10,length=nA),y=1:nY,it=1:nT))
//' s1[,cash := a + y + 0.3*it]
//' s2 <- copy(s1)
//' s2[,cash := a + y + 0.2*it + 0.09*it^2]
//' save <- seq(0,10,length=nA)
//' dataR <- list( cash1 = s1[["cash"]],
//'                cash2 = s2[["cash"]],
//'                dims = c(nA,nY,nT),
//'                savings = save)
//' blitz <- devC(data=dataR)
//' s3 <- data.table(expand.grid(a=seq(0,10,length=nA),y=1:nY,it=1:nT,save=save))
//' s3[,cons := a + y + 0.3*it - save]
//' print(all.equal(s3[,array(cons,c(nA,nY,nT,nA))],blitz$vmax))
// [[Rcpp::export]]
Rcpp::List devC( Rcpp::List data ) {

// current example: v(a,y,t) = max(v1(a,y,t),v2(a,y,t))


	Rcpp::NumericVector R_C1 = Rcpp::as<Rcpp::NumericVector>(data["cash1"]);
	Rcpp::NumericVector R_C2 = Rcpp::as<Rcpp::NumericVector>(data["cash2"]);
	Rcpp::NumericVector R_S  = Rcpp::as<Rcpp::NumericVector>(data["savings"]);
	Rcpp::IntegerVector d    = Rcpp::as<Rcpp::IntegerVector>(data["dims"]);

	int ns = R_S.size();

	// out objects 
	Rcpp::NumericVector Vout(d(0) * d(1) * d(2));
	Rcpp::NumericVector Dout(d(0) * d(1) * d(2));

	// conditional value functions
	Array<double,3> V1(d(0),d(1),d(2),FortranArray<3>());
	Array<double,3> V2(d(0),d(1),d(2),FortranArray<3>());

	// cash tensors
	Array<double,3> Cash1(R_C1.begin(), shape(d(0),d(1),d(2)),neverDeleteData,FortranArray<3>());
	Array<double,3> Cash2(R_C2.begin(), shape(d(0),d(1),d(2)),neverDeleteData,FortranArray<3>());

	// savings tensor (just a vector)
	Array<double,1> S( R_S.begin(),R_S.length(),neverDeleteData,FortranArray<1>() );

	// consumption tensor
	Array<double,4> Cons1(d(0),d(1),d(2),ns,FortranArray<4>());
	Array<double,4> Cons2(d(0),d(1),d(2),ns,FortranArray<4>());
	Cons1 = 0;
	Cons2 = 0;

	firstIndex  i;
	secondIndex j;
	thirdIndex  k;
	fourthIndex l;

	Cons1 = Cash1(i,j,k) - S(l);
	Cons2 = Cash2(i,j,k) - S(l);

	Rcpp::NumericVector Out(Cons1.size());

	Out = Cons1;

	Out.attr("dim") = Rcpp::IntegerVector::create(d(0),d(1),d(2),ns);
	Rcpp::List list = Rcpp::List::create( Rcpp::_["vmax"] = Out);
	return list;
}
