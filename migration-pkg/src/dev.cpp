



// developments

#include <blitz/array.h>
#include <Rcpp.h>

using namespace blitz;


//' dev3
//'
//' @examples
//' nA <- 5L; nY <- 3L; nT <- 3L
//' s1 <- data.table(expand.grid(a=seq(0,10,length=nA),y=1:nY,it=1:nT))
//' s1[,cash := a + y + 0.3*it]
//' s2 <- copy(s1)
//' s2[,cash := a + y + 0.2*it + 0.09*it^2]
//' s3 <- data.table(expand.grid(a=seq(0,10,length=nA),y=1:nY,it=1:(nT-1),save=save))
//' s3[,cons := a + y + 0.3*it - save]
//' save <- seq(0,10,length=nA)
//' dataR <- list( cash1 = s1[["cash"]],
//'                cash2 = s2[["cash"]],
//'                dims = c(nA,nY,nT),
//'                savings = save,
//'                theta = 1.2,beta=0.95,myNA=-99)
//' blitz <- dev3(data=dataR)
//' V1_R = array(0,c(nA,nY,nT))
//' V1_R[ , ,3] = s1[it==3,log(cash)]
//' C = s3[,array(nA,nY,nT,nA)]
//' W = rep(0,nA)
//' for (ti in 2:1) {
//'     for (ia in 1:nA) {
//'          for(iy in 1:nY) {
//'              for (ja in 1:nA){
//'                  if (C[ia,iy,ti,ja]<0) {
//'					  	V1_R[ia,iy,ti] <- dataR$myNA
//'                  } else {
//'                     W[ia] = log( C[ia,iy,ti,ja] ) + data$R*beta*V1_R[ja,iy,ti+1]
//'					    V1_R[ia,iy,ti] = max(W);
//'				    }
//'				}
//'          }
//'      }
//' }
//' print(all.equal(V1_R,blitz$V1))
// [[Rcpp::export]]
Rcpp::List dev3( Rcpp::List data ) {

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

	// consumption tensor and ctmp
	Array<double,4> Cons1(d(0),d(1),d(2),ns,FortranArray<4>());
	Array<double,4> Cons2(d(0),d(1),d(2),ns,FortranArray<4>());
	Array<double,3> ctmp(d(0),d(1),ns,FortranArray<3>());
	Cons1 = 0;
	Cons2 = 0;

	// full value tensor and tmp
	Array<double,3> w(d(0),d(1),ns,FortranArray<3>());
	Array<double,2> vtmp(d(0),d(1),FortranArray<2>());
	Array<double,2> vplustmp(d(0),d(1),FortranArray<2>());
	w = 0;
	vtmp = 0;
	vplustmp = 0;


	// tensor index descriptors
	firstIndex  i;	// assets
	secondIndex j;  // income
	thirdIndex  k;	// time
	fourthIndex l;	// savings

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

		} else {
			ctmp      = Cons1(Range::all(),Range::all(),t,Range::all());		// consumption at all savings states in period t
			vplustmp  = V1(Range::all(),Range::all(),t+1);
			w  = log( ctmp(i,j,k) ) + beta * vplustmp(k,j);
			V1(Range::all(),Range::all(),t) = max( w, k);
			Rprintf("V1 in t = %d\n",t );
			Rcpp::Rcout << V1 << endl;
			//vtmp(i,j) = max( w(i,j,k) , k );

			//V1(Range::all(),Range::all(),t) = vtmp;

		}

	}

	Rcpp::NumericVector Out2(V1.size());
	Out2 = V1;

	Out2.attr("dim") = Rcpp::IntegerVector::create(d(0),d(1),d(2));
	Rcpp::List list = Rcpp::List::create( Rcpp::_["V1"] = Out2);
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
	Rcpp::Rcout << "cash( , ,3) :" << endl;
	Rcpp::Rcout << vtmp  << endl;

	Rcpp::Rcout << "V1 :" << endl;
	Rcpp::Rcout << V1  << endl;

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

	Rprintf("value of S.size() : %d\n",ns);

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
