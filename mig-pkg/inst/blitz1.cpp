


// blitztest inside package


#include <blitz/array.h>
#include <Rcpp.h>

using namespace blitz;

//' Blitz mini discrete choice model
//'
//' in this example we map actually arrays from R to C++
//' Arrays are vectors with a dimension attribute
//' @example examples/example-blitz1.r
// [[Rcpp::export]]
Rcpp::List dev_blitz1( Rcpp::List data ) {

	Rcpp::NumericVector A  = Rcpp::as<Rcpp::NumericVector>(data["option1"]);
	Rcpp::NumericVector A2 = Rcpp::as<Rcpp::NumericVector>(data["option2"]);
	Rcpp::NumericVector S  = Rcpp::as<Rcpp::NumericVector>(data["savings"]);

	// read dimension attribute of IN arrays
	Rcpp::IntegerVector d1 = A.attr("dim");
	Rcpp::IntegerVector d2 = A2.attr("dim");

	// out objects are defined on dims 1 and 2
	Rcpp::NumericVector Vout(d1(0) * d1(1));
	Rcpp::NumericVector Dout(d1(0) * d1(1));

	// create 2 blitz cubes B1 and B2
	// Note that by default, Blitz arrays are row-major. 
	// From R and Fortran, we get column major arrays. so use option "fortranArray" to define storage order.
	Array<double,3> B1(A.begin(), shape(d1(0),d1(1),d1(2)), neverDeleteData, FortranArray<3>());
	Array<double,3> B2(A2.begin(), shape(d2(0),d2(1),d2(2)), neverDeleteData, FortranArray<3>());

	// create a blitz savings cube
	// has same dimensions as B1
	Array<double,3> SS(S.begin(), shape(d1(0),d1(1),d1(2)), neverDeleteData, FortranArray<3>());

	// declare blitz dimension descriptors
	firstIndex  i;
	secondIndex  j;
	thirdIndex  k;

	// compute U - savings for each option
	B1 = B1 - SS;
	B2 = B2 - SS;

	// create to "value functions" to store values after maximizing
	// over the third dimension of B1 and B2
	Array<double,2> V1(d1(0),d1(1),FortranArray<2>());
	Array<double,2> V2(d1(0),d1(1),FortranArray<2>());

	// find maximal value
	V1 = max(B1,k);
	V2 = max(B2,k);

	// map as blitz arrays
	// onto out vectors
	Array<double,2> Vmax(Vout.begin(), shape(d1(0),d1(1)), neverDeleteData,FortranArray<2>());
	Array<double,2> Dmax(Dout.begin(), shape(d2(0),d2(1)), neverDeleteData,FortranArray<2>());

	// take discrete choice
	Vmax = where(V1 > V2, V1, V2);
	Dmax = where(V1 > V2, 1, 2);

	// copy to NumericVectors to return values
	Vout = Vmax;
	Dout = Dmax;

	// add dimension attributes
	// http://gallery.rcpp.org/articles/accessing-xts-api/
	Vout.attr("dim") = Rcpp::IntegerVector::create(d1(0),d1(1));
	Dout.attr("dim") = Rcpp::IntegerVector::create(d1(0),d1(1));
	Rcpp::List list = Rcpp::List::create( Rcpp::_["vmax"] = Vout, Rcpp::_["dchoice"] = Dout);
	return list;
}




//' Blitz mini discrete choice model 2
//'
//' in this example we map vectors to C
//' @example examples/example-blitz2.r
// [[Rcpp::export]]
Rcpp::List dev_blitz2( Rcpp::List data ) {

	Rcpp::NumericVector A  = Rcpp::as<Rcpp::NumericVector>(data["option1"]);
	Rcpp::NumericVector A2 = Rcpp::as<Rcpp::NumericVector>(data["option2"]);
	Rcpp::NumericVector S  = Rcpp::as<Rcpp::NumericVector>(data["savings"]);
	Rcpp::IntegerVector d  = Rcpp::as<Rcpp::IntegerVector>(data["dims"]);

	Rcpp::Rcout << d(0) << std::endl;
	Rcpp::Rcout << d(1) << std::endl;
	Rcpp::Rcout << d(2) << std::endl;
	// out objects are defined on dims 1 and 2
	Rcpp::NumericVector Vout(d(0) * d(1));
	Rcpp::NumericVector Dout(d(0) * d(1));
	Rcpp::NumericVector V1out(d(0) * d(1));
	Rcpp::NumericVector V2out(d(0) * d(1));

	 //create 2 blitz cubes B1 and B2
	Array<double,3> B1(A.begin(), shape(d(0),d(1),d(2)), neverDeleteData );
	Array<double,3> B2(A2.begin(), shape(d(0),d(1),d(2)), neverDeleteData);

	 //create a blitz savings cube
	 //has same dimensions as B1
	Array<double,3> SS(S.begin(), shape(d(0),d(1),d(2)), neverDeleteData);

	 //declare blitz dimension descriptors
	thirdIndex  k;

	 //compute U - savings for each option
	B1 = B1 - SS;
	B2 = B2 - SS;

	// create to "value functions" to store values after maximizing
	// over the third dimension of B1 and B2
	Array<double,2> V1(d(0),d(1));
	Array<double,2> V2(d(0),d(1));

	// find maximal value
	V1 = max(B1,k);
	V2 = max(B2,k);

	V1out = V1;
	V2out = V2;

	//// take discrete choice
	Array<double,2> Vmax(shape(d(0),d(1)));
	Array<double,2> Dmax(shape(d(0),d(1)));
	Vmax = 0;
	Dmax = 0;
	Vmax = where(V1 > V2, V1, V2) ;
	Dmax = where(V1 > V2, 1, 2) ;

	//// copy to NumericVectors to return values
	Vout = Vmax;
	Dout = Dmax;

	// add dimension attributes
	// http://gallery.rcpp.org/articles/accessing-xts-api/
	//Vout.attr("dim") = Rcpp::IntegerVector::create(d(0),d(1));
	//Dout.attr("dim") = Rcpp::IntegerVector::create(d(0),d(1));
	//Rcpp::List list = Rcpp::List::create( Rcpp::_["vmax"] = 1);
	Rcpp::List list = Rcpp::List::create( Rcpp::_["vmax"] = Vout, Rcpp::_["dchoice"] = Dout, Rcpp::_["V1"] = V1out, Rcpp::_["V2"] = V2out );
	return list;
}
