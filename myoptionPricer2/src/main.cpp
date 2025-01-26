#include<Rcpp.h>
#include<vector>
#include<ctime>
#include<cstdlib>
#include"AsianOption.h"
#include"getVecMean.h"
#include"getVecStdDev.h"
#include"EuropeanOption.h"
using namespace Rcpp;
using std::vector;

// [[Rcpp::export]]
double getEuropeanUpAndInCallPrice(
    int nInt,
    double Strike,
    double Spot,
    double Vol,
    double Rfr,
    double Expiry,
    double barrier,
    int nReps = 1000){

  // set the seed
  srand( time(NULL) );

  // create a new instance of a class
  EuropeanOption myEuropean(nInt, Strike, Spot, Vol, Rfr, Expiry,barrier);

  // call the method to get option price
  double price = myEuropean.getEuropeanUpAndInCallPrice(nReps);

  // return option price
  return price;
}
