#include <Rcpp.h>
using namespace Rcpp;
#include <iostream>
using namespace std;

// function to be called from R
// [[Rcpp::export]]
NumericMatrix make_burst(int iter, int layers) {

  NumericMatrix points(iter, 3); // initially zero
  NumericMatrix coeffs(9, layers);

  // set coefficients
  for(int i = 0; i < 9; i++) {
    for(int j = 0; j < layers; j++) {
      coeffs(i,j) = R::runif(-1,1);
    }
  }

  // initial values
  points(0, 0) = R::runif(-1, 1);
  points(0, 1) = R::runif(-1, 1);
  points(0, 2) = R::runif(-1, 1);

  // iterate
  int r;
  int f;
  double x;
  double y;
  double z;
  double s;
  double theta;

  double u1;
  double u2;
  double u3;

  const double pi = 3.1415926535;

  for(int t = 1; t < iter; t++) {

    r = rand() % layers; // which affine transform to use?
    f = rand() % 2;      // which variant function to use?

    // co-ordinates after random transform
    x = coeffs(0, r) * points(t-1, 0) + coeffs(1, r) * points(t-1, 1) + coeffs(2, r);
    y = coeffs(3, r) * points(t-1, 0) + coeffs(4, r) * points(t-1, 1) + coeffs(5, r);
    z = coeffs(6, r) * points(t-1, 0) + coeffs(7, r) * points(t-1, 1) + coeffs(8, r);

    if(f == 0) {
      s = 1;
      theta = atan(x/y);
      x = s * pow(cos(theta), 2) * sin(x);
      y = s * pow(sin(theta), 2) * sin(y);
      z = s * sin(z);

    } else {
      s = 2;
      theta = atan(x/y);
      x = s * pow(cos(theta), 2) * sin(x);
      y = s * pow(sin(theta), 2) * sin(y);
      z = s * sin(z);
    }

    // store results
    points(t, 0) = x;
    points(t, 1) = y;
    points(t, 2) = (z + points(t-1, 2))/2;

  }

  return points;
}


