#include <Rcpp.h>
using namespace Rcpp;

double distance(double x1, double y1, double x2, double y2) {
  double d = sqrt((x2 - x1)*(x2 - x1) + (y2 - y1)*(y2 - y1));
  return d;
}

// [[Rcpp::export]]
NumericVector design(DataFrame dt, DataFrame func) {
  NumericVector s(0);
  double d = 0;
  
  NumericVector dt_x = dt["x"];
  NumericVector dt_y = dt["y"];
  NumericVector func_x = func["x"];
  NumericVector func_y = func["y"];
  
  for(int i = 0; i < dt.nrows(); i++) {
    for(int j = 0; j < func.nrows(); j++) {
      d = distance(dt_x[i], dt_y[i], func_x[j], func_y[j]);
      if(d < 0.045) {
        s.push_back(i + 1);
        break;
      }
    }
  }
  
  return s;
}
