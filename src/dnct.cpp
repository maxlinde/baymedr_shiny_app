// [[Rcpp::depends(BH)]]

#include <Rcpp.h>
#include <boost/math/distributions/non_central_t.hpp>

using namespace Rcpp;
using namespace boost::math;
using namespace std;

// [[Rcpp::export]]
NumericVector dnct(const double x, const double df, const NumericVector ncp) {
    int n = ncp.size();
    NumericVector out(n);
    for(int i = 0; i < n; ++i) {
        non_central_t dist(df, ncp[i]);
        out[i] = pdf(dist, x);
    }
    return out;
}
