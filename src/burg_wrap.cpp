// src/burg_wrap.cpp
#include <Rcpp.h>
extern "C" void burg(int n, const double* x, int pmax, double* coefs, double* var1, double* var2);

// [[Rcpp::export(name = "C_Burg_beacon")]]
SEXP C_Burg_beacon(SEXP xS, SEXP pS) {
    Rcpp::NumericVector x(xS);
    int p = Rcpp::as<int>(pS);
    int n = x.size();
    if (n < 2) Rcpp::stop("x must have length >= 2");
    if (p < 1 || p >= n) Rcpp::stop("order.max must be in [1, length(x)-1]");
    
    Rcpp::NumericVector coefs((size_t)p * (size_t)p);
    Rcpp::NumericVector var1(p + 1), var2(p + 1);
    
    burg(n, REAL(x), p, REAL(coefs), REAL(var1), REAL(var2));
    
    return Rcpp::List::create(coefs, var1, var2);
}