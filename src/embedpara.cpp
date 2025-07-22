// [[Rcpp::depends(RcppParallel)]]
#include <Rcpp.h>
#include <RcppParallel.h>

using namespace Rcpp;
using namespace RcppParallel;

struct EmbedWorker : public Worker {
    const RVector<double> x;
    const int order;
    RMatrix<double> result;

    EmbedWorker(const NumericVector x, const int order, NumericMatrix result)
        : x(x), order(order), result(result) {}

    void operator()(std::size_t begin, std::size_t end) {
        for (std::size_t i = begin; i < end; ++i) {
            for (int j = 0; j < order; ++j) {
                result(i, j) = x[i + order - j - 1];
            }
        }
    }
};

//' C++ version of embed function
//'
//' @param x A numeric vector.
//' @param dimension A numeric.
//' @param numCores A numeric. A number of cores
//' @export
// [[Rcpp::export]]
NumericMatrix embedParallelCpp(NumericVector x, int dimension, int numCores) {
    int n = x.size();
    NumericMatrix result(n - dimension + 1, dimension);

    EmbedWorker worker(x, dimension, result);
    parallelFor(0, n - dimension + 1, worker, numCores);

    return result;
}
