#define ARMA_64BIT_WORD
#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;

// Create assignments as before
// [[Rcpp::export]]
NumericVector create_assignments(const NumericVector& degrees, double threshold) {
  int n = degrees.size();
  NumericVector assignments(n);
  for (int i = 0; i < n; i++) {
    assignments[i] = (degrees[i] >= threshold) ? 1.0 : 0.0;
  }
  return assignments;
}

// Calculate correlation using only core-core edges
// [[Rcpp::export]]
List calculate_correlation_sparse_cpp(const arma::sp_mat& A, const NumericVector& assignments) {
  int n = A.n_rows;
  int core_count = 0;
  for (int i = 0; i < n; i++) {
    if (assignments[i] == 1.0)
      core_count++;
  }
  // denom_E equals number of off-diagonal core-core pairs
  double denom_E = core_count * (core_count - 1);
  double numerator = 0.0;
  double denom_A = 0.0;
  
  // Iterate over non-zero entries
  for (arma::sp_mat::const_iterator it = A.begin(); it != A.end(); ++it) {
    int i = it.row();
    int j = it.col();
    if (i == j) continue;
    // Only consider edge if both nodes are core
    if (assignments[i] == 1.0 && assignments[j] == 1.0) {
      double w = *it;
      numerator += w;
      denom_A += w * w;
    }
  }
  
  double correlation = (denom_E > 0.0 && denom_A > 0.0) ?
  numerator / std::sqrt(denom_E * denom_A) : R_NegInf;
  
  return List::create(
    Named("correlation") = correlation,
    Named("numerator") = numerator,
    Named("denom_E") = denom_E,
    Named("denom_A") = denom_A,
    Named("core_count") = core_count
  );
}

// Process threshold using the sparse version
// [[Rcpp::export]]
List process_threshold_sparse_cpp(const arma::sp_mat& A,
                                  const NumericVector& degrees,
                                  double threshold) {
  NumericVector assignments = create_assignments(degrees, threshold);
  List result = calculate_correlation_sparse_cpp(A, assignments);
  result["assignments"] = assignments;
  return result;
}
