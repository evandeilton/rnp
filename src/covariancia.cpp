// Backend C++ (RcppArmadillo): matrizes de covariancia e correlacao.
// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
using namespace Rcpp;

// Matriz de covariancia amostral (divisor n-1) das colunas de X.
// [[Rcpp::export]]
arma::mat cov_cpp(const arma::mat& X) {
  return arma::cov(X); // arma usa divisor n-1 por padrao
}

// Matriz de correlacao das colunas de X.
// metodo: 1=pearson, 2=spearman (correlacao de Pearson sobre os ranks).
// [[Rcpp::export]]
arma::mat cor_cpp(const arma::mat& X, int metodo) {
  if (metodo == 2) {
    arma::mat R(X.n_rows, X.n_cols);
    for (arma::uword j = 0; j < X.n_cols; ++j) {
      arma::vec col = X.col(j);
      arma::uvec ord = arma::sort_index(col);
      arma::vec ranks(col.n_elem);
      // ranks medios para empates
      arma::uword i = 0;
      while (i < col.n_elem) {
        arma::uword k = i;
        while (k + 1 < col.n_elem && col(ord(k + 1)) == col(ord(i))) ++k;
        double rmean = (double)(i + k) / 2.0 + 1.0; // rank base 1
        for (arma::uword t = i; t <= k; ++t) ranks(ord(t)) = rmean;
        i = k + 1;
      }
      R.col(j) = ranks;
    }
    return arma::cor(R);
  }
  return arma::cor(X);
}
