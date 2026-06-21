// Backend C++ (RcppArmadillo): regressao regularizada (ridge e elastic net).
// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
using namespace Rcpp;

// Ridge com solucao fechada. X inclui a coluna de intercepto na 1a posicao,
// que NAO e penalizada. beta = (X'X + lambda D)^{-1} X'y, D = diag(0,1,...,1).
// [[Rcpp::export]]
arma::vec ridge_cpp(const arma::mat& X, const arma::vec& y, double lambda) {
  const arma::uword p = X.n_cols;
  arma::mat D = arma::eye<arma::mat>(p, p);
  D(0, 0) = 0.0; // nao penaliza o intercepto
  arma::mat A = X.t() * X + lambda * D;
  return arma::solve(A, X.t() * y);
}

static inline double soft_threshold(double z, double g) {
  if (z > g) return z - g;
  if (z < -g) return z + g;
  return 0.0;
}

// Elastic net por coordinate descent. Espera X JA padronizada (cada coluna com
// (1/n) sum x_ij^2 = 1, sem coluna de intercepto) e y JA centrado.
// Objetivo: (1/2n)||y - Xb||^2 + lambda(alpha||b||_1 + (1-alpha)/2 ||b||^2).
// Retorna os coeficientes na escala padronizada. lasso = alpha 1; ridge = alpha 0.
// [[Rcpp::export]]
arma::vec enet_cd_cpp(const arma::mat& X, const arma::vec& y, double lambda,
                      double alpha, int max_iter, double tol) {
  const arma::uword n = X.n_rows;
  const arma::uword p = X.n_cols;
  arma::vec beta(p, arma::fill::zeros);
  arma::vec r = y; // residuo = y - X beta (beta = 0 no inicio)
  const double denom = 1.0 + lambda * (1.0 - alpha);
  for (int it = 0; it < max_iter; ++it) {
    double maxdelta = 0.0;
    for (arma::uword j = 0; j < p; ++j) {
      arma::vec xj = X.col(j);
      // z_j = (1/n) x_j' (r + x_j beta_j) = (1/n) x_j' r + beta_j
      double zj = arma::dot(xj, r) / (double) n + beta(j);
      double bj_new = soft_threshold(zj, lambda * alpha) / denom;
      double delta = bj_new - beta(j);
      if (delta != 0.0) {
        r -= xj * delta;           // atualiza residuo
        beta(j) = bj_new;
        if (std::abs(delta) > maxdelta) maxdelta = std::abs(delta);
      }
    }
    if (maxdelta < tol) break;
  }
  return beta;
}
