// Backend C++ (RcppArmadillo): regressao robusta por minimos quadrados
// reponderados iterativamente (IRLS) com funcoes psi de Huber e bisquare.
// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
using namespace Rcpp;

// Pesos de Huber: w = 1 se |u|<=c, c/|u| caso contrario.
static arma::vec pesos_huber(const arma::vec& u, double c) {
  arma::vec w(u.n_elem, arma::fill::ones);
  for (arma::uword i = 0; i < u.n_elem; ++i) {
    double a = std::abs(u(i));
    if (a > c && a > 0) w(i) = c / a;
  }
  return w;
}

// Pesos bisquare (Tukey): w = (1-(u/c)^2)^2 se |u|<=c, 0 caso contrario.
static arma::vec pesos_bisquare(const arma::vec& u, double c) {
  arma::vec w(u.n_elem, arma::fill::zeros);
  for (arma::uword i = 0; i < u.n_elem; ++i) {
    double a = std::abs(u(i));
    if (a <= c) { double t = 1.0 - (a / c) * (a / c); w(i) = t * t; }
  }
  return w;
}

// IRLS robusto. X inclui intercepto. psi_tipo: 0=huber, 1=bisquare.
// Escala estimada pelo MAD a cada iteracao. Retorna coeficientes, pesos finais
// e numero de iteracoes.
// [[Rcpp::export]]
Rcpp::List irls_cpp(const arma::mat& X, const arma::vec& y, int psi_tipo,
                    double c_tuning, int max_iter, double tol) {
  const arma::uword n = X.n_rows;
  // inicio: OLS
  arma::vec beta = arma::solve(X, y);
  arma::vec beta_old = beta;
  int it = 0;
  arma::vec w(n, arma::fill::ones);
  for (it = 0; it < max_iter; ++it) {
    arma::vec res = y - X * beta;
    double mad = arma::median(arma::abs(res - arma::median(res))) / 0.6745;
    if (mad < 1e-12) mad = 1e-12;
    arma::vec u = res / mad;
    w = (psi_tipo == 1) ? pesos_bisquare(u, c_tuning) : pesos_huber(u, c_tuning);
    // WLS: beta = (X' W X)^{-1} X' W y
    arma::mat Xw = X.each_col() % arma::sqrt(w);
    arma::vec yw = y % arma::sqrt(w);
    beta = arma::solve(Xw.t() * Xw, Xw.t() * yw);
    if (arma::norm(beta - beta_old, 2) < tol) { ++it; break; }
    beta_old = beta;
  }
  return Rcpp::List::create(
    Rcpp::Named("coeficientes") = beta,
    Rcpp::Named("pesos")        = w,
    Rcpp::Named("iteracoes")    = it
  );
}
