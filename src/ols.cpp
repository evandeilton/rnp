// Backend C++ (RcppArmadillo): ajuste de minimos quadrados ordinarios via QR.
// X deve ja conter a coluna de intercepto (se desejada).
// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
using namespace Rcpp;

// Ajuste OLS estavel via decomposicao QR.
// Retorna coeficientes, valores ajustados, residuos, sigma2, matriz de
// covariancia dos coeficientes (vcov) e graus de liberdade residuais.
// [[Rcpp::export]]
Rcpp::List ols_fit_cpp(const arma::mat& X, const arma::vec& y) {
  const arma::uword n = X.n_rows;
  const arma::uword p = X.n_cols;
  if (n <= p) Rcpp::stop("Numero de observacoes deve exceder o de parametros.");

  arma::mat Q, R;
  if (!arma::qr_econ(Q, R, X)) Rcpp::stop("Falha na decomposicao QR.");
  arma::vec beta = arma::solve(R, Q.t() * y);

  arma::vec ajustados = X * beta;
  arma::vec residuos = y - ajustados;
  const arma::uword gl = n - p;
  double sigma2 = arma::dot(residuos, residuos) / (double) gl;

  // (X'X)^{-1} = R^{-1} R^{-T}
  arma::mat Rinv = arma::inv(arma::trimatu(R));
  arma::mat XtXinv = Rinv * Rinv.t();
  arma::mat vcov = sigma2 * XtXinv;

  return Rcpp::List::create(
    Rcpp::Named("coeficientes") = beta,
    Rcpp::Named("ajustados")    = ajustados,
    Rcpp::Named("residuos")     = residuos,
    Rcpp::Named("sigma2")       = sigma2,
    Rcpp::Named("vcov")         = vcov,
    Rcpp::Named("gl")           = (double) gl
  );
}
