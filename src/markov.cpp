// Backend C++ (RcppArmadillo) para cadeias de Markov de tempo discreto.
// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
using namespace Rcpp;

// Matriz de transicao em n passos: P^n (por exponenciacao por quadrados).
// [[Rcpp::export]]
arma::mat markov_npassos_cpp(const arma::mat& P, int n) {
  const arma::uword d = P.n_rows;
  if (P.n_cols != d) Rcpp::stop("P deve ser quadrada.");
  if (n < 0) Rcpp::stop("n deve ser >= 0.");
  arma::mat resultado = arma::eye<arma::mat>(d, d);
  arma::mat base = P;
  int e = n;
  while (e > 0) {
    if (e & 1) resultado = resultado * base;
    base = base * base;
    e >>= 1;
  }
  return resultado;
}

// Distribuicao estacionaria pi tal que pi P = pi, sum(pi) = 1.
// Obtida pelo autovetor a esquerda associado ao autovalor 1 (autovetor a
// direita de P^T), tomando a parte real e normalizando.
// [[Rcpp::export]]
arma::vec markov_estacionaria_cpp(const arma::mat& P) {
  const arma::uword d = P.n_rows;
  if (P.n_cols != d) Rcpp::stop("P deve ser quadrada.");
  arma::cx_vec autoval;
  arma::cx_mat autovec;
  arma::eig_gen(autoval, autovec, P.t());
  // localiza o autovalor mais proximo de 1
  arma::uword idx = 0;
  double melhor = std::abs(autoval(0) - std::complex<double>(1.0, 0.0));
  for (arma::uword i = 1; i < autoval.n_elem; ++i) {
    double d_i = std::abs(autoval(i) - std::complex<double>(1.0, 0.0));
    if (d_i < melhor) { melhor = d_i; idx = i; }
  }
  arma::vec v = arma::real(autovec.col(idx));
  double s = arma::accu(v);
  if (std::abs(s) < 1e-300) Rcpp::stop("Distribuicao estacionaria indefinida.");
  v = v / s;          // normaliza para somar 1
  return arma::abs(v); // remove sinal residual de pequenas componentes
}
