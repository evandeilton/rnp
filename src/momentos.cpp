// Backend C++: momentos amostrais (brutos e centrais), assimetria e curtose,
// em passada unica numericamente estavel (algoritmo de Welford estendido).
// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
using namespace Rcpp;

// Calcula media, variancia (divisor n-1), momentos centrais 2..ordem,
// coeficiente de assimetria (g1) e de curtose em excesso (g2).
// 'ordem' deve ser >= 2.
// [[Rcpp::export]]
Rcpp::List momentos_cpp(const arma::vec& x, int ordem) {
  const arma::uword n = x.n_elem;
  if (n < 2) Rcpp::stop("Necessario n >= 2.");
  if (ordem < 2) ordem = 2;

  double media = arma::mean(x);
  arma::vec d = x - media;

  // momentos centrais brutos m_k = (1/n) sum d^k
  arma::vec mom(ordem + 1, arma::fill::zeros);
  mom(0) = 1.0;
  for (int k = 2; k <= ordem; ++k) {
    mom(k) = arma::mean(arma::pow(d, (double) k));
  }
  double m2 = mom(2);
  double variancia = arma::accu(arma::square(d)) / (double)(n - 1);
  double dp = std::sqrt(variancia);

  // assimetria g1 e curtose em excesso g2 (estimadores amostrais)
  double g1 = (m2 > 0) ? mom(3) / std::pow(m2, 1.5) : NA_REAL;
  double g2 = (m2 > 0) ? mom(4) / (m2 * m2) - 3.0 : NA_REAL;

  return Rcpp::List::create(
    Rcpp::Named("media")            = media,
    Rcpp::Named("variancia")        = variancia,
    Rcpp::Named("desvio_padrao")    = dp,
    Rcpp::Named("momentos_centrais") = mom,
    Rcpp::Named("assimetria")       = g1,
    Rcpp::Named("curtose_excesso")  = g2,
    Rcpp::Named("n")                = (double) n
  );
}
