// Backend C++ (RcppArmadillo): indice de silhueta para avaliacao de cluster.
// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
using namespace Rcpp;

// Silhueta de cada observacao a partir da matriz de distancias D (n x n) e do
// vetor de clusters (rotulos inteiros). s(i) = (b - a) / max(a, b), com
// a = dissimilaridade media ao proprio cluster e b = menor dissimilaridade
// media a um cluster vizinho.
// [[Rcpp::export]]
arma::vec silhueta_cpp(const arma::mat& D, const arma::ivec& clusters) {
  const arma::uword n = D.n_rows;
  arma::ivec labels = arma::unique(clusters);
  const arma::uword K = labels.n_elem;
  arma::vec s(n, arma::fill::zeros);
  for (arma::uword i = 0; i < n; ++i) {
    int ci = clusters(i);
    double a = 0.0; arma::uword na = 0;
    arma::vec mediaOutros(K); mediaOutros.fill(arma::datum::inf);
    for (arma::uword k = 0; k < K; ++k) {
      int lab = labels(k);
      double soma = 0.0; arma::uword cnt = 0;
      for (arma::uword j = 0; j < n; ++j) {
        if (clusters(j) != lab) continue;
        if (lab == ci && j == i) continue; // exclui o proprio ponto
        soma += D(i, j); ++cnt;
      }
      if (lab == ci) {
        if (cnt > 0) { a = soma / cnt; na = cnt; }
      } else if (cnt > 0) {
        mediaOutros(k) = soma / cnt;
      }
    }
    if (na == 0) { s(i) = 0.0; continue; } // cluster unitario
    double b = mediaOutros.min();
    double denom = std::max(a, b);
    s(i) = (denom > 0) ? (b - a) / denom : 0.0;
  }
  return s;
}
