// Backend C++ (RcppArmadillo) para calculo de matrizes de distancia.
// Expostos apenas via wrappers rnp_* em R. Nao chamar diretamente.
// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
using namespace Rcpp;

// Calcula a matriz de distancias par-a-par (n x n) entre as linhas de X.
// metodo: 1=euclidiana, 2=manhattan, 3=minkowski, 4=canberra, 5=mahalanobis.
// Para mahalanobis, inv_cov deve ser a inversa da matriz de covariancia (p x p);
// para os demais metodos inv_cov e ignorada (pode ser matriz vazia).
// [[Rcpp::export]]
arma::mat dist_pairwise_cpp(const arma::mat& X, int metodo, double p,
                            const arma::mat& inv_cov) {
  const arma::uword n = X.n_rows;
  arma::mat D(n, n, arma::fill::zeros);
  for (arma::uword i = 0; i < n; ++i) {
    for (arma::uword j = i + 1; j < n; ++j) {
      arma::rowvec d = X.row(i) - X.row(j);
      double val = 0.0;
      switch (metodo) {
        case 1: // euclidiana
          val = std::sqrt(arma::accu(arma::square(d)));
          break;
        case 2: // manhattan
          val = arma::accu(arma::abs(d));
          break;
        case 3: // minkowski
          val = std::pow(arma::accu(arma::pow(arma::abs(d), p)), 1.0 / p);
          break;
        case 4: { // canberra
          arma::rowvec num = arma::abs(d);
          arma::rowvec den = arma::abs(X.row(i)) + arma::abs(X.row(j));
          double acc = 0.0;
          for (arma::uword c = 0; c < d.n_elem; ++c) {
            if (den(c) > 0.0) acc += num(c) / den(c);
          }
          val = acc;
          break;
        }
        case 5: { // mahalanobis
          arma::mat q = d * inv_cov * d.t();
          val = std::sqrt(std::max(0.0, q(0, 0)));
          break;
        }
        default:
          Rcpp::stop("Metodo de distancia desconhecido.");
      }
      D(i, j) = val;
      D(j, i) = val;
    }
  }
  return D;
}
