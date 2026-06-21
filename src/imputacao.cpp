// Backend C++ (RcppArmadillo): imputacao por k vizinhos mais proximos.
// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
using namespace Rcpp;

// Imputa NA em X usando a media dos k vizinhos mais proximos (distancia
// euclidiana sobre as colunas observadas em comum). NA representado por NaN.
// Retorna a matriz com os NA preenchidos.
// [[Rcpp::export]]
arma::mat knn_imputa_cpp(arma::mat X, int k) {
  const arma::uword n = X.n_rows;
  const arma::uword p = X.n_cols;
  arma::mat out = X;
  for (arma::uword i = 0; i < n; ++i) {
    for (arma::uword j = 0; j < p; ++j) {
      if (!std::isnan(X(i, j))) continue;
      // calcula distancias de i a todas as linhas com valor observado em j
      std::vector<std::pair<double,double>> viz; // (dist, valor)
      for (arma::uword r = 0; r < n; ++r) {
        if (r == i || std::isnan(X(r, j))) continue;
        double soma = 0.0; arma::uword comuns = 0;
        for (arma::uword c = 0; c < p; ++c) {
          if (c == j) continue;
          if (!std::isnan(X(i, c)) && !std::isnan(X(r, c))) {
            double dd = X(i, c) - X(r, c);
            soma += dd * dd; ++comuns;
          }
        }
        if (comuns == 0) continue;
        double dist = std::sqrt(soma / comuns); // distancia media normalizada
        viz.push_back(std::make_pair(dist, X(r, j)));
      }
      if (viz.empty()) continue;
      std::sort(viz.begin(), viz.end(),
                [](const std::pair<double,double>& a,
                   const std::pair<double,double>& b){ return a.first < b.first; });
      arma::uword kk = std::min((arma::uword) k, (arma::uword) viz.size());
      double acc = 0.0;
      for (arma::uword t = 0; t < kk; ++t) acc += viz[t].second;
      out(i, j) = acc / kk;
    }
  }
  return out;
}
