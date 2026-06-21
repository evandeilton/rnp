// Backend C++: utilitarios empiricos rapidos (ECDF, ranks, binning).
// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
using namespace Rcpp;

// Funcao de distribuicao empirica avaliada em 'pontos'.
// Retorna, para cada ponto t, a fracao de x <= t.
// [[Rcpp::export]]
arma::vec ecdf_cpp(const arma::vec& x, const arma::vec& pontos) {
  arma::vec xs = arma::sort(x);
  const arma::uword n = xs.n_elem;
  arma::vec out(pontos.n_elem);
  for (arma::uword i = 0; i < pontos.n_elem; ++i) {
    // numero de elementos <= pontos(i)
    arma::uword cnt = 0;
    // busca binaria: upper_bound
    arma::uword lo = 0, hi = n;
    double t = pontos(i);
    while (lo < hi) {
      arma::uword mid = (lo + hi) / 2;
      if (xs(mid) <= t) lo = mid + 1; else hi = mid;
    }
    cnt = lo;
    out(i) = (double) cnt / (double) n;
  }
  return out;
}

// Ranks medios (com tratamento de empates), base 1.
// [[Rcpp::export]]
arma::vec ranks_cpp(const arma::vec& x) {
  const arma::uword n = x.n_elem;
  arma::uvec ord = arma::sort_index(x);
  arma::vec ranks(n);
  arma::uword i = 0;
  while (i < n) {
    arma::uword k = i;
    while (k + 1 < n && x(ord(k + 1)) == x(ord(i))) ++k;
    double rmean = (double)(i + k) / 2.0 + 1.0;
    for (arma::uword t = i; t <= k; ++t) ranks(ord(t)) = rmean;
    i = k + 1;
  }
  return ranks;
}

// Contagem de frequencias por classe definida por 'breaks' (crescente).
// Intervalos (breaks[j], breaks[j+1]], com a primeira classe fechada a esquerda.
// [[Rcpp::export]]
arma::uvec binning_cpp(const arma::vec& x, const arma::vec& breaks) {
  const arma::uword nb = breaks.n_elem - 1;
  arma::uvec cont(nb, arma::fill::zeros);
  for (arma::uword i = 0; i < x.n_elem; ++i) {
    double v = x(i);
    if (v < breaks(0) || v > breaks(nb)) continue;
    // primeira classe inclui o limite inferior
    if (v == breaks(0)) { cont(0)++; continue; }
    for (arma::uword j = 0; j < nb; ++j) {
      if (v > breaks(j) && v <= breaks(j + 1)) { cont(j)++; break; }
    }
  }
  return cont;
}
