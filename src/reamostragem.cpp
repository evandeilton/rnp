// Backend C++ (RcppArmadillo): reamostragem (bootstrap, jackknife, permutacao).
// Estatisticas internas por id: 0=media, 1=mediana, 2=desvio-padrao, 3=variancia.
// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
using namespace Rcpp;

static double estatistica_id(const arma::vec& v, int id) {
  switch (id) {
    case 0: return arma::mean(v);
    case 1: return arma::median(v);
    case 2: return arma::stddev(v);     // divisor n-1
    case 3: return arma::var(v);        // divisor n-1
    default: Rcpp::stop("Estatistica desconhecida (use 0=media,1=mediana,2=dp,3=var).");
  }
  return 0.0;
}

// B replicas bootstrap (amostragem com reposicao) da estatistica indicada.
// [[Rcpp::export]]
arma::vec bootstrap_stat_cpp(const arma::vec& x, int B, int stat_id) {
  const arma::uword n = x.n_elem;
  if (n < 1) Rcpp::stop("x vazio.");
  arma::vec rep(B);
  Rcpp::RNGScope scope;                 // respeita set.seed() do R
  arma::vec amostra(n);
  for (int b = 0; b < B; ++b) {
    for (arma::uword i = 0; i < n; ++i) {
      arma::uword idx = (arma::uword) std::floor(R::unif_rand() * n);
      if (idx >= n) idx = n - 1;
      amostra(i) = x(idx);
    }
    rep(b) = estatistica_id(amostra, stat_id);
  }
  return rep;
}

// n replicas jackknife (deixa-um-de-fora) da estatistica indicada.
// [[Rcpp::export]]
arma::vec jackknife_stat_cpp(const arma::vec& x, int stat_id) {
  const arma::uword n = x.n_elem;
  if (n < 2) Rcpp::stop("Jackknife requer n >= 2.");
  arma::vec rep(n);
  for (arma::uword j = 0; j < n; ++j) {
    arma::vec v(n - 1);
    arma::uword t = 0;
    for (arma::uword i = 0; i < n; ++i) {
      if (i == j) continue;
      v(t++) = x(i);
    }
    rep(j) = estatistica_id(v, stat_id);
  }
  return rep;
}

// Distribuicao de permutacao da diferenca de medias entre dois grupos.
// Combina x e y, permuta os rotulos B vezes e devolve a diferenca de medias.
// [[Rcpp::export]]
arma::vec permutacao_difmedias_cpp(const arma::vec& x, const arma::vec& y, int B) {
  const arma::uword nx = x.n_elem;
  const arma::uword ny = y.n_elem;
  const arma::uword n = nx + ny;
  arma::vec z(n);
  for (arma::uword i = 0; i < nx; ++i) z(i) = x(i);
  for (arma::uword i = 0; i < ny; ++i) z(nx + i) = y(i);
  arma::vec dif(B);
  Rcpp::RNGScope scope;
  for (int b = 0; b < B; ++b) {
    arma::vec zc = z;
    // embaralhamento de Fisher-Yates
    for (arma::uword i = n - 1; i > 0; --i) {
      arma::uword k = (arma::uword) std::floor(R::unif_rand() * (i + 1));
      if (k > i) k = i;
      double tmp = zc(i); zc(i) = zc(k); zc(k) = tmp;
    }
    double mx = arma::mean(zc.subvec(0, nx - 1));
    double my = arma::mean(zc.subvec(nx, n - 1));
    dif(b) = mx - my;
  }
  return dif;
}
