// Backend C++ (RcppArmadillo): funcoes de series temporais (ACF, PACF, media
// movel, suavizacao exponencial).
// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
using namespace Rcpp;

// Funcao de autocorrelacao (ACF) ate lag_max (inclui lag 0 = 1).
// [[Rcpp::export]]
arma::vec acf_cpp(const arma::vec& x, int lag_max) {
  const arma::uword n = x.n_elem;
  double media = arma::mean(x);
  arma::vec d = x - media;
  double c0 = arma::dot(d, d) / n;
  arma::vec out(lag_max + 1);
  out(0) = 1.0;
  for (int k = 1; k <= lag_max; ++k) {
    double ck = 0.0;
    for (arma::uword t = 0; t + k < n; ++t) ck += d(t) * d(t + k);
    ck /= n;
    out(k) = ck / c0;
  }
  return out;
}

// PACF por Durbin-Levinson, lags 1..lag_max.
// [[Rcpp::export]]
arma::vec pacf_cpp(const arma::vec& x, int lag_max) {
  arma::vec r = acf_cpp(x, lag_max); // r(0..lag_max)
  arma::vec phi_prev(lag_max + 1, arma::fill::zeros);
  arma::vec phi_cur(lag_max + 1, arma::fill::zeros);
  arma::vec pacf(lag_max);
  double v = r(0);
  phi_prev(1) = r(1) / r(0);
  pacf(0) = phi_prev(1);
  v = r(0) * (1 - phi_prev(1) * phi_prev(1));
  for (int k = 2; k <= lag_max; ++k) {
    double num = r(k);
    for (int j = 1; j < k; ++j) num -= phi_prev(j) * r(k - j);
    double phi_kk = num / v;
    for (int j = 1; j < k; ++j) phi_cur(j) = phi_prev(j) - phi_kk * phi_prev(k - j);
    phi_cur(k) = phi_kk;
    pacf(k - 1) = phi_kk;
    v = v * (1 - phi_kk * phi_kk);
    phi_prev = phi_cur;
  }
  return pacf;
}

// Media movel simples de janela k (centrada se center=true).
// Posicoes sem janela completa recebem NA (NaN).
// [[Rcpp::export]]
arma::vec media_movel_cpp(const arma::vec& x, int k, bool center) {
  const arma::uword n = x.n_elem;
  arma::vec out(n); out.fill(arma::datum::nan);
  int half = center ? k / 2 : 0;
  for (arma::uword i = 0; i < n; ++i) {
    int ini = center ? (int) i - half : (int) i - (k - 1);
    int fim = center ? (int) i - half + k - 1 : (int) i;
    if (ini < 0 || fim >= (int) n) continue;
    double s = 0.0;
    for (int t = ini; t <= fim; ++t) s += x(t);
    out(i) = s / k;
  }
  return out;
}

// Suavizacao exponencial simples (EWMA) com parametro alpha.
// [[Rcpp::export]]
arma::vec ewma_cpp(const arma::vec& x, double alpha) {
  const arma::uword n = x.n_elem;
  arma::vec out(n);
  out(0) = x(0);
  for (arma::uword i = 1; i < n; ++i) {
    out(i) = alpha * x(i) + (1.0 - alpha) * out(i - 1);
  }
  return out;
}
