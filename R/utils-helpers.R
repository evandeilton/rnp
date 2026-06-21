## Internal validation helpers -----------------------------------------------

# Type-stable, fast, defensive. Used throughout rnp modules.

abort_vetor <- function(x, nm = "x", allow_null = FALSE) {
  if (is.null(x)) {
    if (allow_null) return(invisible(NULL))
    rlang::abort(c("{.arg {nm}} nao pode ser NULL.",
                   i = "Informe um vetor numerico ou caractere."))
  }
  if (!is.null(dim(x))) {
    rlang::abort(c("{.arg {nm}} deve ser um vetor, nao uma matriz/data.frame.",
                   i = "Dimensoes detectadas: {paste(dim(x), collapse = ' x ')}."))
  }
  invisible(NULL)
}

abort_numerico <- function(x, nm = "x", allow_na = TRUE) {
  abort_vetor(x, nm)
  if (!is.numeric(x)) {
    rlang::abort(c("{.arg {nm}} deve ser numerico.",
                   i = "Classe atual: {paste(class(x), collapse = '/')}."))
  }
  if (!allow_na && anyNA(x)) {
    rlang::abort(c("{.arg {nm}} contem valores faltantes.",
                   i = "Use {.code na.rm = TRUE} ou remova os NA's manualmente."))
  }
  invisible(NULL)
}

abort_positivo <- function(x, nm = "x", strict = TRUE) {
  abort_numerico(x, nm)
  if (strict && any(x <= 0, na.rm = TRUE)) {
    rlang::abort(c("{.arg {nm}} deve conter apenas valores positivos.",
                   i = "Operacao matematica exige x > 0."))
  } else if (any(x < 0, na.rm = TRUE)) {
    rlang::abort(c("{.arg {nm}} deve conter apenas valores nao-negativos."))
  }
  invisible(NULL)
}

abort_proporcao <- function(p, nm = "p") {
  if (!is.numeric(p) || length(p) != 1L) {
    rlang::abort("{.arg {nm}} deve ser um escalar numerico em [0, 1].")
  }
  if (is.na(p) || p < 0 || p > 1) {
    rlang::abort("{.arg {nm}} deve estar no intervalo [0, 1].")
  }
  invisible(NULL)
}

abort_inteiro_pos <- function(n, nm = "n") {
  if (!is.numeric(n) || length(n) != 1L || is.na(n)) {
    rlang::abort("{.arg {nm}} deve ser um escalar inteiro positivo.")
  }
  if (n != floor(n) || n < 1L) {
    rlang::abort("{.arg {nm}} deve ser inteiro positivo (>= 1).")
  }
  invisible(NULL)
}

abort_confianca <- function(conf) {
  if (!is.numeric(conf) || length(conf) != 1L || is.na(conf)) {
    rlang::abort("{.arg conf} deve ser um escalar numerico em (0, 1).")
  }
  if (conf <= 0 || conf >= 1) {
    rlang::abort("{.arg conf} deve estar em (0, 1), ex.: 0.95 para 95%.")
  }
  invisible(NULL)
}

# Fast NA-safe rounding with consistent integer preservation
arredonda <- function(x, digits = 4L) {
  if (is.integer(digits)) digits <- as.numeric(digits)
  round(x, digits = digits)
}

# Coerce to double safely
as_dbl <- function(x) {
  vctrs::vec_cast(x, to = double())
}

# Drop NA preserving type
sem_na <- function(x) {
  vctrs::vec_slice(x, !is.na(x))
}

# Internal: validate alpha
check_alpha <- function(alpha) {
  if (!is.numeric(alpha) || length(alpha) != 1L || is.na(alpha)) {
    rlang::abort("{.arg alpha} (nivel de significancia) deve ser escalar numerico.")
  }
  if (alpha <= 0 || alpha >= 1) rlang::abort("{.arg alpha} deve estar em (0, 1).")
  invisible(alpha)
}

# Internal: check sides
check_lado <- function(lado) {
  lado <- rlang::arg_match(lado, c("bilateral", "direita", "esquerda"))
  lado
}

# Format numeric for printing in PT-BR
fmt_num <- function(x, digits = 4) {
  out <- formatC(x, digits = digits, format = "f", flag = "-")
  out[is.na(x)] <- NA_character_
  out
}

# Safe quantile method enum
check_quantile_type <- function(type) {
  if (!is.numeric(type) || length(type) != 1L ||
      !(type %in% 1:9)) {
    rlang::abort("{.arg type} deve ser inteiro entre 1 e 9 (Hyndman-Fan).")
  }
  as.integer(type)
}

# Quiet load of suggested packages, returning TRUE/FALSE
tem_pacote <- function(pkg) {
  rlang::is_installed(pkg)
}

precisa_pacote <- function(pkg, funcao = NULL) {
  if (!tem_pacote(pkg)) {
    msg <- glue::glue("Pacote '{pkg}' e necessario")
    if (!is.null(funcao)) msg <- glue::glue("{msg} para a funcao '{funcao}'")
    msg <- glue::glue("{msg}. Instale com: install.packages('{pkg}')")
    rlang::abort(msg, call = rlang::caller_env())
  }
  invisible(TRUE)
}
