# =============================================================================
# 00_functions.R – helper functions (base R + splines only)
# =============================================================================

num <- function(x) suppressWarnings(as.numeric(x))

# Keep only allowed codes; everything else (e.g. 'prefer not to answer') -> NA
keep_codes <- function(x, ok) { x <- num(x); x[!(x %in% ok)] <- NA; x }

# ---- Files -------------------------------------------------------------------
find_file <- function(pattern, dir) {
  f <- list.files(dir, pattern = "\\.csv$", ignore.case = TRUE)
  hit <- f[grepl(pattern, f, ignore.case = TRUE, perl = TRUE)]
  if (length(hit) == 0) return(NA_character_)
  if (length(hit) > 1)
    stop(sprintf("Pattern '%s' matched several files (%s). Put the exact file name in config.R.",
                 pattern, paste(hit, collapse = ", ")), call. = FALSE)
  file.path(dir, hit)
}

read_fox <- function(path) {
  d <- if (requireNamespace("data.table", quietly = TRUE)) {
    as.data.frame(data.table::fread(path, showProgress = FALSE))
  } else {
    utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  }
  names(d) <- tolower(trimws(names(d)))
  need <- c(cfg$v$id, cfg$v$day)
  if (!all(need %in% names(d)))
    stop(sprintf("%s is missing %s", basename(path),
                 paste(setdiff(need, names(d)), collapse = ", ")), call. = FALSE)
  d[[cfg$v$id]]  <- as.character(d[[cfg$v$id]])
  d[[cfg$v$day]] <- num(d[[cfg$v$day]])
  d[!is.na(d[[cfg$v$day]]) & !is.na(d[[cfg$v$id]]) & d[[cfg$v$id]] != "", , drop = FALSE]
}

# Return the requested columns, creating all-NA columns for any that are absent
cols <- function(d, vars) {
  for (v in setdiff(vars, names(d))) d[[v]] <- NA_real_
  d[, vars, drop = FALSE]
}

# ---- Time matching -----------------------------------------------------------
# anchor: data.frame with row_id, fox_insight_id, lo, hi, target (absolute days)
# tab:    a questionnaire table. For each anchor row, picks the response with
#         lo <= days_elapsed <= hi that is closest to `target` (ties -> earlier).
match_nearest <- function(anchor, tab, vars, prefix) {
  idv <- cfg$v$id; dv <- cfg$v$day
  b <- cols(tab, c(idv, dv, vars))
  m <- merge(anchor[, c("row_id", idv, "lo", "hi", "target")], b, by = idv)
  m <- m[which(m[[dv]] >= m$lo & m[[dv]] <= m$hi), , drop = FALSE]
  out <- data.frame(row_id = anchor$row_id)
  if (nrow(m) == 0) {
    for (v in vars) out[[paste0(prefix, v)]] <- NA_real_
    return(out)
  }
  m$dist <- abs(m[[dv]] - m$target)
  m <- m[order(m$row_id, m$dist, m[[dv]]), ]
  m <- m[!duplicated(m$row_id), c("row_id", vars), drop = FALSE]
  names(m) <- c("row_id", paste0(prefix, vars))
  merge(out, m, by = "row_id", all.x = TRUE)
}

# Mean of each variable over all responses with lo <= days <= hi (chronicity)
window_mean <- function(anchor, tab, vars, prefix, min_n = 2) {
  idv <- cfg$v$id; dv <- cfg$v$day
  b <- cols(tab, c(idv, dv, vars))
  m <- merge(anchor[, c("row_id", idv, "lo", "hi")], b, by = idv)
  m <- m[which(m[[dv]] >= m$lo & m[[dv]] <= m$hi), , drop = FALSE]
  out <- data.frame(row_id = anchor$row_id)
  for (v in vars) {
    x <- m[!is.na(m[[v]]), c("row_id", v)]
    if (nrow(x) == 0) { out[[paste0(prefix, v)]] <- NA_real_; next }
    mn <- tapply(x[[v]], x$row_id, mean)
    n  <- tapply(x[[v]], x$row_id, length)
    mn[n < min_n] <- NA
    out[[paste0(prefix, v)]] <- as.numeric(mn[match(out$row_id, names(mn))])
  }
  out
}

# ---- Scores ------------------------------------------------------------------
# Sum score, prorated when at most `max_missing` items are missing
sum_score <- function(d, items, ok, max_missing = 2) {
  m <- sapply(items, function(v) keep_codes(cols(d, v)[[1]], ok))
  if (is.null(dim(m))) m <- matrix(m, nrow = 1)
  n_ok <- rowSums(!is.na(m))
  s <- rowSums(m, na.rm = TRUE) * length(items) / pmax(n_ok, 1)
  s[n_ok < length(items) - max_missing] <- NA
  s
}

# ---- Models ------------------------------------------------------------------
complete_for <- function(d, f, extra = character(0)) {
  v <- unique(c(all.vars(f), extra))
  d[stats::complete.cases(d[, v, drop = FALSE]), , drop = FALSE]
}

fit_logit <- function(f, d, w = NULL) {
  if (is.null(w)) stats::glm(f, data = d, family = stats::binomial())
  else { d$.w <- w; stats::glm(f, data = d, family = stats::quasibinomial(), weights = .w) }
}

# Cluster-robust (sandwich) covariance for a glm; equivalent to sandwich::vcovCL (HC0
# with G/(G-1) adjustment)
vcov_cluster <- function(fit, cluster) {
  b  <- stats::coef(fit); keep <- !is.na(b)
  X  <- stats::model.matrix(fit)[, keep, drop = FALSE]
  bu <- summary(fit)$cov.unscaled
  wres <- as.vector(stats::residuals(fit, type = "working")) * fit$weights
  S  <- rowsum(X * wres, cluster, reorder = FALSE)
  G  <- nrow(S)
  V  <- bu %*% crossprod(S) %*% bu * G / (G - 1)
  dimnames(V) <- list(names(b)[keep], names(b)[keep])
  V
}

coef_table <- function(fit, cluster, terms) {
  V <- vcov_cluster(fit, cluster); b <- stats::coef(fit)[rownames(V)]
  se <- sqrt(diag(V)); z <- b / se
  out <- data.frame(term = names(b), or = exp(b), or_lo = exp(b - 1.96 * se),
                    or_hi = exp(b + 1.96 * se), p = 2 * stats::pnorm(-abs(z)),
                    row.names = NULL)
  out[out$term %in% terms, ]
}

# Wald test that a set of coefficients (e.g. interaction terms) are all zero
wald_test <- function(fit, cluster, pattern) {
  V <- vcov_cluster(fit, cluster); b <- stats::coef(fit)[rownames(V)]
  k <- grep(pattern, names(b))
  if (length(k) == 0) return(c(chisq = NA, df = NA, p = NA))
  chi <- as.numeric(t(b[k]) %*% solve(V[k, k, drop = FALSE]) %*% b[k])
  c(chisq = chi, df = length(k), p = stats::pchisq(chi, length(k), lower.tail = FALSE))
}

# Marginal (standardised) risks under exposure = 1 and = 0
gcomp <- function(fit, d, expo, w = NULL) {
  d1 <- d; d1[[expo]] <- 1; d0 <- d; d0[[expo]] <- 0
  p1 <- stats::predict(fit, d1, type = "response"); p0 <- stats::predict(fit, d0, type = "response")
  if (is.null(w)) w <- rep(1, nrow(d))
  r1 <- stats::weighted.mean(p1, w); r0 <- stats::weighted.mean(p0, w)
  c(risk_exposed = r1, risk_unexposed = r0, rd = r1 - r0, rr = r1 / r0)
}

# Resample participants (clusters) with replacement
cluster_resample <- function(d, idvar) {
  ids <- unique(d[[idvar]])
  pick <- sample(ids, length(ids), replace = TRUE)
  idx <- split(seq_len(nrow(d)), d[[idvar]])
  rows <- unlist(idx[as.character(pick)], use.names = FALSE)
  out <- d[rows, , drop = FALSE]
  reps <- vapply(idx[as.character(pick)], length, 1L)
  out$.boot_id <- rep(seq_along(pick), reps)
  out
}

boot_ci <- function(est_fun, d, idvar, B) {
  res <- replicate(B, tryCatch(est_fun(cluster_resample(d, idvar)),
                               error = function(e) rep(NA_real_, length(est_fun(d)))))
  res <- matrix(res, ncol = B)
  t(apply(res, 1, stats::quantile, probs = c(0.025, 0.975), na.rm = TRUE))
}

# E-value for a risk ratio (VanderWeele & Ding 2017)
evalue <- function(rr) {
  if (is.na(rr)) return(NA_real_)
  if (rr < 1) rr <- 1 / rr
  rr + sqrt(rr * (rr - 1))
}

# ---- Disclosure control ------------------------------------------------------
suppress_counts <- function(df, count_cols, min_cell = cfg$min_cell) {
  for (cc in count_cols) {
    x <- df[[cc]]
    df[[cc]] <- ifelse(!is.na(x) & x > 0 & x < min_cell, paste0("<", min_cell), as.character(x))
  }
  df
}

write_out <- function(df, name) {
  dir.create(cfg$output_dir, showWarnings = FALSE, recursive = TRUE)
  utils::write.csv(df, file.path(cfg$output_dir, name), row.names = FALSE)
  message("  wrote ", file.path(cfg$output_dir, name))
}

fmt <- function(x, d = 2) formatC(x, format = "f", digits = d)
