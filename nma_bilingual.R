library(tidyverse)
library(cmdstanr)
library(googlesheets4)

# ── 0. Load data ────────────────────────────────────────────────────────────
ss <- "1Bv0_BjwayZBXeYDtM439AdTjKdH16fvQfarGahmzVSc"
df_data <- read_sheet(ss, "Alvin")

df_data <- df_data |>
  filter(is.na(exclusion_reason)) |> 
  mutate(
    vocab_measure = if_else(vocab_measure == "single-language",
                            "language-specific", vocab_measure),
    N                = as.numeric(N),
    vocab_score_mean = as.numeric(vocab_score_mean),
    vocab_score_sd   = as.numeric(vocab_score_sd),
    age_mean         = as.numeric(age_mean) |> 
      coalesce(rowMeans(cbind(age_max, age_min)))
  )

# ── 1. Effect size helpers: Cohen's d / Hedges g (independent-groups) ──────
smd_independent <- function(m_trt, sd_trt, n_trt, m_ref, sd_ref, n_ref,
                            type = c("g","d")) {
  type <- match.arg(type)
  
  sp <- sqrt(((n_trt - 1) * sd_trt^2 + (n_ref - 1) * sd_ref^2) / (n_trt + n_ref - 2))
  d  <- (m_trt - m_ref) / sp
  
  J  <- 1 - 3 / (4 * (n_trt + n_ref) - 9)
  g  <- J * d
  
  yi <- if (type == "d") d else g
  
  vi <- (n_trt + n_ref) / (n_trt * n_ref) + yi^2 / (2 * (n_trt + n_ref - 2))
  tibble(y = yi, se_y = sqrt(vi), var_y = vi, d = d, g = g, J = J)
}

# ── 2. Assign NMA arms and score language ───────────────────────────────────
# Normalize language strings and split into a set
parse_lang_set <- function(x) {
  x <- tolower(trimws(x))
  x <- gsub("\\s*\\(.*?\\)", "", x)          # drop parenthetical notes
  x <- gsub("\\s+", " ", x)
  x <- gsub("\\band\\b", "/", x)
  x <- gsub("[,;|&]+", "/", x)
  x <- gsub("\\s*/\\s*", "/", x)
  
  # Handle common abbreviations/synonyms (extend as needed)
  x <- gsub("\\beng\\b", "english", x)
  x <- gsub("\\bsp\\b|\\bspa\\b|\\bespanol\\b|\\bespañol\\b", "spanish", x)
  x <- gsub("\\bmandarin chinese\\b|\\bchinese\\b", "mandarin", x)
  
  parts <- strsplit(x, "/", fixed = TRUE)
  lapply(parts, function(v) unique(na.omit(trimws(v[v != ""]))))
}

# TRUE if any overlap, FALSE if none; NA if either side missing/empty
lang_overlap <- function(a, b) {
  A <- parse_lang_set(a)
  B <- parse_lang_set(b)
  mapply(function(x, y) {
    if (length(x) == 0 || length(y) == 0) return(NA)
    length(intersect(x, y)) > 0
  }, A, B)
}

df_arms <- df_data |>
  filter(
    !is.na(vocab_score_mean), !is.na(vocab_score_sd), !is.na(N),
    vocab_measure %in% c("language-specific", "total", "conceptual") |
      lang_group == "monolingual"
  ) |>
  mutate(
    nma_arm = case_when(
      lang_group == "monolingual"                                         ~ 1L,
      lang_group != "monolingual" & vocab_measure == "language-specific"  ~ 2L,
      lang_group != "monolingual" & vocab_measure == "total"              ~ 3L,
      lang_group != "monolingual" & vocab_measure == "conceptual"         ~ 4L
    ),
    # Language this score represents (only meaningful for mono + bi_L1 arm)
    score_language = case_when(
      lang_group == "monolingual"                     ~ L1,
      nma_arm == 2L & !is.na(assessment_tool_L1)      ~ L1,
      nma_arm == 2L & is.na(assessment_tool_L1) & !is.na(assessment_tool_L2) ~ L2,
      nma_arm %in% c(3L, 4L)                          ~ NA_character_
    )
  ) |>
  filter(!is.na(nma_arm))

grand_age_mean <- mean(df_arms$age_mean, na.rm = TRUE)

# ── 3. Build contrasts ──────────────────────────────────────────────────────
df_contrasts <- df_arms |>
  group_by(short_cite, compare_group, vocab_type) |>
  group_modify(function(grp, key) {
    
    mono    <- grp |> filter(nma_arm == 1L)
    non_ref <- grp |> filter(nma_arm != 1L)
    if (nrow(non_ref) == 0L) return(tibble())
    
    if (nrow(mono) > 0L) {
      # Has monolingual: cross all trt × ref ppt pairs, then filter by language
      pairs <- cross_join(
        non_ref |> select(arm_trt = nma_arm, ppt_trt = ppt_group,
                          m_trt   = vocab_score_mean, sd_trt = vocab_score_sd,
                          n_trt   = N, age_trt = age_mean,
                          lang_trt = score_language),
        mono |>    select(arm_ref = nma_arm, ppt_ref = ppt_group,
                          m_ref   = vocab_score_mean, sd_ref = vocab_score_sd,
                          n_ref   = N, age_ref = age_mean,
                          lang_ref = score_language)
      ) |>
        filter(
          # bi_L1 (arm 2): only match when score languages agree
          # bi_total / bi_conceptual (arms 3,4): no language restriction
          arm_trt != 2L |
            (lang_overlap(lang_trt, lang_ref) %in% TRUE)
        )
    } else {
      # Bi-only study: use lowest bilingual arm as reference
      ref_arm  <- min(non_ref$nma_arm)
      ref_rows <- non_ref |> filter(nma_arm == ref_arm)
      trt_rows <- non_ref |> filter(nma_arm != ref_arm)
      if (nrow(trt_rows) == 0L) return(tibble())
      
      pairs <- cross_join(
        trt_rows |> select(arm_trt = nma_arm, ppt_trt = ppt_group,
                           m_trt   = vocab_score_mean, sd_trt = vocab_score_sd,
                           n_trt   = N, age_trt = age_mean,
                           lang_trt = score_language),
        ref_rows |> select(arm_ref = nma_arm, ppt_ref = ppt_group,
                           m_ref   = vocab_score_mean, sd_ref = vocab_score_sd,
                           n_ref   = N, age_ref = age_mean,
                           lang_ref = score_language)
      )
    }
    
    pairs
  }) |>
  ungroup() |>
  mutate(
    age_c = (age_trt - grand_age_mean) / 12
  )

es <- with(
  df_contrasts,
  # Effect sizes (choose type = "g" or "d")
  smd_independent(m_trt, sd_trt, n_trt, m_ref, sd_ref, n_ref, type = "g")
)

key_trt <- paste(df_contrasts$short_cite, df_contrasts$ppt_trt)
key_ref <- paste(df_contrasts$short_cite, df_contrasts$ppt_ref)

key_all <- sort(unique(c(key_trt, key_ref)))

df_contrasts <- df_contrasts |>
  bind_cols(es) |> 
  filter(!is.na(y), !is.na(se_y)) |>
  # ── Integer indices ──────────────────────────────────────────────────────
  mutate(
    study_id   = as.integer(factor(short_cite)),
    comp_id    = as.integer(factor(paste(short_cite, compare_group, vocab_type))),
    # IMPORTANT: ppt_id should link same cohort across timepoints
    ppt_trt_id = match(paste(short_cite, ppt_trt), key_all),
    ppt_ref_id = match(paste(short_cite, ppt_ref), key_all),
    paired     = as.integer(ppt_trt_id == ppt_ref_id)
  )

# ── 4. Diagnostics ──────────────────────────────────────────────────────────
# Confirm bi_L1 contrasts are all language-matched
matched <- df_contrasts |>
  filter(arm_trt == 2L, arm_ref == 1L) |>
  mutate(match = lang_overlap(lang_trt, lang_ref)) |>
  count(lang_trt, lang_ref, match)

lost <- anti_join(
  df_arms |> distinct(short_cite),
  df_contrasts |> distinct(short_cite),
  by = "short_cite"
)
if (nrow(lost) > 0) message("Studies with no valid contrasts: ",
                            paste(lost$short_cite, collapse = ", "))

# ── 5. Covariate matrix ─────────────────────────────────────────────────────
X <- model.matrix(~ age_c + vocab_type, data = df_contrasts)[, -1, drop = FALSE]

N_ppt <- max(c(df_contrasts$ppt_trt_id, df_contrasts$ppt_ref_id))

# ── 6. Stan data list ───────────────────────────────────────────────────────
stan_data <- list(
  N_obs      = nrow(df_contrasts),
  N_study    = n_distinct(df_contrasts$study_id),
  N_ppt      = N_ppt,
  K          = 4L,   # 1=mono, 2=bi_L1, 3=bi_total, 4=bi_conceptual
  P          = ncol(X),
  
  y          = df_contrasts$y,
  se_y       = df_contrasts$se_y,
  
  arm_trt    = df_contrasts$arm_trt,
  arm_ref    = df_contrasts$arm_ref,
  study_id   = df_contrasts$study_id,
  ppt_trt_id = df_contrasts$ppt_trt_id,
  ppt_ref_id = df_contrasts$ppt_ref_id,
  paired     = df_contrasts$paired,
  
  X          = X
)

# ── 7. Fit model ────────────────────────────────────────────────────────────
mod <- cmdstan_model("nma_bilingual.stan")

fit <- mod$sample(
  data            = stan_data,
  seed            = 42,
  chains          = 4,
  parallel_chains = 4,
  iter_warmup     = 1000,
  iter_sampling   = 2000,
  adapt_delta     = 0.95
)

# ── 8. Results ──────────────────────────────────────────────────────────────
df_res <- fit$summary(c("mu_d", "tau", "Omega", "beta",
                        "sigma_within", "sigma_ppt", "sigma_add_paired", "sigma_add_unpaired",
                        "d_total_vs_L1", "d_conceptual_vs_L1", "d_conceptual_vs_total"))
