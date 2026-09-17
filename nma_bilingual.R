library(tidyverse)
library(cmdstanr)
library(googlesheets4)
library(here)

# ── 0. Load data ────────────────────────────────────────────────────────────
ss <- "1Bv0_BjwayZBXeYDtM439AdTjKdH16fvQfarGahmzVSc"
df_data_a <- read_sheet(ss, "Alvin")
df_data_g <- read_sheet(ss, "Gianmatteo")
df_data_f <- read_sheet(ss, "Federica")
df_main <- read_sheet(ss, "main")

clean_df <- function(df_dat) {
  df_dat |> 
    filter_out(str_detect(has_full_text, "exclude"),
               str_detect(compare_group, "exclude")) |> 
    mutate(across(where(is.list), 
                  \(x) map_dbl(x, \(y) ifelse(is.null(y) || is.na(y), NA, 
                                              y |> str_remove(" \\(.*\\)") |> as.numeric()))),
           notes = coalesce(notes, `...35`),
           across(where(is.character), \(x) na_if(x, "/"))) |> 
    select(-`...35`)
}

df_data_f <- bind_rows(
  df_data_f |> 
    mutate(compare_group = map(
      compare_group, 
      \(x) ifelse(is.null(x), NA,
                  replace_values(as.character(x),
                                 "1 & 2" ~ "1",
                                 "3 & 4" ~ "3",
                                 "5 & 6" ~ "5"
                  )))),
  df_data_f |> 
    filter(map_lgl(compare_group, \(x) ifelse(is.null(x), FALSE, str_detect(x, " & ")))) |> 
    mutate(compare_group = map(
      compare_group, 
      \(x) ifelse(is.null(x), NA,
                  replace_values(as.character(x),
                                 "1 & 2" ~ "2",
                                 "3 & 4" ~ "4",
                                 "5 & 6" ~ "6"
                  ))))
)

df_data_raw <- bind_rows(
  df_data_a |>
    filter(is.na(exclusion_reason)),
  clean_df(df_data_g),
  clean_df(df_data_f)
) |> 
  select(-starts_with("clt_"))

df_exclusions <- bind_rows(
  df_data_a |> 
    filter(!is.na(exclusion_reason)) |> 
    mutate(compare_group = map_chr(compare_group, \(x) ifelse(is.null(x), "", as.character(x)))) |> 
    select(long_cite, short_cite, has_full_text, compare_group, exclusion_reason, notes),
  df_data_g |> 
    mutate(compare_group = map_chr(compare_group, \(x) ifelse(is.null(x), "", as.character(x)))) |> 
    filter(str_detect(has_full_text, "exclude") |
             str_detect(compare_group, "exclude") |
             notes == "no comparison") |> 
    select(long_cite, short_cite, has_full_text, compare_group, notes),
  df_data_f |> 
    mutate(compare_group = map_chr(compare_group, \(x) ifelse(is.null(x), "", as.character(x)))) |> 
    filter(str_detect(has_full_text, "exclude") |
             str_detect(compare_group, "exclude") |
             notes == "no comparison") |> 
    select(long_cite, short_cite, has_full_text, compare_group, notes),
) |> 
  mutate(exclusion_reason = case_when(
    !is.na(exclusion_reason) ~ exclusion_reason,
    notes == "no comparison" ~ "no comparison",
    str_detect(compare_group, "exclude") ~ compare_group,
    str_detect(has_full_text, "exclude") ~ has_full_text,
    has_full_text == "no" ~ "no full text",
    .default = NA
  )) |> 
  select(long_cite, short_cite, exclusion_reason) |> 
  bind_rows(df_main |> 
              mutate(has_full_text = str_replace_all(has_full_text, "(exclue|exlculde|excude)", "exclude")) |> 
              filter(str_detect(str_to_lower(has_full_text), "exclude")) |> 
              select(long_cite, short_cite, exclusion_reason = has_full_text)) |> 
  distinct() |> 
  group_by(long_cite, short_cite) |> 
  summarise(exclusion_reason = str_c(exclusion_reason, collapse = " / "),
            .groups = "drop") |> 
  mutate(exclusion_reason = exclusion_reason |> str_to_lower() |> str_remove_all("exclude(d) ?"))

write_csv(df_exclusions |> select(exclusion_reason) |> distinct(), here("data", "exclusions.csv"))
df_exclusions_cleaned <- read_csv(here("data", "exclusions_cleaned.csv"))
df_exclusions <- df_exclusions |> 
  mutate(exclusion_reason = str_replace(exclusion_reason, " / $", " /")) |> 
  left_join(df_exclusions_cleaned, by = join_by(exclusion_reason))
df_exclusion_summary <- df_exclusions |> 
  group_by(exclusion_reason_cleaned) |> 
  summarise(n = n())
df_data_raw <- df_data_raw |> 
  filter(!(long_cite %in% df_exclusions$long_cite))

# sanity check
df_main |> 
  filter(!long_cite %in% c(df_data_raw$long_cite, df_exclusions$long_cite),
         !inclusion_decision_new %in% c("E+E", "M+E")) |> 
  View()

clean_lang <- function(lang) {
  lang |> 
    str_replace_all("French, English,  \\(n = 1 Hebrew, n = 1 Turkish, and n = 4 Italian\\)", "various") |> 
    str_remove_all(" ?\\(.*\\) ?") |> 
    str_remove_all("\"]") |> 
    str_remove_all("-speaking") |> 
    str_replace_all("( and |, | or )", "/") |> 
    str_remove_all("\\.$") |> 
    str_remove_all("[0-9]+%? ?") |> 
    replace_when(
      str_detect(lang, "([Dd]ifferent|mixed|[Oo]ther|:|non-?English|Moroccan)") ~ "various",
      str_detect(lang, "(^/$|^na|^n=|Not reported)") ~ NA,
      str_detect(lang, "Peruvian") ~ "Quechua/Spanish",
      lang == "english" ~ "English"
    )
}

df_data_cleaned <- df_data_raw |> 
  mutate(
    lang_group = lang_group |> 
      str_to_lower() |> 
      str_replace_all(
        c(" \\(.*" = "",
          "multi/bilingual" = "multilingual",
          "bilngual" = "bilingual")
      ),
    vocab_measure = vocab_measure |> 
      str_to_lower() |> 
      str_replace_all(
        c(
          " vocabulary" = "",
          " spe" = "-spe",
          " \\(english\\)" = "",
          "cifc" = "cific"
        )
      ) |> na_if("/"),
    vocab_measure = ifelse(lang_group == "monolingual" & is.na(vocab_measure),
                           "language-specific", vocab_measure),
    across(c(L1, L2), clean_lang),
    N = str_remove_all(N, " \\(.*\\)"),
    assessment_tool_L1 = na_if(assessment_tool_L1, "na"),
    assessment_tool_L1 = na_if(assessment_tool_L1, "Not reported"),
    assessment_tool_L2 = na_if(assessment_tool_L2, "na")) |> 
  filter(vocab_type %in% c("receptive", "expressive"))

df_data_cleaned |> 
  select(infant_type, L1, L2, is_plurilingual, bilingual_type, 
         country, assessment_tool_L1, assessment_tool_L2, vocab_units) |> 
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value") |> 
  mutate(variable = replace_values(variable,
                                   c("L1", "L2") ~ "lang",
                                   c("assessment_tool_L1", "assessment_tool_L2") ~ "assessment_tool")) |> 
  distinct() |> 
  arrange(variable, value) |> 
  write_csv(here("data", "variable_mapping.csv"))

var_map <- read_csv(here("data", "variable_mapping_std.csv")) |> 
  mutate(variable = map(variable, \(v) {
    if (v == "lang") return(c("L1", "L2"))
    if (v == "assessment_tool") return(c("assessment_tool_L1", "assessment_tool_L2"))
    v
  })) |> 
  unnest(variable)

df_data_std <- df_data_cleaned |> 
  pivot_longer(cols = c(infant_type, L1, L2, is_plurilingual, bilingual_type, 
                        country, assessment_tool_L1, assessment_tool_L2, vocab_units), 
               names_to = "variable", values_to = "value") |> 
  left_join(var_map, by = join_by(variable, value)) |> 
  select(-value) |> 
  pivot_wider(names_from = variable, values_from = std_value)

df_data <- df_data_std |>
  mutate(
    N                = as.numeric(N),
    vocab_score_mean = as.numeric(vocab_score_mean),
    vocab_score_sd   = as.numeric(vocab_score_sd),
    age_mean         = as.numeric(age_mean) |> 
      coalesce(rowMeans(cbind(age_max, age_min))),
    infant_typical   = infant_type == "typical"
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
# TRUE if any overlap, FALSE if none; NA if either side missing/empty
lang_overlap <- function(a, b) {
  A <- str_split(a, ",")
  B <- str_split(b, ",")
  # mapply(function(x, y) {
  #   if (length(x) == 0 || length(y) == 0) return(NA)
  #   length(intersect(x, y)) > 0
  # }, A, B)
  map2_lgl(A, B, \(x, y) {
    if (length(x) == 0 || length(y) == 0) return(NA)
    if ("zho" %in% x && ("cmn" %in% y || "yue" %in% y)) return(TRUE)
    if ("zho" %in% y && ("cmn" %in% x || "yue" %in% x)) return(TRUE)
    length(intersect(x, y)) > 0
  })
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
  group_by(short_cite, compare_group, vocab_type, vocab_units, 
           infant_typical, assessment_procedure) |> # include for more moderators
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

####### BELOW UNTESTED

# ── 5. Build stan_data for a given moderator formula ────────────────────────
build_stan_data <- function(df_contrasts, formula = ~ 1) {
  
  # ~ 1 gives an intercept-only matrix; drop it to get 0 columns
  X <- model.matrix(formula, data = df_contrasts)
  X <- X[, setdiff(colnames(X), "(Intercept)"), drop = FALSE]
  
  if (nrow(df_contrasts) != nrow(X)) {
    stop("Mismatch in nrow between df_contrasts and X")
  }
  
  N_ppt <- max(c(df_contrasts$ppt_trt_id, df_contrasts$ppt_ref_id))
  
  list(
    N_obs      = nrow(df_contrasts),
    N_study    = n_distinct(df_contrasts$study_id),
    N_ppt      = N_ppt,
    K          = 4L,
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
}

mod <- cmdstan_model("nma_bilingual.stan")

# ── 6. Fit models ───────────────────────────────────────────────────────────
model_specs <- list(
  base                  = ~ 1,
  age                   = ~ age_c,
  vocab_type            = ~ vocab_type,
  infant_typical        = ~ infant_typical,
  assessment_procedure  = ~ assessment_procedure
)

fit_one <- function(spec_formula) {
  stan_data <- build_stan_data(df_contrasts, spec_formula)
  mod$sample(
    data            = stan_data,
    seed            = 42,
    chains          = 4,
    parallel_chains = 4,
    iter_warmup     = 1000,
    iter_sampling   = 2000,
    adapt_delta     = 0.95
  )
}

fits <- map(model_specs, fit_one)
saveRDS(fits, here("outputs", "fits.rds"))

# ── 7. Results ──────────────────────────────────────────────────────────────

summarise_fit <- function(fit, spec_name, draws = FALSE) {
  vars <- c("mu_d", "tau", "Omega", "sigma_within", "sigma_ppt",
            "d_total_vs_L1", "d_conceptual_vs_L1", "d_conceptual_vs_total")
  
  # add beta only if the model had moderators
  if (any(grepl("^beta", fit$metadata()$stan_variables)))
    vars <- c(vars, "beta")
  
  if (draws) {
    fit$draws(vars) |> 
      as_tibble() |> 
      mutate(model = spec_name, .before = 1)
  } else {
    fit$summary(
      vars,
      mean, median, sd, mad,
      \(x) posterior::quantile2(x, probs = c(0.025, 0.975)),
      posterior::default_convergence_measures()
    ) |>
      mutate(model = spec_name, .before = 1)
  }
}

df_res_all <- imap(fits, summarise_fit) |> bind_rows()
write_csv(df_res_all, "df_res_all.csv")

df_draws <- imap(fits, \(f, n) summarise_fit(f, n, draws = TRUE)) |> bind_rows()

df_res_all |> 
  separate_wider_delim(variable, delim = "[", too_few = "align_start", 
                       names = c("variable", "comparison")) |> 
  filter(variable != "Omega",
         !str_starts(variable, "d_")) |> 
  mutate(comparison = str_remove_all(comparison, "(,1)?]") |> 
           factor(levels = c(1, 2, 3),
                  labels = c("single-language", "total", "conceptual")),
         model = factor(model, 
                        levels = c("base", "age", "vocab_type", 
                                   "assessment_procedure", "infant_typical")),
         variable = factor(variable, 
                           levels = c("mu_d", "sigma_within", "sigma_ppt", 
                                      "tau", "beta"),
                           labels = c("Hedge's g", "Within corr var", 
                                      "Between var", "Heterogeneity", 
                                      "Regression coef")) |> 
           fct_rev()) |> 
  ggplot(aes(x = mean, y = variable, col = comparison)) +
  geom_vline(xintercept = 0, lty = "dashed") +
  geom_errorbar(aes(xmin = q2.5, xmax = q97.5), height = 0) +
  geom_point() +
  facet_grid(~ model) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.position = "bottom")


# # ── 5. Covariate matrix ─────────────────────────────────────────────────────
# X <- model.matrix(~ age_c + vocab_type + infant_typical + assessment_procedure, 
#                   data = df_contrasts)[, -1, drop = FALSE]
# if (nrow(df_contrasts) != nrow(X)) stop("Mismatch in nrow between df_contrasts and X")
# 
# N_ppt <- max(c(df_contrasts$ppt_trt_id, df_contrasts$ppt_ref_id))
# 
# # ── 6. Stan data list ───────────────────────────────────────────────────────
# stan_data <- list(
#   N_obs      = nrow(df_contrasts),
#   N_study    = n_distinct(df_contrasts$study_id),
#   N_ppt      = N_ppt,
#   K          = 4L,   # 1=mono, 2=bi_L1, 3=bi_total, 4=bi_conceptual
#   P          = ncol(X),
#   
#   y          = df_contrasts$y,
#   se_y       = df_contrasts$se_y,
#   
#   arm_trt    = df_contrasts$arm_trt,
#   arm_ref    = df_contrasts$arm_ref,
#   study_id   = df_contrasts$study_id,
#   ppt_trt_id = df_contrasts$ppt_trt_id,
#   ppt_ref_id = df_contrasts$ppt_ref_id,
#   paired     = df_contrasts$paired,
#   
#   X          = X
# )
# 
# # ── 7. Fit model ────────────────────────────────────────────────────────────
# mod <- cmdstan_model("nma_bilingual.stan")
# 
# fit <- mod$sample(
#   data            = stan_data,
#   seed            = 42,
#   chains          = 4,
#   parallel_chains = 4,
#   iter_warmup     = 1000,
#   iter_sampling   = 2000,
#   adapt_delta     = 0.95
# )
# 
# # ── 8. Results ──────────────────────────────────────────────────────────────
# df_res <- fit$summary(c("mu_d", "tau", "Omega", "beta",
#                         "sigma_within", "sigma_ppt", # "sigma_add_paired", "sigma_add_unpaired",
#                         "d_total_vs_L1", "d_conceptual_vs_L1", "d_conceptual_vs_total"),
#                       mean, median, sd, mad, \(x) posterior::quantile2(x, probs = c(0.025, 0.975)),
#                       posterior::default_convergence_measures())
