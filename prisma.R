library(tidyverse)
library(here)

source(here("writing", "bilingual-nma-ms", "plot_helpers.R"))

df_data <- readRDS(here("outputs", "df_data.rds"))
df_contrasts <- readRDS(here("outputs", "df_contrasts.rds"))
df_exclusions <- readRDS(here("outputs", "df_exclusions.rds"))

n_studies <- n_distinct(df_data$long_cite)
n_obs <- nrow(df_data)
n_contrasts <- nrow(df_contrasts)

# Search-hit total (pre-dedup); screened records are the Rayyan export.
n_database <- 3747L
n_screened <- nrow(read_csv(here("data", "20240713-AWMT", "articles.csv"), show_col_types = FALSE))
n_duplicates <- n_database - n_screened

excl_map <- c(
  "wrong/mixed population" = "Wrong/mixed population",
  "DLL"                    = "Wrong/mixed population",
  "no comparison"          = "Wrong study design",
  "wrong study design"     = "Wrong study design",
  "no/missing data"        = "Insufficient data",
  "wrong outcome"          = "Wrong outcome",
  "repeated data"          = "Repeated data",
  "article language"       = "Article not in English",
  "review"                 = "Review/no original data",
  "no original data"       = "Review/no original data",
  "retracted"              = "Article retracted"
)

n_notretrieved <- df_exclusions |>
  filter(exclusion_reason_cleaned == "no full text") |>
  nrow()

excl <- df_exclusions |>
  filter(exclusion_reason_cleaned != "no full text") |>
  mutate(reason = recode(exclusion_reason_cleaned, !!!excl_map)) |>
  count(reason, name = "n") |>
  arrange(desc(n))

n_excluded_ft <- sum(excl$n)
n_assessed <- n_studies + n_excluded_ft
n_sought <- n_assessed + n_notretrieved
n_records_excluded <- n_screened - n_sought
stopifnot(n_records_excluded >= 0L)

fig <- plot_prisma(
  n_database, n_duplicates, n_screened, n_records_excluded,
  n_sought, n_notretrieved, n_assessed, excl,
  n_studies, n_obs, n_contrasts
)

fig_dir <- here("writing", "bilingual-nma-ms", "figs")
dir.create(fig_dir, showWarnings = FALSE)
ggsave(file.path(fig_dir, "fig-prisma.pdf"), fig, width = 7.2, height = 4.55, device = cairo_pdf)
ggsave(file.path(fig_dir, "fig-prisma.png"), fig, width = 7.2, height = 4.55, dpi = 220, device = ragg::agg_png)
ggsave(file.path(fig_dir, "fig-prisma.svg"), fig, width = 7.2, height = 4.55, device = svglite::svglite)
