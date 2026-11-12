library(tidyverse)
library(here)
library(PRISMA2020)

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

hide <- c("register_results", "excluded_automatic", "excluded_other")
counts <- tribble(
  ~data,                       ~n,
  "database_results",          as.character(n_database),
  "duplicates",                as.character(n_duplicates),
  "records_screened",          as.character(n_screened),
  "records_excluded",          as.character(n_records_excluded),
  "dbr_sought_reports",        as.character(n_sought),
  "dbr_notretrieved_reports",  as.character(n_notretrieved),
  "dbr_assessed",              as.character(n_assessed),
  "dbr_excluded",              paste(excl$reason, excl$n, sep = ", ", collapse = "; "),
  "new_studies",               as.character(n_studies),
  "new_reports",               as.character(n_obs)
)

prisma_tbl <- read.csv(
  system.file("extdata", "PRISMA.csv", package = "PRISMA2020"),
  stringsAsFactors = FALSE
) |>
  as_tibble() |>
  left_join(counts, by = "data", suffix = c("", "_new")) |>
  mutate(
    n = case_when(
      data %in% hide ~ NA_character_,
      !is.na(n_new) ~ n_new,
      .default = n
    ),
    boxtext = case_when(
      data == "new_studies" ~ "Studies included in review",
      data == "new_reports" ~ "Observations included in review",
      .default = boxtext
    )
  ) |>
  select(-n_new) |>
  add_row(
    data = "new_comp",
    box = "box10",
    description = "Pairwise comparisons included in review",
    boxtext = "Pairwise comparisons included in review",
    tooltips = "Pairwise comparisons included in review",
    n = as.character(n_contrasts)
  )

write_csv(prisma_tbl, here("data", "PRISMA.csv"), na = "NA")

inc_n <- function(lab, n) paste0(lab, "\n(n = ", scales::comma(n), ")")
prisma_data <- PRISMA_data(read.csv(here("data", "PRISMA.csv")))
prisma_data$new_studies <- NA
prisma_data$new_reports_text <- paste(
  inc_n("Studies included in review", n_studies),
  inc_n("Observations included in review", n_obs),
  "Pairwise comparisons included in review",
  sep = "\n"
)
prisma_data$new_reports <- scales::comma(n_contrasts)

# PRISMA2020 wraps box text at 40 chars and collapses newlines; keep ours.
ns <- asNamespace("stringr")
orig_wrap <- get("str_wrap", ns)
unlockBinding("str_wrap", ns)
assign(
  "str_wrap",
  function(string, width = 80, indent = 0, exdent = 0, whitespace_only = TRUE) string,
  ns
)
lockBinding("str_wrap", ns)

fig_dir <- here("writing", "figs")
dir.create(fig_dir, showWarnings = FALSE)
tryCatch({
  fig <- suppressWarnings(PRISMA_flowdiagram(
    prisma_data,
    interactive = FALSE,
    previous = FALSE,
    other = FALSE,
    fontsize = 7
  ))
  walk(
    c("pdf", "png", "svg"),
    \(ext) PRISMA_save(fig, file.path(fig_dir, paste0("fig-prisma.", ext)), overwrite = TRUE)
  )
}, finally = {
  unlockBinding("str_wrap", ns)
  assign("str_wrap", orig_wrap, ns)
  lockBinding("str_wrap", ns)
})
