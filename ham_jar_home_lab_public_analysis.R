# Reproducible analysis script for the cooked ham JAR/home-lab/purchase-intent study.
# Place dataset.xlsx in the working directory before running this script.

HAM_ROOT_DIR <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

required_packages <- c(
  "readxl", "dplyr", "tidyr", "stringr", "purrr", "janitor", "readr",
  "lubridate", "forcats", "tibble", "ggplot2", "lme4", "lmerTest",
  "emmeans", "ordinal", "ggrepel"
)
missing_packages <- setdiff(required_packages, rownames(installed.packages()))
if (length(missing_packages) > 0) {
  stop("Install missing packages before running: ", paste(missing_packages, collapse = ", "))
}
suppressPackageStartupMessages(invisible(lapply(required_packages, library, character.only = TRUE)))


root_dir <- HAM_ROOT_DIR
setwd(root_dir)

out_dir <- file.path(root_dir, "data_processed")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

xlsx_candidates <- file.path(root_dir, c("dataset.xlsx", "dataset(1).xlsx"))
xlsx_path <- xlsx_candidates[file.exists(xlsx_candidates)][1]

if (is.na(xlsx_path)) {
  stop("dataset.xlsx or dataset(1).xlsx was not found in: ", root_dir)
}

read_ham_sheet <- function(sheet_name) {
  readxl::read_excel(
    path = xlsx_path,
    sheet = sheet_name,
    na = c("", "NA", "N/A", "na", "n/a")
  ) |>
    janitor::remove_empty(which = c("rows", "cols")) |>
    janitor::clean_names()
}

first_non_na <- function(x) {
  y <- x[!is.na(x)]
  if (length(y) == 0) {
    return(x[NA_integer_][1])
  }
  y[1]
}

as01 <- function(x) {
  as.integer(as.character(x))
}

factor_yes_no <- function(x) {
  factor(x, levels = c(0, 1), labels = c("no", "yes"))
}

mean_or_na <- function(x) {
  if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
}

sd_or_na <- function(x) {
  if (sum(!is.na(x)) <= 1) NA_real_ else sd(x, na.rm = TRUE)
}

min_or_na <- function(x) {
  if (all(is.na(x))) NA else min(x, na.rm = TRUE)
}

max_or_na <- function(x) {
  if (all(is.na(x))) NA else max(x, na.rm = TRUE)
}

add_jar_variables <- function(df) {
  jar_specs <- tibble::tribble(
    ~jar_var,       ~short,
    "jar_color",    "color",
    "jar_fat",      "fat",
    "jar_salt",     "salt",
    "jar_tender",   "tender"
  )

  for (i in seq_len(nrow(jar_specs))) {
    v <- jar_specs$jar_var[i]
    s <- jar_specs$short[i]

    if (!v %in% names(df)) {
      stop("Missing JAR variable: ", v)
    }

    x <- df[[v]]

    df[[paste0(s, "_too_low")]]  <- as.integer(!is.na(x) & x < 0)
    df[[paste0(s, "_too_high")]] <- as.integer(!is.na(x) & x > 0)
    df[[paste0(s, "_dev")]]      <- as.integer(!is.na(x) & x != 0)
    df[[paste0(s, "_abs")]]      <- abs(x)

    df[[paste0(s, "_jar3")]] <- factor(
      dplyr::case_when(
        is.na(x) ~ NA_character_,
        x < 0    ~ "too_low",
        x == 0   ~ "jar",
        x > 0    ~ "too_high"
      ),
      levels = c("too_low", "jar", "too_high")
    )
  }

  dev_cols <- paste0(jar_specs$short, "_dev")
  abs_cols <- paste0(jar_specs$short, "_abs")

  df$jar_dev_count <- rowSums(df[, dev_cols], na.rm = FALSE)
  df$jar_severity  <- rowSums(df[, abs_cols], na.rm = FALSE)

  df
}

expected_sheets <- c(
  "description",
  "product packaging",
  "product composition",
  "consumer",
  "consumer questionnaire (home)",
  "consumer questionnaire (lab)",
  "product sensory properties",
  "product purchase informations"
)

available_sheets <- readxl::excel_sheets(xlsx_path)
missing_sheets <- setdiff(expected_sheets, available_sheets)

if (length(missing_sheets) > 0) {
  stop(
    "The following expected sheets were not found: ",
    paste(missing_sheets, collapse = ", ")
  )
}

raw <- purrr::map(expected_sheets, read_ham_sheet)
names(raw) <- expected_sheets

description_raw <- raw[["description"]]

qa_sheet_overview <- tibble::tibble(
  sheet = names(raw),
  n_rows = purrr::map_int(raw, nrow),
  n_cols = purrr::map_int(raw, ncol)
)

packaging_product <- raw[["product packaging"]] |>
  mutate(
    product = as.character(product),
    across(
      any_of(c("organic", "no_nitrite", "salt_light", "fat_light", "label_rouge")),
      as01
    ),
    across(
      any_of(c("organic", "no_nitrite", "salt_light", "fat_light", "label_rouge")),
      ~ replace_na(.x, 0L)
    ),
    brand_type = factor(brand_type),
    company_code = factor(company_code),
    nutriscore = factor(nutriscore, levels = c("A", "B", "C", "D", "E"), ordered = TRUE)
  ) |>
  distinct(product, .keep_all = TRUE)

composition_replicates <- raw[["product composition"]] |>
  mutate(
    product = as.character(product),
    replicate = as.integer(replicate),
    expiration_date = suppressWarnings(as.Date(expiration_date)),
    fat_measured = as.numeric(fat_measured),
    salt_measured = as.numeric(salt_measured)
  )

composition_product <- composition_replicates |>
  group_by(product) |>
  summarise(
    n_composition_replicates = n(),
    fat_measured_mean = mean_or_na(fat_measured),
    fat_measured_sd   = sd_or_na(fat_measured),
    salt_measured_mean = mean_or_na(salt_measured),
    salt_measured_sd   = sd_or_na(salt_measured),
    expiration_date_min = min_or_na(expiration_date),
    expiration_date_max = max_or_na(expiration_date),
    .groups = "drop"
  )

consumer_raw <- raw[["consumer"]] |>
  mutate(
    consumer = as.character(consumer),
    test_location_from_id = case_when(
      stringr::str_starts(consumer, "H") ~ "home",
      stringr::str_starts(consumer, "L") ~ "lab",
      TRUE ~ NA_character_
    ),
    test_location = coalesce(as.character(test_location), test_location_from_id),
    test_location = factor(test_location, levels = c("home", "lab")),
    gender = factor(gender),
    age = factor(age, levels = c("18-30", "31-50", "51+"), ordered = TRUE),
    city = factor(city),
    consumption_frequency_by_month = as.numeric(consumption_frequency_by_month)
  ) |>
  select(-test_location_from_id)

qa_consumer_duplicates <- consumer_raw |>
  count(consumer, name = "n") |>
  filter(n > 1)

if (nrow(qa_consumer_duplicates) > 0) {
}

consumer_clean <- consumer_raw |>
  arrange(consumer) |>
  group_by(consumer) |>
  summarise(across(everything(), first_non_na), .groups = "drop") |>
  rename(consumer_test_location = test_location)

home_questionnaire <- raw[["consumer questionnaire (home)"]] |>
  mutate(
    consumer = as.character(consumer),
    difficulty_free_comment = as.numeric(difficulty_free_comment)
  )

lab_questionnaire <- raw[["consumer questionnaire (lab)"]] |>
  mutate(
    consumer = as.character(consumer),
    across(
      any_of(c("zero_nitrite", "organic", "label_rouge", "less_fat", "less_salt")),
      as.numeric
    ),
    type_brand = factor(type_brand)
  ) |>
  arrange(consumer) |>
  group_by(consumer) |>
  summarise(across(everything(), first_non_na), .groups = "drop")

sensory_raw <- raw[["product sensory properties"]] |>
  mutate(
    consumer = as.character(consumer),
    product = as.character(product),
    test_location = case_when(
      stringr::str_starts(consumer, "H") ~ "home",
      stringr::str_starts(consumer, "L") ~ "lab",
      TRUE ~ NA_character_
    ),
    test_location = factor(test_location, levels = c("home", "lab")),
    liking = as.numeric(liking),
    across(
      any_of(c("jar_color", "jar_fat", "jar_salt", "jar_tender")),
      as.numeric
    )
  ) |>
  add_jar_variables()

purchase_clean <- raw[["product purchase informations"]] |>
  mutate(
    consumer = as.character(consumer),
    product = as.character(product),
    ham_usually_bought = as01(ham_usually_bought),
    purchase_intent = as.integer(purchase_intent),
    price = as.numeric(price),
    ham_usually_bought_f = factor_yes_no(ham_usually_bought),
    purchase_intent_f = factor(
      purchase_intent,
      levels = c(-1, 0, 1),
      labels = c("no", "uncertain", "yes")
    ),
    purchase_yes = as.integer(purchase_intent == 1),
    purchase_no = as.integer(purchase_intent == -1)
  )

sensory_full <- sensory_raw |>
  left_join(consumer_clean, by = "consumer") |>
  mutate(
    test_location = coalesce(test_location, consumer_test_location),
    test_location = factor(test_location, levels = c("home", "lab"))
  ) |>
  select(-consumer_test_location) |>
  left_join(packaging_product, by = "product") |>
  left_join(composition_product, by = "product") |>
  left_join(purchase_clean, by = c("consumer", "product"))

sensory_analysis <- sensory_full |>
  select(
    -any_of(c(
      "description_visual",
      "description_texture",
      "description_flavor"
    ))
  ) |>
  mutate(
    product = factor(product),
    consumer = factor(consumer),
    test_location = factor(test_location, levels = c("home", "lab")),
    brand_type = factor(brand_type),
    company_code = factor(company_code)
  )

product_context_summary <- sensory_analysis |>
  group_by(product, test_location) |>
  summarise(
    n_eval = n(),
    n_consumer = n_distinct(consumer),
    liking_mean = mean_or_na(liking),
    liking_sd = sd_or_na(liking),
    jar_dev_count_mean = mean_or_na(jar_dev_count),
    jar_severity_mean = mean_or_na(jar_severity),
    pct_all_jar = mean(jar_dev_count == 0, na.rm = TRUE),
    pct_any_jar_dev = mean(jar_dev_count > 0, na.rm = TRUE),
    pct_color_dev = mean(color_dev == 1, na.rm = TRUE),
    pct_fat_dev = mean(fat_dev == 1, na.rm = TRUE),
    pct_salt_dev = mean(salt_dev == 1, na.rm = TRUE),
    pct_tender_dev = mean(tender_dev == 1, na.rm = TRUE),
    .groups = "drop"
  )

context_wide <- product_context_summary |>
  tidyr::pivot_wider(
    names_from = test_location,
    values_from = c(
      n_eval,
      n_consumer,
      liking_mean,
      liking_sd,
      jar_dev_count_mean,
      jar_severity_mean,
      pct_all_jar,
      pct_any_jar_dev,
      pct_color_dev,
      pct_fat_dev,
      pct_salt_dev,
      pct_tender_dev
    ),
    names_sep = "_"
  )

context_gain <- context_wide |>
  filter(!is.na(liking_mean_home), !is.na(liking_mean_lab)) |>
  mutate(
    context_gain_liking = liking_mean_home - liking_mean_lab,
    context_gain_jar_dev_count =
      jar_dev_count_mean_home - jar_dev_count_mean_lab,
    context_gain_jar_severity =
      jar_severity_mean_home - jar_severity_mean_lab
  ) |>
  arrange(desc(context_gain_liking))

purchase_product_summary <- purchase_clean |>
  group_by(product) |>
  summarise(
    n_purchase_eval = n(),
    price_mean = mean_or_na(price),
    price_sd = sd_or_na(price),
    usual_bought_rate = mean(ham_usually_bought == 1, na.rm = TRUE),
    purchase_intent_mean = mean_or_na(purchase_intent),
    pct_purchase_yes = mean(purchase_intent == 1, na.rm = TRUE),
    pct_purchase_uncertain = mean(purchase_intent == 0, na.rm = TRUE),
    pct_purchase_no = mean(purchase_intent == -1, na.rm = TRUE),
    .groups = "drop"
  )

product_panel <- packaging_product |>
  full_join(composition_product, by = "product") |>
  left_join(context_wide, by = "product") |>
  left_join(
    context_gain |>
      select(
        product,
        context_gain_liking,
        context_gain_jar_dev_count,
        context_gain_jar_severity
      ),
    by = "product"
  ) |>
  left_join(purchase_product_summary, by = "product") |>
  arrange(product)

model_liking <- sensory_analysis |>
  filter(
    !is.na(liking),
    !is.na(jar_dev_count),
    !is.na(test_location)
  ) |>
  mutate(
    product = factor(product),
    consumer = factor(consumer),
    test_location = factor(test_location, levels = c("home", "lab"))
  )

model_liking_common_products <- model_liking |>
  filter(as.character(product) %in% as.character(context_gain$product))

model_purchase <- sensory_analysis |>
  filter(
    test_location == "home",
    !is.na(purchase_intent),
    !is.na(liking),
    !is.na(jar_dev_count)
  ) |>
  mutate(
    product = factor(product),
    consumer = factor(consumer),
    purchase_intent_ord = ordered(
      purchase_intent,
      levels = c(-1, 0, 1),
      labels = c("no", "uncertain", "yes")
    ),
    purchase_yes = as.integer(purchase_intent == 1)
  )

qa_sensory_consumers_not_in_consumer <- sensory_raw |>
  distinct(consumer) |>
  anti_join(consumer_clean, by = "consumer")

qa_sensory_products_not_in_packaging <- sensory_raw |>
  distinct(product) |>
  anti_join(packaging_product, by = "product")

qa_sensory_products_not_in_composition <- sensory_raw |>
  distinct(product) |>
  anti_join(composition_product, by = "product")

qa_purchase_without_sensory <- purchase_clean |>
  distinct(consumer, product) |>
  anti_join(
    sensory_raw |> distinct(consumer, product),
    by = c("consumer", "product")
  )

qa_home_sensory_without_purchase <- sensory_raw |>
  filter(test_location == "home") |>
  distinct(consumer, product) |>
  anti_join(
    purchase_clean |> distinct(consumer, product),
    by = c("consumer", "product")
  )

qa_summary <- list(
  sheet_overview = qa_sheet_overview,
  consumer_duplicates = qa_consumer_duplicates,
  sensory_consumers_not_in_consumer = qa_sensory_consumers_not_in_consumer,
  sensory_products_not_in_packaging = qa_sensory_products_not_in_packaging,
  sensory_products_not_in_composition = qa_sensory_products_not_in_composition,
  purchase_without_sensory = qa_purchase_without_sensory,
  home_sensory_without_purchase = qa_home_sensory_without_purchase
)

readr::write_csv(qa_sheet_overview, file.path(out_dir, "qa_sheet_overview.csv"))
readr::write_csv(qa_consumer_duplicates, file.path(out_dir, "qa_consumer_duplicates.csv"))
readr::write_csv(
  qa_sensory_consumers_not_in_consumer,
  file.path(out_dir, "qa_sensory_consumers_not_in_consumer.csv")
)
readr::write_csv(
  qa_purchase_without_sensory,
  file.path(out_dir, "qa_purchase_without_sensory.csv")
)
readr::write_csv(
  qa_home_sensory_without_purchase,
  file.path(out_dir, "qa_home_sensory_without_purchase.csv")
)

processed <- list(
  description = description_raw,
  packaging_product = packaging_product,
  composition_replicates = composition_replicates,
  composition_product = composition_product,
  consumer = consumer_clean,
  home_questionnaire = home_questionnaire,
  lab_questionnaire = lab_questionnaire,
  sensory_raw = sensory_raw,
  sensory_full = sensory_full,
  sensory_analysis = sensory_analysis,
  purchase = purchase_clean,
  product_context_summary = product_context_summary,
  purchase_product_summary = purchase_product_summary,
  context_gain = context_gain,
  product_panel = product_panel,
  model_liking = model_liking,
  model_liking_common_products = model_liking_common_products,
  model_purchase = model_purchase,
  qa = qa_summary
)

saveRDS(processed, file.path(out_dir, "ham_processed_list.rds"))
saveRDS(sensory_analysis, file.path(out_dir, "sensory_analysis_no_text.rds"))
saveRDS(model_liking, file.path(out_dir, "model_liking.rds"))
saveRDS(model_liking_common_products, file.path(out_dir, "model_liking_common_products.rds"))
saveRDS(model_purchase, file.path(out_dir, "model_purchase.rds"))
saveRDS(product_panel, file.path(out_dir, "product_panel.rds"))
saveRDS(context_gain, file.path(out_dir, "context_gain.rds"))

readr::write_csv(sensory_analysis, file.path(out_dir, "sensory_analysis_no_text.csv"))
readr::write_csv(model_liking, file.path(out_dir, "model_liking.csv"))
readr::write_csv(model_liking_common_products, file.path(out_dir, "model_liking_common_products.csv"))
readr::write_csv(model_purchase, file.path(out_dir, "model_purchase.csv"))
readr::write_csv(product_panel, file.path(out_dir, "product_panel.csv"))
readr::write_csv(context_gain, file.path(out_dir, "context_gain.csv"))
readr::write_csv(product_context_summary, file.path(out_dir, "product_context_summary.csv"))
readr::write_csv(purchase_product_summary, file.path(out_dir, "purchase_product_summary.csv"))

root_dir <- HAM_ROOT_DIR
setwd(root_dir)

processed_dir <- file.path(root_dir, "data_processed")
analysis_dir  <- file.path(root_dir, "analysis_outputs")
table_dir     <- file.path(analysis_dir, "tables")
figure_dir    <- file.path(analysis_dir, "figures")
model_dir     <- file.path(analysis_dir, "models")

dir.create(table_dir,  showWarnings = FALSE, recursive = TRUE)
dir.create(figure_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(model_dir,  showWarnings = FALSE, recursive = TRUE)

find_file <- function(filename) {
  candidates <- c(
    file.path(processed_dir, filename),
    file.path(root_dir, filename)
  )
  found <- candidates[file.exists(candidates)]
  if (length(found) == 0) {
    stop("File not found: ", filename,
         "\nSearched in:\n  ",
         paste(candidates, collapse = "\n  "))
  }
  found[1]
}

read_processed_csv <- function(filename) {
  utils::read.csv(find_file(filename), stringsAsFactors = FALSE, check.names = FALSE)
}

write_table <- function(x, filename) {
  utils::write.csv(x, file.path(table_dir, filename), row.names = FALSE, na = "")
  invisible(x)
}

write_model_summary <- function(model, filename) {
  out <- capture.output(summary(model))
  writeLines(out, file.path(model_dir, filename))
  invisible(out)
}

save_plot <- function(plot, filename, width = 7, height = 5, dpi = 300) {
  ggplot2::ggsave(
    filename = file.path(figure_dir, filename),
    plot = plot,
    width = width,
    height = height,
    dpi = dpi
  )
  invisible(plot)
}

std <- function(x) {
  as.numeric(scale(x))
}

mean_or_na <- function(x) {
  if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
}

sd_or_na <- function(x) {
  if (sum(!is.na(x)) <= 1) NA_real_ else sd(x, na.rm = TRUE)
}

se_or_na <- function(x) {
  n <- sum(!is.na(x))
  if (n <= 1) NA_real_ else sd(x, na.rm = TRUE) / sqrt(n)
}

ci95_or_na <- function(x) {
  n <- sum(!is.na(x))
  if (n <= 1) NA_real_ else 1.96 * sd(x, na.rm = TRUE) / sqrt(n)
}

lmer_control <- lme4::lmerControl(
  optimizer = "bobyqa",
  optCtrl = list(maxfun = 2e5)
)

glmer_control <- lme4::glmerControl(
  optimizer = "bobyqa",
  optCtrl = list(maxfun = 2e5)
)

tidy_lm <- function(model, conf.int = TRUE) {
  co <- as.data.frame(summary(model)$coefficients)
  out <- data.frame(
    term = rownames(co),
    estimate = co[, "Estimate"],
    std.error = co[, "Std. Error"],
    statistic = co[, "t value"],
    p.value = co[, "Pr(>|t|)"],
    row.names = NULL,
    check.names = FALSE
  )
  if (conf.int) {
    ci <- tryCatch(stats::confint(model), error = function(e) NULL)
    if (!is.null(ci)) {
      ci <- as.data.frame(ci)
      ci$term <- rownames(ci)
      names(ci)[1:2] <- c("conf.low", "conf.high")
      out <- dplyr::left_join(out, ci[, c("term", "conf.low", "conf.high")], by = "term")
    } else {
      out$conf.low <- NA_real_
      out$conf.high <- NA_real_
    }
  }
  out
}

tidy_mixed_fixed <- function(model, conf.int = TRUE) {
  co <- as.data.frame(coef(summary(model)))
  out <- data.frame(term = rownames(co), row.names = NULL, check.names = FALSE)
  out$estimate <- co[["Estimate"]]
  out$std.error <- co[["Std. Error"]]
  if ("df" %in% names(co)) out$df <- co[["df"]]
  stat_col <- intersect(c("t value", "z value"), names(co))[1]
  if (!is.na(stat_col)) out$statistic <- co[[stat_col]]
  p_col <- grep("^Pr\\(", names(co), value = TRUE)[1]
  if (!is.na(p_col)) out$p.value <- co[[p_col]] else out$p.value <- NA_real_
  if (conf.int) {
    ci <- tryCatch(suppressMessages(confint(model, parm = "beta_", method = "Wald")), error = function(e) NULL)
    if (!is.null(ci)) {
      ci <- as.data.frame(ci)
      ci$term <- rownames(ci)
      names(ci)[1:2] <- c("conf.low", "conf.high")
      out <- dplyr::left_join(out, ci[, c("term", "conf.low", "conf.high")], by = "term")
    } else {
      out$conf.low <- out$estimate - 1.96 * out$std.error
      out$conf.high <- out$estimate + 1.96 * out$std.error
    }
  }
  out
}

model_liking <- read_processed_csv("model_liking.csv")
model_liking_common <- read_processed_csv("model_liking_common_products.csv")
model_purchase <- read_processed_csv("model_purchase.csv")
context_gain <- read_processed_csv("context_gain.csv")
product_panel <- read_processed_csv("product_panel.csv")
product_context_summary <- read_processed_csv("product_context_summary.csv")
purchase_product_summary <- read_processed_csv("purchase_product_summary.csv")

prepare_liking_data <- function(df) {
  df |>
    mutate(
      product = factor(product),
      consumer = factor(consumer),
      test_location = factor(test_location, levels = c("home", "lab")),
      jar_dev_count = as.numeric(jar_dev_count),
      jar_severity = as.numeric(jar_severity),
      jar_dev_count_f = factor(jar_dev_count, levels = 0:4),
      liking = as.numeric(liking)
    )
}

model_liking <- prepare_liking_data(model_liking)
model_liking_common <- prepare_liking_data(model_liking_common)

model_purchase <- model_purchase |>
  mutate(
    product = factor(product),
    consumer = factor(consumer),
    test_location = factor(test_location, levels = c("home", "lab")),
    liking = as.numeric(liking),
    jar_dev_count = as.numeric(jar_dev_count),
    jar_severity = as.numeric(jar_severity),
    price = as.numeric(price),
    ham_usually_bought = as.integer(ham_usually_bought),
    ham_usually_bought_f = factor(
      ham_usually_bought,
      levels = c(0, 1),
      labels = c("no", "yes")
    ),
    purchase_intent = as.integer(purchase_intent),
    purchase_intent_f = factor(
      purchase_intent,
      levels = c(-1, 0, 1),
      labels = c("no", "uncertain", "yes")
    ),
    purchase_intent_ord = ordered(
      purchase_intent,
      levels = c(-1, 0, 1),
      labels = c("no", "uncertain", "yes")
    ),
    purchase_yes = as.integer(purchase_intent == 1)
  )

context_gain <- context_gain |>
  mutate(product = factor(product))

product_panel <- product_panel |>
  mutate(product = factor(product))

sample_overview <- tibble::tibble(
  dataset = c(
    "model_liking",
    "model_liking_common_products",
    "model_purchase",
    "context_gain",
    "product_panel"
  ),
  n_rows = c(
    nrow(model_liking),
    nrow(model_liking_common),
    nrow(model_purchase),
    nrow(context_gain),
    nrow(product_panel)
  ),
  n_products = c(
    n_distinct(model_liking$product),
    n_distinct(model_liking_common$product),
    n_distinct(model_purchase$product),
    n_distinct(context_gain$product),
    n_distinct(product_panel$product)
  ),
  n_consumers = c(
    n_distinct(model_liking$consumer),
    n_distinct(model_liking_common$consumer),
    n_distinct(model_purchase$consumer),
    NA_integer_,
    NA_integer_
  )
)

write_table(sample_overview, "00_sample_overview.csv")

location_overview <- model_liking |>
  group_by(test_location) |>
  summarise(
    n_evaluations = n(),
    n_consumers = n_distinct(consumer),
    n_products = n_distinct(product),
    liking_mean = mean(liking, na.rm = TRUE),
    liking_sd = sd(liking, na.rm = TRUE),
    jar_dev_count_mean = mean(jar_dev_count, na.rm = TRUE),
    jar_severity_mean = mean(jar_severity, na.rm = TRUE),
    .groups = "drop"
  )

write_table(location_overview, "01_location_overview_all_products.csv")

common_location_overview <- model_liking_common |>
  group_by(test_location) |>
  summarise(
    n_evaluations = n(),
    n_consumers = n_distinct(consumer),
    n_products = n_distinct(product),
    liking_mean = mean(liking, na.rm = TRUE),
    liking_sd = sd(liking, na.rm = TRUE),
    jar_dev_count_mean = mean(jar_dev_count, na.rm = TRUE),
    jar_severity_mean = mean(jar_severity, na.rm = TRUE),
    .groups = "drop"
  )

write_table(common_location_overview, "02_location_overview_common_products.csv")

context_gain_tests <- tibble::tibble(
  statistic = c(
    "mean_home_liking",
    "mean_lab_liking",
    "mean_context_gain_home_minus_lab",
    "paired_t_statistic",
    "paired_t_p_value",
    "pearson_home_lab_liking",
    "spearman_home_lab_liking",
    "pearson_context_gain_vs_jar_dev_gap",
    "pearson_context_gain_vs_jar_severity_gap"
  ),
  value = c(
    mean(context_gain$liking_mean_home, na.rm = TRUE),
    mean(context_gain$liking_mean_lab, na.rm = TRUE),
    mean(context_gain$context_gain_liking, na.rm = TRUE),
    unname(t.test(context_gain$liking_mean_home,
                  context_gain$liking_mean_lab,
                  paired = TRUE)$statistic),
    t.test(context_gain$liking_mean_home,
           context_gain$liking_mean_lab,
           paired = TRUE)$p.value,
    cor(context_gain$liking_mean_home,
        context_gain$liking_mean_lab,
        use = "complete.obs",
        method = "pearson"),
    cor(context_gain$liking_mean_home,
        context_gain$liking_mean_lab,
        use = "complete.obs",
        method = "spearman"),
    cor(context_gain$context_gain_liking,
        context_gain$context_gain_jar_dev_count,
        use = "complete.obs",
        method = "pearson"),
    cor(context_gain$context_gain_liking,
        context_gain$context_gain_jar_severity,
        use = "complete.obs",
        method = "pearson")
  )
)

write_table(context_gain_tests, "03_context_gain_tests.csv")

m_context_gain_dev <- lm(
  context_gain_liking ~ context_gain_jar_dev_count,
  data = context_gain
)

m_context_gain_severity <- lm(
  context_gain_liking ~ context_gain_jar_severity,
  data = context_gain
)

write_model_summary(m_context_gain_dev, "m_context_gain_dev_lm.txt")
write_model_summary(m_context_gain_severity, "m_context_gain_severity_lm.txt")

context_gain_lm_tables <- bind_rows(
  tidy_lm(m_context_gain_dev, conf.int = TRUE) |>
    mutate(model = "context_gain_liking ~ context_gain_jar_dev_count"),
  tidy_lm(m_context_gain_severity, conf.int = TRUE) |>
    mutate(model = "context_gain_liking ~ context_gain_jar_severity")
) |>
  relocate(model)

write_table(context_gain_lm_tables, "04_context_gain_lm_coefficients.csv")

context_gain_loo <- dplyr::bind_rows(lapply(
  as.character(context_gain$product),
  function(p) {
    d <- context_gain |> filter(as.character(product) != p)
    tibble::tibble(
      omitted_product = p,
      n_products = nrow(d),
      r_gain_vs_jar_dev_gap = cor(
        d$context_gain_liking,
        d$context_gain_jar_dev_count,
        use = "complete.obs"
      ),
      r_gain_vs_jar_severity_gap = cor(
        d$context_gain_liking,
        d$context_gain_jar_severity,
        use = "complete.obs"
      ),
      mean_context_gain = mean(d$context_gain_liking, na.rm = TRUE)
    )
  }
))

write_table(context_gain_loo, "05_context_gain_leave_one_out.csv")

context_gain_no_j09 <- context_gain |> filter(as.character(product) != "J09")

if (nrow(context_gain_no_j09) >= 3) {
  context_gain_no_j09_tests <- tibble::tibble(
    statistic = c(
      "n_products",
      "mean_context_gain_no_J09",
      "paired_t_p_value_no_J09",
      "pearson_context_gain_vs_jar_dev_gap_no_J09",
      "pearson_context_gain_vs_jar_severity_gap_no_J09"
    ),
    value = c(
      nrow(context_gain_no_j09),
      mean(context_gain_no_j09$context_gain_liking, na.rm = TRUE),
      t.test(context_gain_no_j09$liking_mean_home,
             context_gain_no_j09$liking_mean_lab,
             paired = TRUE)$p.value,
      cor(context_gain_no_j09$context_gain_liking,
          context_gain_no_j09$context_gain_jar_dev_count,
          use = "complete.obs"),
      cor(context_gain_no_j09$context_gain_liking,
          context_gain_no_j09$context_gain_jar_severity,
          use = "complete.obs")
    )
  )
  write_table(context_gain_no_j09_tests, "06_context_gain_sensitivity_no_J09.csv")
}

p_home_lab <- context_gain |>
  ggplot(aes(x = liking_mean_lab, y = liking_mean_home, label = product)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_point(size = 2.4) +
  ggrepel::geom_text_repel(size = 3, max.overlaps = Inf) +
  coord_equal(xlim = c(0, 10), ylim = c(0, 10)) +
  labs(
    x = "Blind lab liking, product mean",
    y = "Home-use liking, product mean",
    title = "Home-use vs blind laboratory liking",
    subtitle = "Common products only; dashed line indicates equal liking"
  ) +
  theme_bw()

save_plot(p_home_lab, "01_home_lab_liking_scatter.png", width = 6, height = 6)

p_context_gain <- context_gain |>
  ggplot(aes(
    x = context_gain_jar_dev_count,
    y = context_gain_liking,
    label = product
  )) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dotted") +
  geom_point(size = 2.4) +
  geom_smooth(
    mapping = aes(x = context_gain_jar_dev_count, y = context_gain_liking),
    method = "lm",
    se = TRUE,
    inherit.aes = FALSE
  ) +
  ggrepel::geom_text_repel(size = 3, max.overlaps = Inf) +
  labs(
    x = "Home - lab difference in mean JAR deviation count",
    y = "Home - lab difference in mean liking",
    title = "Context gain in liking is associated with JAR-deviation gap"
  ) +
  theme_bw()

save_plot(p_context_gain, "02_context_gain_vs_jar_gap.png", width = 7, height = 5)

jar_burden_summary_all <- model_liking |>
  group_by(test_location, jar_dev_count) |>
  summarise(
    n = n(),
    liking_mean = mean(liking, na.rm = TRUE),
    liking_sd = sd(liking, na.rm = TRUE),
    liking_se = se_or_na(liking),
    liking_ci95 = ci95_or_na(liking),
    .groups = "drop"
  ) |>
  arrange(test_location, jar_dev_count)

write_table(jar_burden_summary_all, "07_jar_burden_liking_all_products.csv")

jar_burden_summary_common <- model_liking_common |>
  group_by(test_location, jar_dev_count) |>
  summarise(
    n = n(),
    liking_mean = mean(liking, na.rm = TRUE),
    liking_sd = sd(liking, na.rm = TRUE),
    liking_se = se_or_na(liking),
    liking_ci95 = ci95_or_na(liking),
    .groups = "drop"
  ) |>
  arrange(test_location, jar_dev_count)

write_table(jar_burden_summary_common, "08_jar_burden_liking_common_products.csv")

jar_attribute_deviation_rate <- model_liking |>
  group_by(test_location) |>
  summarise(
    n = n(),
    color_dev_rate = mean(color_dev == 1, na.rm = TRUE),
    fat_dev_rate = mean(fat_dev == 1, na.rm = TRUE),
    salt_dev_rate = mean(salt_dev == 1, na.rm = TRUE),
    tender_dev_rate = mean(tender_dev == 1, na.rm = TRUE),
    jar_dev_count_mean = mean(jar_dev_count, na.rm = TRUE),
    jar_severity_mean = mean(jar_severity, na.rm = TRUE),
    .groups = "drop"
  )

write_table(jar_attribute_deviation_rate, "09_jar_attribute_deviation_rate_by_location.csv")

p_jar_burden <- jar_burden_summary_common |>
  ggplot(aes(
    x = jar_dev_count,
    y = liking_mean,
    group = test_location,
    shape = test_location,
    linetype = test_location
  )) +
  geom_line() +
  geom_point(size = 2.8) +
  geom_errorbar(
    aes(ymin = liking_mean - liking_ci95,
        ymax = liking_mean + liking_ci95),
    width = 0.08
  ) +
  scale_x_continuous(breaks = 0:4) +
  labs(
    x = "Number of attributes deviating from JAR",
    y = "Mean liking",
    title = "Liking decreases monotonically with cumulative JAR deviations",
    subtitle = "Common home/lab products"
  ) +
  theme_bw()

save_plot(p_jar_burden, "03_jar_burden_liking_common_products.png", width = 7, height = 5)

m_liking_01_location <- lmer(
  liking ~ test_location + product + (1 | consumer),
  data = model_liking_common,
  REML = FALSE,
  control = lmer_control
)

m_liking_02_add_jar_count <- lmer(
  liking ~ test_location + jar_dev_count + product + (1 | consumer),
  data = model_liking_common,
  REML = FALSE,
  control = lmer_control
)

m_liking_03_location_x_jar_count <- lmer(
  liking ~ test_location * jar_dev_count + product + (1 | consumer),
  data = model_liking_common,
  REML = FALSE,
  control = lmer_control
)

m_liking_04_jar_count_factor <- lmer(
  liking ~ test_location * jar_dev_count_f + product + (1 | consumer),
  data = model_liking_common,
  REML = FALSE,
  control = lmer_control
)

m_liking_02_random_product <- lmer(
  liking ~ test_location + jar_dev_count + (1 | product) + (1 | consumer),
  data = model_liking_common,
  REML = FALSE,
  control = lmer_control
)

write_model_summary(m_liking_01_location, "m_liking_01_location_product_fixed.txt")
write_model_summary(m_liking_02_add_jar_count, "m_liking_02_add_jar_count_product_fixed.txt")
write_model_summary(m_liking_03_location_x_jar_count, "m_liking_03_location_x_jar_count_product_fixed.txt")
write_model_summary(m_liking_04_jar_count_factor, "m_liking_04_jar_count_factor_product_fixed.txt")
write_model_summary(m_liking_02_random_product, "m_liking_02_random_product.txt")

liking_model_comparison <- anova(
  m_liking_01_location,
  m_liking_02_add_jar_count,
  m_liking_03_location_x_jar_count
) |>
  as.data.frame() |>
  tibble::rownames_to_column("model")

write_table(liking_model_comparison, "10_liking_model_comparison_product_fixed.csv")

liking_fixed_effects <- bind_rows(
  tidy_mixed_fixed(m_liking_01_location, conf.int = TRUE) |>
    mutate(model = "location + product"),
  tidy_mixed_fixed(m_liking_02_add_jar_count, conf.int = TRUE) |>
    mutate(model = "location + JAR count + product"),
  tidy_mixed_fixed(m_liking_03_location_x_jar_count, conf.int = TRUE) |>
    mutate(model = "location * JAR count + product"),
  tidy_mixed_fixed(m_liking_02_random_product, conf.int = TRUE) |>
    mutate(model = "location + JAR count + random product")
) |>
  relocate(model)

write_table(liking_fixed_effects, "11_liking_model_fixed_effects.csv")

liking_singularity <- tibble::tibble(
  model = c(
    "m_liking_01_location",
    "m_liking_02_add_jar_count",
    "m_liking_03_location_x_jar_count",
    "m_liking_04_jar_count_factor",
    "m_liking_02_random_product"
  ),
  singular = c(
    lme4::isSingular(m_liking_01_location),
    lme4::isSingular(m_liking_02_add_jar_count),
    lme4::isSingular(m_liking_03_location_x_jar_count),
    lme4::isSingular(m_liking_04_jar_count_factor),
    lme4::isSingular(m_liking_02_random_product)
  )
)

write_table(liking_singularity, "12_liking_model_singularity.csv")

emm_location_m1 <- emmeans::emmeans(m_liking_01_location, ~ test_location)
emm_location_m2 <- emmeans::emmeans(m_liking_02_add_jar_count, ~ test_location)
emm_jar_factor <- emmeans::emmeans(
  m_liking_04_jar_count_factor,
  ~ jar_dev_count_f | test_location
)

write_table(
  as.data.frame(emm_location_m1),
  "13_emmeans_location_without_jar_adjustment.csv"
)
write_table(
  as.data.frame(emm_location_m2),
  "14_emmeans_location_with_jar_adjustment.csv"
)
write_table(
  as.data.frame(emm_jar_factor),
  "15_emmeans_jar_count_by_location.csv"
)

direction_vars <- c(
  "color_too_low",  "color_too_high",
  "fat_too_low",    "fat_too_high",
  "salt_too_low",   "salt_too_high",
  "tender_too_low", "tender_too_high"
)

missing_direction_vars <- setdiff(direction_vars, names(model_liking_common))
if (length(missing_direction_vars) > 0) {
  stop("Missing direction-specific JAR variables: ",
       paste(missing_direction_vars, collapse = ", "))
}

formula_direction <- as.formula(
  paste(
    "liking ~ test_location +",
    paste(direction_vars, collapse = " + "),
    "+ product + (1 | consumer)"
  )
)

m_direction <- lmer(
  formula_direction,
  data = model_liking_common,
  REML = FALSE,
  control = lmer_control
)

formula_direction_interaction <- as.formula(
  paste(
    "liking ~ test_location * (",
    paste(direction_vars, collapse = " + "),
    ") + product + (1 | consumer)"
  )
)

m_direction_interaction <- lmer(
  formula_direction_interaction,
  data = model_liking_common,
  REML = FALSE,
  control = lmer_control
)

write_model_summary(m_direction, "m_direction_specific_jar_penalty.txt")
write_model_summary(m_direction_interaction, "m_direction_specific_jar_penalty_interactions.txt")

direction_model_comparison <- anova(m_direction, m_direction_interaction) |>
  as.data.frame() |>
  tibble::rownames_to_column("model")

write_table(direction_model_comparison, "16_direction_model_interaction_test.csv")

direction_labels <- tibble::tibble(
  term = direction_vars,
  label = c(
    "Color too low",
    "Color too high",
    "Fat too low",
    "Fat too high",
    "Salt too low",
    "Salt too high",
    "Tenderness too low",
    "Tenderness too high"
  ),
  attribute = c(
    "Color", "Color",
    "Fat", "Fat",
    "Salt", "Salt",
    "Tenderness", "Tenderness"
  ),
  direction = c(
    "Too low", "Too high",
    "Too low", "Too high",
    "Too low", "Too high",
    "Too low", "Too high"
  )
)

direction_effects <- tidy_mixed_fixed(
  m_direction,
  conf.int = TRUE
) |>
  filter(term %in% direction_vars) |>
  left_join(direction_labels, by = "term") |>
  arrange(estimate)

write_table(direction_effects, "17_direction_specific_jar_penalties.csv")

p_direction <- direction_effects |>
  mutate(label = reorder(label, estimate)) |>
  ggplot(aes(x = estimate, y = label)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(size = 2.4) +
  geom_errorbar(
    aes(xmin = conf.low, xmax = conf.high),
    width = 0.2,
    orientation = "y"
  ) +
  labs(
    x = "Estimated penalty on liking",
    y = NULL,
    title = "Direction-specific multivariate JAR penalties",
    subtitle = "Adjusted for all JAR deviations, product, test location and consumer"
  ) +
  theme_bw()

save_plot(p_direction, "04_direction_specific_jar_penalties.png", width = 7, height = 5)

product_jar_summary <- model_liking |>
  group_by(product, test_location) |>
  summarise(
    n = n(),
    liking_mean = mean_or_na(liking),
    jar_color_mean = mean_or_na(jar_color),
    jar_fat_mean = mean_or_na(jar_fat),
    jar_salt_mean = mean_or_na(jar_salt),
    jar_tender_mean = mean_or_na(jar_tender),
    jar_dev_count_mean = mean_or_na(jar_dev_count),
    jar_severity_mean = mean_or_na(jar_severity),
    salt_measured_mean = first(salt_measured_mean),
    fat_measured_mean = first(fat_measured_mean),
    .groups = "drop"
  )

write_table(product_jar_summary, "18_product_context_mean_jar_and_composition.csv")

composition_correlations <- product_jar_summary |>
  group_by(test_location) |>
  summarise(
    n_products = n_distinct(product),
    cor_salt_measured_jarsalt = cor(
      salt_measured_mean,
      jar_salt_mean,
      use = "complete.obs",
      method = "pearson"
    ),
    cor_fat_measured_jarfat = cor(
      fat_measured_mean,
      jar_fat_mean,
      use = "complete.obs",
      method = "pearson"
    ),
    cor_salt_measured_liking = cor(
      salt_measured_mean,
      liking_mean,
      use = "complete.obs",
      method = "pearson"
    ),
    cor_fat_measured_liking = cor(
      fat_measured_mean,
      liking_mean,
      use = "complete.obs",
      method = "pearson"
    ),
    .groups = "drop"
  )

write_table(composition_correlations, "19_composition_product_level_correlations.csv")

composition_model <- model_liking_common |>
  filter(
    !is.na(jar_salt),
    !is.na(jar_fat),
    !is.na(salt_measured_mean),
    !is.na(fat_measured_mean)
  ) |>
  mutate(
    salt_z = std(salt_measured_mean),
    fat_z = std(fat_measured_mean)
  )

m_jarsalt_composition <- lmer(
  jar_salt ~ salt_z * test_location + (1 | product) + (1 | consumer),
  data = composition_model,
  REML = FALSE,
  control = lmer_control
)

m_jarfat_composition <- lmer(
  jar_fat ~ fat_z * test_location + (1 | product) + (1 | consumer),
  data = composition_model,
  REML = FALSE,
  control = lmer_control
)

write_model_summary(m_jarsalt_composition, "m_jarsalt_measured_salt_context.txt")
write_model_summary(m_jarfat_composition, "m_jarfat_measured_fat_context.txt")

composition_model_effects <- bind_rows(
  tidy_mixed_fixed(m_jarsalt_composition, conf.int = TRUE) |>
    mutate(model = "JARSalt ~ measured salt * context"),
  tidy_mixed_fixed(m_jarfat_composition, conf.int = TRUE) |>
    mutate(model = "JARFat ~ measured fat * context")
) |>
  relocate(model)

write_table(composition_model_effects, "20_composition_mixed_model_fixed_effects.csv")

p_salt <- product_jar_summary |>
  filter(!is.na(salt_measured_mean), !is.na(jar_salt_mean)) |>
  ggplot(aes(
    x = salt_measured_mean,
    y = jar_salt_mean,
    label = product,
    shape = test_location
  )) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(size = 2.4) +
  geom_smooth(
    mapping = aes(x = salt_measured_mean, y = jar_salt_mean, linetype = test_location),
    method = "lm",
    se = FALSE,
    inherit.aes = FALSE
  ) +
  ggrepel::geom_text_repel(size = 2.8, max.overlaps = 20) +
  labs(
    x = "Measured salt content",
    y = "Mean JARSalt score",
    title = "Measured salt content and perceived salt JAR",
    subtitle = "Negative = not salty enough; positive = too salty"
  ) +
  theme_bw()

save_plot(p_salt, "05_measured_salt_vs_jarsalt.png", width = 7, height = 5)

p_fat <- product_jar_summary |>
  filter(!is.na(fat_measured_mean), !is.na(jar_fat_mean)) |>
  ggplot(aes(
    x = fat_measured_mean,
    y = jar_fat_mean,
    label = product,
    shape = test_location
  )) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(size = 2.4) +
  geom_smooth(
    mapping = aes(x = fat_measured_mean, y = jar_fat_mean, linetype = test_location),
    method = "lm",
    se = FALSE,
    inherit.aes = FALSE
  ) +
  ggrepel::geom_text_repel(size = 2.8, max.overlaps = 20) +
  labs(
    x = "Measured fat content",
    y = "Mean JARFat score",
    title = "Measured fat content and perceived fat JAR",
    subtitle = "Negative = not fatty enough; positive = too fatty"
  ) +
  theme_bw()

save_plot(p_fat, "06_measured_fat_vs_jarfat.png", width = 7, height = 5)

purchase_descriptive_by_intent <- model_purchase |>
  group_by(purchase_intent_f) |>
  summarise(
    n = n(),
    liking_mean = mean_or_na(liking),
    liking_sd = sd_or_na(liking),
    jar_dev_count_mean = mean_or_na(jar_dev_count),
    jar_severity_mean = mean_or_na(jar_severity),
    price_mean = mean_or_na(price),
    usual_bought_rate = mean(ham_usually_bought == 1, na.rm = TRUE),
    .groups = "drop"
  )

write_table(purchase_descriptive_by_intent, "21_purchase_descriptive_by_intent.csv")

purchase_descriptive_by_usual <- model_purchase |>
  group_by(ham_usually_bought_f) |>
  summarise(
    n = n(),
    liking_mean = mean_or_na(liking),
    purchase_intent_mean = mean_or_na(purchase_intent),
    pct_purchase_yes = mean(purchase_yes == 1, na.rm = TRUE),
    jar_dev_count_mean = mean_or_na(jar_dev_count),
    price_mean = mean_or_na(price),
    .groups = "drop"
  )

write_table(purchase_descriptive_by_usual, "22_purchase_descriptive_by_usual_purchase.csv")

purchase_model_data <- model_purchase |>
  filter(
    !is.na(purchase_intent),
    !is.na(purchase_yes),
    !is.na(liking),
    !is.na(jar_dev_count),
    !is.na(price),
    !is.na(ham_usually_bought_f)
  ) |>
  mutate(
    liking_z = std(liking),
    price_z = std(price),
    jar_dev_count_z = std(jar_dev_count)
  )

m_purchase_lmm <- lmer(
  purchase_intent ~ liking_z + price_z + ham_usually_bought_f +
    jar_dev_count + (1 | product) + (1 | consumer),
  data = purchase_model_data,
  REML = FALSE,
  control = lmer_control
)

m_purchase_lmm_product_fixed <- lmer(
  purchase_intent ~ liking_z + price_z + ham_usually_bought_f +
    jar_dev_count + product + (1 | consumer),
  data = purchase_model_data,
  REML = FALSE,
  control = lmer_control
)

m_purchase_glmer_yes <- glmer(
  purchase_yes ~ liking_z + price_z + ham_usually_bought_f +
    jar_dev_count + (1 | product) + (1 | consumer),
  data = purchase_model_data,
  family = binomial,
  control = glmer_control
)

m_purchase_clmm <- tryCatch(
  ordinal::clmm(
    purchase_intent_ord ~ liking_z + price_z + ham_usually_bought_f +
      jar_dev_count + (1 | product) + (1 | consumer),
    data = purchase_model_data,
    Hess = TRUE,
    nAGQ = 1
  ),
  error = function(e) e
)

write_model_summary(m_purchase_lmm, "m_purchase_intent_lmm_random_product.txt")
write_model_summary(m_purchase_lmm_product_fixed, "m_purchase_intent_lmm_product_fixed.txt")
write_model_summary(m_purchase_glmer_yes, "m_purchase_yes_glmer_random_product.txt")

if (inherits(m_purchase_clmm, "error")) {
  writeLines(
    c("CLMM failed:", m_purchase_clmm$message),
    file.path(model_dir, "m_purchase_intent_clmm_error.txt")
  )
} else {
  write_model_summary(m_purchase_clmm, "m_purchase_intent_clmm_random_product.txt")
}

purchase_lmm_effects <- tidy_mixed_fixed(
  m_purchase_lmm,
  conf.int = TRUE
) |>
  mutate(model = "purchase_intent_lmm_random_product") |>
  relocate(model)

purchase_lmm_fixedproduct_effects <- tidy_mixed_fixed(
  m_purchase_lmm_product_fixed,
  conf.int = TRUE
) |>
  mutate(model = "purchase_intent_lmm_product_fixed") |>
  relocate(model)

purchase_glmer_effects <- tidy_mixed_fixed(
  m_purchase_glmer_yes,
  conf.int = TRUE
) |>
  mutate(
    model = "purchase_yes_glmer_random_product",
    odds_ratio = exp(estimate),
    odds_ratio_low = exp(conf.low),
    odds_ratio_high = exp(conf.high)
  ) |>
  relocate(model)

write_table(purchase_lmm_effects, "23_purchase_lmm_fixed_effects.csv")
write_table(purchase_lmm_fixedproduct_effects, "24_purchase_lmm_product_fixed_effects.csv")
write_table(purchase_glmer_effects, "25_purchase_yes_glmer_fixed_effects.csv")

if (!inherits(m_purchase_clmm, "error")) {
  purchase_clmm_summary <- as.data.frame(coef(summary(m_purchase_clmm))) |>
    tibble::rownames_to_column("term")
  write_table(purchase_clmm_summary, "26_purchase_clmm_summary.csv")
}

p_purchase_liking <- purchase_descriptive_by_intent |>
  ggplot(aes(x = purchase_intent_f, y = liking_mean)) +
  geom_col() +
  labs(
    x = "Purchase intent",
    y = "Mean liking",
    title = "Liking by purchase intent"
  ) +
  theme_bw()

save_plot(p_purchase_liking, "07_purchase_intent_liking_mean.png", width = 6, height = 4)

p_purchase_jar <- purchase_descriptive_by_intent |>
  ggplot(aes(x = purchase_intent_f, y = jar_dev_count_mean)) +
  geom_col() +
  labs(
    x = "Purchase intent",
    y = "Mean JAR deviation count",
    title = "JAR deviation burden by purchase intent"
  ) +
  theme_bw()

save_plot(p_purchase_jar, "08_purchase_intent_jar_deviation_mean.png", width = 6, height = 4)

purchase_coef_plot_data <- purchase_lmm_effects |>
  filter(term %in% c(
    "liking_z",
    "price_z",
    "ham_usually_bought_fyes",
    "jar_dev_count"
  )) |>
  mutate(
    label = recode(
      term,
      liking_z = "Liking, z",
      price_z = "Price, z",
      ham_usually_bought_fyes = "Usually bought: yes",
      jar_dev_count = "JAR deviation count"
    ),
    label = reorder(label, estimate)
  )

p_purchase_coef <- purchase_coef_plot_data |>
  ggplot(aes(x = estimate, y = label)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(size = 2.4) +
  geom_errorbar(
    aes(xmin = conf.low, xmax = conf.high),
    width = 0.2,
    orientation = "y"
  ) +
  labs(
    x = "Estimated effect on purchase intent",
    y = NULL,
    title = "Purchase intent model",
    subtitle = "Linear mixed model; product and consumer random intercepts"
  ) +
  theme_bw()

save_plot(p_purchase_coef, "09_purchase_intent_lmm_coefficients.png", width = 7, height = 4.5)

product_exploratory <- product_panel |>
  select(
    product,
    any_of(c(
      "brand_type",
      "company_code",
      "organic",
      "no_nitrite",
      "salt_light",
      "fat_light",
      "label_rouge",
      "nutriscore",
      "salt_measured_mean",
      "fat_measured_mean",
      "liking_mean_home",
      "liking_mean_lab",
      "context_gain_liking",
      "jar_dev_count_mean_home",
      "jar_dev_count_mean_lab",
      "context_gain_jar_dev_count",
      "price_mean",
      "usual_bought_rate",
      "purchase_intent_mean",
      "pct_purchase_yes"
    ))
  ) |>
  arrange(desc(context_gain_liking))

write_table(product_exploratory, "27_product_exploratory_panel_for_discussion.csv")

models <- list(
  m_context_gain_dev = m_context_gain_dev,
  m_context_gain_severity = m_context_gain_severity,
  m_liking_01_location = m_liking_01_location,
  m_liking_02_add_jar_count = m_liking_02_add_jar_count,
  m_liking_03_location_x_jar_count = m_liking_03_location_x_jar_count,
  m_liking_04_jar_count_factor = m_liking_04_jar_count_factor,
  m_liking_02_random_product = m_liking_02_random_product,
  m_direction = m_direction,
  m_direction_interaction = m_direction_interaction,
  m_jarsalt_composition = m_jarsalt_composition,
  m_jarfat_composition = m_jarfat_composition,
  m_purchase_lmm = m_purchase_lmm,
  m_purchase_lmm_product_fixed = m_purchase_lmm_product_fixed,
  m_purchase_glmer_yes = m_purchase_glmer_yes,
  m_purchase_clmm = m_purchase_clmm
)

saveRDS(models, file.path(model_dir, "all_models_no_text.rds"))

writeLines(
  capture.output(sessionInfo()),
  file.path(analysis_dir, "session_info.txt")
)

root_dir <- HAM_ROOT_DIR
setwd(root_dir)

processed_dir <- file.path(root_dir, "data_processed")
out_root <- file.path(root_dir, "analysis_outputs", "sensitivity_checks")
table_dir <- file.path(out_root, "tables")
figure_dir <- file.path(out_root, "figures")
model_dir <- file.path(out_root, "models")

dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(model_dir, recursive = TRUE, showWarnings = FALSE)

read_processed <- function(name) {
  rds_path <- file.path(processed_dir, paste0(name, ".rds"))
  csv_path <- file.path(processed_dir, paste0(name, ".csv"))
  if (file.exists(rds_path)) {
    readRDS(rds_path)
  } else if (file.exists(csv_path)) {
    readr::read_csv(csv_path, show_col_types = FALSE)
  } else {
    stop("Cannot find ", name, ".rds or ", name, ".csv in ", processed_dir)
  }
}

model_liking <- read_processed("model_liking")
model_liking_common_products <- read_processed("model_liking_common_products")
model_purchase <- read_processed("model_purchase")
context_gain <- read_processed("context_gain")
product_panel <- read_processed("product_panel")

model_liking_common_products <- model_liking_common_products |>
  mutate(
    product = factor(product),
    consumer = factor(consumer),
    test_location = factor(test_location, levels = c("home", "lab"))
  )

model_purchase <- model_purchase |>
  mutate(
    product = factor(product),
    consumer = factor(consumer),
    purchase_intent_ord = ordered(
      purchase_intent,
      levels = c(-1, 0, 1),
      labels = c("no", "uncertain", "yes")
    ),
    ham_usually_bought_f = factor(
      ham_usually_bought,
      levels = c(0, 1),
      labels = c("no", "yes")
    )
  )

write_model_summary <- function(model, filename) {
  sink(file.path(model_dir, filename))
  print(summary(model))
  sink()
}

fixed_effects_table <- function(model, model_name = NA_character_) {
  s <- summary(model)
  cm <- as.data.frame(coef(s))
  cm$term <- rownames(cm)
  rownames(cm) <- NULL

  names(cm) <- gsub("Estimate", "estimate", names(cm), fixed = TRUE)
  names(cm) <- gsub("Std. Error", "std_error", names(cm), fixed = TRUE)
  names(cm) <- gsub("t value", "statistic", names(cm), fixed = TRUE)
  names(cm) <- gsub("z value", "statistic", names(cm), fixed = TRUE)

  p_col <- grep("^Pr\\(", names(cm), value = TRUE)
  if (length(p_col) == 1) {
    cm$p_value <- cm[[p_col]]
  } else {
    cm$p_value <- NA_real_
  }

  if (!"std_error" %in% names(cm)) cm$std_error <- NA_real_
  if (!"estimate" %in% names(cm)) cm$estimate <- NA_real_

  cm |>
    mutate(
      conf_low = estimate - 1.96 * std_error,
      conf_high = estimate + 1.96 * std_error,
      model = model_name
    ) |>
    select(model, term, estimate, std_error, conf_low, conf_high, everything())
}

model_info_table <- function(..., model_names = NULL) {
  models <- list(...)
  if (is.null(model_names)) model_names <- paste0("model_", seq_along(models))
  tibble(
    model = model_names,
    nobs = vapply(models, stats::nobs, numeric(1)),
    logLik = vapply(models, function(x) as.numeric(logLik(x)), numeric(1)),
    AIC = vapply(models, AIC, numeric(1)),
    BIC = vapply(models, BIC, numeric(1))
  )
}

safe_lmer <- function(formula, data, REML = FALSE) {
  lmerTest::lmer(
    formula,
    data = data,
    REML = REML,
    control = lme4::lmerControl(
      optimizer = "bobyqa",
      optCtrl = list(maxfun = 2e5),
      check.conv.singular = "ignore"
    )
  )
}

safe_cor <- function(x, y, method = "pearson") {
  ok <- is.finite(x) & is.finite(y)
  if (sum(ok) < 3) return(NA_real_)
  suppressWarnings(cor(x[ok], y[ok], method = method))
}

normal_p <- function(est, se) {
  z <- est / se
  2 * pnorm(abs(z), lower.tail = FALSE)
}

fit_location_jar_models <- function(dat, tag) {
  dat <- droplevels(dat)

  m_location <- safe_lmer(
    liking ~ test_location + product + (1 | consumer),
    data = dat,
    REML = FALSE
  )

  m_location_jar <- safe_lmer(
    liking ~ test_location + jar_dev_count + product + (1 | consumer),
    data = dat,
    REML = FALSE
  )

  m_location_jar_int <- safe_lmer(
    liking ~ test_location * jar_dev_count + product + (1 | consumer),
    data = dat,
    REML = FALSE
  )

  comparison <- model_info_table(
    m_location, m_location_jar, m_location_jar_int,
    model_names = c("location_product", "location_jar_product", "location_jar_interaction_product")
  )

  lrt_jar <- as.data.frame(anova(m_location, m_location_jar)) |>
    tibble::rownames_to_column("model") |>
    mutate(comparison = "add_jar_dev_count")

  lrt_int <- as.data.frame(anova(m_location_jar, m_location_jar_int)) |>
    tibble::rownames_to_column("model") |>
    mutate(comparison = "add_location_x_jar_interaction")

  fixed <- bind_rows(
    fixed_effects_table(m_location, "location_product"),
    fixed_effects_table(m_location_jar, "location_jar_product"),
    fixed_effects_table(m_location_jar_int, "location_jar_interaction_product")
  )

  write_csv(comparison, file.path(table_dir, paste0("01_", tag, "_model_info.csv")))
  write_csv(bind_rows(lrt_jar, lrt_int), file.path(table_dir, paste0("02_", tag, "_lrt.csv")))
  write_csv(fixed, file.path(table_dir, paste0("03_", tag, "_fixed_effects.csv")))

  write_model_summary(m_location, paste0("01_", tag, "_location_product.txt"))
  write_model_summary(m_location_jar, paste0("02_", tag, "_location_jar_product.txt"))
  write_model_summary(m_location_jar_int, paste0("03_", tag, "_location_jar_interaction_product.txt"))

  invisible(list(
    location = m_location,
    location_jar = m_location_jar,
    location_jar_interaction = m_location_jar_int,
    comparison = comparison,
    fixed = fixed
  ))
}

common_no_j09 <- model_liking_common_products |>
  filter(as.character(product) != "J09") |>
  droplevels()

j09_models <- fit_location_jar_models(common_no_j09, tag = "exclude_J09")

context_gain_no_j09 <- context_gain |>
  filter(as.character(product) != "J09")

j09_context_summary <- tibble(
  analysis = c("all_common_products", "exclude_J09"),
  n_products = c(nrow(context_gain), nrow(context_gain_no_j09)),
  r_liking_vs_jar_dev_count_gap = c(
    safe_cor(context_gain$context_gain_liking, context_gain$context_gain_jar_dev_count, "pearson"),
    safe_cor(context_gain_no_j09$context_gain_liking, context_gain_no_j09$context_gain_jar_dev_count, "pearson")
  ),
  r_liking_vs_jar_severity_gap = c(
    safe_cor(context_gain$context_gain_liking, context_gain$context_gain_jar_severity, "pearson"),
    safe_cor(context_gain_no_j09$context_gain_liking, context_gain_no_j09$context_gain_jar_severity, "pearson")
  ),
  spearman_liking_vs_jar_dev_count_gap = c(
    safe_cor(context_gain$context_gain_liking, context_gain$context_gain_jar_dev_count, "spearman"),
    safe_cor(context_gain_no_j09$context_gain_liking, context_gain_no_j09$context_gain_jar_dev_count, "spearman")
  ),
  spearman_liking_vs_jar_severity_gap = c(
    safe_cor(context_gain$context_gain_liking, context_gain$context_gain_jar_severity, "spearman"),
    safe_cor(context_gain_no_j09$context_gain_liking, context_gain_no_j09$context_gain_jar_severity, "spearman")
  )
)
write_csv(j09_context_summary, file.path(table_dir, "04_context_gain_correlations_exclude_J09.csv"))

loo_rows <- vector("list", nrow(context_gain))
for (i in seq_len(nrow(context_gain))) {
  omitted <- as.character(context_gain$product[i])
  dat_i <- context_gain |>
    filter(as.character(product) != omitted)

  lm_count <- lm(context_gain_liking ~ context_gain_jar_dev_count, data = dat_i)
  lm_sev <- lm(context_gain_liking ~ context_gain_jar_severity, data = dat_i)

  loo_rows[[i]] <- tibble(
    omitted_product = omitted,
    n_products = nrow(dat_i),
    pearson_liking_vs_jar_dev_count_gap = safe_cor(dat_i$context_gain_liking, dat_i$context_gain_jar_dev_count, "pearson"),
    spearman_liking_vs_jar_dev_count_gap = safe_cor(dat_i$context_gain_liking, dat_i$context_gain_jar_dev_count, "spearman"),
    pearson_liking_vs_jar_severity_gap = safe_cor(dat_i$context_gain_liking, dat_i$context_gain_jar_severity, "pearson"),
    spearman_liking_vs_jar_severity_gap = safe_cor(dat_i$context_gain_liking, dat_i$context_gain_jar_severity, "spearman"),
    slope_jar_dev_count_gap = coef(lm_count)[["context_gain_jar_dev_count"]],
    p_jar_dev_count_gap = summary(lm_count)$coefficients["context_gain_jar_dev_count", "Pr(>|t|)"],
    slope_jar_severity_gap = coef(lm_sev)[["context_gain_jar_severity"]],
    p_jar_severity_gap = summary(lm_sev)$coefficients["context_gain_jar_severity", "Pr(>|t|)"]
  )
}
loo_context <- bind_rows(loo_rows)
write_csv(loo_context, file.path(table_dir, "05_leave_one_product_out_context_gain_correlations.csv"))

loo_summary <- loo_context |>
  summarise(
    n_leave_one_out = n(),
    min_r_count = min(pearson_liking_vs_jar_dev_count_gap, na.rm = TRUE),
    median_r_count = median(pearson_liking_vs_jar_dev_count_gap, na.rm = TRUE),
    max_r_count = max(pearson_liking_vs_jar_dev_count_gap, na.rm = TRUE),
    min_r_severity = min(pearson_liking_vs_jar_severity_gap, na.rm = TRUE),
    median_r_severity = median(pearson_liking_vs_jar_severity_gap, na.rm = TRUE),
    max_r_severity = max(pearson_liking_vs_jar_severity_gap, na.rm = TRUE)
  )
write_csv(loo_summary, file.path(table_dir, "06_leave_one_product_out_context_gain_summary.csv"))

p_loo <- loo_context |>
  tidyr::pivot_longer(
    cols = c(pearson_liking_vs_jar_dev_count_gap, pearson_liking_vs_jar_severity_gap),
    names_to = "correlation_type",
    values_to = "r"
  ) |>
  ggplot(aes(x = reorder(omitted_product, r), y = r)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point() +
  coord_flip() +
  facet_wrap(~ correlation_type, scales = "free_x") +
  labs(
    title = "Leave-one-product-out correlations",
    x = "Omitted product",
    y = "Pearson correlation"
  ) +
  theme_bw()

ggsave(file.path(figure_dir, "01_leave_one_product_out_correlations.png"), p_loo, width = 8, height = 5, dpi = 300)

jar_direction_terms <- c(
  "color_too_low", "color_too_high",
  "fat_too_low", "fat_too_high",
  "salt_too_low", "salt_too_high",
  "tender_too_low", "tender_too_high"
)

analysis_direction <- model_liking_common_products |>
  filter(
    !is.na(liking),
    !if_any(all_of(jar_direction_terms), is.na),
    !is.na(test_location)
  ) |>
  droplevels()

f_no_int <- as.formula(
  paste(
    "liking ~ test_location +",
    paste(jar_direction_terms, collapse = " + "),
    "+ product + (1 | consumer)"
  )
)

f_int <- as.formula(
  paste(
    "liking ~ test_location * (",
    paste(jar_direction_terms, collapse = " + "),
    ") + product + (1 | consumer)"
  )
)

m_direction_no_int <- safe_lmer(f_no_int, data = analysis_direction, REML = FALSE)
m_direction_int <- safe_lmer(f_int, data = analysis_direction, REML = FALSE)

write_model_summary(m_direction_no_int, "04_direction_specific_no_location_interactions.txt")
write_model_summary(m_direction_int, "05_direction_specific_with_location_interactions.txt")

write_csv(
  bind_rows(
    fixed_effects_table(m_direction_no_int, "direction_specific_no_interaction"),
    fixed_effects_table(m_direction_int, "direction_specific_with_location_interactions")
  ),
  file.path(table_dir, "07_direction_specific_location_interaction_fixed_effects.csv")
)

lrt_direction <- as.data.frame(anova(m_direction_no_int, m_direction_int)) |>
  tibble::rownames_to_column("model")
write_csv(lrt_direction, file.path(table_dir, "08_direction_specific_location_interaction_lrt.csv"))

coefs <- lme4::fixef(m_direction_int)
V <- as.matrix(vcov(m_direction_int))
coef_names <- names(coefs)

get_interaction_name <- function(term) {
  cand1 <- paste0("test_locationlab:", term)
  cand2 <- paste0(term, ":test_locationlab")
  if (cand1 %in% coef_names) return(cand1)
  if (cand2 %in% coef_names) return(cand2)
  NA_character_
}

linear_combo <- function(terms) {
  L <- rep(0, length(coefs))
  names(L) <- coef_names
  terms <- terms[!is.na(terms)]
  for (tt in terms) {
    if (tt %in% names(L)) L[tt] <- L[tt] + 1
  }
  est <- sum(L * coefs)
  se <- sqrt(as.numeric(t(L) %*% V %*% L))
  tibble(
    estimate = est,
    std_error = se,
    conf_low = est - 1.96 * se,
    conf_high = est + 1.96 * se,
    p_value_normal_approx = normal_p(est, se)
  )
}

loc_penalty_rows <- list()
for (term in jar_direction_terms) {
  int_term <- get_interaction_name(term)

  interaction_p <- NA_real_
  fixed_int <- fixed_effects_table(m_direction_int, "direction_specific_with_location_interactions")
  p_match <- fixed_int |> filter(term == int_term) |> pull(p_value)
  if (length(p_match) == 1) interaction_p <- p_match

  home_est <- linear_combo(c(term)) |>
    mutate(location = "home", jar_term = term, interaction_term = int_term, interaction_p_value = interaction_p)

  lab_est <- linear_combo(c(term, int_term)) |>
    mutate(location = "lab", jar_term = term, interaction_term = int_term, interaction_p_value = interaction_p)

  loc_penalty_rows[[length(loc_penalty_rows) + 1]] <- home_est
  loc_penalty_rows[[length(loc_penalty_rows) + 1]] <- lab_est
}

location_specific_penalties <- bind_rows(loc_penalty_rows) |>
  select(jar_term, location, estimate, std_error, conf_low, conf_high, p_value_normal_approx, interaction_term, interaction_p_value)

write_csv(location_specific_penalties, file.path(table_dir, "09_direction_specific_penalties_by_location.csv"))

p_dir_loc <- location_specific_penalties |>
  mutate(
    jar_term = factor(jar_term, levels = rev(jar_direction_terms)),
    location = factor(location, levels = c("home", "lab"))
  ) |>
  ggplot(aes(x = estimate, y = jar_term, shape = location)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(position = position_dodge(width = 0.55)) +
  geom_errorbar(
    aes(xmin = conf_low, xmax = conf_high),
    orientation = "y",
    width = 0.2,
    position = position_dodge(width = 0.55)
  ) +
  labs(
    title = "Direction-specific JAR penalties by location",
    x = "Estimated penalty on liking",
    y = "JAR deviation"
  ) +
  theme_bw()

ggsave(file.path(figure_dir, "02_direction_specific_penalties_by_location.png"), p_dir_loc, width = 8, height = 5, dpi = 300)

analysis_purchase_ord <- model_purchase |>
  filter(
    !is.na(purchase_intent_ord),
    !is.na(liking),
    !is.na(price),
    !is.na(ham_usually_bought_f),
    !is.na(jar_dev_count)
  ) |>
  mutate(
    liking_z = as.numeric(scale(liking)),
    price_z = as.numeric(scale(price)),
    product = factor(product),
    consumer = factor(consumer)
  ) |>
  droplevels()

clmm_formula_full <- purchase_intent_ord ~ liking_z + price_z + ham_usually_bought_f + jar_dev_count +
  (1 | consumer) + (1 | product)

clmm_formula_consumer_only <- purchase_intent_ord ~ liking_z + price_z + ham_usually_bought_f + jar_dev_count +
  (1 | consumer)

clmm_fit_status <- "full_consumer_and_product_random_intercepts"

m_purchase_clmm <- tryCatch(
  ordinal::clmm(
    clmm_formula_full,
    data = analysis_purchase_ord,
    Hess = TRUE,
    nAGQ = 1,
    control = ordinal::clmm.control(maxIter = 200, gradTol = 1e-4)
  ),
  error = function(e) {
    clmm_fit_status <<- "fallback_consumer_random_intercept_only"
    ordinal::clmm(
      clmm_formula_consumer_only,
      data = analysis_purchase_ord,
      Hess = TRUE,
      nAGQ = 1,
      control = ordinal::clmm.control(maxIter = 200, gradTol = 1e-4)
    )
  }
)

sink(file.path(model_dir, "06_purchase_intent_ordinal_clmm.txt"))
cat("CLMM fit status:", clmm_fit_status, "\n\n")
print(summary(m_purchase_clmm))
sink()

clmm_coef <- as.data.frame(coef(summary(m_purchase_clmm))) |>
  tibble::rownames_to_column("term") |>
  rename(
    estimate = Estimate,
    std_error = `Std. Error`,
    statistic = `z value`,
    p_value = `Pr(>|z|)`
  ) |>
  mutate(
    parameter_type = ifelse(grepl("\\|", term), "threshold", "predictor"),
    odds_ratio = ifelse(parameter_type == "predictor", exp(estimate), NA_real_),
    odds_ratio_low = ifelse(parameter_type == "predictor", exp(estimate - 1.96 * std_error), NA_real_),
    odds_ratio_high = ifelse(parameter_type == "predictor", exp(estimate + 1.96 * std_error), NA_real_),
    model_fit_status = clmm_fit_status
  )

write_csv(clmm_coef, file.path(table_dir, "10_purchase_intent_ordinal_clmm_coefficients.csv"))

p_clmm <- clmm_coef |>
  filter(parameter_type == "predictor") |>
  mutate(term = factor(term, levels = rev(term))) |>
  ggplot(aes(x = odds_ratio, y = term)) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  geom_point() +
  geom_errorbar(
    aes(xmin = odds_ratio_low, xmax = odds_ratio_high),
    orientation = "y",
    width = 0.2
  ) +
  scale_x_log10() +
  labs(
    title = "Ordinal mixed model for purchase intent",
    x = "Odds ratio for higher purchase-intent category, log scale",
    y = NULL
  ) +
  theme_bw()

ggsave(file.path(figure_dir, "03_purchase_intent_ordinal_clmm_odds_ratios.png"), p_clmm, width = 7, height = 4.5, dpi = 300)

set.seed(20260705)
B <- 2000
common_products <- sort(unique(as.character(model_liking_common_products$product)))
boot_data <- model_liking_common_products |>
  select(product, test_location, liking, jar_dev_count, jar_severity) |>
  filter(!is.na(liking), !is.na(jar_dev_count), !is.na(jar_severity)) |>
  mutate(product = as.character(product), test_location = as.character(test_location))

cell_mean_boot <- function(df, variable) {
  if (nrow(df) == 0) return(NA_real_)
  idx <- sample.int(nrow(df), size = nrow(df), replace = TRUE)
  mean(df[[variable]][idx], na.rm = TRUE)
}

boot_once <- function(b) {
  product_rows <- vector("list", length(common_products))
  for (i in seq_along(common_products)) {
    p <- common_products[i]
    h <- boot_data[boot_data$product == p & boot_data$test_location == "home", ]
    l <- boot_data[boot_data$product == p & boot_data$test_location == "lab", ]

    h_liking <- cell_mean_boot(h, "liking")
    l_liking <- cell_mean_boot(l, "liking")
    h_count <- cell_mean_boot(h, "jar_dev_count")
    l_count <- cell_mean_boot(l, "jar_dev_count")
    h_sev <- cell_mean_boot(h, "jar_severity")
    l_sev <- cell_mean_boot(l, "jar_severity")

    product_rows[[i]] <- tibble(
      boot = b,
      product = p,
      liking_mean_home = h_liking,
      liking_mean_lab = l_liking,
      context_gain_liking = h_liking - l_liking,
      jar_dev_count_mean_home = h_count,
      jar_dev_count_mean_lab = l_count,
      context_gain_jar_dev_count = h_count - l_count,
      jar_severity_mean_home = h_sev,
      jar_severity_mean_lab = l_sev,
      context_gain_jar_severity = h_sev - l_sev
    )
  }
  bind_rows(product_rows)
}

boot_list <- vector("list", B)
for (b in seq_len(B)) {
  boot_list[[b]] <- boot_once(b)
}
boot_context_gain <- bind_rows(boot_list)

boot_context_gain_ci <- boot_context_gain |>
  group_by(product) |>
  summarise(
    context_gain_liking_boot_mean = mean(context_gain_liking, na.rm = TRUE),
    context_gain_liking_low = quantile(context_gain_liking, 0.025, na.rm = TRUE),
    context_gain_liking_high = quantile(context_gain_liking, 0.975, na.rm = TRUE),
    context_gain_jar_dev_count_boot_mean = mean(context_gain_jar_dev_count, na.rm = TRUE),
    context_gain_jar_dev_count_low = quantile(context_gain_jar_dev_count, 0.025, na.rm = TRUE),
    context_gain_jar_dev_count_high = quantile(context_gain_jar_dev_count, 0.975, na.rm = TRUE),
    context_gain_jar_severity_boot_mean = mean(context_gain_jar_severity, na.rm = TRUE),
    context_gain_jar_severity_low = quantile(context_gain_jar_severity, 0.025, na.rm = TRUE),
    context_gain_jar_severity_high = quantile(context_gain_jar_severity, 0.975, na.rm = TRUE),
    .groups = "drop"
  ) |>
  left_join(
    context_gain |>
      mutate(product = as.character(product)) |>
      select(product, context_gain_liking, context_gain_jar_dev_count, context_gain_jar_severity),
    by = "product"
  ) |>
  arrange(desc(context_gain_liking))

write_csv(boot_context_gain_ci, file.path(table_dir, "11_bootstrap_context_gain_product_ci.csv"))

boot_corr <- boot_context_gain |>
  group_by(boot) |>
  summarise(
    r_liking_vs_count_gap = safe_cor(context_gain_liking, context_gain_jar_dev_count, "pearson"),
    r_liking_vs_severity_gap = safe_cor(context_gain_liking, context_gain_jar_severity, "pearson"),
    spearman_liking_vs_count_gap = safe_cor(context_gain_liking, context_gain_jar_dev_count, "spearman"),
    spearman_liking_vs_severity_gap = safe_cor(context_gain_liking, context_gain_jar_severity, "spearman"),
    .groups = "drop"
  )

boot_corr_ci <- boot_corr |>
  summarise(
    r_liking_vs_count_gap_mean = mean(r_liking_vs_count_gap, na.rm = TRUE),
    r_liking_vs_count_gap_low = quantile(r_liking_vs_count_gap, 0.025, na.rm = TRUE),
    r_liking_vs_count_gap_high = quantile(r_liking_vs_count_gap, 0.975, na.rm = TRUE),
    r_liking_vs_severity_gap_mean = mean(r_liking_vs_severity_gap, na.rm = TRUE),
    r_liking_vs_severity_gap_low = quantile(r_liking_vs_severity_gap, 0.025, na.rm = TRUE),
    r_liking_vs_severity_gap_high = quantile(r_liking_vs_severity_gap, 0.975, na.rm = TRUE),
    spearman_liking_vs_count_gap_mean = mean(spearman_liking_vs_count_gap, na.rm = TRUE),
    spearman_liking_vs_count_gap_low = quantile(spearman_liking_vs_count_gap, 0.025, na.rm = TRUE),
    spearman_liking_vs_count_gap_high = quantile(spearman_liking_vs_count_gap, 0.975, na.rm = TRUE),
    spearman_liking_vs_severity_gap_mean = mean(spearman_liking_vs_severity_gap, na.rm = TRUE),
    spearman_liking_vs_severity_gap_low = quantile(spearman_liking_vs_severity_gap, 0.025, na.rm = TRUE),
    spearman_liking_vs_severity_gap_high = quantile(spearman_liking_vs_severity_gap, 0.975, na.rm = TRUE)
  )

write_csv(boot_corr, file.path(table_dir, "12_bootstrap_context_gain_correlations_all_iterations.csv"))
write_csv(boot_corr_ci, file.path(table_dir, "13_bootstrap_context_gain_correlation_ci.csv"))

p_context_ci <- boot_context_gain_ci |>
  mutate(product = factor(product, levels = product[order(context_gain_liking)])) |>
  ggplot(aes(x = product, y = context_gain_liking)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point() +
  geom_errorbar(aes(ymin = context_gain_liking_low, ymax = context_gain_liking_high), width = 0.2) +
  coord_flip() +
  labs(
    title = "Bootstrap CIs for home-lab liking differences",
    x = "Product",
    y = "Home liking - lab liking"
  ) +
  theme_bw()

ggsave(file.path(figure_dir, "04_bootstrap_context_gain_liking_ci.png"), p_context_ci, width = 7, height = 5.5, dpi = 300)

p_context_gap <- boot_context_gain_ci |>
  ggplot(aes(x = context_gain_jar_dev_count, y = context_gain_liking, label = product)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point() +
  geom_errorbar(aes(ymin = context_gain_liking_low, ymax = context_gain_liking_high), width = 0.02) +
  ggrepel::geom_text_repel(max.overlaps = Inf) +
  labs(
    title = "Context gain in liking vs. context gap in JAR deviation burden",
    x = "Home - lab JAR deviation count",
    y = "Home - lab liking"
  ) +
  theme_bw()

ggsave(file.path(figure_dir, "05_context_gain_vs_jar_gap_with_bootstrap_ci.png"), p_context_gap, width = 7, height = 5.5, dpi = 300)

m_product_location <- safe_lmer(
  liking ~ product * test_location + (1 | consumer),
  data = model_liking_common_products,
  REML = FALSE
)
write_model_summary(m_product_location, "07_product_by_location_liking_model.txt")

emm <- emmeans::emmeans(m_product_location, ~ test_location | product)
emm_contrast <- emmeans::contrast(
  emm,
  method = list(home_minus_lab = c(1, -1)),
  by = "product"
)

emm_context_gain_raw <- as.data.frame(summary(emm_contrast, infer = c(TRUE, TRUE)))

ci_low_col <- intersect(
  c("lower.CL", "asymp.LCL", "lower.HPD", "LCL", "lower"),
  names(emm_context_gain_raw)
)[1]
ci_high_col <- intersect(
  c("upper.CL", "asymp.UCL", "upper.HPD", "UCL", "upper"),
  names(emm_context_gain_raw)
)[1]
p_col <- intersect(c("p.value", "p.value.", "Pr(>|t|)", "Pr(>|z|)"), names(emm_context_gain_raw))[1]
ratio_col <- intersect(c("t.ratio", "z.ratio"), names(emm_context_gain_raw))[1]

if (is.na(ci_low_col) || is.na(ci_high_col)) {
  stop(
    "Could not find confidence interval columns in emmeans output. Column names were: ",
    paste(names(emm_context_gain_raw), collapse = ", ")
  )
}

emm_context_gain <- emm_context_gain_raw |>
  mutate(
    product = as.character(product),
    context_gain_model_estimate = estimate,
    std_error = SE,
    df = if ("df" %in% names(emm_context_gain_raw)) .data[["df"]] else NA_real_,
    conf_low = .data[[ci_low_col]],
    conf_high = .data[[ci_high_col]],
    p_value = if (!is.na(p_col)) .data[[p_col]] else NA_real_,
    statistic = if (!is.na(ratio_col)) .data[[ratio_col]] else NA_real_
  ) |>
  select(
    product,
    contrast,
    context_gain_model_estimate,
    std_error,
    df,
    conf_low,
    conf_high,
    statistic,
    p_value,
    everything()
  ) |>
  arrange(desc(context_gain_model_estimate))

write_csv(emm_context_gain, file.path(table_dir, "14_emmeans_product_specific_context_gain.csv"))

p_emm_context <- emm_context_gain |>
  mutate(product = factor(product, levels = product[order(context_gain_model_estimate)])) |>
  ggplot(aes(x = product, y = context_gain_model_estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point() +
  geom_errorbar(aes(ymin = conf_low, ymax = conf_high), width = 0.2) +
  coord_flip() +
  labs(
    title = "Model-based product-specific home-lab liking differences",
    x = "Product",
    y = "Home liking - lab liking"
  ) +
  theme_bw()

ggsave(file.path(figure_dir, "06_emmeans_product_specific_context_gain.png"), p_emm_context, width = 7, height = 5.5, dpi = 300)

run_summary <- tibble(
  item = c(
    "exclude_J09_models",
    "leave_one_product_out_correlations",
    "direction_specific_location_interactions",
    "ordinal_purchase_intent_model",
    "bootstrap_context_gain_ci",
    "emmeans_product_specific_context_gain"
  ),
  output = c(
    "01-04_* files in sensitivity_checks/tables; 01-03_* model summaries",
    "05-06_* tables and 01_leave_one_product_out_correlations.png",
    "07-09_* tables and 02_direction_specific_penalties_by_location.png",
    "10_purchase_intent_ordinal_clmm_coefficients.csv and 03_purchase_intent_ordinal_clmm_odds_ratios.png",
    "11-13_* tables and 04-05_* figures",
    "14_emmeans_product_specific_context_gain.csv and 06_emmeans_product_specific_context_gain.png"
  )
)
write_csv(run_summary, file.path(table_dir, "00_sensitivity_run_summary.csv"))

root_dir <- HAM_ROOT_DIR
setwd(root_dir)

proc_dir <- file.path(root_dir, "data_processed")
analysis_dir <- file.path(root_dir, "analysis_outputs")
analysis_table_dir <- file.path(analysis_dir, "tables")
sens_dir <- file.path(analysis_dir, "sensitivity_checks")
sens_table_dir <- file.path(sens_dir, "tables")

out_dir <- file.path(root_dir, "manuscript_outputs_revised")
out_table_dir <- file.path(out_dir, "tables")
out_figure_dir <- file.path(out_dir, "figures")
out_supp_dir <- file.path(out_dir, "supplementary")
out_supp_table_dir <- file.path(out_supp_dir, "tables")
out_supp_figure_dir <- file.path(out_supp_dir, "figures")

out_figure_tiff_dir <- file.path(out_dir, "figures_tiff")
out_supp_figure_tiff_dir <- file.path(out_supp_dir, "figures_tiff")

for (d in c(out_dir, out_table_dir, out_figure_dir, out_figure_tiff_dir,
            out_supp_dir, out_supp_table_dir, out_supp_figure_dir,
            out_supp_figure_tiff_dir)) {
  dir.create(d, showWarnings = FALSE, recursive = TRUE)
}

read_data_any <- function(rds_path, csv_path = NULL, required = TRUE) {
  if (file.exists(rds_path)) return(readRDS(rds_path))
  if (!is.null(csv_path) && file.exists(csv_path)) {
    return(readr::read_csv(csv_path, show_col_types = FALSE))
  }
  msg <- paste0(
    "Could not find data file. Tried: ", rds_path,
    if (!is.null(csv_path)) paste0(" and ", csv_path) else ""
  )
  if (required) stop(msg) else return(NULL)
}

read_csv_optional <- function(path) {
  if (file.exists(path)) {
    readr::read_csv(path, show_col_types = FALSE)
  } else {
    NULL
  }
}

read_csv_required <- function(path) {
  if (!file.exists(path)) stop("Required file not found: ", path)
  readr::read_csv(path, show_col_types = FALSE)
}

has_rows <- function(x) {
  !is.null(x) && is.data.frame(x) && nrow(x) > 0
}

first_existing_col <- function(df, candidates) {
  hit <- intersect(candidates, names(df))
  if (length(hit) == 0) NA_character_ else hit[1]
}

standardize_p_value_col <- function(df) {
  if ("p_value" %in% names(df)) return(df)
  p_col <- first_existing_col(df, c("Pr(>Chisq)", "Pr..Chisq.", "p.value", "p.value.", "p"))
  if (!is.na(p_col)) {
    df$p_value <- df[[p_col]]
  } else {
    df$p_value <- NA_real_
  }
  df
}

mean_or_na <- function(x) {
  if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
}

sd_or_na <- function(x) {
  if (sum(!is.na(x)) <= 1) NA_real_ else sd(x, na.rm = TRUE)
}

se_or_na <- function(x) {
  n <- sum(!is.na(x))
  if (n <= 1) NA_real_ else sd(x, na.rm = TRUE) / sqrt(n)
}

ci95_low <- function(x) {
  n <- sum(!is.na(x))
  if (n <= 1) return(NA_real_)
  mean(x, na.rm = TRUE) - qt(0.975, df = n - 1) * se_or_na(x)
}

ci95_high <- function(x) {
  n <- sum(!is.na(x))
  if (n <= 1) return(NA_real_)
  mean(x, na.rm = TRUE) + qt(0.975, df = n - 1) * se_or_na(x)
}

fmt_num <- function(x, digits = 2) {
  ifelse(is.na(x) | is.nan(x), "—", formatC(x, format = "f", digits = digits))
}

fmt_pct <- function(x, digits = 1) {
  ifelse(is.na(x) | is.nan(x), "—", paste0(formatC(100 * x, format = "f", digits = digits), "%"))
}

fmt_p <- function(p) {
  dplyr::case_when(
    is.na(p) | is.nan(p) ~ "—",
    p < 0.001 ~ "<0.001",
    TRUE ~ formatC(p, format = "f", digits = 3)
  )
}

pretty_term_label <- function(x) {
  x <- as.character(x)
  labels <- c(
    "test_locationlab" = "Blind lab vs home-use",
    "jar_dev_count" = "JAR-deviation count",
    "test_locationlab:jar_dev_count" = "Blind lab × JAR-deviation count",
    "liking_z" = "Liking, z-score",
    "price_z" = "Price, z-score",
    "ham_usually_bought_fyes" = "Usually bought: yes",
    "color_too_low" = "Color too low",
    "color_too_high" = "Color too high",
    "fat_too_low" = "Fat too low",
    "fat_too_high" = "Fat too high",
    "salt_too_low" = "Salt too low",
    "salt_too_high" = "Salt too high",
    "tender_too_low" = "Tenderness too low",
    "tender_too_high" = "Tenderness too high"
  )
  out <- labels[x]
  out[is.na(out)] <- x[is.na(out)]
  unname(out)
}

paper_theme <- function(base_size = 11) {
  theme_bw(base_size = base_size) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(linewidth = 0.25),
      strip.background = element_rect(fill = "white", colour = "black"),
      legend.position = "right",
      plot.title = element_blank(),
      plot.subtitle = element_blank(),
      axis.title = element_text(face = "bold")
    )
}

write_tiff_plot <- function(plot, filename, width, height, dpi = 600) {
  dir.create(dirname(filename), showWarnings = FALSE, recursive = TRUE)

  close_device_safely <- function() {
    if (grDevices::dev.cur() > 1) {
      try(grDevices::dev.off(), silent = TRUE)
    }
  }

  success <- FALSE

  tryCatch({
    grDevices::tiff(
      filename = filename,
      width = width,
      height = height,
      units = "in",
      res = dpi,
      compression = "lzw"
    )
    print(plot)
    close_device_safely()
    success <<- TRUE
  }, error = function(e) {
    close_device_safely()
  })

  if (!success) {
    tryCatch({
      grDevices::tiff(
        filename = filename,
        width = width,
        height = height,
        units = "in",
        res = dpi,
        compression = "none"
      )
      print(plot)
      close_device_safely()
      success <<- TRUE
    }, error = function(e) {
      close_device_safely()
    })
  }

  if (!success) {
    tryCatch({
      ggplot2::ggsave(
        filename = filename,
        plot = plot,
        width = width,
        height = height,
        dpi = dpi,
        device = "tiff"
      )
      success <<- TRUE
    }, error = function(e) {
    })
  }

  if (!file.exists(filename) || file.info(filename)$size == 0) {
    stop("TIFF export did not create a valid file: ", filename)
  }

  invisible(filename)
}

get_tiff_copy_dir <- function(dir) {
  norm_dir <- normalizePath(dir, mustWork = FALSE)
  if (identical(norm_dir, normalizePath(out_figure_dir, mustWork = FALSE))) {
    return(out_figure_tiff_dir)
  }
  if (identical(norm_dir, normalizePath(out_supp_figure_dir, mustWork = FALSE))) {
    return(out_supp_figure_tiff_dir)
  }
  file.path(dir, "tiff")
}

save_plot_all <- function(plot, filename_base, width = 7, height = 5,
                          dir = out_figure_dir, dpi = 600) {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)

  png_path <- file.path(dir, paste0(filename_base, ".png"))
  pdf_path <- file.path(dir, paste0(filename_base, ".pdf"))
  tiff_path <- file.path(dir, paste0(filename_base, ".tiff"))

  ggplot2::ggsave(
    filename = png_path,
    plot = plot, width = width, height = height, dpi = dpi
  )
  ggplot2::ggsave(
    filename = pdf_path,
    plot = plot, width = width, height = height
  )

  write_tiff_plot(plot, tiff_path, width = width, height = height, dpi = dpi)

  tiff_copy_dir <- get_tiff_copy_dir(dir)
  dir.create(tiff_copy_dir, showWarnings = FALSE, recursive = TRUE)
  tiff_copy_path <- file.path(tiff_copy_dir, paste0(filename_base, ".tiff"))

  if (!identical(normalizePath(dirname(tiff_path), mustWork = FALSE),
                 normalizePath(tiff_copy_dir, mustWork = FALSE))) {
    ok <- file.copy(tiff_path, tiff_copy_path, overwrite = TRUE)
    if (!ok) warning("Could not copy TIFF file to dedicated TIFF directory: ", tiff_copy_path)
  }

  invisible(tibble::tibble(
    filename_base = filename_base,
    png = png_path,
    pdf = pdf_path,
    tiff = tiff_path,
    tiff_submission_copy = tiff_copy_path,
    width_in = width,
    height_in = height,
    dpi = dpi
  ))
}

write_csv_out <- function(df, filename, dir = out_table_dir) {
  readr::write_csv(df, file.path(dir, filename), na = "")
}

make_est_ci <- function(est, low, high, digits = 2) {
  paste0(fmt_num(est, digits), " [", fmt_num(low, digits), ", ", fmt_num(high, digits), "]")
}

select_existing <- function(df, cols) {
  df |> dplyr::select(dplyr::any_of(cols))
}

model_liking <- read_data_any(
  file.path(proc_dir, "model_liking.rds"),
  file.path(proc_dir, "model_liking.csv")
)

model_liking_common <- read_data_any(
  file.path(proc_dir, "model_liking_common_products.rds"),
  file.path(proc_dir, "model_liking_common_products.csv")
)

model_purchase <- read_data_any(
  file.path(proc_dir, "model_purchase.rds"),
  file.path(proc_dir, "model_purchase.csv")
)

context_gain <- read_data_any(
  file.path(proc_dir, "context_gain.rds"),
  file.path(proc_dir, "context_gain.csv")
)

product_panel <- read_data_any(
  file.path(proc_dir, "product_panel.rds"),
  file.path(proc_dir, "product_panel.csv")
)

context_tests <- read_csv_required(file.path(analysis_table_dir, "03_context_gain_tests.csv"))
liking_model_comp <- read_csv_required(file.path(analysis_table_dir, "10_liking_model_comparison_product_fixed.csv")) |>
  standardize_p_value_col()
liking_fixed <- read_csv_required(file.path(analysis_table_dir, "11_liking_model_fixed_effects.csv"))
jar_penalties <- read_csv_required(file.path(analysis_table_dir, "17_direction_specific_jar_penalties.csv"))
composition_corr <- read_csv_required(file.path(analysis_table_dir, "19_composition_product_level_correlations.csv"))
purchase_lmm <- read_csv_required(file.path(analysis_table_dir, "23_purchase_lmm_fixed_effects.csv"))
purchase_glmer <- read_csv_required(file.path(analysis_table_dir, "25_purchase_yes_glmer_fixed_effects.csv"))

corr_excl_j09 <- read_csv_optional(file.path(sens_table_dir, "04_context_gain_correlations_exclude_J09.csv"))
loo_corr <- read_csv_optional(file.path(sens_table_dir, "05_leave_one_product_out_context_gain_correlations.csv"))
dir_location_lrt <- read_csv_optional(file.path(sens_table_dir, "08_direction_specific_location_interaction_lrt.csv"))
dir_by_location <- read_csv_optional(file.path(sens_table_dir, "09_direction_specific_penalties_by_location.csv"))
purchase_clmm <- read_csv_optional(file.path(sens_table_dir, "10_purchase_intent_ordinal_clmm_coefficients.csv"))
boot_context_ci <- read_csv_optional(file.path(sens_table_dir, "11_bootstrap_context_gain_product_ci.csv"))
boot_corr_ci <- read_csv_optional(file.path(sens_table_dir, "13_bootstrap_context_gain_correlation_ci.csv"))
emm_context_gain <- read_csv_optional(file.path(sens_table_dir, "14_emmeans_product_specific_context_gain.csv"))

if (is.null(purchase_clmm)) purchase_clmm <- tibble::tibble()
if (is.null(boot_context_ci)) boot_context_ci <- tibble::tibble(product = character())
if (is.null(boot_corr_ci)) boot_corr_ci <- tibble::tibble()
if (is.null(emm_context_gain)) emm_context_gain <- tibble::tibble(product = character())

if (!is.null(loo_corr) && "omitted_product" %in% names(loo_corr) && !"product_removed" %in% names(loo_corr)) {
  loo_corr <- loo_corr |> rename(product_removed = omitted_product)
}
if (!is.null(dir_by_location) && "jar_term" %in% names(dir_by_location) && !"term" %in% names(dir_by_location)) {
  dir_by_location <- dir_by_location |> rename(term = jar_term)
}

for (nm in c("model_liking", "model_liking_common", "model_purchase")) {
  obj <- get(nm)
  if ("product" %in% names(obj)) obj$product <- as.character(obj$product)
  if ("consumer" %in% names(obj)) obj$consumer <- as.character(obj$consumer)
  if ("test_location" %in% names(obj)) obj$test_location <- as.character(obj$test_location)
  assign(nm, obj)
}
context_gain$product <- as.character(context_gain$product)
product_panel$product <- as.character(product_panel$product)

common_products <- sort(unique(context_gain$product))
key_products <- c("J09", "J31", "J10", "J01", "J02", "J24", "J30", "J23")

boot_context_for_join <- boot_context_ci |>
  select_existing(c(
    "product",
    "context_gain_liking_low",
    "context_gain_liking_high",
    "context_gain_jar_dev_count_low",
    "context_gain_jar_dev_count_high",
    "context_gain_jar_severity_low",
    "context_gain_jar_severity_high"
  ))

emm_for_join <- emm_context_gain |>
  select_existing(c(
    "product",
    "context_gain_model_estimate",
    "conf_low",
    "conf_high",
    "p_value"
  ))
if (ncol(emm_for_join) > 1) {
  emm_for_join <- emm_for_join |>
    rename(
      emmeans_context_gain = context_gain_model_estimate,
      emmeans_context_gain_low = conf_low,
      emmeans_context_gain_high = conf_high,
      emmeans_p_value = p_value
    )
}

product_context <- product_panel |>
  filter(product %in% common_products) |>
  left_join(boot_context_for_join, by = "product") |>
  left_join(emm_for_join, by = "product") |>
  arrange(desc(context_gain_liking))

optional_numeric_cols <- c(
  "context_gain_liking_low", "context_gain_liking_high",
  "context_gain_jar_dev_count_low", "context_gain_jar_dev_count_high",
  "context_gain_jar_severity_low", "context_gain_jar_severity_high",
  "emmeans_context_gain", "emmeans_context_gain_low", "emmeans_context_gain_high",
  "emmeans_p_value"
)
for (cc in optional_numeric_cols) {
  if (!cc %in% names(product_context)) product_context[[cc]] <- NA_real_
}

product_context <- product_context |>
  mutate(
    context_gain_significant_boot = case_when(
      !is.na(context_gain_liking_low) & context_gain_liking_low > 0 ~ "Home-use > lab",
      !is.na(context_gain_liking_high) & context_gain_liking_high < 0 ~ "Home-use < lab",
      TRUE ~ "CI includes zero"
    ),
    product_label_main = if_else(product %in% key_products, product, NA_character_)
  ) |>
  arrange(desc(context_gain_liking))

sample_summary <- function(df, dataset_label) {
  tibble::tibble(
    dataset = dataset_label,
    mean_level = "Evaluation-level mean",
    n_evaluations = nrow(df),
    n_consumers = dplyr::n_distinct(df$consumer),
    n_products = dplyr::n_distinct(df$product),
    liking_mean = mean_or_na(df$liking),
    liking_sd = sd_or_na(df$liking),
    jar_dev_count_mean = mean_or_na(df$jar_dev_count),
    jar_severity_mean = mean_or_na(df$jar_severity)
  )
}

purchase_summary <- model_purchase |>
  summarise(
    dataset = "Home-use evaluations with purchase-intent data",
    mean_level = "Evaluation-level mean",
    n_evaluations = n(),
    n_consumers = n_distinct(consumer),
    n_products = n_distinct(product),
    liking_mean = mean_or_na(liking),
    liking_sd = sd_or_na(liking),
    jar_dev_count_mean = mean_or_na(jar_dev_count),
    jar_severity_mean = mean_or_na(jar_severity),
    price_mean = mean_or_na(price),
    purchase_intent_mean = mean_or_na(purchase_intent),
    usual_purchase_rate = mean(ham_usually_bought == 1, na.rm = TRUE),
    purchase_yes_rate = mean(purchase_intent == 1, na.rm = TRUE)
  )

table1_raw <- bind_rows(
  sample_summary(
    model_liking |> filter(test_location == "home"),
    "Home-use evaluations, all products"
  ),
  sample_summary(
    model_liking |> filter(test_location == "lab"),
    "Blind laboratory evaluations"
  ),
  sample_summary(
    model_liking_common |> filter(test_location == "home"),
    "Home-use evaluations, 16 lab-overlap products"
  )
) |>
  mutate(
    price_mean = NA_real_,
    purchase_intent_mean = NA_real_,
    usual_purchase_rate = NA_real_,
    purchase_yes_rate = NA_real_
  ) |>
  bind_rows(purchase_summary) |>
  mutate(
    liking = paste0(fmt_num(liking_mean, 2), " ± ", fmt_num(liking_sd, 2))
  ) |>
  select(
    dataset, mean_level, n_evaluations, n_consumers, n_products,
    liking_mean, liking_sd, liking,
    jar_dev_count_mean, jar_severity_mean,
    price_mean, purchase_intent_mean, usual_purchase_rate, purchase_yes_rate
  )

write_csv_out(table1_raw, "table1_sample_overview.csv")

table1_formatted <- table1_raw |>
  transmute(
    Dataset = dataset,
    `Mean level` = mean_level,
    `Evaluations, n` = n_evaluations,
    `Consumers, n` = n_consumers,
    `Products, n` = n_products,
    `Liking, mean ± SD` = liking,
    `JAR-deviation count, mean` = fmt_num(jar_dev_count_mean, 2),
    `JAR severity, mean` = fmt_num(jar_severity_mean, 2),
    `Price, mean` = fmt_num(price_mean, 2),
    `Purchase intent, mean` = fmt_num(purchase_intent_mean, 2),
    `Usually bought, %` = fmt_pct(usual_purchase_rate, 1),
    `Purchase yes, %` = fmt_pct(purchase_yes_rate, 1)
  )

write_csv_out(table1_formatted, "table1_sample_overview_formatted.csv")

writeLines(
  c(
    "Table 1 note:",
    "Means in Table 1 are evaluation-level means and are therefore weighted by the number of evaluations per product.",
    "Product-level home-lab comparisons are reported in Table 2. Dashes indicate not applicable or unavailable values."
  ),
  con = file.path(out_table_dir, "table1_note.txt")
)

table2_main_raw <- product_context |>
  transmute(
    product,
    n_eval_home,
    n_eval_lab,
    liking_mean_home,
    liking_mean_lab,
    context_gain_liking,
    context_gain_liking_low,
    context_gain_liking_high,
    jar_dev_count_mean_home,
    jar_dev_count_mean_lab,
    context_gain_jar_dev_count,
    jar_severity_mean_home,
    jar_severity_mean_lab,
    context_gain_jar_severity,
    context_gain_significant_boot
  ) |>
  arrange(desc(context_gain_liking))

write_csv_out(table2_main_raw, "table2_product_context_gain_main.csv")

table2_main_formatted <- table2_main_raw |>
  transmute(
    Product = product,
    `Home n` = n_eval_home,
    `Lab n` = n_eval_lab,
    `Home liking` = fmt_num(liking_mean_home, 2),
    `Lab liking` = fmt_num(liking_mean_lab, 2),
    `Liking gap, home-lab [95% boot CI]` = make_est_ci(
      context_gain_liking,
      context_gain_liking_low,
      context_gain_liking_high,
      2
    ),
    `Home JAR-deviation count` = fmt_num(jar_dev_count_mean_home, 2),
    `Lab JAR-deviation count` = fmt_num(jar_dev_count_mean_lab, 2),
    `JAR-deviation count gap, home-lab` = fmt_num(context_gain_jar_dev_count, 2),
    `JAR severity gap, home-lab` = fmt_num(context_gain_jar_severity, 2),
    `Bootstrap classification` = context_gain_significant_boot
  )

write_csv_out(table2_main_formatted, "table2_product_context_gain_main_formatted.csv")

table2_full_supp <- product_context |>
  select_existing(c(
    "product", "n_eval_home", "n_eval_lab",
    "liking_mean_home", "liking_mean_lab", "context_gain_liking",
    "context_gain_liking_low", "context_gain_liking_high",
    "emmeans_context_gain", "emmeans_context_gain_low", "emmeans_context_gain_high",
    "emmeans_p_value",
    "jar_dev_count_mean_home", "jar_dev_count_mean_lab", "context_gain_jar_dev_count",
    "jar_severity_mean_home", "jar_severity_mean_lab", "context_gain_jar_severity",
    "pct_any_jar_dev_home", "pct_any_jar_dev_lab",
    "brand_type", "no_nitrite", "salt_light", "fat_light", "label_rouge", "nutriscore",
    "salt_measured_mean", "fat_measured_mean", "price_mean", "usual_bought_rate",
    "purchase_intent_mean"
  )) |>
  arrange(desc(context_gain_liking))

write_csv_out(table2_full_supp, "table_s1_product_context_gain_full.csv", out_supp_table_dir)

table3a <- liking_model_comp |>
  mutate(
    model_label = dplyr::recode(
      model,
      "m_liking_01_location" = "Location + product",
      "m_liking_02_add_jar_count" = "Location + JAR-deviation count + product",
      "m_liking_03_interaction" = "Location × JAR-deviation count + product",
      "m_liking_03_location_x_jar_count" = "Location × JAR-deviation count + product",
      .default = model
    ),
    p_value_formatted = fmt_p(p_value)
  ) |>
  select(model_label, npar, AIC, BIC, logLik, Chisq, Df, p_value, p_value_formatted)

write_csv_out(table3a, "table3a_liking_model_comparison.csv")

table3b_all <- liking_fixed |>
  filter(term %in% c("test_locationlab", "jar_dev_count", "test_locationlab:jar_dev_count")) |>
  mutate(
    model_label = dplyr::recode(
      model,
      "location + product" = "Location + product",
      "location + JAR count + product" = "Location + JAR-deviation count + product",
      "location * JAR count + product" = "Location × JAR-deviation count + product",
      "location + JAR count + random product" = "Location + JAR-deviation count + random product",
      .default = model
    ),
    term_label = pretty_term_label(term),
    estimate_ci = make_est_ci(estimate, conf.low, conf.high, 2),
    p_value_formatted = fmt_p(p.value)
  ) |>
  select(model_label, term, term_label, estimate, conf.low, conf.high,
         estimate_ci, std.error, df, statistic, p.value, p_value_formatted)

main_model_labels <- c(
  "Location + product",
  "Location + JAR-deviation count + product",
  "Location × JAR-deviation count + product"
)

table3b <- table3b_all |>
  filter(model_label %in% main_model_labels)

write_csv_out(table3b, "table3b_liking_key_fixed_effects.csv")

table3b_random_product_supp <- table3b_all |>
  filter(!model_label %in% main_model_labels)

if (nrow(table3b_random_product_supp) > 0) {
  write_csv_out(
    table3b_random_product_supp,
    "table_s13_liking_random_product_sensitivity.csv",
    out_supp_table_dir
  )
}

jar_terms <- c(
  "color_too_low", "color_too_high",
  "fat_too_low", "fat_too_high",
  "salt_too_low", "salt_too_high",
  "tender_too_low", "tender_too_high"
)

jar_prevalence <- lapply(jar_terms, function(v) {
  x_all <- model_liking[[v]]
  x_home <- model_liking[[v]][model_liking$test_location == "home"]
  x_lab <- model_liking[[v]][model_liking$test_location == "lab"]
  tibble::tibble(
    term = v,
    n_overall = sum(!is.na(x_all)),
    n_deviation_overall = sum(x_all == 1, na.rm = TRUE),
    prevalence_overall = mean(x_all == 1, na.rm = TRUE),
    n_home = sum(!is.na(x_home)),
    n_deviation_home = sum(x_home == 1, na.rm = TRUE),
    prevalence_home = mean(x_home == 1, na.rm = TRUE),
    n_lab = sum(!is.na(x_lab)),
    n_deviation_lab = sum(x_lab == 1, na.rm = TRUE),
    prevalence_lab = mean(x_lab == 1, na.rm = TRUE)
  )
}) |>
  bind_rows()

table4 <- jar_penalties |>
  mutate(
    term_label = if ("term_label" %in% names(jar_penalties)) term_label else pretty_term_label(term),
    attribute = if ("attribute" %in% names(jar_penalties)) attribute else NA_character_,
    direction = if ("direction" %in% names(jar_penalties)) direction else NA_character_,
    penalty = if ("penalty" %in% names(jar_penalties)) penalty else -estimate,
    penalty_low = if ("penalty_low" %in% names(jar_penalties)) penalty_low else -conf.high,
    penalty_high = if ("penalty_high" %in% names(jar_penalties)) penalty_high else -conf.low,
    penalty_ci = make_est_ci(penalty, penalty_low, penalty_high, 2),
    p_value_formatted = fmt_p(p.value)
  ) |>
  left_join(jar_prevalence, by = "term") |>
  arrange(desc(penalty)) |>
  select(
    term, term_label, attribute, direction,
    estimate, conf.low, conf.high,
    penalty, penalty_low, penalty_high, penalty_ci,
    n_deviation_overall, prevalence_overall,
    n_deviation_home, prevalence_home,
    n_deviation_lab, prevalence_lab,
    std.error, df, statistic, p.value, p_value_formatted
  )

write_csv_out(table4, "table4_direction_specific_jar_penalties.csv")

table4_formatted <- table4 |>
  transmute(
    `JAR deviation` = term_label,
    Attribute = attribute,
    Direction = direction,
    `Penalty, liking points [95% CI]` = penalty_ci,
    `Overall prevalence, n (%)` = paste0(n_deviation_overall, " (", fmt_pct(prevalence_overall, 1), ")"),
    `Home-use prevalence, n (%)` = paste0(n_deviation_home, " (", fmt_pct(prevalence_home, 1), ")"),
    `Blind lab prevalence, n (%)` = paste0(n_deviation_lab, " (", fmt_pct(prevalence_lab, 1), ")"),
    p = p_value_formatted
  )

write_csv_out(table4_formatted, "table4_direction_specific_jar_penalties_formatted.csv")

if (has_rows(purchase_clmm)) {
  table5_main <- purchase_clmm |>
    filter(parameter_type == "predictor") |>
    mutate(
      term_label = pretty_term_label(term),
      effect_type = "Ordinal odds ratio",
      effect = odds_ratio,
      effect_low = odds_ratio_low,
      effect_high = odds_ratio_high,
      effect_ci = make_est_ci(effect, effect_low, effect_high, 2),
      p_value_formatted = fmt_p(p_value)
    ) |>
    select(term, term_label, effect_type, effect, effect_low, effect_high,
           estimate, std_error, statistic, p_value, effect_ci, p_value_formatted,
           model_fit_status)
} else {
  table5_main <- purchase_glmer |>
    filter(term != "(Intercept)") |>
    mutate(
      term_label = pretty_term_label(term),
      effect_type = "Odds ratio for purchase yes",
      effect = odds_ratio,
      effect_low = odds_ratio_low,
      effect_high = odds_ratio_high,
      effect_ci = make_est_ci(effect, effect_low, effect_high, 2),
      p_value_formatted = fmt_p(p.value),
      p_value = p.value
    ) |>
    select(term, term_label, effect_type, effect, effect_low, effect_high,
           estimate, std.error, statistic, p_value, effect_ci, p_value_formatted)
}

write_csv_out(table5_main, "table5_purchase_intent_ordinal_model.csv")

table5_main_formatted <- table5_main |>
  transmute(
    Predictor = term_label,
    `Effect type` = effect_type,
    `Effect [95% CI]` = effect_ci,
    p = p_value_formatted
  )

write_csv_out(table5_main_formatted, "table5_purchase_intent_ordinal_model_formatted.csv")

purchase_lmm_clean <- purchase_lmm |>
  filter(term != "(Intercept)") |>
  transmute(
    outcome_model = "Purchase intent LMM (-1, 0, 1)",
    term,
    term_label = pretty_term_label(term),
    effect_type = "Coefficient",
    effect = estimate,
    effect_low = conf.low,
    effect_high = conf.high,
    p_value = p.value,
    effect_ci = make_est_ci(effect, effect_low, effect_high, 2),
    p_value_formatted = fmt_p(p_value)
  )

purchase_glmer_clean <- purchase_glmer |>
  filter(term != "(Intercept)") |>
  transmute(
    outcome_model = "Purchase yes GLMM",
    term,
    term_label = pretty_term_label(term),
    effect_type = "Odds ratio",
    effect = odds_ratio,
    effect_low = odds_ratio_low,
    effect_high = odds_ratio_high,
    p_value = p.value,
    effect_ci = make_est_ci(effect, effect_low, effect_high, 2),
    p_value_formatted = fmt_p(p_value)
  )

purchase_clmm_clean <- if (has_rows(purchase_clmm)) {
  purchase_clmm |>
    filter(parameter_type == "predictor") |>
    transmute(
      outcome_model = "Purchase intent ordinal CLMM",
      term,
      term_label = pretty_term_label(term),
      effect_type = "Odds ratio",
      effect = odds_ratio,
      effect_low = odds_ratio_low,
      effect_high = odds_ratio_high,
      p_value = p_value,
      effect_ci = make_est_ci(effect, effect_low, effect_high, 2),
      p_value_formatted = fmt_p(p_value)
    )
} else {
  tibble::tibble()
}

table5_supp <- bind_rows(purchase_lmm_clean, purchase_glmer_clean, purchase_clmm_clean) |>
  arrange(outcome_model, match(term, c("liking_z", "price_z", "ham_usually_bought_fyes", "jar_dev_count")))

write_csv_out(table5_supp, "table_s2_purchase_intent_all_models.csv", out_supp_table_dir)

if (!is.null(corr_excl_j09)) {
  write_csv_out(corr_excl_j09, "table_s3_context_gain_correlations_exclude_J09.csv", out_supp_table_dir)
}
if (!is.null(loo_corr)) {
  write_csv_out(loo_corr, "table_s4_leave_one_product_out_context_gain_correlations.csv", out_supp_table_dir)
}
if (!is.null(dir_location_lrt)) {
  write_csv_out(dir_location_lrt, "table_s5_directional_jar_location_interaction_lrt.csv", out_supp_table_dir)
}
if (!is.null(dir_by_location)) {
  write_csv_out(dir_by_location, "table_s6_directional_jar_penalties_by_location.csv", out_supp_table_dir)
}
if (has_rows(boot_corr_ci)) {
  write_csv_out(boot_corr_ci, "table_s7_bootstrap_context_gain_correlation_ci.csv", out_supp_table_dir)
}
if (has_rows(boot_context_ci)) {
  write_csv_out(boot_context_ci, "table_s8_bootstrap_context_gain_product_ci.csv", out_supp_table_dir)
}
if (has_rows(emm_context_gain)) {
  write_csv_out(emm_context_gain, "table_s9_emmeans_product_specific_context_gain.csv", out_supp_table_dir)
}
write_csv_out(composition_corr, "table_s10_composition_product_level_correlations.csv", out_supp_table_dir)

jar_burden_summary <- model_liking_common |>
  filter(!is.na(liking), !is.na(jar_dev_count), !is.na(test_location)) |>
  mutate(
    context_label = if_else(test_location == "home", "Home-use", "Blind lab")
  ) |>
  group_by(context_label, jar_dev_count) |>
  summarise(
    n = n(),
    mean_liking = mean_or_na(liking),
    sd_liking = sd_or_na(liking),
    se_liking = se_or_na(liking),
    ci_low = ci95_low(liking),
    ci_high = ci95_high(liking),
    .groups = "drop"
  )
write_csv_out(jar_burden_summary, "table_s11_jar_deviation_burden_by_context.csv", out_supp_table_dir)

fig1_data <- product_context |>
  mutate(product_label = if_else(product %in% key_products, product, NA_character_))

fig1 <- ggplot(
  fig1_data,
  aes(x = liking_mean_lab, y = liking_mean_home)
) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", linewidth = 0.5) +
  geom_point(aes(size = n_eval_home), shape = 21, fill = "white", colour = "black", stroke = 0.8) +
  ggrepel::geom_text_repel(
    aes(label = product_label),
    na.rm = TRUE,
    size = 3.2,
    max.overlaps = Inf,
    min.segment.length = 0,
    box.padding = 0.35,
    point.padding = 0.25,
    seed = 1
  ) +
  scale_size_area(max_size = 9, name = "Home-use n") +
  coord_equal(xlim = c(3, 8.25), ylim = c(3, 8.25)) +
  labs(
    x = "Blind laboratory liking, product mean",
    y = "Home-use liking, product mean"
  ) +
  paper_theme()

save_plot_all(fig1, "figure1_home_lab_liking_scatter", width = 7, height = 5.2)

fig1_all_labels <- ggplot(fig1_data, aes(x = liking_mean_lab, y = liking_mean_home)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", linewidth = 0.5) +
  geom_point(aes(size = n_eval_home), shape = 21, fill = "white", colour = "black", stroke = 0.8) +
  ggrepel::geom_text_repel(
    aes(label = product),
    size = 2.9,
    max.overlaps = Inf,
    min.segment.length = 0,
    box.padding = 0.35,
    point.padding = 0.25,
    seed = 1
  ) +
  scale_size_area(max_size = 9, name = "Home-use n") +
  coord_equal(xlim = c(3, 8.25), ylim = c(3, 8.25)) +
  labs(
    x = "Blind laboratory liking, product mean",
    y = "Home-use liking, product mean"
  ) +
  paper_theme()

save_plot_all(
  fig1_all_labels,
  "figure_s1_home_lab_liking_scatter_all_labels",
  width = 7, height = 5.2,
  dir = out_supp_figure_dir
)

fig2_data <- product_context |>
  mutate(product_label = if_else(product %in% key_products, product, NA_character_))

fig2 <- ggplot(
  fig2_data,
  aes(x = context_gain_jar_dev_count, y = context_gain_liking)
) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.45) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.45) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 0.6) +
  geom_point(shape = 21, fill = "white", colour = "black", size = 3, stroke = 0.8) +
  ggrepel::geom_text_repel(
    aes(label = product_label),
    na.rm = TRUE,
    size = 3.1,
    max.overlaps = Inf,
    min.segment.length = 0,
    box.padding = 0.35,
    point.padding = 0.25,
    seed = 2
  ) +
  labs(
    x = "JAR-deviation count gap, home-use minus blind lab",
    y = "Liking gap, home-use minus blind lab"
  ) +
  paper_theme()

save_plot_all(fig2, "figure2_context_gain_vs_jar_deviation_gap", width = 7.2, height = 5.2)

if (all(c(
  "context_gain_liking_low", "context_gain_liking_high",
  "context_gain_jar_dev_count_low", "context_gain_jar_dev_count_high"
) %in% names(fig2_data))) {
  fig2_detailed <- ggplot(
    fig2_data,
    aes(x = context_gain_jar_dev_count, y = context_gain_liking)
  ) +
    geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.45) +
    geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.45) +
    geom_smooth(method = "lm", se = FALSE, linewidth = 0.6) +
    geom_errorbar(
      aes(ymin = context_gain_liking_low, ymax = context_gain_liking_high),
      width = 0.02,
      linewidth = 0.35,
      alpha = 0.65,
      na.rm = TRUE
    ) +
    geom_errorbar(
      aes(xmin = context_gain_jar_dev_count_low, xmax = context_gain_jar_dev_count_high),
      orientation = "y",
      height = 0.02,
      linewidth = 0.35,
      alpha = 0.65,
      na.rm = TRUE
    ) +
    geom_point(shape = 21, fill = "white", colour = "black", size = 3, stroke = 0.8) +
    ggrepel::geom_text_repel(
      aes(label = product),
      size = 2.8,
      max.overlaps = Inf,
      min.segment.length = 0,
      box.padding = 0.35,
      point.padding = 0.25,
      seed = 2
    ) +
    labs(
      x = "JAR-deviation count gap, home-use minus blind lab",
      y = "Liking gap, home-use minus blind lab"
    ) +
    paper_theme()

  save_plot_all(
    fig2_detailed,
    "figure_s2_context_gain_vs_jar_deviation_gap_bootstrap_ci",
    width = 7.4,
    height = 5.4,
    dir = out_supp_figure_dir
  )
}

fig3_data <- jar_burden_summary |>
  mutate(
    context_label = factor(context_label, levels = c("Blind lab", "Home-use"))
  )

fig3 <- ggplot(
  fig3_data,
  aes(x = jar_dev_count, y = mean_liking, linetype = context_label, shape = context_label)
) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 2.8) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.08, linewidth = 0.45) +
  scale_x_continuous(breaks = 0:4) +
  labs(
    x = "Number of attributes deviating from JAR",
    y = "Mean liking",
    linetype = "Evaluation context",
    shape = "Evaluation context"
  ) +
  paper_theme()

save_plot_all(fig3, "figure3_jar_deviation_burden_liking", width = 7, height = 5)

fig3_n <- fig3 +
  geom_text(aes(label = paste0("n=", n)), vjust = -1.1, size = 2.6, show.legend = FALSE)

save_plot_all(
  fig3_n,
  "figure_s3_jar_deviation_burden_liking_with_n",
  width = 7,
  height = 5.2,
  dir = out_supp_figure_dir
)

fig4_data <- table4 |>
  arrange(desc(penalty)) |>
  mutate(
    y_pos = dplyr::row_number(),
    y_pos = max(y_pos) - y_pos + 1,
    term_label_chr = as.character(term_label),
    prevalence_label = paste0(term_label_chr, " (", fmt_pct(prevalence_overall, 1), ")")
  )

fig4 <- ggplot(
  fig4_data,
  aes(x = penalty, y = y_pos)
) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.45) +
  geom_errorbar(aes(xmin = penalty_low, xmax = penalty_high), orientation = "y", height = 0.14, linewidth = 0.45) +
  geom_point(size = 2.5) +
  scale_y_continuous(
    breaks = fig4_data$y_pos,
    labels = fig4_data$term_label_chr,
    expand = expansion(add = 0.5)
  ) +
  labs(
    x = "Penalty in liking points",
    y = NULL
  ) +
  paper_theme()

save_plot_all(fig4, "figure4_direction_specific_jar_penalties", width = 7.2, height = 5.2)

fig4_prev_data <- fig4_data |>
  mutate(label_with_prev = paste0(term_label_chr, " (", fmt_pct(prevalence_overall, 1), ")"))

fig4_prev <- ggplot(fig4_prev_data, aes(x = penalty, y = y_pos)) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.45) +
  geom_errorbar(aes(xmin = penalty_low, xmax = penalty_high), orientation = "y", height = 0.14, linewidth = 0.45) +
  geom_point(size = 2.5) +
  scale_y_continuous(
    breaks = fig4_prev_data$y_pos,
    labels = fig4_prev_data$label_with_prev,
    expand = expansion(add = 0.5)
  ) +
  labs(
    x = "Penalty in liking points",
    y = NULL
  ) +
  paper_theme()

save_plot_all(
  fig4_prev,
  "figure_s4_direction_specific_jar_penalties_with_prevalence",
  width = 7.5,
  height = 5.4,
  dir = out_supp_figure_dir
)

fig5_order_top_to_bottom <- c("Usually bought: yes", "Liking, z-score", "Price, z-score", "JAR-deviation count")
fig5_data <- table5_main |>
  mutate(
    term_label = pretty_term_label(term),
    y_pos = length(fig5_order_top_to_bottom) - match(term_label, fig5_order_top_to_bottom) + 1
  ) |>
  filter(!is.na(y_pos))

fig5 <- ggplot(
  fig5_data,
  aes(x = effect, y = y_pos)
) +
  geom_vline(xintercept = 1, linetype = "dashed", linewidth = 0.45) +
  geom_errorbar(aes(xmin = effect_low, xmax = effect_high), orientation = "y", height = 0.14, linewidth = 0.45) +
  geom_point(size = 2.6) +
  scale_x_log10() +
  scale_y_continuous(
    breaks = length(fig5_order_top_to_bottom):1,
    labels = fig5_order_top_to_bottom,
    expand = expansion(add = 0.5)
  ) +
  labs(
    x = "Odds ratio, log scale",
    y = NULL
  ) +
  paper_theme()

save_plot_all(fig5, "figure5_purchase_intent_model_effects", width = 7, height = 5)

if (all(c("context_gain_liking_low", "context_gain_liking_high") %in% names(product_context))) {
  fig_s5 <- product_context |>
    mutate(product = factor(product, levels = product[order(context_gain_liking)])) |>
    ggplot(aes(x = context_gain_liking, y = product)) +
    geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.45) +
    geom_errorbar(aes(xmin = context_gain_liking_low, xmax = context_gain_liking_high), orientation = "y", height = 0.14, linewidth = 0.45) +
    geom_point(size = 2.4) +
    labs(
      x = "Liking gap, home-use minus blind lab",
      y = "Product"
    ) +
    paper_theme()

  save_plot_all(
    fig_s5,
    "figure_s5_product_specific_context_gain_bootstrap_ci",
    width = 7,
    height = 5.4,
    dir = out_supp_figure_dir
  )
}

if (!is.null(loo_corr)) {
  loo_long <- loo_corr |>
    select(product_removed, pearson_liking_vs_jar_dev_count_gap, pearson_liking_vs_jar_severity_gap) |>
    pivot_longer(
      cols = starts_with("pearson"),
      names_to = "correlation_type",
      values_to = "r"
    ) |>
    mutate(
      correlation_type = dplyr::recode(
        correlation_type,
        "pearson_liking_vs_jar_dev_count_gap" = "JAR-deviation count gap",
        "pearson_liking_vs_jar_severity_gap" = "JAR severity gap"
      )
    )

  fig_s6 <- ggplot(loo_long, aes(x = product_removed, y = r, group = correlation_type, linetype = correlation_type, shape = correlation_type)) +
    geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.45) +
    geom_line(linewidth = 0.55) +
    geom_point(size = 2.2) +
    labs(
      x = "Product removed",
      y = "Pearson correlation with liking gap",
      linetype = "JAR gap variable",
      shape = "JAR gap variable"
    ) +
    paper_theme() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  save_plot_all(
    fig_s6,
    "figure_s6_leave_one_product_out_correlations",
    width = 8,
    height = 5,
    dir = out_supp_figure_dir
  )
}

if (!is.null(dir_by_location)) {
  fig_s7_data <- dir_by_location |>
    mutate(
      penalty = if ("penalty" %in% names(dir_by_location)) penalty else -estimate,
      penalty_low = if ("penalty_low" %in% names(dir_by_location)) penalty_low else -conf_high,
      penalty_high = if ("penalty_high" %in% names(dir_by_location)) penalty_high else -conf_low,
      term_label = pretty_term_label(term),
      location_label = if_else(location == "home", "Home-use", "Blind lab"),
      term_label = factor(term_label, levels = rev(unique(term_label[order(penalty, decreasing = TRUE)])))
    )

  fig_s7 <- ggplot(fig_s7_data, aes(x = penalty, y = term_label, shape = location_label)) +
    geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.45) +
    geom_errorbar(aes(xmin = penalty_low, xmax = penalty_high), orientation = "y", height = 0.14, linewidth = 0.4,
                  position = position_dodge(width = 0.55)) +
    geom_point(size = 2.4, position = position_dodge(width = 0.55)) +
    labs(
      x = "Penalty in liking points",
      y = NULL,
      shape = "Evaluation context"
    ) +
    paper_theme()

  save_plot_all(
    fig_s7,
    "figure_s7_direction_specific_penalties_by_location",
    width = 7.5,
    height = 5.4,
    dir = out_supp_figure_dir
  )
}

comp_product <- product_panel |>
  filter(!is.na(liking_mean_home) | !is.na(liking_mean_lab)) |>
  select(
    product,
    salt_measured_mean, fat_measured_mean,
    liking_mean_home, liking_mean_lab,
    jar_dev_count_mean_home, jar_dev_count_mean_lab
  )

jar_comp <- model_liking |>
  group_by(product, test_location) |>
  summarise(
    jar_salt_mean = mean_or_na(jar_salt),
    jar_fat_mean = mean_or_na(jar_fat),
    liking_mean = mean_or_na(liking),
    .groups = "drop"
  ) |>
  left_join(product_panel |> select(product, salt_measured_mean, fat_measured_mean), by = "product") |>
  filter(test_location %in% c("home", "lab")) |>
  mutate(context_label = if_else(test_location == "home", "Home-use", "Blind lab"))

fig_s8a <- ggplot(jar_comp, aes(x = salt_measured_mean, y = jar_salt_mean, shape = context_label)) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.55) +
  geom_point(size = 2.4) +
  ggrepel::geom_text_repel(aes(label = product), size = 2.4, max.overlaps = 12, seed = 3) +
  labs(
    x = "Measured salt content",
    y = "Mean JARSalt",
    shape = "Evaluation context"
  ) +
  paper_theme()

save_plot_all(fig_s8a, "figure_s8a_measured_salt_vs_jarsalt", width = 7, height = 5, dir = out_supp_figure_dir)

fig_s8b <- ggplot(jar_comp, aes(x = fat_measured_mean, y = jar_fat_mean, shape = context_label)) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.55) +
  geom_point(size = 2.4) +
  ggrepel::geom_text_repel(aes(label = product), size = 2.4, max.overlaps = 12, seed = 4) +
  labs(
    x = "Measured fat content",
    y = "Mean JARFat",
    shape = "Evaluation context"
  ) +
  paper_theme()

save_plot_all(fig_s8b, "figure_s8b_measured_fat_vs_jarfat", width = 7, height = 5, dir = out_supp_figure_dir)

purchase_desc <- model_purchase |>
  filter(!is.na(purchase_intent_f)) |>
  mutate(
    purchase_intent_f = factor(purchase_intent_f, levels = c("no", "uncertain", "yes"), labels = c("No", "Uncertain", "Yes"))
  ) |>
  group_by(purchase_intent_f) |>
  summarise(
    n = n(),
    liking_mean = mean_or_na(liking),
    liking_ci_low = ci95_low(liking),
    liking_ci_high = ci95_high(liking),
    jar_dev_count_mean = mean_or_na(jar_dev_count),
    jar_ci_low = ci95_low(jar_dev_count),
    jar_ci_high = ci95_high(jar_dev_count),
    price_mean = mean_or_na(price),
    .groups = "drop"
  )

write_csv_out(purchase_desc, "table_s12_purchase_intent_descriptive_summary.csv", out_supp_table_dir)

fig_s9 <- ggplot(purchase_desc, aes(x = purchase_intent_f, y = liking_mean)) +
  geom_point(size = 2.8) +
  geom_errorbar(aes(ymin = liking_ci_low, ymax = liking_ci_high), width = 0.12, linewidth = 0.45) +
  labs(
    x = "Purchase intent",
    y = "Mean liking"
  ) +
  paper_theme()

save_plot_all(fig_s9, "figure_s9_liking_by_purchase_intent", width = 6, height = 4.5, dir = out_supp_figure_dir)

captions <- tibble::tribble(
  ~item, ~placement, ~caption,
  "Figure 1", "Main text", "Home-use and blind laboratory liking by product. Each point represents one product evaluated in both contexts. Point size indicates the number of home-use evaluations. The dashed diagonal line represents equality between home-use and blind laboratory product means.",
  "Figure 2", "Main text", "Home-lab liking gap and JAR-deviation gap. The JAR-deviation gap was calculated as home-use minus blind laboratory mean JAR-deviation count; therefore, negative x-axis values indicate a higher JAR-deviation burden in the blind laboratory condition. Positive y-axis values indicate higher liking in home-use evaluation.",
  "Figure 3", "Main text", "Mean liking by cumulative JAR-deviation burden and evaluation context. JAR-deviation count is the number of the four assessed attributes—color, fat, salt, and tenderness—that deviated from just-about-right. Error bars show 95% confidence intervals for evaluation-level means.",
  "Figure 4", "Main text", "Direction-specific multivariate JAR penalties. Penalties are shown as positive liking-point losses associated with each deviation from JAR, estimated while jointly adjusting for the other JAR deviations. Prevalence of each deviation is reported in Table 4.",
  "Figure 5", "Main text", "Ordinal mixed model for purchase intent in home-use evaluations. Odds ratios greater than 1 indicate higher odds of stronger purchase intent. The model includes liking, price, usual purchase, JAR-deviation count, and product and consumer random effects.",
  "Table 1", "Main text", "Sample overview. Means are evaluation-level means and are therefore weighted by the number of evaluations per product; product-level home-lab comparisons are reported in Table 2.",
  "Table 2", "Main text", "Product-level home-lab liking gaps and JAR-deviation gaps for the 16 products evaluated in both home-use and blind laboratory contexts. Bootstrap 95% confidence intervals are shown for home-lab liking gaps.",
  "Table 3", "Main text", "Liking model comparison and key fixed effects. The table should emphasize that the blind-lab effect is reduced after adding JAR-deviation count, while the location × JAR-deviation interaction is not significant.",
  "Table 4", "Main text", "Direction-specific multivariate JAR penalties with overall, home-use, and blind laboratory prevalence of each deviation.",
  "Table 5", "Main text", "Ordinal mixed model for purchase intent. Sensitivity analyses using LMM and binary GLMM are provided in Supplementary Table S2."
)

write_csv_out(captions, "captions_and_table_notes.csv", out_dir)

writeLines(
  c(
    "Recommended wording for Results/Discussion:",
    "JAR-deviation burden was context-dependent, whereas JAR penalties themselves were largely stable across home-use and blind laboratory evaluations.",
    "The home-laboratory gap in liking was not primarily due to a change in the hedonic impact of JAR deviations, but to a higher frequency and severity of JAR deviations under blind laboratory conditions.",
    "",
    "Important table note:",
    "Means in Table 1 are evaluation-level means and are therefore weighted by the number of evaluations per product. Product-level home-lab comparisons are reported in Table 2.",
    "",
    "Important Figure 2 note:",
    "JAR-deviation gap = home-use minus blind laboratory. Negative x-axis values indicate more JAR deviations in blind laboratory evaluation.",
    "",
    "Important Figure 4 note:",
    "Figure 4 is ordered with the largest penalty at the top.",
    "",
    "TIFF output note:",
    "All figures are saved as PNG, PDF, and 600-dpi TIFF. TIFF copies for submission are also written to figures_tiff/ and supplementary/figures_tiff/."
  ),
  con = file.path(out_dir, "README_revised_outputs.txt")
)

figure_table_index <- tibble::tribble(
  ~item, ~filename_base_or_csv, ~placement, ~purpose,
  "Table 1", "table1_sample_overview.csv / table1_sample_overview_formatted.csv", "Main text", "Sample size and evaluation-level means by dataset; note that means are product-frequency weighted.",
  "Table 2", "table2_product_context_gain_main.csv / table2_product_context_gain_main_formatted.csv", "Main text", "Short product-level home-lab liking gaps and JAR-deviation gaps.",
  "Table S1", "table_s1_product_context_gain_full.csv", "Supplement", "Full product-level table including product descriptors, composition, price, and purchase variables.",
  "Table 3A", "table3a_liking_model_comparison.csv", "Main text", "Model comparison showing improvement after adding JAR-deviation count.",
  "Table 3B", "table3b_liking_key_fixed_effects.csv", "Main text", "Key fixed effects for location, JAR-deviation count, and their interaction; product fixed-effect models only.",
  "Table S13", "table_s13_liking_random_product_sensitivity.csv", "Supplement", "Sensitivity model with random product effect for the key JAR-deviation count model.",
  "Table 4", "table4_direction_specific_jar_penalties.csv / table4_direction_specific_jar_penalties_formatted.csv", "Main text", "Direction-specific multivariate JAR penalties with prevalence.",
  "Table 5", "table5_purchase_intent_ordinal_model.csv / table5_purchase_intent_ordinal_model_formatted.csv", "Main text", "Ordinal mixed model for purchase intent.",
  "Table S2", "table_s2_purchase_intent_all_models.csv", "Supplement", "Purchase-intent sensitivity models: LMM, binary GLMM, ordinal CLMM.",
  "Figure 1", "figure1_home_lab_liking_scatter", "Main text", "Home-use vs blind-lab product mean liking; equality line only.",
  "Figure 2", "figure2_context_gain_vs_jar_deviation_gap", "Main text", "Main context-gain finding with explicit home-minus-lab axis meaning.",
  "Figure 3", "figure3_jar_deviation_burden_liking", "Main text", "Cumulative JAR-deviation burden and liking by evaluation context.",
  "Figure 4", "figure4_direction_specific_jar_penalties", "Main text", "Multivariate direction-specific JAR penalty forest plot, largest penalties at top.",
  "Figure 5", "figure5_purchase_intent_model_effects", "Main text", "Purchase-intent ordinal CLMM odds ratios.",
  "Figure S1", "figure_s1_home_lab_liking_scatter_all_labels", "Supplement", "Figure 1 with all product labels.",
  "Figure S2", "figure_s2_context_gain_vs_jar_deviation_gap_bootstrap_ci", "Supplement", "Detailed Figure 2 with bootstrap CIs.",
  "Figure S3", "figure_s3_jar_deviation_burden_liking_with_n", "Supplement", "Figure 3 with cell sample sizes.",
  "Figure S4", "figure_s4_direction_specific_jar_penalties_with_prevalence", "Supplement", "Figure 4 with prevalence in y-axis labels.",
  "Figure S5", "figure_s5_product_specific_context_gain_bootstrap_ci", "Supplement", "Product-specific context gain with bootstrap CIs.",
  "Figure S6", "figure_s6_leave_one_product_out_correlations", "Supplement", "Leave-one-product-out robustness of context-gain correlations.",
  "Figure S7", "figure_s7_direction_specific_penalties_by_location", "Supplement", "Exploratory directional penalties by home/lab context.",
  "Figure S8A", "figure_s8a_measured_salt_vs_jarsalt", "Supplement", "Measured salt vs JARSalt product-level validation.",
  "Figure S8B", "figure_s8b_measured_fat_vs_jarfat", "Supplement", "Measured fat vs JARFat product-level validation.",
  "Figure S9", "figure_s9_liking_by_purchase_intent", "Supplement", "Descriptive relationship between liking and purchase-intent category."
)

readr::write_csv(figure_table_index, file.path(out_dir, "figure_table_index.csv"), na = "")

make_tiff_manifest <- function(dir, placement) {
  files <- list.files(dir, pattern = "\\.tiff$", full.names = TRUE)
  if (length(files) == 0) {
    return(tibble::tibble(
      placement = character(),
      file = character(),
      path = character(),
      size_mb = numeric()
    ))
  }
  tibble::tibble(
    placement = placement,
    file = basename(files),
    path = files,
    size_mb = round(file.info(files)$size / 1024^2, 3)
  )
}

tiff_manifest <- dplyr::bind_rows(
  make_tiff_manifest(out_figure_dir, "main figures"),
  make_tiff_manifest(out_figure_tiff_dir, "main figures TIFF submission copy"),
  make_tiff_manifest(out_supp_figure_dir, "supplementary figures"),
  make_tiff_manifest(out_supp_figure_tiff_dir, "supplementary figures TIFF submission copy")
) |>
  arrange(placement, file)

readr::write_csv(tiff_manifest, file.path(out_dir, "tiff_file_manifest.csv"), na = "")

expected_main_tiffs <- paste0(
  c(
    "figure1_home_lab_liking_scatter",
    "figure2_context_gain_vs_jar_deviation_gap",
    "figure3_jar_deviation_burden_liking",
    "figure4_direction_specific_jar_penalties",
    "figure5_purchase_intent_model_effects"
  ),
  ".tiff"
)

missing_main_tiffs <- setdiff(expected_main_tiffs, list.files(out_figure_dir, pattern = "\\.tiff$"))
if (length(missing_main_tiffs) > 0) {
  warning("Some main TIFF figures were not found in the main figures folder: ",
          paste(missing_main_tiffs, collapse = ", "))
} else {
}
