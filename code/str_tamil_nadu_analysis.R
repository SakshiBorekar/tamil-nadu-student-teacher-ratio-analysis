# =====================================================================
# What Drives Student-Teacher Ratios Across Tamil Nadu?
# Team: Sakshi Borekar, Manish, Tanvi, Vineetha
#
# This script was recovered from the code chunks embedded in the knitted
# report (Final-project-mark.html). The code is unchanged; chunk markers
# were added so it matches the order of the report.
#
# Needs two data files in the working folder:
#   antyodaya_shrid.csv      (Mission Antyodaya village data)
#   shrid_loc_names.csv      (SHRID -> state/district names)
# Packages: tidyverse, dplyr, janitor, broom, boot, stringr, patchwork, gt
# Note: the district train/test split uses runif() with no seed, so the
# Part A numbers change slightly on every run. The bootstrap uses set.seed(34).
# =====================================================================

# ---- chunk 1 ----
library(tidyverse)
library(dplyr)
library(janitor)
library(broom)
library(boot)
library(stringr)
library(patchwork)
library(gt)

# ---- chunk 2 ----
### Output folders ----
dir.create("outputs", showWarnings = FALSE)
dir.create("outputs/plots", showWarnings = FALSE)
dir.create("outputs/models", showWarnings = FALSE)

### DATA IMPORT ----
data_file <- "antyodaya_shrid.csv"
shrid_names_file <- "shrid_loc_names.csv"
state_of_interest <- "tamil nadu"
min_observations_per_district <- 12

# Read & clean data
antyodaya <- read_csv(data_file, show_col_types = FALSE) |> clean_names() |> distinct()
Shrid_names <- read_csv(shrid_names_file, show_col_types = FALSE) |> clean_names() |> distinct()
antyodaya_merged <- merge(antyodaya, Shrid_names, by = "shrid2", all.x = TRUE)

tn_data <- antyodaya_merged |>
  filter(tolower(state_name) == tolower(state_of_interest)) |>
  mutate(across(where(is.character), ~ na_if(., ""))) |>
  filter(!is.na(total_population))

# ---- chunk 3 ----
# Predictor sets
education_vars <- c(
  "availability_of_high_school", "availability_of_middle_school",
  "availability_of_mid_day_meal_sch", "any_primary_sch_toilet",
  "is_aanganwadi_centre_available", "availability_of_ssc_school"
)

health_vars <- c(
  "total_hhd_availing_pension_under", "sub_centre",
  "total_no_of_pregnant_women", "total_no_of_pregnant_women_recei",
  "phc", "total_childs_aged_0_to_3_years_r"
)

facilities_vars <- c(
  "closed_drainage", "is_post_office_available",
  "total_hhd_having_piped_water_con", "regular_market"
)

other_vars <- c(
  "total_trained_trainee_under_skil", "no_of_children_not_attending_sch",
  "total_no_of_elected_representati", "total_hhd_with_clean_energy",
  "availability_of_railway_station", "total_childs_categorized_non_stu"
)

all_predictors <- c(
  education_vars, health_vars, facilities_vars, other_vars
)

existing_predictors <- all_predictors[all_predictors %in% names(tn_data)]
tn_data <- tn_data |> mutate(across(all_of(existing_predictors), ~ as.numeric(.)))

# ---- chunk 4 ----
# --- helpers ---
safe_rmse <- function(actual, pred) {
  ok <- !(is.na(actual) | is.na(pred))
  if (sum(ok) == 0) return(NA_real_)
  sqrt(mean((actual[ok] - pred[ok])^2))
}

clean_and_compute_str <- function(df) {
  df <- df |> mutate(
    total_primary_school_students = as.numeric(total_primary_school_students),
    total_primary_school_teachers = as.numeric(total_primary_school_teachers)
  )
  df |> filter(
    !is.na(total_primary_school_students),
    !is.na(total_primary_school_teachers),
    total_primary_school_teachers > 0
  ) |> mutate(
    student_teacher_ratio = total_primary_school_students / total_primary_school_teachers
  ) |> filter(
    is.finite(student_teacher_ratio),
    student_teacher_ratio >= 1,
    student_teacher_ratio <= 500
  )
}

build_formula <- function(response, predictors, train_df) {
  preds_exist <- predictors[predictors %in% names(train_df)]
  preds_non_const <- preds_exist[sapply(preds_exist, function(v) {
    vals <- train_df[[v]]
    vals <- vals[!is.na(vals)]
    length(vals) > 0 && length(unique(vals)) > 1
  })]
  if (length(preds_non_const) == 0) return(as.formula(paste(response, "~ 1")))
  as.formula(paste(response, "~", paste(preds_non_const, collapse = " + ")))
}

# ===============================================================
# MAIN LOOP — NO DISTRICT IS SKIPPED (this was your requirement)
# ===============================================================

districts <- tn_data$district_name |> unique() |> na.omit()
summary_list <- list()

for (district in districts) {
  
  message("Processing district: ", district)
  df_d <- tn_data |> filter(district_name == district) |> clean_and_compute_str()
  
  if (nrow(df_d) < min_observations_per_district) {
    message("  ⚠ Few rows but STILL CONTINUING (as requested)")
  }
  
  df_d <- df_d |> mutate(split = ifelse(runif(n()) <= 0.7, "Train", "Test"))
  train <- df_d |> filter(split == "Train")
  test  <- df_d |> filter(split == "Test")
  
  # build & stepwise
  fml <- build_formula("student_teacher_ratio", all_predictors, train)
  base_mod <- lm(fml, data = train)
  model_step <- tryCatch(step(base_mod, direction="both", trace=FALSE),
                         error = function(e) base_mod)
  
  pred_train <- fitted(model_step)
  pred_test  <- tryCatch(predict(model_step, newdata = test), error = function(e) rep(NA, nrow(test)))
  
  rmse_train <- safe_rmse(train$student_teacher_ratio, pred_train)
  rmse_test  <- safe_rmse(test$student_teacher_ratio, pred_test)
  adj_r2_train <- summary(model_step)$adj.r.squared
  
  # ------------------ RESIDUAL PLOT (PINK + SKYBLUE) ------------------
  plot_df <- bind_rows(
    tibble(fitted = pred_train, residuals = resid(model_step), dataset = "Training"),
    tibble(fitted = pred_test, residuals = test$student_teacher_ratio - pred_test, dataset = "Testing")
  )
  
  p <- ggplot(plot_df, aes(fitted, residuals, color = dataset)) +
    geom_point(size = 2, alpha = 0.8) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    scale_color_manual(values = c("Training" = "hotpink", "Testing" = "skyblue")) +
    facet_wrap(~dataset, ncol = 1) +
    labs(
      title = paste("Residuals vs Fitted —", stringr::str_to_title(district)),
      subtitle = paste("Adj R²:", round(adj_r2_train, 3),
                       "| RMSE(train):", round(rmse_train, 3),
                       "| RMSE(test):", round(rmse_test, 3)),
      x = "Fitted STR", y = "Residuals"
    ) +
    theme_minimal() +
    theme(legend.position = "none")
  
  print(p)  # required for RMarkdown output & console preview
  
  ggsave(file.path("outputs/plots",
                   paste0("residuals_", str_replace_all(district, "\\s+", "_"), ".png")),
         p, width = 7, height = 8)
  # ----------------------------------------------------------
  
  summary_list[[district]] <- tibble(
    district = district,
    n_train = nrow(train),
    n_test = nrow(test),
    adj_r2_train = adj_r2_train,
    rmse_train = rmse_train,
    rmse_test = rmse_test
  )
}

# ---- chunk 5 ----
summary_rows <- bind_rows(summary_list)

# 🔥 Show summary table inside the HTML output (not saved to CSV)
summary_rows %>%
  gt() %>%
  tab_header(
    title = "District-wise Model Performance for STR — Tamil Nadu",
    subtitle = "Adj-R², RMSE (Train/Test), Train/Test Sample Split"
  )

# ---- chunk 6 ----
all_predictors_raw <- c(health_vars, education_vars, facilities_vars, other_vars)
all_predictors <- all_predictors_raw[all_predictors_raw %in% names(tn_data)]

missing_vars <- setdiff(all_predictors_raw, names(tn_data))
if (length(missing_vars) > 0) {
  message("Variables removed due to missing: ", paste(missing_vars, collapse = ", "))
}

tn_data <- tn_data %>% mutate(district_name = factor(district_name))


tn_data <- tn_data %>%
  mutate(
    total_primary_school_students = as.numeric(total_primary_school_students),
    total_primary_school_teachers = as.numeric(total_primary_school_teachers),
    student_teacher_ratio = total_primary_school_students / total_primary_school_teachers
  ) %>%
  filter(
    !is.na(student_teacher_ratio),
    is.finite(student_teacher_ratio),
    student_teacher_ratio >= 1,
    student_teacher_ratio <= 200,
    total_primary_school_teachers > 0
  )

# ---------------- Q7: State-level regression ----------------
fml_state <- as.formula(
  paste("student_teacher_ratio ~", paste(all_predictors, collapse = " + "), "+ district_name")
)

continuous_vars <- all_predictors[
  sapply(tn_data[all_predictors], function(x) length(unique(na.omit(x))) > 2)
]

tn_data_scaled <- tn_data %>%
  mutate(across(all_of(continuous_vars), scale))

state_model <- lm(fml_state, data = tn_data_scaled)

summary(state_model)

# ---- chunk 7 ----
coef_table <- broom::tidy(state_model) %>%
  mutate(
    term = str_remove(term, "^district_name"),
    term = str_remove_all(term, "`")
  ) %>%
  rename(
    Variable = term,
    Estimate = estimate,
    Std_Error = std.error,
    P_Value = p.value
  ) %>%
  arrange(desc(abs(Estimate)))   # sorted by effect size

# Split into 2 parts
half_n <- ceiling(nrow(coef_table) / 2)

coef_table_part1 <- coef_table[1:half_n, ]
coef_table_part2 <- coef_table[(half_n + 1):nrow(coef_table), ]

# ---------- TABLE 1 ----------
coef_table_part1 %>%
  gt() %>%
  tab_header(
    title = "Part B — State-Level Regression Coefficients (Table 1)"
  ) %>%
  fmt_number(
    columns = c(Estimate, Std_Error, P_Value),
    decimals = 4
  )

# ---- chunk 8 ----
# ---------- TABLE 2 ----------
coef_table_part2 %>%
  gt() %>%
  tab_header(
    title = "Part B — State-Level Regression Coefficients (Table 2)"
  ) %>%
  fmt_number(
    columns = c(Estimate, Std_Error, P_Value),
    decimals = 4
  )

# ---- chunk 9 ----
tn_model_aug <- broom::augment(state_model)



ggplot(tn_model_aug, aes(x = .fitted, y = .resid)) +
geom_point(alpha = 0.7, color = "blue") +
geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 1) +
labs(
title = "Residuals vs Fitted — State-level Regression Model (Tamil Nadu)",
x = "Fitted Values",
y = "Residuals"
) +
theme_minimal()

# ---- chunk 10 ----
cat("\n\n") # ensures the plot is printed in RMarkdown

# ---- chunk 11 ----
coef_df_q9 <- broom::tidy(state_model) %>%
  dplyr::filter(p.value < 0.05) %>%
  dplyr::filter(!stringr::str_detect(term, "district_name"), term != "(Intercept)")

if(nrow(coef_df_q9) > 0){
  p_q9 <- ggplot(coef_df_q9, aes(x = reorder(term, estimate), y = estimate)) +
    geom_col(fill = "skyblue") +
    geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error),
                  width = 0.25) +
    coord_flip() +
    labs(
      title = "Q9 — Significant Non-district Predictors of STR (Tamil Nadu)",
      x = "Predictor",
      y = "Coefficient Estimate"
    ) +
    theme_minimal()
  
  print(p_q9)      # <<< REQUIRED for plot to show in knitted PDF/HTML
  cat("\n\n")
  
  knitr::kable(coef_df_q9)   # <<< table now prints inside RMarkdown
} else {
  cat("⚠ Q9: No significant (p < 0.05) non-district predictors found.\n\n")
}

# ---- chunk 12 ----
# Q10 — Bootstrapping (same logic, no change in approach)

# Safe bootstrap function
boot_fn_safe <- function(data, indices) {
  d <- data[indices, , drop = FALSE]
  fit <- tryCatch(lm(fml_state, data = d), error = function(e) NULL)

  coef_full <- rep(NA_real_, length(coef(state_model)))
  names(coef_full) <- names(coef(state_model))

  if (!is.null(fit)) {
    cf <- coef(fit)
    coef_full[names(cf)] <- cf
  }
  return(coef_full)
}

# Run bootstrap — only object name updated
set.seed(34)
boot_results <- boot(tn_data_scaled, boot_fn_safe, R = 300)

# Convert to df
boot_estimates <- as.data.frame(boot_results$t)
colnames(boot_estimates) <- names(coef(state_model))

# Summaries
boot_summary <- boot_estimates %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Estimate") %>%
  group_by(Variable) %>%
  summarise(
    Mean = mean(Estimate, na.rm = TRUE),
    Lower_CI = quantile(Estimate, 0.025, na.rm = TRUE),
    Upper_CI = quantile(Estimate, 0.975, na.rm = TRUE)
  ) %>%
  filter(!grepl("district_name", Variable), Variable != "(Intercept)")

# Table — visible in RMarkdown
print(
  knitr::kable(boot_summary,
               caption = "Q10 — Bootstrapped 95% CI for STR Predictors (Tamil Nadu)")
)

# ---- chunk 13 ----
# Plot — visible in RMarkdown (no saving to PNG)
p_q10 <- ggplot(boot_summary, aes(x = reorder(Variable, Mean), y = Mean)) +
  geom_point(size = 3, color = "darkblue") +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), width = 0.25, color = "black") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  coord_flip() +
  labs(
    title = "Q10 — Bootstrapped 95% Confidence Intervals",
    x = "Variable",
    y = "Bootstrap Mean Coefficient"
  ) +
  theme_minimal()

print(p_q10)  # REQUIRED to show in RMarkdown

# ---- chunk 14 ----
cat("\n\n")
