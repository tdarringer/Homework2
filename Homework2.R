library(dplyr)
library(tidyr)
library(tibble)
library(readr)
library(stringr)
library(forcats)
library(purrr)
sales_raw <- tibble(
  sale_id = 1:8,
  customer_id = c("C1", "C2", "C1", "C3", "C2", "C4", "C5", "C2"),
  item_raw = c(" Pen", "PEN ", "Notebook", " pen ",
               "FOLDER", "Folder ", " ERASER ", "pen"),
  amount_raw = c("$10.00", "$20.00", NA, "$15.00",
                 "$30.00", "$25.00", NA, "$20.00"),
  discount_raw = c("0%", "10%", "5%", "0%", "20%", "10%", "0%", "0%"),
  code_raw = c("ST-101", " ON-102", "ST-103", "ON-104",
               "bad", "ST-106", "ON-107X", "on-108"),
  tags = c("gift;urgent", "online", "gift", "member",
           "online;member", "gift", "member", "gift;member")
)
# Problem 1B

sales_codes <- sales_clean %>%
  mutate(
    code_clean = code_raw %>%
      str_trim() %>%
      str_to_upper(),
    
    valid_code = str_detect(
      code_clean,
      "^(ST|ON)-\\d{3}$"
    )
  )

sales_codes %>%
  select(
    sale_id,
    code_raw,
    code_clean,
    valid_code
  )
invalid_codes <- sales_codes %>%
  filter(!valid_code) %>%
  select(
    sale_id,
    code_raw,
    code_clean
  )

invalid_codes

valid_code_parts <- sales_codes %>%
  filter(valid_code) %>%
  select(
    sale_id,
    code_clean
  ) %>%
  separate_wider_delim(
    code_clean,
    delim = "-",
    names = c("channel", "number")
  )

valid_code_parts

sale_tags <- sales_clean %>%
  select(
    sale_id,
    tags
  ) %>%
  separate_longer_delim(
    tags,
    delim = ";"
  )

sale_tags
# separate_wider_delim() splits one string into multiple columns,
# while separate_longer_delim() splits one string into multiple rows.

# Problem 1C

customer_summary <- sales_clean %>%
  summarise(
    sales_n = n(),
    known_n = sum(!is.na(net_amount)),
    large_n = sum(net_amount >= 20, na.rm = TRUE),
    
    large_share = if_else(
      known_n == 0,
      NA_real_,
      large_n / known_n
    ),
    
    .by = customer_id
  ) %>%
  arrange(customer_id)

customer_summary
# For C1, the numerator of large_share is large_n = 0.
# The denominator is known_n = 1 because one of C1's two sales
# has a missing net amount. Therefore large_share = 0 / 1 = 
sales_factors <- sales_clean %>%
  mutate(
    item_factor = fct_infreq(
      factor(item_clean)
    ),
    
    item_group = fct_lump_n(
      factor(item_clean),
      n = 2,
      other_level = "Other"
    )
  )

levels(sales_factors$item_factor)

sales_factors %>%
  count(item_factor)

levels(sales_factors$item_group)

sales_factors %>%
  count(item_group)
# fct_infreq() changes the order of factor levels based on frequency
# without combining categories.
# fct_lump_n(..., n = 2) keeps the two most frequent categories
# and combines all remaining categories into "Other".

# Problem 2

reports_raw <- tibble(
  form = c("A", "A", "A", "B", "B", "B"),
  line = c(1, 2, 3, 1, 2, 3),
  customer_id = c("C1", NA, "C2", "C3", NA, "C4"),
  round = c(1, 3, 1, 1, 2, 1),
  primary_amount = c(10, -999, 20, NA, 0, 30),
  backup_amount = c(NA, 15, 25, NA, 99, NA)
)

customers <- tibble(
  customer_id = c("C1", "C2", "C3", "C5"),
  region = c("North", "South", NA, "West")
)

# Problem 2A

reports_labeled <- reports_raw %>%
  arrange(form, line) %>%
  group_by(form) %>%
  fill(
    customer_id,
    .direction = "down"
  ) %>%
  ungroup()

reports_labeled

reports_clean <- reports_labeled %>%
  mutate(
    primary_clean = na_if(
      primary_amount,
      -999
    ),
    
    amount = coalesce(
      primary_clean,
      backup_amount
    )
  )

reports_clean %>%
  select(
    form,
    line,
    customer_id,
    round,
    primary_amount,
    backup_amount,
    amount
  )
# -999 is converted to NA because it represents a missing primary amount.
# coalesce() uses primary_clean when it is recorded and uses backup_amount
# only when the primary amount is missing. A recorded zero is kept because
# zero is not missing.

reports_base <- reports_clean %>%
  select(
    customer_id,
    round,
    amount
  ) %>%
  mutate(
    recorded_row = TRUE
  )

reports_complete <- reports_base %>%
  complete(
    customer_id,
    round = 1:3
  ) %>%
  mutate(
    recorded_row = replace_na(
      recorded_row,
      FALSE
    )
  ) %>%
  arrange(
    customer_id,
    round
  )

reports_complete

completion_counts <- tibble(
  original_rows = nrow(reports_base),
  added_rows = sum(!reports_complete$recorded_row),
  missing_before = sum(is.na(reports_base$amount)),
  missing_after = sum(is.na(reports_complete$amount))
)

completion_counts

# An explicit missing amount before completion is C3, round 1:
# the row exists, but amount is NA.
#
# An implicit missing customer-round combination is C1, round 2:
# that row did not exist before complete() was used.

# Problem 2B

customer_id_problems <- customers %>%
  filter(
    is.na(customer_id) |
      duplicated(customer_id) |
      duplicated(customer_id, fromLast = TRUE)
  )

customer_id_problems

report_duplicates <- reports_clean %>%
  count(
    customer_id,
    round
  ) %>%
  filter(n > 1)

report_duplicates

customers_marked <- customers %>%
  mutate(
    customer_found = TRUE
  )

reports_joined <- reports_clean %>%
  left_join(
    customers_marked,
    by = join_by(customer_id),
    relationship = "many-to-one",
    na_matches = "never"
  ) %>%
  mutate(
    customer_found = replace_na(
      customer_found,
      FALSE
    )
  )

reports_joined %>%
  select(
    customer_id,
    round,
    amount,
    region,
    customer_found
  )

reports_only <- reports_clean %>%
  distinct(customer_id) %>%
  anti_join(
    customers %>%
      distinct(customer_id),
    by = join_by(customer_id),
    na_matches = "never"
  )

reports_only

customers_only <- customers %>%
  distinct(customer_id) %>%
  anti_join(
    reports_clean %>%
      distinct(customer_id),
    by = join_by(customer_id),
    na_matches = "never"
  )

customers_only
# C3 has customer_found = TRUE even though region is NA because
# C3 exists in the customers table; its region value is simply missing.
#
# C4 has customer_found = FALSE because there is no matching C4 row
# in the customers table.

# Problem 2C

before_duplicate_join <- reports_clean %>%
  summarise(
    row_count = n(),
    total_amount = sum(amount, na.rm = TRUE)
  )

before_duplicate_join

#reports_duplicated <- reports_clean %>%
  #left_join(
  #  customers_bad,
 #   by = join_by(customer_id),
 #   relationship = "many-to-many",
 #   na_matches = "never"
 # )
#
#after_duplicate_join <- reports_duplicated %>%
#  summarise(
  #  row_count = n(),
#    total_amount = sum(amount, na.rm = TRUE)
##  )
#
#after_duplicate_join

# Problem 2C note:
# The assignment references customers_bad, but no customers_bad object
# was defined in the provided setup code shown above.
reports_wide <- reports_complete %>%
  select(
    customer_id,
    round,
    amount
  ) %>%
  pivot_wider(
    names_from = round,
    values_from = amount,
    names_prefix = "round_"
  )

reports_wide

reports_long_again <- reports_wide %>%
  pivot_longer(
    cols = starts_with("round_"),
    names_to = "round",
    names_prefix = "round_",
    values_to = "amount",
    values_drop_na = TRUE
  ) %>%
  mutate(
    round = as.integer(round)
  )

reports_long_again
nrow(reports_long_again)
# I predicted 5 rows after pivoting back to long form.
# values_drop_na = TRUE removes all rows where amount is NA.
# This removes both newly created missing customer-round combinations
# and the original unknown amount for C3 round 1, so that unknown amount
# cannot be recovered after dropping missing values.

# Problem 3A

two_sample_t <- function(
    x,
    y,
    equal_var = FALSE,
    alpha = 0.05
) {
  
  n_x <- length(x)
  n_y <- length(y)
  
  x_bar <- mean(x)
  y_bar <- mean(y)
  
  s2_x <- var(x)
  s2_y <- var(y)
  
  if (equal_var) {
    
    s2_p <- (
      (n_x - 1) * s2_x +
        (n_y - 1) * s2_y
    ) / (
      n_x + n_y - 2
    )
    
    se <- sqrt(
      s2_p * (
        1 / n_x +
          1 / n_y
      )
    )
    
    df <- n_x + n_y - 2
    method <- "Pooled"
    
  } else {
    
    se <- sqrt(
      s2_x / n_x +
        s2_y / n_y
    )
    
    df <- (
      s2_x / n_x +
        s2_y / n_y
    )^2 / (
      (s2_x / n_x)^2 / (n_x - 1) +
        (s2_y / n_y)^2 / (n_y - 1)
    )
    
    method <- "Welch"
  }
  
  t_stat <- (x_bar - y_bar) / se
  
  p_value <- 2 * pt(
    abs(t_stat),
    df = df,
    lower.tail = FALSE
  )
  
  reject <- p_value < alpha
  
  list(
    method = method,
    t_stat = t_stat,
    df = df,
    p_value = p_value,
    reject = reject
  )
}

welch_ref <- t.test(
  x_check,
  y_check,
  var.equal = FALSE
)

pooled_ref <- t.test(
  x_check,
  y_check,
  var.equal = TRUE
)

welch_ref
pooled_ref


verification <- tibble(
  method = c("Welch", "Pooled"),
  
  t_difference = c(
    abs(
      welch_ours$t_stat -
        unname(welch_ref$statistic)
    ),
    abs(
      pooled_ours$t_stat -
        unname(pooled_ref$statistic)
    )
  ),
  
  df_difference = c(
    abs(
      welch_ours$df -
        unname(welch_ref$parameter)
    ),
    abs(
      pooled_ours$df -
        unname(pooled_ref$parameter)
    )
  ),
  
  p_difference = c(
    abs(
      welch_ours$p_value -
        welch_ref$p.value
    ),
    abs(
      pooled_ours$p_value -
        pooled_ref$p.value
    )
  )
) %>%
  mutate(
    all_below_1e8 =
      t_difference < 1e-8 &
      df_difference < 1e-8 &
      p_difference < 1e-8
  )

verification


# Problem 3B

B <- 1000
n_x <- 20
n_y <- 40
alpha <- 0.05

scenarios <- data.frame(
  scenario = c(
    "equal_null",
    "unequal_null",
    "equal_shift",
    "unequal_shift"
  ),
  mean_x = c(0, 0, 0, 0),
  mean_y = c(0, 0, 1, 1),
  sd_x = c(1, 3, 1, 3),
  sd_y = c(1, 1, 1, 1)
)

total_rows <- nrow(scenarios) * B * 2

simulation_results <- tibble(
  scenario = character(total_rows),
  replicate = integer(total_rows),
  method = character(total_rows),
  t_stat = double(total_rows),
  df = double(total_rows),
  p_value = double(total_rows),
  reject = logical(total_rows)
)

set.seed(2026)

row_i <- 1

for (s in seq_len(nrow(scenarios))) {
  
  for (b in seq_len(B)) {
    
    x <- rnorm(
      n = n_x,
      mean = scenarios$mean_x[s],
      sd = scenarios$sd_x[s]
    )
    
    y <- rnorm(
      n = n_y,
      mean = scenarios$mean_y[s],
      sd = scenarios$sd_y[s]
    )
    
    welch_result <- two_sample_t(
      x,
      y,
      equal_var = FALSE,
      alpha = alpha
    )
    
    pooled_result <- two_sample_t(
      x,
      y,
      equal_var = TRUE,
      alpha = alpha
    )
    
    simulation_results[row_i, ] <- list(
      scenarios$scenario[s],
      b,
      welch_result$method,
      welch_result$t_stat,
      welch_result$df,
      welch_result$p_value,
      welch_result$reject
    )
    
    row_i <- row_i + 1
    
    simulation_results[row_i, ] <- list(
      scenarios$scenario[s],
      b,
      pooled_result$method,
      pooled_result$t_stat,
      pooled_result$df,
      pooled_result$p_value,
      pooled_result$reject
    )
    
    row_i <- row_i + 1
  }
}


head(simulation_results, 6)

nrow(simulation_results)



head(simulation_results, 6)

nrow(simulation_results)


total_rows <- nrow(scenarios) * B * 2

simulation_results <- tibble(
  scenario = character(total_rows),
  replicate = integer(total_rows),
  method = character(total_rows),
  t_stat = double(total_rows),
  df = double(total_rows),
  p_value = double(total_rows),
  reject = logical(total_rows)
)

set.seed(2026)

row_i <- 1

nrow(simulation_results)
row_i


nrow(simulation_results)
row_i


simulation_summary <- simulation_results %>%
  summarise(
    mean_t = mean(t_stat),
    sd_t = sd(t_stat),
    rejection_rate = mean(reject),
    .by = c(
      scenario,
      method
    )
  ) %>%
  arrange(
    scenario,
    method
  ) %>%
  mutate(
    across(
      c(
        mean_t,
        sd_t,
        rejection_rate
      ),
      ~ round(.x, 3)
    )
  )

simulation_summary

# Under equal variances and equal means, both tests had Type I error rates
# near 0.05: 0.038 for the pooled test and 0.044 for Welch's test.
# When variances were unequal but the means were still equal, the pooled
# test's rejection rate rose to 0.139, while Welch's test stayed much closer
# to the nominal 0.05 level at 0.061, showing that Welch is better calibrated.
# When the means differed, both tests had high power under equal variances;
# under unequal variances, the pooled test had higher apparent power, but its
# inflated Type I error means that higher rejection rate does not by itself
# make it the better test.

plot_data <- simulation_summary %>%
  select(
    scenario,
    method,
    rejection_rate
  ) %>%
  pivot_wider(
    names_from = method,
    values_from = rejection_rate
  )

rates <- rbind(
  plot_data$Welch,
  plot_data$Pooled
)

barplot(
  rates,
  beside = TRUE,
  names.arg = plot_data$scenario,
  ylim = c(0, 1),
  ylab = "Rejection rate",
  xlab = "Scenario",
  main = "Rejection Rates by Scenario and Test",
  legend.text = c("Welch", "Pooled")
)

abline(
  h = 0.05,
  lty = 2
)