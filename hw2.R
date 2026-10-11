install.packages(c("dplyr", "tidyr", "tibble", "readr", "stringr", "forcats", "purrr"))
library(dplyr); library(tidyr); library(tibble)
library(readr); library(stringr); library(forcats); library(purrr)

#PROBLEM 1
sales_raw <- tibble(
  sale_id = 1:8,
  customer_id = c("C1", "C2", "C1", "C3", "C2", "C4", "C5", "C2"),
  item_raw = c(" Pen", "PEN ", "Notebook", " pen ",
               "FOLDER", "Folder ", " ERASER ", "pen"),
  amount_raw = c("$10.00", "$20.00", NA, "$15.00",
                 "$30.00", "$25.00", NA, "$20.00"),
  discount_raw = c("0%", "10%", "5%", "0%", "20%", "10%", "0%", "0%"),
  code_raw = c("ST-101", "ON-102", "ST-103", "ON-104",
               "bad", "ST-106", "ON-107X", "on-108"),
  tags = c("gift;urgent", "online", "gift", "member",
           "online;member", "gift", "member", "gift;member")
)

#PART A
sales_clean<-sales_raw |>
  mutate(
    item_clean=str_to_lower(str_squish(item_raw)),
    amount=parse_number(amount_raw),
    discount_rate=parse_number(discount_raw)/100,
    net_amount=amount*(1-discount_rate),
    size=case_when(
      is.na(net_amount) ~ "unrecorded",
      (net_amount >= 20) ~ "large",
      .default = "small"
    ))

sales_clean |> select(sale_id, item_clean, amount,
                      discount_rate, net_amount, size)


#PART B
sales_codes<-sales_clean |> 
  mutate(
  code_clean=str_to_upper(str_trim(code_raw)),
  valid_code=str_detect(code_clean,"^(ST|ON)-[0-9]{3}$")
  )

invalid_codes<-sales_codes |> filter(!valid_code) |> select(sale_id,code_raw,code_clean)

valid_code_parts <- sales_codes |>
  filter(valid_code) |>
  separate_wider_delim(code_clean, delim = "-",
                       names = c("channel", "number"),
                       cols_remove = FALSE) |>
  select(sale_id, code_clean, channel, number)

valid_code_parts 

sale_tags<-sales_clean |> 
  select(sale_id,tags) |> 
  separate_longer_delim(tags,delim=";")

sale_tags
#separate_wider_delim splits 1 column into several new columns with the same number of rows
#separate_longer_delim splits 1 column into several new rows with same number of columns


#PART C

customer_summary<-sales_clean |>
  summarise(
    sales_n=n(),
    known_n=sum(!is.na(net_amount)),
    large_n=sum(net_amount>=20,na.rm=TRUE),
    large_share = if_else(known_n == 0, NA_real_, large_n / known_n),
    .by = customer_id
  ) |>
  arrange(customer_id)

customer_summary

#C1 has a numerator of 0 and a denominator of 1 which is why the proportion is 0

sales_factors <- sales_clean |> 
  mutate(
    item_factor=fct_infreq(factor(item_clean)),
    item_group=fct_lump_n(factor(item_clean),n=2,other_level="Other")
  ) 
levels(sales_factors$item_factor)
sales_factors |> count(item_factor)

levels(sales_factors$item_group)
sales_factors |> count(item_group)

#fct_infreq uses the frequency of each level and orders it based on most frequent to least
#fct_lump_n keeps the most common n levels and then combines the rest into a merged group
#fct_infreq doesn't change the data only the order of the levels while fct_lump_n does because a new level gets created for the grouped levels


#PROBLEM 2
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

reports_labeled <- reports_raw |>
  arrange(form, line) |>
  group_by(form) |>
  fill(customer_id, .direction = "down") |>
  ungroup()

reports_labeled


reports_clean <- reports_labeled |>
  mutate(
    primary_clean = na_if(primary_amount, -999),
    amount = coalesce(primary_clean, backup_amount)
  )

reports_clean |>
  select(form, line, customer_id, round, primary_amount, backup_amount, amount)

#coalesce() only uses backup_amount when primary_clean is NA. 0 is still data, it just means nothing not missing

reports_complete <- reports_clean |>
  select(customer_id, round, amount) |>
  mutate(recorded_row = TRUE) |>
  complete(customer_id, round = 1:3, fill = list(recorded_row = FALSE))

reports_complete

nrow(reports_clean)
sum(!reports_complete$recorded_row)    
sum(is.na(reports_clean$amount))       
sum(is.na(reports_complete$amount))  

#Explicit missing: C3 round 1 had a row but its amount was NA
#Implicit missing: C1 round 2 had no row at all before complete()

#PART B
sum(duplicated(customers$customer_id))
customers |> filter(is.na(customer_id))
reports_clean |> count(customer_id, round) |> filter(n > 1)


customers_marked <- customers |>
  mutate(customer_found = TRUE)

reports_joined <- reports_clean |>
  left_join(customers_marked,
            by = join_by(customer_id),
            relationship = "many-to-one",
            na_matches = "never") |>
  mutate(customer_found = coalesce(customer_found, FALSE))

reports_joined |>
  select(customer_id, round, amount, region, customer_found)


reports_clean |>
  anti_join(customers, by = join_by(customer_id), na_matches = "never") |>
  distinct(customer_id)

customers |>
  anti_join(reports_clean, by = join_by(customer_id), na_matches = "never") |>
  distinct(customer_id)

#C3 and C4 both have a missing region, but customer_found is TRUE for C3 because it is in customers with no recorded region and FALSE for C4 because it is not in customers at all

#PART C
customers_bad <- bind_rows(customers, tibble(customer_id = "C2", region = "East"))

reports_duplicated <- reports_clean |>
  left_join(customers_bad, by = join_by(customer_id),
            relationship = "many-to-many")

nrow(reports_clean)
nrow(reports_duplicated)
sum(reports_clean$amount, na.rm = TRUE)
sum(reports_duplicated$amount, na.rm = TRUE)

reports_duplicated |> count(customer_id, round) |> filter(n > 1)

#Rows went from 6 to 7 and the total went from 75 to 95. The repeated report is C2 round 1. customers_bad has two C2 rows (South and East), so C2's one
#report matched twice and its amount of 20 was counted twice


#With relationship = "many-to-one" R would throw an error because C2
#matches two rows in customers_bad. Allowing "many-to-many" doesn't fix the problem it just lets the join run and duplicates the row

reports_wide <- reports_complete |>
  select(customer_id, round, amount) |>
  pivot_wider(names_from = round, names_prefix = "round_", values_from = amount)

reports_wide
# 5 rows would remain since values_drop_na = TRUE deletes the 7 NA cells instead of filling them in so the unknown amounts are just removed as rows and never recovered


#PROBLEM 3
two_sample_t <- function(x, y, equal_var = FALSE, alpha = 0.05) {
  n_x <- length(x)
  n_y <- length(y)
  mean_x <- mean(x)
  mean_y <- mean(y)
  var_x <- var(x)
  var_y <- var(y)
  
  if (equal_var) {
    sp2 <- ((n_x - 1) * var_x + (n_y - 1) * var_y) / (n_x + n_y - 2)
    se <- sqrt(sp2 * (1 / n_x + 1 / n_y))
    df <- n_x + n_y - 2
    method <- "Pooled"
  } else {
    se <- sqrt(var_x / n_x + var_y / n_y)
    df <- (var_x / n_x + var_y / n_y)^2 /
      ((var_x / n_x)^2 / (n_x - 1) + (var_y / n_y)^2 / (n_y - 1))
    method <- "Welch"
  }

  
  t_stat <- (mean_x - mean_y) / se
  p_value <- 2 * pt(abs(t_stat), df = df, lower.tail = FALSE)
  
  list(method = method, t_stat = t_stat, df = df,
       p_value = p_value, reject = p_value < alpha)
}

x_check <- c(12, 15, 14, 10, 13)
y_check <- c(9, 11, 8, 10, 7, 12, 9, 8)

welch_result <- two_sample_t(x_check, y_check)
pooled_result <- two_sample_t(x_check, y_check, equal_var = TRUE)
welch_result
pooled_result


welch_ref <- t.test(x_check, y_check, var.equal = FALSE)
pooled_ref <- t.test(x_check, y_check, var.equal = TRUE)

abs(welch_result$t_stat - welch_ref$statistic) < 1e-8
abs(welch_result$df - welch_ref$parameter) < 1e-8
abs(welch_result$p_value - welch_ref$p.value) < 1e-8

abs(pooled_result$t_stat - pooled_ref$statistic) < 1e-8
abs(pooled_result$df - pooled_ref$parameter) < 1e-8
abs(pooled_result$p_value - pooled_ref$p.value) < 1e-8

#All 6 comparisons print TRUE so the differences from t.test() are
#smaller than 1e-8 

B <- 1000
n_x <- 20
n_y <- 40
alpha <- 0.05
scenarios <- data.frame(
  scenario = c("equal_null", "unequal_null",
               "equal_shift", "unequal_shift"),
  mean_x = c(0, 0, 0, 0),
  mean_y = c(0, 0, 1, 1),
  sd_x = c(1, 3, 1, 3),
  sd_y = c(1, 1, 1, 1)
)
# Run once immediately before the simulation, not inside a loop.
set.seed(2026)

n_total <- nrow(scenarios) * B * 2
simulation_results <- data.frame(
  scenario = character(n_total),
  replicate = integer(n_total),
  method = character(n_total),
  t_stat = numeric(n_total),
  df = numeric(n_total),
  p_value = numeric(n_total),
  reject = logical(n_total)
)



row <- 1
for (s in 1:nrow(scenarios)) {
  for (b in 1:B) {
    x <- rnorm(n_x, mean = scenarios$mean_x[s], sd = scenarios$sd_x[s])
    y <- rnorm(n_y, mean = scenarios$mean_y[s], sd = scenarios$sd_y[s])
    
    welch <- two_sample_t(x, y, equal_var = FALSE, alpha = alpha)
    pooled <- two_sample_t(x, y, equal_var = TRUE, alpha = alpha)
    
    simulation_results[row, ] <- list(scenarios$scenario[s], b, welch$method,
                                      welch$t_stat, welch$df, welch$p_value, welch$reject)
    row <- row + 1
    
    simulation_results[row, ] <- list(scenarios$scenario[s], b, pooled$method,
                                      pooled$t_stat, pooled$df, pooled$p_value, pooled$reject)
    row <- row + 1
  }
}

head(simulation_results)
nrow(simulation_results)

simulation_summary <- simulation_results |>
  summarise(
    mean_t = round(mean(t_stat), 3),
    sd_t = round(sd(t_stat), 3),
    rejection_rate = round(mean(reject), 3),
    .by = c(scenario, method)
  )

simulation_summary

#When the means were equal Welch rejected about 5% of the time in both scenarios but the pooled test rejected about 14% of the time when the variances were unequal so its Type I error was too high.
#When the means were different both tests had about 95% power with equal variances but with unequal variances the pooled test rejected more often than Welch
#The pooled test's higher power isn't a good thing here because it also rejects too often when there's no real difference, so some of those rejections are just false positives



