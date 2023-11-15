library(conflicted)
library(tidyverse)
library(rlang)
conflicts_prefer(dplyr::filter)

unzip(zipfile = "passenger_data.zip")
data = "passenger_data.csv" |>
  read_csv()

tab_29 = data |>
  group_by(Month, Period_of_Week, Time_of_Day) |>
  summarise(
    Avg_C0 = mean(C0),
    Arrival_Rate = n() / (360 * n_distinct(Departure_Date)),
    Wait_Avg = mean(Wait_Time, na.rm = TRUE),
    term = Wait_Avg * Arrival_Rate,
    Est_Serv = (term + sqrt(term^2 + 4 * term)) / (2 * Wait_Avg),
    Arr_Rate_per_server = Arrival_Rate / Avg_C0,
    Est_Serv_per_server = Est_Serv / Avg_C0
  ) |>
  select(!term) |>
  filter(!is.na(Wait_Avg))

plot(tab_29$Arr_Rate_per_server, tab_29$Est_Serv_per_server)

mod = tab_29 |>
  lm(formula = Est_Serv ~ 0 + Avg_C0 + Arrival_Rate)
summary(mod)

coef = coef(mod)

est_perf = function(c, lambda, mins = 5) {
  mu_R = as.numeric(t(coef) %*% c(c, lambda))
  return(1 - (lambda/mu_R)*exp((lambda - mu_R)*mins))
}

tab_32 = data |>
  group_by(Month, Period_of_Week, Time_of_Day) |>
  summarise(
    Arrival_Rate = n() / (360 * n_distinct(Departure_Date)),
    Wait_Avg = mean(Wait_Time, na.rm = TRUE),
    term = Wait_Avg *Arrival_Rate,
    Est_Serv = (term + sqrt(term^2 + 4 * term)) / (2 * Wait_Avg),
    Est_Rho = Wait_Avg * (Est_Serv - Arrival_Rate),
    perf_lt5 = est_perf(c = Est_Serv, lambda = Arrival_Rate, mins = 5) * 100,
    perf_lt10 = est_perf(c = Est_Serv, lambda = Arrival_Rate, mins = 10) * 100,
    perf_lt15 = est_perf(c = Est_Serv, lambda = Arrival_Rate, mins = 15) * 100,
    perf_lt20 = est_perf(c = Est_Serv, lambda = Arrival_Rate, mins = 20) * 100,
    perf_lt25 = est_perf(c = Est_Serv, lambda = Arrival_Rate, mins = 25) * 100,
    perf_lt30 = est_perf(c = Est_Serv, lambda = Arrival_Rate, mins = 30) * 100
  ) |>
  select(!c(Arrival_Rate:term)) |>
  filter(!is.na(Est_Serv))

pred_serv = function(p = 1, lambda, mins = 5) {
  term = lambda * mins
  arg = (term/(1 - p))*exp(term)
  return((lamW::lambertW0(arg) - coef[2]*term)/(coef[1]*mins))
}

tab_34 = data |>
  group_by(Month, Period_of_Week, Time_of_Day) |>
  summarise(
    Arrival_Rate = n() / (360 * n_distinct(Departure_Date)),
    Wait_Avg = mean(Wait_Time, na.rm = TRUE),
    term = Wait_Avg *Arrival_Rate,
    Est_Serv = (term + sqrt(term^2 + 4 * term)) / (2 * Wait_Avg),
    perf_lt5 = est_perf(c = Est_Serv, lambda = Arrival_Rate, mins = 5) * 100,
    perf_lt10 = est_perf(c = Est_Serv, lambda = Arrival_Rate, mins = 10) * 100,
    perf_lt15 = est_perf(c = Est_Serv, lambda = Arrival_Rate, mins = 15) * 100,
    perf_lt20 = est_perf(c = Est_Serv, lambda = Arrival_Rate, mins = 20) * 100,
    perf_lt25 = est_perf(c = Est_Serv, lambda = Arrival_Rate, mins = 25) * 100,
    perf_lt30 = est_perf(c = Est_Serv, lambda = Arrival_Rate, mins = 30) * 100
  ) |>
  select(!term)
