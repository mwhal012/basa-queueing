library(conflicted)
library(tidyverse)
library(rlang)
conflicts_prefer(dplyr::filter)

unzip(zipfile = "passenger_data.zip")
data = "passenger_data.csv" |>
  read_csv()

table18 <- data %>%
  group_by(Month, Period_of_Week, Time_of_Day) %>%
  summarise(
    Num_Pass = n(),
    Unique_Dates = n_distinct(Departure_Date)
  ) %>%
  mutate(
    Num_Min = 360 * Unique_Dates,
    Arrival_Rate = Num_Pass / (360 * Unique_Dates)
  )


table20 <- data %>%
  group_by(Month, Period_of_Week, Time_of_Day) %>%
  summarise(
    Avg_C0 = mean(C0),
    C0_Distribution = prop.table(table(C0)) * 100,
    C0_Value = as.numeric(names(table(C0)))
  ) %>%
  mutate(C0_Distribution = sprintf("%.2f%%", C0_Distribution)) # needs a reframe

# 23a is used in a later calculation.
# It shouldn't be put in the presentation.
table23a <- data %>%
  group_by(Month, Period_of_Week, Time_of_Day) %>%
  summarise(
    avgCstart = mean(C_Start),
    avgCavg = mean(C_avg),
    Avg_C0 = mean(C0),
    Arrival_Rate = n() / (360 * n_distinct(Departure_Date)),
    Count = n(),
    Wait_Avg = mean(Wait_Time, na.rm = TRUE),
    LessThan5 = mean(Wait_Time < 5, na.rm = TRUE) * 100,
    LessThan10 = mean(Wait_Time < 10, na.rm = TRUE) * 100,
    LessThan15 = mean(Wait_Time < 15, na.rm = TRUE) * 100,
    LessThan20 = mean(Wait_Time < 20, na.rm = TRUE) * 100,
    LessThan25 = mean(Wait_Time < 25, na.rm = TRUE) * 100,
    LessThan30 = mean(Wait_Time < 30, na.rm = TRUE) * 100
  ) |>
  filter(!is.na(Wait_Avg))
table23 = table23a |>
  select(!c(avgCavg, Avg_C0, Arrival_Rate))

table25 <- data %>%
  group_by(Month, Period_of_Week, Time_of_Day) %>%
  summarise(
    Arrival_Rate = n() / (360 * n_distinct(Departure_Date)),
    Wait_Avg = mean(Wait_Time, na.rm = TRUE),
    term = Wait_Avg * Arrival_Rate,
    Est_Serv = (term + sqrt(term^2 + 4 * term)) / (2 * Wait_Avg),
    Est_Rho = Wait_Avg * (Est_Serv - Arrival_Rate),
    EstLessThan5 = (1 - (Arrival_Rate/Est_Serv)*exp(-(Est_Serv-Arrival_Rate)*5))*100,
    EstLessThan10 = (1 - (Arrival_Rate/Est_Serv)*exp(-(Est_Serv-Arrival_Rate)*10))*100,
    EstLessThan15 = (1 - (Arrival_Rate/Est_Serv)*exp(-(Est_Serv-Arrival_Rate)*15))*100,
    EstLessThan20 = (1 - (Arrival_Rate/Est_Serv)*exp(-(Est_Serv-Arrival_Rate)*20))*100,
    EstLessThan25 = (1 - (Arrival_Rate/Est_Serv)*exp(-(Est_Serv-Arrival_Rate)*25))*100,
    EstLessThan30 = (1 - (Arrival_Rate/Est_Serv)*exp(-(Est_Serv-Arrival_Rate)*30))*100
  ) |>
  select(!c(Arrival_Rate:term)) |>
  filter(!is.na(Est_Serv))

table29 = data |>
  group_by(Month, Period_of_Week, Time_of_Day) |>
  summarise(
    avgCstart = mean(C_Start),
    avgCavg = mean(C_avg),
    Avg_C0 = mean(C0),
    Arrival_Rate = n() / (360 * n_distinct(Departure_Date)),
    Wait_Avg = mean(Wait_Time, na.rm = TRUE),
    term = Wait_Avg * Arrival_Rate,
    Est_Serv = (term + sqrt(term^2 + 4 * term)) / (2 * Wait_Avg),
    Arr_Rate_per_server = Arrival_Rate / Avg_C0,
    Est_Serv_per_server = Est_Serv / Avg_C0
  ) |>
  select(!c(term, Wait_Avg)) |>
  filter(!is.na(Est_Serv))

plot(table29$Arr_Rate_per_server, table29$Est_Serv_per_server)

mod = table29 |>
  lm(formula = Est_Serv ~ 0 + Avg_C0 + Arrival_Rate)
summary(mod)

coef = coef(mod)

est_perf = function(c, lambda, mins = 5) {
  mu_R = as.numeric(t(coef) %*% c(c, lambda))
  return(1 - (lambda/mu_R)*exp((lambda - mu_R)*mins))
}

table32 = data |>
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

table34 = table23a |>
  mutate(
    pred_serv = pred_serv(p = 0.85, lambda = Arrival_Rate, mins = 10)
  ) |>
  filter(Time_of_Day > 1)

ggplot(data = table34) + geom_point(
  mapping = aes(
    x = Avg_C0,
    y = pred_serv
  )
)

SeptemberData = data |>
  filter(!is.na(Wait_Time)) |>
  filter(Month == 9 & Period_of_Week == "1 - WEEKDAY" & Time_of_Day > 1) |>
  mutate(Time_of_Day = factor(Time_of_Day))

ggplot(
  data = SeptemberData,
  mapping = aes(
    y = Time_of_Day,
    x = Wait_Time,
    group = factor(Time_of_Day),
    colour = factor(Time_of_Day),
    fill = factor(Time_of_Day)
  )
) +
geom_violin(
  alpha = 0.4,
  scale = "count",
  width = 1.3,
  adjust = 1.8,
  linewidth = 0.8,
  draw_quantiles = .85,
  show.legend = FALSE
) +
scale_y_discrete(
  limits = c("4", "3", "2"),
  labels = c("18:00 - 0:00", "12:00 - 18:00", "6:00 - 12:00"),
  name = NULL
) +
scale_colour_viridis_d(
  option = "D",
  end = 0.95,
  direction = -1
) +
scale_fill_viridis_d(
  option = "D",
  end = 0.95,
  direction = -1
) +
coord_cartesian(
  xlim = c(0, 20)
) +
theme_minimal() +
theme(
  plot.title = element_text(hjust = 0.5),
  plot.subtitle = element_text(hjust = 0.5),
  axis.ticks = element_blank(),
  panel.grid = element_blank(),
  validate = TRUE
) +
labs(
  title = "Queue Wait Time by Time of Day",
  subtitle = "September Weekday",
  caption = "with 85th percentile"
) +
xlab("wait (mins)")
