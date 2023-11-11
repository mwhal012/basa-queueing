library(dplyr)

data = passenger_data

table1 <- data %>%
  group_by(Day_of_Week, Time_of_Day) %>%
  summarise(
    Num_Pass = n(),
    Unique_Dates = n_distinct(Departure_Date)
  ) %>%
  mutate(
    Num_Min = 360 * Unique_Dates,
    Arrival_Rate = Num_Pass / (360 * Unique_Dates)
  )

table2 <- data %>%
  group_by(Day_of_Week, Time_of_Day) %>%
  summarise(
    Avg_C0 = mean(C0),
    C0_Distribution = prop.table(table(C0)) * 100,
    C0_Value = as.numeric(names(table(C0)))
  ) %>%
  mutate(C0_Distribution = sprintf("%.2f%%", C0_Distribution))


table3 <- data %>%
  group_by(Day_of_Week, Time_of_Day) %>%
  summarise(
    Wait_Avg = mean(Wait_Time, na.rm = TRUE),
    LessThan5 = mean(Wait_Time < 5, na.rm = TRUE) * 100,
    LessThan10 = mean(Wait_Time < 10, na.rm = TRUE) * 100,
    LessThan15 = mean(Wait_Time < 15, na.rm = TRUE) * 100,
    LessThan20 = mean(Wait_Time < 20, na.rm = TRUE) * 100,
    LessThan25 = mean(Wait_Time < 25, na.rm = TRUE) * 100
  )

table4 <- data %>%
  group_by(Day_of_Week, Time_of_Day) %>%
  summarise(Arrival_Rate = n() / (360 * n_distinct(Departure_Date)),
            Wait_Avg = mean(Wait_Time, na.rm = TRUE),
            Est_Serv = ((Wait_Avg * Arrival_Rate) + sqrt((Wait_Avg * Arrival_Rate)^2 + 4 * (Wait_Avg * Arrival_Rate))) / (2 * Wait_Avg),
            Est_Phi = Wait_Avg * (Est_Serv - Arrival_Rate),
            EstLessThan5 = (1 - (Arrival_Rate/Est_Serv)*exp(-(Est_Serv-Arrival_Rate)*5))*100,
            EstLessThan10 = (1 - (Arrival_Rate/Est_Serv)*exp(-(Est_Serv-Arrival_Rate)*10))*100,
            EstLessThan15 = (1 - (Arrival_Rate/Est_Serv)*exp(-(Est_Serv-Arrival_Rate)*15))*100,
            EstLessThan20 = (1 - (Arrival_Rate/Est_Serv)*exp(-(Est_Serv-Arrival_Rate)*20))*100,
            EstLessThan25 = (1 - (Arrival_Rate/Est_Serv)*exp(-(Est_Serv-Arrival_Rate)*25))*100
  )

