library(tidyverse)
library(rlang)
library(conflicted)
"dplyr" |>
	conflict_prefer_all(quiet = TRUE)

unzip("./passenger_data.zip")
data = "./passenger_data.csv" |>
	read_csv(
		col_types = cols(
			X = col_integer(),
			Airfield = col_factor(),
			S2 = col_datetime(),
			Wait_Time = col_integer(),
			C_Start = col_integer(),
			C0 = col_integer(),
			C_avg = col_double(),
			Sch_Departure = col_datetime(),
			Act_Departure = col_datetime(),
			BFO_Dest_City = col_factor(),
			BFO_Destination_Country_Code = col_factor(),
			order = col_integer(),
			Pass_ID = col_integer(),
			Departure_Date = col_date(),
			Departure_Time = col_time(),
			Time_of_Day = col_factor(),
			Period_of_Week = col_factor(),
			Day_of_Week = col_factor(),
			Month = col_factor(),
			Season = col_factor(),
			Year = col_factor(),
			Flight_ID = col_integer(),
			Delay_in_Seconds = col_integer(),
			.default = col_guess()
		)
	)

# ~1500 observations from Airfield SAF make it unsuitable for clustering
# similarly the destinations below have too few observations
data = data |>
  filter(
    Airfield == "AUC",
    BFO_Destination_Country_Code != "WIC",
    BFO_Dest_City != "BOR008",
    Year == "2028" # otherwise only 4 obs for 2030
  ) |>
  filter(S2 > ymd_hms("2028-08-31 00:00:00")) |>
  mutate(
    BFO_Dest_City_or_CC = if_else(
      BFO_Destination_Country_Code == "BOR",
      true = paste(
        "City",
        BFO_Dest_City,
        sep = "-"
      ),
      false = paste(
        "Country",
        BFO_Destination_Country_Code,
        sep = "-"
      )
    ) |>
      as_factor(),
    .after = BFO_Destination_Country_Code
  ) |>
  unite(
    col = cluster,
    Time_of_Day,
    BFO_Dest_City_or_CC,
    sep = "_",
    remove = FALSE,
    na.rm = FALSE
  ) |>
  mutate(cluster = as_factor(cluster))
summary(data)
# number of hours: 732 apiece
summary(data$cluster)
summary(data$cluster)/732
