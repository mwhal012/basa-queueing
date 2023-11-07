library(tidyverse)
library(rlang)
library(conflicted)
"dplyr" |>
	conflict_prefer_all(quiet = TRUE)

data = "./passenger_data.csv.gz" |>
	read_csv(
		col_types = cols(
			...1 = col_integer(),
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
			Time_of_Day = col_integer(),
			Period_of_Week = col_factor(),
			Day_of_Week = col_integer(),
			Month = col_integer(),
			Season = col_integer(),
			Year = col_integer(),
			Flight_ID = col_integer(),
			Delay_in_Seconds = col_integer(),
			.default = col_guess()
		)
	) |>
	filter(!is.na(C_Start)) |>
	mutate(
		C_average = (C_Start + C0)/2,
		.after = C0
	) |>
	filter(C_average != C_avg)
summary(data)