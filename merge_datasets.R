library(tidyverse)
library(rlang)
library(conflicted)
"dplyr" |>
	conflict_prefer_all(quiet = TRUE)

# cast columns to common types
# for the purpose of merging compatible dataframes
common_parse = function(x) {
	x |>
		read_csv(
			col_types = cols(
				S2 = col_character(),
				Sch_Departure = col_character(),
				Act_Departure = col_character(),
				Departure_Date = col_character(),
				Departure_Time = col_character()
			)
		)
}

# place leading zeroes on hours, minutes, seconds
# if they are of length one
leadzero = function(x) {
	if_else(
		nchar(x) == 1,
		true = 0 |>
			paste0(x),
		false = x |>
			as.character()
	)
}

# throws warnings about NA in datetime fields
data = "datasets/years20262030.csv" |>
	common_parse() |>
	bind_rows(
		"datasets/BASA_AUC_2028_912.csv" |>
			common_parse()
	)
Pc = "datasets/dat_P_sub_c.csv" |>
	read_csv(
		col_types = cols(
			S2 = col_character(),
			Sch_Departure = col_character(),
			Act_Departure = col_character(),
			Departure_Date = col_character()
		)
	) |>
	mutate(
		dep_hour = Act_Departure |>
			hour() |>
			leadzero(),
		dep_min = Act_Departure |>
			minute() |>
			leadzero()
	) |>
	mutate(
		Departure_Time = dep_hour |>
			paste(
				dep_min,
				"00",
				sep = ":"
			),
		.keep = "unused",
		.after = Departure_Date
	)
data = data |>
	bind_rows(Pc)
rm(Pc)
data = data |>
	select(
		!c(Period_of_Week, valid_P_ID:Sch_Act_Flag)
	) |>
	mutate(
		Time_of_Day = parse_number(Time_of_Day),
		Day_of_Week = parse_number(Day_of_Week),
		Month = parse_number(Month),
		Season = parse_number(Season)
	) |>
	mutate(
		Period_of_Week = if_else(
			Day_of_Week <= 5, # Monday is 1, Tuesday is 2 etc.
			true = "1 - WEEKDAY",
			false = "2 - WEEKEND"
		),
		.after = Day_of_Week
	) |>
	type_convert(
		col_types = cols(
			Airfield = col_factor(),
			S2 = col_datetime(),
			C_avg = col_double(),
			Sch_Departure = col_datetime(),
			Act_Departure = col_datetime(),
			BFO_Dest_City = col_factor(),
			BFO_Destination_Country_Code = col_factor(),
			Departure_Date = col_date(format = "%Y-%m-%d"),
			Departure_Time = col_time(format = "%H:%M:%S")
		),
		guess_integer = TRUE
	) |>
	relocate(X, .before = "Airfield")
# Fill in missing Act_Departure values
# using Departure_Date and Departure_Time
for (
	row_index in data |>
		select(Act_Departure) |>
		is.na() |>
		which()
) {
	data$Act_Departure[row_index] = data$Departure_Date[[row_index]] |>
		paste(
			data$Departure_Time[[row_index]],
			sep = " "
		) |>
		as_datetime()
}
data = data |> # remove nonsensical S2 >= Act_Departure
	filter(
		is.na(S2) | S2 <= Act_Departure,
		Airfield %in% c("AUC", "SAF"),
		!is.na(C0) & C0 != 0
	)

data |>
	write_csv(
		file = "./passenger_data.csv",
		append = FALSE,
		col_names = TRUE,
		quote = "needed",
		escape = "backslash"
	)