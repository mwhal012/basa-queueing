library(tidyverse)
library(rlang)
library(stringi)
library(conflicted)
"dplyr" |>
	conflict_prefer_all(quiet = TRUE)

# place leading zeroes on hours and minutes
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

leadzero_datetime = function(x) {
  if_else(
    str_detect(x, " ....$"), # only one hour digit
      true = str_split(x, " ") |>
        stri_join_list(sep = " 0"),
      false = x
  )
}

data = "datasets/dat_P_sub_c.csv" |>
	read_csv(
		col_types = cols(
			S2 = col_character(),
			Sch_Departure = col_character(),
			Act_Departure = col_character(),
			Departure_Date = col_character()
		)
	)|>
  mutate(
    S2 = leadzero_datetime(S2),
    Sch_Departure = leadzero_datetime(Sch_Departure),
    Act_Departure = leadzero_datetime(Act_Departure)
  ) |>
  type_convert( # needed to parse Act_Departure in next step
    col_types = cols(
      S2 = col_datetime(),
      Sch_Departure = col_datetime(),
      Act_Departure = col_datetime()
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
	mutate(
		Time_of_Day = parse_number(Time_of_Day),
		Month = parse_number(Month),
		Season = parse_number(Season)
	) |>
	mutate(
		Period_of_Week = if_else(
			Day_of_Week < 6, # Monday is 1, Tuesday is 2 etc.
			true = "1 - WEEKDAY",
			false = "2 - WEEKEND"
		),
		.after = Day_of_Week
	) |>
  mutate(
    BFO_Dest_City = BFO_Dest_City |>
      str_detect(pattern = regex("[0-9]$")) |>
      if_else(
        true = BFO_Dest_City,
        false = BFO_Dest_City |>
          str_remove(pattern = regex("^BOR"))
      )
  ) |>
	type_convert(
		col_types = cols(
			Airfield = col_factor(),
			C_avg = col_double(),
			BFO_Dest_City = col_factor(),
			BFO_Destination_Country_Code = col_factor(),
			Departure_Date = col_date(format = "%Y-%m-%d"),
			Departure_Time = col_time(format = "%H:%M:%S")
		),
		guess_integer = TRUE
	) |>
  select(!c(valid_P_ID, WT_flag:Sch_Act_Flag))

data = data |> # remove nonsensical S2 >= Act_Departure as well as S2 = NA etc.
	filter(
		S2 <= Act_Departure,
		!is.na(C0) & C0 != 0,
		!is.na(BFO_Dest_City) & BFO_Dest_City != "."
	)

data |>
	write_csv(
		file = "./passenger_data.csv",
		append = FALSE,
		col_names = TRUE,
		quote = "needed",
		escape = "backslash"
	)
zip(
  zipfile = "./passenger_data.zip",
  files = "./passenger_data.csv"
)
file.remove("./passenger_data.csv")
