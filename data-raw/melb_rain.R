library(fpp3)

melb_rain <- "https://github.com/gshmueli/ptsf-R3-code-data/raw/main/Data/MelbourneRainfall.csv" |>
	readr::read_csv() |>
  rename(
    Rainfall = RainfallAmount_millimetres
  ) |> 
	mutate(
		Date = dmy(Date), # Date
		Wet = (Rainfall > 0), # Rainfall indicator
	) |>
	as_tsibble(index = Date)

usethis::use_data(melb_rain, overwrite = TRUE)
