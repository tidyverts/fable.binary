library(fpp3)

melb_rain <- readr::read_csv(here::here("data-raw/MelbourneRainfall.csv")) |>
  rename(Rainfall = RainfallAmount_millimetres) |> 
	mutate(
		Date = dmy(Date), # Date
		Wet = (Rainfall > 0), # Rainfall indicator
	) |>
	as_tsibble(index = Date)

usethis::use_data(melb_rain, overwrite = TRUE)
