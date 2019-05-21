#' Forecast Many datasets
#'
#' @param x A number.
#' @param y A number.
#' @return The sum of \code{x} and \code{y}.
#' @examples
rfl_multiforecast <- function (df_clean, key="Key", dateCol="Date", activityCol="Vol", units="weeks", forecastLength=52,  horizon=10) {
    key <- as.name(key)

  predictions <- df_clean %>%
    nest(-!!key) %>%
    mutate(modelOutputs = map(
      data,
      ~ rfl_forecast(
        .,
        dateCol,
        activityCol,
        forecastLength,
        horizon = horizon,
        units = units
      )),
      performance=map(modelOutputs, ~ .[["performance"]]),
      data=map(modelOutputs, ~ .[["data"]]),
      forecast=map(modelOutputs, ~ .[["forecast"]]),
      model=map(modelOutputs, ~ .[["model"]]),
      mapePlot=map(modelOutputs, ~ .[["mapePlot"]])
    )

  return <- list()
  return$data <- predictions %>% unnest(data)
  return$performance <- predictions %>% unnest(performance)
  return$models <- predictions$model
  ##NEXT Make names of model and mapePlot == Key
  return$forecasts <- predictions %>% unnest(forecast)
  return$mapePlots <- predictions$mapePlot

  return
}
