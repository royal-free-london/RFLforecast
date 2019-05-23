#' Forecast Many datasets
#'
#' @param x A number.
#' @param y A number.
#' @return The sum of \code{x} and \code{y}.
#' @examples
#'
##TODO Make names of model and mapePlot == Key


rfl_multiforecast <- function (df_clean, key="Key", dateCol="Date", activityCol="Vol", units="weeks", forecastLength=52, crossVal=TRUE,   horizon=10) {
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
        crossVal=crossVal,
        horizon = horizon,
        units = units
      )),

      data=map(modelOutputs, ~ .[["data"]]),
      forecast=map(modelOutputs, ~ .[["forecast"]]),
      model=map(modelOutputs, ~ .[["model"]])
    )



  return <- list()
  return$data <- predictions %>% unnest(data)
  return$models <- predictions$model
  return$forecasts <- predictions %>% unnest(forecast)

  if (crossVal) {
    predictions <- predictions %>%
      mutate(
        performance = map(modelOutputs, ~ .[["performance"]]),
        mapePlot = map(modelOutputs, ~ .[["mapePlot"]])
      )
    return$performance <- predictions %>% unnest(performance)
    return$mapePlots <- predictions$mapePlot
  }

  return
}
