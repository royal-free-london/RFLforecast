#' Forecast Many datasets
#'
#' @param x A number.
#' @param y A number.
#' @return The sum of \code{x} and \code{y}.
#' @examples
#'
##TODO
# - Make names of model and mapePlot == Key


rfl_multiforecast <-
  function (df_clean,
            key = "Key",
            dateCol = "Date",
            activityCol = "Vol",
            forecastLength = 52,
            frequency = 'day',
            n.changepoints=25,
            changepoint.range = 0.8,
            changepoint.prior.scale = 0.05,
            crossVal = TRUE,
            units = "weeks",
            horizon = 10) {

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
        n.changepoints=n.changepoints,
        changepoint.range = changepoint.range,
        changepoint.prior.scale = changepoint.prior.scale,
        crossVal=crossVal,
        horizon = horizon,
        units = units,
        frequency=frequency
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
