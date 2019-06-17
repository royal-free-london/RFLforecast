#' Forecast one dataset
#'
#' @param x A number.
#' @param y A number.
#' @return The sum of \code{x} and \code{y}.
#' @examples
#'
#TODO
# - option to use forecastTo date instead of forecastLength
# - best way to include prohpet so loads on library(RFLforecast)
# ? no zeros option

rfl_forecast <-
  function(data,
           dateCol,
           activityCol,
           forecastLength,
           frequency='day',
           crossVal=TRUE,
           n_changepoints=25,
           ...) {
    activityCol <- as.name(activityCol) # set colum to predict based on
    dateCol <- as.name(dateCol) # set date column
    ds <- data %>% # reorganise data according to the 'prophet' packages required layout
      rename(ds := !!dateCol, y := !!activityCol) %>%
      mutate(ds = as_datetime(ds)) %>%
      select(ds, y)

    # ds_dateRange <-
    #   difftime(min(ds$ds), max(ds$ds), units = "days") # TODO to use in automated testing variables

    m <- prophet(seasonality.mode = 'multiplicative', n_changepoints = n_changepoints)
    m <- add_country_holidays(m, country_name = 'UK')
    m <- fit.prophet(m, ds)

    future <- make_future_dataframe(m, periods = forecastLength, freq=frequency)

    forecast <- predict(m, future)
    df_combined <- right_join(ds, forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')], keep=)

    if (crossVal) {
      ds_cv <-
        cross_validation(m, ...) #https://facebook.github.io/prophet/docs/diagnostics.html

      list(
        data = df_combined,
        performance = performance_metrics(ds_cv),
        mapePlot = plot_cross_validation_metric(ds_cv, metric = 'mape'),
        model = m,
        forecast = forecast
      )
    } else {
      list(
        data = df_combined,
        model = m,
        forecast = forecast
      )
    }
  }
