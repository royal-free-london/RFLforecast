#' Summarise output of forecast into next year
#'
#' @param x A number.
#' @param y A number.
#' @return The sum of \code{x} and \code{y}.
#' @examples
#'
##TODO


rfl_summariseForecast <- function(df,
                                  activityCol = NULL,
                                  dateCol = NULL,
                                  output_format = "long",
                                  ...) {
  months <- c(3, 6, 9, 12)
  colsKey <- quos(...)
  activityCol <- enquo(activityCol)
  dateCol <- enquo(dateCol)

  for (i in months) {
    quants <- rfl_getQuantiles(df = df,
                               activityCol = !!activityCol,
                               dateCol = !!dateCol,
                               from = today() %m+% months(i-3),
                               to = today() %m+% months(i),
                               suffix = paste0(i, "_months"),
                               output_format = output_format,
                               !!!colsKey)
    if (i != months[[1]]) {
      quantsAll <- rbind(quantsAll, quants)
    } else {
      quantsAll <- quants
    }
  }
  quantsAll
}

rfl_getQuantiles <-
  function (df,
            activityCol = NULL,
            dateCol = NULL,
            from = today(),
            to = today() + months(12),
            suffix = "12_months",
            output_format = "wide",
            ...) {
    colsKey <- quos(...)
    activityCol <- enquo(activityCol)
    dateCol <- enquo(dateCol)

    quantileData <- df %>%
      select(!!!colsKey,!!activityCol,!!dateCol) %>%
      group_by(!!!colsKey) %>%
      filter(!!dateCol >= from,!!dateCol < to) %>%
       mutate(
         !!paste0("median_", suffix) := median(!!activityCol, na.rm = TRUE),
         !!paste0("65-percentile_",  suffix) := quantile(!!activityCol, 0.65, na.rm = TRUE),
         !!paste0("75-percentile_",  suffix) := quantile(!!activityCol, 0.75, na.rm = TRUE),
          !!paste0("85-percentile_",  suffix) := quantile(!!activityCol, 0.85, na.rm = TRUE)
        ) %>%
      summarise_at(vars(matches(suffix)), mean)
    if(output_format=="long") {
      quantileData <- gather(quantileData, newKey, Vol, -c(!!!colsKey)) %>%
        separate(newKey, c("Metric", "TimePeriod", "TimeUnits"), sep="_")
    }

    quantileData
  }
