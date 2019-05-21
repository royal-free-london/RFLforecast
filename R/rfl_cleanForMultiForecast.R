#' clean data to use rfl_multiForecast()
#'
#' @param x A number.
#' @param y A number.
#' @return The sum of \code{x} and \code{y}.
#' @examples
rfl_cleanForMultiForecast <-
  function(data, #A data frame. Organised in long format with a time and numeric vairable
           dateCol, #the time/date variable
           activityCol, #the variable being used
           repNAs = FALSE,
           repSmall = FALSE,
           ...) {
    colsKey <- quos(...)
    activityCol <- enquo(activityCol)
    dateCol <- enquo(dateCol)

    df_sum <- data %>%
      group_by(!!dateCol,!!!colsKey) %>%
      summarise(!!activityCol := sum(!!activityCol))#make sure data is at 'Key' level

    if (repNAs) {
      df_postNAs <- df_sum %>%
        spread(!!dateCol, !!activityCol) %>%
        mutate_all(funs(replace(., is.na(.), 0))) %>% # replace NAs with zeros (so forecasting works)
        gather(!!dateCol, !!activityCol,-c(!!!colsKey))
    } else {
      df_postNAs <- df_sum
    }

    if (repSmall) {
      df_postSmall <- df_postNAs %>%
        spread(!!dateCol, !!activityCol) %>%
        mutate(tooSmall = ifelse(
          rowSums(df_noNAs %>% select(-Key)) > smallSpecTotalVol,
          "no",
          "yes"
        )) # tag small STS
      select(-tooSmall) %>%
        gather(!!dateCol, !!activityCol,-c(!!!colsKey))
    } else {
      df_postSmall <- df_postNAs
    }

    df_incKey <- df_postSmall %>%
      mutate(Key = paste(!!!colsKey, sep = "_")) %>% #make unique key
      ungroup() %>%
      select(-c(!!!colsKey))

    df_clean <-  df_incKey %>%
      group_by(Key, !!dateCol) %>%
      summarise(!!activityCol := sum(!!activityCol))

    return(df_incKey)

  }
