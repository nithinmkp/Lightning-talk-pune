
# Code for WACR data from RBI(Monthly) ---------------------------------------------
library(tidyverse)
library(zoo)
library(xts)
library(lubridate)
library(janitor)

wacr_data_fn <- function(df, freq = "quarter") {
  # Remove unwanted rows
  df <- df[-c(1:2, 4), ]
  # Rename Columns
  names(df) <- df[1, ]
  # Remove no longer needed row 1
  df <- df[-1, ]
  # Remove special character in column names
  names(df) <- str_replace(names(df), pattern = "\\.", replacement = "")
  # Convert year in to proper format
  df <- df %>% separate(Year, into = c("Year", NA), sep = "-")
  # Split data into multiple series
  if (freq == "year") {
    df_list <- split(df, df$Range) %>% map(~ .x %>% select(c(Year, Range, Annual)))
    names_list <- names(df_list)
    df_final <- df_list %>%
      map(~ .x %>% select(-Range)) %>%
      reduce(left_join, by = "Year")
    names(df_final)[-1] <- names_list
    df_final <- df_final %>% as_tibble()
    df_final[, -1] <- lapply(df_final[, -1], as.numeric)
    df_final <- df_final %>% mutate(across(where(is.numeric), round, 3))
    df_final <- df_final[complete.cases(df_final), ]
  } else {
    df_list <- split(df, df$Range) %>% map(~ .x %>% select(-c(Range, Annual)))
    df_list <- map(df_list, ~ remove_empty(.x, which = "cols"))
    # Function to create dataframe in desired format
    df_fn <- function(dt) {
      dt$Year <- as.numeric(dt$Year)
      if (sum(is.na(dt[nrow(dt), ])) <= 3) {
        dt2 <- data.frame(
          Year = rep(
            c(
              dt$Year,
              dt$Year[length(dt$Year)] + 1
            ),
            c(9, rep(12, nrow(dt) - 1), 3)
          ),
          Month = rep(names(dt)[-1], times = nrow(dt)),
          WACR = c(t(dt[, -1]))
        )
      } else {
        dt2 <- data.frame(
          Year = rep(dt$Year, c(9, rep(12, nrow(dt) - 2), 15)),
          Month = rep(names(dt)[-1], times = nrow(dt)),
          WACR = c(t(dt[, -1]))
        )
      }

      dt2 <- dt2 %>% unite("Year-Month", Year, Month, sep = "-")
      dt2$`Year-Month` <- as.yearmon(dt2$`Year-Month`, "%Y-%b")
      dt2 <- dt2 %>% as_tibble()
      dt2[, 2] <- lapply(dt2[, 2], as.numeric)
      dt2 <- dt2 %>% mutate(across(where(is.numeric), round, 3))
      dt2 <- dt2[complete.cases(dt2), ]
      dt2$`Year-Month` <- as.yearmon(dt2$`Year-Month`)

      return(dt2)
    }
    # Creating the list of datasets
    df_list <- map(df_list, df_fn)

    # MErging the datasets and renaming

    df_final <- df_list %>% reduce(left_join, by = "Year-Month")
    names(df_final)[-1] <- paste0("WACR_", names(df_list))
    return(df_final)
  }
}

wacr_qtr_function <- function(df, fn = "mean") {
  df2 <- group(df, floor(nrow(df) / 3), force_equal = T)
  df2_xts <- as.xts(df2, df2$`Year-Month`)
  df2_qtr <- apply.quarterly(df2_xts[, -c(1, ncol(df2_xts))], fn)
  yr <- year(index(df2_qtr))
  yr_qtr <- paste(yr, paste0("Q", quarter(yearqtr(index(df2_qtr)), fiscal_start = 4)))
  yr_qtr <- as.yearqtr(yr_qtr)

  df2_final <- data.frame(Period = yr_qtr, df2_qtr, row.names = NULL)
  if (fn == "sum") {
    df2_final <- df2_final[, -1] * 3
    df2_final <- data.frame(Period = yr_qtr, df2_final)
  }




  return(df2_final)
}
