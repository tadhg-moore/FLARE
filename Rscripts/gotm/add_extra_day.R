add_extra_day <- function(df, tz = 'GMT'){
  last_row <- df[nrow(df),]
  new_date <- format((as.POSIXct(last_row[1,1], tz = tz)+86400), format = '%Y-%m-%d %H:%M:%S')
  last_row[1,1] <- new_date
  df <- rbind.data.frame(df, last_row)
  return(df)
}