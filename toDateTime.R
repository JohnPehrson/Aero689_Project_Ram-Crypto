## Changes the normal TimeId for the dataframes to a continuous, readable date-time variable

toDateTime <- function(df) {           

  sf_year <- substr(df$TimeId, 1, 4)  
  sf_month <- substr(df$TimeId, 5, 6)  
  sf_day <- substr(df$TimeId, 7, 8)  
  time_proper <- character(length(sf_year))     
  
  for (i in 1:length(sf_year)) {
    time_proper[[i]] <- paste(sf_day[i],"-",sf_month[i],"-",sf_year[i], sep = "")
  } 
  
  time_proper <- lubridate::dmy(as.character(time_proper))
  df$TimeId <- time_proper
  
  return(df)
}
