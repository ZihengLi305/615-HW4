library(data.table)
library(lubridate)
library(dplyr)

file_root <- "https://www.ndbc.noaa.gov/view_text_file.php?filename=44013h"
tail <- ".txt.gz&dir=data/historical/stdmet/"

years <- 1985:2023

fulldata_list <- list()

for (year in years) {
  path <- paste0(file_root, year, tail)
  header <- scan(path, what = 'character', nlines = 1)
  skip_lines <- ifelse(year >= 2007, 2, 1)
  buoy <- fread(path, header = FALSE, skip = skip_lines)
  num_cols <- ncol(buoy)
  if (length(header) > num_cols) {
    header <- header[1:num_cols]
  } else if (length(header) < num_cols) {
    header <- c(header, paste0("V", (length(header) + 1):num_cols))
  }
  
  colnames(buoy) <- header
  
  if ("YY" %in% colnames(buoy)) {
    buoy$YY <- ifelse(buoy$YY < 100, ifelse(buoy$YY > 20, 1900 + buoy$YY, 2000 + buoy$YY), buoy$YY)
  }
  
  if ("YY" %in% colnames(buoy) & "MM" %in% colnames(buoy) & "DD" %in% colnames(buoy) & "hh" %in% colnames(buoy) & "mm" %in% colnames(buoy)) {
    buoy$Date <- ymd_hms(paste(buoy$YY, buoy$MM, buoy$DD, buoy$hh, buoy$mm))
  }
  
  fulldata_list[[as.character(year)]] <- buoy
}

fulldata_list <- rbindlist(fulldata_list, fill = TRUE)

fulldata_list <- fulldata_list %>%
  mutate(Year = coalesce(as.numeric(YY), as.numeric(YYYY), as.numeric(`#YY`))) %>%
  select(-YY, -YYYY, -`#YY`) %>%
  select(Year, everything())

fulldata_list <- fulldata_list %>%
  mutate(Wind_Direction = coalesce(WD, WDIR)) %>%
  select(-WD, -WDIR)

fulldata_list <- fulldata_list %>%
  mutate(Pressure = coalesce(BAR, PRES)) %>%
  select(-BAR, -PRES)

if (all(c("Year", "MM", "DD", "hh", "mm") %in% colnames(fulldata_list))) {
  fulldata_list[, date := ymd_hms(paste(Year, MM, DD, hh, mm))]
}

fulldata_list = fulldata_list %>% mutate(date=0)

summary(fulldata_list[, .(Year, MM, DD, hh, mm)])

unique(fulldata_list$Year)
unique(fulldata_list$MM)
unique(fulldata_list$DD)
unique(fulldata_list$hh)
unique(fulldata_list$mm)


fulldata_list <- fulldata_list %>%
  mutate(date = ifelse(complete.cases(Year, MM, DD, hh, mm),
                       make_datetime(year = Year, month = MM, day = DD, hour = hh, min = mm),
                       as.POSIXct(NA)))

fulldata_list$date <- make_datetime(
  year = ifelse(is.na(fulldata_list$Year), 2000, fulldata_list$Year), 
  month = ifelse(is.na(fulldata_list$MM), 1, fulldata_list$MM),      
  day = ifelse(is.na(fulldata_list$DD), 1, fulldata_list$DD),         
  hour = ifelse(is.na(fulldata_list$hh), 0, fulldata_list$hh),        
  min = ifelse(is.na(fulldata_list$mm), 0, fulldata_list$mm)
)

str(fulldata_list)

