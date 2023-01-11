library(lubridate)
library(RPostgreSQL)
library(tidyverse)

readQuery<-function(file) {
  query_text <- readLines(file)
  query_code <- query_text[-(grep("--", query_text))]
  paste(query_code, collapse = " ")
}

babase <- DBI::dbConnect(
  RPostgreSQL::PostgreSQL(),
  host = "localhost",
  port = 22222,
  user = "jansen",
  dbname = "babase",
  password = rstudioapi::askForPassword("Database password"))


swerb_data_query <- readQuery("./sql/swerb_data.sql")
swerb_data_raw <- as.tibble(dbGetQuery(babase, swerb_data_query))

end_of_quarter <- ymd('2018-09-30')

#swerb_data <-
  swerb_data_raw %>%
  filter(between(date, end_of_quarter - years(2), end_of_quarter)) %>%
  group_by(did) %>%
  mutate(times_of_interest = if_else(event == 'B', min(time),
                                     if_else(event == 'E', max(time), time))) %>%
  ungroup() %>%
  filter(time == times_of_interest) %>%
  mutate(year_month = floor_date(date, unit = "month")) %>%
  mutate(trip = if_else(am(hms(time))==TRUE, "Morning", "Afternoon")) %>%
  mutate(observer = if_else(nchar(observers) > 3, "multiple",
                            ifelse(is.na(observers), "no observers",
                                   observers))) %>%
  mutate(driver = if_else(nchar(drivers) > 3, "multiple",
                          ifelse(is.na(drivers), "no drivers",
                                 drivers))) %>%
 # mutate(travel_time = case_when(event == 'B' ~ time_length(hms(time) - hms(depart_time),unit="minute")))
 select(everything())
    
    
swerb_data %>%
  filter(!is.na(depart_time)) %>%
  #filter(depart_time > hms("11:00:00")) %>%
  group_by(observer, year_month, trip) %>%
  summarise(average_depart_time = mean(chron::times(depart_time), na.rm = TRUE),
            average_travel_time = mean(travel_time, na.rm = TRUE),
            sd_travel_time = sd(travel_time, na.rm = TRUE),
            trips = n(),
            missing_time = sum(is.na(depart_time)),
            has_adtime =sum(adcode %in% c('D', 'A'))) %>%
  #mutate(average_depart_time = as.POSIXct(strptime(average_depart_time, format="%H:%M:%S"))) %>%
  mutate(percentage_MDT = has_adtime/trips) %>%
  ungroup() %>%
  ggplot(aes(x=year_month)) +
  geom_point(aes(y=average_travel_time, color = observer)) +
  geom_errorbar(aes(ymin=average_travel_time - sd_travel_time,
                    ymax=average_travel_time + sd_travel_time,
                    color = observer)) +
  geom_text(aes(y=average_travel_time + sd_travel_time + 20, label = trips)) +
  #geom_point(data= swerb_data, aes(x=year_month, y=depart_time),alpha = 0.1 ) +
  geom_line(aes(x=year_month, y=average_travel_time, color = observer)) +
  facet_grid(observer~trip, scales = "free_y") +
  ggthemes::theme_tufte()


