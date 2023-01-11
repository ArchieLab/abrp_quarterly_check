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

swerb_data <-  
  swerb_data_raw %>% 
  arrange(date) %>% 
  filter(between(date, end_of_quarter - years(2), end_of_quarter)) %>%  
  mutate(year_month = fct_inorder(format(date, format="%b-%y"), ordered=TRUE))  %>% 
  mutate(trip = if_else(am(hms(time))==TRUE, "Morning", "Afternoon")) %>%
  #mutate(trip = factor(trip, levels = c("Morning", "Afternoon"))) %>%
  group_by(did) %>% 
  mutate(times_of_interest = if_else(event == 'B', min(time),
                                     if_else(event == 'E', max(time), NA_character_))) %>% 
  ungroup() %>% 
  filter(time == times_of_interest) %>%
  #mutate(year_month = floor_date(date, unit = "month")) %>%
  # mutate(year_month = factor()) %>% 
  # mutate(year_month = factor(format(date, format="%b-%y"), 
  #                            levels=seq(min(swerb_data$date), max(swerb_data$date), by = "1 month"))) %>% 
  mutate(observer = ifelse(is.na(observers), "no observers",
                           if_else(nchar(observers) > 3, "multiple", 
                                   observers))) %>%
  
  mutate(driver = ifelse(is.na(drivers), "no drivers",
                         if_else(nchar(drivers) > 3, "multiple", 
                                 drivers))) %>%
  mutate(travel_time = case_when(event == 'B' ~ time_length(hms(time) - hms(depart_time),unit="minute"))) %>% 
  mutate(adcode_description = if_else(adcode_description == "Descent", "MDT observed",
                                    if_else(adcode_description == "Ascent", "MAT observed", 
                                            adcode_description))) %>% 
  mutate(adcode_description = factor(adcode_description, levels = c("Descent, before arrival", "MDT observed", 
                                                                    "Descent, time unknown",
                                                                    "MAT observed", "Ascent, after departure")))  

swerb_summary <- swerb_data %>%
  filter(!is.na(depart_time)) %>%
    group_by(observer, year_month, trip) %>%
    summarise(average_depart_time = mean(chron::times(depart_time), na.rm = TRUE),
              average_travel_time = mean(travel_time, na.rm = TRUE),
              sd_travel_time = sd(travel_time, na.rm = TRUE),
              trips = n(),
              missing_time = sum(is.na(depart_time))
              ) %>% 
  ungroup() %>% 
  mutate(average_depart_time = as.POSIXct(strptime(average_depart_time, format="%H:%M:%S"))) %>% 
  mutate(trips = if_else(observer %in% c("no observers", "multiple"), NA_integer_, trips))
         

observer_colors <- c("ILS" = "#4b6cf2",
                     "JKW" = "#FF3200", 
                     "RSM" = "#1BB6AF", 
                     "SNS" = "#E9A17C", 
                     "multiple" = "#878787",
                     "no observers" = "#d8d6d6") 

observer_sizes <- c("ILS" = 5,
                     "JKW" = 5, 
                     "RSM" = 5, 
                     "SNS" = 5, 
                     "multiple" = 3,
                     "no observers" = 3) 




swerb_summary %>% 
  filter(!(trip == "Afternoon" & average_depart_time < as.POSIXct(strptime("12:00:00", format="%H:%M:%S")))) %>% 
  arrange(desc(observer)) %>% 
  mutate(y_min = if_else(trip == "Morning", 
                        as.POSIXct(strptime("5:00:00", format="%H:%M:%S")),
                        as.POSIXct(strptime("12:00:00", format="%H:%M:%S"))),
         y_max = if_else(trip == "Morning", 
                        as.POSIXct(strptime("5:45:00", format="%H:%M:%S")),
                        as.POSIXct(strptime("12:45:00", format="%H:%M:%S")))) %>%
  ggplot(aes(x=year_month, y=average_depart_time, color = observer)) + 
    geom_blank(aes(y = y_min)) +
    geom_blank(aes(y = y_max)) +
    #geom_line(show.legend=FALSE) +
    geom_point(aes(size = observer)) +
    scale_size_manual(values = observer_sizes) +
    geom_text(aes(label=trips), size = 2, color = "black") +
    facet_wrap(~trip, nrow=2, scales = "free_y") +
    scale_color_manual(values=observer_colors)  +
    ggtitle("Average depart times from camp") +
    xlab("Year-month") +
    ylab("Time") +
    ggthemes::theme_tufte() +
    theme(axis.text.x = element_text(angle = 90))

ggsave("./plots/depart_times.pdf", width = 11, height = 8.5)

swerb_summary %>% 
  filter(!(trip == "Afternoon" & average_depart_time < as.POSIXct(strptime("12:00:00", format="%H:%M:%S")))) %>% 
  ggplot(aes(x=year_month, y=average_travel_time, color = observer)) + 
    #geom_line(show.legend=FALSE) +
    geom_point(aes(size = observer)) +
    scale_size_manual(values = observer_sizes) +
    geom_text(aes(label=trips), size = 2, color = "black") +
    facet_wrap(~trip, nrow=2) +
    # ylim(45, 120) +
    scale_y_continuous(breaks = seq(45,120,by = 15)) +
    scale_color_manual(values=observer_colors)  +
    ggtitle("Average travel times from camp to first group") +
    xlab("Year-month") +
    ylab("Duration in minutes") +
    ggthemes::theme_tufte() +
    theme(axis.text.x = element_text(angle = 90))

ggsave("./plots/travel_times.pdf", width = 11, height = 8.5)

# # swerb_MDT <-
#   swerb_data %>%
#     filter(trip == "Morning" & event == "B") %>%
#     group_by(observer, year_month, trip) %>%
#     summarise(trips = n(),
#               has_MDT = sum(!is.na(adtime))) %>%
#     ungroup() %>%
#     mutate(percentage_MDT = has_MDT/trips) %>%
#     ggplot(aes(x=year_month, y=percentage_MDT, color = observer)) + 
#         geom_line(show.legend=FALSE) +
#         geom_point(size = 5) +
#         geom_text(aes(label=trips), size = 2, color = "black") +
#         facet_wrap(~trip, nrow=2, scales = "free_y") +
#         scale_color_manual(values=observer_colors)  +
#         ggtitle("Proportion of trips that have a MDT") +
#         xlab("Year-month") +
#         ylab("% of trips") +
#         ggthemes::theme_tufte()
# 
# ggsave("./plots/MDT_times.pdf", width = 11, height = 8.5)


# # swerb_MAT <-
#   swerb_data %>%
#     filter(trip == "Afternoon" & event == "E") %>%
#     group_by(observer, year_month, trip) %>%
#     summarise(trips = n(),
#               has_MDT = sum(!is.na(adtime))) %>%
#     ungroup() %>%
#     mutate(percentage_MDT = has_MDT/trips) %>%
#     ggplot(aes(x=year_month, y=percentage_MDT, color = observer)) + 
#         geom_line(show.legend=FALSE) +
#         geom_point(size = 5) +
#         geom_text(aes(label=trips), size = 2, color = "black") +
#         facet_wrap(~trip, nrow=2, scales = "free_y") +
#         scale_color_manual(values=observer_colors)  +
#         ggtitle("Proportion of trips that have a MAT") +
#         xlab("Year-month") +
#         ylab("% of trips") +
#         ggthemes::theme_tufte()
# 
# ggsave("./plots/MAT_times.pdf", width = 11, height = 8.5)    


brks <- c(0, 0.25, 0.5, 0.75, 1)

adcode_colors <- c("Descent, before arrival" = "#878787", 
                   "MDT observed" = "#FF3200", 
                   "Descent, time unknown" = "#1BB6AF",
                   "MAT observed" = "#4b6cf2",
                   "Ascent, after departure" = "#d8d6d6") 


swerb_data %>% 
  filter(nchar(observers) == 3) %>%
  filter((trip == "Morning" & event == "B") |  trip == "Afternoon" & event == "E") %>%
  filter(!is.na(observers)) %>%
  group_by(observer, year_month, trip, adcode_description) %>%
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count)) %>%
  ggplot(aes(x = factor(year_month), y = perc, fill = factor(adcode_description))) +
    geom_bar(stat="identity", width = 0.7) +
    scale_y_continuous(breaks = brks, labels = scales::percent(brks)) +
    labs(x = "Year-month", y = "Percentages", fill = "Adcode") +
    scale_fill_manual(values=adcode_colors)  +
    facet_grid(trip~observer) +
    ggtitle("Proportion of adcodes") +
    ggthemes::theme_tufte() + 
    theme(axis.text.x = element_text(angle = 90))
  
ggsave("./plots/MDT-MAT_alternative.pdf", width = 11, height = 8.5)    
  
swerb_data %>% 
    filter(trip == "Afternoon" & event == "E") %>%
  filter(!is.na(observers)) %>%
  group_by(observer, year_month, trip, adcode_description) %>%
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count)) %>%
  ggplot(aes(x = factor(year_month), y = perc, fill = factor(adcode_description))) +
    geom_bar(stat="identity", width = 0.7) +
    scale_y_continuous(breaks = brks, labels = scales::percent(brks)) +
    labs(x = "Year-month", y = "Percentages", fill = "Adcode") +
  facet_wrap(~observer) +
  ggthemes::theme_tufte() + 
  ggtitle("Proportion of adcodes for last group in PM") +
  theme(axis.text.x = element_text(angle = 90))
  
ggsave("./plots/MAT_alternative.pdf", width = 11, height = 8.5)  
  
  
  
  