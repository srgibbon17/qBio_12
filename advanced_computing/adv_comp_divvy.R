library(tidyverse)
library(lubridate)
library(glue)

# load the data
bike_data <- read_csv('BSD-QBio/tutorials/advanced_computing/data/202207-divvy-tripdata.csv')
bd <- bike_data

# use lubridate to convert to datetime format
# and tidyverse to convert to factors
bd <- bd %>%
  mutate(started_at = as_datetime(started_at),
         ended_at = as_datetime(ended_at), 
         member_casual = factor(member_casual), 
         rideable_type = factor(rideable_type), 
         m_day = mday(date(started_at)))

plot_ride_map <- function(ride_date, df){
  # ride_date should be of form "YYYY-MM-DD"
  # filter the data by date and plot
  filter(df, date(df$started_at) == ride_date) %>%
    ggplot(aes(color = member_casual)) + 
      geom_point(aes(x=start_lng, y=start_lat), alpha=0.1) + 
      geom_point(aes(x=end_lng, y=end_lat), alpha=0.1) +
      geom_segment(aes(x=start_lng, y=start_lat, xend=end_lng, yend=end_lat), alpha=0.01) +
      theme_bw() +
      xlab('Longitude') + 
      ylab('Latitude') + 
      ggtitle(glue("Starting locations for {ride_date}")) + 
      coord_quickmap()
}

# call the function for an arbitrary date
plot_ride_map(ride_date='2022-07-15', df=bd)


# bar plot
bd %>%
  ggplot(aes(x=m_day, fill = rideable_type)) + 
    geom_bar() + 
    facet_wrap(~member_casual) + 
    ylab('Number of rides') + 
    xlab('Day of July 2022')

# busiest hour and day of the week
# add weekday to bd
bd <- bd %>%
  mutate(weekday = wday(started_at), 
         start_hour = hour(started_at))

# weekday labels
wday_labels <- c(
  '1' = 'Sun',
  '2' = 'Mon',
  '3' = 'Tues',
  '4' = 'Wed',
  '5' = 'Thur',
  '6' = 'Fri',
  '7' = 'Sat'
)

# plot counts by starting hour, faceted by weekday
bd %>%
  ggplot(aes(x=hour(started_at))) + 
    geom_bar() + 
    facet_wrap(~weekday, nrow=1, ncol=7, labeller = labeller(weekday=wday_labels)) +
    ylab('Ride Counts') +
    xlab('Hour') + 
    ggtitle('Ride Count by Starting Time and Weekday')

# plot the normalized counts
days_in_July <- unique(date(bd$started_at))
# count the days by weekday
wday_counts <- table(wday(days_in_July))
# normalize the count data
normalized_counts <- bd %>%
  count(weekday, start_hour)

# almost certainly a better way to do this, but this works...
for (row in 1:168) {
  normalized_counts$n[row] <- normalized_counts$n[row] / wday_counts[normalized_counts$weekday[row]]
}

normalized_counts %>%
  ggplot(aes(x=start_hour, y=n)) + 
  geom_col() + 
  facet_wrap(~weekday, nrow=1, ncol=7, labeller = labeller(weekday=wday_labels)) +
  ylab('Ride Counts/Day') +
  xlab('Hour') + 
  ggtitle('Normalized Ride Count by Starting Time and Weekday')

  
  
ggplot(aes(x=hour(started_at), weights=1/wday_counts)) + 
  geom_bar() + 
  facet_wrap(~weekday, nrow=1, ncol=7, labeller = labeller(weekday=wday_labels)) +
  ylab('Ride Counts') +
  xlab('Hour') + 
  ggtitle('Ride Count by Starting Time and Weekday')


# plot ride lengths
# add a ride length column to the data
bd <- bd %>%
  mutate(ride_length = ended_at - started_at, 
         ride_length_classes = case_when(
           ride_length > minutes(30) ~ 'Long',
           ride_length < minutes(5) ~ 'Short',
           T ~ 'Medium'
         ))

wday_labels <- c('Sun','Mon','Tues','Wed','Thur','Fri','Sat')

bd %>%
  ggplot(aes(x=weekday)) + 
    geom_bar() +
    facet_wrap(~ride_length_classes) + 
    ggtitle('Ride Counts by Length and Weekday') + 
    xlab('Weekday') + 
    ylab('Count')

# load the neighborhood data
neighborhood_data <- read_csv('position_neighborhoods_3dp.csv')
# rename
names(neighborhood_data) <- c('start_lat','start_lng','neighborhood','locality','county')


