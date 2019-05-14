# All visulization with R
# Boxplot
library(ggplot2)
library(gridExtra)
library(treemap)
airline_1997 = read.csv('airline_1997.csv')
airline_2002 = read.csv('airline_2002.csv')
airline_1997 = airline_1997[,1 &16]
airline_2002 = airline_2002[,1 &16]
airline_1997_2002 = rbind(airline_1997, airline_2002)
airline_1997_2002$Year = as.factor(airline_1997_2002$Year)

ggplot(data = airline_1997_2002, mapping = aes(x = Year, y = DepDelay)) +
  geom_boxplot() +
  coord_cartesian(ylim = c(-50, 50)) +
  labs(title = 'Departure delay by year')

# treemap
draw_treemap = function(data_name, year, time_range) {
  data_path = paste('Desktop/480 final/tables/', data_name, sep = '')
  data_path
  df = read.csv(data_path)
  treemap(df,
          index=c(time_range),
          vSize=paste('flight_amount_', year, sep = ''),
          vColor=paste('average_departure_delay_', year, sep = ''),
          type="value",
          title = paste('Average Departure Delay And Flight Amount By ', 
                        time_range, sep = ''),
          fontsize.title = 13
  )
}

# different months
draw_treemap('by_month_1997.csv', '1997', 'month')
draw_treemap('by_month_2002.csv', '2002', 'month')

# different hours
#hour 24 only has 197 observarions, which is not normal, we cobine it with 0
draw_treemap(data_name = 'by_hour_1997.csv', year = 1997, time_range = 'hours')

draw_treemap(data_name = 'by_hour_2002.csv', year = 2002, time_range = 'hours')



# line graph
# different months
draw_line_graph_month = function(data_name1, data_name2) {
  df1 = read.csv(paste('Desktop/480 final/tables/', 
                       data_name1, sep = ''))
  df2 = read.csv(paste('Desktop/480 final/tables/', 
                       data_name2, sep = ''))
  df1$year = 1997
  df2$year = 2002
  colnames(df1) = c('month', 'average_departure_delay',
                    'min_departure_delay', 'max_departure_delay',
                    'std_departure_delay', 'X50_percent_departure_delay',
                    'flight_amount','year')
  colnames(df2) = c('month', 'average_departure_delay',
                    'min_departure_delay', 'max_departure_delay',
                    'std_departure_delay', 'X50_percent_departure_delay',
                    'flight_amount','year')
  df = rbind(df1, df2)
  df$month = as.factor(df$month)
  df$year = as.factor(df$year)
  ggplot(data = df, mapping = aes(x = month, y = average_departure_delay, 
                                  group = year, col = year)) + 
    geom_point() + 
    geom_line() +
    labs(title = 'Average Departure Delay VS Month') +
    theme(plot.title = element_text(hjust = 0.5))
  
}
draw_line_graph_month('by_month_1997.csv', 'by_month_2002.csv')

# different hours
draw_line_graph_hour = function(data_name1, data_name2) {
  df1 = read.csv(paste('Desktop/480 final/tables/', 
                       data_name1, sep = ''))
  df2 = read.csv(paste('Desktop/480 final/tables/', 
                       data_name2, sep = ''))
  df1$year = 1997
  df2$year = 2002
  colnames(df1) = c('hours', 'average_departure_delay',
                    'flight_amount', 'year')
  colnames(df2) = c('hours', 'average_departure_delay',
                    'flight_amount', 'year')
  df = rbind(df1, df2)
  df$hours = as.factor(df$hours)
  df$year = as.factor(df$year)
  ggplot(data = df, mapping = aes(x = hours, y = average_departure_delay, 
                                  group = year, col = year)) + 
    geom_point() + 
    geom_line() +
    labs(title = 'Average Departure Delay VS Hour') +
    theme(plot.title = element_text(hjust = 0.5))
  
}

draw_line_graph_hour(data_name1 = 'by_hour_1997.csv', 'by_hour_2002.csv')