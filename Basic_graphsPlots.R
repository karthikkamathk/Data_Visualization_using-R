# The R packages used are nycflights13, magrittr, ggplot2, viridis and maps (install if needed)
# The datasets in nycflights13 contain lengthy information regarding all the flights that departed from the three New York City airports in 2013

# I have used magrittr format whenever it can provide a more readable and concise style.
# I have also used ggplot2 to create all the plots, and ggsave to export them

# Clean up the current environment
rm(list=ls())

library(nycflights13)
head(flights)
head(airports)

library(magrittr)
library(ggplot2)

### QUESTION 1 -----------------------------------------------------------

# Create a new dataset 'flights_2' that contains only the flights from 'JFK' to 'LAX'
# Recast the 'carrier' variable as a factor, with levels in the following order: 'AA', 'DL', 'UA', 'VX', 'B6'

# Solution

flights_2 = flights %>% subset(and(origin %>% equals('JFK'), dest %>% equals('LAX'))) #dataset containing details of flights from
#from JFK to LAX.

flights_2$carrier %<>% factor(levels = c('AA', 'DL', 'UA', 'VX', 'B6')) #Factorising the carrier variable in the orderered levels listed.


### QUESTION 2 -----------------------------------------------------------

# Create a barplot where the bars show the number of flights from 'JFK' to 'LAX' for each of the carriers

# Solution
pbar = ggplot(data=flights_2, aes(x=carrier, fill=carrier)) + 
  geom_bar( width=0.5) +  #adding bar plot showing number of flights according to the carriers. 
  ylab('No: of Flights') +
  ggtitle('Bar plot for flight counts from JFK to LAX for each carrier')

ggsave(pbar, file = 'Que2_bar_plot_18201357.pdf', width = 8, height = 6)


### QUESTION 3 -----------------------------------------------------------

# Calculate the average delay at arrival for each carrier for flights from 'JFK' to 'LAX'
# Plot the estimated densities for each of the underlying empirical distributions (i.e. 1 figure with 5 continuous lines, each corresponding to a different carrier) 

# Solution

flights_2  %>% is.na %>% colSums #checking missing  values in variables
flights_2 %<>% na.omit #deleting the rows having missing values.

#Average arrival delay for each carriers
flights_2 %>% aggregate(arr_delay ~ carrier, data = ., FUN = . %>% mean)

#density plot for arrival delay with respect ot carriers.
pden = ggplot(data=flights_2, aes(x=arr_delay, colour=carrier)) + 
  geom_density() +   
  ylab('Density') +
  ggtitle('Density plot for each empirical distributions')

ggsave(pden, file = 'Que3_density_plot_18201357.pdf', width = 8, height = 6)

### QUESTION 4 -----------------------------------------------------------

# Create multiple scatterplots to visually explore how delay at departure affects delay at arrival by carriers ('JFK' to 'LAX' only)
# The scatterplots share the same y-axis but have diffent x-axis and different points colours

# Solution

#Scatter plot with respect to departure delay and arrival delay
pscat = ggplot(flights_2, aes(x = dep_delay, y = arr_delay, colour = carrier)) +
  geom_point() + facet_grid(~carrier) + xlab('Departure Delay') +
  ylab('Arrival Delay') +
  ggtitle('Scatter plot for departure delay vs arrival delay')

ggsave(pscat, file = 'Que4_scatter_plot_18201357.pdf', width = 8, height = 4)

### QUESTION 5 -----------------------------------------------------------

# Using the magrittr format, define a function called 'speed' that takes a flights data.frame and adds a new column with value equal to the average speed in miles per hour
# Plot bloxplots for the speed by month, for all flights from 'JFK' to 'LAX'

# Solution

#function speed to find average speed (miles/hour) and adding a varaible to store this result
speed = . %>% transform(., avg_speed = distance %>% divide_by(air_time) %>% multiply_by(60)) 

flights_2info = flights_2 %>% speed #Calling the fucntion speed with flights_2 dataset

#adding box plot showing average speed for flights from JFK to LAX in each months.
pbox = ggplot(data=flights_2info, aes(x = factor(month), y = avg_speed, colour = month)) + 
  geom_boxplot( width=0.5) +   
  xlab('Months') +
  ylab('Average Speed \n(Miles per hour)') +
  ggtitle('Box plot for average speed from JFK to LAX for each month')

ggsave(pbox, file = 'Que5_box_plot_18201357.pdf', width = 8, height = 6)

### QUESTION 6 -----------------------------------------------------------

# The following code is not easy to read
# Add some brief comments to explain what these lines do and to make the code more readable

library(maps) #Loading the library maps.

# A subset from the dataset flights which contains only the carriers 'AA', 'DL', 'UA', 'VX', 'B6' 
# are aggregated according to the number of flights towards each destination. Using set_colnames function 
# destination column is changed to name and number of flights to counts.
airport_info = flights %>% 
  subset(carrier %>% is_in(c('AA', 'DL', 'UA', 'VX', 'B6'))) %>% 
  aggregate(year ~ dest, ., length) %>% 
  set_colnames(c('name','counts'))

N = nrow(airport_info) #Total number of rows in 'airport_info' dataset.

#To the airport_info dataset 2 new varaibles are created by filling NA (null) values.
airport_info$lon = rep(NA,N)
airport_info$lat = rep(NA,N)

#Longitude and Latitude of each destination is found out from the 'airports' dataset by matching 
#the index of the name variable from 'airports_info' dataset. Thus the index for the destination 
#from airports data is used to get the longitude and latitude for the same destination in aiports_info 
#dataset. This is iterated N (total rows in airport_info dataset) times.

for (i in 1:N) {
  index = which(airports$faa == airport_info$name[i])
  if (length(index) != 0) {
    airport_info$lon[i] = airports$lon[index]
    airport_info$lat[i] = airports$lat[index]
  }
}


library(viridis) #Library viridis is loaded for more color scale palettes and functions.

#A graphical plot showing the map of united states is drawn with the help of maps library and ggplot
# and flights counts density are marked in the map according to the latitude and longitude for the 
#destination names. That is bigger the circle larger the flight count in the airport mentioned 
#in the map and vice versa.

#ggplot       - graphical visualisation of latitude and longitude. Here alpha determines the opacity between points.
#borders      - includes the borders of the country.
#geom_point   - marks the point of latitude and longitude on the map and size varies with the flight count.
#theme_void   - Selected an empty theme.
#coord_cartesian - setting limits helps to visualy zoom/magnify the plot without changing the original content.
#scale_color_viridis - viridis colour scale is used
#scale_size_continuous - This function is used for proper scaling of points for a better visibility in the plot.
#legend.position - this is used for changing the legend position and here we have avoided the legend.
#ggtitle      - Used for giving the title for the plot.

ggplot(airport_info, aes(x = lon, y = lat, col = counts, alpha = I(0.75))) + 
  borders('state') + 
  geom_point(aes(size = counts)) + 
  theme_void() + 
  coord_cartesian(xlim = c(-125, -65), ylim = c(25, 50)) +
  scale_color_viridis(option = 'A', direction = -1) +
  scale_size_continuous(range = c(1,15)) +
  theme(legend.position='none') + ggtitle('\t\tAirports by flights counts')


