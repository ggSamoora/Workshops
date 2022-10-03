# load the tidyverse library
# if not installed, install using the following line: install.packages("tidyverse")
library(tidyverse)

# load the dataset
df <- read_csv("R-Ladies Dammam Data.csv")

# mutate function
# creates a new column in the dataset based on existing columns
# for this example we are creating two new columns
# 1st: converting rental price from Omani Riyal to Saudi Riyal
# 2nd: calculating price (in Saudi Riyal) per square meter
df2 <- df %>% mutate(rental_price_SR = rental_price * 9.76) %>% 
  mutate(price_per_m2_SR = rental_price_SR / size_in_m2)

# select function
# allows us to select specific columns in the dataset
df %>% select(city, furnished, rental_price)

# arrange
# sorts a dataset by a certain column
# by default, it sorts in ascending order
df3 <- df %>% arrange(rental_price)

# to sort by descending order, use the desc() function
df3 <- df %>% arrange(desc(rental_price))

# filter
# filters dataset based on a condition in a column or columns
# this example filters the dataset to show apartments that have 2 rooms
# AND the rental price is less than 200 Omani riyals
# AND the city is Muscat
df %>% filter(num_of_rooms == "Two", rental_price < 200, city == "Muscat") %>% View()

# group_by and summarise
# allow us to effectively group data and summarise based on our criteria
# so this example creates a new dataframe which shows the average rental price for each num_of_rooms category
df %>% 
  group_by(num_of_rooms) %>% 
  summarise(avg_price = mean(rental_price)) 

# ggplot2 
# bar chart to show the number of apartments per city
ggplot(data = df, mapping = aes(x=city)) + 
  geom_bar()

# scatterplot to show the relation between size and rental price
ggplot(data = df, mapping = aes(x=size_in_m2, y=rental_price, color=age)) +
  geom_point() + 
  facet_wrap(~city) # this line creates separate scatterplots based on the city


# bonus question not in recording
# how to create a violin plot
ggplot(df, aes(x=rental_type, y=rental_price)) +
  geom_violin()

# how to mutate a new column which creates 3 different pricing groups
df5 <- df %>% 
  mutate(price_group = if_else(rental_price >= 0 & rental_price < 200, "0 to 200", 
                               if_else(rental_price >= 200 & rental_price < 500, "200 to 500", "500 or more")))

         