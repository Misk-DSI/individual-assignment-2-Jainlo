library(tidyr)
library(dplyr)
library(tidyverse)
library(GGally)
install.packages("janitor")
install.packages("here")
library(janitor)
library(here)
install.packages("reactable")
library(reactable)
install.packages("SmartEDA")
library(SmartEDA)

### Set up
# Get current working directory
getwd()
# Make sure the file we want to read from exists
file.exists("./data/menu.csv")
# Read the csv file
menu <- read_csv("./data/menu.csv")

### Explore 
glimpse(menu)
head(menu)
# check to see if there is any missing data
summary(menu)
# Since we have a lot of variables, This is a quick way to look at them all at once
names(menu)
# plot a subset of the data to see if there are interesting relationships
ggpairs(menu, columns=c("Calories", "Sugars", "Saturated Fat (% Daily Value)"))
ggpairs(menu, columns=c("Calories","Cholesterol", "Total Fat (% Daily Value)"))


### Tidy the data
# Clean the variable names
menu %>%
  clean_names() -> menu

# Tidy the dataset
#1. Every column is a variable.
#2. Every row is an observation.
#3. Every cell is a single value.
# Delete serving size cause it's not useful
menu_withou_serv = select(menu, -3)

names(menu_withou_serv)

# Gather the category and item together since they're not very distinct variables
menu_withou_serv %>%
  unite("Item",category, item, sep=" ") -> menu_withou_serv

dim(menu_withou_serv) #260  22

menu_withou_serv %>%
  pivot_longer(!Item, names_to= "Nutritional_Facts", values_to= "Values") -> menu_tidy #data types
dim(menu_tidy) # 5460 3
glimpse(menu_tidy)
reactable(menu_tidy)

### EDA (identifying patterns and relationships among variables)

#check for correlation
#boxplot for Calories vs Category to spot outliers and max calories category
#filter(menu_tidy, Nutritional_Facts %in% "calories") -> dd
#glimpse(dd)

#Through a boxplot we can catch any skewness and check the dispersion of the data
# We can see that there aren't many outliars, and there exists a single extreme outliar 
#in the Chicken & Fish category
menu %>% 
  ggplot(aes(x= calories, y= category)) +
  geom_boxplot() 
#Lets try to find that max value
Max_Calories_ChickenFish <- menu$item[[which.max((menu$calories))]]
Max_Calories_ChickenFish

#Whats the highest calory item in each category
menu %>%
  group_by(category)%>%
  menu$item[[which.max(menu$calories)]] ##this doesnt work cause it gives 240 numbers vs 1 number, how should i fix it

#calories and protien
menu %>%
  group_by(category)%>%
  ggplot(aes(x=calories, y=protein))+
  geom_point()

#calories and cholesterol 
menu %>%
  group_by(category)%>%
  ggplot(aes(x=calories, y=cholesterol))+
  geom_point()

# Sodium is a food perservant, lets check sodium content and see if we can find any interesting insight
# Once again we notice an extreme outlier in the data, which also turns out to be "Chicken McNuggets (40 piece)"
menu %>% 
  ggplot(aes(x= category, y= sodium)) +
  geom_jitter()
Max_Sodium_ChickenFish <- menu$item[[which.max(menu$sodium)]]
Max_Sodium_ChickenFish

#range of sodium content
menu %>%
  group_by(category)%>%
  summarise(min = min(calories),
            max = max(calories)) -> MinMaxCal

reactable(MinMaxCal)
MinMaxCal %>%
  summarise(min = mean(min),
            max = mean(max)) ->AvgMinMax
AvgMinMax
#Skewness

# Are high levels of sodium correlated to something else?
#menu %>%
#  ggplot(aes)

#Does ordering grilled chicken instead of crispy increase a sandwich's nutritional value?
#menu %>% 
#  group_by(item)
#  ggplot(aes(x= "Sandwitch" %in% item, y= category)) +
#  geom_boxplot() 
#r <- str_extract(menu$item, "Sandwitch")
#r
#menu %>% 
#  ggplot(aes(x= calories, y= category)) +
#  geom_pointrange(aes(ymin =  mean(menu$calories) - sd(menu$calories),
#                    ymax = mean(menu$calories) + sd(menu$calories))) +
#  geom_boxplot() 

#According to FDA, sodium daily intake should be less than 2,300 milligrams. McDonalnds items fall between the range of 110 and 783. 
