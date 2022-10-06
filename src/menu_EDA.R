library(tidyr)
library(dplyr)
library(tidyverse)
library(GGally)
install.packages("janitor")
install.packages("here")
library(janitor)
library(here)

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

### EDA (identifying patterns and relationships among variables)

