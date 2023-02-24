#Gunja Gupta
#22-02-2023
#Horticulture data

#Import libraries 
library(readr)
library(dplyr)
library(ggplot2)
library(tidyverse)

#Read csv
crops_data <- read.csv("C:/Users/Gunja Gupta/Downloads/y2023_feb__data_wrangle_data_viz-20230206T051808Z-001/Horticulture/DataVisualisation_7.5.csv")

###Cleaning data###

#rename columns to lowercase and replace spaces with underscores
colnames(crops_data) <- gsub(" ", "_", tolower(colnames(crops_data)))

#Subsetting for each crop

my_crops_data <- subset(crops_data, name == "Gunja")

citrus_data <- subset(my_crops_data, crop == "Citrus")
grapes_data <- subset(my_crops_data, crop == "Grapes")
guava_data <- subset(my_crops_data, crop == "Guava")
mango_data <- subset(my_crops_data, crop == "Mango")

my_crops_data$table_number <- as.integer(my_crops_data$table_number)

###Summarising data###

#My crops data
names(my_crops_data)
summary(my_crops_data)
head(my_crops_data)
tail(my_crops_data)
nrow(my_crops_data)
ncol(my_crops_data)
summary(select_if(my_crops_data, is.numeric))

#Citrus data
names(citrus_data)
summary(citrus_data)

head(citrus_data)
tail(citrus_data)
nrow(citrus_data)
ncol(citrus_data)

summary(select_if(citrus_data, is.numeric))

#Grapes data
names(guava_data)
summary(guava_data)
head(guava_data)
tail(guava_data)
nrow(guava_data)
ncol(guava_data)
summary(select_if(guava_data, is.numeric))

#Guava data
names(my_crops_data)
summary(my_crops_data)
head(my_crops_data)
tail(my_crops_data)
nrow(my_crops_data)
ncol(my_crops_data)
summary(select_if(my_crops_data, is.numeric))

#Mango data
names(mango_data)
summary(mango_data)
head(mango_data)
tail(mango_data)
nrow(mango_data)
ncol(mango_data)
summary(select_if(mango_data, is.numeric))

###Plotting###

# Calculate total area and production by crop, district, and state
crop_area <- my_crops_data %>% 
  group_by(crop, districts, state) %>% 
  summarise(area_in_thousand_ha = sum(area_in_thousand_ha))

crop_production <- my_crops_data %>% 
  group_by(crop, districts, state) %>% 
  summarise(production_in_thousand_mt = sum(production_in_thousand_mt))

# Create plot of total area and production by crop, district, and state

#Plot 1
# Create plot of total area and production by crop, district, and state
ggplot(crop_area, aes(x = state, y = area_in_thousand_ha, fill = crop)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  labs(x = "State", y = "Area (in thousand ha)", fill = "Crop", 
       title = "Total Area Used for Each Crop in Each State") +
  theme(plot.title = element_text(hjust = 0.5, size = 12 , face = "bold"), 
        axis.text = element_text(size = 8),
        axis.text.x = element_text(size = 8, angle = 90),
        axis.title = element_text(size = 8, face = "bold"))

#Plot 2
#Production
ggplot(crop_production, aes(x = state, y = production_in_thousand_mt, fill = crop)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  labs(x = "State", y = "Production (in thousand mt)", fill = "Crop", 
       title = "Total Production of Each Crop in Each State") +
  theme(plot.title = element_text(hjust = 0.5, size = 12 , face = "bold"), 
        axis.text = element_text(size = 8),
        axis.text.x = element_text(size = 8, angle = 90),
        axis.title = element_text(size = 8, face = "bold"))

#Plot 3
# Create a scatter plot of area and production for each crop
ggplot(my_crops_data, aes(x=area_in_thousand_ha, y=production_in_thousand_mt, color=crop)) +
  geom_point() +
  labs(title="Crop Production in India", x="Area (thousand ha)", y="Production (thousand mt)") +
  theme(plot.title = element_text(hjust = 0.5, size = 12 , face = "bold"), 
        axis.text = element_text(size = 8),
        axis.text.x = element_text(size = 8, angle = 90),
        axis.title = element_text(size = 8, face = "bold"))

#Plot 4
#Boxplot of the Distribution of production across states for each crop
ggplot(my_crops_data, aes(x = crop, y = production_in_thousand_mt, fill = crop)) +
  geom_boxplot() +
  labs(title = "Distribution of Production across States for Each Crop", x = "Crop", y = "Production (in kg/ha)", fill = "Crop") +
  theme(plot.title = element_text(hjust = 0.5, size = 12 , face = "bold"), 
        axis.text = element_text(size = 8),
        axis.text.x = element_text(size = 8, angle = 0),
        axis.title = element_text(size = 8, face = "bold"))

#Plot 5 
#Scatter plot of the total Distribution of production across all states for each crop by year
# Subset the data for the two years
my_crops_data_2016 <- filter(my_crops_data, year == 2016)
my_crops_data_2017 <- filter(my_crops_data, year == 2017)

# Calculate total production for each crop and year
total_prod_2016 <- my_crops_data_2016 %>% group_by(crop) %>% summarise(total_production = sum(production_in_thousand_mt))
total_prod_2017 <- my_crops_data_2017 %>% group_by(crop) %>% summarise(total_production = sum(production_in_thousand_mt))

# Combine the two data frames
total_prod <- bind_rows(total_prod_2016, total_prod_2017) %>% 
  mutate(year = rep(c("2016", "2017"), each = n_distinct(total_prod_2016$crop)))

# Create the scatter plot
ggplot(total_prod, aes(x = crop, y = total_production, color = year)) +
  geom_point(size = 3, position = position_dodge(width = 0.7)) +
  labs(title = "Total Production of Each Crop in India (2016-2017)", x = "Crop", y = "Total Production (in thousand MT)", color = "Year") +
  theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
        axis.text = element_text(size = 8),
        axis.text.x = element_text(size = 8, angle = 0),
        axis.title = element_text(size = 8, face = "bold")) 
