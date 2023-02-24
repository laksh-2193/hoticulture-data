# Importing library
library(dplyr)
library(ggplot2)
library(janitor)
library(reshape2)

list.files()

#Reading CSV file
cropdata <- read.csv("DataVisualisation_7.5 - AllCrops_7.5.csv")

#Creating table for my data
my_cropdata <- subset(cropdata, Name == "Deepak")

#Cleaning data
my_my_cropdata <- clean_names(my_cropdata)

#Numerical summaries
summary(my_cropdata)
head(my_cropdata)
tail(my_cropdata)
nrow(my_cropdata)
ncol(my_cropdata)
summary(select_if(my_cropdata, is.numeric))

#Creating table for different crops
Almond_data <- subset(my_cropdata, crop == "Almond")
Almond_data$id <- 1:nrow(Almond_data)

Aonka_data <- subset(my_cropdata, crop == "Aonla")
Aonka_data$id <- 1:nrow(Aonka_data)

Apple_data <- subset(my_cropdata, crop == "Apple")
Apple_data$id <- 1:nrow(Apple_data)

Banana_data <- subset(my_cropdata, crop == "Banana")
Banana_data$id <- 1:nrow(Banana_data)

#-------------------------------------------------------------------------------------------------graph 1
# Create a barplot to compare the area_in_thousand_ha for crops
ggplot(my_cropdata, aes(x=crop, y=area_in_thousand_ha, fill=crop)) + 
  geom_bar(stat="identity", position="dodge") +
  labs(x="Crop", y="Area in thousand ha", title="Comparison of all crops by area") +
  theme(plot.title = element_text(hjust = 0.5))

# Create a barplot to compare the production_in_thousand_mt for crops
ggplot(my_cropdata, aes(x=crop, y=production_in_thousand_mt, fill=crop)) + 
  geom_bar(stat="identity", position="dodge") +
  labs(x="Crop", y="Production in thousand MT", title="Comparison of all crops by production") +
  theme(plot.title = element_text(hjust = 0.5))


#------------------------------------------------------------------------------------------------graph 2

# Subset the data for apple and banana crops
apple_banana_data <- my_cropdata[my_cropdata$crop %in% c("Apple", "Banana"),]

# Create the scatterplot
ggplot(data = apple_banana_data, aes(x = area_in_thousand_ha, y = production_in_thousand_mt, color = crop)) +
  geom_point() +
  labs(title = "Production vs. Area for Apple and Banana Crops",
       x = "Area (in thousand hectares)",
       y = "Production (in thousand metric tonnes)",
       color = "Crop") +
  theme_bw()

#----------------------------------------------------------------------------------------------graph 3

ggplot(my_cropdata, aes(x=year, y=state, fill=production_in_thousand_mt)) +
  geom_tile(color="white") +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(title="Crop Production in India",
       subtitle="Production (in thousand metric tons) by Crop and State",
       x="Year",
       y="State") +
  theme(plot.title = element_text(size=16, face="bold"),
        plot.subtitle = element_text(size=12),
        axis.text.x = element_text(angle = 90, vjust = 0.5),
        panel.background = element_blank())



#--------------------------------------------------------------------------------------------graph 4 

ggplot(my_cropdata, aes(x=crop, y=state, fill=production_in_thousand_mt)) +
  geom_tile(color="white") +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(title="Crop Production in India",
       subtitle="Production (in thousand metric tons) by Crop and State",
       x="Crop",
       y="State") +
  theme(plot.title = element_text(size=16, face="bold"),
        plot.subtitle = element_text(size=12),
        axis.text.x = element_text(angle = 90, vjust = 0.5),
        panel.background = element_blank())
#-------------------------------------------------------------------------------------------graph 5

ggplot(my_cropdata, aes(x=reorder(state, production_in_thousand_mt), y=production_in_thousand_mt, fill=crop)) +
  geom_bar(stat="identity", position=position_dodge()) +
  scale_fill_brewer(palette="Dark2") +
  ggtitle("Top Producing Districts by Crop") +
  ylab("Production (Thousand Metric Tons)") +
  xlab("Districts") +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


#-----------------------------------------------------------------------------------------graph 6

ggplot(my_cropdata, aes(x = state, y = production_in_thousand_mt, fill = crop)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme(legend.position = "top") +
  labs(x = "District", y = "Production (in thousand MT)", title = "Crop Production by District") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_smooth(method = "lm", se = FALSE, aes(group = crop, color = crop), formula = y ~ x)
