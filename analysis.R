library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(viridis)

crops_data <- read_csv('data.csv')
colnames(crops_data) <- tolower(colnames(crops_data))
head(crops_data)
crops_data <- na.omit(crops_data)
head(crops_data)
text_cols <- c('name','table_name','state','districts','crop')
crops_data[,text_cols] <- apply(crops_data[,text_cols],2,str_to_lower)
head(crops_data)


north <- c("jammu and kashmir","himachal pradesh","haryana","punjab","rajasthan")
central <- c("uttarakhand","uttar pradesh","madhya pradesh","chhattisgarh")
east <- c("bihar","jharkhand","west bengal","orissa","sikkim")
northeast <- c("meghalaya","assam","arunachal pradesh","nagaland","manipur","tripura","mizoram","galand")
west <- c("gujrat","maharashtra","gujarat")
south <- c("karnataka","telangana","telanganana","andhra pradesh","kerala","tamil naidu")

crops_data$location <- ifelse(crops_data$state %in% north, "North",
                              ifelse(crops_data$state %in% central, "Central",
                                     ifelse(crops_data$state %in% east, "East",
                                            ifelse(crops_data$state %in% northeast, "Northeast",
                                                   ifelse(crops_data$state %in% west, "West",
                                                          ifelse(crops_data$state %in% south, "South", "Other"))))))

almonds <- filter(crops_data,crop=="almond")
almonds <- select(almonds,state,districts,area_in_thousand_ha,production_in_thousand_mt,year,location)
almonds_production <- almonds %>%
  group_by(state,year) %>%
  summarize(total_production = sum(production_in_thousand_mt))
ggplot(data=almonds_production,aes(x=state,y=total_production,fill=as.factor(year))) + geom_col() + theme(plot.background = element_rect(size = 30))+labs(y="Total Production (in 1000 mt)",x="Geographical Location",title="Almond Production")

fruits <- c("almond", "aonla", "apple", "banana", "citrus", "grapes", "guava", "mango","papaya", 
            "pineapple", "pomegranate", "sapota", "strawberry", "walnut", "muskmelon", "watermellon") 
vegetables <- c("beans", "bottleguard", "brinjal","cabbage","capsicum","carrot",
                "cauliflower", "cucumbur", "greenchilli", "okra", "onion","peas",
                "potato", "radish", "sweetpotato", "tapioca", "tomato")


crops_data$crop_type <- ifelse(crops_data$crop %in% fruits, "fruits",
                               ifelse(crops_data$crop %in% vegetables, "vegetables", "Other"))
head(crops_data)

crop_data <- select(crops_data, state, districts, area_in_thousand_ha, production_in_thousand_mt, year, location,crop_type)
crop_types <- crop_data %>%
  group_by(crop_type,state)
ggplot(data=crop_types,aes(x=location, fill=crop_type)) + geom_bar(position="dodge")


crops_data$production_per_area <- crops_data$production_in_thousand_mt/crops_data$area_in_thousand_ha
crops_data$production_per_area[!is.finite(crops_data$production_per_area)] <- NA
fertility <- crops_data %>% group_by(location) %>% summarize(mean_production_per_area = mean(production_per_area,na.rm = TRUE))
fertility



my_data <- subset(crops_data, name == "lakshay")
table(my_data$crop)

#Scatter Plot for crops
ggplot(my_data, aes(x=area_in_thousand_ha, y=production_in_thousand_mt, color=crop)) +
  geom_point() +
  labs(title="Crop Production in India", x="Area (thousand ha)", y="Production (thousand mt)") +
  theme(plot.title = element_text(hjust = 0.5, size = 12 , face = "bold"), 
        axis.text = element_text(size = 8),
        axis.text.x = element_text(size = 8, angle = 90),
        axis.title = element_text(size = 8, face = "bold"))

#Scatter plot of the Distribution of production across states for each crop
ggplot(my_data, aes(x = area_in_thousand_ha, y = production_in_thousand_mt, color = crop)) +
  geom_point() +
  facet_wrap(~ state) +
  labs(title = "Distribution of production across states for each crop", 
       x = "Area in thousand hectares", 
       y = "Production in thousand metric tons") +
  theme(plot.title = element_text(hjust = 0.5, size = 12 , face = "bold"), 
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 8, face = "bold"))

#Boxplot of the Distribution of production across states for each crop
ggplot(my_data, aes(x = crop, y = production_in_thousand_mt, fill = crop)) +
  geom_boxplot() +
  labs(title = "Distribution of Production across States for Each Crop", x = "Crop", y = "Production (in kg/ha)", fill = "Crop") +
  theme(plot.title = element_text(hjust = 0.5, size = 12 , face = "bold"), 
        axis.text = element_text(size = 8),
        axis.text.x = element_text(size = 8, angle = 0),
        axis.title = element_text(size = 8, face = "bold"))

#Crop Production
ggplot(my_data, aes(x = state, y = production_in_thousand_mt, fill = crop)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  labs(x = "State", y = "Production (in thousand mt)", fill = "Crop", 
       title = "Total Production of Each Crop in Each State") +
  theme(plot.title = element_text(hjust = 0.5, size = 12 , face = "bold"), 
        axis.text = element_text(size = 8),
        axis.text.x = element_text(size = 8, angle = 90),
        axis.title = element_text(size = 8, face = "bold"))


colnames(my_data)

my_data_subset <- my_data %>% select(c("crop", "area_in_thousand_ha", "production_in_thousand_mt", "year"))

# Group the data by crop and year, and summarize by taking the sum of area and production
my_data_summary <- my_data_subset %>% 
  group_by(crop, year) %>% 
  summarize(total_area = sum(area_in_thousand_ha), total_production = sum(production_in_thousand_mt))

# Create the plot using ggplot2
ggplot(my_data_summary, aes(x = total_area, y = total_production, color = as.factor(year))) +
  geom_point(size = 2.5) +
  facet_wrap(~ crop, ncol = 3) +
  scale_color_discrete(name = "Year") +
  labs(x = "Area (in thousand ha)", y = "Production (in thousand mt)", title = "Production vs Area by Year")
