getwd()
library(dplyr)
library(ggplot2)
library(leaflet)
library(zoo)

Cities <- dir("Cities") # Get all cities names

for (i in (1:length(Cities))) {
  periods <- dir(paste0("Cities/",Cities[i]))
  for (j in (1:length(periods))) {  
  
    temp <- read.csv(paste0("Cities/",Cities[i],"/",periods[j]))
    temp$city <- Cities[i]
    temp$period <- gsub("\\..*","",periods[j])
    
    if (exists("data2")) {
      data2 <- rbind(data2,temp)
    } else {
      data2 = temp
    }
  }
}


# A summary of variables
summary(data2)


Barcelona <- subset(data2, city == "Barcelona")
upper <- mean(Barcelona$price) + 3 * sd(Barcelona$price) # any kid taller than this would be an outlier
lower <- mean(Barcelona$price) - 3 * sd(Barcelona$price) # any kid shorter than this would be an outlier
# Filter the outliers
Barcelona<- Barcelona[Barcelona$price < upper & Barcelona$price > lower, ]


Amsterdam <- subset(data2, city == "Amsterdam")
upper <- mean(Amsterdam$price) + 3 * sd(Amsterdam$price) # any kid taller than this would be an outlier
lower <- mean(Amsterdam$price) - 3 * sd(Amsterdam$price) # any kid shorter than this would be an outlier
# Filter the outliers
Amsterdam<- Amsterdam[Amsterdam$price < upper & Amsterdam$price > lower, ]


New_York <- subset(data2, city == "New York")
upper <- mean(New_York$price) + 3 * sd(New_York$price) # any kid taller than this would be an outlier
lower <- mean(New_York$price) - 3 * sd(New_York$price) # any kid shorter than this would be an outlier
# Filter the outliers
New_York<- New_York[New_York$price < upper & New_York$price > lower, ]


Rome <- subset(data2, city == "Rome")
upper <- mean(Rome$price) + 3 * sd(Rome$price) # any kid taller than this would be an outlier
lower <- mean(Rome$price) - 3 * sd(Rome$price) # any kid shorter than this would be an outlier
# Filter the outliers
Rome<- Rome[Rome$price < upper & Rome$price > lower, ]


Paris <- subset(data2, city == "Paris")
upper <- mean(Paris$price) + 3 * sd(Paris$price) # any kid taller than this would be an outlier
lower <- mean(Paris$price) - 3 * sd(Paris$price) # any kid shorter than this would be an outlier
# Filter the outliers
Paris<- Paris[Paris$price < upper & Paris$price > lower, ]






# Average price, median price and count over time by room type

data3 <- data2 %>% 
  group_by(.dots=c("city","period")) %>% 
  summarise(AvePrice = mean(price, na.rm = TRUE),
            MedianPrice = median(price, na.rm = TRUE),
            MinPrice = min(price, na.rm = TRUE),
            Count = n())

data3$period <- gsub("_", "-", data3$period)
data3$period<- as.Date(as.yearmon(data3$period, "%Y-%m"))

str(data3)


p<-ggplot(data3, aes(x=period, y=AvePrice, group=city)) +
  geom_line(aes(color=city), size=1.5)+
  scale_x_date(date_breaks = "3 month", date_labels = "%Y %m")+
  ggtitle("Average Price by city") + 
  ylab("Average price $") + 
  xlab("Year - Month")
p

p<-ggplot(data3, aes(x=period, y=MedianPrice, group=city)) +
  geom_line(aes(color=city), size=1.5)+
  scale_x_date(date_breaks = "3 month", date_labels = "%Y %m")+
  ggtitle("Median Price by city") + 
  ylab("Median price $") + 
  xlab("Year - Month")
p

p<-ggplot(data3, aes(x=period, y=Count, group=city)) +
  geom_line(aes(color=city), size=1.5)+
  scale_x_date(date_breaks = "3 month", date_labels = "%Y %m")+
  ggtitle("Number of houses by city") + 
  ylab("Number of houses") + 
  xlab("Year - Month")
p






# Average availability, median availability

data3 <- data2 %>% 
  group_by(.dots=c("city","period")) %>% 
  summarise(AveAvailability = mean(availability_365, na.rm = TRUE),
            MedianAvailability = median(availability_365, na.rm = TRUE),
            MinAvailability = min(availability_365, na.rm = TRUE),
            Count = n())

data3$period <- gsub("_", "-", data3$period)
data3$period<- as.Date(as.yearmon(data3$period, "%Y-%m"))

str(data3)


p<-ggplot(data3, aes(x=period, y=AveAvailability, group=city)) +
  geom_line(aes(color=city), size=1.5)+
  scale_x_date(date_breaks = "3 month", date_labels = "%Y %m")+
  ggtitle("Average Availability by city") + 
  ylab("Average Availability (days)") + 
  xlab("Year - Month")
p

p<-ggplot(data3, aes(x=period, y=MedianAvailability, group=city)) +
  geom_line(aes(color=city), size=1.5)+
  scale_x_date(date_breaks = "3 month", date_labels = "%Y %m")+
  ggtitle("Median Availability by city") + 
  ylab("Median Availability (days)") + 
  xlab("Year - Month")
p














# Average price, median price and count over time by room time

data3 <- data2 %>% 
  group_by(.dots=c("room_type","period")) %>% 
  summarise(AvePrice = mean(price, na.rm = TRUE),
            MedianPrice = median(price, na.rm = TRUE),
            MinPrice = min(price, na.rm = TRUE),
            Count = n())

data3$period <- gsub("_", "-", data3$period)
data3$period<- as.Date(as.yearmon(data3$period, "%Y-%m"))

str(data3)


p<-ggplot(data3, aes(x=period, y=AvePrice, group=room_type)) +
  geom_line(aes(color=room_type), size=1.5)+
  scale_x_date(date_breaks = "3 month", date_labels = "%Y %m")+
  ggtitle("Average Price by room type") + 
  ylab("Average price $") + 
  xlab("Year - Month")
p

p<-ggplot(data3, aes(x=period, y=MedianPrice, group=room_type)) +
  geom_line(aes(color=room_type), size=1.5)+
  scale_x_date(date_breaks = "3 month", date_labels = "%Y %m")+
  ggtitle("Median Price by room type") + 
  ylab("Median price $") + 
  xlab("Year - Month")
p

library(scales)
p<-ggplot(data3, aes(x=period, y=Count, group=room_type)) +
  geom_line(aes(color=room_type), size=1.5)+
  scale_x_date(date_breaks = "3 month", date_labels = "%Y %m")+
  ggtitle("Number of houses by room type") + 
  ylab("Number of houses") + 
  xlab("Year - Month")+
  scale_y_continuous(labels=comma)+
  scale_y_continuous(breaks = round(seq(0, max(data3$Count), by = 20000),1))
p




# Average availability, median availability by room type

data3 <- data2 %>% 
  group_by(.dots=c("room_type","period")) %>% 
  summarise(AveAvailability = mean(availability_365, na.rm = TRUE),
            MedianAvailability = median(availability_365, na.rm = TRUE),
            MinAvailability = min(availability_365, na.rm = TRUE),
            Count = n())

data3$period <- gsub("_", "-", data3$period)
data3$period<- as.Date(as.yearmon(data3$period, "%Y-%m"))

str(data3)


p<-ggplot(data3, aes(x=period, y=AveAvailability, group=room_type)) +
  geom_line(aes(color=room_type), size=1.5)+
  scale_x_date(date_breaks = "3 month", date_labels = "%Y %m")+
  ggtitle("Average Availability by room type") + 
  ylab("Average Availability (days)") + 
  xlab("Year - Month")
p

p<-ggplot(data3, aes(x=period, y=MedianAvailability, group=room_type)) +
  geom_line(aes(color=room_type), size=1.5)+
  scale_x_date(date_breaks = "3 month", date_labels = "%Y %m")+
  ggtitle("Median Availability by room type") + 
  ylab("Median Availability (days)") + 
  xlab("Year - Month")
p




library(ggplot2) # Load the necessary library for fancy plots
library(gridExtra) # The library to put two panels with plots together

p1 <- ggplot(data = Barcelona, aes(price)) + # initialize a plot for data
  xlim(0, 1000) + # Set the limits of x axis to keep plots comparable
  ylim(0, 1000000) + # Set the limits of y axis to keep plots comparable
  ggtitle("Barcelona") + # Set the title to be able to identify plots
  geom_histogram(binwidth = 20) # Set the type of plot as a histogram

p2 <- ggplot(data = Amsterdam, aes(price)) + # initialize a plot for data
  xlim(0, 1000) + # Set the limits of x axis to keep plots comparable
  ylim(0, 1000000) + # Set the limits of y axis to keep plots comparable
  ggtitle("Amsterdam") + # Set the title to be able to identify plots
  geom_histogram(binwidth = 20) # Set the type of plot as a histogram

p3 <- ggplot(data = Rome, aes(price)) + # initialize a plot for data
  xlim(0, 1000) + # Set the limits of x axis to keep plots comparable
  ylim(0, 1000000) + # Set the limits of y axis to keep plots comparable
  ggtitle("Rome") + # Set the title to be able to identify plots
  geom_histogram(binwidth = 20) # Set the type of plot as a histogram

p4 <- ggplot(data = New_York, aes(price)) + # initialize a plot for data
  xlim(0, 1000) + # Set the limits of x axis to keep plots comparable
  ylim(0, 1000000) + # Set the limits of y axis to keep plots comparable
  ggtitle("New York") + # Set the title to be able to identify plots
  geom_histogram(binwidth = 20) # Set the type of plot as a histogram

p5 <- ggplot(data = Paris, aes(price)) + # initialize a plot for data
  xlim(0, 1000) + # Set the limits of x axis to keep plots comparable
  ylim(0, 2000000) + # Set the limits of y axis to keep plots comparable
  ggtitle("Paris") + # Set the title to be able to identify plots
  geom_histogram(binwidth = 20) # Set the type of plot as a histogram

grid.arrange(p1, p2, p3, p4, p5)







library(ggplot2) # Load the necessary library for fancy plots
library(gridExtra) # The library to put two panels with plots together

p1 <- ggplot(data = Barcelona, aes(availability_365)) + # initialize a plot for data
  xlim(0, 365) + # Set the limits of x axis to keep plots comparable
  ylim(0, 150000) + # Set the limits of y axis to keep plots comparable
  ggtitle("Barcelona") + # Set the title to be able to identify plots
  geom_histogram(binwidth = 5) # Set the type of plot as a histogram

p2 <- ggplot(data = Amsterdam, aes(availability_365)) + # initialize a plot for data
  xlim(0, 365) + # Set the limits of x axis to keep plots comparable
  ylim(0, 150000) + # Set the limits of y axis to keep plots comparable
  ggtitle("Amsterdam") + # Set the title to be able to identify plots
  geom_histogram(binwidth = 5) # Set the type of plot as a histogram

p3 <- ggplot(data = Rome, aes(availability_365)) + # initialize a plot for data
  xlim(0, 365) + # Set the limits of x axis to keep plots comparable
  ylim(0, 150000) + # Set the limits of y axis to keep plots comparable
  ggtitle("Rome") + # Set the title to be able to identify plots
  geom_histogram(binwidth = 5) # Set the type of plot as a histogram

p4 <- ggplot(data = New_York, aes(availability_365)) + # initialize a plot for data
  xlim(0, 365) + # Set the limits of x axis to keep plots comparable
  ylim(0, 150000) + # Set the limits of y axis to keep plots comparable
  ggtitle("New York") + # Set the title to be able to identify plots
  geom_histogram(binwidth = 5) # Set the type of plot as a histogram

p5 <- ggplot(data = Paris, aes(availability_365)) + # initialize a plot for data
  xlim(0, 365) + # Set the limits of x axis to keep plots comparable
  ylim(0, 150000) + # Set the limits of y axis to keep plots comparable
  ggtitle("Paris") + # Set the title to be able to identify plots
  geom_histogram(binwidth = 5) # Set the type of plot as a histogram

grid.arrange(p1, p2, p3, p4, p5)














# Load RColorBrewer
library(RColorBrewer)

# Classic palette BuPu, with 4 colors
coul <- brewer.pal(8, "Set1") 

# Add more colors to this palette :
coul <- colorRampPalette(coul)(25)

# Plot it
pie(rep(1, length(coul)), col = coul , main="") 



# All rooms visualization for newest period 2020-12


NewYorkNewest <- subset(New_York, period == "2020_12")
leaflet(data = NewYorkNewest) %>% addProviderTiles(providers$CartoDB.DarkMatterNoLabels) %>%  addCircleMarkers(~longitude, ~latitude, weight = 1, radius=1, fillOpacity = 0.1, opacity = 0.1)
colors <- colorFactor(palette = 'Set1', NewYorkNewest$neighbourhood_group)
leaflet(data = NewYorkNewest) %>%
  addProviderTiles(providers$CartoDB.DarkMatterNoLabels) %>%
  addCircleMarkers(~longitude, ~latitude, color = ~colors(neighbourhood_group), weight = 1, radius=1, fillOpacity = 0.1, opacity = 0.1,label = paste("Name:", NewYorkNewest$name)) %>% 
  addLegend("bottomright", pal=colors, values = ~neighbourhood_group,
            title = "Neighbourhood groups",
            opacity = 1
  )

str(NewYorkNewest)


AmsterdamNewest <- subset(Amsterdam, period == "2020_12")
AmsterdamNewest$neighbourhood <- as.character(AmsterdamNewest$neighbourhood)
leaflet(data = AmsterdamNewest) %>% addProviderTiles(providers$CartoDB.DarkMatterNoLabels) %>%  addCircleMarkers(~longitude, ~latitude, weight = 1, radius=1, fillOpacity = 0.1, opacity = 0.1)

colors <- colorFactor(palette = coul, AmsterdamNewest$neighbourhood)
leaflet(data = AmsterdamNewest) %>%
  addProviderTiles(providers$CartoDB.DarkMatterNoLabels) %>%
  addCircleMarkers(~longitude, ~latitude, color = ~colors(neighbourhood), weight = 1, radius=1, fillOpacity = 0.1, opacity = 0.1,label = paste("Name:", NewYorkNewest$name)) %>% 
  addLegend("bottomright", pal=colors, values = ~neighbourhood,
            title = "Neighbourhood",
            opacity = 1
  )





BarcelonaNewest <- subset(Barcelona, period == "2020_12")
BarcelonaNewest$neighbourhood <- as.character(BarcelonaNewest$neighbourhood)
leaflet(data = BarcelonaNewest) %>% addProviderTiles(providers$CartoDB.DarkMatterNoLabels) %>%  addCircleMarkers(~longitude, ~latitude, weight = 1, radius=1, fillOpacity = 0.1, opacity = 0.1)
colors <- colorFactor(palette = coul, BarcelonaNewest$neighbourhood_group)
leaflet(data = BarcelonaNewest) %>%
  addProviderTiles(providers$CartoDB.DarkMatterNoLabels) %>%
  addCircleMarkers(~longitude, ~latitude, color = ~colors(neighbourhood_group), weight = 1, radius=1, fillOpacity = 0.1, opacity = 0.1,label = paste("Name:", NewYorkNewest$name)) %>% 
  addLegend("bottomright", pal=colors, values = ~neighbourhood_group,
            title = "Neighbourhood groups",
            opacity = 1
  )



library(pals)



RomeNewest <- subset(Rome, period == "2020_12")
RomeNewest$neighbourhood <- as.character(RomeNewest$neighbourhood)
leaflet(data = RomeNewest) %>% addProviderTiles(providers$CartoDB.DarkMatterNoLabels) %>%  addCircleMarkers(~longitude, ~latitude, weight = 1, radius=1, fillOpacity = 0.1, opacity = 0.1)
colors <- colorFactor(palette = coul, RomeNewest$neighbourhood)
leaflet(data = RomeNewest) %>%
  addProviderTiles(providers$CartoDB.DarkMatterNoLabels) %>%
  addCircleMarkers(~longitude, ~latitude, color = ~colors(neighbourhood), weight = 1, radius=1, fillOpacity = 0.1, opacity = 0.1,label = paste("Name:", RomeNewest$name)) %>% 
  addLegend("bottomright", pal=colors, values = ~neighbourhood,
            title = "Neighbourhood groups",
            opacity = 1
  )








ParisNewest <- subset(Paris, period == "2020_12")
ParisNewest$neighbourhood <- as.character(ParisNewest$neighbourhood)
leaflet(data = ParisNewest) %>% addProviderTiles(providers$CartoDB.DarkMatterNoLabels) %>%  addCircleMarkers(~longitude, ~latitude, weight = 1, radius=1, fillOpacity = 0.1, opacity = 0.1)
colors <- colorFactor(palette = coul, ParisNewest$neighbourhood)
leaflet(data = ParisNewest) %>%
  addProviderTiles(providers$CartoDB.DarkMatterNoLabels) %>%
  addCircleMarkers(~longitude, ~latitude, color = ~colors(neighbourhood), weight = 1, radius=1, fillOpacity = 0.1, opacity = 0.1,label = paste("Name:", RomeNewest$name)) %>% 
  addLegend("bottomright", pal=colors, values = ~neighbourhood,
            title = "Neighbourhood groups",
            opacity = 1
  )



























# Average price plot and number of houses

data3 <- AmsterdamNewest %>% 
  group_by(.dots=c("neighbourhood")) %>% 
  summarise(AvePrice = mean(price, na.rm = TRUE),
            MedianPrice = median(price, na.rm = TRUE),
            Count = n())


# Let's read the jeoJson file that is stored on the web with the geojsonio library:
library(geojsonio)
spdf <- geojson_read("22Areas.json",  what = "sp")


poly_state <- spdf

data3$neighbourhood <- as.character(data3$neighbourhood)

data3$neighbourhood[data3$neighbourhood == "Bijlmer-Centrum"] <- "Bijlmer-Centrum, Amstel III"
data3$neighbourhood[data3$neighbourhood == "Bijlmer-Oost"] <- "Bijlmer-Oost"
data3$neighbourhood[data3$neighbourhood == "Bos en Lommer"] <- "Bos en Lommer"
data3$neighbourhood[data3$neighbourhood == "Buitenveldert - Zuidas"] <- "Buitenveldert, Zuidas"
data3$neighbourhood[data3$neighbourhood == "Centrum-Oost"] <- "Centrum-Oost"
data3$neighbourhood[data3$neighbourhood == "Centrum-West"] <- "Centrum-West" 
data3$neighbourhood[data3$neighbourhood == "De Aker - Nieuw Sloten"] <- "De Aker, Sloten, Nieuw-Sloten"
data3$neighbourhood[data3$neighbourhood == "De Baarsjes - Oud-West"] <- "Oud West, De Baarsjes"
data3$neighbourhood[data3$neighbourhood == "De Pijp - Rivierenbuurt"] <- "De Pijp, Rivierenbuurt"
data3$neighbourhood[data3$neighbourhood == "Gaasperdam - Driemond"] <- "Gaasperdam, Driemond" 
data3$neighbourhood[data3$neighbourhood == "Geuzenveld - Slotermeer"] <- "Geuzenveld, Slotermeer, Sloterdijken" 
data3$neighbourhood[data3$neighbourhood == "IJburg - Zeeburgereiland"] <- "IJburg, Zeeburgereiland"
data3$neighbourhood[data3$neighbourhood == "Noord-Oost"] <- "Noord-Oost"
data3$neighbourhood[data3$neighbourhood == "Noord-West"] <- "Noord-West"
data3$neighbourhood[data3$neighbourhood == "Oostelijk Havengebied - Indische Buurt"] <- "Indische Buurt, Oostelijk Havengebied"
data3$neighbourhood[data3$neighbourhood == "Osdorp"] <- "Osdorp"
data3$neighbourhood[data3$neighbourhood == "Oud-Noord"] <- "Oud-Noord"
data3$neighbourhood[data3$neighbourhood == "Oud-Oost"] <- "Oud-Oost"
data3$neighbourhood[data3$neighbourhood == "Slotervaart"] <- "Slotervaart" 
data3$neighbourhood[data3$neighbourhood == "Watergraafsmeer"] <- "Watergraafsmeer"
data3$neighbourhood[data3$neighbourhood == "Westerpark"] <- "Westerpark"
data3$neighbourhood[data3$neighbourhood == "Zuid"] <- "Oud-Zuid"

data4 <- data3[match(poly_state$Gebied, data3$neighbourhood),]

poly_state$price <- data4$AvePrice
poly_state$neigh <- data4$neighbourhood
poly_state$number <- data4$Count


poly_state@data <- poly_state@data %>%
  mutate(content = paste0(neigh,': ',round(price)," $"))


library(raster)
library(rgeos)
library(leaflet)

centers <- data.frame(gCentroid(poly_state, byid = TRUE))
centers$region <- poly_state$Gebied



poly_state %>%
  leaflet() %>%
  addTiles() %>%
  addPolygons(data = poly_state,
              weight=1, opacity = 1.0,color = 'white',
              fillOpacity = 0.9, smoothFactor = 0.5,
              fillColor = ~colorBin('RdBu',price, reverse = TRUE)(price),
              label = ~content) %>%
  
  
  addLabelOnlyMarkers(data = centers,
                      lng = ~x, lat = ~y, label = ~region,
                      labelOptions = labelOptions(noHide = TRUE, direction = 'top', textOnly = TRUE)) %>%

  addLegend(
    "topright",
    pal = colorBin('RdBu', poly_state@data$price,reverse = TRUE),
    values = poly_state@data$price, 
    opacity = 0.9
  )


poly_state@data <- poly_state@data %>%
  mutate(content = paste0(neigh,': ',round(count)," places"))

poly_state %>%
  leaflet() %>%
  addTiles() %>%
  addPolygons(data = poly_state,
              weight=1, opacity = 1.0,color = 'white',
              fillOpacity = 0.9, smoothFactor = 0.5,
              fillColor = ~colorBin('RdBu',number, reverse = TRUE)(number),
              label = ~content) %>%
  
  addLabelOnlyMarkers(data = centers,
                      lng = ~x, lat = ~y, label = ~region,
                      labelOptions = labelOptions(noHide = TRUE, direction = 'top', textOnly = TRUE)) %>%
  
  addLegend(
    "topright",
    pal = colorBin('RdBu', poly_state@data$number,reverse = TRUE),
    values = poly_state@data$number, 
    opacity = 0.9
  )











# Amsterdam information

summary(AmsterdamNewest)


naniar::gg_miss_var(AmsterdamNewest) +
  theme_minimal()+
  labs(y = "Look at all the Missing Values") 



df <- rbind(AmsterdamNewest,NewYorkNewest,RomeNewest,ParisNewest,BarcelonaNewest)

property_df <-  df %>% 
  group_by(city, room_type) %>% 
  summarize(Freq = n())

property_df 


# propertydf <- propertydf %>% 
#   filter(property_type %in% c("Apartment","House","Condominium","Townhouse", "Loft"))

total_property <-  df %>% 
  filter(room_type %in% c("Private room","Entire home/apt","Entire home/apt")) %>% 
  group_by(city) %>% 
  summarize(sum = n())



property_ratio <- merge (property_df, total_property, by="city")

property_ratio <- property_ratio %>% 
  mutate(ratio = Freq/sum)

ggplot(property_ratio, aes(x=city, y = ratio, fill = room_type)) +
  geom_bar(position = "dodge", stat="identity") + 
  xlab("Borough") + ylab ("Count") +
  scale_fill_discrete(name = "Property Type") + 
  scale_y_continuous(labels = scales::percent) +
  ggtitle("Which types of Listings are there in Cities?",
          subtitle = "Map showing Count of Room Type  ") +
  theme(plot.title = element_text(face = "bold", size = 14) ) +
  theme(plot.subtitle = element_text(face = "bold", color = "grey35", hjust = 0.5)) +
  theme(plot.caption = element_text(color = "grey68"))+scale_color_gradient(low="#d3cbcb", high="#852eaa")+
  scale_fill_manual("Property Type", values=c("#e06f69","#357b8a", "#7db5b8", "#59c6f3", "#f6c458")) +
  xlab("City") + ylab("Percentage") + theme(axis.text.x = element_text(angle = 45, hjust = 1))





property_df <-  AmsterdamNewest %>% 
  group_by(neighbourhood, room_type) %>% 
  summarize(Freq = n())

# propertydf <- propertydf %>% 
#   filter(property_type %in% c("Apartment","House","Condominium","Townhouse", "Loft"))

total_property <-  AmsterdamNewest %>% 
  filter(room_type %in% c("Private room","Entire home/apt","Entire home/apt")) %>% 
  group_by(neighbourhood) %>% 
  summarize(sum = n())

property_ratio <- merge (property_df, total_property, by="neighbourhood")

property_ratio <- property_ratio %>% 
  mutate(ratio = Freq/sum)

ggplot(property_ratio, aes(x=neighbourhood, y = ratio, fill = room_type)) +
  geom_bar(position = "dodge", stat="identity") + 
  xlab("Borough") + ylab ("Count") +
  scale_fill_discrete(name = "Property Type") + 
  scale_y_continuous(labels = scales::percent) +
  ggtitle("Which types of Listings are there in Amsterdam?",
          subtitle = "Map showing Count of Room Type  ") +
  theme(plot.title = element_text(face = "bold", size = 14) ) +
  theme(plot.subtitle = element_text(face = "bold", color = "grey35", hjust = 0.5)) +
  theme(plot.caption = element_text(color = "grey68"))+scale_color_gradient(low="#d3cbcb", high="#852eaa")+
  scale_fill_manual("Property Type", values=c("#e06f69","#357b8a", "#7db5b8", "#59c6f3", "#f6c458")) +
  xlab("Neighborhood") + ylab("Percentage") + theme(axis.text.x = element_text(angle = 45, hjust = 1))





AmsterdamNewest %>% 
  filter(!(is.na(neighbourhood))) %>% 
  filter(!(neighbourhood == "Unknown")) %>% 
  group_by(neighbourhood) %>% 
  summarise(mean_price = mean(price, na.rm = TRUE)) %>% 
  ggplot(aes(x = reorder(neighbourhood, mean_price), y = mean_price, fill = neighbourhood)) +
  geom_col(stat ="identity", color = "black", fill="#357b8a") +
  coord_flip() +
  theme_gray() +
  labs(x = "Neighbourhood Group", y = "Price") +
  geom_text(aes(label = round(mean_price,digit = 2)), hjust = 2.0, color = "white", size = 3.5) +
  ggtitle("Mean Price comparison for each Neighbourhood Group", subtitle = "Price vs Neighbourhood Group") + 
  xlab("Neighbourhood Group") + 
  ylab("Mean Price") +
  theme(legend.position = "none",
        plot.title = element_text(color = "black", size = 14, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(color = "darkblue", hjust = 0.5),
        axis.title.y = element_text(),
        axis.title.x = element_text(),
        axis.ticks = element_blank())



AmsterdamNewest %>% 
  filter(!(is.na(room_type))) %>% 
  filter(!(room_type == "Unknown")) %>% 
  group_by(room_type) %>% 
  summarise(mean_price = mean(price, na.rm = TRUE)) %>% 
  ggplot(aes(x = reorder(room_type, mean_price), y = mean_price, fill = room_type)) +
  geom_col(stat ="identity", color = "black", fill="#357b8a") +
  coord_flip() +
  theme_gray() +
  labs(x = "Room Type", y = "Price") +
  geom_text(aes(label = round(mean_price,digit = 2)), hjust = 2.0, color = "white", size = 3.5) +
  ggtitle("Mean Price comparison with all Room Types", subtitle = "Price vs Room Type") + 
  xlab("Room Type") + 
  ylab("Mean Price") +
  theme(legend.position = "none",
        plot.title = element_text(color = "black", size = 14, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(color = "darkblue", hjust = 0.5),
        axis.title.y = element_text(),
        axis.title.x = element_text(),
        axis.ticks = element_blank())

library(tidyr)

AmsterdamNewest_filtered_data <- AmsterdamNewest %>% 
  filter(price < quantile(AmsterdamNewest$price, 0.9) & price > quantile(AmsterdamNewest$price, 0.1))


train <- AmsterdamNewest_filtered_data %>% sample_frac(.7) %>% filter(price > 0)

test  <- anti_join(AmsterdamNewest_filtered_data, train, by = 'id') %>% filter(price > 0)





Model1<- lm (price ~ neighbourhood + latitude + longitude + room_type + minimum_nights  + number_of_reviews + reviews_per_month + calculated_host_listings_count +
                        availability_365, data = train)

summary_Model1 <- summary(Model1)
mse_1 <- summary_Model1$sigma^2
r_sq_1 <- summary_Model1$r.squared
adj_r_sq_1 <- summary_Model1$adj.r.squared

summary_Model1

par(mfrow=c(2,2)) 
plot(Model1)





null <- lm(price~1, data = train)
full <- lm(price ~ neighbourhood + latitude + longitude + room_type + minimum_nights  + number_of_reviews + 
             calculated_host_listings_count + availability_365, data = train)
step(null, scope =list(lower=null, upper= full), direction = "both")
Model2 <- lm(formula = price ~ room_type + neighbourhood + availability_365 + 
                       calculated_host_listings_count + number_of_reviews + longitude + 
                       minimum_nights, data = train, nbest = 2, nvmax = 9)
summary_Model2 <- summary(Model2)
mse_2 <- summary_Model2$sigma^2
r_sq_2 <-summary_Model2$r.squared
adj_r_sq_2 <- summary_Model2$adj.r.squared
summary_Model2





null <- lm(price~1, data = train)
full <- lm(price ~ neighbourhood + latitude + longitude + room_type + minimum_nights  + number_of_reviews + 
             calculated_host_listings_count + availability_365, data = train)
n=dim(train[1])
step(null, scope =list(lower=null, upper= full), direction = "both")
Model3 <- lm(formula = price ~ room_type + neighbourhood + availability_365 + 
               calculated_host_listings_count + number_of_reviews + longitude + 
               minimum_nights, data = train, nbest = 2, nvmax = 9)
summary_Model3 <- summary(Model3)
mse_3 <- summary_Model3$sigma^2
r_sq_3 <- summary_Model3$r.squared
adj_r_sq_3 <- summary_Model3$adj.r.squared
summary_Model3


pred <- predict(Model3, newdata = test)
rmse(pred, test$price)
mape(pred, test$price)







#pi is a vector that contains predicted values for test set.
pi <- predict(object = Model4, newdata = test)

mean((pi - test$price)^2)

mean(abs(pi - test$price))










# Ordinary Least Squares Regression 


qplot(x = reviews_per_month, y = price, data = data2)



qplot(x = disp, y = mpg, data = mtcars) +
  geom_smooth(method = "lm", se = FALSE) + # Fit regression line
  # add guide lines
  geom_hline(yintercept = 15, lty = 2) + geom_vline(xintercept = 350, lty = 2)




mtcars_lm_0 <- lm(mpg ~ disp, data = mtcars)
summary(mtcars_lm_0)


anova(mtcars_lm_0, mtcars_lm_1)

shapiro.test(mtcars_lm_0$residuals)

# R's basic plotting functionality is really easy to use
# Remember the scale function from Module 2. We standardize residuals
qqnorm(scale(mtcars_lm_0$residuals))
qqline(scale(mtcars_lm_0$residuals)) # Adds the line


# This is easy to remember
qplot(x = mtcars_lm_0$fitted.values, y = mtcars_lm_0$residuals)




# Regression tree
library(rpart)
library("rpart.plot")
housing_rt_0 <- rpart(price ~ neighbourhood + latitude + longitude + room_type + minimum_nights  + number_of_reviews + 
                        calculated_host_listings_count + availability_365, data = AmsterdamNewest, method  = "anova")
rpart.plot(housing_rt_0)

housing_rt_0$variable.importance


summary(housing_rt_0, "cp")


plotcp(housing_rt_0, upper = "splits")


# Prune the Tree To have 11 Splits
housing_rt_0pr <- prune(housing_rt_0, cp = 0.013)
rpart.plot(housing_rt_0pr)



# Let us create a new dataset to predict
newHousing <- data.frame(CRIM = c(1.05393, 4.1), DIS = c(4.4986, 1.2), TAX = c(307, 100))
predict(housing_rt_0pr, newHousing)









hyper_grid <- expand.grid(
  minsplit = seq(5, 20, 1),
  maxdepth = seq(8, 15, 1)
)

models <- list()

for (i in 1:nrow(hyper_grid)) {
  
  # get minsplit, maxdepth values at row i
  minsplit <- hyper_grid$minsplit[i]
  maxdepth <- hyper_grid$maxdepth[i]
  
  # train a model and store in the list
  models[[i]] <- rpart(
    formula = price ~ neighbourhood + latitude + longitude + room_type + minimum_nights  + number_of_reviews + 
      calculated_host_listings_count + availability_365, data = train,
    method  = "anova",
    control = list(minsplit = minsplit, maxdepth = maxdepth)
  )
}

# function to get optimal cp
get_cp <- function(x) {
  min    <- which.min(x$cptable[, "xerror"])
  cp <- x$cptable[min, "CP"] 
}

# function to get minimum error
get_min_error <- function(x) {
  min    <- which.min(x$cptable[, "xerror"])
  xerror <- x$cptable[min, "xerror"] 
}

hyper_grid %>%
  mutate(
    cp    = purrr::map_dbl(models, get_cp),
    error = purrr::map_dbl(models, get_min_error)
  ) %>%
  arrange(error) %>%
  top_n(-5, wt = error)

optimal_tree <- rpart(
  formula = price ~ neighbourhood + latitude + longitude + room_type + minimum_nights  + number_of_reviews + 
    calculated_host_listings_count + availability_365,
  data    = train,
  method  = "anova",
  control = list(minsplit = 16, maxdepth = 14, cp = 0.01)
)

rpart.plot(optimal_tree)

library(Metrics)

pred <- predict(optimal_tree, newdata = test)
rmse(pred, test$price)
mape(pred, test$price)
















# Let's read the jeoJson file that is stored on the web with the geojsonio library:
library(geojsonio)
spdf <- geojson_read("Paris.geojson",  what = "sp")


poly_state <- spdf

poly_state@data <- poly_state@data %>%
  mutate(WATER_LAND_RATIO = as.numeric(as.character(125)) / as.numeric(as.character(128)),
         content = paste0(neighborhood,': ',WATER_LAND_RATIO))
poly_state %>%
  leaflet() %>%
  addTiles() %>%
  addPolygons(data = poly_state,
              weight=1, opacity = 1.0,color = 'white',
              fillOpacity = 0.9, smoothFactor = 0.5,
              fillColor = ~colorBin('RdBu',WATER_LAND_RATIO)(WATER_LAND_RATIO),
              label = ~content) %>%
  addLegend(
    "topright",
    pal = colorBin('RdBu', poly_state@data$WATER_LAND_RATIO),
    values = poly_state@data$WATER_LAND_RATIO, 
    opacity = 0.9
  )








NewYorkNewest <- subset(New_York, period == "2020_12")

pal <- colorFactor(palette = c("red", "green", "blue", "purple", "yellow"), domain = NewYorkNewest$neighbourhood_group)

leaflet(data = NewYorkNewest) %>%
  addProviderTiles(providers$CartoDB.DarkMatterNoLabels) %>%
  addCircleMarkers(~longitude, ~latitude, color = ~pal(neighbourhood_group), weight = 1, radius=1, fillOpacity = 0.1, opacity = 0.1,
                   
                   label = paste("Name:", NewYorkNewest$name)) %>% 
  addLegend("bottomright", pal = pal, values = ~neighbourhood_group,
            title = "Neighbourhood groups",
            opacity = 1
  )


new <- new[complete.cases(new$map_info),]


data3 <- new %>% 
  group_by(.dots=c("map_info")) %>% 
  summarise(AvePrice = mean(price, na.rm = TRUE),
            MedianPrice = median(price, na.rm = TRUE),
            Count = n())



data4 <- data3[match(poly_state$neighborhood, data3$map_info),]


data4$AvePrice[data4$Count < 2] <- NA


poly_state$price <- data4$AvePrice
poly_state$neigh <- data4$map_info
poly_state$number <- data4$Count


poly_state@data <- poly_state@data %>%
  mutate(content = paste0(neigh,': ',round(price)," $"))

poly_state %>%
  leaflet() %>%
  addTiles() %>%
  addPolygons(data = poly_state,
              weight=1, opacity = 1.0,color = 'white',
              fillOpacity = 0.9, smoothFactor = 0.5,
              fillColor = ~colorBin('RdBu',price, reverse = TRUE)(price),
              label = ~content) %>%
  addLegend(
    "topright",
    pal = colorBin('RdBu', poly_state@data$price,reverse = TRUE),
    values = poly_state@data$price, 
    opacity = 0.9
  )







library(sp)
library(rgdal)

startg <- Sys.time()

#Reading each of the maps. dsn is the folder of the map and layer is the name of the .shp file inside.
countries_map<- poly_state

#This is a function to reverse geocoding based on coordinates
rev_geo<-function(lat,long){
  #First the coordinates are transformed to spatialpoints
  points<-SpatialPoints(matrix(c(long,
                                 lat),ncol=2,nrow=1))
  #Creating a projection of the coordinates on the map of countries
  proj4string(points) <- proj4string(countries_map)
  #To see where the name of the country is stored in the map object, you need to explore it in R and see the ´data¡ element. In this case, ´NAME¡ has the information that we want. The function over returns the name of the country given the coordinates projected in the countries_map
  country<-as.character(over(points, countries_map)$neighbor)
  return(country)
}
  

library(snow)
library(foreach)
library(doParallel)

cl <- makeCluster(detectCores() - 1) 
registerDoParallel(cl)

#Now for each row in the dataset I am going to return the reverse geocoding
# I am using parallel processing here to make the process faster
map_info<-foreach(i=1:nrow(NewYorkNewest), 
                  .packages = c("sp", "rgdal"), .combine=rbind) %dopar% {
                    rev_geo(as.numeric(NewYorkNewest[i,"latitude"]),
                            as.numeric(NewYorkNewest[i,"longitude"]))
                  }

stopCluster(cl)

endg <- Sys.time()

new<- cbind(NewYorkNewest, map_info)













# Wordcloud

library(wordcloud2)
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

wordcloud2(data=AmsterdamNewest, size=1.6, color='random-dark')

wordcloud(words = AmsterdamNewest$name, min.freq = 1,max.words=1000, random.order=FALSE, colors=brewer.pal(8, "Dark2"))


# Load the data as a corpus
docs <- Corpus(VectorSource(AmsterdamNewest$name))

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")

# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("blabla1", "blabla2")) 
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
# Text stemming
# docs <- tm_map(docs, stemDocument)

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
