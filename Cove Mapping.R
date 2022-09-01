setwd("C:/users/pnkid/Desktop")


library(zipcodeR)
library(tidyverse)
library(maps)
library(viridis)
library(ggthemes)
library(dplyr)
library(geosphere)
library(rgeos)
library(ggplot2)
library(showtext)
library(hrbrthemes)
library(scales)

font_add("proxima_bold", "Proxima Nova Bold.ttf")

help(package="maps")

raw_cove <- read_csv("cove_guests.csv")
raw_cove$age = as.numeric(difftime(Sys.Date(),
                                   as.Date(raw_cove$birthday, "%m/%d/%Y")
                                   , units = "weeks"))/52.25
raw_cove$gender <- as.factor(raw_cove$gender)
gender_cove <- raw_cove %>%
  filter(!is.na(gender)) %>%
  rename(Gender = gender)

ggplot(gender_cove, aes(x=age)) +
  geom_histogram(aes(y=..density..), fill="#0075c9")+
  scale_y_continuous(labels = scales::percent_format()) +
  theme(text=element_text(size=16), legend.position="none") + 
  ylab('Share of Attendance') + 
  xlab('Age') + 
  xlim(18,70)

ggplot(gender_cove, aes(x=age, fill=Gender,group=Gender)) +
  geom_histogram(aes(y=-..density..),alpha=0.6,
               data = ~ subset(., Gender %in% c("m")))+
  geom_histogram(aes(y=..density..),alpha=0.6,
               data = ~ subset(., !Gender %in% c("m")))+
  scale_y_continuous(labels = function(x) percent(abs(x)), limits = c(-0.1, 0.1)) +
  scale_fill_discrete(labels=c("Female", "Male")) +
  theme(text=element_text(size=16)) + 
  ylab('Share of Attendance') + 
  xlab('Age') + 
  xlim(18,70)
  
ggplot(gender_cove, aes(x=age, fill=Gender,group=Gender)) +
  geom_histogram(aes(y=..density..),alpha=0.6,
                 data = ~ subset(., Gender %in% c("m")))+
  geom_histogram(aes(y=..density..),alpha=0.6,
                 data = ~ subset(., !Gender %in% c("m")))+
  scale_y_continuous(labels = function(x) percent(abs(x))) +
  scale_fill_discrete(labels=c("Female", "Male")) +
  theme(text=element_text(size=16)) + 
  ylab('Share of Attendance') + 
  xlab('Age') + 
  xlim(18,70)

#Heath likes
ggplot(gender_cove, aes(x=age, color=Gender)) +
  geom_histogram(fill="white", alpha=0.5, position=position_dodge(width=0.7)) +
  scale_color_discrete(labels=c("Female", "Male")) +
  theme(text=element_text(size=16)) + 
  scale_y_continuous(labels=scales::comma_format()) +
  ylab('Attendance') +  
  xlab('Age') + 
  xlim(18,70)

#Heath likes with custom bins

gender_cove$bin <- ifelse(
  gender_cove$age<23, "18-23", ifelse(
    gender_cove$age<30, "23-30", ifelse(
      gender_cove$age <40, "31-40", ifelse(
        gender_cove$age <50, "41-50", ">50"
      )
    )))

gender_cove$bin <- factor(gender_cove$bin, levels=c("18-23", "23-30", "31-40", "41-50", ">50"))

ggplot(gender_cove, aes(x=bin, fill=Gender)) +
  geom_bar(stat="identity", fill="white", alpha=0.5, position=position_dodge(width=0.7)) +
  scale_color_discrete(labels=c("Female", "Male")) +
  theme(text=element_text(size=16)) + 
  scale_y_continuous(labels=scales::comma_format()) +
  ylab('Attendance') +  
  xlab('Age')

ggplot(gender_cove, aes(x=as.factor(bin), fill=as.factor(Gender) )) + 
  geom_bar(position=position_dodge(width=0.7)) +
  scale_fill_discrete(labels=c("Female", "Male"), name="Gender") +
  theme(text=element_text(size=30)) + 
  scale_y_continuous(labels=scales::comma_format()) +
  ylab('Attendance') +  
  xlab('Age')
  
  



zip_coord <- read_csv("zip_coord.csv")
zip_coord$ZIP <- sub("^0+", "", zip_coord$ZIP)  
cove_guests <- read_csv("cove_guests.csv")
cove_guests$zip <- as.character(cove_guests$zip)
cove_guests <- cove_guests %>% 
  filter(!state == "HAWAII") %>%
  filter(!state == "ALBERTA") %>%
  filter(!state == "QUEBEC") %>%
  filter(!state == "PR") %>%
  filter(!state == "ONTARIO") %>%
  filter(!state == "AK")
  
states_coords <- read.csv("states.csv")
                                      
unique(cove_guests$state)

zip_freq <- cove_guests %>% count(zip) %>%
  rename(ZIP = zip) %>%
  left_join(zip_coord) %>%
  drop_na() %>%
  filter(!LNG < -126) %>%
  filter(!LAT < 25.2)


# Plot function
plot_connection=function( dep_lon, dep_lat, arr_lon, arr_lat, ...){
  inter <- gcIntermediate(c(dep_lon, dep_lat), c(arr_lon, arr_lat), n=50, addStartEnd=TRUE, breakAtDateLine=F)             
  inter=data.frame(inter)
  diff_of_lon=abs(dep_lon) + abs(arr_lon)
  if(diff_of_lon > 180){
    lines(subset(inter, lon>=0), ...)
    lines(subset(inter, lon<0), ...)
  }else{
    lines(inter, ...)
  }
}


ZipCodeSourceFile = "http://download.geonames.org/export/zip/US.zip"
temp <- tempfile()
download.file(ZipCodeSourceFile , temp)
ZipCodes <- read.table(unz(temp, "US.txt"), sep="\t")
unlink(temp)
names(ZipCodes) = c("CountryCode", "ZIP", "PlaceName", 
                    "AdminName1", "AdminCode1", "AdminName2", "AdminCode2", 
                    "AdminName3", "AdminCode3", "latitude", "longitude", "accuracy")
ZipCodes$ZIP <- as.character(ZipCodes$ZIP)

StateCount <- zip_freq %>%
  left_join(ZipCodes) %>%
  group_by(AdminName1) %>%
  summarize(visit_count = sum(n, na.rm=TRUE)) %>%
  rename(state = AdminName1) %>%
  inner_join(states_coords)




par(mar=c(0,0,0,0))

maps::map('state',
          col="#f2f2f2", fill=TRUE, bg="white", lwd=0.05,
          mar=rep(0,4),border=1 
)

maps::map('state',
          col="#000000", fill=FALSE, bg="white", lwd=0.05,
          mar=rep(0,4),border=0
)

#points(x=zip_freq$LNG, y=zip_freq$LAT, col="#0075c9", cex=0.7, pch=20)

# Compute the connection between Buenos Aires and Paris
college_station_coords <- c(30.6280, -96.3344)

#zipcode mapping
for(i in 1:nrow(zip_freq)){
  plot_connection(college_station_coords[2], college_station_coords[1], zip_freq$LNG[i], zip_freq$LAT[i], col="#0075c9", lwd=(zip_freq$n / 10))
}

# State mapping
for(i in 1:nrow(StateCount)){
  plot_connection(college_station_coords[2], college_station_coords[1], StateCount$longitude[i], StateCount$latitude[i], col="#0075c9", lwd=(log(StateCount$visit_count[i])*2))
}

# Show this connection
lines(inter, col="slateblue", lwd=2)


#################
# Cloropath
#################

# I need to 'fortify' the data to be able to show it with ggplot2 (we need a data frame format)
library(broom)
spdf@data = spdf@data %>% mutate(google_name = gsub(" \\(United States\\)", "", google_name))
spdf_fortified <- tidy(spdf, region = "google_name")

# Calculate the centroid of each hexagon to add the label:
library(rgeos)
centers <- cbind.data.frame(data.frame(gCentroid(spdf, byid=TRUE), id=spdf@data$iso3166_2))

# Now I can plot this shape easily as described before:
ggplot() +
  geom_polygon(data = spdf_fortified, aes( x = long, y = lat, group = group), fill="skyblue", color="white") +
  geom_text(data=centers, aes(x=x, y=y, label=id)) +
  theme_void() +
  coord_map()

###############################

# Geospatial data available at the geojson format
library(geojsonio)

spdf <- geojson_read("https://eric.clst.org/assets/wiki/uploads/Stuff/gz_2010_us_040_00_500k.json",  what = "sp")

# I need to fortify the data AND keep trace of the commune code! (Takes ~2 minutes)
library(broom)
spdf_fortified <- tidy(spdf, region = "NAME")
spdf_fortified <- spdf_fortified %>%
  rename(state = id) %>%
  left_join(StateCount) %>%
  filter(!state == "Hawaii") %>%
  filter(!state == "Alaska") %>%
  filter(!state == "Puerto Rico")

#test <- geojson_read("https://raw.githubusercontent.com/gregoiredavid/france-geojson/master/communes.geojson",  what = "sp")

# Now I can plot this shape easily as described before:

library(Cairo)
CairoWin()


ggplot() +
  geom_polygon(data = spdf_fortified, aes( x = long, y = lat, group = group, fill = visit_count), color="grey") +
  theme_void() +
  coord_map() + 
  scale_fill_viridis(labels = scales::comma, trans = "log", breaks=c(1,100,500,1000,5000,20000), name="Number of Visitors", guide = guide_legend( keyheight = unit(3, units = "mm"), keywidth=unit(12, units = "mm"), label.position = "bottom", title.position = 'top', nrow=1) ) +
  theme(
    text = element_text(color = "#22211d"),
    
    plot.title = element_text(size= 22, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    plot.subtitle = element_text(size= 30, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.43, l = 2, unit = "cm")),
    plot.caption = element_text( size=20, color = "#4e4d47", margin = margin(b = 0.3, r=-99, unit = "cm") ),
    legend.text = element_text(size=20),
    legend.title = element_text(size=28),
    legend.position = c(0.2, 0.09)
  )

ggplot() +
  geom_polygon(data = spdf_fortified, aes( x = long, y = lat, group = group, fill = visit_count), color="grey") +
  theme_void() +
  coord_map() + 
  scale_fill_viridis(option="magma", labels = scales::comma, trans = "log", breaks=c(1,100,500,1000,5000,20000), name="Number of Visitors", guide = guide_legend( keyheight = unit(3, units = "mm"), keywidth=unit(12, units = "mm"), label.position = "bottom", title.position = 'top', nrow=1) ) +
  theme(
    text = element_text(color = "#22211d"),
    
    plot.title = element_text(size= 22, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    plot.subtitle = element_text(size= 30, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.43, l = 2, unit = "cm")),
    plot.caption = element_text( size=20, color = "#4e4d47", margin = margin(b = 0.3, r=-99, unit = "cm") ),
    legend.text = element_text(size=20),
    legend.title = element_text(size=28),
    legend.position = c(0.2, 0.09)
  )
