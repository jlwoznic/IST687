dummyDF<- data.frame(state.name, stringsAsFactors = FALSE)
dummyDF$state<-tolower(dummyDF$state.name)

install.packages("mapproj")
library(mapproj)
install.packages("ggmap")
library(ggmap)

us <- map_data("state")
map.simple<-ggplot(dummyDF, aes(map_id=state))
map.simple<-map.simple + geom_map(map = us, fill="white", color="black")
map.simple<- map.simple + expand_limits(x=us$long, y=us$lat)
map.simple<-map.simple + coord_map() + ggtitle("basic USA map")
map.simple

latlong <- NewLatLon("miami, fl")
map.simple<-map.simple + geom_point(aes(x=latlong$lon,y=latlon$lat), color="darkred", size=3)

cities<-c("Manhattan, NY", "Boston, MA", "Philadelphia, PA", "Tampa, FL", "Chicago, IL", 
          "Boise, ID", "San Francisco, CA", "Seattle, WA", "Houston, TX")
cities<-tolower(cities)
bus<-c(10,7,6,5,7,3,10,7,5)
weather<-c(5,3,6,7,3,6,10,7,2)
living<-c(7,6,6,7,5,4,6,8,2)
city.df<-data.frame(cities, bus, weather, living)
city.df$geoCode<-NewLatLon(cities)

map.simple + geom_point(data=city.df, aes(x=geoCode$lon,y=geoCode$lat))
map.simple + geom_point(data=city.df, aes(x=geocode$lon, y=geoCode$lat, size=bus, color=weather))

