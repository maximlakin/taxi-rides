# Union Square 40.736, -73.9905

# install.packages("gpclib") # General polygon clipping routines for R based on Alan Murta's C library
library(ggplot2)
library(ggmap) # A collection of functions to visualize spatial data and models on top of static maps from various online sources (e.g Google Maps and Stamen Maps). It includes tools common to those tasks, including functions for geolocation and routing
library(dplyr) # A fast, consistent tool for working with data frame like objects, both in memory and out of memory
library(reshape2) # Flexibly restructure and aggregate data using just two functions: melt and dcast (or acast).
library(zoo) # An S3 class with methods for totally ordered indexed observations. It is particularly aimed at irregular time series of numeric vectors/matrices and factors. zoo's key design goals are independence of a particular index/date/time class and consistency with ts and base R by providing methods to extend standard generics
library(scales) # Graphical scales map data to aesthetics, and provide methods for automatically determining breaks and labels for axes and legends
library(extrafont) # Tools to using fonts other than the standard PostScript fonts
library(grid) # A rewrite of the graphics layout capabilities, plus some support for interaction.
library(RPostgreSQL) # postgres connection
library(rgdal) # Provides bindings to Frank Warmerdam's Geospatial Data Abstraction Library (GDAL) (>= 1.6.3) and access to projection/transformation operations from the PROJ.4 library. The GDAL and PROJ.4 libraries are external to the package, and, when installing the package from source, must be correctly installed first. Both GDAL raster and OGR vector map data can be imported into R, and GDAL raster data and OGR vector data exported. Use is made of classes defined in the sp package. Windows and Mac Intel OS X binaries (including GDAL, PROJ.4 and Expat) are provided on CRAN
library(maptools) # Set of tools for manipulating and reading geographic data, in particular ESRI shape-files; C code used from shapelib. It includes binary access to GSHHG shoreline files. The package also provides interface wrappers for exchanging spatial objects with packages such as PBSmapping, spatstat, maps, RArcInfo, Stata tmap, WinBUGS, Mondrian, and others
gpclibPermit()
source("helpers.R")

tracts = spTransform(readOGR("../nyct2010_15b", layer = "nyct2010"), CRS("+proj=longlat +datum=WGS84"))
tracts@data$id = as.character(as.numeric(rownames(tracts@data)) + 1)

tracts.points = fortify(tracts, region = "id")
tracts.map = inner_join(tracts.points, tracts@data, by = "id")
nyc_map = filter(tracts.map, BoroName == "Manhattan")
nyc_map$sandbox <- runif(length(nyc_map$lat), min=0, max=1)
tracts.map$sandbox <- runif(length(tracts.map$lat), min=0, max=1)
ggplot() + geom_polygon(data=tracts.map, aes(x=long, y=lat, group=group, fill=sandbox))





ex_staten_island_map = filter(tracts.map, BoroName != "Staten Island")



# NYC dot maps
pickups = query("SELECT * FROM trips_by_lat_long_cab_type ORDER BY count")
pickups = mutate(pickups, cab_type_id = factor(cab_type_id))

centroids = query("SELECT * FROM neighborhood_centroids")
head(centroids,2)
# map
alpha_range = c(0.14, 0.75)
size_range = c(0.72, 1.02)

p = ggplot() +
    geom_polygon(data = ex_staten_island_map,
                 aes(x = long, y = lat, group = group),
                 fill = "#405060", color = "#405060", size = 0) +
    geom_point(data = pickups,
               aes(x = pickup_long, y = pickup_lat, alpha = count, size = count, color = cab_type_id)) +
    scale_alpha_continuous(range = alpha_range, trans = "log", limits = range(pickups$count)) +
    scale_size_continuous(range = size_range, trans = "log", limits = range(pickups$count)) +
    scale_color_manual(values = c("#00ddff", "#708090")) +
    coord_map(xlim = range(ex_staten_island_map$long), ylim = range(ex_staten_island_map$lat)) +
    theme_dark_map() +
    theme(legend.position = "none")

fname = "~/taxi_pickups_map_hires.png"
png(filename = fname, width = 2880, height = 4068, bg = "#203040")
print(p)
dev.off()

# general stats

ex_staten_island_map
head(filter(ex_staten_island_map, id == "2"),5)

speed_stats = query("select pickup_nyct2010_gid as id, avg(speed), avg(trip_distance/geo_distance) from speed_summary where speed > 0 and speed < 100 and trip_distance > 0 and geo_distance > 0 group by pickup_nyct2010_gid")
names(speed_stats) <- c("id","speed","dist")
speed_stats$id = as.character(speed_stats$id)

plt = inner_join(ex_staten_island_map, speed_stats, by = "id")
names(plt)
p = ggplot() +
  geom_polygon(data = plt,
               aes(x = long, y = lat, group = group, fill=speed),
               color = "#405060", size = 0) +
  scale_fill_gradient(name="", low = "#405060", high = "#aaffaa", trans = "log", limits = range(plt$speed),breaks=c(20, 1200),labels=c("Slow", "Fast"))+
  coord_map(xlim = range(ex_staten_island_map$long), ylim = range(ex_staten_island_map$lat)) +
  theme_dark_map() + 
  title_with_subtitle("Average Speed")

fname = "~/taxi_pickups_stats.png"
png(filename = fname, width = 1000, height = 1000, bg = "#203040")
print(p)
dev.off()


p = ggplot() +
  geom_polygon(data = plt,
               aes(x = long, y = lat, group = group, fill=dist),
               color = "#405060", size = 0) +
  scale_fill_gradient(name="", low = "#405060", high = "#ff8888", trans = "log", limits = range(plt$dist),breaks=c(2, 50),labels=c("Low", "High"))+
  coord_map(xlim = range(ex_staten_island_map$long), ylim = range(ex_staten_island_map$lat)) +
  theme_dark_map() + 
  title_with_subtitle("Path Deviation")

fname = "~/taxi_pickups_stats2.png"
png(filename = fname, width = 1000, height = 1000, bg = "#203040")
print(p)
dev.off()

# Pickup & Drop off
trips = query("SELECT pickup_longitude, pickup_latitude, dropoff_longitude, dropoff_latitude FROM trips limit 100")

p = ggplot() +
    geom_polygon(data = ex_staten_island_map,
                 aes(x = long, y = lat, group = group),
                 fill = "#405060", color = "#405060", size = 0) +
    geom_point(data = trips,
               aes(x = pickup_longitude, y = pickup_latitude), size=1.0, alpha=1.0, color = "#00DDFF") +
    geom_point(data = trips,
               aes(x = dropoff_longitude, y = dropoff_latitude), size=1.0, alpha=1.0, color = "#ff6666") +
    coord_map(xlim = range(ex_staten_island_map$long), ylim = range(ex_staten_island_map$lat)) +
    theme_dark_map() +
    theme(legend.position = "none")

print(p)


# Pickup & Drop off
hour = 19
# trips = query(sprintf("SELECT trip_distance, pickup_longitude, pickup_latitude, dropoff_longitude, dropoff_latitude FROM trips where pickup_longitude != 0 and dropoff_longitude != 0 and trip_distance !=0 and extract('hour' from pickup_datetime) = %s and (pickup_datetime > '01/01/2014' and pickup_datetime < '03/21/2014') or (pickup_datetime > '09/21/2014' and pickup_datetime < '03/21/2015')  or (pickup_datetime > '09/21/2015' and pickup_datetime < '01/01/2016') limit 100000", hour) )
trips = query(sprintf("SELECT trip_distance, pickup_longitude, pickup_latitude, dropoff_longitude, dropoff_latitude FROM trips where cab_type_id = 1 and pickup_longitude != 0 and dropoff_longitude != 0 and trip_distance !=0 and extract('hour' from pickup_datetime) = %s and ((pickup_datetime > '01/01/2014' and pickup_datetime < '03/21/2014') or (pickup_datetime > '09/21/2014' and pickup_datetime < '12/31/2014')) limit 100000", hour) )
p = ggplot() +
    geom_polygon(data = ex_staten_island_map,
                 aes(x = long, y = lat, group = group),
                 fill = "#405060", color = "#405060", size = 0) +
    
    geom_point(data = trips,
               aes(x = dropoff_longitude, y = dropoff_latitude, color = "dropoff"), alpha=0.2, size=0.1) +   
    geom_point(data = trips,
               aes(x = pickup_longitude, y = pickup_latitude, color = "pickup"), alpha=0.2, size=0.1) +  
     
    coord_map(xlim = range(ex_staten_island_map$long), ylim = range(ex_staten_island_map$lat)) +
    theme_dark_map() +
    title_with_subtitle("Winter Rides",sprintf("%s pm",hour-12))+
    guides(colour = guide_legend(override.aes = list(size=4)))+
    scale_colour_manual(name  ="Legend", breaks=c("pickup","dropoff"),
                            labels=c("Pickups", "Dropoffs"), values=c("#6688ff","#88ffff"))
    
fname = sprintf("~/nyc_map_%s.png",hour)
png(filename = fname, width = 1000, height = 1000, bg = "#203040")
print(p)
dev.off()

# Distance
hour = 17
trips = query(sprintf("SELECT trip_distance, pickup_longitude, pickup_latitude, dropoff_longitude, dropoff_latitude FROM trips where pickup_longitude != 0 and dropoff_longitude != 0 and trip_distance !=0 and extract('hour' from pickup_datetime) = %s limit 100000", hour) )
p = ggplot() +
    geom_polygon(data = ex_staten_island_map,
                 aes(x = long, y = lat, group = group),
                 fill = "#405060", color = "#405060", size = 0) +
    geom_point(data = trips,
               aes(x = dropoff_longitude, y = dropoff_latitude, color = trip_distance), alpha=0.5, size=0.1) +  
    coord_map(xlim = range(ex_staten_island_map$long), ylim = range(ex_staten_island_map$lat)) +
    theme_dark_map() +
    title_with_subtitle("Taxi Rides","distance")+
    guides(colour = guide_legend(override.aes = list(size=4)))+
    scale_color_gradient(low = "#203040", high = "#00eeff", limits=c(0, 10))
    
fname = sprintf("~/nyc_map_dist.png",hour)
png(filename = fname, width = 1000, height = 1000, bg = "#203040")
print(p)
dev.off()

# Speed
hour = 0
trips = query(sprintf("SELECT trip_distance, extract(epoch from dropoff_datetime-pickup_datetime)/60 as time, trip_distance/(extract(epoch from dropoff_datetime-pickup_datetime)/3600) as speed, pickup_longitude, pickup_latitude, dropoff_longitude, dropoff_latitude FROM trips where extract(epoch from dropoff_datetime-pickup_datetime) > 0 and pickup_longitude != 0 and dropoff_longitude != 0 and trip_distance !=0 and extract('hour' from pickup_datetime) = %slimit 1000", hour) )
trips = filter(trips, speed < 100)
p = ggplot() +
    geom_polygon(data = ex_staten_island_map,
                 aes(x = long, y = lat, group = group),
                 fill = "#405060", color = "#405060", size = 0) +
    geom_segment(data = trips,
                 aes(x = pickup_longitude, y = pickup_latitude, xend = dropoff_longitude, yend = dropoff_latitude, color=speed, alpha=speed)) +
    coord_map(xlim = range(ex_staten_island_map$long), ylim = range(ex_staten_island_map$lat)) +
    theme_dark_map() +
    title_with_subtitle("Taxi Rides","distance")+
    guides(colour = guide_legend(override.aes = list(size=4)))+
    scale_color_gradient(low = "#ff7777", high = "#00eeff", limits = range(trips$speed))+
    scale_alpha_continuous(range = c(0.0, 0.8), limits = range(trips$speed))
    
    
fname = sprintf("~/nyc_map_speed.png",hour)
png(filename = fname, width = 1000, height = 1000, bg = "#203040")
print(p)
dev.off()

total_pickups = query("select date, sum(trips) from daily_dropoffs_by_borough where type='yellow' and date < '01/01/2015' group by date order by date")
total_pickups = filter(total_pickups, sum > 100)
total_pickups$sum <- total_pickups$sum/1000


fname = "~/nyc_map_time.png"
png(filename = fname, width = 1500, height = 600)
plot(total_pickups$sum~as.Date(total_pickups$date,"%d/%m/%y"), type="l",xlab="day",ylab="pickups (K)",main="Daily Cab Pickups (2014)")
dev.off()
