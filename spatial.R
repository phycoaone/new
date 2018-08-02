setwd("C:/Users/athena/Desktop/Project")
install.packages("ggmap")
library(sf)
library(tidyverse)
library(ggmap)

## Install ggmap if you haven't already
#install.packages("ggmap")

## Intro to R simple features and the concepts underlying spatial data analysis
## sf help manual can be found here: https://r-spatial.github.io/sf/

## Check working directory, set to 
# getwd()
# setwd()

## Unzipping the Chicago shapefiles
unzip("chicago_tracts_2010.zip")

## Reading in the shapefile. Note the geometry column, this is what
## stores the coordinates that make up a shape
chicago_gdf <- st_read("chicago_tracts_2010.shp")
glimpse(chicago_gdf) #commarea,geoid,deometry

## Plotting the 'commarea' column of our shapefile, this shows all 77
## Chicago community areas
plot(chicago_gdf["commarea"])

## plot() will always plot every column unless you tell it a specific one
plot(chicago_gdf)

## Loading some more interesting data on Chicago incomes
chicago_income <- read_csv("chicago_tracts_income.csv")
glimpse(chicago_income)#geoid,income...

## Columns of data imported with sf sometimes start as factors, we should
## convert them to characters. These two are essentially the same thing:
chicago_gdf$geoid <- as.character(chicago_gdf$geoid)
chicago_gdf <- chicago_gdf %>% mutate(commarea = as.character(chicago_gdf$commarea))

## Same for the income data imported with read_csv. Must use characters so that
## leading zeroes are not dropped. as.numeric() will drop leading zeroes
chicago_income$geoid <- as.character(chicago_income$geoid)

## Geospatial dataframes are just like normal ones. They can be filtered,
## joined, and mutated, but ONLY st_ functions will work on the geometry column.
chicago_gdf <- left_join(chicago_gdf, chicago_income, by = "geoid")
glimpse(chicago_gdf)

## Plot the new columns added to our spatial data frame
plot(chicago_gdf["total"])
plot(chicago_gdf[4])
plot(chicago_gdf["per_under_25k"])

## Plot them side-by-side for comparison, they're very different maps
plot(chicago_gdf[c("total", "per_under_25k")])

## Plot a random sample of half the rows in our dataframe, note that each
## row is equal to one shape
chicago_gdf_sample <- chicago_gdf %>% sample_frac(0.5)
plot(chicago_gdf_sample["per_under_25k"])

## Now let's read in some point data, note that this isn't a spatial data frame
chicago_permits <- read_csv("chicago_latlon_permits.csv")

## We need to convert the latitude and longitude here into a geometry column
## Latitude and longitude almost always use the CRS 4326
chicago_permits <- st_as_sf(chicago_permits,
                            coords = c("longitude", "latitude"),
                            crs = 4326)
glimpse(chicago_permits)

## Don't plot the whole dataframe, it may take a long time
chicago_permits_sample <- chicago_permits %>% sample_frac(0.1)

## As you can see, most permits aren't for very expensive things, but let's
## see where the expensive development is happening
plot(chicago_permits_sample["estimated_cost"])

## Filter the data so that only 99th quantile projects remain
chicago_permits_sample <- chicago_permits_sample %>%
  filter(estimated_cost >= quantile(chicago_permits_sample$estimated_cost, 0.99))

## Now we can see a trend, but it's still till hard to visualize
## There's too many points and no points of reference (roads, boundaries, etc.)
plot(chicago_permits_sample["estimated_cost"])

## We can somewhat solve this problem by performing a spatial join, merging the boundary
## data of the tract shapes with the point data of the permits

## First reload the tract data to get rid of the data we added to it
chicago_gdf <- st_read("chicago_tracts_2010.shp")

## We must set the CRS of the read Chicago tracts file before performing a merge
## Merges only work on two geometries with the same CRS
chicago_gdf <- st_set_crs(chicago_gdf, 4326)
#or to check its crs:st_crs(chicago_gdf)

## To perform a merge, use st_join(). The left-hand geometry is preserved
chicago_permits_merged <- st_join(chicago_permits, chicago_gdf, join = st_within)

## At this point, we want to aggregate using group_by() to get the mean estimated cost
## of a building permit for each tract, we no longer need the point geometries (or may cause a problem when aggregate)
## We can get rid of them using:
chicago_permits_merged$geometry <- NULL

## Notice the new geoid and commarea columns, which were merged on from the tracts
## Now we know which tract each building permit is in, so we can aggregate them
chicago_permits_agg <- chicago_permits_merged %>%
  group_by(geoid) %>%
  summarize(cost = mean(estimated_cost))

## The result is the mean cost of permits per census tract, but now we have no geometry
## to plot. To fix this, we can merge back on the Chicago census tracts using their geoid
chicago_permits_agg <- chicago_gdf %>%
  left_join(chicago_permits_agg, by = "geoid")

### NOTE ### 
# In order to make R recognize the geometry column as the thing you're trying to plot, you must
# merge ONTO the data frame which already contains geometry. Merging a data frame with geometry
# onto one without geometry will cause R to lose the attribute that makes the geometry column
# special, and it will just plot your data normally
############

## Now we can finally plot our choropleth!
plot(chicago_permits_agg["cost"])

## It's ugly! What's the problem here?


## We can fix some of the issues by adjusting the scale using ggplot
## ggplot uses a special geom for sf objects called geom_sf(), the color and fill aesthetics
## are what will give the choropleth its color

## First we should make a new set of breaks by which to classify the data, we can break it into
## quantiles for easier viewing using the ntile() function
chicago_permits_agg <- chicago_permits_agg %>%
  mutate(qtile = factor(ntile(cost, 9)))

## Now we can create a ggplot with our binned, factorized data
ggplot() +
  geom_sf(data = chicago_permits_agg, aes(color = qtile, fill = qtile)) 

## But ggplot alone is just as ugly as plot(), so let's clean it up

## First, we should reorder our factors:

chicago_permits_agg$qtile <- factor(chicago_permits_agg$qtile, levels=rev(levels(chicago_permits_agg$qtile)))

## Now plot with labels and a proper color palette
ggplot() +
  geom_sf(data = chicago_permits_agg, aes(color = qtile, fill = qtile)) +
  scale_color_brewer(palette = "YlGn", guide = FALSE, direction = -1) + 
  scale_fill_brewer(palette = "YlGn", name = "Quantile", direction = -1) + 
  theme_void() +
  theme(
    panel.grid = element_line(color = "transparent")  # Small hack to fix grid lines in map plots
  ) +
  labs(
    title = "Quantiles of Estimated Costs for Building Permits in Chicago",
    subtitle = "2006 to Present"
  )

## What are the problems with this map?



## Now for some geocoding! (if we have time)

## Load the bikes tickets dataset
bikes <- read_csv("chicago_bike_sample.csv")

## Note that this contains only addresses, no lat/lon, no geometry. We need a way to get
## lat/lon from the address field if we want to plot. That's where geocoding comes in, 
## we'll be doing it the easy way
bikes <- bikes %>%
  mutate(full_address = paste(violation_location, "Chicago IL")) %>%
  mutate_geocode(location = full_address, source = "dsk")

## New fields lon and lat are added to the bikes data frame, we can plot these with ggplot
## using our census tracts as boundaries
ggplot() +
  geom_sf(data = chicago_gdf) +
  geom_point(
    data = bikes,
    aes(x = lon, y = lat),
    size = 2,
    color = "purple") +
  theme_void() +
  theme(
    panel.grid = element_line(color = "transparent")  # Small hack to fix grid lines in map plots
  )

## If your map looks really screwy, it's possible that the geocoder incorrectly coded the addresses
## we gave it. We can get ride of these outliers with a little trick: keep only the points within
## the boundaries of our tract files

## First convert the points to an sf geometry, then join them to the tracts
bikes <- st_as_sf(bikes, coords = c("lon", "lat"), crs = 4326, remove = FALSE)
bikes <- st_join(bikes, chicago_gdf, join = st_within)

## Any point that is missing a geoid after merging is NOT within the tract areas, so
## we can filter them out
bikes <- bikes %>% filter(!is.na(geoid))
bikes$geometry <- NULL

## Then replot
ggplot() +
  geom_sf(data = chicago_gdf) +
  geom_point(
    data = bikes,
    aes(x = lon, y = lat),
    size = 2,
    color = "purple") +
  theme_void() +
  theme(
    panel.grid = element_line(color = "transparent") ) # Small hack to fix grid lines in map plots


## A quick leaflet example, time permitting
install.packages("leaflet")
install.packages("viridis")
library(leaflet)

pal <- colorFactor("viridis", chicago_permits_agg$qtile)

leaflet() %>% setView(lng = -87.63, lat = 41.87, zoom = 11) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(
    data = chicago_permits_agg,
    color = "#e2e2e2",
    fillOpacity = 0.5,
    weight = 1,
    smoothFactor = 0.2,
    fillColor = ~pal(chicago_permits_agg$qtile)
    )

