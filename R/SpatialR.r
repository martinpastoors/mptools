# --------------------------------------------------------------------------------------
# SpatialR course
# https://heima.hafro.is/~einarhj/spatialr/
# https://github.com/fishvice/spatialr
# Einar Hjorleifsson and Julian Burgos
# 16 April 2020
# --------------------------------------------------------------------------------------

library(tidyverse)
library(sf)
library(maps)
library(readxl)

# --------------------------------------------------------------------------------------
# Day 1 - Monday (10:00 - 17:00)
# R & RStudio (EH)
# Spatial data in R (JB)
# Introduction to ggplot (EH)
# --------------------------------------------------------------------------------------

iceland <- map_data("world", region = "Iceland")
m <- ggplot(iceland, aes(long, lat, group = group))
m1 <- m + geom_point()
m2 <- m + geom_line()
m3 <- m + geom_path()
m4 <- m + geom_polygon()
m1; m2; m3; m4

# Projections
m <- 
  ggplot(iceland, aes(long, lat, group = group)) +
  geom_path()
m + coord_fixed(ratio = 2.4) 
m + coord_quickmap()
m + coord_map()
m + coord_map(projection = "stereographic")

# Adding layers
minke <- read_csv("ftp://ftp.hafro.is/pub/data/csv/minke.csv")
m <-
  ggplot() +
  geom_polygon(data = iceland, 
               aes(long, lat, group = group)) +
  geom_point(data = minke, 
             aes(lon, lat, colour = sex))
m + coord_quickmap()

# Customization
m <- 
  ggplot() +
  geom_polygon(data = iceland, 
               aes(long, lat, group = group),
               fill = "grey") +
  geom_point(data = minke,
             aes(lon, lat, colour = sex)) +
  scale_x_continuous(name = NULL,
                     breaks = NULL) +
  scale_y_continuous(name = NULL,
                     breaks = NULL) +
  scale_colour_brewer(name = "Sex",
                      palette = "Set1") +
  theme(legend.position = c(0.5, 0.5)) +
  coord_quickmap(xlim = c(-25, -20), ylim = c(65, 66.6))
m

# list of colours
colours()

# More layers
smb <-read_csv("ftp://ftp.hafro.is/pub/data/csv/is_smb.csv") 
smb2019 <- filter(smb, year == 2019)
glimpse(smb2019)

# background layer
m <- 
  ggplot() +
  geom_polygon(data = iceland, aes(long, lat, group = group),
               fill = "grey") +
  coord_map() +
  scale_x_continuous(name = NULL, breaks = NULL) +
  scale_y_continuous(name = NULL, breaks = NULL)

# geom_segment for begin and end
m +
  geom_segment(data = smb2019,
               aes(x    = lon1, y    = lat1,
                   xend = lon2, yend = lat2))

# size of cod
p <-
  m +
  geom_point(data = smb2019,
             aes(lon1, lat1, size = cod_n),
             alpha = 0.2,
             colour = "red")
p

# scale to 20
p +
  scale_size_area(max_size = 20)

p +
  scale_size_area(max_size = 20) +
  geom_segment(data = smb2019,
               aes(x    = lon1, y    = lat1,
                   xend = lon2, yend = lat2))

track <- read_csv("ftp://ftp.hafro.is/pub/data/csv/is_smb_vms2019.csv")
m +
  geom_path(data = track,
            aes(x = lon, y = lat, colour = vessel))

# geom_tile
s <-
  smb2019 %>% 
  group_by(ir_lon, ir_lat) %>% 
  summarise(Cod = mean(cod_n))
p <-
  m +
  geom_tile(data = s,
            aes(x = ir_lon, y = ir_lat, fill = Cod))
p

p +
  scale_fill_viridis_c(option = "B", direction = -1)


ggplot() +
  geom_tile(data = s,
            aes(x = ir_lon, y = ir_lat, fill = Cod)) +
  scale_x_continuous(name = NULL,
                     breaks = NULL) +
  scale_y_continuous(name = NULL,
                     breaks = NULL) +
  scale_fill_viridis_c(option = "B", direction = -1) +
  geom_polygon(data = iceland, 
               aes(long, lat, group = group),
               fill = "grey")  +
  coord_map()

# Raster model
xlim <- c(-28, -10)
ylim <- c(62.5, 67.5)

depth <- 
  marmap::getNOAA.bathy(lon1 = xlim[1], lon2 = xlim[2],
                lat1 = ylim[1], lat2 = ylim[2],
                resolution = 1) %>% 
  fortify() %>%  # turn the object into a data.frame
  filter(z <= 0)
glimpse(depth)

m +
  geom_raster(data = depth,
              aes(x = x, y = y, fill = z)) +
  coord_quickmap() +
  scale_x_continuous(name = NULL, breaks = NULL) +
  scale_y_continuous(name = NULL, breaks = NULL) +
  labs(fill = "Depth [m]")

# add contours to raster
m2 <- 
  m +
  geom_contour(data = depth, aes(x, y, z = z),
               breaks = c(-25, -50, -100, -200, -400),
               lwd = 0.1) 
m2

# facetting
smb2 <- 
  # more on filter later
  filter(smb, year %in% c(1995, 2000, 2005, 2010, 2015, 2019))
p <-
  m + 
  geom_point(data = smb2,
             aes(lon1, lat1, size = haddock_kg),
             alpha = 0.15,
             colour = "red") +
  scale_size_area(max_size = 15)
p +
  facet_wrap(~ year)

# --------------------------------------------------------------------------------------
# Day 2 - Tuesday (09:00 - 17:00)
# Data transformation (EH)
# The sf class (JB)
# Reading/writing spatial data(JB)
# --------------------------------------------------------------------------------------

# pivot_longer
# To make a table longer, we can employ the pivot_longer-function. Lets just take the biological variables:

smb <-read_csv("ftp://ftp.hafro.is/pub/data/csv/is_smb.csv") 

wide <- 
  smb %>% 
  select(id, cod_kg:monkfish_n)

long <- 
  wide %>% 
  # select just the abundance variables
  select(id, ends_with("_n")) %>% 
  tidyr::pivot_longer(cols = -id, names_to = "species", values_to = "n") %>% 
  mutate(species = str_remove(species, "_n"))

long <-
  wide %>% 
  # step 1
  pivot_longer(-id) %>%
  # step 2
  separate(name, sep = "_", into = c("species", "variable")) %>% 
  # step 3
  pivot_wider(names_from = variable)

# Simple features in R

# Create the geometry for individual features. In this case, two points.1
# Here we used the st_point() function to convert a vector into an sfg object. 
# sfg = single feature geometry
p1 <- sf::st_point(c(2, 3))
p2 <- sf::st_point(c(1, 4))

class(p1) # Class "sfg" and "POINT"

pts <- sf::st_sfc(p1, p2)
class(pts)
pts

# Attach some data for the points and get an sf object
mydata <- tibble(a = c (10, 20), b = c ("A", "B"))

mysf <- st_as_sf(mydata, geometry = pts)
class(mysf)
mysf

# st_point 	numeric vector (or one-row matrix)
# st_multipoint 	numeric matrix with points in row
# st_linestring 	numeric matrix with points in row
# st_multilinestring 	list with numeric matrices with points in rows
# st_polygon 	list with numeric matrices with points in rows
# st_multipolygon 	list of lists with numeric matrices
# st_geometrycollection 	list with (non-geometrycollection) simple feature objects

# List columns: This is a regular dataframe with three rows
my_df <- data.frame(a = 7:9, b = c("a", "b", "c"), c = c (10, 20, 30))

# This is a list with three elements
my_list <- list(1:2, "Hello", c("A", "B", "C"))

# Now let's use the list as a column in the dataframe
my_df$d <- my_list
print(my_df)

# A deeper look at SF data: bormicon data
bor <- read_sf("ftp://ftp.hafro.is/pub/data/shapes/bormicon.gpkg")
class(bor)
glimpse(bor)

# We can extract the metadata elements like this:
st_geometry_type(bor)

# This gets the coordinate reference system:
st_crs(bor)

# bounding box
st_bbox(bor)


# Exercise
# Examine the metadata of the data sets in the following locations:

ospar_st  <- st_read("ftp://ftp.hafro.is/pub/data/shapes/ospar.gpkg")
helcom_st <- st_read("ftp://ftp.hafro.is/pub/data/shapes/helcom.gpkg")
plot(ospar_st)
class(ospar_st)
head(ospar_st)

ospar_sf  <- read_sf("ftp://ftp.hafro.is/pub/data/shapes/ospar.gpkg")
helcom_sf <- read_sf("ftp://ftp.hafro.is/pub/data/shapes/helcom.gpkg")
plot(ospar_sf)
class(ospar_sf)
head(ospar_sf)

# What is the difference between the st_read and the read_sf() functions?
# st_read generates an sf object where the attributes of columns are not given (in fact they belong to the data)
# whereas in read_sf the attributes belong to the names. 

# default plot
plot(bor)

# plot the geometry
# Here, we used the st_geometry() function to extract the geometry of the sf object, dropping the attributes (i.e. the “non spatial” data).
plot(bor %>% st_geometry())

# plotting with ggplot
bor %>%
  ggplot() +
  theme_bw() +
  geom_sf(aes(fill = name)) +
  labs(fill = "Area")


# Making your own SF objects
# ============================
# We created a POINT geometry with the starting and ending of each tow (lat1 and lon1) using 
# the coords argument to pass the column names with the longitude (or x coordinate) first, 
# and the latitude (or y coordinate) second (this is a common mistake). 
# We also specified the coordinate reference system (4326 is the ESPG code 
# for unprojected data using the WSG84 datum).

smb <- 
  read_csv("ftp://ftp.hafro.is/pub/data/csv/is_smb.csv")

smb <- 
  read_csv("ftp://ftp.hafro.is/pub/data/csv/is_smb.csv") %>%
  st_as_sf(coords = c("lon1", "lat1"), crs = 4326)
plot(smb)

# Another example. Here we have VMS data from some bottom trawls. The data contains a series of 
# locations (latitude and longitude) during each trawl. If we do the same as in the smb dataset, 
# we obtain an sf object with POINT geometry.
vms <- read_csv("ftp://ftp.hafro.is/pub/data/csv/is_smb_vms2019.csv") %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

# vms
ggplot() + geom_sf(data=vms, alpha=0.25)

# Rather than just points, we want to join the points from each haul to form a line. 
# To do this we need to group the points by the ID column, and then use st_cast to convert the 
# group of points into linestrings.

vms <- 
  read_csv("ftp://ftp.hafro.is/pub/data/csv/is_smb_vms2019.csv") %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  mutate(fishing = if_else(speed > 2 & speed < 4, 1, 0)) %>%
  group_by(vid) %>%
  summarise(do_union = FALSE) %>%
  st_cast("LINESTRING")

ggplot() +
  geom_sf(data=vms, aes(colour = as.factor(vid))) +
  theme(legend.position = "none")

# The sdm object contains the latitud and longitude at the start and end of each haul. 
# Let’s make a it a LINESTRING, with straight lines for each haul.
#
# do_union: logical; in case summary does not create a geometry column, 
# should geometries be created by unioning using st_union, or simply by combining 
# using st_combine? Using st_union resolves internal boundaries, but in case of 
# unioning points, this will likely change the order of the points; see Details.

smb <- 
  read_csv("ftp://ftp.hafro.is/pub/data/csv/is_smb.csv")

tracks <- 
  smb %>%
  filter(year == 2019) %>%
  dplyr::select(id, 
                lat_1 = lat1, 
                lon_1 = lon1,
                lat_2 = lat2,
                lon_2 = lon2) %>%
  pivot_longer(cols = -id) %>%
  separate(name, sep = "_", into = c("info", "num")) %>%
  pivot_wider(names_from = info) %>%
  st_as_sf(coords = c ("lon", "lat"), crs = 4326) %>%
  group_by(id) %>%
  summarize(do_union = FALSE) %>%
  st_cast("LINESTRING")

ggplot() +
  geom_sf(data = tracks)

# sf manipulation

# Let’s load a set of polygons with the ICES ecoregions, and select the small ones.
ices_er <- read_sf("ftp://ftp.hafro.is/pub/data/shapes/ices_ecoregions.gpkg")
ggplot() +
  geom_sf(data = ices_er, aes(fill=ecoregion))

# small ones
sm_ices_er <- 
  ices_er %>%
  filter(area_km2 < 100)

ggplot() +
  geom_sf(data = sm_ices_er, aes(fill=ecoregion))

# Class Spatial
library(sp)
data(meuse)
class(meuse)

# Convert to class sp
coordinates (meuse) = ~ x + y 
class(meuse)

# From spatial to sf and back
meuse.sf <- meuse %>% st_as_sf() 
meuse.sp <- meuse %>% as("Spatial") # From sf to spatial
ggplot() + geom_sf(data=meuse.sf)

# Reading and writing spatial data
# --------------------------------

# vector drivers
drivers1 <- st_drivers("vector") %>% select(long_name)
nrow(drivers1)

# raster drivers
drivers2 <- st_drivers("raster") %>% select(long_name)
nrow(drivers2)

# GDAL: The Geospatial Data Abstraction Library (GDAL) is an external library for reading and 
# writing raster and vector geospatial data in many (>220) formats and databases.
# A full list of vector data drivers: https://gdal.org/drivers/vector/index.html
# and of raster data drivers: https://gdal.org/drivers/raster/index.html
# In R, the rgdal package provides the functions readOGR() and writeOGRL(), and readGDAL() 
# and writeGDAL() to read and write vector and raster data, respectively. 
# But rgdal uses objects of class sp.

# Shapefiles

# One of the most commonly used is the shapefile, developed by ESRI (the makers of ArcGis). 
# A lot of the vector data you will find online is in this format. But it has limitations 
# (see http://switchfromshapefile.org/).
# They can hold either points, lines or polygons.
# A shapefile is not a single file, but several files with the same name, different extensions, 
# in the same folder. There are three mandatory files:
#  .shp shape format (geometric entities) 
#  .shx index of the geometric entities 
#  .dbf attributes (data)
# Other files usually include a .prj file, with the coordinate system.

# Let’s take a look at some shapefiles. This bit of code creates the “data_raw” folder 
# (in case it does not exist), downloads a zip file with two shapefiles, 
# extracts the shapefiles from the zip file, and finally deletes the zip file.

dir.create("./data_raw")
target.file <- "./data_raw/Iceland_shapefiles.zip"
download.file(url = "ftp://ftp.hafro.is/pub/data/shapes/Iceland_shapefiles.zip",
              destfile = target.file)
unzip(target.file, overwrite = TRUE, exdir = "./data_raw")
file.remove(target.file)  # Delete the zip file

coast <- read_sf("./data_raw/Iceland_shapes/Iceland_coast.shp")
glimpse(coast)

write_sf(coast, "./data_raw/my_shapefile.shp")


st_simplify(helcom, dTolerance = 5000)

# --------------------------------------------------------------------------------------
# Day 3 - Wednesday (09:00 - 17:00)
# Coordinate Reference Systems (JB)
# Working with geometries (JB)
# Reproducible document writing in R? (EH)
# Spatial operations (JB)
# --------------------------------------------------------------------------------------

library(tidyverse)
library(sf)
library(maps)
library(rnaturalearth)
library(units)

# 3.1 Coordinate Reference Systems
# ------------------------------

# A fundamental aspect of spatial data is defining its Coordinate Reference System (CRS). 
# Briefly, a CRS defines how to represent the Earth’s surface on the plane. Until we define 
# a projection, coordinates are just numbers.
# A CRS is a combination of an ellipsoid, a datum, and a projection.

# Datum
# The datum provides the information needed to “anchor” coordinates to the surface of the 
# Earth. Defines the origin point and the direction of the coordinate axes.

# Projections

# Which projection to use

# CRS in R

# not working; needs rnaturalearthhires that I cannot seem to install
# iceland <- ne_countries(returnclass = "sf", country = "Iceland", scale = 10) %>% st_geometry()

iceland <- 
  rnaturalearth::ne_countries(returnclass = "sf", country = "Iceland", scale=50) %>% st_geometry()
st_crs(iceland) # Find out the CRS

# If the metadata does not includes the CRS, or if we create our own sf object, 
# the CRS will not be defined.
mysf <- 
  data.frame(lat = c(66,65.5), lon = c(-20, -19)) %>%
  st_as_sf(coords = c("lon", "lat"))
st_crs(mysf)

# We can define it with st_crs() (or with st_set_crs() if we use it in a pipe)
mysf <- 
  data.frame(lat = c(66,65.5), lon = c(-20, -19)) %>%
  st_as_sf(coords = c("lon", "lat")) %>%
  st_set_crs(4326)
st_crs(mysf)

# Changing the projection of vector data
# In most cases we use the st_transform() function to project sf objects. 
# This function can receive the proj4 string or the EPSG code of the target projection.

europe <- 
  rnaturalearth::ne_countries (returnclass = "sf", continent = "Europe")  %>%
  st_geometry()

# Transform to the Mollweide projection
europe_moll <- europe %>% st_transform("+proj=moll")

plot(europe, axes = TRUE)
plot(europe_moll, axes = TRUE)

st_crs(europe)
st_crs(europe_moll)

# Centred in Paris
europe_laea1 <- st_transform(europe,
                             crs = "+proj=laea +lon_0=-2.35 +lat_48.8")

# Centred in Moscow
europe_laea2 <- st_transform(europe,
                             crs = "+proj=laea +lon_0=37.6 +lat_0=55.7")


plot(europe_laea1, axes = TRUE)
plot(europe_laea2, axes = TRUE)

# Projections in ggplot

coast <- read_sf("ftp://ftp.hafro.is/pub/data/shapes/iceland_coastline.gpkg")
st_crs(coast)
depth <- read_sf("ftp://ftp.hafro.is/pub/data/shapes/iceland_contours.gpkg")
st_crs(depth)

# the first geom defines the projection
ggplot() +
  geom_sf(data = coast, fill = "darkgray") +
  geom_sf(data = depth)

ggplot() +
  geom_sf(data = depth) +
  geom_sf(data = coast, fill = "darkgray")
  
ggplot() +
  geom_sf(data = depth) +
  geom_sf(data = coast, fill = "darkgray") +
  coord_sf(crs = 4087) # World Equidistant Cylindrical projection

ggplot() +
  geom_sf(data = depth) +
  geom_sf(data = coast, fill = "darkgray") +
  coord_sf(crs = 4087) # World Equidistant Cylindrical projection

ggplot() +
  geom_sf(data = depth) +
  geom_sf(data = coast, fill = "darkgray") +
  coord_sf(crs = 4326) # WGS84

# ?? How to project if there is not EPSG code like in the coast dataset?

p1 <- 
  ggplot() +
  geom_sf(data = europe) +
  ggtitle("longlat")
p2 <-
  ggplot() +
  geom_sf(data = europe_moll) +
  ggtitle("moll")
p3 <-
  ggplot() +
  geom_sf(data = europe_laea1) +
  ggtitle("Center Paris")
p4 <- 
  ggplot() +
  geom_sf(data = europe_laea2) +
  ggtitle("Center Moscow")

# 3.2 Working with geometries in sf objects
# -------------------------------------

# Remember to use projected data

# We want to get areas in m2…not square degrees!
# For geometric operations:
#   * Make sure that they are in a projected crs.
#   * Make sure that all layers have the same crs.

# sf has several functions to extract basic information from objects of class sf:
nfu <- read_sf("ftp://ftp.hafro.is/pub/data/shapes/nephrops_fu.gpkg") %>%
  st_transform(3395) # Transform to World Mercator

st_is_simple(nfu)
st_is_valid(nfu)
st_is_empty(nfu)
st_is_longlat(nfu)
st_dimension(nfu)
st_geometry_type(nfu)
st_crs(nfu)
st_bbox(nfu)

# Generating new geometries

# 1. Sampling

# The function st_sample() can be used to sample locations from polygons or linestrings. The result is an sfc object with the sampled POINT geometries.

set.seed(100)

# sample within polygons
pts1 <- st_sample(nfu, size = 100, type = "random")

# cast to lines only and sample from lines
nfu_lines <- st_cast(nfu, "LINESTRING")
pts2 <- st_sample(nfu_lines, size = 100, type = "random")

# plot samples from polygons and lines
ggplot() +
  geom_sf(data = nfu) +
  geom_sf(data = pts1, color = "red") +
  geom_sf(data = pts2, color = "blue")

# plot from lines only
ggplot() +
  geom_sf(data = nfu_lines) +
  geom_sf(data = pts2, color = "blue")

# 2. Centroids
# The mean position of geometries (LINESTRING OR POLYGONS) can be obtained by the st_centroid() function.

centr <- st_centroid(nfu)
centr2 <- st_point_on_surface(nfu) # Not exactly the centroid, but always in the polygon

ggplot() +
  geom_sf(data = nfu) +
  geom_sf(data = centr, color = "red") +
  geom_sf(data = centr2, color = "blue")

# 3. Buffers
# The function st_buffer() can be used for points, linestrings or polygons.

buf <- st_buffer(pts1, dist = 100000) # 100 km buffer
ggplot() +
  geom_sf(data = buf, linetype = "dashed") +
  geom_sf(data = pts1)

st_crs(nfu)
st_crs(nfu_lines)
st_crs(europe)


# plotting a buffer around the nephrops areas and including the map of europe (cut out)
buf <- st_buffer(nfu_lines, dist = 50000) # note: the distance is in units fitting with the CRS
ggplot() +
  theme(legend.position="none") +
  geom_sf(data = europe) +
  geom_sf(data = buf, aes(fill = name)) +
  geom_sf(data = nfu, colour="black", fill=NA) +
  coord_sf(crs = 4326, xlim = c(-20, 10), ylim = c(40, 62), expand = FALSE)

# convert NFU lines to a CRS in meters; so that we know what distance we are drawing
nfu_lines2 <- st_transform(nfu_lines, crs=7801)
buf2       <- st_buffer(nfu_lines2, dist = 5000) 
ggplot() +
  theme(legend.position="none") +
  geom_sf(data = europe) +
  geom_sf(data = buf2, aes(fill = name)) +
  # geom_sf(data = nfu_lines2, colour="black", fill=NA) +
  coord_sf(crs = 4326, xlim = c(-20, 10), ylim = c(40, 62), expand = FALSE)

# plotting buffer around the NFU polygons
buf <- st_buffer(nfu, dist = 20000)
buf <- st_buffer(nfu, dist = -20000)
ggplot() +
  theme(legend.position="none") +
  geom_sf(data = buf, aes(fill = name)) +
  geom_sf(data = nfu, colour="black", fill=NA) 
  
# Convex hull 
# A convex hull is the smallest convex polygon that includes a group of points. 
# We can use the st_convex_hull() function to get it, but first we need to join the POINT geometries 
# into a single MULTIPOINT geometry. Otherwise we get a “convex hull” for each individual point.

pts1_u <- st_union(pts1)
chull  <- st_convex_hull(pts1_u)

ggplot() +
  geom_sf(data = chull) +
  geom_sf(data = pts1)

# Concave hulls
# Concave hulls are similar to convex hull, but they are concave (duh!). 
# To compute a concave hull we need the package concaveman Note that the concaveman takes an sf object, 
# but it does not accept an sfc object. (i.e. a geometry)

library(concaveman)
conc1 <- concaveman(st_as_sf(pts1), concavity = 1)   # very narrow concave fitting
conc2 <- concaveman(st_as_sf(pts1), concavity = 2.5) # more loose concave fitting
conc3 <- concaveman(st_as_sf(pts1), concavity = 100) # like convex hull

ggplot() +
  geom_sf(data = conc1, color = "red", fill= NA) +
  geom_sf(data = pts1)

ggplot() +
  geom_sf(data = conc2, color = "blue", fill = NA) +
  geom_sf(data = pts1)

ggplot() +
  geom_sf(data = conc3, color = "green", fill = NA) +
  geom_sf(data = pts1)

# Grids
# The function st_grid provides rectangular or hexagonal grids. You can get polygons, nodes or centroids.

sq_grid  <- st_make_grid(nfu, n = c(10, 15))
hex_grid <- st_make_grid(nfu, n = c(10, 15), square = FALSE)

ggplot() +
  geom_sf(data = sq_grid, fill = NA, color = "blue") +
  theme_bw()

ggplot() +
  geom_sf(data = hex_grid, fill = NA, color = "red") +
  theme_bw()

# Simplification
# Sometimes it is necessary to simplify vector data, reducing the memory, 
# disk space and bandwidth they require, and speeding up geometrical operations. 
# For this we use the st_simplify() function.

helcom <- read_sf("ftp://ftp.hafro.is/pub/data/shapes/helcom.gpkg") %>%
  st_transform(3395)

# simplify with tolerance 
helcom_simple <- st_simplify(helcom, dTolerance = 5000)
helcom_simpler <- st_simplify(helcom, dTolerance = 10000)
helcom_simplest <- st_simplify(helcom, dTolerance = 20000)

object.size(helcom)
object.size(helcom_simple)
object.size(helcom_simpler)
object.size(helcom_simplest)

ggplot() + 
  geom_sf(data = helcom, colour="blue") +
  geom_sf(data = helcom_simple, colour="red", alpha=0.5, fill=NA) +
  geom_sf(data = helcom_simpler, colour="green", alpha=0.5, fill=NA) +
  geom_sf(data = helcom_simplest, colour="yellow", alpha=0.5, fill=NA) 
  

# Measurements
# --------------------

# Areas and lengths

# The sf package provides a series of unary measures that return a single values describing 
# some properties of geometries.
st_area(nfu)
st_length(nfu_lines)

# Distance betrween objects

# The function st_distance() returns a dense numeric matrix with distances between geometries.

set.seed(100)
poly <- nfu[1:3, ]

pts_a <- 
  st_sample(poly, 5) %>%
  st_as_sf()%>%
  mutate(lab = 1:5)

pts_b <- 
  st_sample(poly, 2) %>%
  st_as_sf()%>%
  mutate(lab = c("a","b"))

st_distance(pts_a, pts_b)

ggplot() +
  geom_sf(data = poly, aes(fill = name)) +
  geom_sf(data = pts_a) +
  geom_sf(data = pts_b, colour="red") +
  geom_sf_text(data = pts_a,
               nudge_x = 20000,
               aes(label = lab)) +
  geom_sf_text(data = pts_b,
               nudge_x = 20000,
               aes(label = lab))

# Aggregation

#Aggregation involves summarising a dataset by a grouping variables, usually one of the attribute columns. 
# The sf package provides methods for stats::aggregate and dplyr::summarise. When used, they:
# take a grouping predicate (e.g. from group_by())
# take one or more aggregation functions
# aggregate the attributes per group
# aggregate the geometries
# if do_union==TRUE, union the aggregated geometries.

vms <- read_csv("ftp://ftp.hafro.is/pub/data/csv/small_vms.csv") %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

# multipoint
vms_tr <- 
  vms %>%
  group_by(id) %>%
  summarise(n = n()) 

# this only plots the geometry
plot(vms_tr %>% st_geometry())

# linestring
vms_gr <- 
  vms %>%
  group_by(id) %>%
  summarise(n = n()) %>% 
  st_cast("LINESTRING")

plot(vms_gr %>% st_geometry())

# Let’s aggregate the ICES ecoregions into our three groups:
  
ices_er <- 
  read_sf("ftp://ftp.hafro.is/pub/data/shapes/ices_ecoregions.gpkg") %>%
  mutate(zone = c(1, 2, 2, 3, 3, 3, 3, 3, 2, 2, 2, 1, 1, 1, 1, 1, 2)) # Add a grouping variable: 1=Arctic & subarctic, 2=North Atlantic, 3=Mediterranean & Black Sea

ices_er_zones <- 
  ices_er %>%
  group_by(zone) %>%
  summarize(area = sum(area_km2))

ggplot() +
  geom_sf(data = ices_er_zones, aes(fill = as.character(zone)))

# We can use rbind to join spatial objects, but the column names and coordinate reference system need to be the same.

sm_ices_er <- 
  ices_er %>%
  filter(area_km2 < 100) %>% 
  mutate(size="small")

lg_ices_er <- 
  ices_er %>%
  filter(area_km2 >= 100) %>% 
  mutate(size="large")

all_ices_er <- rbind(sm_ices_er, lg_ices_er)

ggplot() +
  geom_sf(data = all_ices_er, aes(fill = as.character(size)))

# Relationship between attributes and geometry

# Most sf objects contain attributes and geometry. Because geometric operations do not change existing attributes, 
# we need to be careful about if the attributes make sense in terms of the new geometries.
# We need to distinguish between three attributegeometry relationships (AGR)
# 1. Constant attributes. They are valid everywhere within a geometry. Examples: bottom type, depth class.
# 2. Aggregate attributes. They are a summary value over the geometry. Examples: total abundance, mean abundance.
# 3. Identity attributes. They identify uniquely the entire geometry. Example: EEZs, ICES ecoregions. 
#    A sample of this geometry has not the identity property anymore. It becomes a constant attribute.

# Check the AGR like this:
st_agr(ices_er) 

# set the AGR
st_agr(ices_er) <- c(ecoregion = "identity",
                     area_km2 = "aggregate",
                     zone = "constant")
st_agr(ices_er) 

# 3.3 Reproducible document writing in R - skipped

# 3.4 Spatial operations

# Nephrops data from DATRAS

## get station data
survey <- read_csv("ftp://ftp.hafro.is/pub/data/csv/datras_2018_haul.csv",
                   guess_max = 2000) %>%
  filter(year == 2018) %>%
  dplyr::select(id,
                survey,
                lat = shootlat, # Get starting point
                lon = shootlong)

## get length data
nephrops <- 
  read_csv("ftp://ftp.hafro.is/pub/data/csv/datras_2018_length.csv") %>%
  filter(latin == "Nephrops norvegicus") %>%
  left_join(survey, by = "id") %>%
  group_by(id) %>%
  summarise(survey = first(survey),
            lat = first(lat),
            lon = first(lon),
            num = sum(n)) %>%
  ungroup()

# convert to sf object

nephrops <- 
  nephrops %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

# load nephrops functional units
nfu <- read_sf("ftp://ftp.hafro.is/pub/data/shapes/nephrops_fu.gpkg")

# They have the same CRS
st_crs(nephrops) ; st_crs(nfu)

# plot the data
ggplot() +
  geom_sf(data = nfu, aes(fill = name), alpha = 0.5) +
  geom_sf(data = nephrops, size = 0.3)

# Binary predicates are operations that test the topological relationship between two geometries, 
# resulting in a TRUE or FALSE for every combination of features in the two sf objects used as input.

# Which functional units contain observations?
test1 <- 
  st_within(nephrops, nfu) %>% 
  as.data.frame() %>% 
  rename(rowid = row.id, fu=col.id)

# link the test to the data
nephrops2 <-
  nephrops %>% 
  rowid_to_column() %>% 
  left_join(test1) %>% 
  mutate(where = ifelse(is.na(fu), "out","in"))

# plot the data with separate colours for inside and outside of FUs
ggplot() +
  geom_sf(data = nfu, aes(fill = name), alpha = 0.5) +
  # geom_sf(data = nephrops, size = 0.3) +
  geom_sf(data = nephrops2, aes(colour=where), size = 0.7)

# Spatial subsetting and binary predicates

# We can subset a sf object by asking if they relate or not in some way to another sf object. 
# In other words, we can do a spatial subsetting.

nfu_kattegat<- 
  nfu %>% 
  filter(name == "Kattegat")

# Within the Kattegat
nephrops_kattegat <- 
  nephrops %>%
  filter(st_within(x = ., y = nfu_kattegat, sparse = FALSE))

# plot only observations in the kattegat
ggplot() +
  geom_sf(data = nfu, aes(fill = name), alpha = 0.5) +
  geom_sf(data = nephrops_kattegat, size = 0.3) +
  theme(legend.position = "none")

# Outside the Kattegat
nephrops_no_kattegat <- 
  nephrops %>%
  filter(st_disjoint(x = ., y = nfu_kattegat, sparse = FALSE))

ggplot() +
  geom_sf(data = nfu, aes(fill = name), alpha = 0.5) +
  geom_sf(data = nephrops_no_kattegat, size = 0.3) +
  theme(legend.position = "none")

# At least 200 km from Kattegat
distance <- set_units(200, km)

# calculate (NOTE: requires LWGEOM package)
nephrops_near_kattegat <- 
  nephrops %>%
  filter(st_is_within_distance(x = ., y = nfu_kattegat, sparse = FALSE, dist = distance))

ggplot() +
  geom_sf(data = nfu, aes(fill = name), alpha = 0.5) +
  geom_sf(data = nephrops_near_kattegat, size = 0.3) +
  theme(legend.position = "none")

# Spatial (and non-spatial) joins

nephrops <- 
  nephrops %>%
  st_join(nfu %>% dplyr::select(nfu = name),
          join = st_intersects)

ggplot() +
  geom_sf(data = nephrops, aes(color = nfu), size = 0.3) +
  theme(legend.position = "none")

# Spatial join with polygons

# ecoregions
ier <- read_sf("ftp://ftp.hafro.is/pub/data/shapes/ices_ecoregions.gpkg") %>%
  dplyr::select(ecoregion)  # 17 polygons

# ospar
ospar <- read_sf("ftp://ftp.hafro.is/pub/data/shapes/ospar.gpkg") %>%
  dplyr::select(subregion)  # 50 polygons

ospar_ier <- st_join(ospar, ier) %>%
  mutate(label = str_c(subregion,"-", ecoregion))

ggplot() +
  geom_sf(data = ospar, col = "red", fill = NA) +
  geom_sf(data = ier, col = "blue", fill = NA)

ggplot() + geom_sf(data = ospar_ier, aes(fill = subregion))
ggplot() + geom_sf(data = ospar_ier, aes(fill = label)) + theme(legend.position = "none") + facet_wrap(~ecoregion)

# --------------------------------------------------------------------------------------
# Day 4 - Thursday (09:00 - 17:00)
# Rasters (JB)
# Interactive maps - leaflet (EH)
# --------------------------------------------------------------------------------------

# --------------------------------------------------------------------------------------
# Day 5 - Friday (09:00 - 13:00)
# Interpolation (JB)
# Little more on ggplot2 (EH)
# --------------------------------------------------------------------------------------









