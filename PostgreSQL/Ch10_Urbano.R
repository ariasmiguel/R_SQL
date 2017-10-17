### RPostgreSQL -------------------------------------------------------------------------
# Using R for spatial data analysis.

# Packages ----------------------------------------------------------------------------
library("adehabitatLT")
library("adehabitatHR")
library("adehabitatHS")
library("adehabitatMA")
library(RPostgreSQL)

# Before using the database to analyse your animal tracking data with R, you can ------
# use adehabitat to replicate Fig. 10.1:
set.seed(0) # this allows you to replicate
# The exact same 'random' numbers as we did for the figure
simdat <- simm.crw(c(1:501))[[1]]
plot(simdat$x, simdat$y, type = "l", lty = "dashed",
     xlab = "x", ylab = "y", asp = T)
lines(simdat$x[seq(1, 501, by =10)], simdat$y[seq(1, 501, by = 10)], lwd = 2)
lines(simdat$x[seq(1, 501, by = 100)], simdat$y[seq(1, 501, by = 100)], lwd = 4,
      col = "grey")

# Connect to the database -----------------------------------------------------------
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname="gps_tracking_db", host="localhost",
                 port="5432", user="postgres", password="Lennux10")
dbListTables(con)

# The following code retrieves the first five lines from the gps_data_animals table:
fetch(dbSendQuery(con, "SELECT * FROM main.gps_data_animals LIMIT 5;"), -1)

# You can see that R did not undesrtand geom column correctly.
rs <- dbSendQuery(con, "SELECT animals_id, acquisition_Time, longitude, latitude,
                  ST_X(ST_Transform(geom, 32632)) AS x32,
                  ST_Y(ST_Transform(geom, 32632)) AS y32, roads_dist,
                  ndvi_modis, corine_land_cover_code, altitude_srtm
                  FROM main.gps_data_animals where gps_validity_code = 1;")
locs <- fetch(rs, -1)
dbClearResult(rs)

# The -1 indicates that you want all data, and then 'clear' the result ser.
head(locs)
head(locs$acquisition_time)

# Use lubridate to inspect the time elements of acquisition_time.
library(lubridate)
table(month(locs$acquisition_time), hour(locs$acquisition_time))

# To prevent mistakes due to daylight saving time, it is much easier to work with
# UTC time. Thus you have to convert the dates back to UTC time.

locs$UTC_time <- with_tz(locs$acquisition_time, tz = "UTC")
head(locs$acquisition_time)
head(locs$UTC_time)

# Data inspection and exploration ---------------------------------------------------
# The main strength of R, lies in its visualisation capabilities. The visualisation
# of different aspects of the data is one of the major tasks during an exploratory
# analysis.

ltrj <- as.ltraj(locs[, c("x32", "y32")], locs$UTC_time, locs$animals_id)
class(ltrj)
class(ltrj[[1]])
head(ltrj[[1]])

# The head function shows us that the data.frames within an ltraj object have ten 
# columns. The first three columns define the location of the animal: the x and y
# coordinate and its date. The following cols describe the step (or change in
# location) toward the next location.

id(ltrj)
id(ltrj[1])

# The summary function gives some basic information on the ltraj-object:
(sumy <- summary(ltrj))

# To ensure homogeneous data for the following analyses, you will only keep
# animals that have a complete year of data (number of dats >= 365):
(duration <- difftime(sumy$date.end, sumy$date.begin, units = "days"))

ltrj <- ltrj[duration >= 365]

# For animals that were tracked for a longer period than one year, you remove
# locations in excess. Of course, if your ecological question is addressing space
# use during another period (e.g. spring) than you would want to keep all animals
# that provide this info, and Animal 6 may be retained for analysis, while
# removing all locations that are not required for analysis.

ltrj <- cutltraj(ltrj, "difftime(date, date[1], units='days')>365")
summary(ltrj)

# Now, all animals are tracked for a whole year.
# With the plot function, you can show the different trajectories:
par(mfrow = c(1,2))
plot(ltrj[1])
plot(ltrj[2])

# The plotltr function allows us to show other characteristics than the spatial
# representation of the trajectory. For instance, it is very useful to get an 
# overview of the sampling interval of the data. We discussed before how the
# sampling interval has a large effect on the patterns that you can observe
# in the data.
plotltr(ltrj[1], which = "dt/60/60", ylim = c(0, 24))
abline(h = seq(4, 24, by = 4), col = "grey")

# Function to remove locations that are not part of a predefined sampling regime:
removeOutside <- function(x, date.ref, dt_hours = 1, tol_mins = 5){
  require(adehabitatLT)
  x <- ld(x)
  tmp <- x$date + tol_mins * 60
  tmp_h <- as.POSIXlt(tmp)$hour
  tmp_m <- as.POSIXlt(tmp)$min
  hrs <- as.POSIXlt(date.ref)$hour
  hrs <- seq(hrs, 24, by = dt_hours)
  x <- x[tmp_h %in% hrs & tmp_m < (2 * tol_mins), ]
  x <- dl(x)
  return(x)
}

# Use this function on the ltraj; you specify a reference date at midnight,
# and the expected time lags in hours (dt_hours) is 4, and you set a tolerance of
# +- 3 min (tol_mins):
ltrj <- removeOutside(ltrj, dt_hours = 4, tol_mins = 3,
                      date.ref=as.POSIXct("2005-01-01 00:00:00", tz="UTC"))
plotltr(ltrj[1], which = "dt/60/60", ylim = c(0, 24))
abline(h = seq(4, 24, by =4), col = "grey")

# Now there are no longer observations that deviate from the 4-hour schedule we
# programmed in our GPS sensors, i.e. all time lags between consecutive locations
# are a multiple of four. You still see gaps in the data, i.e. some gaps are larger
# than 4h. Thus, there are missing values. The summary did not show the presence
# of missing data in the trajectory; you therefore have to specify the occurence
# of missing locations.

# The setNA function allows us to place the missing values into the trajectory at
# places where the GPS was expected to obtain a fix but failed to do so. You have
# to indicate a GPS schedule, which is in your case 4 h:
ltrj <- setNA(ltrj, as.POSIXct("2004-01-01 00:00:00", tz = "UTC"), dt=4*60*60)
summary(ltrj)

# Now you see that the trajectories do contain a fair number of missing locations.
# If locations are missing randomly, it will not bias the results of an analysis.
# However, when missing values occur in runs, this may affect your results. In 
# adehabitat, there are two figures to inspect patterns in the missing data. The 
# function plotNAltraj shows for a trajectory where the missing values occur and
# can be very instructive to show important gaps in the data:

da <- ltrj[[1]]$date
# The vector with dates allows us to zoom in on a range of dates in the plot
plotNAltraj(ltrj[1], xlim = c(da[1], da[500]))

# Missing values in November and December are not likely to occur independent of
# each other. Can test this with the runsNAltraj function whether there is statistical
# significant clustering of the missing values:
runsNAltraj(ltrj[1]) # indeed, as the figures showed, it is not random

# It is known that for a GPS receiver, it is more difficult to contact the satellites
# within dense forests, and so when an animla is in such a forest at time t, it is 
# more likely to be still in this forest at time t + 1 than at time t + 2, thus 
# causing temporal dependence in the fix-acquisition probability. Unfortunately,
# as said, this temporal dependence in the 'missingness' of locations holds the risk
# of introducing biases in the results of your analysis.
# Visual inspection of figures like Fig. 10.6 can help the assessment of whether
# the temporal dependence in missing locations will have large effects on the analysis.
# Other figures can also help this assessment. For instance, you can plot the number
# of missing locations for each hour of the day, or for periods of the year:

par(mfrow=c(1, 2))
plot(c(0, 4, 8, 12, 16, 20),
     tapply(ltrj[[1]]$x, as.POSIXlt(ltrj[[1]]$date)$hour,
            function(x)mean(is.na(x))),
      xlab="hour", ylab="prop. missing", type="o", ylim=c(0,1), main="a")
periods <- trunc(as.POSIXlt(ltrj[[1]]$date)$yday/10)
plot(tapply(ltrj[[1]]$x, periods, function(x)mean(is.na(x))),
     xlab="period", ylab="prop. missing", type="o", ylim=c(0,1),
     main ="b")

# Expect bias on the results to be minimal.
# The NSD is a commonly used metric for animal space use; it is the squared 
# straight-line distance between each point and the first point of the
# trajectory. It is a very useful metric to assess, for instance, the occurrence
# of seasonal migration patterns. An animal that migrates between summer and winter
# ranges will often show a characteristic hump shape in the NSD.
plotltr(ltrj, which = "R2n")

# The NSD proflies strongly suggest the occurence of seasonal migration in 
# all five animals. During the summer the animals seem to be in one area
# and during the winder in another. The NSD is a one-dimensional representation
# of the animal's space use, which facilitates the inspection of it against the
# second dimension of time. On the other hand, it removes information present
# in two-dimensional locations provided by the GPS.
# As an alternative for the NSD, you can plot both spatial dimensions against time:
par(mfrow=c(1,2))
plot(ltrj[[1]]$date, ltrj[[1]]$x, pch=19, type="o", xlab="Time",
     ylab="x", main="a")
plot(ltrj[[1]]$date, ltrj[[1]]$y, pch=19, type="o", xlab="Time",
     ylab="y", main="b")

# To avoid the intrinsic reduction of information by collapsing two dimensions
# into one single dimension, you also plot both spatial dimensions and use colour
# to depict the temporal dimension:
ltrj2 <- na.omit.ltraj(ltrj)
par(mfrow=c(3,2))
for(i in c(1:5)){
  plot(ltrj2[[i]][c("x","y")], col=rainbow(12)[as.POSIXlt(ltrj2[[i]]$date)$mon+1],
       pch=19, asp=T)
  segments(ltrj2[[i]]$x[-nrow(ltrj[[i]])], ltrj2[[i]]$y[-nrow(ltrj2[[i]])],
           ltrj2[[i]]$x[-1],ltrj2[[i]]$y[-1],
           col=rainbow(12)[as.POSIXlt(ltrj2[[i]]$date[-1])$mon+1])
  title(c("a","b","c","d","e")[i])
}
plot(c(0:100), c(0:100), type ="n", xaxt="n", yaxt="n", xlab="", ylab="", bty="n")
mon <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",
         "Oct", "Nov", "Dec")
legend(x=20, y=90, legend=mon[c(0:5)+1], pch=19, col=rainbow(12)[1:6], bty="n")
legend(x=60, y=90, legend=mon[c(6:11)+1], pch=19, col=rainbow(12)[7:12], bty="n")

# Now that you have familiarised yourselves with the structure of the data and
# have ensured that your data are appropriate for the analysis, you can proceed with
# answering your ecological questions in the following sections.

# Home Range Estimation -----------------------------------------------------------
# Home range is the area in which an animal lives. It has been quantified by the 
# concept of the 'utilization distribution (UD)'. The most common estimator for the
# UD is the kernel density estimator. 
# The kernelUD function calculates kernel utilization density from 2D locations:

library(adehabitatHR)
library(sp)
trj <- ld(ltrj)
trj <- trj[!is.na(trj$x),]
(kUD <- kernelUD(SpatialPointsDataFrame(trj[c("x", "y")],
                  data = data.frame(id=trj$id)), h=100, grid=200, kern="epa"))
image(kUD[[1]])

# During our data exploration, we found that our roe deer occupy a separate
# summer and winter range: Are both ranges of similar size? For this, you have
# to compute the home range separetly for summer and winder. 

trj$yday <- yday(trj$date)
trj$season <- ifelse(trj$yday>150 & trj$yday<300, "summer", NA)
trj$season <- ifelse(trj$yday>350 | trj$yday<100, "winter", trj$season)

trj <- trj[!is.na(trj$season),]
(kUD <- kernelUD(SpatialPointsDataFrame(trj[c("x","y")],
                                        data=data.frame(id=paste(trj$id, trj$season))),
                 h=100, grid=200, kern="epa"))
area <- kernel.area(kUD, percent = c(50, 75, 95), unin="m", unout="ha")
(areas <- data.frame(A50=unlist(area[1,]), A75=unlist(area[2,]),
                     A95=unlist(area[3,]), id=rep(c(1:5), each=2),
                     seas=rep(c("S", "W"), 5)))

# Can visualise these results clearly with boxplots:
par(mfrow = c(1, 3))
boxplot(areas$A50 ~ areas$seas, cex = 2, main = "a")
boxplot(areas$A75 ~ areas$seas, cex = 2, main = "b")
boxplot(areas$A95 ~ areas$seas, cex = 2, main = "c")

# Habitat Use and Habitat Selection Analysis -------------------------------------
# Seasonal migrations are often triggered by changes in the environment. You can
# then wonder which environmental conditions are changing when the animal moves
# between its seasonal ranges. For instance, snowfall is a driver for many migratory
# ungulates in northern and alpine environments, and winter ranges are often
# characterised by less snow cover than the summer ranges in winter.

# If roe deer move to lower altitudes during winter, then they probably also move
# closer to roads, which are usually found in valley bottoms. From an applied 
# perspective, it is thus an interesting question to see whether there is a seasonal
# movement closer to roads, which could partly explain seasonal patterns often
# observed in ungulate vehicle collisions.

# First add the environment data to your inspected trajectory with the merge
# function:

trj <- ld(ltrj)
trj <- merge(trj, locs, by.x=c("id", "date"),
             by.y=c("animals_id","UTC_time"), all.x=T)

# You inspect then whether there is a relationship between the distance to the
# roads and the altitude:
library(lattice)
xyplot(roads_dist ~ altitude_srtm| factor(id),
       xlab = "altitude", ylab = "dist. road", col = 1,
       strip = function(bg = "white", ...) strip.default(bg = "white", ...),
       data = trj)

# There's an interesting relationship between altitude and distance to roads.
# Each individual shows two clusters, which are possibly corresponding with the
# two seasonal ranges you detected before. Within each cluster, there is a positive
# relationship between altitude and distance to roads; i.e. at higher altitudes,
# the distance to raods is greater. However, when you compare both clusters, it seems
# that the cluster at higher altitude is often also closer to roads (except for
# Animal 1). Overall, it seems that in your data, there is no obvious positive
# relationship between altitude and distance to roads.

xyplot(altitude_srtm ~ as.POSIXlt(acquisition_time)$yday | factor(id),
       xlab = "day of year", ylab="altitude", col = 1,
       strip = function(bg = "white",...) strip.default(bg = "white", ...),
       data = trj)

# There are marked seasonal changes in the altitude of the roe deer positions. 
# As you expected, roe deer are at lower altitudes during the winter than they are
# during the summer. This pattern explains the occurrence of the two clusters of
# points for each individual.
# Can now proceed by testing the statistical significance of these resuls. You
# use the same seasonal cutoff points as before:

trj$yday <- yday(trj$date)
trj$season <- ifelse(trj$yday > 150 & trj$yday < 300, "summer", NA)
trj$season <- ifelse(trj$yday > 350 | trj$yday < 100, "winter", trj$season)
trj2 <- trj[!is.na(trj$season), ]
fit <- lm(altitude_srtm ~ as.factor(season), data = trj2)

summary(fit)

# In winter, the roe deer are on average 572m lower than during the summer, which is
# a 33% decrease.

# The available locations are commonly sampled randomly at two scales: within the
# study area to study home range replacement, or within the individual home range
# to study home range use. We will demonstrate third-order habitat selection and use
# a minimum convex polygon (MCP) to characterise the area available for each roe
# deer, from which we sample 2,000 random locations. The mcp-function in R requires,
# the use of a SpatialPointsDataFrame when using multiple animals:
trj2 <- na.omit(trj[,c("id", "x", "y")])
sptrj <- SpatialPointsDataFrame(SpatialPoints(trj2[,c("x","y")]),
                                data=data.frame(id=trj2$id))
ranges <- mcp(sptrj, percent = 100)

# Now sample for each individual randomly from its available range, and you
# place the coordinates from these sampled locations together with the animal's
# id in a data.frame. Operations like this, in which something needs to be 
# repeated for a number of individuals, are easily performed using the list format,
# which is also the reason that 'adehabitatLT' uses a list to store trajectories.
# However, a database works with data.frames; therefore, you will have to bind
# the data.frames in the list together in one large data.frame
set.seed(0) # to ensure replication of the same randomly sampled points
rndpts <- lapply(c(1:5),
                 function(x, ranges){spsample(ranges[ranges@data$id==x,], n=2000,
                                              type="random", iter=100)}, ranges=ranges)
rndpts <- lapply(c(1:5),function(x,rndpts){data.frame(rndpts[[x]]@coords,id=x)},
  rndpts=rndpts)
rndpts <- do.call("rbind", rndpts)

plot(ranges)
points(rndpts[c(1:100), c("x", "y")], pch =16) # shows the first 100 random points

# The easiest way to obtain the environmental data for these random points is to
# simply upload them into the database and extract the required information from
# there. To facilitate the ordering of your random locations, you add a column nb
# numbered 1 to the number of random points. The function dbWriteTable writes a 
# table to the database; you can specify a schema (analysis) in addition to the table
# name (rndpts_tmp):
rndpts$nb <- c(1:nrow(rndpts))
dbWriteTable(con, c("analysis", "rndpts_tmp"), rndpts)

# Next, you use the database to couple the locations in this table to the 
# environmental data stored in the database. The easiest way to do this is by first
# adding a geometry column for the random locations:
dbSendQuery(con,
            "ALTER TABLE analysis.rndpts_tmp ADD COLUMN geom geometry(point, 4326);")
dbSendQuery(con,
            "UPDATE analysis.rndpts_tmp
            SET geom = ST_Transform((ST_SetSRID(ST_MakePoint(x,y),23032)),4326);")

# You extract the altitude and land cover for the random locations from the rasters
# stored in the databse with the following queries (the details of these SQL
# queries were discussed earlier in the book):
altitude <- fetch(dbSendQuery(con,
      "SELECT ST_Value(srtm_dem.rast, geom) AS altitude
      FROM env_data.srtm_dem, analysis.rndpts_tmp
      WHERE ST_Intersects(srtm_dem.rast, geom) ORDER BY nb;"), -1)
landcover <- fetch(dbSendQuery(con,
      "SELECT ST_Value(corine_land_cover.rast, ST_Transform(geom,3035)) AS landcover
      FROM env_data.corine_land_cover, analysis.rndpts_tmp
      WHERE ST_Intersects(corine_land_cover.rast, ST_Transform(geom, 3035))
      ORDER BY nb;"), -1)

# Extract the distance to the closest road for the random locations from the roads
# stored in the database with the following query (this query can require a few
# minutes):
mindist <- fetch(dbSendQuery(con,
    "SELECT min(distance) as dist_roads
    FROM (SELECT nb, ST_Distance(roads.geom::geography,
          rndpts_tmp.geom::geography)::integer AS distance
          FROM env_data.roads, analysis.rndpts_tmp) AS a
    GROUP BY a.nb
    ORDER BY nb;"), -1)

# Add these environemtal data to the randomly sampled available locations
rndpts <- cbind(rndpts, altitude, landcover, mindist)
head(rndpts)

# Now that there's no need for these locations in the database you can remove the
# table:
dbRemoveTable(con, c("analysis", "rndpts_tmp"))

# To ensure proper comparison of used and available habitats, you provide the
# same levels to both data sets. Moreover, to allow our seasonal comparison, you
# also need to allocate the random locations to the summer and winter season:
trj <- na.omit(trj[,c("id", "x", "y", "roads_dist", "corine_land_cover_code",
                      "altitude_srtm", "season")])
names(trj) <- c("id", "x", "y", "dist_roads", "landcover", "altitude", "season")
trj$landcover <- factor(trj$landcover, levels = c(18, 21, 23:27, 29, 31, 32))
# Allocate the random locations to each season
rndpts$season <- c("summer", "winter")
rndpts$landcover <- factor(rndpts$landcover, levels = c(18,21,23:27,29,31,32))

# Now, compute for each individual the number of locations inside each habitat
# type:
use_win <- table(trj$id[trj$season=="winter"],
                 trj$landcover[trj$season=="winter"])
ava_win <- table(rndpts$id[rndpts$season=="winter"],
                 rndpts$landcover[rndpts$season=="winter"])
use_sum <- table(trj$id[trj$season=="summer"],
                 trj$landcover[trj$season=="summer"])
ava_sum <- table(rndpts$id[rndpts$season=="summer"],
                 rndpts$landcover[rndpts$season=="summer"])

# Determine the proportions of each class
calc.prop <- function(x){(x/sum(x))}
use_win <- t(apply(use_win, 1, calc.prop))
ava_win <- t(apply(ava_win, 1, calc.prop))
use_sum <- t(apply(use_sum, 1, calc.prop))
ava_sum <- t(apply(ava_sum, 1, calc.prop))

# To avoid division by zero, we add a small number to each element in the table
use_win <- use_win+0.01
ava_win <- ava_win+0.01
use_sum <- use_sum+0.01
ava_sum <- ava_sum+0.01

library(adehabitatHS)
sr_win <- widesIII(use_win, ava_win, avknown = FALSE, alpha = 0.05)
sr_sum <- widesIII(use_sum, ava_sum, avknown = FALSE, alpha = 0.05)

# The function widesIII computes the selection ratios for individually tracked
# animals; it also provides a number of statistical tests.
plot(c(1:10)-0.1, sr_win$wi, xaxt="n", ylab="Selection Ratio",
     xlab="Habitat type", col="blue", ylim=c(0,4))
axis(1, at=c(1:10), labels=c(18,21,23:27,29,31,32))
abline(h=1, col="dark grey")
points(c(1:10)+0.1, sr_sum$wi, col="red")
segments(c(1:10)-0.1, sr_win$ICwiupper, c(1:10)-0.1, sr_win$ICwilower, col="blue")
segments(c(1:10)+0.1, sr_sum$ICwiupper, c(1:10)+0.1, sr_sum$ICwilower, col="red")
