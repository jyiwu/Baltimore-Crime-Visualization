# # ===================================================
# Author: Jingyi Wu
# Instructor: Yufeng Huang
# Description: write a function that plots crimes 
#              incidence in Baltimore city
# Data: Baltimore crime data
# Source: https://data.baltimorecity.gov/
# ===================================================
 
# clear everything
rm(list = ls()) 

# libraries 
#   need to install.packages() these
#   let me know if installation does not work
library(maps)
library(maptools)
library(dplyr)
library(ggplot2)


# download, unzip and read the shape file
url_zip <- 'https://dl.dropboxusercontent.com/s/chyvmlrkkk4jcgb/school_distr.zip'
if(!file.exists('school_distr.zip')) download.file(url_zip, 'school_distr.zip')     # download file as zip
unzip('school_distr.zip')   # unzip in the default folder
schdstr_shp <- readShapePoly('school.shp')  # read shape file
xlim <- schdstr_shp@bbox[1,]
ylim <- schdstr_shp@bbox[2,]


#   example of how to use the shape file
#   if there are no error code reading the above, you can directly plot the map of Baltimore (lines within are school districts)
#   we'll be overlaying our plots of crime incidents on this map:
plot(schdstr_shp, axes = T)     # axes = T gives x and y axes


# ======= now let's follow instructions in the pdf file ======

# download and load the crime csv data
#   link is https://dl.dropboxusercontent.com/s/4hg5ffdds9n2nx3/baltimore_crime.csv

df = read.csv("https://dl.dropboxusercontent.com/s/4hg5ffdds9n2nx3/baltimore_crime.csv", header = TRUE, stringsAsFactors = FALSE)

# transform dates and time variables depending on what you need

date <- as.Date(df$CrimeDate,format = "%m/%d/%Y")
df$month <- as.numeric(format(date, "%m"))
df$day <- as.numeric(format(date, "%d"))
time <- as.POSIXlt(strptime(df$CrimeTime,"%H:%M:%S"))
hour <- as.numeric(format(time, "%H"))
minute <- as.numeric(format(time, "%M"))/60
df$time <- hour + minute
head(df)

# split coordinates into longitude and latitude, both as numeric
# note: no for/while/repeat loop, and no substr() function

df$longitude <- gsub(".*,|\\s|\\)","",df$Location1)
df$latitude <- gsub(",\\s.*|\\(","",df$Location1)
df <- select(df,c("Location","District","CrimeDate","month","day","CrimeTime","time","latitude","longitude","Description"))
head(df)

# generate geographic and time patterns for crimes with keyword "ASSAULT"
# note: no copy and paste of the same/similar command many times
assult <- grep("ASSAULT", df$Description)
df_assault <- data.frame(df$longitude[assult],df$latitude[assult],df$time[assult])
colnames(df_assault) <- c("longitude","latitude", "time")
head(df_assault)

par(mfrow = c(2,2))
pdf("plot.pdf")
for (i in 0:3) {
  i = i * 6
  u = i + 6
  plot(schdstr_shp, axes = T, main = paste0("hour: ", i, "-", u) )
  points(df_assault$longitude[df_assault$time > i & df_assault$time < u], 
        df_assault$latitude[df_assault$time > i & df_assault$time < u], 
        pch = 16, cex = 0.5, col=rgb(1,0,0.05,0.1))
  if (i > 24) break
} 
dev.off()


