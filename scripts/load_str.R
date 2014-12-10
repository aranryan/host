require("stringr")
require("plyr") #Hadley said if you load plyr first it should be fine
require("dplyr")
require("reshape2")
require("ggplot2")
require("zoo")
require("xts")
require("lubridate")
require("seasonal")
  Sys.setenv(X13_PATH = "C:/Aran Installed/x13as")
  checkX13()
require("forecast")
require("car")
require("tidyr")

########
#
# load STR data
#


wb = loadWorkbook(filename)
data1 = readWorksheet(wb,sheet1,...)
data2 = readWorksheet(wb,sheet2,...)




# first read in the first two rows, and grab the names of the markets and 
# segments from the first row
first2 <- read.csv("input_data/str_us_top25.csv", header = FALSE, sep = ",", 
                   nrows=2, stringsAsFactors=FALSE, na.strings=c("", "NA"))
row1 <- first2[1,2:ncol(first2)]
# uses apply to remove columns that are NA
row1 <- row1[, !apply(is.na(row1), 2, all)]

# reference file to use in converting market names to shorter abbreviations
str_geoseg <- read.csv("reference/str_geoseg.csv", header = TRUE, sep = ",")

# this is a function that I defined in functions script
# it takes an input dataset, then vector of old values, then a vector of 
# new values and it replaces the old with the new, converting any factors to 
# text along the way
row1 <- recoder_func(row1, str_geoseg$str_long, str_geoseg$str_geoseg)

# repeats each element 3 times, returning a vector
row1 <- rep(row1,each=3)
row1 <- unlist(row1)

# then create row2
row2 <- first2[2,2:ncol(first2)]

# use car::recode for simple recoding
row2 <- recode(row2, '"Supply" = "supt"; "Demand" = "demt"; 
               "Revenue" = "rmrevt"')
row2 <- unlist(row2)
series_names <- paste(row1, row2, sep="_")
series_names <- c("date", series_names)
head(series_names)

# now read in the table without headers
lodus_m <- read.csv("input_data/str_us_top25.csv", header = FALSE, sep = ",", skip = 2,
                    na.strings = c("NA", ""), stringsAsFactors=FALSE)
# in the source file there is a footnote that happens at first and second column,
# row 11,000 or so
# this removes rows that are NA in the third column
# effectively it takes those rows that are not NA and keeps them
# based on 
# https://heuristically.wordpress.com/2009/10/08/delete-rows-from-r-data-frame/
lodus_m <- lodus_m[!is.na(lodus_m[,3]),]

# still have the issue that there are commas in the data, this removes them
col2cvt <- 2:ncol(lodus_m) # columns from 2 to the end
# runs a function to remove commas and convert to numeric
lodus_m[,col2cvt] <- lapply(lodus_m[,col2cvt],function(x){as.numeric(gsub(",", "", x))})

# names the columns and formats first column as dates
colnames(lodus_m) <- series_names
# as.yearmon is a format for monthly dates, then I take as.Date of that
lodus_m$date <- as.Date(as.yearmon(format(lodus_m$date, nsmall =2), "%Y%m"))

# takes the summed data and spreads it into a tidy format with
# tidyr and then calculates the occupancy and revpar series
# first needs to go from xts to dataframe
b1 <- data.frame(date=time(lodus_m), lodus_m) %>% 
  # creates column called segvar that contains the column names, and one next to 
  # it with the values, dropping the time column
  gather(segvar, value, -date, na.rm = FALSE) %>%
  # in the following the ^ means anything not in the list
  # with the list being all characters and numbers
  # so it separates segvar into two colums using sep
  separate(segvar, c("seg", "variable"), sep = "[^[:alnum:]]+") %>%
  # keeps seg as a column and spreads variable into multiple columns containing
  # the values
  spread(variable,value) %>%
  # days_in_month is a function I borrowed. leap_impact=0 ignores leap year
  # this uses transform to create a new column where the new column is
  # created by using sapply on the date column to apply the days_in_month
  # function with the leap_impact argument set to 0
  transform(days = sapply(date, days_in_month,leap_impact=0)) %>%
  # adds several new calculated columns
  mutate(occ = demt / supt) %>%
  mutate(revpar = rmrevt / supt) %>%
  mutate(adr = rmrevt / demt) %>%
  # converts several concepts to millions
  mutate(supt = supt / 1000000) %>%
  mutate(demt = demt / 1000000) %>%
  mutate(rmrevt = rmrevt / 1000000) %>%
  mutate(demd = demt / days) %>%
  mutate(supd = supt / days) 
  
head(b1)
str(b1)

lodus_m <- b1

lodus_m <- lodus_m %>%
  read.zoo() %>%
  as.xts

#############################
#
# creates quarterly by summing monthly
#

# get it ready to convert
# takes it from a tidy format and melts it creating a dataframe with the
# following columns (date, seg, variable, value), and then creates the unique
# variable names and then reads into a zoo object spliting on the 
# second column
a_mz <- lodus_m %>%
 select(-occ, -adr, -revpar, -demd, -supd) %>%
 melt(id=c("date","seg"), na.rm=FALSE) %>%
 mutate(variable = paste(seg, "_", variable, sep='')) %>%
 select(-seg) %>%
 read.zoo(split = 2) 

# convert to quarterly

# I couldn't use apply because the object is 
# a xts, not a dataframe, see 
# http://codereview.stackexchange.com/questions/39180/best-way-to-apply-across-an-xts-object

# sets up the start of the index that will be used for the quarterly object
# uses vapply to essentially run an apply across the xts object because
# apply doesn't work on an xts object
# for vapply we need to give the expected length in FUN.VALUE and a
# start date and quarterly frequency
# The function that I'm applying to each column is m_to_q, which I wrote, the type="sum"
# is giving the type of aggregation to use in it
start <- as.yearqtr((start(a_mz)))
lodus_q <- zooreg(vapply(a_mz, m_to_q, FUN.VALUE = 
                           numeric(floor(nrow(a_mz)/3)), 
                         type="sum"), start=start, frequency=4)
head(lodus_q)

