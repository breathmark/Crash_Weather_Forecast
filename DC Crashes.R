### MGT 6203 ###
### Team 39 Project "Understanding Impacts of Weather on Auto Accidents, for D.C." ###
### Primary Data Source: Crashes in DC, from Open Data DC with data available from 1975 to 2023;
### Secondary Data Source :Washington, DC Weather Data, from NOAA with data available from 1936 to 2023 (Link) 

# Install packages

if (!require(stats)) install.packages("stats")
library("stats")
library("systemfonts")
library("ragg")
if (!require(tidyverse)) install.packages("tidyverse")
library(tidyverse)
if (!require(lubridate)) install.packages("lubridate")
library(lubridate)
if (!require(ggplot2)) install.packages("ggplot2")
library(ggplot2) 
if (!require(ggExtra))install.packages("ggExtra")
library("ggExtra")
if (!require(ggExtra))install.packages("ggpubr")
library("ggpubr")
if (!require(corrplot)) install.packages("corrplot")
library(corrplot) 
if (!require(MASS)) install.packages("MASS")
library(MASS) 
if (!require(caret)) install.packages("caret")
library(caret) 
if (!require(moments)) install.packages("moments")
library(moments) 
if(!require(dplyr)) install.packages("dplyr")
library("dplyr")

# Data exploration
data <- read.csv("/Users/sz/Desktop/OMSA/MGT6203/Project/Crashes_in_DC.csv")

# Data dimension: 289063 rows and 58 attributes
# str(data)
# dim(data)

### DATA CLEANING ###

# check missing values

# attributes MPDLATITUDE, MPDLONGTITUDE, MPDGEOX, MPDGEOY, TODATE have
# significant NA values; 
# namelist = list()
# countlist = list()
# df_na = data.frame(matrix(nrow = 58, ncol = 2))
# for (m in colnames(data)) {
#     namelist <-append(namelist, m)
#     countlist <- append(countlist, sum(is.na(data[[m]])))
# }
# 
# df_na$X1<-namelist
# df_na$X2 <-countlist
# df_na
# despite REPORTDATE has no na values, it has 1044 empty values
# data %>% filter(REPORTDATE == '') %>% nrow()
# drop rows with empty REPORTDATE values
data <- data %>% filter(REPORTDATE != '')

# WARD has 1 "UNKNOWN" and  1418 "Null" text values
# unique(data$WARD)
data %>% filter(WARD == "UNKNOWN") %>% nrow() # one row has WARD == "UNKNOWN"
data %>% filter(WARD == "Null") %>% nrow() # 1418 rows have WARD == "Null"

# remove those rows
data <-data[!data$WARD %in% c("UNKNOWN",  "Null"),]

# ROUTE ID has 11715 unknown values in text type
# data %>% filter(ROUTEID == "Route not found") %>% nrow()

# remove 11715 rows, ending with 274885 rows
data <- data %>% filter(ROUTEID != "Route not found")


# check which ID to use 
# length(unique(data$CCN))
# length(unique(data$EVENTID))
# length(unique(data$CRIMEID)) # should use CRIMEID as unique identifier

# based on attributes' definitions and missing value counts, drop the following 36 attributes:

drop_list = c("X", "Y", "OBJECTID", "CCN", "MEASURE", "OFFSET","STREETSEGID", "ROADWAYSEGID", "FROMDATE", "TODATE", "ADDRESS", "LATITUDE", "LONGITUDE",
              "XCOORD","YCOORD", "EVENTID", "UNKNOWNINJURIES_BICYCLIST","UNKNOWNINJURIES_DRIVER","UNKNOWNINJURIES_PEDESTRIAN","MAR_ADDRESS", "MAR_SCORE", 
              "TOTAL_VEHICLES", "TOTAL_BICYCLES", "TOTAL_PEDESTRIANS", "TOTAL_TAXIS", "TOTAL_GOVERNMENT", "NEARESTINTSTREETNAME", "OFFINTERSECTION", 
              "INTAPPROACHDIRECTION", "LOCATIONERROR","LASTUPDATEDATE", "MPDLATITUDE","MPDLONGITUDE", "MPDGEOX", "MPDGEOY", "UNKNOWNINJURIESPASSENGER", "MAR_ID")
df<- data[, !(names(data) %in% drop_list)]


# format date, weekday,hour, year
df$Date<- as.Date(df$REPORTDATE, "%Y/%m/%d")
df$Hour <- hour(strptime(df$REPORTDATE, "%Y/%m/%d %H:%M:%S"))
df$WeekDay <-wday(df$Date, label = TRUE, week_start = 1)
df$Month <- month(df$Date, label = TRUE)
df$Year <-year(df$Date)

# find out year range and distribution
# df%>% count(Year)

# filter data for 12 years from 2011 to 2022
df <- filter(df, Year >=2011, Year <= 2022)

### WEATHER DATA CLEANING & JOINING ###

# set up weather data
data_w <-read.csv("/Users/sz/Desktop/OMSA/MGT6203/Project/Weather.csv")
# str(data_w)
# dim(data_w) # 4748 rows and 29 attributes

# check missing values
# namelist_w = list()
# countlist_w = list()
# df_na_w = data.frame(matrix(nrow = 29, ncol = 2))
# for (m in (1:length(colnames(data_w)))) {
#   namelist_w <-append(namelist_w, colnames(data_w)[m])
#   countlist_w <- append(countlist_w, sum(is.na(data_w[[m]]))/length(data_w[[m]]))
# }
# 
# df_na_w$X1<-namelist_w
# df_na_w$X2 <-countlist_w
# df_na_w

# Based on missing values and data documentation,
# drop attributes STATION, NAME, TAVG, WESD, WT01 ~ WT22 except for WT01 (Fog), WT03 (Thunder), 
# 9 attributes remain
drop_list_w <-c("STATION", "NAME", "TAVG", "WESD", "WT02", "WT04", "WT05", "WT06",
                "WT07", "WT08", "WT09", "WT10", "WT11", "WT12","WT13","WT14",   "WT15" , "WT16",
                "WT17" ,   "WT18", "WT19", "WT20", "WT21", "WT22" )
df_w <-data_w[,!(names(data_w)%in% drop_list_w)]

# change WT01, WT03 to factor variables
df_w <-df_w %>% mutate (WT01 = ifelse(is.na(WT01), 0,1))
df_w<-df_w %>% mutate(WT03 = ifelse(is.na(WT03), 0,1))


# change date type and create Year column
df_w$Date.y <- as.Date(df_w$DATE)
df_w$Year.y <- year(as.Date(df_w$DATE))

# check date counts across years
# df_w %>% group_by(Year.y) %>% summarise(length(unique(Date.y))) # confirm date coverage is complete

### JOINING TWO DATASETS ###

# filter weather data to the same time period
df_w <- filter(df_w, Year.y >=2011, Year.y <= 2022)

# left join two data sets
df_all <- left_join(df, df_w, by = c("Date" = "Date.y"))

# removing redundant columns
df_all <- df_all[!names(df_all) %in%c("Year.y", "DATE", "REPORTDATE")]

# cleaning unused levels
df_all <- droplevels(df_all)

# CHECK COMBINED DATASET 
colnames(df_all) # attribute list
str(df_all)
nrow(df_all) #241104 rows

### DESCRIPTIVE ANALYTICS ###

# check year distribution
Yr <-ggplot(df_all, aes(Year))+ geom_bar() + scale_x_continuous(breaks = seq(2011, 2022,1)) 

# check month distribution
Mth <-ggplot(df_all, aes(Month)) + geom_bar() + xlab("Month")

# check weekday distribution
Wdy <- ggplot(df_all, aes(WeekDay)) + geom_bar() + xlab ("Week Day")

# check hour distribution
Hr <- ggplot(df_all, aes(Hour)) + geom_bar() + xlab("Hour")

ggarrange(Yr, Mth, Wdy, Hr, ncol =2, nrow = 2)
# check major injuries distribution, still has null
ggplot(df_all, aes(WARD)) + geom_bar() + theme(axis.text.x = element_text(angle = 90))

# check range of injury values
# Fatal: 1-2, Major: 1-44, Minor: 1-17
sapply(df_all[, grepl("FATAL", names(df_all))],min)
sapply(df_all[, grepl("FATAL", names(df_all))],max)
sapply(df_all[, grepl("MAJOR", names(df_all))],min)
sapply(df_all[, grepl("MAJOR", names(df_all))],max)
sapply(df_all[, grepl("MINOR", names(df_all))],min)
sapply(df_all[, grepl("MINOR", names(df_all))],max)


# weather variable boxplots by year

AWND <- ggplot(df_all, aes(x = (Year), y = AWND)) + geom_boxplot(aes(group = Year)) + 
  scale_x_continuous(breaks = seq(2011, 2022,1)) + xlab("Daily Average Wind")

PRCP <- ggplot(df_all, aes(x = Year, y = PRCP)) + geom_boxplot(aes(group = Year)) + 
  scale_x_continuous(breaks = seq(2011, 2022,1)) + xlab ("Daily Precipation ") 

SNOW <- ggplot(df_all, aes(x = Year, y = SNOW)) + geom_boxplot(aes(group = Year)) + 
  scale_x_continuous(breaks = seq(2011, 2022,1)) + xlab ("Daily Snow")

SNWD <-ggplot(df_all, aes(x = Year, y = SNWD)) + geom_boxplot(aes(group = Year)) + 
  scale_x_continuous(breaks = seq(2011, 2022,1)) + xlab (" Daily Snow Depth")

TMax <- ggplot(df_all, aes(x = Year, y = TMAX)) + geom_boxplot(aes(group = Year)) + 
  scale_x_continuous(breaks = seq(2011, 2022,1)) + xlab ("Daily Max Temerature")

TMin <- ggplot(df_all, aes(x = Year, y = TMIN)) + geom_boxplot(aes(group = Year)) + 
  scale_x_continuous(breaks = seq(2011, 2022,1)) + xlab ("Daily Min Temperature")



Fog <- df_all %>% group_by(Year) %>% summarise(FogDay = mean(WT01)) %>% 
  ggplot() + geom_line(aes(x = Year, y = FogDay)) + scale_x_continuous(breaks = seq(2011, 2022,1)) + 
  xlab ("Fog Days")

Thunder <- df_all %>% group_by(Year) %>% summarise(ThunderDay = mean(WT03)) %>%
  ggplot() + geom_line(aes(x = Year, y = ThunderDay)) + scale_x_continuous(breaks = seq(2011, 2022,1)) + 
  xlab ("Thunder Days")

ggarrange(AWND, PRCP, SNOW, SNWD, TMax, TMin, Fog, Thunder, ncol = 3, nrow = 3)

# correlation between weather attributes
weather.data <- df_all %>% dplyr::select(AWND, PRCP, SNOW, SNWD, TMAX, TMIN, WT01, WT03)
weather.corr <- cor(weather.data)
corrplot(weather.corr, type="upper", order="hclust")

# strong correlations between TMAX and TMIN, Precipitation and Fog, Snow and snow depth;
# remove TMAX, Fog, thunder, snow depth attributes

drop_list_w2 <- c("TMAX", "SNWD", "WT01", "WT03" )
df_all <- df_all[, !names(df_all) %in% drop_list_w2]

### DATA TRANSFORMATION ###

## Transform weather data ##

# weather data are highly skewed
df_all %>%reframe(mean(AWND), sd(AWND), skewness(AWND),
                  mean(PRCP), sd(PRCP), skewness(PRCP),
                  mean(SNOW), sd(SNOW), skewness(SNOW),
                  mean(TMIN), sd(TMIN), skewness(TMIN))%>%as_tibble()


# transform TMIN, SNOW, PRCP variables to a binary variable with 1 as being below freezing 32 Fahrenheit and 0 as above
df_all <- df_all %>% mutate(IsFreezing = ifelse(TMIN <32, 1,0))
df_all <- df_all %>% mutate(IsSnow = ifelse(SNOW >0, 1,0))
df_all <- df_all %>% mutate(IsRain = ifelse(PRCP >0, 1, 0))

# # transform snow to a multi-category variable
df_all <-df_all %>% mutate(SNOW_Type = case_when(SNOW == 0 ~ "NoSnow", SNOW <= 3 & SNOW >0 ~ "Less than 3",
                                                 SNOW >3 & SNOW <=6 ~ "Less than 6", SNOW > 6 ~ "More than 6"))

# transform precipitation to a multi-category variable
df_all <- df_all %>% mutate(PRCP_Type = case_when(PRCP == 0 ~ "NoRain",
                                                  PRCP <=2 & PRCP >0 ~ "Less than 2", PRCP > 2 ~ "More than 2"))


## Crash Data Transformation ##

# transform injury variables to "IsFatal" and "IsMajor"
df_all <- df_all %>% mutate(IsFatal = rowSums(across (starts_with("FATAL")))) %>% mutate(IsFatal = ifelse(IsFatal ==0, 0,1))
df_all <- df_all %>% mutate(IsMajor= rowSums(across (starts_with("MAJOR")))) %>% mutate (IsMajor = ifelse(IsMajor == 0, 0,1))

# check fatal accident rates and counts
# mean(df_all$IsFatal)
# mean(df_all$IsMajor)
 
# df_all %>% count(IsFatal)
# df_all %>% count(IsMajor)

# roll up "IsFatal" and "IsMajor" into single variable "IsSevere"
df_all <- df_all %>% mutate(IsSevere = rowSums(across(c("IsFatal", "IsMajor"))))%>% mutate(IsSevere = ifelse(IsSevere == 0,0,1))
# check "IsSevere" accident rate and counts (IsSevere = IsFatal+IsMajor)

# transform "PEDESTRIANSIMPAIRED", "DRIVERSIMPAIRED", "BICYCLISTSIMPAIRED" into "IsImpaired" variable
df_all <- df_all %>% mutate(
          IsImpaired = rowSums(across(c("PEDESTRIANSIMPAIRED", "DRIVERSIMPAIRED", "BICYCLISTSIMPAIRED")))
          ) %>% mutate(IsImpaired = ifelse(IsImpaired == 0 ,0, 1)
      )
# check "IsImpaired" accidents rate and counts

# transform "SPEEDINGINVOLVED" to a binary variable "IsSpeeding"
df_all <- df_all %>% mutate(IsSpeeding = ifelse(SPEEDING_INVOLVED == 0, 0, 1))
# show means and counts in tibble
df_all %>%summarize(mean(IsImpaired), mean(IsSpeeding), mean(IsSevere)) %>% as_tibble()

# add variable to indicate the Covid years (2020, 2021, 2022)
df_all <- df_all %>% mutate(IsCovid = ifelse(Year <2020, 0, 1))

## changing charactor variables to factor variables
df_all <- df_all%>% mutate_at(c("CRIMEID", "SNOW_Type", "PRCP_Type"), as.factor)


### CREATE TWO DATASETS ###

## AGGREGATE CRIMEID BY DATE, ADD Accident_Total, Impaired_Perc, Speeding_Perc variables
df_vol <- df_all %>% group_by( Date, WeekDay, Month, Year, AWND, PRCP, SNOW, TMIN, 
                               PRCP_Type, SNOW_Type, IsFreezing, IsSnow, IsRain, IsCovid
                                )%>% summarise(Accident_Total =n(), 
                                              Impaired_Perc = sum(IsImpaired)/Accident_Total, 
                                              Speeding_Perc = sum(IsSpeeding)/Accident_Total)



# normalize Accident Volume data
df_vol_normalized <- as.data.frame(sapply(df_vol[, c("Accident_Total",
                                                     "Impaired_Perc",
                                                     "Speeding_Perc",
                                                     "AWND", "PRCP", "SNOW", "TMIN")], scale))
df_vol_normalized <- cbind(df_vol_normalized, Date = df_vol$Date, WeekDay =df_vol$WeekDay, Month = df_vol$Month, Year =df_vol$Year)
                                          

### Roll UP BY DATE, ADD Accident_Total, Impaired_Perc, Speeding_Perc, Severe_Perc variables ###

df_vol_Severe <- df_all %>% group_by(Date, WeekDay, Month, Year, AWND, PRCP, SNOW, TMIN, 
                                     PRCP_Type, SNOW_Type, IsFreezing, IsSnow, IsRain, IsCovid
                                      ) %>% summarise (Accident_Total =n(), 
                                                      Impaired_Perc = sum(IsImpaired)/Accident_Total, 
                                                      Speeding_Perc = sum(IsSpeeding)/Accident_Total,
                                                      Severe_Perc = sum(IsSevere)/Accident_Total)                                           

# normalize Severity data
df_vol_Severe_normalized <- as.data.frame(sapply(df_vol_Severe[, c("Severe_Perc",
                                                     "Impaired_Perc",
                                                     "Speeding_Perc",
                                                     "AWND", "PRCP", "SNOW", "TMIN")], scale))

df_vol_Severe_normalized <- cbind(df_vol_Severe_normalized, Date = df_vol_Severe$Date, 
                                  WeekDay = df_vol_Severe$WeekDay, Month = df_vol_Severe$Month, Year = df_vol_Severe$Year)

# check accident volume, impaired, speeding, severe percentages' distributions by Year
# there is a significant change around 2016, when the data was migrated from the old system to a new system
# supports to trim the time period from 12 years to 7 years  (2016 - 2022)

volume.box <- df_vol %>% ggplot(aes(x= Year, y = Accident_Total)) + geom_boxplot(aes(group = Year))
impaired.box <- df_vol %>% ggplot(aes(x= Year, y = Impaired_Perc)) + geom_boxplot(aes(group = Year))
speeding.box <- df_vol %>% ggplot(aes(x = Year, y = Speeding_Perc)) + geom_boxplot(aes(group = Year))
severe.box <- df_vol_Severe %>% ggplot(aes(x = Year, y = Severe_Perc)) + geom_boxplot(aes(group = Year))

ggarrange(volume.box, impaired.box, speeding.box, severe.box, nrow = 2, ncol = 2)

### SPLIT TRAINING & TEST DATA SETS ###

## for Accident Volume ##
# if including the Covid period of 2020 - 2022
df_vol_train <- df_vol %>% filter(Year <2022 & Year >=2016)
df_vol_test <- df_vol %>% filter(Year == 2022)

# if testing normalized dataset
df_vol_normalized_train <- df_vol_normalized%>%filter(Year <2022 & Year >=2016)
df_vol_normalized_test <- df_vol_normalized%>%filter(Year == 2022)

# if excluding Covid period of 2020 - 2022
df_vol_train_2 <- df_vol %>% filter(Year <2019 & Year >=2016)
df_vol_test_2 <- df_vol %>% filter (Year == 2019)

## for Severe Percentage ##
# if including covid
df_vol_Severe_train <- df_vol_Severe %>% filter(Year <2022 & Year >=2016)
df_vol_Severe_test <- df_vol_Severe %>% filter(Year ==2022)

# if testing normalized dataset
df_vol_Severe_normalized_train <- df_vol_Severe_normalized %>% filter(Year <2022 & Year >=2016)
df_vol_Severe_normalized_test <- df_vol_Severe_normalized %>% filter(Year ==2022)

#if excluding covid
df_vol_Severe_train_2 <- df_vol_Severe %>% filter(Year <2019 & Year >= 2016)
df_vol_Severe_test_2 <- df_vol_Severe %>% filter (Year ==2019)


## Descriptive Analytics on Accident_Total and Severe_Perc response variables vs. weather variables
AWND.sc <- ggplot(df_vol_train, aes(x=AWND, y = Accident_Total)) + geom_point()
PRCP.sc <- ggplot(df_vol_train, aes(x=PRCP, y = Accident_Total))+ geom_point()
SNOW.sc <- ggplot(df_vol_train, aes(x=SNOW, y = Accident_Total)) + geom_point()
TMIN.sc <-ggplot(df_vol_train, aes(x=TMIN, y = Accident_Total)) + geom_point()
IsSnow.sc <- df_vol_train%>% filter(IsSnow ==1) %>% ggplot(aes(x=SNOW,y = Accident_Total)) + geom_point() + xlab("IsSnow")
IsRain.sc <- df_vol_train%>% filter(IsRain ==1) %>% ggplot(aes(x=PRCP,y = Accident_Total)) + geom_point() +xlab("IsRain")
IsFreezing.sc <- df_vol_train%>% filter(IsFreezing ==1) %>% ggplot(aes(x=TMIN,y = Accident_Total)) + geom_point() +xlab("IsFreezing")

weather.sc <- ggarrange(AWND.sc, PRCP.sc, SNOW.sc, TMIN.sc, IsSnow.sc, IsRain.sc, IsFreezing.sc,ncol =3, nrow =3)               
weather.sc

# check normalized scatterplot: normalization doesn't improve
AWND.n.sc <- ggplot(df_vol_normalized_train, aes(x=AWND, y = Accident_Total)) + geom_point()
PRCP.n.sc <- ggplot(df_vol_normalized_train, aes(x=PRCP, y = Accident_Total))+ geom_point()
SNOW.n.sc <- ggplot(df_vol_normalized_train, aes(x=SNOW, y = Accident_Total)) + geom_point()
TMIN.n.sc <-ggplot(df_vol_normalized_train, aes(x=TMIN, y = Accident_Total)) + geom_point()

weather.n.sc <-ggarrange(AWND.n.sc, PRCP.n.sc, SNOW.n.sc, TMIN.n.sc,ncol =2, nrow =2)
weather.n.sc

# Weather features have large influential points and very weak R-square with Accident_Total
# Cook's Distance tests on normalized weather features
AWND.lm <- lm(Accident_Total ~ AWND, data = df_vol_train)

summary(AWND.lm) #insignificant
PRCP.lm <- lm(Accident_Total ~PRCP, data = df_vol_train)
summary(PRCP.lm) #insignificant, adj.R very low
SNOW.lm <- lm(Accident_Total ~SNOW, data = df_vol_train)
summary(SNOW.lm) #significant, adj.R 0.0056
TMIN.lm <- lm(Accident_Total ~TMIN, data = df_vol_train)
summary(TMIN.lm) # signficant, adj. R 0.0277
Impaired.lm <- lm(Accident_Total ~ Impaired_Perc, data = df_vol_train)
summary(Impaired.lm) # significant, adj. R 0.0248
Speeding.lm <- lm(Accident_Total ~ Speeding_Perc, data = df_vol_train)
summary(Speeding.lm) # signficant, adj. R 0.0384

n = nrow(df_vol_train)
AWND.cooksD <- fortify(AWND.lm) %>% ggplot(aes(seq_along(.cooksd), y= .cooksd)) +
  geom_point(shape = 21, colour = "black", fill = "white", size = 2) + geom_hline(yintercept = 4/n, col = "red") + 
  xlab("AWND Cooks'D")
PRCP.cooksD <- fortify(PRCP.lm) %>% ggplot(aes(seq_along(.cooksd), y= .cooksd)) + 
  geom_point(shape = 21, colour = "black", fill = "white", size = 2) + geom_hline(yintercept = 4/n, col = "red") +
  xlab("PRCP Cooks'D")
SNOW.cooksD <- fortify(SNOW.lm) %>% ggplot(aes(seq_along(.cooksd), y= .cooksd)) + 
  geom_point(shape = 21, colour = "black", fill = "white", size = 2) + geom_hline(yintercept = 4/n, col = "red") +
  xlab("SNOW Cooks'D")
TMIN.cooksD <- fortify(TMIN.lm) %>% ggplot(aes(seq_along(.cooksd), y= .cooksd)) + 
  geom_point(shape = 21, colour = "black", fill = "white", size = 2) + geom_hline(yintercept = 4/n, col = "red") +
  xlab("TMIN Cooks'D")

ggarrange(AWND.cooksD, PRCP.cooksD, SNOW.cooksD, TMIN.cooksD, ncol = 2, nrow = 2)

# check normalized data regression relationships
# Normalized weather variables don't improve adj. R^2 but shrink coefficients
AWND.lm.n <- lm(Accident_Total ~ AWND, data = df_vol_normalized_train)
summary(AWND.lm.n) #insignificant
PRCP.lm.n <- lm(Accident_Total ~PRCP, data = df_vol_normalized_train)
summary(PRCP.lm.n) #insignificant
SNOW.lm.n <- lm(Accident_Total ~SNOW, data = df_vol_normalized_train)
summary(SNOW.lm.n) #significant, adj.R 0.0056
TMIN.lm.n <- lm(Accident_Total ~TMIN, data = df_vol_normalized_train)
summary(TMIN.lm.n) # significant, adj. R 0.0277
Impaired.lm.n <- lm(Accident_Total ~ Impaired_Perc, data = df_vol_normalized_train)
summary(Impaired.lm.n) # significant, adj. R 0.0248
Speeding.lm.n <- lm(Accident_Total ~ Speeding_Perc, data = df_vol_normalized_train)
summary(Speeding.lm.n) # signficant, adj. R 0.0384

# check IsSnow, IsRain, IsFreezing models 
# regressing Accident_Total against weather filtered by IsSnow, IsRain, IsFreezing shows largely insiginificant, except for SNOW
df_vol.IsSnow <- filter(df_vol_train, IsSnow ==1)
IsSnow.model <-lm(Accident_Total~SNOW, data = df_vol.IsSnow)
summary(IsSnow.model) # significant, adj. R 0.11

df_vol.IsRain <- filter(df_vol_train, IsRain ==1)
IsRain.model <-lm(Accident_Total~PRCP, data = df_vol.IsRain)
summary(IsRain.model) # insignificant

df_vol.IsFreezing <- filter(df_vol_train, IsFreezing ==1)
IsFreezing.model <-lm(Accident_Total ~ TMIN, data = df_vol.IsFreezing)
summary(IsFreezing.model) # insignificant

# check the model pre and post removing influential points
# but we cannot remove influential points because weather data are real
# many influential points will impact model fitting

AWND.cd.num<- cooks.distance(AWND.lm)
influential.obs <- as.numeric(names(AWND.cd.num)[(AWND.cd.num > (4/n))])
outliers.removed <- df_vol_train[-influential.obs, ]

outliers.present.chart <- ggplot(data = df_vol_train, aes(x = AWND, y = Accident_Total)) +
  geom_point() +
  geom_smooth(method = lm) +
  ggtitle("Outliers Present")

outliers.removed.chart<- ggplot(data = outliers.removed, aes(x = AWND, y = Accident_Total)) +
  geom_point() +
  geom_smooth(method = lm) +
  ggtitle("Outliers Removed")

ggarrange(outliers.present.chart, outliers.removed.chart, nrow = 2)


### BUILD MODELS ON ACCIDENT VOLUME DATA ###

# linear regression on weather features
# PRCP is insignificant, AWND is noisy and less value-added
model_PRCP <- lm(Accident_Total ~ PRCP, data = df_vol_train)
summary(model_PRCP) # not significant, adj. R very low

model_PRCP_SNOW <-lm(Accident_Total ~ PRCP + SNOW, data = df_vol_train)
summary(model_PRCP_SNOW) # SNOW significant, adj.R 0.005

model_PRCP_SNOW_TMIN <-lm(Accident_Total ~ PRCP + SNOW + TMIN, data = df_vol_train)
summary(model_PRCP_SNOW_TMIN) # SNOW & TMIN significant, adj.R 0.03161

model_PRCP_SNOW_TMIN_AWND <- lm(Accident_Total ~ PRCP + SNOW + TMIN + AWND, data = df_vol_train)
summary(model_PRCP_SNOW_TMIN_AWND) # SNOW, TMIN, AWND significant, adj.R 0.03621


# test interaction variables "PRCP_TMIN" & "AWND_SNOW"
model_interact_1 <- lm(Accident_Total ~ SNOW + SNOW * AWND + TMIN + TMIN *PRCP, data = df_vol_train)
summary(model_interact_1) # AWND, TMIN are significant, adj. R 0.03709, but less intuitive

model_interact_2 <- lm(Accident_Total ~ SNOW + SNOW *IsFreezing + TMIN + TMIN *AWND + TMIN *PRCP_Type, data = df_vol_train)
summary(model_interact_2) # no variables are significant, adj. R 0.03662

# linear regression on weather factors
# conclusion 1: best weather features are SNOW and TMIN; interaction terms are not significant
model_weatherFactor <- lm(Accident_Total ~AWND + SNOW_Type + PRCP_Type + IsFreezing, data = df_vol_train)
summary(model_weatherFactor) # weather factors don't improve adj. R; adj. R 0.0140


# build stepwise model on weather and crash variables
# Adj. R improves to 0.1011, crash features explain about 6% of data variability
# in the Stepwise model, all variables are significant, but AWND and PRCP add the least value to reduce AIC
model_min <- lm(Accident_Total ~1, data = df_vol_train)
model_weather_crash <- lm(Accident_Total ~ SNOW + TMIN + PRCP + AWND + Impaired_Perc + Speeding_Perc, 
                          data = df_vol_train)

stepAIC(model_min, direction = 'forward', scope = formula(model_weather_crash), trace = TRUE)
summary(model_weather_crash) 

# modeling on weather, crash, and date variables (WeekDay, Month)
# stepwise feature selection analysis
# in the Stepwise model, all variables are significant, but AWND and PRCP add the least value to reduce AIC
# adj. R 0.1772, WeekDay and Month explain additional 8.5% of data variability
model_max<-lm(Accident_Total ~ SNOW + TMIN +PRCP + AWND + Impaired_Perc + Speeding_Perc +
                WeekDay + Month, data = df_vol_train)
stepAIC(model_min, direction = 'forward', scope = formula(model_max), trace = TRUE)
summary(model_max) 

# model_max using normalized dataset
# same significant variables, same adj. R, but with smaller coefficents
model_max_normalized <- lm(Accident_Total ~ SNOW + TMIN +PRCP + AWND + Impaired_Perc + Speeding_Perc +
                             WeekDay + Month, data = df_vol_normalized_train)
summary(model_max_normalized)

# prepare test dataset excluding Covid period
# significant variables are the same, but adj. R increased to 0.3506!
# AIC is significantly lower!
# in the Stepwise model, SNOW, TMIN, crash variables, WeekDay, Month are significant; AWND and PRCP are insignificant
model_min_PreCovid <- lm(Accident_Total~1, data = df_vol_train_2)
model_max_PreCovid<-lm(Accident_Total ~ SNOW + TMIN + PRCP + AWND + Impaired_Perc + Speeding_Perc +
                      WeekDay + Month, data = df_vol_train_2)
model_min_PreCovid <- lm(Accident_Total~1, data = df_vol_train_2)
summary(model_max_PreCovid) 
stepAIC(model_min_PreCovid, direction = 'forward', scope = formula(model_max_PreCovid), trace = TRUE)

# cross validation using the dataset including the Covid period
# cross validation on regression model using independent variables: SNOW, TMIN, Impaired_Perc, Speeding_Perc, WeekDay, Month
library(leaps)
set.seed(39)

df_vol_train_drop <- c('Date', 'Year', 'AWND', 'PRCP', 'SNOW_Type', 'PRCP_Type', 'IsFreezing', 'IsCovid')
df_vol_train_cv <- df_vol_train[, !names(df_vol_train) %in% df_vol_train_drop]

train.control <- trainControl(method = "cv", number = 10)
# Train the model
cv.model <- train(Accident_Total ~., data = df_vol_train_cv,
                    method = "leapBackward", 
                    tuneGrid = data.frame(nvmax = 1:10),
                    trControl = train.control)


# check results
cv.model$results
# model 10 is the best, R^2 0.1553 (close to our best model)
cv.model$bestTune
# show its variables are : WeekDay.Q (Tuesday), Month(May), SNOW, TMIN, Impaired_Perc, Speeding_Perc
summary(cv.model$finalModel)
# check coefficient
coef(cv.model$finalModel, 10)

### CONCLUDE BEST MODEL IS: 
Best_Model_vol <-lm(Accident_Total ~SNOW + TMIN + WeekDay + Month + Impaired_Perc + Speeding_Perc, df_vol_train) 
coef(Best_Model_vol)

## predict on test set ##
# set test set
df_vol_test <- df_vol_test[, !names(df_vol_test) %in% df_vol_train_drop]
# fit training set
fit_df_vol_cv <-lm(Accident_Total ~SNOW + TMIN + WeekDay + Month + Impaired_Perc + Speeding_Perc, data = df_vol_train)

## measure model performance ##
# MSE of training set is 218.694
df_vol_fit_MSE <-mean(fit_df_vol_cv$residuals^2)
print(df_vol_fit_MSE)

# predict
pred_df_vol_test <- predict(fit_df_vol_cv, df_vol_test, type = 'response')

# MSE of testing set is 497.9333, reasonable given the low adj. R of the model
result_df_vol_test <- data.frame(pred = pred_df_vol_test, actual = df_vol_test$Accident_Total)
df_vol_pred_MSE <- mean((result_df_vol_test$pred - result_df_vol_test$actual)^2)
print(df_vol_pred_MSE)

# MAE Percentage 52.99%
df_vol_pred_MAE<- sum(abs(result_df_vol_test$pred - result_df_vol_test$actual)/result_df_vol_test$actual)/nrow(result_df_vol_test)*100
print(df_vol_pred_MAE) 

# R-Square 16.94%
result_df_vol_test.SSR <- sum((result_df_vol_test$pred - mean(result_df_vol_test$pred))^2)
result_df_vol_test.TSS <- sum((mean(result_df_vol_test$pred) - result_df_vol_test$actual)^2)
result_df_vol_test.R <- result_df_vol_test.SSR/result_df_vol_test.TSS

print("Accident Volume Test Set R Square")
print(result_df_vol_test.R) 


### Charting Fit Model Performance ###
df_vol_fort <-fortify(fit_df_vol_cv)

# plot fitted vs.residual
fit_resid <- ggplot(df_vol_fort, aes(.fitted, .resid)) + stat_smooth(method="loess") +
  geom_point() + xlab("Fitted Values") + ylab("Residuals")
# plot Q-Q plot
Q_Q <-ggplot(df_vol_fort, aes(sample = .resid)) + stat_qq() +
 xlab("Theoretical Quantiles")+ ylab("Fitted Residuals")

ggarrange(fit_resid, Q_Q, nrow=2)

### Charting Predicted Model Performance ###

# Residual vs. Fit values chart shows 
# 1) non linear traits between independent and dependent variables
# 2) heteroskedasticity in error terms
# 3) existence of outliers

# Residual Q-Q plot shows decent linear upward trend, with curveture on both ends

result_df_vol_test$resid<- result_df_vol_test$actual - result_df_vol_test$pred
pred_resid <- ggplot(result_df_vol_test, aes(pred, resid)) + stat_smooth(method="loess") +
  geom_point() + xlab("Predicted Values") + ylab("Residuals")
pred_Q_Q <- ggplot(result_df_vol_test, aes(sample = resid)) + stat_qq() +
  xlab("Theoretical Quantiles")+ ylab("Predicted Accident Total")

ggarrange(pred_resid, pred_Q_Q, nrow = 2)


### BUILD MODELS ON ACCIDENT SEVERITY DATA ###

## scatterplots to check Severity relationship with weather features

AWND.severe.sc <- ggplot(df_vol_Severe_train, aes(x=AWND, y = Severe_Perc)) + geom_point()
PRCP.severe.sc <- ggplot(df_vol_Severe_train, aes(x=PRCP, y = Severe_Perc))+ geom_point()
SNOW.severe.sc <- ggplot(df_vol_Severe_train, aes(x=SNOW, y = Severe_Perc)) + geom_point()
TMIN.severe.sc <-ggplot(df_vol_Severe_train, aes(x=TMIN, y = Severe_Perc)) + geom_point()
IsSnow.severe.sc <- df_vol_Severe_train%>% filter(IsSnow ==1) %>% ggplot(aes(x=SNOW,y = Severe_Perc)) + geom_point() + xlab("IsSnow")
IsRain.severe.sc <- df_vol_Severe_train%>% filter(IsRain ==1) %>% ggplot(aes(x=PRCP,y = Severe_Perc)) + geom_point() + xlab("IsRain")
IsFreezing.severe.sc <- df_vol_Severe_train%>% filter(IsFreezing ==1) %>% ggplot(aes(x=TMIN,y = Severe_Perc)) + geom_point() + xlab("IsFreezing")

weather.severe.sc <- ggarrange(AWND.severe.sc, PRCP.severe.sc, SNOW.severe.sc, TMIN.severe.sc, IsSnow.severe.sc, IsRain.severe.sc, IsFreezing.severe.sc,
                        ncol =3, nrow =3)               
weather.severe.sc

## scatterplot to check relationship using normalized data set
AWND.sv.n.sc <- ggplot(df_vol_Severe_normalized_train, aes(x=AWND, y = Severe_Perc)) + geom_point()
PRCP.sv.n.sc <- ggplot(df_vol_Severe_normalized_train, aes(x=PRCP, y = Severe_Perc))+ geom_point()
SNOW.sv.n.sc <- ggplot(df_vol_Severe_normalized_train, aes(x=SNOW, y = Severe_Perc)) + geom_point()
TMIN.sv.n.sc <-ggplot(df_vol_Severe_normalized_train, aes(x=TMIN, y = Severe_Perc)) + geom_point()
Impaired_Perc.sv.sc <-ggplot(df_vol_Severe_normalized_train, aes(x=Impaired_Perc, y = Severe_Perc)) + geom_point()
Speeding_Perc.sv.sc <-ggplot(df_vol_Severe_normalized_train, aes(x=Speeding_Perc, y = Severe_Perc)) + geom_point()

weather.sv.n.sc <-ggarrange(AWND.sv.n.sc, PRCP.sv.n.sc, SNOW.sv.n.sc, TMIN.sv.n.sc,Impaired_Perc.sv.sc, Speeding_Perc.sv.sc,
                         ncol =3, nrow =2)
weather.sv.n.sc

# building models
# linear regression on Severe_Perc and weather data
AWND.severe.lm <- lm(Severe_Perc ~ AWND, data = df_vol_Severe_train)
summary(AWND.severe.lm) # insiginificant
PRCP.severe.lm <- lm(Severe_Perc ~PRCP, data = df_vol_Severe_train)
summary(PRCP.severe.lm) #insignificant
SNOW.severe.lm <- lm(Severe_Perc ~SNOW, data = df_vol_Severe_train)
summary(SNOW.severe.lm) #significant, adj.R 0.0020
TMIN.severe.lm <- lm(Severe_Perc ~TMIN, data = df_vol_Severe_train)
summary(TMIN.severe.lm) # significant, adj. R 0.007
Impaired.severe.lm <- lm(Severe_Perc ~ Impaired_Perc, data = df_vol_Severe_train)
summary(Impaired.severe.lm) # significant, adj. R 0.0045
Speeding.severe.lm <- lm(Severe_Perc ~ Speeding_Perc, data = df_vol_Severe_train)
summary(Speeding.severe.lm) # significant, adj. R 0.006

# linear regression on normalized Severe_Perc and weather data
# normalization doesn't improve adj. R square but shrink coefficients
AWND.severe.n.lm <- lm(Severe_Perc ~ AWND, data = df_vol_Severe_normalized_train)
summary(AWND.severe.n.lm) # insignificant
PRCP.severe.n.lm <- lm(Severe_Perc~PRCP, data = df_vol_Severe_normalized_train)
summary(PRCP.severe.n.lm) #insignificant
SNOW.severe.n.lm <- lm(Severe_Perc ~SNOW, data = df_vol_Severe_normalized_train)
summary(SNOW.severe.n.lm) #significant, adj.R 0.0020
TMIN.severe.n.lm <- lm(Severe_Perc ~TMIN, data = df_vol_Severe_normalized_train)
summary(TMIN.severe.n.lm) # significant, adj. R 0.007
Impaired.severe.n.lm <- lm(Severe_Perc ~ Impaired_Perc, data = df_vol_Severe_normalized_train)
summary(Impaired.severe.n.lm) # significant, adj. R 0.0045
Speeding.severe.n.lm <- lm(Severe_Perc ~ Speeding_Perc, data = df_vol_Severe_normalized_train)
summary(Speeding.severe.n.lm) # significant, adj. R 0.006

# build regression models through iterations
model_PRCP_sv <- lm(Severe_Perc ~ PRCP, data = df_vol_Severe_train)
summary(model_PRCP_sv) # not significant, adj. R very low

model_PRCP_SNOW_sv <-lm(Severe_Perc ~ PRCP + SNOW, data = df_vol_Severe_train)
summary(model_PRCP_SNOW_sv) # SNOW significant, adj.R 0.003

model_PRCP_SNOW_TMIN_sv <-lm(Severe_Perc ~ PRCP + SNOW + TMIN, data = df_vol_Severe_train)
summary(model_PRCP_SNOW_TMIN_sv) # SNOW & TMIN significant, adj.R 0.009

model_PRCP_SNOW_TMIN_AWND_sv <- lm(Severe_Perc ~ PRCP + SNOW + TMIN + AWND, data = df_vol_Severe_train)
summary(model_PRCP_SNOW_TMIN_AWND_sv) # SNOW, TMIN significant, adj.R 0.008

Severe_ <- lm(Severe_Perc ~ SNOW + TMIN + Impaired_Perc + Speeding_Perc + WeekDay + Month, data = df_vol_Severe_train) 
summary(Severe_) 

# build stepwise model
# significant features are SNOW, TMIN, crash features. SNOW adds less incremental value
# adj.R-square 0.0183
model_min_Severe <-lm(Severe_Perc ~1, data = df_vol_Severe_train)
model_max_Severe<-lm(Severe_Perc ~ SNOW + TMIN +PRCP + AWND + Impaired_Perc + Speeding_Perc +
                WeekDay + Month, data = df_vol_Severe_train)
stepAIC(model_min_Severe, direction = 'forward', scope = formula(model_max_Severe), trace = TRUE)
Best_Model_Severe <- lm(Severe_Perc ~ TMIN + Impaired_Perc + Speeding_Perc, data = df_vol_Severe_train)
summary(Best_Model_Severe)

# using normalized Severe data
# same adj. R square 0.0191, but the coefficients are improved
model_min_Severe_norm <-lm(Severe_Perc ~1, data = df_vol_Severe_normalized_train)
model_max_Severe_norm<-lm(Severe_Perc ~ SNOW + TMIN +PRCP + AWND + Impaired_Perc + Speeding_Perc +
                       WeekDay + Month, data = df_vol_Severe_normalized_train)
stepAIC(model_min_Severe_norm, direction = 'forward', scope = formula(model_max_Severe_norm), trace = TRUE)
Best_Model_Severe_norm <- lm(Severe_Perc ~ SNOW + TMIN + Impaired_Perc + Speeding_Perc, data = df_vol_Severe_normalized_train)
summary(Best_Model_Severe_norm)

# using pre covid data
# adj. R square 0.0196, not much improved as compared with model with Covid years
# significant variables are TMIN, Impared_Perc
Best_Model_Severe_PreCovid <- lm(Severe_Perc ~ SNOW + TMIN + Impaired_Perc + Speeding_Perc, data = df_vol_Severe_train_2)
summary(Best_Model_Severe_PreCovid)

## predict on test set ##
# set test set
severe_drop <- c("Date", "WeekDay", "Month", "Year", "SNOW", "IsFreezing", "IsSnow", "IsRain", "IsCovid")
df_vol_Severe_test <- df_vol_Severe_test[, !names(df_vol_Severe_test) %in% severe_drop]
# fit training set
df_Severe_fit <-lm(Severe_Perc ~ TMIN + Impaired_Perc + Speeding_Perc, data = df_vol_Severe_train)

## measure model performance ##
# MSE of training set is 0.033%
df_Severe_fit_MSE <-mean(df_Severe_fit$residuals^2)
print(df_Severe_fit_MSE)
# predict
pred_df_vol_Severe_test <- predict(df_Severe_fit, df_vol_Severe_test, type = 'response')

# MSE of testing set is 0.051% reasonable given the low adj. R of the model
result_df_vol_Severe_test <- data.frame(pred = pred_df_vol_Severe_test, actual = df_vol_Severe_test$Severe_Perc)
df_vol_Severe_pred_RMSE <- (mean((result_df_vol_Severe_test$pred - result_df_vol_Severe_test$actual)^2))^.05
print(df_vol_Severe_pred_RMSE)

# MAE Percentage 1.80%
df_vol_Severe_pred_MAE<- mean(abs(result_df_vol_Severe_test$pred - result_df_vol_Severe_test$actual))
print(df_vol_Severe_pred_MAE) 

# R-Square 0.0185, close to fit model adj.R 0.0183
result_df_vol_Severe_test.SSR <- sum((result_df_vol_Severe_test$pred - mean(result_df_vol_Severe_test$pred))^2)
result_df_vol_Severe_test.TSS <- sum((mean(result_df_vol_Severe_test$pred) - result_df_vol_Severe_test$actual)^2)
result_df_vol_Severe_test.R <- result_df_vol_Severe_test.SSR/result_df_vol_Severe_test.TSS

print("Severity Test Set R Square")
print(result_df_vol_Severe_test.R) 


### Charting Fit Model Performance ###
# fitted model doesn't seem to fit well
df_vol_Severe_fort <-fortify(df_Severe_fit)

# plot fitted vs.residual
fit_resid_Severe <- ggplot(df_vol_Severe_fort, aes(.fitted, .resid)) + stat_smooth(method="loess") +
  geom_point() + xlab("Fitted Values") + ylab("Residuals")
# plot Q-Q plot
Q_Q_Severe <-ggplot(df_vol_Severe_fort, aes(sample = .resid)) + stat_qq() +
  xlab("Theoretical Quantiles")+ ylab("Fitted Residuals")

ggarrange(fit_resid_Severe, Q_Q_Severe, nrow=2)

### Charting Predicted Model Performance ###

# Residual vs. Fit values chart shows 
# 1) linear relationship between dependent and independent variables are present
# 2) heteroskedasticity in error terms
# 3) existence of outliers


result_df_vol_Severe_test$resid<- result_df_vol_Severe_test$actual - result_df_vol_Severe_test$pred
pred_resid_Severe <- ggplot(result_df_vol_Severe_test, aes(pred, resid)) + stat_smooth(method="loess") +
  geom_point() + xlab("Predicted Values") + ylab("Residuals")
pred_Q_Q_Severe <- ggplot(result_df_vol_Severe_test, aes(sample = resid)) + stat_qq() +
  xlab("Theoretical Quantiles")+ ylab("Predicted Accident Total")

ggarrange(pred_resid_Severe, pred_Q_Q_Severe, nrow = 2)
