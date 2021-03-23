# IPUMS USA
# R demo scripts
# Ryan Womack
# 2021-03-22

# IPUMS USA data is available for download from
# https://usa.ipums.org

# install ipumsr
install.packages("ipumsr", dependencies=TRUE)

# Using ipumsr to work with IPUMS USA data

# run your data extract on the IPUMS site
# download 1) the data file (default is .DAT format); 2) DDI link; 3) R code file; 4) optionally, download the text .cbk file if you want the codebook in this format

# the .DAT file will be zipped, but the ipumsr package can handle automatically extracting it

# The code block between the ##### lines is the auto-generated download file from the IPUMS site
# usually it is best to just run this as is without modification

#########################
# NOTE: To load data, you must download both the extract's data and the DDI
# and also set the working directory to the folder with these files (or change the path below).

if (!require("ipumsr")) stop("Reading IPUMS data into R requires the ipumsr package. It can be installed using the following command: install.packages('ipumsr')")

ddi <- read_ipums_ddi("usa_00003.xml")
data <- read_ipums_micro(ddi)
########################

# note that we must import the DDI file format first, before working with data

# the DDI file contains instructions on how to import the data properly

# FYI, DDI is the Data Documentation Initiative's standard format for documenting metadata, data, and codebook information in a machine-readable XML specification

# OPTIONAL - I have created a smaller data extract to fit within Github file limits.
# If you are having issues with your own extract,
# or just want something smaller to play with,
# download usa_sample.RData from github and
# use the following commands to load it
#
# load(usa_sample.RData)
# data <- usa_sample


# this code to generate a smaller sample is
#
# usa_sample <- sample_n(data, 100000)
# save(usa_sample, file="usa_sample.RData")

# there is also a .csv version of the smaller sample for those who prefer that format



# Now let's look at the data a bit
library(tidyverse)

attach(data)

# just confirming some basics
summary(YEAR)
table(YEAR)

# for information on properly coding and setting up your variable labels, see
vignette("value-labels", package = "ipumsr")
# hint, there is no quick fix here for all labelling issues
# but you can access existing labels with
ipums_val_labels(SEX)

names(data)

hist(AGE)

# using the data structure
data %>%
  group_by(YEAR) %>%
  summarise(mean = mean(AGE, na.rm=TRUE), n = n())

ggplot(data, aes(x=AGE)) + facet_wrap(YEAR) + geom_histogram()

ggplot(data, aes(x=AGE)) + facet_wrap(YEAR) + geom_density()

table(RACE)
ipums_val_labels(RACE)

data %>%
  group_by(YEAR) %>%
  summarise(mean = mean(RACE, na.rm=TRUE), n = n())

data$RACE2<-as.factor(RACE)

attach(data)

ggplot(data, aes(x=AGE)) + facet_wrap(YEAR) + geom_density(aes(color=RACE2))

ipums_val_labels(RACE)

# filter for white and Black only
# going to toggle back and forth between "data" as the full dataset and "data2" as the filtered data in examples below

data2 <-
  data %>%
  filter(RACE<3)

attach(data2)
ggplot(data2, aes(x=AGE)) + facet_wrap(YEAR) + geom_density(aes(color=RACE2))

data2 %>%
  group_by(YEAR, RACE) %>%
  summarise(mean = mean(AGE, na.rm=TRUE), n = n())

attach(data)
data %>%
  group_by(YEAR, RACE) %>%
  summarise(mean = mean(AGE, na.rm=TRUE), n = n())

## Education

ipums_val_labels(EDUC)
data %>%
  group_by(YEAR) %>%
  summarise(mean = mean(EDUC, na.rm=TRUE), n = n())

## Income
ipums_val_labels(INCTOT)

data %>%
  group_by(YEAR) %>%
  summarise(mean = mean(INCTOT, na.rm=TRUE), n = n())

data$INCTOT <- na_if(data$INCTOT,9999999)

data %>%
  group_by(YEAR) %>%
  summarise(mean = mean(INCTOT, na.rm=TRUE), n = n())

# note that the "D" stands for detailed in the variables - LANGUAGE vs. LANGUAGED - LANGUAGED *may* be a more detailed response

## Ancestry
ipums_val_labels(ANCESTR1)
as.data.frame(ipums_val_labels(ANCESTR1))


# look for Russian ancestry
data2 <-
  data %>%
  filter(ANCESTR1==122)

attach(data2)
table(ANCESTR1)
table(RACE)
ggplot(data2, aes(x=AGE)) + facet_wrap(YEAR) + geom_density()

# Language
ipums_val_labels(LANGUAGE)
as.data.frame(ipums_val_labels(LANGUAGE))

# look for Russian Language
data2 <-
  data %>%
  filter(LANGUAGE== 18)

attach(data2)
table(LANGUAGE)
table(RACE)
ggplot(data2, aes(x=AGE)) + facet_wrap(YEAR) + geom_density()

# labor force, occupation, occupational scoring
attach(data)
ipums_val_labels(LABFORCE)

data %>%
  group_by(YEAR) %>%
  summarise(mean = mean(LABFORCE, na.rm=TRUE), n = n())

table(YEAR,LABFORCE)
# we might want to clean up the NA data here

table(YEAR,LABFORCE,SEX)

ipums_val_labels(OCC)

# This one is complex, so best to refer to web documentation at https://usa.ipums.org/usa/volii/occ2018.shtml

# look for Librarians
data2 <-
  data %>%
  filter(OCC==2435)

attach(data2)
table(OCC)
ggplot(data2, aes(x=AGE)) + facet_wrap(YEAR) + geom_density()

ipums_val_labels(OCCSCORE)
table(OCCSCORE)

attach(data)
table(OCCSCORE)

# ending on a more colorful note - age distributions of librarians by race
attach(data2)
ggplot(data2, aes(x=AGE)) + geom_density(aes(color=RACE2))

# consult with IPUMS directly for detailed assistance, and check their webpages for additional tutorials and exercises
