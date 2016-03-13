setwd("/Users/zheng/Desktop/marcus/Exploratary_Data_Analysis_Course_Project")

# Question 1: Have total emissions from PM2.5 decreased in the United States from
# 1999 to 2008? Using the base plotting system, make a plot showing the total 
# PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008.

# Load packages
library(dplyr)

# Load data into R, make sure the data is located in your working directory
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Get a first view of the data, looks like we only need the data from NEI for
# first plot
head(NEI)
head(SCC)

# Group data by year and calculate some statistics for Emissions per year
# No missing observations for the variable Emissions, Good!
NEIyear <- NEI %>%
  group_by(year) %>%
  summarise(totalCount = n(), 
            count = sum(is.na(Emissions)),
            sumEmissions = sum(Emissions),
            avgEmissions = mean(Emissions),
            maxEmissions = max(Emissions),
            minEmissions = min(Emissions)
            )

# Create and save plot
png("plot1.png", width = 480, height = 480)
plot(NEIyear$year, NEIyear$sumEmissions,
     type = "o",
     xlab="Year", 
     ylab = "Total PM2.5 emissions",
     )
title(main = "Total PM2.5 emissions 1999 - 2008 in the United States")
dev.off()

# Total emissions from PM2.5 have decreased in the United States from
# 1999 to 2008.