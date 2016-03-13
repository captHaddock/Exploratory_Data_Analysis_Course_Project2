# Question 2: Have total emissions from PM2.5 decreased in the Baltimore City, 
# Maryland (fips == "24510") from 1999 to 2008? Use the base plotting system to 
# make a plot answering this question.

# Load packages
library(dplyr)

# Load data into R, make sure the data is located in your working directory
NEI <- readRDS("summarySCC_PM25.rds")

# Group data by year and calculate some statistics for Emissions per year for
# Baltimore.
# Strange that max value is the same (with 3 decimals) for the years 2002 and
# 2005. Would be intressting to know if it was the same place thoose values were
# measured from, maybe check that out later.
NEIyearBaltimore <- NEI %>%
  filter(fips == "24510") %>%
  group_by(year) %>%
  summarise(totalCount = n(), 
            count = sum(is.na(Emissions)),
            sumEmissions = sum(Emissions),
            avgEmissions = mean(Emissions),
            maxEmissions = max(Emissions),
            minEmissions = min(Emissions)
  )

# Create and save plot
png("plot2.png", width = 480, height = 480)
plot(NEIyearBaltimore$year, NEIyearBaltimore$sumEmissions,
     type = "o",
     xlab="Year", 
     ylab = "Total PM2.5 emissions",
)
title(main = "Total PM2.5 emissions 1999 - 2008 in Baltimore City")
dev.off()

# Total emissions from PM2.5 decreased in the Baltimore City from 1999 to 2008.
# (During the period it has not been a decrease from every measure point.)