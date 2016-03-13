# Question 5: How have emissions from motor vehicle sources changed from 1999–2008
# in Baltimore City 

# Load packages
library(dplyr)

# Load data into R, make sure the data is located in your working directory
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Grepping for emmisions from motor vehicle sources
motor <- grep("motor", SCC$Short.Name, ignore.case = TRUE)
motor2 <- SCC[motor, ]

# Filtering NEI data for motor vehicle source and Baltimore City
NEImotor <- NEI %>%
  filter(fips == "24510") %>%
  inner_join(motor2, by = "SCC")

# Group data by year and calculate some statistics.
NEImotorYearBaltimore <- NEImotor %>%
  group_by(year) %>%
  summarise(totalCount = n(), 
            count = sum(is.na(Emissions)),
            sumEmissions = sum(Emissions),
            avgEmissions = mean(Emissions),
            maxEmissions = max(Emissions),
            minEmissions = min(Emissions)
  )

# Create and save plot
png("plot5.png", width = 480, height = 480)
plot(NEImotorYearBaltimore$year, NEImotorYearBaltimore$sumEmissions,
     type = "o",
     xlab="Year", 
     ylab = "Total PM2.5 emissions",
)
title(main = "PM2.5 emissions from motor vehicles in Baltimore City")
dev.off()

# In Baltimore City PM2.5 emissions from motor vehicle sources are more or less 
# the same 2008 as they were 1999. (From the data we can see that there have 
# been high values measured between theese years)