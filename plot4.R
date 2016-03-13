# Question 4: Across the United States, how have emissions from coal combustion- 
# related sources changed from 1999–2008?

# Load packages
library(dplyr)

# Load data into R, make sure the data is located in your working directory
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Take a look at distinct Short.Name to see what to grep for
distinctShortName <- SCC %>% 
  distinct(Short.Name) %>% 
  select(Short.Name)

# Grepping for coal combustion emissions
combCoal <- grep("comb.*coal", SCC$Short.Name, ignore.case = TRUE)
combCoal2 <- SCC[combCoal, ]

# Filtering NEI data for coal combustion emissions, looks OK
NEIcombCoal <- NEI %>%
  inner_join(combCoal2, by = "SCC")

# Group data by year and calculate some statistics for Emissions from coal 
# combustion per year.
NEIcombCoalyear <- NEIcombCoal %>%
  group_by(year) %>%
  summarise(totalCount = n(), 
            count = sum(is.na(Emissions)),
            sumEmissions = sum(Emissions),
            avgEmissions = mean(Emissions),
            maxEmissions = max(Emissions),
            minEmissions = min(Emissions)
  )

# Create and save plot
png("plot4.png", width = 480, height = 480)
plot(NEIcombCoalyear$year, NEIcombCoalyear$sumEmissions,
     type = "o",
     xlab="Year", 
     ylab = "Total PM2.5 Emissions",
)
title(main = "Total PM2.5 Emissions from coal combustion sources")
dev.off()

# Across the United States, emissions from coal combustion-related sources 
# have decreased from 1999–2008?