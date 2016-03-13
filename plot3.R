# Question 3: Of the four types of sources indicated by the type (point, 
# nonpoint, onroad, nonroad) variable, which of these four sources have seen
# decreases in emissions from 1999–2008 for Baltimore City? Which have seen 
# increases in emissions from 1999–2008? Use the ggplot2 plotting system to 
# make a plot answer this question.

# Load packages
library(dplyr)
library(ggplot2)

# Load data into R, make sure the data is located in your working directory
NEI <- readRDS("summarySCC_PM25.rds")

NEIyearTypeBaltimore <- NEI %>%
  filter(fips == "24510") %>%
  group_by(year, type) %>%
  summarise(totalCount = n(), 
            count = sum(is.na(Emissions)),
            sumEmissions = sum(Emissions),
            avgEmissions = mean(Emissions),
            maxEmissions = max(Emissions),
            minEmissions = min(Emissions)
  )

# Create and save plot
png("plot3.png", width = 480, height = 480)

p <- ggplot(NEIyearTypeBaltimore, 
            aes(x = year, 
                y = sumEmissions, 
                color = type)
)

p + geom_line() +
  labs(title = "Total PM2.5 emissions by type in Baltimore City 1999 - 2008",
       x = "Year",
       y = "Total PM2.5 emissions"
  )

dev.off()

# For the types nonpoint, onroad, nonroad there have been a decrease in 
# PM2.5 emissions in Baltimore City 1999 - 2008. For the type point there has
# been a small increase in PM2.5 emissions.