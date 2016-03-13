# Question 6: Compare emissions from motor vehicle sources in Baltimore City 
# with emissions from motor vehicle sources in Los Angeles County, California
# (fips == "06037"). Which city has seen greater changes over time in motor 
# vehicle emissions?

# Load packages
library(dplyr)

# Load data into R, make sure the data is located in your working directory
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Grepping for emmisions from motor vehicle sources
motor <- grep("motor", SCC$Short.Name, ignore.case = TRUE)
motor2 <- SCC[motor, ]

# Filtering NEI data for motor vehicle source and Baltimore City and Losa Angeles
NEImotor <- NEI %>%
  filter(fips %in% c("24510", "06037")) %>%
  inner_join(motor2, by = "SCC")

# Group data by year and fips and calculate some statistics.
NEImotorYear <- NEImotor %>%
  group_by(year, fips) %>%
  summarise(totalCount = n(), 
            count = sum(is.na(Emissions)),
            sumEmissions = sum(Emissions),
            avgEmissions = mean(Emissions),
            maxEmissions = max(Emissions),
            minEmissions = min(Emissions)
  )

# Create and save plot
png("plot6.png", width = 480, height = 480)

p <- ggplot(NEImotorYear, 
            aes(x = year, 
                y = sumEmissions, 
                color = fips)
)

p + geom_line() +
  labs(title = "PM2.5 emissions by state 1999 - 2008",
       x = "Year",
       y = "Total PM2.5 emissions") +
  scale_colour_discrete(name = "County", label = c("Los Angeles","Baltimore"))

dev.off()