# Sam Clark
# 2019-03-09

# start fresh
rm(list=ls())

# load libraries
library(rJava)
library(xlsx)
library(dplyr)
library(ggplot2)
library(reshape2)
library(scales)
library(stringr)
library(gridExtra)

# URL for total both-sex population Excel file from WPP 2017
URL = "https://population.un.org/wpp/DVD/Files/1_Indicators%20(Standard)/EXCEL_FILES/1_Population/WPP2017_POP_F01_1_TOTAL_POPULATION_BOTH_SEXES.xlsx"
# temp file to store the downloaded Excel file
worldPops = tempfile(fileext = ".xlsx")
# download the Excel file
download.file(url = URL, destfile = worldPops, mode="wb")

# extract the population estimates
popEstimates <- read.xlsx(worldPops, 1, startRow=17, endRow=290, sringsAsFactors=FALSE, as.data.frame=TRUE)
# extract the population projections, medium variant
popProjections <- read.xlsx(worldPops, 2, startRow=17, endRow=290, sringsAsFactors=FALSE, as.data.frame=TRUE)

# world regions we're interested in
regions <- c('World','Africa','China','India','Europe','Latin America and the Caribbean','Northern America')

# create a filter list from the world regions 
filterList <- grepl(paste("^",paste(tolower(regions), collapse="$|^"),"$",sep=""), tolower(popEstimates$Region..subregion..country.or.area..))

# periods for population estimates
estimatePeriods <- c('X1950','X1970','X1990','X2010')
# filter the estimates to include rows corresponding to the world regions and columns for the estimate periods 
regionEstimates <- filter(popEstimates, filterList)[,estimatePeriods]
# View(regionEstimates)

# periods for the population projections
projectionPeriods <- c('X2020','X2040','X2060','X2080','X2100')
# filter the projections to include rows corresponding to the world regions and columns for the projection periods 
regionProjections <- filter(popProjections, filterList)[,projectionPeriods]
# View(regionProjections)

# glue things together into a matrix with rows for regions and columns for population numbers
pops <- cbind(regions,regionEstimates,regionProjections,stringsAsFactors=FALSE)
pops[which(pops[,1]=="Latin America and the Caribbean"),1] <- "South America"
pops[which(pops[,1]=="Northern America"),1] <- "North America"
dim(pops)
# have a look
pops

# calculate the fraction of world population in each region: world is row 1
fracs.region <- pops
for (i in 1:nrow(fracs.region)) {
  fracs.region[i,2:10] <- fracs.region[i,2:10]/pops[1,2:10]
}
# drop the world and keep just the regions
fracs.region <- fracs.region[2:nrow(fracs.region),]
# have a look
fracs.region

# transform to long format with columns for region, period, and fraction
fracs.gg <- melt(fracs.region[,c(1,2:10)],ic=c(region))
# remove the 'X' the is prepended to the region column names
fracs.gg$variable <- str_sub(fracs.gg$variable,-4,-1)
# have a look
fracs.gg

# rename the columns
colnames(fracs.gg) <- c("Region","Year","Fraction")
# factorize the period variable
fracs.gg$Year <- factor(fracs.gg$Year)
# have a look
str(fracs.gg)

# plot
ggplot(fracs.gg) +
  geom_line(aes(x=Year, y=Fraction, group=Region, colour=Region, size=Region)) +
  geom_vline(xintercept = 4.5,size=2,colour="white") +
  geom_line(aes(x=Year, y=Fraction, group=Region, colour=Region, size=Region)) +
  scale_size_manual(values = c(2.5,1,1,1,1,1)) +
  geom_point(aes(x=Year, y=Fraction, group=Region, colour=Region)) +
  theme(axis.text = element_text(size=25),
        axis.title = element_text(size=30),
        legend.text = element_text(size=25),
        legend.title = element_text(size=30),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0))) +
  labs(y="Fraction of World Population") +
  scale_y_continuous(labels=scales::percent_format(accuracy = 1)) +
  theme(legend.key.height=unit(1.5,"line"),
        legend.key.width=unit(2,"line"),
        legend.title=element_text(size=30),
        legend.text=element_text(size =25),
        legend.key = element_rect(size = 4),
        legend.key.size = unit(1.5, 'lines'),
        legend.justification = c(-0.1,1.1),
        legend.position = c(0,1)) 

# save the plot
ggsave("Region Fraction of Global Population by Time.pdf")




