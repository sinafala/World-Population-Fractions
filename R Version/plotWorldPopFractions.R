# Sam Clark
# 2020-01-09

# start fresh
rm(list=ls())

# load necessary packages
# List of necessary packages
list.of.packages <- c(
  'openxlsx'
  # ,'rJava'
  # ,'xlsx'
  ,'dplyr'
  ,'ggplot2'
  ,'reshape2'
  ,'scales'
  ,'stringr'
  ,'gridExtra'
  ,'stringr'
)
# identify required packages that are not already installed
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
# install required packages that are not already installed
if(length(new.packages)) install.packages(new.packages,type="binary")
# load the required packages
loaded <- lapply(list.of.packages, require, character.only = TRUE)
# check that they were all loaded: equality shoudl be TRUE
length(list.of.packages) == length(loaded)

# URL for total both-sex population Excel file from WPP 2019
URL = "https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/EXCEL_FILES/1_Population/WPP2019_POP_F01_1_TOTAL_POPULATION_BOTH_SEXES.xlsx"
# temp file to store the downloaded Excel file
worldPops = tempfile(fileext = ".xlsx")
# download the Excel file
download.file(url = URL, destfile = worldPops, mode="wb")

# extract the population estimates
popEstimates <- read.xlsx(worldPops,sheet=1,startRow=17,colNames=TRUE)
# extract the population projections, medium variant
popProjections <- read.xlsx(worldPops,sheet=2,startRow=17,colNames=TRUE)
# view results 
# View(popEstimates)
# View(popProjections)

# rename region column
colnames(popEstimates)[3] <- 'Region'
colnames(popProjections)[3] <- 'Region'

# world regions we're interested in
regions <- c('World','Africa','China','India','Europe','Latin America and the Caribbean','Northern America')

# create a filter list from the world regions 
filterList <- grepl(paste("^",paste(tolower(regions), collapse="$|^"),"$",sep=""), tolower(popEstimates$Region))

# periods for population estimates
estimatePeriods <- c('1950','1970','1990','2010')
# filter the estimates to include rows corresponding to the world regions and columns for the estimate periods 
regionEstimates <- filter(popEstimates, filterList)[,c('Region',estimatePeriods)]
# remove potential duplicate rows, ignoring region column (differing cases)
dups <- duplicated(regionEstimates[,-1])
regionEstimates <- regionEstimates[!dups,]
# View(regionEstimates)

# periods for the population projections
projectionPeriods <- c('2020','2040','2060','2080','2100')
# filter the projections to include rows corresponding to the world regions and columns for the projection periods 
regionProjections <- filter(popProjections, filterList)[,c('Region',projectionPeriods)]
# remove potential duplicate rows, ignoring region column (differing cases)
dups <- duplicated(regionProjections[,-1])
regionProjections <- regionProjections[!dups,]
# View(regionProjections)

# glue things together into a matrix with rows for regions and columns for population numbers1 
# use merge to ensure that regions are matched
pops <- merge(regionEstimates,regionProjections,by.x='Region',by.y='Region')
# clean up some names
pops[which(pops[,1]=="Latin America and the Caribbean"),1] <- "South America"
pops[which(pops[,1]=="Northern America"),1] <- "North America"
# title case for the region names
pops[,1] <- str_to_title(pops[,1])
# transform numbers stored as strings to numeric 
pops[,-1] <- sapply(pops[,-1], as.numeric)
# have a look
dim(pops)
str(pops)
pops
# View(pops)

# add a row for all other regions
# first identify row with the world
pops.world <- which(pops$Region=="World")
# total up identified regions and substract from the world
region.other <- pops[pops.world,-1] - colSums(pops[-pops.world,-1])
# add the region
region.other <- c("Other",region.other)
# create matching names
names(region.other) <- colnames(pops)
# add the other world region to pops
pops <- rbind(pops,region.other)
# check, column totals should be twice world population
colSums(pops[,-1]) == 2*pops[pops.world,-1]
# have a look
pops
# View(pops)

# calculate the fraction of world population in each region, including the world
fracs.region <- pops
# first identify row with the world
fracs.world <- which(fracs.region$Region=="World")
for (i in 1:nrow(fracs.region)) {
  fracs.region[i,2:10] <- fracs.region[i,2:10]/pops[fracs.world,2:10]
}
# have a look: there are missing regions so these regions to not add up to world total
fracs.region
# drop the world and keep just the regions
fracs.region <- fracs.region[-fracs.world,]
# check: does it add to 1
colSums(fracs.region[,-1])
# have a look
fracs.region
# View(fracs.region)

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
# factorize and sort regsion variable
unique(fracs.gg$Region)
fracs.gg$Region <- factor(fracs.gg$R,levels=c("Africa","China","India","Europe","North America","South America","Other"))
# have a look
str(fracs.gg)
# View(fracs.gg)

# plot
# Stacked bar version
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
region.colors <- gg_color_hue(7)
region.colors[5] <- "pink"
region.colors[6] <- "lightblue"
region.colors[7] <- "darkgreen"
ggplot(fracs.gg) +
  geom_line(aes(x=Year, y=Fraction, group=Region, colour=Region, size=Region)) +
  geom_vline(xintercept = 4.5,size=2,colour="white") +
  # geom_line(aes(x=Year, y=Fraction, group=Region, colour=Region, size=Region)) +
  scale_size_manual(values = c(2.5,1,1,1,1,1,1)) +
  scale_color_manual(values=rev(region.colors)) +
  geom_point(aes(x=Year, y=Fraction, group=Region, colour=Region)) +
  theme(axis.text = element_text(size=15),
        axis.title = element_text(size=20),
        legend.text = element_text(size=15),
        legend.title = element_text(size=20),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0))) +
  labs(y="Fraction of World Population") +
  scale_y_continuous(labels=scales::percent_format(accuracy = 1)) +
  theme(legend.key.height=unit(1.5,"line"),
        legend.key.width=unit(2,"line"),
        legend.title=element_text(size=20),
        legend.text=element_text(size =15),
        legend.key = element_rect(size = 4),
        legend.key.size = unit(1.5, 'lines'),
        legend.justification = c(-0.1,1.1),
        legend.position = c(0,1)) +
  ggtitle("Data from World Population Prospects 2019: https://bit.ly/2NryzPp")

# save the plot
ggsave("Region Fraction of Global Population by Time.pdf")
ggsave("Region Fraction of Global Population by Time.jpg")

# make region an ordered factor
fracs.gg.stack <- fracs.gg
unique(fracs.gg.stack$Region)
fracs.gg.stack$Region <- factor(fracs.gg.stack$R,levels=c("Other","South America","North America","Europe","India","China","Africa"))
# have a look
str(fracs.gg.stack)
# View(fracs.gg)

# Stacked bar version
ggplot(fracs.gg.stack, aes(fill=Region, y=Fraction, x=Year)) + 
  geom_bar(position="fill", stat="identity") +
  scale_y_continuous(labels=scales::percent_format(accuracy = 1),expand = c(0,0)) +
  scale_fill_manual(values=region.colors) +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(axis.text = element_text(size=15),
        axis.title = element_text(size=20),
        legend.text = element_text(size=15),
        legend.title = element_text(size=20),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0))) +
  labs(y="Fraction of World Population") +
  geom_hline(yintercept = seq(0.25,0.75,0.25)) +
  ggtitle("Data from World Population Prospects 2019: https://bit.ly/2NryzPp")

# save the plot
ggsave("Region Fraction of Global Population by Time - Stacked Bar.pdf")
ggsave("Region Fraction of Global Population by Time - Stacked Bar.jpg")


