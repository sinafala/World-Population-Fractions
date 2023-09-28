# Sam Clark
# 2022-09-30

# start fresh
rm(list=ls())

# load necessary packages
# List of necessary packages
list.of.packages <- c(
  'openxlsx'
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

# URL for total both-sex population Excel file from WPP 2022
URL = "https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/EXCEL_FILES/2_Population/WPP2022_POP_F01_1_POPULATION_SINGLE_AGE_BOTH_SEXES.xlsx"
# temp file to store the downloaded Excel file
worldPops = tempfile(fileext = ".xlsx")
# downlojad the Excel file: adjust timeout value set to 600 seconds right now
options(timeout = max(600, getOption("timeout")))
download.file(url = URL, destfile = worldPops, mode="wb", )

# extract the population estimates
popEstimates <- read.xlsx(worldPops,sheet=1,startRow=17,colNames=TRUE)
# extract the population projections, medium variant
popProjections <- read.xlsx(worldPops,sheet=2,startRow=17,colNames=TRUE)
# view results 
# View(popEstimates)
# View(popProjections)

# sum ages for total population
# first, convert to numeric
ageCols <- 12:112
popEstimates[,ageCols] <- apply(popEstimates[,ageCols],2,function(x) as.numeric(as.character(x)))
popProjections[,ageCols] <- apply(popProjections[,ageCols],2,function(x) as.numeric(as.character(x)))
# sum age columns
popEstimates["totalPop"] <- rowSums(popEstimates[,ageCols],na.rm=TRUE)
popProjections["totalPop"] <- rowSums(popProjections[,ageCols],na.rm=TRUE)
# view results
# View(popEstimates)
# View(popProjections)
# save with population totals
save(popEstimates,file="popEstimates")
save(popProjections,file="popProjections")

# rename region column
colnames(popEstimates)[3] <- 'Region'
colnames(popProjections)[3] <- 'Region'

# keep just region, year, and totalPop
estimates <- popEstimates[,c("Region","Year","totalPop")]
projections <- popProjections[,c("Region","Year","totalPop")]
# view results
# View(estimates)
# View(projections)

# create wide versions of the data
estimatesWide <- reshape(estimates, idvar = "Region", timevar = "Year", direction = "wide")
projectionsWide <- reshape(projections, idvar = "Region", timevar = "Year", direction = "wide")
# view results
# View(estimatesWide)
# View(projectionsWide)

# create final estimates dataframe
# world regions we're interested in
regions <- c('World','Africa','China','India','Europe','Latin America and the Caribbean','Northern America')

# create a filter list from the world regions 
filterList <- grepl(paste("^",paste(tolower(regions), collapse="$|^"),"$",sep=""), tolower(estimatesWide$Region))

# periods for population estimates
estimatePeriods <- c('totalPop.1950','totalPop.1970','totalPop.1990','totalPop.2010','totalPop.2020')
# filter the estimates to include rows corresponding to the world regions and columns for the estimate periods 
regionEstimates <- filter(estimatesWide, filterList)[,c('Region',estimatePeriods)]
# remove potential duplicate rows, ignoring region column (differing cases)
dups <- duplicated(regionEstimates[,-1])
regionEstimates <- regionEstimates[!dups,]
# View(regionEstimates)

# create final estimates dataframe
# periods for the population projections
projectionPeriods <- c('totalPop.2030','totalPop.2040','totalPop.2050','totalPop.2060','totalPop.2070','totalPop.2080','totalPop.2090','totalPop.2100')
# filter the projections to include rows corresponding to the world regions and columns for the projection periods 
regionProjections <- filter(projectionsWide, filterList)[,c('Region',projectionPeriods)]
# remove potential duplicate rows, ignoring region column (differing cases)
dups <- duplicated(regionProjections[,-1])
regionProjections <- regionProjections[!dups,]
# View(regionProjections)

# glue things together into a matrix with rows for regions and columns for population numbers1 
# use merge to ensure that regions are matched
pops <- merge(regionEstimates,regionProjections,by.x='Region',by.y='Region')
# remove 'totalPop.' from column names
popCols <- dim(pops)[2]
colYears <- str_sub(colnames(pops)[2:popCols],-4,-1)
colnames(pops) <- c(colnames(pops)[1],colYears)
# View(pops)
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
# save pops
save(pops,file="pops.RData")
# save pops as xlsx
write.xlsx(pops,file="pops.xlsx")

# calculate the fraction of world population in each region, including the world
fracs.region <- pops
# first identify row with the world
fracs.world <- which(fracs.region$Region=="World")
for (i in 1:nrow(fracs.region)) {
  fracs.region[i,2:popCols] <- fracs.region[i,2:popCols]/pops[fracs.world,2:popCols]
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
# save fracs.region
save(fracs.region,file="fracs.RData")
# save fracs.region as xlsx
write.xlsx(fracs.region,file="fracs.xlsx",overwrite=TRUE)

# transform to long format with columns for region, period, and fraction
fracs.gg <- melt(fracs.region,ic=c(Region))
# rename the columns
colnames(fracs.gg) <- c("Region","Year","Fraction")
# factorize the period variable
fracs.gg$Year <- factor(fracs.gg$Year)
# factorize and sort regsion variable
unique(fracs.gg$Region)
fracs.gg$Region <- factor(fracs.gg$Region,levels=c("Africa","China","India","Europe","Northern America","South America","Other"))
# have a look
fracs.gg
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
  geom_vline(xintercept = 5.2,size=2,colour="white") +
  # geom_line(aes(x=Year, y=Fraction, group=Region, colour=Region, size=Region)) +
  scale_size_manual(values = c(2.5,1,1,1,1,1,1)) +
  scale_color_manual(values=rev(region.colors)) +
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
        legend.title=element_text(size=25),
        legend.text=element_text(size =20),
        legend.key = element_rect(size = 4),
        legend.key.size = unit(1.5, 'lines'),
        legend.justification = c(-0.1,1.1),
        legend.position = c(0,1)) +
  ggtitle("Data from World Population Prospects 2022: https://bit.ly/2NryzPp")

# save the plot
ggsave("Region Fraction of Global Population by Time.pdf")
ggsave("Region Fraction of Global Population by Time.jpg")

# make region an ordered factor
fracs.gg.stack <- fracs.gg
unique(fracs.gg.stack$Region)
fracs.gg.stack$Region <- factor(fracs.gg.stack$R,levels=c("Other","South America","North America","Europe","India","China","Africa"))
# have a look
str(fracs.gg.stack)
# # View(fracs.gg)

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
  ggtitle("Data from World Population Prospects 2022: https://bit.ly/2NryzPp")

# save the plot
ggsave("Region Fraction of Global Population by Time - Stacked Bar.pdf")
ggsave("Region Fraction of Global Population by Time - Stacked Bar.jpg")

# Shorter stacked bar
ggplot(fracs.gg.stack, aes(fill=Region, y=Fraction, x=Year)) + 
  geom_bar(position="fill", stat="identity") +
  scale_y_continuous(labels=scales::percent_format(accuracy = 1),expand = c(0,0)) +
  scale_fill_manual(values=region.colors) +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(title = element_text(size=7),
        axis.text = element_text(size=10),
        axis.title = element_text(size=10),
        legend.text = element_text(size=10), 
        legend.title = element_text(size=10),
        axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))) +
  labs(y="Fraction of World Population") +
  geom_hline(yintercept = seq(0.25,0.75,0.25)) +
  ggtitle("Data from World Population Prospects 2022: https://bit.ly/2NryzPp")

ggsave("Region Fraction of Global Population by Time - Stacked Bar Short.pdf", height = 4, width = 7)
ggsave("Region Fraction of Global Population by Time - Stacked Bar Short.jpg", height = 4, width = 7)
 
