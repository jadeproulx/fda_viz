library(ggplot2)
library(zipcode)
library(ggmap)
register_google(key="AIzaSyDTS5HU0hRzTpUYhnFzUj2YFc0S2DfiVYo")

setwd("~/Documents/Data Projects/fda_viz")

#=============================================
# Get data
#=============================================

if (!file.exists("data")) {
  dir.create("data")
}

file_url <- "https://www.accessdata.fda.gov/scripts/inspsearch/genexcel.cfm?textSearch=&classification=&state=&project=Foodborne%20Biological%20Hazards%2CPesticides%20and%20Chemical%20Contaminants%2CMolecular%20Biology%20and%20Natural%20Toxins%2CFood%20and%20Color%20Additives%20Petition%20Review%2CTechnical%20Assistance%3A%20Food%20and%20Cosmetics%2CFood%20Composition%7C%20Standards%7C%20Labeling%20and%20Econ%2CColors%20and%20Cosmetics%20Technology&inspDateEndFrom=&inspDateEndTo=&classificationDecision=&start=1&end=53663&country=&city=&zip=&center=&sortBy=&district="
download.file(file_url, destfile="./data/fda_recall_data.csv", method='curl')
fda_recall_data <- read.csv("./data/fda_recall_data.csv", header=TRUE)

date_downloaded <- date()

#=============================================
# Explore data
#=============================================

x <- which(fda_recall_data$Country=="US")
length(x)
# 4,495 inspections were not carried out in the US.

freq.district.plot <- ggplot(subset(fda_recall_data, 
                                 Classification=="OAI" | 
                                   Classification=="VAI"), 
                          aes(x=District)) +
  geom_bar(stat="count", position="dodge", aes(fill=Classification)) +
  theme_bw()
freq.district.plot

#=============================================
# Add longitude and latitute to dataset
#=============================================

data(zipcode)
fda_recall_data$Clean.Zip.Code <- clean.zipcodes(fda_recall_data$Zip.Code)
fda_recall_data_cleanzip <- merge(fda_recall_data, zipcode[,c("zip", "latitude", "longitude")], 
                                  by.x="Clean.Zip.Code", by.y="zip")


#=============================================
# Visualize inspection data on maps
#=============================================

USA <- get_map(location="United States", zoom=3)

full.plot <- ggmap(USA) +
  geom_point(data=fda_recall_data_cleanzip, 
             aes(x=longitude, y=latitude, col=Classification), size=0.5)
full.plot

full.density.plot <- ggmap(USA) +
  stat_density2d(data=fda_recall_data_cleanzip, 
             aes(x=longitude, y=latitude, col=Classification), size=0.5)
full.density.plot


recall.plot <- ggmap(USA) +
  geom_point(data=subset(fda_recall_data_cleanzip, Classification != "NAI"), 
             aes(x=longitude, y=latitude, col=Classification), size=0.5)
recall.plot
