library(ggplot2)
library(dplyr)
library(ggmap)
library(zipcode)
library(countrycode)

register_google(key="AIzaSyDTS5HU0hRzTpUYhnFzUj2YFc0S2DfiVYo")

setwd("~/Documents/Data Projects/fda_viz")

#=============================================
# Get & read data
#=============================================

if (!file.exists("data")) {
  dir.create("data")
}

file_url <- "https://www.accessdata.fda.gov/scripts/inspsearch/genexcel.cfm?textSearch=&classification=&state=&project=Foodborne%20Biological%20Hazards%2CPesticides%20and%20Chemical%20Contaminants%2CMolecular%20Biology%20and%20Natural%20Toxins%2CFood%20and%20Color%20Additives%20Petition%20Review%2CTechnical%20Assistance%3A%20Food%20and%20Cosmetics%2CFood%20Composition%7C%20Standards%7C%20Labeling%20and%20Econ%2CColors%20and%20Cosmetics%20Technology&inspDateEndFrom=&inspDateEndTo=&classificationDecision=&start=1&end=53663&country=&city=&zip=&center=&sortBy=&district="
download.file(file_url, destfile="./data/fda_recall_data.csv", method='curl')
date_downloaded <- date()

fda_recall_data <- read.csv("./data/fda_recall_data.csv", header=TRUE)

fda_recall_data$Country.Name <- countrycode(fda_recall_data$Country, 
                                            origin="iso2c", destination="country.name")

#=============================================
# Explore data
#=============================================

# Are all inspections carried out in the US?
country_count <- fda_recall_data %>%
  group_by(Country.Name) %>%
  tally(sort=TRUE)
head(country_count)
# 4,495 inspections were not carried out in the US.

plot.country <- ggplot(subset(country_count, Country.Name != "United States"),
                       aes(x=reorder(Country.Name, -n), y=n)) +
  geom_bar(stat="identity") +
  labs(x="", y="") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
plot.country

#============================
# Which firm has had the most inspections 
firm_count <- fda_recall_data %>%
  group_by(Firm.Name) %>%
  tally(sort=TRUE)
head(firm_count,10)
# CHECK BIMBO'S LOCATION AND SEE IF IT IS CLOSE TO CFSAN CENTERS
# MAKE IT A NCIE TABLE. 

# Which firm has had the most OAI inspections? 
firm_class_count <- fda_recall_data %>%
  group_by(Firm.Name, Classification) %>%
  tally(sort=TRUE)

head(subset(firm_class_count, Classification=="OAI"), 10)

plot.OAI.firm <- ggplot(subset(firm_class_count, Classification=="OAI" & n >3),
                       aes(x=reorder(Firm.Name, -n), y=n)) +
  geom_bar(stat="identity") +
  labs(x="", y="# OAI Inspections") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 75, hjust = 1))
plot.OAI.firm


#==========================================
# Which project category has had the most inspections? 
project_count <- fda_recall_data %>%
  group_by(Project) %>%
  tally(sort=TRUE)
# FOODBORNE BIOLOGICAL HAZARDS ARE INSPECECTED MOST OFTEN
# IN THE LAST 5 YEARS, 4 OUT OF 7 PROJECT AREAS HAVE REPORTED INSPECTIONS

project_class_count <- fda_recall_data %>%
  group_by(Project, Classification) %>%
  tally(sort=TRUE)

head(subset(project_class_count, Classification=="OAI"))
head(subset(project_class_count, Classification=="VAI"))
# WHILE FOODBORNE BIOLOGICAL HAZARDS ARE MOST OFTEN INSPECTED, 
# THEY HAVEN'T REPORTED ANYONE FAILING FOR THAT REASON IN 5 YEARS


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

USA <- get_map(location="United States", zoom=2)

full.plot <- ggmap(USA) +
   geom_point(data=subset(fda_recall_data_cleanzip, Classification=="NAI"), 
             aes(x=longitude, y=latitude, col=Classification), size=0.5, alpha=0.3) +
  geom_point(data=subset(fda_recall_data_cleanzip, Classification!="NAI"), 
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
