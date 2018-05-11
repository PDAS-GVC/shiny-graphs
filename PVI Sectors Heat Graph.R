# Note: The following code offers two Heatmap Graphs: One offered at Governorate
# level, other at Community level by Governorate Note1:Make sure LastUpdate_Value
# is a continues value, while Sector and Governorate are as Factor. In Original
# PVI DB missing values have a '-' wich make R to read it as Character. Review
# this.

# Install/Load requiered packages install.packages('readxl','ggplot2','reshape2')
library(readxl)
library(ggplot2)
library(reshape2)

# Get end.value data soruce
sectors.df.end <- read_excel("C:/Users/julen/Desktop/180501 PVI-DA Specialist/Reports/Palestine/ECHO17FR/DB/PAL PVI Sectors Endline (Qassem).xlsx")
# Restructure end.value data source
sectors.df.end.rest <- melt(sectors.df.end, id = (c("#", "Community", "Governorate",
  "Organization", "Update")))
# Format end.value data source
names(sectors.df.end.rest)[names(sectors.df.end.rest) == "variable"] <- "Sector"
sectors.df.end.rest$value <- sectors.df.end.rest$value * 100
sectors.df.end.rest$Governorate <- factor(sectors.df.end.rest$Governorate)
sectors.df.end.rest$Sector <- factor(sectors.df.end.rest$Sector, levels = c("Access",
  "Access to Services", "Civil Society Presence", "Demography", "Education", "Energy",
  "Gender", "Health", "Land Status", "Livelihoods", "Protection", "Relation with PA",
  "Settler Violence", "Shelter", "Transportation", "Wash", "Totals"))
# Aggregate end.value at Governorate level/Sector
attach(sectors.df.end.rest)
sectors.df.end.rest.gov <- aggregate(sectors.df.end.rest, by = list(Governorate,
  Sector), FUN = mean)
drops <- c("Sector", "Governorate", "Community", "Organization", "Update")
sectors.df.end.rest.gov <- sectors.df.end.rest.gov[, !(names(sectors.df.end.rest.gov) %in%
  drops)]
colnames(sectors.df.end.rest.gov)[colnames(sectors.df.end.rest.gov) == "Group.1"] <- "end.Governorate"
colnames(sectors.df.end.rest.gov)[colnames(sectors.df.end.rest.gov) == "Group.2"] <- "end.Sector"
colnames(sectors.df.end.rest.gov)[colnames(sectors.df.end.rest.gov) == "value"] <- "end.value"
colnames(sectors.df.end.rest.gov)[colnames(sectors.df.end.rest.gov) == "#"] <- "end.#"
sectors.df.end.rest.gov["end.#"] <- c(999:(999 - nrow(sectors.df.end.rest.gov) +
  1))
detach(sectors.df.end.rest)

# Get base.value data soruce
sectors.df.base <- read_excel("C:/Users/julen/Desktop/180501 PVI-DA Specialist/Reports/Palestine/ECHO17FR/DB/PAL PVI Sectors Baseline (Qassem).xlsx")
# Restructure base.value data source
sectors.df.base.rest <- melt(sectors.df.base, id = (c("#", "Community", "Governorate",
  "Organization", "Update")))
# Format base.value data source
names(sectors.df.base.rest)[names(sectors.df.base.rest) == "variable"] <- "Sector"
sectors.df.base.rest$value <- sectors.df.base.rest$value * 100
sectors.df.base.rest$Governorate <- factor(sectors.df.base.rest$Governorate)
sectors.df.base.rest$Sector <- factor(sectors.df.base.rest$Sector, levels = c("Access",
  "Access to Services", "Civil Society Presence", "Demography", "Education", "Energy",
  "Gender", "Health", "Land Status", "Livelihoods", "Protection", "Relation with PA",
  "Settler Violence", "Shelter", "Transportation", "Wash", "Totals"))
# Aggregate base.value at Governorate level/Sector
attach(sectors.df.base.rest)
sectors.df.base.rest.gov <- aggregate(sectors.df.base.rest, by = list(Governorate,
  Sector), FUN = mean)
drops <- c("Sector", "Governorate", "Community", "Organization", "Update")
sectors.df.base.rest.gov <- sectors.df.base.rest.gov[, !(names(sectors.df.base.rest.gov) %in%
  drops)]
colnames(sectors.df.base.rest.gov)[colnames(sectors.df.base.rest.gov) == "Group.1"] <- "base.Governorate"
colnames(sectors.df.base.rest.gov)[colnames(sectors.df.base.rest.gov) == "Group.2"] <- "base.Sector"
colnames(sectors.df.base.rest.gov)[colnames(sectors.df.base.rest.gov) == "value"] <- "base.value"
colnames(sectors.df.base.rest.gov)[colnames(sectors.df.base.rest.gov) == "#"] <- "base.#"
sectors.df.base.rest.gov["base.#"] <- c(999:(999 - nrow(sectors.df.base.rest.gov) +
  1))
detach(sectors.df.base.rest)

#Build compiled data source for graph
#Bind end.value + base.value data sources (columns)
sectors.df.rest.gov <- cbind(sectors.df.base.rest.gov, sectors.df.end.rest.gov)
drops <- c("end.Sector", "end.Governorate", "end.#")
sectors.df.rest.gov <- sectors.df.rest.gov[, !(names(sectors.df.rest.gov) %in% drops)]
colnames(sectors.df.rest.gov)[colnames(sectors.df.rest.gov) == "base.Sector"] <- "Sector"
colnames(sectors.df.rest.gov)[colnames(sectors.df.rest.gov) == "base.Governorate"] <- "Governorate"
colnames(sectors.df.rest.gov)[colnames(sectors.df.rest.gov) == "base.#"] <- "#"
# Calculate end.value & base.value average for 'All Governorates'
attach(sectors.df.rest.gov)
sectors.df.rest.gov.temp <- aggregate(sectors.df.rest.gov, by = list(Sector), FUN = mean)
drops <- c("Sector")
sectors.df.rest.gov.temp <- sectors.df.rest.gov.temp[, !(names(sectors.df.rest.gov.temp) %in%
  drops)]
colnames(sectors.df.rest.gov.temp)[colnames(sectors.df.rest.gov.temp) == "Group.1"] <- "Sector"
sectors.df.rest.gov.temp["Governorate"] <- "All Governorates"
detach(sectors.df.rest.gov)
# Bind end.value & base.value for 'All Governorates' with Governorate data source (rows)
sectors.df.rest.gov <- rbind(sectors.df.rest.gov, sectors.df.rest.gov.temp)
rm(sectors.df.rest.gov.temp)
# Create compiled label: end.value(end.value-base.value)
sectors.df.rest.gov["temp1"] <- " ("
sectors.df.rest.gov["change"] <- sectors.df.rest.gov$end.value - sectors.df.rest.gov$base.value
sectors.df.rest.gov["temp2"] <- ")"
sectors.df.rest.gov <- transform(sectors.df.rest.gov, comp.label = paste0(round(end.value,
  0), temp1, round(change, 1), temp2))
drops <- c("temp1", "temp2")
sectors.df.rest.gov <- sectors.df.rest.gov[, !(names(sectors.df.rest.gov) %in% drops)]

#Obtained data sources
#View(sectors.df.end) #Original end.value data source
#View(sectors.df.end.rest) #Restructured end.value data source (sector as value)
#View(sectors.df.end.rest.gov) #Restructured end.value data source aggregated per Governorate
#View(sectors.df.base) #Original base.value data source
#View(sectors.df.base.rest) #Restructured base.value data source (sector as value)
#View(sectors.df.base.rest.gov) #Restructured base.value data source aggregated per Governorate
#View(sectors.df.rest.gov) #Binded  restructured end.value & base.value data source aggregated per Governorate with value for "All Governorates" and compiled label end.value(change)

#PVI Sectors Vulnerability Heatmap Graph at Governorate level
dev.new()
#Object definition:
ggplot(data=sectors.df.rest.gov, aes(Sector, Governorate)) + #Change the DF name here
  geom_tile(aes(fill = end.value)) +
  geom_text(aes(label = comp.label,alpha=0.75))+
  scale_alpha(guide = 'none')+
  scale_fill_gradientn(colours=c("white", "lightpink", "lightcoral", "orangered2", "firebrick4"), guide="colorbar") +
  #scale_fill_gradient(low ="white", high ="red")+
  labs(title="Sectors's Vulnerability by Governorate",subtitle="Average for the Governorate", fill= "PVI Endline Value \n(change)", x="Sectors",y="Governorates",caption= "More darker red = more vulnerability; \nthe number between parenthesis represents the change: endline - baseline values.")+
  #Tittle and format
  #ggtitle(label = "Sectors's Vulnerability by Governorate")+
  theme   (plot.title = element_text(face = 'bold',size = 15),
           plot.subtitle=element_text(size=12),
           plot.caption=element_text(hjust = 0),
           panel.background = element_blank(),
           panel.grid.minor = element_line(colour="black"),
           legend.title = element_text(size = 12, face='bold'),
           legend.text = element_text(size = 9),
           legend.position="top",
           legend.background = element_rect(fill="gray95", size=.5, linetype="dotted"),
           legend.key.width = unit(2, "cm"),
           axis.title=element_text(size=12,face="bold"),
           axis.text.x = element_text(angle=-90))
