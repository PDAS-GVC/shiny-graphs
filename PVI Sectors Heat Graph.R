#Note: The following code offers two Heatmap Graphs: One offered at Governorate level, other at Community level by Governorate
#Note1:Make sure LastUpdate_Value is a continues value, while Sector and Governorate are as Factor. In Original PVI DB missing values have a "-" wich make R to read it as Character. Review this.

#Install/Load requiered packages
#install.packages("readxl","ggplot2","reshape2")
library(readxl)
library(ggplot2)
library(reshape2)


#Get data soruce
sectors.df <- read_excel("C:/Users/julen/Desktop/180501 PVI-DA Specialist/Reports/Palestine/ECHO17FR/DB/PAL PVI Sectors Endline (Qassem).xlsx")
#Restructure data source
sectors.df.rest <- melt(sectors.df, id=(c("#", "Community","Governorate","Organization","Update")))
#Format data source
names(sectors.df.rest)[names(sectors.df.rest) == 'variable'] <- 'Sector'
sectors.df.rest$value <- sectors.df.rest$value * 100
sectors.df.rest$Governorate <- factor(sectors.df.rest$Governorate)
sectors.df.rest$Sector <- factor(sectors.df.rest$Sector, levels=c("Access","Access to Services","Civil Society Presence","Demography","Education","Energy","Gender","Health","Land Status","Livelihoods","Protection","Relation w/ PA","Settler Violence","Shelter","Transportation","Wash","Total"))
#Aggregate at Governorate level
attach(sectors.df.rest)
sectors.df.rest.gov<-aggregate(sectors.df.rest, by=list(Governorate,Sector), FUN=mean)
colnames(sectors.df.rest.gov)[colnames(sectors.df.rest.gov) == 'Group.1'] <- 'Governorate'
colnames(sectors.df.rest.gov)[colnames(sectors.df.rest.gov) == 'Group.2'] <- 'Sector'


#Graph at Governorate level
dev.new()
#Object definition:
ggplot(data=sectors.df.rest.gov, aes(Sector, Governorate)) + #Change the DF name here
  geom_tile(aes(fill = value)) +
  geom_text(aes(label = round(value, 0)))+
  scale_fill_gradientn(colours=c("white", "lightpink", "lightcoral", "orangered2", "firebrick4"), guide="colorbar") +
  #scale_fill_gradient(low ="white", high ="red")+
  labs(fill = "PVI Endline Value \n(avg. for Gov.)", x="Sectors",y="Governorates")+
  #Tittle and format
  ggtitle(label = "Sectors's Vulnerability by Governorate")+
  theme   (plot.title = element_text(face = 'bold',size = 25.0),
           panel.background = element_blank(),
           panel.grid.minor = element_line(colour="black"),
           legend.title = element_text(size = 15, face='bold'),
           legend.text = element_text(size = 12),
           legend.position="top",
           legend.background = element_rect(fill="gray95", size=.5, linetype="dotted"),
           legend.key.width = unit(2, "cm"),
           axis.title=element_text(size=20,face="bold"),
           axis.text.y = element_text(size=15),
           axis.text.x = element_text(size=15, angle=-90))

#Graph at community level by Governorate
dev.new()
#Object definition:
Gov.filter <- subset(d80219.PVI.SectorialLastUpdate_Restruct1, Governorate == "Bethlehem", select=c(Community, Governorate,Sector,LastUpdate_Value)) #Change here the DF & Governorate
options(repr.plot.width=100, repr.plot.height=8)
ggplot(data=Gov.filter, aes(Sector, Community)) +
  geom_tile(aes(fill = LastUpdate_Value), color = "white") +
  geom_text(aes(label = round(LastUpdate_Value, 0)))+
  scale_fill_gradientn(colours=c("white", "lightpink", "lightcoral", "orangered2", "firebrick4"), values=rescale(c(0, 40, 50, 70, 100)), guide="colorbar") +
  #scale_fill_gradient(low ="white", high ="red")+
  ylab("Communities") +
  xlab("Sectors") +
  labs(fill = "PVI Endline Value")+
  #Tittle and format
  ggtitle(label = "Sectors's Vulnerability for Jenin, Jericho, Salfit & Qalqiliya Communities")+ #Change here the Title of the graph (governorate)
  theme  (plot.title = element_text(face = 'bold',size = 25.0),
          panel.background = element_blank(),
          panel.grid.minor = element_line(colour="black"),
          legend.title = element_text(size = 15, face='bold'),
          legend.text = element_text(size = 12),
          legend.position="top",
          legend.background = element_rect(fill="gray95", size=.5, linetype="dotted"),
          legend.key.width = unit(2, "cm"),
          axis.title=element_text(size=20,face="bold"),
          axis.text.y = element_text(size=15),
          axis.text.x = element_text(size=15, angle=-90))

#EXTRAS:
#Subset for multiple Governorates at the same time
Gov.filter <- subset(d80219.PVI.SectorialLastUpdate_Restruct1, (Governorate == "Salfit" | Governorate == "Jenin" | Governorate == "Jericho" | Governorate == "Qalqiliya"), select=c(Community, Governorate,Sector,LastUpdate_Value))

#The order of item in an axis can be manipulated by reordering the factor levels of the cut variable (e.g.)
> levels(diamonds$cut)
[1] "Fair"      "Good"      "Very Good" "Premium"
[5] "Ideal"
> diamonds$cut <- factor(diamonds$cut, levels = rev(levels(diamonds$cut)))
> levels(diamonds$cut)
[1] "Ideal"     "Premium"   "Very Good" "Good"
[5] "Fair"
