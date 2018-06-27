#Note: The following code offers 3 Scattered dots graphs depending on how to color the points and the level of desagregation: color by Governorate or by Year of the endline for all communities;
#color by Year of endline for communities by Governorate
#Note1:Make sure baseline and endline (PVI values) are as Double. In Original PVI DB missing values have a "-" wich make R to read it as Character. Review this.

#under test: create and add to the db the agregation at Gov level to plot all in the same graph
#x<-aggregate(PVI_totals[, c(5,8,10)],list(PVI_totals$Governorate), mean)
#newdf <- melt.data.frame(PVI_totals, x,keep.rownames=TRUE)

#Install/Load requiered packages
###install.packages("readxl", "ggplot2", "ggrepel", "reshape2")
library(readxl)
library(ggplot2)
library(ggrepel)
library(reshape2)

# Get data soruce
PVI_totals <- read_excel("C:\\Users\\julen\\Desktop\\180501 PVI-DA Specialist\\Reports\\Palestine\\ECHO17FR\\DB\\PAL PVI Totals AllUpdates.xlsx",
  col_types = c("numeric", "text", "text", "text", "numeric", "numeric", "numeric",
    "numeric"))

# Format data source
names(PVI_totals)[names(PVI_totals) == "2017 (Second Update)"] <- "endline"
PVI_totals["Geo.Level"] <- "Community"
PVI_totals$change <- PVI_totals$endline - PVI_totals$baseline
PVI_totals$baseline <- PVI_totals$baseline * 100
PVI_totals$endline <- PVI_totals$endline * 100
PVI_totals$change <- PVI_totals$change * 100

# Agregate at Governorate level and integrate in data frame
attach(PVI_totals)
a <- aggregate.data.frame(PVI_totals, by=list(Governorate), mean)
a["#"] <- c(999:(999 - nrow(a) + 1))
a["Governorate"] <- a$Group.1
a["Community"] <- a$Governorate
a["Geo.Level"] <- "Governorate"
a1 <- a[,-1]
PVI_totals <- rbind(PVI_totals, a1)
rm(a, a1)

#PVI TREND GRAPH: COLOURED BY GOVERNORATE
#Definition of objects
  lim <- unique(PVI_totals$Governorate)
  cols <- c("firebrick3","darkkhaki","olivedrab3","gray35","dodgerblue4","darkgoldenrod4","darkgoldenrod2","sienna1","lightskyblue4","darkolivegreen") #Define colours ff manual colors want to be set for Governorate category
  MA<- PVI_totals[PVI_totals$change < 0 & PVI_totals$endline < 50,]
  CU<- PVI_totals[PVI_totals$change < 0 & PVI_totals$endline > 50,]
  LM<- PVI_totals[PVI_totals$change > 0 & PVI_totals$endline < 50,]
  FB<- PVI_totals[PVI_totals$change > 0 & PVI_totals$endline > 50,]
dev.new()
#Aesthetics definition
splot <- ggplot() + geom_pointrange(aes(x = change,y = endline,ymin = endline,ymax = baseline,colour = Governorate,group= Governorate,shape = factor(Geo.Level),size=factor(Geo.Level)),data=PVI_totals,linetype = 3,na.rm = FALSE)
#Data points formating
splot+
  #scale_colour_hue(limits= lim, h=c(0,360)+15,c=100,l=40,h.start=1,direction= 1)+ #If hue colors want to be set for Governorate category. diable object "col" and F(x) "scale_colour_manual"
  scale_colour_manual(limits= lim, values= cols)+
  scale_shape_manual(values=c(8, 19))+
  scale_size_manual(values=c(0.5,1), guide=FALSE)+
  scale_x_continuous(name="Change: End-Baseline values", limits=c(min(PVI_totals$change)-2,max(PVI_totals$change)+2),
                     breaks=seq(round(min(PVI_totals$change),-1),round(max(PVI_totals$change)+1),2.5),expand=c(0.0, 0)) +
  scale_y_continuous(name="PVI Endline Value", limits=c(min(PVI_totals$endline,PVI_totals$baseline)-2,max(PVI_totals$endline,PVI_totals$baseline)+2),
                     breaks=seq(round(min(PVI_totals$endline,PVI_totals$baseline),-1),round(max(PVI_totals$endline,PVI_totals$baseline),+1),5),expand=c(0.0, 0))+
#Labelling
  labs(title= "PVI Trend Analysis for Communities", subtitle= "Categorizing Vulnerability change between baseline and endline", caption="NB: Moving Ahead: reduced vulnerability (Endline-Baseline) and Endline value < 50%; Catching-up: reduced Vul. and Endline ≥ 50%; Losing Momentum: increased Vul. and Endline < 50%; Falling Behing: increased Vul. and Endline ≥ 50%;\nLabelled communities represent the outlying 2.5% of all communities")+
  geom_text_repel(mapping=aes(x = change,y = endline,label = Community),
      data=subset(MA, change < quantile(PVI_totals$change, probs = 0.025) | endline < quantile(PVI_totals$endline, probs = 0.025)),
      max.iter=3000,force=1,
      colour = "Grey10",size = 3,
      segment.color= "black",segment.size=0.5,segment.alpha=0.25,
      nudge_x = 0,nudge_y = 1,box.padding=2,point.padding=1,
      xlim=c(NA,-10),ylim=c(NA,50))+
  geom_text_repel(mapping=aes(x = change,y = endline,label = Community),
      data=subset(CU,change < quantile(PVI_totals$change, probs = 0.025) | endline > quantile(PVI_totals$endline, probs = 0.975)),
      max.iter=3000,force=1,
      colour = "Grey10",size = 3,
      segment.color= "black",segment.size=0.5,segment.alpha=0.25,
      nudge_x = 0,nudge_y = 1,box.padding=2,point.padding=1,
      xlim=c(NA,-17),ylim=c(50,NA))+
  geom_text_repel(mapping=aes(x = change,y = endline,label = Community),
      data=subset(LM,change > quantile(PVI_totals$change, probs = 0.975) | endline < quantile(PVI_totals$endline, probs = 0.025)),
      max.iter=3000,force=1,
      colour = "Grey10",size = 3,
      segment.color= "black",segment.size=0.5,segment.alpha=0.25,
      nudge_x = 0,nudge_y = 1,box.padding=2,point.padding=1,
      xlim=c(4,NA),ylim=c(NA,45))+
  geom_text_repel(mapping=aes(x = change,y = endline,label = Community),
      data=subset(FB,change > quantile(PVI_totals$change, probs = 0.975) | endline > quantile(PVI_totals$endline, probs = 0.975)),
      max.iter=3000,force=1,
      colour = "Grey10",size = 3,
      segment.color= "black",segment.size=0.5,segment.alpha=0.25,
      nudge_x = 0,nudge_y = 1,box.padding=2,point.padding=1,
      xlim=c(5,NA),ylim=c(50,NA))+

#Plot background
  #Quadrant lines
  geom_hline(data=PVI_totals,yintercept = 50.0,colour = "grey13",linetype = 2) +
  geom_vline(data=PVI_totals,xintercept = 0.0,colour = "grey13",linetype = 2) +
  #fill the quadrants
  geom_rect(aes(xmin=min(PVI_totals$change)-2,xmax=0,ymin=min(PVI_totals$endline,PVI_totals$baseline)-2,ymax=50),alpha=0.075,fill="green") +
  geom_rect(aes(xmin=min(PVI_totals$change)-2,xmax=0,ymin=50,ymax=max(PVI_totals$endline,PVI_totals$baseline)+2),alpha=0.075,fill="navyblue") +
  geom_rect(aes(xmin=0,xmax=max(PVI_totals$change)+2,ymin=50,ymax=max(PVI_totals$endline,PVI_totals$baseline)+2),alpha=0.075,fill="darkred")+
  geom_rect(aes(xmin=0,xmax=max(PVI_totals$change)+2,ymin=min(PVI_totals$endline,PVI_totals$baseline)-2,ymax=50),alpha=0.075,fill="gold")+
  #Quadrant Label
  geom_text(data=PVI_totals,x = min(PVI_totals$change)+2,y = max(PVI_totals$endline,PVI_totals$baseline),label = 'Catching Up',colour = "darkblue",size = 5.0,parse = FALSE)+
  geom_text(data=PVI_totals,x = min(PVI_totals$change)+2,y = min(PVI_totals$endline,PVI_totals$baseline),label = 'Moving Ahead',colour = "darkgreen",size = 5.0,parse = FALSE)+
  geom_text(data=PVI_totals,x = max(PVI_totals$change)-2,y = max(PVI_totals$endline,PVI_totals$baseline),label = 'Falling Behind',colour = "darkred",size = 5.0,parse = FALSE)+
  geom_text(data=PVI_totals,x = max(PVI_totals$change)-2,y = min(PVI_totals$endline,PVI_totals$baseline),label = 'Losing Momentum',colour = "gold4",size = 5.0,parse = FALSE)+
#General Plot Formatting
  theme(line = element_blank(),
        plot.title = element_text(face = 'bold',size = 15.0),
        plot.subtitle=element_text(size=12,face="plain"),
        plot.caption=element_text(hjust = 0,face="plain",size=9),
        axis.title = element_text(face = 'bold',colour = "black", size= 12),
        axis.text = element_text(face = 'bold',colour = "black", size=9),
        axis.ticks = element_line(),
        legend.text = element_text(size=9),
        legend.title = element_text(face = 'bold', size =12),
        legend.position = "top",
        legend.direction = 'horizontal',
        legend.title.align=0.5)+
#Legend Formatting
  guides(colour = "colorbar", shape = "legend") + guides(colour = guide_legend(title = "Governorate (point line represents change)",
  nrow = 1, title.position = "top", keywidth = 0.5, keyheight = 0.5, default.unit = "inch")) +
  guides(shape = guide_legend("Geographical Level", nrow = 1, title.position = "top",
    keywidth = 0.5, keyheight = 0.5, default.unit = "inch"))
