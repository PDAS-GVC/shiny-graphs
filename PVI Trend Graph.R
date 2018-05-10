#Note: The following code offers 3 Scattered dots graphs depending on how to color the points and the level of desagregation: color by Governorate or by Year of the endline for all communities; 
#color by Year of endline for communities by Governorate 
#Note1:Make sure baseline and endline (PVI values) are as Double. In Original PVI DB missing values have a "-" wich make R to read it as Character. Review this.

#under test: create and add to the db the agregation at Gov level to plot all in the same graph
#x<-aggregate(PVI_totals[, c(5,8,10)],list(PVI_totals$Governorate), mean)
#newdf <- melt.data.frame(PVI_totals, x,keep.rownames=TRUE)


#Load Libraries
library(readxl)
library(ggplot2)
library(ggrepel)
library(reshape2)

#Get data soruce
PVI_totals <- read_excel("C:\\Users\\julen\\Desktop\\180501 PVI-DA Specialist\\Reports\\Palestine\\ECHO17FR\\DB\\PAL PVI Totals AllUpdates.xlsx", 
col_types=c("numeric","text","text","text","numeric","numeric","numeric","numeric"))

#Format data source
names(PVI_totals)[names(PVI_totals) == '2017 (Second Update)'] <- 'endline'
PVI_totals["Geo.Level"]<- "Community"
PVI_totals$change <- PVI_totals$endline - PVI_totals$baseline
PVI_totals$baseline <- PVI_totals$baseline * 100
PVI_totals$endline <- PVI_totals$endline * 100
PVI_totals$change <- PVI_totals$change * 100

#Agregate at Governorate level and integrate in data frame
a <- aggregate.data.frame(PVI_totals$baseline, list(PVI_totals$Governorate), mean)
b <- aggregate.data.frame(PVI_totals$endline, list(PVI_totals$Governorate), mean)
c <- aggregate.data.frame(PVI_totals$Y2016, list(PVI_totals$Governorate), mean)
d <- aggregate.data.frame(PVI_totals$Y2017, list(PVI_totals$Governorate), mean)
colnames(a)[colnames(a) == 'Group.1'] <- 'Governorate'
colnames(a)[colnames(a) == 'x'] <- 'baseline'
colnames(b)[colnames(b) == 'x'] <- 'endline'
colnames(c)[colnames(c) == 'x'] <- 'Y2016'
colnames(d)[colnames(d) == 'x'] <- 'Y2017'
a["#"] <- c(999:(999-nrow(a)+1))
a["Community"]<- a$Governorate
a["Geo.Level"]<- "Governorate"
a["Organization"]<- NA
a["endline"]<- b$endline
a["change"]<- (a$endline - a$baseline)
a["Y2016"]<- c$Y2016
a["Y2017"]<- d$Y2017
PVI_totals <- rbind(PVI_totals, a)
rm(a,b,c,d)

#PVI TREND GRAPH: COLOURED BY GOVERNORATE
dev.new()
#Aesthetics definition
splot <- ggplot() + geom_pointrange(aes(x = change,y = endline,ymin = endline,ymax = baseline,colour = Governorate,group= Governorate,shape = factor(Geo.Level),size=factor(Geo.Level)),data=PVI_totals,linetype = 3,na.rm = FALSE)
#Plot content formatting
  lim <- unique(PVI_totals$Governorate)
  splot+
  scale_colour_hue(limits= lim, h=c(0,360)+15,c=100,l=40,h.start=1,direction= 1)+ #block if manual color (below 2 lines) selected
  #cols <- c("firebrick3","darkkhaki","olivedrab3","gray35","dodgerblue4","darkgoldenrod4","darkgoldenrod1","sienna1","lightskyblue3","darkolivegreen") #Define colours ff manual colors want to be set for Governorate category
  #scale_colour_manual(limits= lim, values= cols)+ #If manual colors want to be set for Governorate category  
  scale_shape_manual(values=c(8, 16))+
  scale_size_manual(values=c(0.5,1), guide=FALSE)+
  scale_x_continuous(name="Change: End-Baseline values", limits=c(min(PVI_totals$change)-2,max(PVI_totals$change)+2),
                     breaks=seq(round(min(PVI_totals$change),-1),round(max(PVI_totals$change)+1),2.5),expand=c(0.0, 0)) +
  scale_y_continuous(name="PVI Endline Value", limits=c(min(PVI_totals$endline,PVI_totals$baseline)-2,max(PVI_totals$endline,PVI_totals$baseline)+2),
                     breaks=seq(round(min(PVI_totals$endline,PVI_totals$baseline),-1),round(max(PVI_totals$endline,PVI_totals$baseline),+1),5),expand=c(0.0, 0))+
#Labelling
  labs(caption="NB: Moving Ahead: reduced vulnerability (Endline-Baseline) and Endline value < 50%; Catching-up: reduced Vul. and Endline ≥ 50%; Losing Momentum: increased Vul. and Endline < 50%; Falling Behing: increased Vul. and Endline ≥ 50%")+
  geom_text_repel(mapping=aes(x = change,y = endline,label = Community),
       data=subset(PVI_totals, change < quantile(PVI_totals$change, probs = 0.05) | change > quantile(PVI_totals$change, probs = 0.95) | 
       endline < quantile(PVI_totals$endline, probs = 0.05) | endline > quantile(PVI_totals$endline, probs = 0.95)), #change values here to set the range of points to be labelled
       max.iter=5000,force=1,
       colour = "Grey10",size = 3,
       segment.color= "black",segment.size=0.5,segment.alpha=0.25,
       nudge_x = 0,nudge_y = 1,box.padding=2,point.padding=0.5,parse = FALSE)+
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
  theme(line = element_blank(),title = element_text(face = 'bold',size = 10.0),plot.background = element_blank(),
        plot.title = element_text(face = 'bold',size = 15.0),
        axis.title = element_text(face = 'bold',colour = "black", size= 12),
        axis.text = element_text(face = 'bold',colour = "black", size=9),
        axis.ticks = element_line(),
        legend.text = element_text(size=9),
        legend.title = element_text(face = 'bold', size =12),
        legend.position = "top",
        legend.direction = 'horizontal',
        legend.title.align=0.5)+
#Legend Formatting
  guides(colour = "colorbar",shape = "legend")+
  guides(colour = guide_legend(title = "Governorate (point line represents change)", nrow = 1, title.position = "top", keywidth=0.5, keyheight=0.5, default.unit="inch"))+
  guides(shape = guide_legend("Geographical Level", nrow = 1, title.position = "top", keywidth=0.5, keyheight=0.5, default.unit="inch"))
  
  


#Graph: Color by endline year for all communities
dev.new()
#Object definition: plot, point color, legend limits
x<-geom_pointrange(aes(x = change,y = endline,ymin = endline,ymax = baseline,colour = LastUpdate_Year,group= LastUpdate_Year))
splot <- ggplot() + geom_pointrange(aes(x = change,y = endline,ymin = endline,ymax = baseline,colour = LastUpdate_Year,group= LastUpdate_Year),data=PVI_totals,linetype = 3,shape = 16,size = 1.0,na.rm = FALSE)
cols <- c("sandybrown","saddlebrown") 
lim <- c("2016", "2017")
#Plot building
splot + scale_colour_manual(limits= lim, values= cols)+
  coord_cartesian(xlim = c(-28,11),ylim = c(30,80)) +
  annotation_logticks(sides="b")+
  labs(x= 'change: End-Baseline values', y= 'PVI Endline Value')+
  geom_text_repel(data=subset(PVI_totals, change < -10 | change > 5 | endline < 40 | endline > 70), 
                  aes(x),
                  max.iter= 3e3,
                  force=1,
                  colour = "Grey10",
                  size = 4.0,
                  nudge_x = 0.0,
                  nudge_y = 1.0,
                  #min.segment.length = unit(0, 'lines'),
                  box.padding=1,
                  label.size=0.7,
                  point.padding=0.5,
                  segment.color= "black",
                  segment.size=0.5,
                  #arrow=arrow(length = unit(0.01, 'npc')),
                  check_overlap=TRUE, parse = FALSE)+
  #Quadrant lines
  geom_hline(data=PVI_totals,yintercept = 50.0,colour = "grey13",linetype = 2) +
  geom_vline(data=PVI_totals,xintercept = 0.0,colour = "grey13",linetype = 2) +
  #fill the quadrants
  geom_rect(aes(xmin=-30,xmax=0,ymin=0,ymax=50),alpha=0.075,fill="green") +
  geom_rect(aes(xmin=-30,xmax=0,ymin=50,ymax=100),alpha=0.075,fill="navyblue") +
  geom_rect(aes(xmin=0,xmax=15,ymin=50,ymax=100),alpha=0.075,fill="darkred")+
  geom_rect(aes(xmin=0,xmax=15,ymin=0,ymax=50),alpha=0.075,fill="gold")+
  #Quadrant Label
  geom_text(data=PVI_totals,x = -26,y = 80,label = 'Catching Up',colour = "darkblue",size = 8.0,parse = FALSE)+
  geom_text(data=PVI_totals,x = -26,y = 30,label = 'Moving Ahead',colour = "darkgreen",size = 8.0,parse = FALSE)+
  geom_text(data=PVI_totals,x = 9,y = 80,label = 'Falling Behind',colour = "darkred",size = 8.0,parse = FALSE)+
  geom_text(data=PVI_totals,x = 9,y = 30,label = 'Losing Momentum',colour = "gold4",size = 8.0,parse = FALSE)+
  #Tittle and format
  ggtitle(label = 'Protection Vulnerability Index: Trend Analysis for 160 communities')+
  #theme_classic()
  theme(line = element_blank(),rect = element_blank(),title = element_text(face = 'bold',size = 10.0),plot.background = element_blank(),
        plot.title = element_text(face = 'bold',size = 20.0),
        axis.title = element_text(face = 'bold',colour = "grey70", size= 20),
        axis.text = element_text(face = 'bold',colour = "grey70", size=15),
        axis.ticks = element_line(),
        legend.background = element_blank(),legend.key = element_blank(),
        legend.text = element_text(size=12),
        legend.title = element_text(face = 'bold', size =15),
        legend.position = "top",
        #legend.justification="left",
        legend.direction = 'horizontal',
        legend.title.align=0.5,
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor = element_line(),
        strip.background = element_blank())+


guides(colour = guide_legend(title = "Year of Endline Value (point line represents change)", nrow = 1, title.position = "top", keywidth=1, keyheight=0.5, default.unit="inch"))

#Color by Year for communities by Governorate
dev.new()
#Object definition: plot, point color, legend limits
splot <- ggplot() + geom_pointrange(aes(x = change,y = endline,ymin = endline,ymax = baseline,colour = LastUpdate_Year,group= LastUpdate_Year), data=subset(PVI_totals, Governorate == "Bethlehem"),linetype = 3,shape = 19,size = 1.0,na.rm = FALSE) #change the Governorate here
cols <- c("sandybrown","saddlebrown") 
lim <- c("2016", "2017")
#Plot building
splot + scale_colour_manual(limits= lim, values= cols)+
  coord_cartesian(xlim = c(-28,11),ylim = c(30,80)) +
  annotation_logticks(sides="b")+
  labs(x= 'change: End-Baseline values', y= 'PVI Endline Value')+
  #Point Labels
  geom_text_repel(data=subset(PVI_totals, Governorate == "Bethlehem"), #change the Governorate here
                  aes(x = change,y = endline,label = Community),
                  max.iter= 150,
                  force=1,
                  colour = "Grey10",
                  size = 5.0,
                  nudge_x = 0.0,
                  nudge_y = 1.0,
                  #min.segment.length = unit(0, 'lines'),
                  box.padding=1,
                  label.size=NA,
                  point.padding=0.5,
                  segment.color= "black",
                  segment.size=0.5,
                  #arrow=arrow(length = unit(0.01, 'npc')),
                  parse = FALSE)+
  #Quadrant lines
  geom_hline(data=PVI_totals,yintercept = 50.0,colour = "grey13",linetype = 2) +
  geom_vline(data=PVI_totals,xintercept = 0.0,colour = "grey13",linetype = 2) +
  #fill the quadrants
  geom_rect(aes(xmin=-30,xmax=0,ymin=0,ymax=50),alpha=0.075,fill="green") +
  geom_rect(aes(xmin=-30,xmax=0,ymin=50,ymax=100),alpha=0.075,fill="navyblue") +
  geom_rect(aes(xmin=0,xmax=15,ymin=50,ymax=100),alpha=0.075,fill="darkred")+
  geom_rect(aes(xmin=0,xmax=15,ymin=0,ymax=50),alpha=0.075,fill="gold")+
  #Quadrant Label
  geom_text(data=PVI_totals,x = -26,y = 80,label = 'Catching Up',colour = "darkblue",size = 8.0,parse = FALSE)+
  geom_text(data=PVI_totals,x = -26,y = 30,label = 'Moving Ahead',colour = "darkgreen",size = 8.0,parse = FALSE)+
  geom_text(data=PVI_totals,x = 9,y = 80,label = 'Falling Behind',colour = "darkred",size = 8.0,parse = FALSE)+
  geom_text(data=PVI_totals,x = 9,y = 30,label = 'Losing Momentum',colour = "gold4",size = 8.0,parse = FALSE)+
  #Tittle and format
  ggtitle(label = 'Protection Vulnerability Index: Trend Analysis for Bethlehem Governorates Communities')+ #change the Governorate here
  #theme_classic()
  theme(line = element_blank(),rect = element_blank(),title = element_text(face = 'bold',size = 10.0),plot.background = element_blank(),
        plot.title = element_text(face = 'bold',size = 20.0),
        axis.title = element_text(face = 'bold',colour = "grey70", size= 20),
        axis.text = element_text(face = 'bold',colour = "grey70", size=15),
        axis.ticks = element_line(),
        legend.background = element_blank(),legend.key = element_blank(),
        legend.text = element_text(size=12),
        legend.title = element_text(face = 'bold', size =15),
        legend.position = "top",
        #legend.justification="left",
        legend.direction = 'horizontal',
        legend.title.align=0.5,
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.minor = element_line(),
        strip.background = element_blank())+
  guides(colour = guide_legend(title = "Year of Endline Value (point line represents change)", nrow = 1, title.position = "top", keywidth=1, keyheight=0.5, default.unit="inch"))

#Extras:
#Include Label repel for the specific governorate
& (change < -5 | change > 2 | endline < 50 | endline > 65)
#Data subset for various governorates
Governorate == "Jenin" | Governorate == "Jericho" | Governorate == "Qalqiliya" | Governorate == "Salfit"
