
#Under developmenp based on PVI Trend Graph
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
