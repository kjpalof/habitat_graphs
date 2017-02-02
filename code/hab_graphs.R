#Habitat graphs
# K.Palof 2-1-17

## Load packages ------
library(tidyverse)
library(gridExtra)
library(extrafont)
loadfonts(device="win")
options(scipen=9999)
theme_set(theme_bw()+ 
            theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank()))
## Load data -------
hab <- read.csv("./data/2016 KGM sediment metals.csv")
constant <- read.csv("./data/PECTEC limits.csv")

levels(hab$creek)

hab %>% filter(creek == "Lower Slate Creek") -> hab_lsc

a <- ggplot(hab_lsc, aes(x=year, y=value)) +geom_point() +facet_wrap(~ metal, scales ="free_y") + 
  ggtitle("Lower Slate Creek")

a
a + geom_hline(data = TEC, aes(yintercept = value) )


hab_lsc %>% filter(metal == "Ag") %>% ggplot(aes(x = year, y= value)) +geom_point() 

constant %>% filter(limit == "TEC") -> TEC
constant %>% filter(limit == "PEC") -> PEC


#------------------------------------------------------------------------------
# SCatterplot with minimum detection levels
# -----------------------------------------------------------------------------
#png(file='jitter.png', res=300, width=7, height=9, units ="in", bg="transparent")

As <- ggplot(subset(hab_lsc, metal %in% "As"), aes(year, value))+ geom_point()+
    geom_hline(data = subset(TEC, metal == "As"), aes(yintercept = value), linetype = "dashed")+
    geom_hline(data = subset(PEC, metal == "As"), aes(yintercept = value))+
    ylab("As (mg/kg)")+ xlab("") + ylim(0,50) +
  theme_bw()+
  theme(legend.position=c(0.85,0.2))+
  theme(panel.grid.major = element_line(colour="transparent"))+
  theme(panel.grid.minor = element_line(colour="transparent"))+
  theme(strip.text = element_blank())+
  theme(strip.background = element_blank())+
  theme(axis.text.x = element_text(size=10,colour="black"),
        axis.title.x = element_text(size=10,colour="black", face="bold"))+
  theme(axis.text.y = element_text(size=10,colour="black"),
        axis.title.y = element_text(size=10,colour="black", face="bold"))+
  theme(plot.background = element_rect(fill = "transparent",colour = NA))+
  theme(text=element_text(size=10,family="Times New Roman"))+
  theme(legend.background = element_rect(fill = "transparent",colour = NA))

Ag <- ggplot(subset(hab_lsc, metal %in% "Ag"), aes(year, value))+ geom_point()+
  #geom_hline(data = subset(TEC, metal == "As"), aes(yintercept = value), linetype = "dashed")+
  #geom_hline(data = subset(PEC, metal == "As"), aes(yintercept = value))+
  ylab("Ag (mg/kg)")+ xlab("") + ylim(0,0.6) +
  theme_bw()+
  theme(legend.position=c(0.85,0.2))+
  theme(panel.grid.major = element_line(colour="transparent"))+
  theme(panel.grid.minor = element_line(colour="transparent"))+
  theme(strip.text = element_blank())+
  theme(strip.background = element_blank())+
  theme(axis.text.x = element_text(size=10,colour="black"),
        axis.title.x = element_text(size=10,colour="black", face="bold"))+
  theme(axis.text.y = element_text(size=10,colour="black"),
        axis.title.y = element_text(size=10,colour="black", face="bold"))+
  theme(plot.background = element_rect(fill = "transparent",colour = NA))+
  theme(text=element_text(size=10,family="Times New Roman"))+
  theme(legend.background = element_rect(fill = "transparent",colour = NA))

Al <- ggplot(subset(hab_lsc, metal %in% "Al"), aes(year, value))+ geom_point()+
  #geom_hline(data = subset(TEC, metal == "As"), aes(yintercept = value), linetype = "dashed")+
  #geom_hline(data = subset(PEC, metal == "As"), aes(yintercept = value))+
  ylab("Al (mg/kg)")+ xlab("") + ylim(0,25000) +
  theme_bw()+
  theme(legend.position=c(0.85,0.2))+
  theme(panel.grid.major = element_line(colour="transparent"))+
  theme(panel.grid.minor = element_line(colour="transparent"))+
  theme(strip.text = element_blank())+
  theme(strip.background = element_blank())+
  theme(axis.text.x = element_text(size=10,colour="black"),
        axis.title.x = element_text(size=10,colour="black", face="bold"))+
  theme(axis.text.y = element_text(size=10,colour="black"),
        axis.title.y = element_text(size=10,colour="black", face="bold"))+
  theme(plot.background = element_rect(fill = "transparent",colour = NA))+
  theme(text=element_text(size=10,family="Times New Roman"))+
  theme(legend.background = element_rect(fill = "transparent",colour = NA))

Cd <- ggplot(subset(hab_lsc, metal %in% "Cd"), aes(year, value))+ geom_point()+
  geom_hline(data = subset(TEC, metal == "Cd"), aes(yintercept = value), linetype = "dashed")+
  geom_hline(data = subset(PEC, metal == "Cd"), aes(yintercept = value))+
  ylab("Cd (mg/kg)")+ xlab("") + ylim(0,5) +
  theme_bw()+
  theme(legend.position=c(0.85,0.2))+
  theme(panel.grid.major = element_line(colour="transparent"))+
  theme(panel.grid.minor = element_line(colour="transparent"))+
  theme(strip.text = element_blank())+
  theme(strip.background = element_blank())+
  theme(axis.text.x = element_text(size=10,colour="black"),
        axis.title.x = element_text(size=10,colour="black", face="bold"))+
  theme(axis.text.y = element_text(size=10,colour="black"),
        axis.title.y = element_text(size=10,colour="black", face="bold"))+
  theme(plot.background = element_rect(fill = "transparent",colour = NA))+
  theme(text=element_text(size=10,family="Times New Roman"))+
  theme(legend.background = element_rect(fill = "transparent",colour = NA))

Cr <- ggplot(subset(hab_lsc, metal %in% "Cr"), aes(year, value))+ geom_point()+
  geom_hline(data = subset(TEC, metal == "Cr"), aes(yintercept = value), linetype = "dashed")+
  geom_hline(data = subset(PEC, metal == "Cr"), aes(yintercept = value))+
  ylab("Cr (mg/kg)")+ xlab("") + ylim(0,160) +
  theme_bw()+
  theme(legend.position=c(0.85,0.2))+
  theme(panel.grid.major = element_line(colour="transparent"))+
  theme(panel.grid.minor = element_line(colour="transparent"))+
  theme(strip.text = element_blank())+
  theme(strip.background = element_blank())+
  theme(axis.text.x = element_text(size=10,colour="black"),
        axis.title.x = element_text(size=10,colour="black", face="bold"))+
  theme(axis.text.y = element_text(size=10,colour="black"),
        axis.title.y = element_text(size=10,colour="black", face="bold"))+
  theme(plot.background = element_rect(fill = "transparent",colour = NA))+
  theme(text=element_text(size=10,family="Times New Roman"))+
  theme(legend.background = element_rect(fill = "transparent",colour = NA))
