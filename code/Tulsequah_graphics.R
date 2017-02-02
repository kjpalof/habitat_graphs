###############################################################################
###############################################################################
#
# Graphics for Tulsequah Mine Dolly Varden sampling
#
#  6/22/2016 Kray Van Kirk 
#
###############################################################################
###############################################################################


###############################################################################
# LIBRARIES
###############################################################################
#install.packages('plyr')
#install.packages('ggplot2')
#install.packages('gridExtra')
#install.packages('dplyr')

library(plyr)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(dunn.test)


#------------------------------------------------------------------------------
# READ IN DATA
#------------------------------------------------------------------------------
mine <- read.csv("recon_data2.csv",header=TRUE,sep=",")


#------------------------------------------------------------------------------
# Parse and organize
# -----------------------------------------------------------------------------
mine %>% 
  mutate(Year = factor(year), Metal = factor(metal), Date = factor(date),
         Location = factor(location),
         tmp = as.character(result), Result = as.numeric(tmp), 
         level = as.character(MRL), Level = as.numeric(level)) -> dat

write.table(dat,"crap2.csv")

dat%>%filter(Metal %in% c("Selenium") & Location %in% c("TRB")) -> dattrb
dat%>%filter(Metal %in% c("Selenium") & Location %in% c("TRM")) -> dattrm
dat%>%filter(Metal %in% c("Selenium") & Location %in% c("UTR")) -> datutr

x<-c(dattrb$Result,dattrm$Result,datutr$Result)
g = factor(c(rep("trb",20),rep("trm",52),rep("utr",29)))

dunn.test(x,g)

## Example based on home




dat%>%
  group_by(Year,Location,Metal) %>%
  summarise(Result = mean(Result),sample=n()) -> dat2

write.table(dat2,"crap.csv")
dat3 <- merge(dat, dat2, by = c('Year', 'Location', 'Metal'))


#------------------------------------------------------------------------------
# Colorblind-friendly palette
# -----------------------------------------------------------------------------
cbPalette <- c( "skyblue1", "magenta", "chartreuse")
cbPalette2 <- c( "chartreuse","magenta","skyblue1")
# -----------------------------------------------------------------------------
#cbPalette <- c( "#009E73", "#0072B2", "#D55E00")

#------------------------------------------------------------------------------
# Manual strip names to add units
# -----------------------------------------------------------------------------
metal_names <- c('Arsenic'  = 'As (mg/kg)',
                 'Cadmium'  = 'Cd (mg/kg)',
                 'Copper'   = 'Cu (mg/kg)',
                 'Lead'     = 'Pb (mg/kg)',
                 'Mercury'  = 'Hg (mg/kg)',
                 'Selenium' = 'Se (mg/kg)',
                 'Silver'   = 'Ag (mg/kg)',
                 'Zinc'     = 'Zn (mg/kg)')

dat%>%
  group_by(Year,Location,Metal) %>%
  summarise(Result = mean(Result),sample=n()) -> dat2

library(extrafont)
#font_import()
loadfonts(device="win")
#windowsFonts(Times=windowsFont("Times New Roman"))
#theme_set(theme_bw(base_size=12,base_family="Times New Roman"))


    #------------------------------------------------------------------------------
# SCatterplot with minimum detection levels
# -----------------------------------------------------------------------------
png(file='jitter.png', res=300, width=7, height=9, units ="in", bg="transparent")
ggplot(subset(dat, Metal %in% "Silver"), aes(Year, Result, colour=Location))+
       geom_jitter(aes(shape = Location))+
       geom_hline(aes(yintercept = Level))+
       #facet_wrap(~Metal, scales="free", ncol=3, labeller = as_labeller(metal_names))+
  ylab("Ag (mg/kg)")+
  scale_colour_manual(values=cbPalette2)+
  scale_shape_manual(values=c(19,15,17))+
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
dev.off()



grid_arrange_shared_legend <- function(...) {
  plots <- list(...)
  g <- ggplotGrob(plots[[1]] + theme(legend.position="bottom"))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  grid.arrange(
    do.call(arrangeGrob, lapply(plots, function(x)
      x + theme(legend.position="none"))),
    legend,
    ncol = 1,
    heights = unit.c(unit(1, "npc") - lheight, lheight))
}


A<-ggplot(subset(dat, Metal %in% "Silver"), aes(Year, Result, colour=Location))+
  geom_jitter(aes(shape = Location))+
  geom_hline(aes(yintercept = Level))+
  #facet_wrap(~Metal, scales="free", ncol=3, labeller = as_labeller(metal_names))+
  ylab("Ag (mg/kg)")+
  xlab("")+
  scale_colour_manual(values=cbPalette2)+
  scale_shape_manual(values=c(19,15,17))+
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

B<-ggplot(subset(dat, Metal %in% "Arsenic"), aes(Year, Result, colour=Location))+
  geom_jitter(aes(shape = Location))+
  geom_hline(aes(yintercept = Level))+
  #facet_wrap(~Metal, scales="free", ncol=3, labeller = as_labeller(metal_names))+
  ylab("As (mg/kg)")+
  xlab("")+
  scale_colour_manual(values=cbPalette2)+
  scale_shape_manual(values=c(19,15,17))+
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

C<-ggplot(subset(dat, Metal %in% "Cadmium"), aes(Year, Result, colour=Location))+
  geom_jitter(aes(shape = Location))+
  geom_hline(aes(yintercept = Level))+
  #facet_wrap(~Metal, scales="free", ncol=3, labeller = as_labeller(metal_names))+
  ylab("Cd (mg/kg)")+
  xlab("")+
  scale_colour_manual(values=cbPalette2)+
  scale_shape_manual(values=c(19,15,17))+
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

D<-ggplot(subset(dat, Metal %in% "Copper"), aes(Year, Result, colour=Location))+
  geom_jitter(aes(shape = Location))+
  geom_hline(aes(yintercept = Level))+
  #facet_wrap(~Metal, scales="free", ncol=3, labeller = as_labeller(metal_names))+
  ylab("Cu (mg/kg)")+
  xlab("")+
  scale_colour_manual(values=cbPalette2)+
  scale_shape_manual(values=c(19,15,17))+
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

E<-ggplot(subset(dat, Metal %in% "Mercury"), aes(Year, Result, colour=Location))+
  geom_jitter(aes(shape = Location))+
  geom_hline(aes(yintercept = Level))+
  #facet_wrap(~Metal, scales="free", ncol=3, labeller = as_labeller(metal_names))+
  ylab("Hg (mg/kg)")+
  xlab("")+
  scale_colour_manual(values=cbPalette2)+
  scale_shape_manual(values=c(19,15,17))+
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

FF<-ggplot(subset(dat, Metal %in% "Lead"), aes(Year, Result, colour=Location))+
  geom_jitter(aes(shape = Location))+
  geom_hline(aes(yintercept = Level))+
  #facet_wrap(~Metal, scales="free", ncol=3, labeller = as_labeller(metal_names))+
  ylab("Pb (mg/kg)")+
  xlab("")+
  scale_colour_manual(values=cbPalette2)+
  scale_shape_manual(values=c(19,15,17))+
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

G<-ggplot(subset(dat, Metal %in% "Selenium"), aes(Year, Result, colour=Location))+
  geom_jitter(aes(shape = Location))+
  geom_hline(aes(yintercept = Level))+
  #facet_wrap(~Metal, scales="free", ncol=3, labeller = as_labeller(metal_names))+
  ylab("Se (mg/kg)")+
  xlab("")+
  scale_colour_manual(values=cbPalette2)+
  scale_shape_manual(values=c(19,15,17))+
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

H<-ggplot(subset(dat, Metal %in% "Zinc"), aes(Year, Result, colour=Location))+
  geom_jitter(aes(shape = Location))+
  geom_hline(aes(yintercept = Level))+
  #facet_wrap(~Metal, scales="free", ncol=3, labeller = as_labeller(metal_names))+
  ylab("Zn (mg/kg)")+
  xlab("")+
  scale_colour_manual(values=cbPalette2)+
  scale_shape_manual(values=c(19,15,17))+
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


#library(grid)
#library(gridExtra)
png(file='jitter.png', res=300, width=7, height=9, units ="in", bg="transparent")  
grid_arrange_shared_legend(A,B,C,D,E,FF,G,H)
dev.off()






#------------------------------------------------------------------------------
# Year + Location + Metal
# -----------------------------------------------------------------------------
dat %>%
  group_by(Year,Location, Metal) %>%
  summarise(mg = mean(Result), var = var(Result), sample = n()) -> mg


png(file='annual_means.png', res=300, width=7, height=9, units ="in", bg="transparent")  
ggplot(mg,aes(Year, mg, group = Location, colour=Location))+
       geom_point(position = position_jitter(w=0.03,h=0.01), aes(shape = Location))+
       geom_line(position = position_jitter(w=0.03,h=0.01))+
       geom_errorbar(aes(ymin = mg - 1*sqrt(var), ymax = mg + 1*sqrt(var)), 
                     width = 0.1, position = position_jitter(w=0.03,h=0.01))+
       facet_wrap(~Metal,scales="free",ncol=3, labeller = as_labeller(metal_names))+
  scale_colour_manual(values=cbPalette)+
  ylab("Concententration")+
  theme_bw()+
  theme(legend.position=c(0.85,0.2))+
  theme(panel.grid.major = element_line(colour="transparent"))+
  theme(panel.grid.minor = element_line(colour="transparent"))+
  theme(strip.text = element_text(size=10, face="bold"))+
  theme(axis.text.x = element_text(size=10,colour="black"),
        axis.title.x = element_text(size=10,colour="black", face="bold"))+
  theme(axis.text.y = element_text(size=10,colour="black"),
        axis.title.y = element_text(size=10,colour="black", face="bold"))+
  theme(plot.background = element_rect(fill = "transparent",colour = NA))+
  theme(legend.background = element_rect(fill = "transparent",colour = NA))
dev.off()


#------------------------------------------------------------------------------
# Location +  Metal
# -----------------------------------------------------------------------------
dat %>%
  group_by(Location, Metal) %>%
  summarise(mg = mean(Result), var = var(Result), sample = n()) -> mg2


png(file='location_means.png', res=300, width=7, height=9, units ="in", bg="transparent")  
ggplot(mg2,aes(Location, mg, label = sample))+
       geom_point(aes(shape = Location))+
       geom_errorbar(aes(ymin = mg - 1*sqrt(var), ymax = mg + 1*sqrt(var)),width = 0.1)+
       geom_label(colour="black", fontface="bold")+
       facet_wrap(~Metal,scales="free",ncol=3)+
  #facet_wrap(~Metal,scales="free",ncol=3, labeller = as_labeller(metal_names))+
  ylab("")+
  xlab("")+
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
  scale_y_discrete()
  theme(plot.background = element_rect(fill = "transparent",colour = NA))+
  theme(text=element_text(size=10,family="Times New Roman"))+
  theme(legend.position = "none")
dev.off()

fmt_dcimals <- function(decimals=2){
  # return a function responpsible for formatting the 
  # axis labels with a given number of decimals 
  function(x) as.character(round(x,decimals))
}
       
A<-ggplot(subset(mg2, Metal %in% "Silver"),aes(Location, mg, label = sample))+
  geom_point(aes(shape = Location))+
  geom_errorbar(aes(ymin = mg - 1*sqrt(var), ymax = mg + 1*sqrt(var)),width = 0.1)+
  geom_label(colour="black", fontface="bold")+
  ylab("Ag (mg/kg)")+
  xlab("")+
  scale_y_continuous(breaks=c(0,0.02,0.04,0.06),labels=c("0.00","0.02","0.04","0.06"))+
  scale_x_discrete(limits=c("UTR","TRM","TRB"))+
  theme_bw()+
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
  theme(legend.position = "none")

B<-ggplot(subset(mg2, Metal %in% "Arsenic"),aes(Location, mg, label = sample))+
  geom_point(aes(shape = Location))+
  geom_errorbar(aes(ymin = mg - 1*sqrt(var), ymax = mg + 1*sqrt(var)),width = 0.1)+
  geom_label(colour="black", fontface="bold")+
  ylab("As (mg/kg)")+
  xlab("")+
  scale_y_continuous(breaks=c(0.35,0.65,0.95,1.25),labels=c("0.35","0.65","0.90","1.25"))+
  theme_bw()+
  scale_x_discrete(limits=c("UTR","TRM","TRB"))+
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
  theme(legend.position = "none")

C<-ggplot(subset(mg2, Metal %in% "Cadmium"),aes(Location, mg, label = sample))+
  geom_point(aes(shape = Location))+
  geom_errorbar(aes(ymin = mg - 1*sqrt(var), ymax = mg + 1*sqrt(var)),width = 0.1)+
  geom_label(colour="black", fontface="bold")+
  ylab("Cd (mg/kg)")+
  xlab("")+
  theme_bw()+
  scale_y_continuous(breaks=c(0.1,0.2,0.3,0.4),labels=c("0.10","0.20","0.30","0.40"))+
  scale_x_discrete(limits=c("UTR","TRM","TRB"))+
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
  theme(legend.position = "none")

D<-ggplot(subset(mg2, Metal %in% "Copper"),aes(Location, mg, label = sample))+
  geom_point(aes(shape = Location))+
  geom_errorbar(aes(ymin = mg - 1*sqrt(var), ymax = mg + 1*sqrt(var)),width = 0.1)+
  geom_label(colour="black", fontface="bold")+
  ylab("Cu (mg/kg)")+
  xlab("")+
  theme_bw()+
  scale_y_continuous(breaks=c(0,3,6,9),labels=c("0.00","3.00","6.00","9.00"))+
  scale_x_discrete(limits=c("UTR","TRM","TRB"))+
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
  theme(legend.position = "none")

E<-ggplot(subset(mg2, Metal %in% "Mercury"),aes(Location, mg, label = sample))+
  geom_point(aes(shape = Location))+
  geom_errorbar(aes(ymin = mg - 1*sqrt(var), ymax = mg + 1*sqrt(var)),width = 0.1)+
  geom_label(colour="black", fontface="bold")+
  ylab("Hg (mg/kg)")+
  xlab("")+
  theme_bw()+
  scale_y_continuous(breaks=c(0.02,0.04,0.06,0.08),labels=c("0.02","0.04","0.06","0.08"))+
  scale_x_discrete(limits=c("UTR","TRM","TRB"))+
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
  theme(legend.position = "none")

FF<-ggplot(subset(mg2, Metal %in% "Lead"),aes(Location, mg, label = sample))+
  geom_point(aes(shape = Location))+
  geom_errorbar(aes(ymin = mg - 1*sqrt(var), ymax = mg + 1*sqrt(var)),width = 0.1)+
  geom_label(colour="black", fontface="bold")+
  ylab("Pb (mg/kg)")+
  xlab("")+
  theme_bw()+
  scale_y_continuous(breaks=c(0.1,0.2,0.3,0.4),labels=c("0.10","0.20","0.30","0.40"))+
  scale_x_discrete(limits=c("UTR","TRM","TRB"))+
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
  theme(legend.position = "none")

G<-ggplot(subset(mg2, Metal %in% "Selenium"),aes(Location, mg, label = sample))+
  geom_point(aes(shape = Location))+
  geom_errorbar(aes(ymin = mg - 1*sqrt(var), ymax = mg + 1*sqrt(var)),width = 0.1)+
  geom_label(colour="black", fontface="bold")+
  ylab("Se (mg/kg)")+
  xlab("")+
  theme_bw()+
  scale_y_continuous(breaks=c(2,3,4),labels=c("2.00","3.00","4.00"))+
  scale_x_discrete(limits=c("UTR","TRM","TRB"))+
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
  theme(legend.position = "none")

H<-ggplot(subset(mg2, Metal %in% "Zinc"),aes(Location, mg, label = sample))+
  geom_point(aes(shape = Location))+
  geom_errorbar(aes(ymin = mg - 1*sqrt(var), ymax = mg + 1*sqrt(var)),width = 0.1)+
  geom_label(colour="black", fontface="bold")+
  ylab("Zn (mg/kg)")+
  xlab("")+
  theme_bw()+
  scale_y_continuous(breaks=c(120,140,160,180),labels=c("120","140","160","180"))+
  scale_x_discrete(limits=c("UTR","TRM","TRB"))+
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
  theme(legend.position = "none")

png(file='location_means.png', res=300, width=7, height=9, units ="in", bg="transparent")  
grid.arrange(A,B,C,D,E,FF,G,H, ncol=2)
dev.off()


#------------------------------------------------------------------------------
# Assorted messy crap
# -----------------------------------------------------------------------------
# ggplot(dat, aes(Metal,Result, fill=Location))+geom_boxplot()+
#   facet_wrap(~Metal, scales="free", labeller = as_labeller(metal_names))+
#   ylab("Concententration")+
#   theme_bw()+
#   theme(legend.position=c(0.85,0.2))+
#   theme(panel.grid.major = element_line(colour="transparent"))+
#   theme(panel.grid.minor = element_line(colour="transparent"))+
#   theme(strip.text = element_text(size=10, face="bold"))+
#   theme(axis.text.x = element_text(size=10,colour="black"),
#         axis.title.x = element_text(size=10,colour="black", face="bold"))+
#   theme(axis.text.y = element_text(size=10,colour="black"),
#         axis.title.y = element_text(size=10,colour="black", face="bold"))+
#   theme(plot.background = element_rect(fill = "transparent",colour = NA))+
#   theme(legend.background = element_rect(fill = "transparent",colour = NA))
# 
# 
# 
# ggplot(dat3, aes(Year, Result, fill=Location))+geom_boxplot()+
#       facet_wrap(~Metal, scales="free", ncol=3, labeller = as_labeller(metal_names))+
#   scale_colour_manual(values=cbPalette)+
#   theme_bw()+
#   theme(legend.position=c(0.85,0.2))+
#   theme(panel.grid.major = element_line(colour="transparent"))+
#   theme(panel.grid.minor = element_line(colour="transparent"))+
#   theme(strip.text = element_text(size=10, face="bold"))+
#   #theme(axis.text.x = element_text(size=10,colour="black"),
#    #     axis.title.x = element_text(size=10,colour="black", face="bold"))+
#   theme(axis.text.y = element_text(size=10,colour="black"),
#         axis.title.y = element_text(size=10,colour="black", face="bold"))+
#   theme(plot.background = element_rect(fill = "transparent",colour = NA))+
#   theme(legend.background = element_rect(fill = "transparent",colour = NA))
# 
# 
# dat %>%
#   group_by(Year, Metal) %>%
#   summarise(mg = mean(Result), var = var(Result)) -> mg3
# 
# 
# ggplot(mg3,aes(Year, mg, colour=Year))+geom_point(position = "dodge")+
#   geom_errorbar(aes(ymin = mg - 2*sqrt(var), ymax = mg + 2*sqrt(var)), width = 0.1, position = "dodge")+
#   facet_wrap(~Metal,scales="free", ncol=3, labeller = as_labeller(metal_names))+
#   scale_colour_manual(values=cbPalette)+
#   ylab("Concententration")+
#   theme_bw()+
#   theme(legend.position=c(0.8,0.2))+
#   theme(panel.grid.major = element_line(colour="transparent"))+
#   theme(panel.grid.minor = element_line(colour="transparent"))+
#   theme(strip.text = element_text(size=10, face="bold"))+
#   theme(axis.text.x = element_text(size=10,colour="black"),
#         axis.title.x = element_text(size=10,colour="black", face="bold"))+
#   theme(axis.text.y = element_text(size=10,colour="black"),
#         axis.title.y = element_text(size=10,colour="black", face="bold"))+
#   theme(plot.background = element_rect(fill = "transparent",colour = NA))+
#   theme(legend.background = element_rect(fill = "transparent",colour = NA))

