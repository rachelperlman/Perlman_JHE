setwd("//Users/Rachel/Desktop/Perlman et al. 2023 Files")

## Libraries
library(plyr)
library(dplyr)
library(tidyr)
library(reshape2)
library(stringr)

library(lme4)
library(lmerTest)
library(ggplot2)
library(cowplot)
library(visreg)
library(grid)
library(gridExtra)
library(ggpubr)

#### FIGURE 1 | Feeding above ground and below ground in dry season vs. wet season ####   
df <- read.csv("df.FA.FB2.csv",header=T,sep=",", fill=T)

str_pad_custom <- function(season){
  new_labels <- stringr::str_pad(season,15, "right")
  return(new_labels)} #yellowgreen and darkgreen

fig1 <- ggplot(subset(df, !is.na(season)), aes(x=season, y=sum/F.Total*100, fill=Activity3)) +
  geom_boxplot(position=position_dodge(0.5), width=0.4, colour="black", lwd = 0.5, outlier.shape = NA, key_glyph=rectangle_key_glyph(color="black", size=0.4, padding=margin(1,1,1,1)))

fig1 <- fig1 +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border=element_rect(size = 1, colour = "black")) + 
  theme(axis.text = element_text(size = 10), axis.title.x = element_text(size = 10), axis.title.y = element_text( size = 10)) +
  theme(plot.margin = unit(c(2.25,1.25,2.25,1.25), "lines")) +
  theme(axis.ticks.x = element_blank(), axis.ticks.y = element_line(size =0.5), axis.ticks.length = unit(.1, "cm")) +
  scale_fill_manual(values=c("yellowgreen", "darkgreen"), labels = str_pad_custom) +
  theme(axis.title.x = element_blank(), axis.text.x = element_text(color="black", size =10, vjust=1),
        axis.title.y = element_text(vjust = 1.25, angle = 90, size = 10), axis.text.y = element_text(color="black", size=8)) +
  geom_point(position = position_jitterdodge(dodge.width = 0.5, jitter.width = 0.1), shape=16, size=0.5, colour="black") +
  ylab("Percent of feeding time") + scale_y_continuous(labels = scales::number_format(accuracy = 0.1)) +
  theme(legend.title = element_blank(), legend.text=element_text(size=8)) + theme(legend.key.size=unit(0.3, "cm")) +
  theme(legend.direction = "horizontal") + theme(legend.position = c(0.5, 1)) + theme(legend.box.margin=margin(c(0,0,15,0))) +
  theme(plot.background = element_blank(), legend.background = element_blank(),legend.box.background = element_blank(), legend.spacing.x = unit(0.1, 'cm')) +
  guides(fill = guide_legend(override.aes = list(shape = 0, size = 1, colour=NA))) +
  theme(axis.title = element_text(family = "Arial"))
fig1

ggsave("//Users/Rachel/Desktop/jhe/Fig1.jpg", plot=fig1, width = 9, height = 9, units = "cm", dpi=1000)

dev.off()


##### FIGURE 2ab | Feeding time residuals and Moving time residuals vs. Rainfall ####
df <- read.csv("df.Feed.csv",header=T,sep=",", fill=T)
modelFEED <- glmer.nb(sum2 ~ Rain30 + MinT + (1|ID), data=df)

df2 <- read.csv("df.Move.csv",header=T,sep=",", fill=T)
df2 <- df2 %>% filter(status == "leader")
modelMOVE <- glmer.nb(sum ~ Rain30 + MinT + (1|ID), data=df2)


# 2a
fig2a <- visreg(modelFEED,"Rain30",type="contrast",scale="linear", line=list(col=c("black")), ylim=c(-4,2), gg=T,
                points=list(pch=21, bg="olivedrab3", col="black"), xlab="Cumulative rainfall (mm)", ylab="Feeding time residuals",band=F) 

fig2a <- fig2a + theme_bw() + geom_point(pch=21, size = 1.5, bg="olivedrab3", col="black") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border=element_rect(size = 1, colour = "black")) +
  theme(axis.ticks=element_line(colour="black", size = 0.5), axis.ticks.length = unit(.15, "cm")) +
  theme(plot.margin = unit(c(1.25,1.25,1.25,1.25), "lines")) +
  theme(axis.title.x = element_text(size = 10, vjust = -1), axis.text.x = element_text(color="black", size = 8), axis.title.y = element_text(vjust = 2.5, angle = 90, size = 10),
        axis.text.y = element_text(color="black", size = 8)) + 
  scale_y_continuous(limits=c(-4,2))

fig2a

# 2b
fig2b <- visreg(modelMOVE,"Rain30",type="contrast",scale="linear", line=list(col=c("black")), ylim=c(-4,2), gg=T,
                points=list(pch=21, bg="darkgoldenrod1", col="black"), xlab="Cumulative rainfall (mm)", ylab="Moving time residuals",band=F) 


fig2b <- fig2b + theme_bw() + geom_point(pch=21, size = 1.5, bg="darkgoldenrod1", col="black") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border=element_rect(size = 1, colour = "black")) +
  theme(axis.ticks=element_line(colour="black", size = 0.5), axis.ticks.length = unit(.15, "cm")) +
  theme(plot.margin = unit(c(1.25,1.25,1.25,1.25), "lines")) +
  theme(axis.title.x = element_text(size = 10, vjust = -1), axis.text.x = element_text(color="black", size = 8), axis.title.y = element_text(vjust = 2.5, angle = 90, size = 10),
        axis.text.y = element_text(color="black", size = 8)) + 
  scale_y_continuous(limits=c(-4,2))

fig2b


## Fig 2ab final  
fig2ab <- ggarrange(fig2a, fig2b, ncol=2, nrow=1, legend = "top", labels = c("A", "B"), vjust = 1.5, hjust = -0.2, font.label = list(size = 16, color = "black", face = "bold"))
fig2ab

ggsave("//Users/Rachel/Desktop/jhe/Fig2.jpg", plot=fig2ab, width =14, height = 7, units = "cm", dpi=1000)

dev.off()

##### FIGURE 3 | uCP residuals vs. Rainfall ####       
df <- read.csv("df.Cpep.csv",header=T,sep=",", fill=T)
model <- glmer(UCP ~  Rain30 + MinT + (1 | ID), family = Gamma(link = "log"), data=df)

fig3 <- visreg(model,"Rain30",type="contrast",scale="linear", line=list(col=c("black")), ylim=c(-3,3), gg=T,
                points=list(pch=21, bg="slateblue3", col="black"), xlab="Cumulative rainfall (mm)", ylab="Urinary C-peptide residuals",band=F) 

fig3 <- fig3 + theme_bw() + geom_point(pch=21, size = 1.25, bg="deeppink4", col="black") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border=element_rect(size = 1, colour = "black")) +
  theme(axis.ticks=element_line(colour="black", size = 0.5), axis.ticks.length = unit(.15, "cm")) +
  theme(plot.margin = unit(c(1.25,1.25,1.25,1.25), "lines")) +
  theme(axis.title.x = element_text(size = 12, vjust = -1), axis.text.x = element_text(color="black", size = 12), axis.title.y = element_text(vjust = 2.5, angle = 90, size = 12),
        axis.text.y = element_text(color="black", size = 12)) + scale_x_continuous(limits=c(0,500))
fig3

ggsave("//Users/Rachel/Desktop/jhe/Fig3.jpg", plot=fig3, width =9, height = 9, units = "cm", dpi=1000)

dev.off()



##### FIGURE 4 | Seasonal variation in rainfall, feeding below-ground, feeding, moving, & uCP ######
df_season<-as_tibble(read.csv("df_season.csv",header=TRUE,stringsAsFactors=FALSE))

## 4A. Rainfall and Feeding below-ground
df_weather1<-as_tibble(read.csv("df_weather1.csv",header=TRUE,stringsAsFactors=FALSE))

df_weather1 <- df_weather1 %>% filter(panel != "Total takeovers")
df_weather1 <- df_weather1 %>% filter(panel != "Time spent feeding")
df_weather1 <- df_weather1 %>% filter(panel != "Time spent moving")
df_weather1 <- df_weather1 %>% filter(panel != "Urinary C-peptide")
df_weather1 <- df_weather1 %>% filter(panel != "FA")
df_weather1 <- df_weather1 %>% filter(panel != "Feeding above-ground")

fig_4a<-ggplot(data = df_weather1, mapping = aes(x = as.factor(x), y = y, fill=as.factor(panel))) +
  geom_rect(data = df_season, aes(xmin = x-0.5, xmax = x+0.5, ymin = -Inf, ymax = Inf, fill=panel), fill = c("#abd9e9","#abd9e9","#fdae61","#fdae61","#fdae61","#fdae61","#fdae61","#fdae61","#fdae61","#abd9e9","#abd9e9","#abd9e9"), alpha = 0.5) +
  scale_x_discrete(labels = c("S","O","N","D","J","F","M","A","M","J","J","A")) +
  scale_y_continuous(sec.axis = sec_axis(~./3, name =""), limits = c(0,60)) + 
  theme(axis.title.y.left=element_text(vjust=1.5)) +  
  theme(axis.title.x=element_blank()) +
  xlab("Months") +
  ylab("% of total feeding time") + 
  theme(legend.title=element_blank(), legend.position = c(0.45,0.94), legend.background = element_blank(),
        legend.key.size = unit(0.15, 'cm'),
        panel.grid.minor = element_blank(), panel.grid.major = element_blank(), plot.title = element_text(hjust = 0)) +
  geom_text(aes(x = 6.75, y = 60, label = "DRY", vjust=-1.5), size=4) +
  geom_text(aes(x = 11, y = 60, label = "WET", vjust=-1.5), size=4) +
  geom_text(aes(x = 1.5, y = 60, label = "WET", vjust=-1.5), size=4) +
  coord_cartesian(clip = "off") +
  theme(axis.title = element_text(size = 10), axis.text=element_text(size=10)) +
  geom_bar(data = subset(df_weather1, panel %in% "Rainfall"), stat = "identity", aes(y=y*3, group = as.factor(panel), shape=as.factor(panel)), fill = "white", colour="black") +
  geom_errorbar(data = subset(df_weather1, panel %in% "Rainfall"), aes(ymin=y*3-se, ymax=y*3+se, group=panel), width=.1, position=position_dodge(0.5), colour="black") +
  geom_point(data = subset(df_weather1, panel %in% "Feeding below-ground"), aes(group = as.factor(panel), shape=as.factor(panel)), colour = "black", size=1.5) +
  geom_line(data = subset(df_weather1, panel %in% "Feeding below-ground"), aes(group=panel), position=position_dodge(0.5), colour="black", linetype=2, size=0.75) +
  geom_errorbar(data = subset(df_weather1, panel %in% "Feeding below-ground"), aes(ymin=y-se, ymax=y+se, group=panel), width=.1, position=position_dodge(0.5), colour="black") +
  scale_shape_manual(labels = c("Feeding below-ground", "Rainfall"), values = c(16,22), name="", guide="legend") +
  scale_fill_manual(labels = c("Feeding below-ground", "Rainfall"), values = c("black","white"), name = "", guide=FALSE) +
  theme(legend.key=element_rect(fill=NA)) +
  guides(shape = guide_legend(override.aes = list(fill = c("black","white"), colour = c("black","black"), size=c(1.5,2)), nrow=2)) +
  theme(plot.margin=unit(c(1,1.52,0.7,2.6),"cm")) +
  theme(legend.text=element_text(size=8)) +
  labs(tag = "Total daily rain (mm)") +
  theme(plot.tag.position = c(1,0.55), plot.tag=element_text(size=10,angle=270))

fig_4a


dev.off()


## 4B. Seasonal - Time spent feeding
df_weather1<-as_tibble(read.csv("df_weather1.csv",header=TRUE,stringsAsFactors=FALSE))
df_weather1 <- na.omit(df_weather1)

df_weather1 <- df_weather1 %>% filter(panel != "Total takeovers")
df_weather1 <- df_weather1 %>% filter(panel != "Time spent moving")
df_weather1 <- df_weather1 %>% filter(panel != "Urinary C-peptide")
df_weather1 <- df_weather1 %>% filter(panel != "FA")
df_weather1 <- df_weather1 %>% filter(panel != "Feeding above-ground")

df_weather1 <- df_weather1 %>% filter(panel != "Rainfall")

fig_4b<-ggplot(data = df_weather1, mapping = aes(x = as.factor(x), y = y, fill=as.factor(panel))) +
  geom_rect(data = df_season, aes(xmin = x-0.5, xmax = x+0.5, ymin = -Inf, ymax = Inf, fill=panel), fill = c("#abd9e9","#abd9e9","#fdae61","#fdae61","#fdae61","#fdae61","#fdae61","#fdae61","#fdae61","#abd9e9","#abd9e9","#abd9e9"), alpha = 0.5) +
  scale_x_discrete(labels = c("S","O","N","D","J","F","M","A","M","J","J","A")) +
  scale_y_continuous(limits=c(51,80), breaks=c(55,60,65,70,75,80)) +
  theme(axis.title.y=element_text(vjust=2)) +
  theme(axis.title.x=element_blank()) +
  xlab("Months") +
  ylab("% of activity budget") +
  theme(axis.title = element_text(size = 10), axis.text=element_text(size=10)) +
  theme(legend.position="none", panel.grid.minor = element_blank(), panel.grid.major = element_blank(), plot.title = element_text(hjust = 0)) +
  geom_point(data = subset(df_weather1, panel %in% "Time spent feeding"), aes(y=y*100, group = as.factor(panel), shape=as.factor(panel)), colour = "black", size=1.5) +
  geom_line(data = subset(df_weather1, panel %in% "Time spent feeding"), aes(y=y*100, group=panel), position=position_dodge(0.5), colour="black", linetype=2, size=0.75) +
  geom_errorbar(data = subset(df_weather1, panel %in% "Time spent feeding"), aes(ymin=(y-se)*100, ymax=(y+se)*100, group=panel), width=.1, position=position_dodge(0.5), colour="black") +
  scale_shape_manual(labels ="Time spent feeding", values = 16, name="", guide="legend") +
  scale_fill_manual(labels ="Time spent feeding", values = "black", name = "", guide=FALSE) +
  theme(plot.margin=unit(c(0.2,2.5,1,2.6),"cm"))
fig_4b

dev.off()


## 4C. Seasonal - Time spent moving
df_weather1<-as_tibble(read.csv("df_weather1.csv",header=TRUE,stringsAsFactors=FALSE))
df_weather1 <- na.omit(df_weather1)

df_weather1 <- df_weather1 %>% filter(panel != "Total takeovers")
df_weather1 <- df_weather1 %>% filter(panel != "Time spent feeding")
df_weather1 <- df_weather1 %>% filter(panel != "Urinary C-peptide")
df_weather1 <- df_weather1 %>% filter(panel != "FA")
df_weather1 <- df_weather1 %>% filter(panel != "Feeding above-ground")
df_weather1 <- df_weather1 %>% filter(panel != "Rainfall")

fig_4c<-ggplot(data = df_weather1, mapping = aes(x = as.factor(x), y = y, fill=as.factor(panel))) +
  geom_rect(data = df_season, aes(xmin = x-0.5, xmax = x+0.5, ymin = -Inf, ymax = Inf, fill=panel), fill = c("#abd9e9","#abd9e9","#fdae61","#fdae61","#fdae61","#fdae61","#fdae61","#fdae61","#fdae61","#abd9e9","#abd9e9","#abd9e9"), alpha = 0.5) +
  scale_x_discrete(labels = c("S","O","N","D","J","F","M","A","M","J","J","A")) +
  scale_y_continuous(limits=c(9,20), breaks=c(10,12,14,16,18,20)) +
  theme(axis.title.y=element_text(vjust=2)) +
  theme(axis.title.x=element_blank()) +
  xlab("Months") +
  ylab("% of activity budget") +
  theme(axis.title = element_text(size = 10), axis.text=element_text(size=10)) + 
  theme(legend.position="none", panel.grid.minor = element_blank(), panel.grid.major = element_blank(), plot.title = element_text(hjust = 0)) +
  geom_point(data = subset(df_weather1, panel %in% "Time spent moving"), aes(y=y*100, group = as.factor(panel), shape=as.factor(panel)), colour = "black", size=1.5) +
  geom_line(data = subset(df_weather1, panel %in% "Time spent moving"), aes(y=y*100, group=panel), position=position_dodge(0.5), colour="black", linetype=2, size=0.75) +
  geom_errorbar(data = subset(df_weather1, panel %in% "Time spent moving"), aes(ymin=(y-se)*100, ymax=(y+se)*100, group=panel), width=.1, position=position_dodge(0.5), colour="black") +
  scale_shape_manual(labels ="Time spent moving", values = 16, name="", guide="legend") +
  scale_fill_manual(labels ="Time spent moving", values = "black", name = "", guide=FALSE) +
  theme(plot.margin=unit(c(0.2,2.5,1,2.6),"cm"))
fig_4c

dev.off()


## 4D. Seasonal - C-peptide
df_weather1<-as_tibble(read.csv("df_weather1.csv",header=TRUE,stringsAsFactors=FALSE))
df_weather1 <- na.omit(df_weather1)

df_weather1 <- df_weather1 %>% filter(panel != "Total takeovers")
df_weather1 <- df_weather1 %>% filter(panel != "Time spent feeding")
df_weather1 <- df_weather1 %>% filter(panel != "Time spent moving")
df_weather1 <- df_weather1 %>% filter(panel != "FA")
df_weather1 <- df_weather1 %>% filter(panel != "Feeding above-ground")
df_weather1 <- df_weather1 %>% filter(panel != "Rainfall")

fig_4d<-ggplot(data = df_weather1, mapping = aes(x = as.factor(x), y = y, fill=as.factor(panel))) +
  geom_rect(data = df_season, aes(xmin = x-0.5, xmax = x+0.5, ymin = -Inf, ymax = Inf, fill=panel), fill = c("#abd9e9","#abd9e9","#fdae61","#fdae61","#fdae61","#fdae61","#fdae61","#fdae61","#fdae61","#abd9e9","#abd9e9","#abd9e9"), alpha = 0.5) +
  scale_x_discrete(labels = c("S","O","N","D","J","F","M","A","M","J","J","A")) +
  scale_y_continuous(limits=c(0,3.5)) +
  theme(axis.title.y=element_text(vjust=2)) +
  theme(axis.title.x = element_text(vjust=-0.5)) +
  xlab("Months") +
  ylab("Urinary C-peptide") +
  theme(axis.title.x = element_text(size = 12), axis.text=element_text(size=10), axis.title.y = element_text(size = 10)) +
  theme(legend.position="none", panel.grid.minor = element_blank(), panel.grid.major = element_blank(), plot.title = element_text(hjust = 0)) +
  geom_point(data = subset(df_weather1, panel %in% "Urinary C-peptide"), aes(y=y, group = as.factor(panel), shape=as.factor(panel)), colour = "black", size=1.5) +
  geom_line(data = subset(df_weather1, panel %in% "Urinary C-peptide"), aes(y=y, group=panel), position=position_dodge(0.5), colour="black", linetype=2, size=0.75) +
  geom_errorbar(data = subset(df_weather1, panel %in% "Urinary C-peptide"), aes(ymin=y-se, ymax=y+se, group=panel), width=.1, position=position_dodge(0.5), colour="black") +
  scale_shape_manual(labels ="Urinary C-peptide", values = 16, name="", guide="legend") +
  scale_fill_manual(labels ="Urinary C-peptide", values = "black", name = "", guide=FALSE) +
  theme(plot.margin=unit(c(0.2,2.5,0.5,2.6),"cm"))

fig_4d

dev.off()


# Fig 4 final
fig4 <- grid.arrange(arrangeGrob(fig_4a), 
                     arrangeGrob(fig_4b, top=textGrob("Time spent feeding", gp=gpar(fontsize=12), hjust=0.3)), 
                     arrangeGrob(fig_4c, top=textGrob("Time spent moving", gp=gpar(fontsize=12), hjust=0.3)), 
                     arrangeGrob(fig_4d, top=textGrob("Urinary C-peptide", gp=gpar(fontsize=12), hjust=0.3)), 
                     ncol=1,  heights = c(1/4, 1/4, 1/4, 1/4))

ggsave("//Users/Rachel/Desktop/jhe/Fig4.jpg", plot=fig4, width = 12, height = 24, units = "cm", dpi=1000)


dev.off()
