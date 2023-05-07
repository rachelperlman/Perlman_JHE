setwd("//Users/Rachel/Desktop/Perlman et al. 2023 Files")


# Fig S1 | Ecological seasonality at Simien Mountains National Park ####################

df_weather1 <- as_tibble(read.csv("df_weather1.csv",header=TRUE,stringsAsFactors=FALSE))

# SMGRP data - FigS1a
df_weather2 <- as_tibble(read.csv("df_weather2.csv",header=TRUE,stringsAsFactors=FALSE))
df_weather2 <- df_weather2 %>% filter(panel != "Maximum Temperature")
df_weather2 <- df_weather2 %>% filter(panel != "Total takeovers")
df_weather2 <- df_weather2 %>% filter(panel != "Daily Rain")
df_weather2 <- df_weather2 %>% filter(panel != "Minimum Temperature Study Period")
df_weather2 <- df_weather2 %>% filter(panel != "Rainfall Study Period")

# Fig S1a
fig_S1a<-ggplot(data = df_weather2, mapping = aes(x = as.factor(x), y = y, fill=as.factor(panel))) +
  geom_rect(data = df_season, aes(xmin = x-0.5, xmax = x+0.5, ymin = -Inf, ymax = Inf, fill=panel), fill = c("#abd9e9","#abd9e9","#fdae61","#fdae61","#fdae61","#fdae61","#fdae61","#fdae61","#fdae61","#abd9e9","#abd9e9","#abd9e9"), alpha = 0.5) +
  scale_x_discrete(labels = c("S","O","N","D","J","F","M","A","M","J","J","A")) +
  scale_y_continuous(sec.axis = sec_axis(~.*55, name ="", labels=NULL, breaks=c(0,100,200,300,400,500)), limits = c(0,10.3), labels = scales::number_format(accuracy = 0.1)) + 
  xlab("Months") +
  ggtitle("(A) SMGRP 2006-2020") +
  ylab("Minimum temperature (°C)") + 
  theme(axis.title = element_text(size = 22), axis.text=element_text(size=16)) +
  theme(axis.title.y.left=element_text(vjust=0.37), axis.title.x=element_text(hjust=1.5, vjust=0.5), axis.ticks.length.y.right=unit(0,"cm"))  +
  theme(legend.title=element_blank(), legend.position = c(0.5,1.05), legend.background = element_blank(),
        legend.key.size = unit(0.5, 'cm'),
        panel.grid.minor = element_blank(), panel.grid.major = element_blank(), plot.title = element_text(hjust = 0.5, size=20)) +
  geom_bar(data = subset(df_weather2, panel %in% "Cumulative rainfall"), stat = "identity", aes(y=y/55, group = as.factor(panel), shape=as.factor(panel)), fill = "white", colour="black") +
  geom_errorbar(data = subset(df_weather2, panel %in% "Cumulative rainfall"), aes(ymin=y/55-(se/55), ymax=y/55+(se/55), group=panel), width=.1, position=position_dodge(0.5), colour="black") +
  
  geom_point(data = subset(df_weather2, panel %in% "Minimum temperature"), aes(group = as.factor(panel), shape=as.factor(panel)), colour = "black", fill="black", size=3) +
  geom_line(data = subset(df_weather2, panel %in% "Minimum temperature"), aes(group=panel), position=position_dodge(0.5), colour="black", linetype=2, size=1) +
  geom_errorbar(data = subset(df_weather2, panel %in% "Minimum temperature"), aes(ymin=y-se, ymax=y+se, group=panel), width=.1, position=position_dodge(0.5), colour="black") +
  
  scale_shape_manual(labels = c("Minimum temperature", "Cumulative rainfall"), values = c(22,22), name="", guide="legend") +
  scale_fill_manual(labels = c("Minimum temperature", "Cumulative rainfall"), values = c("black","white"), name = "", guide=FALSE) +
  guides(shape = guide_legend(override.aes = list(fill = c("black","white"), colour = c("black","black"), size=4), nrow=1)) +
  theme(plot.margin=unit(c(1,0.2,1,1),"cm")) +
  theme(legend.text=element_text(size=22))
fig_S1a


# Study period - FigS1b
df_weather2 <- as_tibble(read.csv("df_weather2.csv",header=TRUE,stringsAsFactors=FALSE))
df_weather2 <- df_weather2 %>% filter(panel != "Maximum Temperature")
df_weather2 <- df_weather2 %>% filter(panel != "Minimum temperature")
df_weather2 <- df_weather2 %>% filter(panel != "Total takeovers")
df_weather2 <- df_weather2 %>% filter(panel != "Daily Rain")
df_weather2 <- df_weather2 %>% filter(panel != "Cumulative rainfall")

# Fig S1b
fig_S1b<-ggplot(data = df_weather2, mapping = aes(x = as.factor(x), y = y, fill=as.factor(panel))) +
  geom_rect(data = df_season, aes(xmin = x-0.5, xmax = x+0.5, ymin = -Inf, ymax = Inf, fill=panel), fill = c("#abd9e9","#abd9e9","#fdae61","#fdae61","#fdae61","#fdae61","#fdae61","#fdae61","#fdae61","#abd9e9","#abd9e9","#abd9e9"), alpha = 0.5) +
  scale_x_discrete(labels = c("S","O","N","D","J","F","M","A","M","J","J","A")) +
  scale_y_continuous(sec.axis = sec_axis(~.*55, name ="Cumulative rainfall (mm)", breaks=c(0,100,200,300,400,500)), limits = c(0,10.3), labels = NULL) + 
  xlab("Months") +
  ggtitle("(B) Study Period 2017-2018") +
  theme(axis.title = element_text(size = 22, color="white"), axis.text=element_text(size=16)) +
  theme(legend.title=element_blank(), legend.position = c(0.5,1.05), legend.background = element_blank(),
        legend.key.size = unit(0.5, 'cm'),
        panel.grid.minor = element_blank(), panel.grid.major = element_blank(), plot.title = element_text(hjust = 0.5, size=20), axis.title.y.left=element_text(size=0), axis.ticks.length.y.left=unit(0,"cm")) +
  geom_bar(data = subset(df_weather2, panel %in% "Rainfall Study Period"), stat = "identity", aes(y=y/55, group = as.factor(panel), shape=as.factor(panel)), fill = "white", colour="black") +
  geom_errorbar(data = subset(df_weather2, panel %in% "Rainfall Study Period"), aes(ymin=y/55-(se/55), ymax=y/55+(se/55), group=panel), width=.1, position=position_dodge(0.5), colour="black") +
  
  geom_point(data = subset(df_weather2, panel %in% "Minimum Temperature Study Period"), aes(group = as.factor(panel), shape=as.factor(panel)), colour = "black", fill="black", size=3) +
  geom_line(data = subset(df_weather2, panel %in% "Minimum Temperature Study Period"), aes(group=panel), position=position_dodge(0.5), colour="black", linetype=2, size=1) +
  geom_errorbar(data = subset(df_weather2, panel %in% "Minimum Temperature Study Period"), aes(ymin=y-se, ymax=y+se, group=panel), width=.1, position=position_dodge(0.5), colour="black") +
  
  scale_shape_manual(labels = c("Minimum Temperature", "Cumulative rainfall"), values = c(22,22), name="", guide="legend") +
  scale_fill_manual(labels = c("Minimum Temperature", "Cumulative rainfall"), values = c("black","white"), name = "", guide=FALSE) +
  guides(shape = guide_legend(override.aes = list(fill = c("black","white"), colour = c("black","black"), size=4), nrow=1)) +
  theme(plot.margin=unit(c(1,1,1,0.2),"cm")) +
  theme(legend.text=element_text(size=22)) +
  labs(tag = "Cumulative rainfall (mm)") +
  theme(plot.tag.position = c(1,0.55), plot.tag=element_text(size=20,angle=270))
fig_S1b


# Fig S1
figS1 <- ggarrange(fig_S1a, fig_S1b, ncol=2, nrow=1, 
                   legend = "top", labels = c("", ""), vjust = 1.5, hjust = -0.2, 
                   font.label = list(size = 16, color = "black", face = "bold"),
                   align="h", common.legend = TRUE)
figS1
ggsave("//Users/Rachel/Desktop/jhe/FigS1.jpg", plot=figS1, width =32, height = 16, units = "cm")
ggsave(figS1, file="figs1.png", width =32, height = 16, units = c("cm"))



# Fig S2 | Urinary C-peptide parallelism #############
parallelism <- as_tibble(read.csv("df_cpep_parallelism.csv",header=TRUE,stringsAsFactors=FALSE))
parallelism$Binding <- parallelism$Binding*100

# ANCOVA
ancova <- aov(Binding ~ Concentration*Label, data=parallelism)
summary(ancova)

# Fig S2
parallelism$Label <- factor(parallelism$Label, levels = c("Standard", "Gelada"))

figS2 <- ggplot(parallelism, aes(Concentration, Binding, by=Label)) + geom_point(aes(shape=Label, size=Label), stroke=1) + 
  scale_x_continuous(trans='log10', name="Concentration") + 
  scale_y_continuous(name ="% Binding", limits = c(0,100)) +
  scale_shape_manual(values=c(16, 2)) + scale_size_manual(values=c(4,3)) +
  stat_smooth(method="lm", se=F, aes(linetype=Label), color="black") +
  theme_classic(base_line_size = 1) + 
  theme(plot.margin = unit(c(0.5,1,1,1), "cm")) + 
  theme(axis.title.x = element_text(vjust=-1, size=16), 
        axis.text.x = element_text(size=16, color="black"),
        axis.title.y = element_text(vjust = 1.5, angle = 90, size = 16), 
        axis.text.y = element_text(size=16, color="black")) +
  theme(axis.ticks.length = unit(0.1,"cm")) + 
  theme(legend.position="top", legend.title=element_blank(), legend.text=element_text(size=16))

figS2

ggsave(figS2, file="c-pep parallelism.png", width =16, height = 16, units = c("cm"))


# Fig S3 | Distribution of UCP data ####
cpep <- as_tibble(read.csv("df.Cpep.csv",header=TRUE,stringsAsFactors=FALSE))

# Fig S3
figS3 <- ggplot(cpep, aes(x=UCP)) + geom_histogram(color="black", fill="gray", binwidth=6, lwd=0.5, boundary=0)

figS3 <- figS3 +   theme_classic() + 
  theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm")) + 
  theme(axis.title.x = element_text(size=10), 
        axis.text.x = element_text(size=8, color="black"),
        axis.title.y = element_text(vjust = 1.5, angle = 90, size = 10), 
        axis.text.y = element_text(size=8, color="black")) +
  scale_y_continuous(name ="Number of samples", expand = expansion(mult = c(0, .1))) +
  scale_x_continuous(name ="Urinary C-peptide values", limits=c(0,40), expand=c(0,0)) +
  theme(axis.ticks.length = unit(0.1,"cm"))

figS3

ggsave(figS3, file="c-pep distribution.png", width =8, height = 8, units = c("cm"))
ggsave("//Users/Rachel/Desktop/jhe/FigS3.jpg", plot=figS3, width =8, height = 8, units = "cm")




# Fig S4 | Variation in UCP across time of sample collection and season ####
cpep1 <- as_tibble(read.csv("//Users/Rachel/Desktop/jhe/Files/df_cpep1.csv",header=TRUE,stringsAsFactors=FALSE))
cpep1 <- as_tibble(read.csv("df.Cpep.csv",header=TRUE,stringsAsFactors=FALSE))
cpep1 <- cpep1 %>% filter(Status == "Leader")
cpep1 <- cpep1 %>% filter(ID != "FRU")

# Fig S4
cpep1$Time <- strptime(cpep1$Time, format="%I:%M")
cpep1$Time <- as.POSIXct(cpep1$Time)

cpep1$season <- NA
cpep1$season[cpep1$Month == "Dec"] <- "Dry season"
cpep1$season[cpep1$Month == "Jan"] <- "Dry season"
cpep1$season[cpep1$Month == "Feb"] <- "Dry season"
cpep1$season[cpep1$Month == "Mar"] <- "Dry season"

cpep1$season[cpep1$Month == "May"] <- "Wet season"
cpep1$season[cpep1$Month == "Jun"] <- "Wet season"
cpep1$season[cpep1$Month == "Jul"] <- "Wet season"
cpep1$season[cpep1$Month == "Aug"] <- "Wet season"


figS4 <- ggplot(data=subset(cpep1, !is.na(season)), aes(x=Time, y=UCP, color=season, fill=season)) + 
  theme_minimal() + theme(legend.position="none") +
  geom_point(shape=21, color="black") + facet_wrap(~ season, ncol = 1)  + 
  theme(strip.background = element_rect(fill=NA, color=NA), strip.text=element_text(size=16)) +
  ylab("Urinary C-peptide") + 
  scale_colour_manual(values = c("black","black")) +
  scale_fill_manual(values=c("#fdae61","#abd9e9")) +
  geom_smooth(method="loess") +
  theme(plot.margin = unit(c(1,1,1,1), "cm")) + 
  theme(axis.title.x = element_text(vjust=-1, size=12), 
        axis.text.x = element_text(size=10, color="black"),
        axis.title.y = element_text(vjust = 1.5, angle = 90, size = 12), 
        axis.text.y = element_text(size=10, color="black"))
figS4

ggsave(figS4, file="c-pep time of sample collection.png", width =16, height = 24, units = c("cm"))
ggsave("//Users/Rachel/Desktop/jhe/FigS4.jpg", plot=figS4, width =16, height = 24, units = "cm")




# Fig S5 | UCP concentrations are negatively associated with mean minimum temperature ####

# Fig S5
modelUCP <- glmer(UCP ~  Rain30 + MinT + (1 | ID), family = Gamma(link = "log"), data=cpep1)

figS5 <- visreg(modelUCP,"MinT",type="contrast",scale="linear", line=list(col=c("black")), ylim=c(-3,3), gg=T,
                points=list(pch=21, bg="slateblue3", col="black"), xlab="Minimum temperature (mm)", ylab="Urinary C-peptide residuals",band=F) 

figS5 <- figS5 + theme_bw() + geom_point(pch=21, size = 1.25, bg="lightblue3", col="black") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border=element_rect(size = 1, colour = "black")) +
  theme(axis.ticks=element_line(colour="black", size = 0.5), axis.ticks.length = unit(.15, "cm")) +
  theme(axis.title.x = element_text(size = 12, vjust = -1), axis.text.x = element_text(color="black", size = 12), axis.title.y = element_text(vjust = 2.5, angle = 90, size = 12),
        axis.text.y = element_text(color="black", size = 12)) + scale_x_continuous(limits=c(6,10.5)) +
  theme(plot.margin = unit(c(1,1,1,1.5), "lines"))


figS5

ggsave(figS5, file="FigS5.png", width =8, height = 8, units = c("cm"))
ggsave("//Users/Rachel/Desktop/jhe/FigS5.jpg", plot=figS5, width =10, height = 10, units = "cm")




