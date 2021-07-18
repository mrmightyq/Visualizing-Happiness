library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(sqldf)
library(ggpubr)
library(RColorBrewer)
library(ggthemes)
library(ggpmisc)
happy <- read_csv("B:/GLOBAL/2063-BASUS/FLORHAM-PARK/NTH/NTH-T/TalentManagement/03_Data & Analytics (Quinn)/Quinn/Syracuse/Information Visualization/World Happiness 2015-2019.csv")
str(happy)
dim(happy)
(11*4)*(782/100)
par(mfrow=c(1,3))
#Basic EDA
hist(happy$HappinessScore, col="blueviolet"
     , main="Distribution of Happiness Score"
     , ylab="Frequency", xlab="Total Happiness")
title(sub="Kaggle World Happiness Report", adj=1, line=3, font=2, cex.sub= .7)
boxplot(happy$`Health (Life Expectancy)`, col="grey", outcol="red", horizontal = TRUE
        , xlab = "Normalized Life Expectancy"
        , main="Distribution of Life Expectancy")
title(sub="Kaggle World Happiness Report", adj=1, line=3, font=2, cex.sub= .7)


d <- density(happy$Freedom)
plot(d, main="Density Plot of Freedom Skewed Right",ylab= "Density",xlab="Freedom")
polygon(d,col="darkolivegreen")
title(sub="Kaggle World Happiness Report", adj=1, line=3, font=2, cex.sub= .7)

par(mfrow=c(1,1))
#Real Analysis
happiness.region <- aggregate(happy$HappinessScore,list(happy$Region), mean)
round(happiness.region,2)
happiness.region <- as.data.frame(happiness.region)
colnames(happiness.region) <- c("region","happiness")
str(happiness.region)

happiness.region <- happiness.region[order(happiness.region$happiness,decreasing=FALSE),]

par(mar=c(4,9,4,4))
#library(RColorBrewer)
#coul <- brewer.pal(10, "Set3")
barplot (happiness.region$happiness, main="Average Happiness Score by Region, 2015-2019"
         , names.arg = happiness.region$region 
         , space=0.15,xlab="Average Happiness Score"
         , horiz = TRUE
         , las = 1
         , cex.names = 0.6
         , cex.axis = 0.6
         , col=ifelse(happiness.region$happiness>6,"dark blue","cornsilk"))
title(sub="Kaggle World Happiness Report", adj=1, line=3, font=2, cex.sub= .7)

# region, year, hapiness score (regional line chart over time)

happy.time<- happy %>%
  group_by(happy$Region, happy$Year) %>%
  summarize(mean = mean(HappinessScore))

happy.df <- as.data.frame(happy.time)

colnames(happy.df) <- c("Region","Year", "Happiness")
happy.df
#export
write.csv(happy.df, "C:/Users/KnudseQ/Desktop/Happy Narrow DF.csv")
ggscatter(happy.df, x = "Year", y = "Happiness",
                color = "Region", palette = "Spectral",
                size = "Happiness",
          alpha = 0.8, title = "Trend of Happiness by Region, 2015-2019", legend="right")
#title(sub="Kaggle World Happiness Report", adj=1, line=3, font=2, cex.sub= .7)

ggplot(happy.df, aes(x = Year, y = Happiness, label="Region"))+ ggtitle("Trend of Happiness by Region, 2015-2019")+
  geom_line(aes(color= Region),size=2, alpha =.6)+
 # scale_color_brewer("Spectral")+
  theme_minimal()
 
       #, palette = "Spectral"
       #,size = "Happiness"
       #,alpha = 0.8
       #, title = "Trend of Happiness by Region, 2015-2019", legend="right")


ggplot(happy, aes(x=happy$Region,
                  y=happy$HappinessScore,
                  color=happy$Region))+
  geom_point() + theme_pubclean() + xlab("Region") +
  ylab("Happiness Score")+ ggtitle("Distribution of Happiness Scores by Region")+
  theme(axis.title = element_text(size = 12), legend.position = "none")+coord_flip()
#title(sub="Kaggle World Happiness Report", adj=1, line=3, font=2, cex.sub= .7)


cor1 <- ggplot(happy, aes(x=happy$`Health (Life Expectancy)`,y=happy$HappinessScore,
                  color=happy$Region))+geom_point()+
  geom_point() + theme_classic() + xlab("Normalized Life Expectancy") +
  ylab("Happiness Score")+ ggtitle("A Positive Correlation Exists Between Happiness and Life Expectancy") +
  theme(legend.title = element_blank(),legend.key.width=unit(0.3,"cm"),legend.key.height=unit(0.3,"cm"),legend.justification=c(.95,0),legend.position = c(1,0)) 


xbp <- ggboxplot(happy$`Health (Life Expectancy)`, width = 0.3, fill = "lightgray") +
  rotate() +
  theme_transparent()
# Box plot of the y variable
ybp <- ggboxplot(happy$HappinessScore, width = 0.3, fill = "lightgray") +
  theme_transparent()
# Create the external graphical objects
# called a "grop" in Grid terminology
xbp_grob <- ggplotGrob(xbp)
ybp_grob <- ggplotGrob(ybp)
# Place box plots inside the scatter plot
xmin <- min(x=happy$`Health (Life Expectancy)`); xmax <- max(x=happy$`Health (Life Expectancy)`)
ymin <- min(happy$HappinessScore); ymax <- max(happy$HappinessScore)
yoffset <- (1/15)*ymax; xoffset <- (1/15)*xmax
# Insert xbp_grob inside the scatter plot
cor1 + annotation_custom(grob = xbp_grob, xmin = xmin, xmax = xmax, 
                       ymin = ymin-yoffset, ymax = ymin+yoffset) +
  # Insert ybp_grob inside the scatter plot
annotation_custom(grob = ybp_grob,
                    xmin = xmin-xoffset, xmax = xmin+xoffset, 
                    ymin = ymin, ymax = ymax)
title(sub="Kaggle World Happiness Report", adj=1, line=3, font=2, cex.sub= .7)

happy.high.2019 <- happy[happy$Year=="2019",]
happy.high.top25 <- happy.high.2019[happy.high.2019$HappinessRank<= 25,]
write.csv(happy.high.2019, "C:/Users/KnudseQ/Desktop/2019 World Happiness.csv")
write.csv(happy.high.top25, "C:/Users/KnudseQ/Desktop/IST719.csv")

p <- ggplot(happy.high.top25, aes(x=happy.high.top25$Family,y=happy.high.top25$HappinessScore, label = happy.high.top25$Country,
                  color=happy.high.top25$Region))+geom_point()+
  geom_point() + theme_classic() + xlab("Normalized Social Support") +
  ylab("Happiness Score")+ ggtitle("The 25 Happiest Countries in 2019") +
  theme(legend.title = element_blank(),legend.key.width=unit(0.3,"cm"),legend.key.height=unit(0.3,"cm"),legend.justification=c(.95,0),legend.position = c(1,0)) 
p +geom_text(check_overlap = TRUE, size = 3, nudge_x = .02) + labs(subtitle = "A Positive Correlation Exsists Between Happiness and Social Support")
#title(sub="Kaggle World Happiness Report", adj=1, line=3, font=2, cex.sub= .7)

p2 <- ggplot(happy.high.top25, aes(x=happy.high.top25$Freedom,y=happy.high.top25$HappinessScore, label = happy.high.top25$Country,
                                  color=happy.high.top25$Region))+geom_point()+
  geom_point() + theme_classic() + xlab("Freedom") +
  ylab("Happiness Score")+ ggtitle("The 25 Happiest Countries in 2019") +
  theme(legend.title = element_blank(),legend.key.width=unit(0.3,"cm"),legend.key.height=unit(0.3,"cm"),legend.justification=c(3,-3),legend.position = c(1,0)) 
p2 +geom_text(check_overlap = FALSE, size = 3, nudge_x = -.01) + labs(subtitle = "A Positive Correlation Exsists Between Happiness and Freedom")
#title(sub="Kaggle World Happiness Report", adj=1, line=3, font=2, cex.sub= .7)

library(caret)

happy.top5 <- happy.high.2019[happy.high.2019$HappinessRank<= -5,]
happy.top5

happy.low.2019 <- happy[happy$Year=="2019",]
happy.low.10 <- happy.low.2019[happy.high.2019$HappinessRank>= 147,]

happy.low5 <- happy.low.10[order(-happy.low.10$HappinessRank),]
happy.low5[1:5,]


#happy$`Economy (GDP per Capita)`<- scale(happy$`Economy (GDP per Capita)`,center=TRUE, scale =TRUE)
#happy$Family <- scale(happy$Family,center=TRUE, scale =TRUE)
#scale(happy$`Health (Life Expectancy)`,center=TRUE, scale =TRUE)
#scale(happy$Freedom,center=TRUE, scale =TRUE)
#happy$`Trust (Government Corruption)`<-scale(happy$`Trust (Government Corruption)`,center=TRUE, scale =TRUE)
#scale(happy$Generosity,center=TRUE, scale =TRUE)


lm1<-lm(happy$HappinessScore~ happy$`Economy (GDP per Capita)`+ happy$Family+
     happy$`Health (Life Expectancy)`+happy$Freedom+happy$`Trust (Government Corruption)`+
     happy$Generosity,data=happy)
summary(lm1)
varimp <- varImp(lm1, scale = FALSE)
colnames(varimp) <- c("Variable", "Importance")

Feature <- c('Economy', 'Family', 'Health', 'Freedom', 'Trust in \n\Government','Generosity')
Importance <- c(13.766868, 7.952739, 7.643747,9.029568, 3.832026,3.379111)
df <- data.frame(Feature, Importance)

barplot (df$Importance, main="Feature Importance for Model Prediciting Happiness"
         , names.arg = df$Feature
         , space=0.15,xlab="Model Importance"
         , horiz = TRUE
         , las = 1
         , cex.names = 0.6
         , cex.axis = 0.6
         , col=ifelse(df$Importance>9,"dark blue","cornsilk"))



library(aplpack)
bagplot(happy$HappinessScore, happy$`Economy (GDP per Capita)`#bagplot example outliers in red; red start is central tendency of this data
        , show.whiskers = F
        , col.loophull = "#aaccff"
        , col.looppoints = "#3355ff"
        , col.baghull = "#7799ff"
        , col.bagpoints = "#000088"
        , transparency = T
        , xlab = "Happiness Score"
        , ylab = "Economy Rating"
        , main = "The Density Clusters in the Middle of the Data Set")



# Not a super effective visual 
ggplot(happy,aes(x=happy$Region,y=happy$HappinessScore))+
  geom_violin(aes(fill=happy$Region),alpha=0.7)+ theme_pubclean() +
  theme(axis.title = element_text(size = 12),legend.position = "none") + coord_flip()
library(ggthemes)
library(repr)
library(rworldmap)
library(dplyr)
library(ggplot2)
library(plotly)
correction<-c("Congo (Brazzaville)"="Democratic Republic of the Congo","Congo (Kinshasa)"="Republic of Congo","United States"="USA","United Kingdom"= "UK")
for(i in names(correction)){
  happy[happy$Country==i,"Country"]<-correction[i]
}
q<-map_data("world")
colnames(q)[5] <- "Country"
df<- left_join(q,happy)
b<-ggplot() +  geom_polygon( aes(x = df$long, y = df$lat, group = df$group,fill= df$HappinessScore),color="white") + 
  coord_equal() +scale_fill_gradient(breaks=c(3,4,5,6,7,8)) +
  ggtitle("Happiness Score Around the World, 2015-2019")+
  xlab("") + ylab("") + guides(shape=TRUE) + labs(fill="Happiness Score") 
b <- b + theme_void()+ theme(
  axis.text.x = element_blank(),
  axis.text.y = element_blank(),
  axis.ticks = element_blank())
b +scale_fill_gradient(low="#eef76d", high="#1EB51E")




b <- ggplot(df, aes(x = df$long, y = df$lat, group = df$group)) +  geom_polygon(aes(fill= df$HappinessScore),color="white") + 
                                                                            scale_fill_viridis_c(option = "rainbow")+
  coord_equal()+ #+scale_fill_gradient(breaks=c(3,5,7,9)) +
  ggtitle("Happiness Score Around the World, 2015-2019")+
  xlab("") + ylab("") + guides(shape=TRUE) + labs(fill="Happiness Score")

b +theme_void()+ theme(
  axis.text.x = element_blank(),
  axis.text.y = element_blank(),
  axis.ticks = element_blank())


b + scale_colour_gradientn(colours = terrain.colors(10))

ggplot() +  geom_polygon( aes(x = df$long, y = df$lat, group = df$group,fill= df$HappinessScore))+ scale_colour_manual(values = terrain.colors(5))


d <- data.frame(
  country=happy$Country,
  value=happy$`Economy (GDP per Capita)`)

n <- joinCountryData2Map(d, joinCode="NAME", nameJoinColumn="Country")
mapCountryData(n, nameColumnToPlot="value", mapTitle="World Map for GDP per Capita-2015",colourPalette="terrain")


library(alluvial)


df<- do.call(rbind, lapply(split(happy, happy$Region), function(x){
  data.frame(happy, Level =  cut(happy$HappinessScore, 
                                 quantile(happy$HappinessScore, probs = c(0, .25, .75, 1)), 
                                 labels = c('Low', 'Medium', 'High'), 
                                 include.lowest = TRUE))
}))
df <- as.data.frame(df)
summary(df)

alluv.df <- aggregate(df$HappinessScore
                      , list(df$Region,df$Level)
                      , sum)
colnames(alluv.df) <- c("Region", "Happiness", "Score")

alluvial(alluv.df[,1:2], freq = alluv.df$Score
         , col = ifelse(alluv.df$Happiness  == "Low", "red",ifelse(alluv.df$Happiness == "Medium", "gold","darkgreen"))
         # , alpha = .8
         # , gap.width = .1
         , cex = .48
)

