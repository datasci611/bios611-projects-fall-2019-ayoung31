library(ggplot2)
library(tidyverse)
library(multcompView)
library(car)

### DATA IMPORT ###

raw <- read_tsv("C:/Users/amyou/Documents/GitHub/bios611-projects-fall-2019-ayoung31/project_1/data/UrbanMinistriesData.tsv") %>% 
  select(c(1,6,7,8)) #only include variables of interest

colnames(raw) <- c('dateChar', 'FoodProvidedFor', 'foodPounds', 'clothing')

### INITIAL DATA CLEANING AND NEW VARIABLE CREATION ###

df <- raw %>%
  drop_na() %>%
  mutate(date = as.Date(dateChar, "%m/%d/%Y")) %>% #Properly format date variable
  select(-dateChar) %>% #drop the character format date
  mutate(year=format(date, "%Y")) %>% #extract year from date
  filter(year > 1983 & year < 2020) %>% #remove invalid years
  mutate(week=format(date, "%W")) %>% #week of year 0-54
  filter(foodPounds < 450121) %>% #remove outlier from pounds of food
  mutate(bigFam = FoodProvidedFor > 5) %>% #create indicator for large families
  mutate(weekday = weekdays(date)) %>% #create day of week variable (mon-sun)
  mutate(weekdayNum = as.numeric(format(date, "%u"))) %>% #create numeric day of week (1-7, mon=1)
  filter(weekdayNum != 6 & weekdayNum != 7) #Not open on weekends now, so these days not useful
  

###################################################################################
### Create datasets of weekly and daily totals ###

weeklyVals <- df %>%
  group_by(year, week) %>%
  summarize(numBig=sum(bigFam), totPounds=sum(foodPounds),totclothes=sum(clothing), visitors=n())
  
dailyVals <- df %>% 
  group_by(date, weekday, weekdayNum) %>% #weekday and weekdayNum here bc want both to be included in dailyVals
  summarize(numBig=sum(bigFam), totPounds=sum(foodPounds),totclothes=sum(clothing), visitors=n()) %>%
  mutate(propBig = numBig/visitors) %>%
  mutate(year=format(date, "%Y"))

###################################################################################
### Week of Year plots ###

#Food trends by week of year (0-54)
foodVweek <- ggplot(data=weeklyVals, aes(x=as.numeric(week), y=totPounds)) +
  geom_point(color="plum4") + 
  geom_smooth(method="lm", color="plum4")+
  scale_x_continuous(breaks=seq(0,54,5)) +
  theme_classic()+
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=16),
        plot.title = element_text(size = 18)) +
  labs(title="Total Pounds of Food Distributed by Week of the Year", x="Week of Year", y="Total Food (lbs) Distributed")
foodVweek

ggsave("foodVweek.png", foodVweek, width=7, height=4)

#Clothes trends by week of year (0-54)
clothesVweek <- ggplot(data=weeklyVals, aes(x=as.numeric(week), y=totclothes)) +
  geom_point(color="plum4") + 
  geom_smooth(method="lm", color="plum4")+
  scale_x_continuous(breaks=seq(0,54,5)) +
  theme_classic()+
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=16),
        plot.title = element_text(size = 18)) +
  labs(title="Total Clothing Items Distributed by Week of the Year", x="Week of Year", y="Total Clothing Items Distributed")
clothesVweek

ggsave("clothesVweek.png", clothesVweek, width=7, height=4)


###################################################################################
### Weekly analysis ###

#On average, how many pounds of food do they distribute per week?
avgWeekLbs <- mean(weeklyVals$totPounds)
confLbs <- t.test(weeklyVals$totPounds, conf.level=.99)$conf.int

#On average, how many clothing items do they distribute per week?
avgWeekClo <- mean(weeklyVals$totclothes)
confClo <- t.test(weeklyVals$totclothes, conf.level=.99)$conf.int


####################################################################################
### Weekday Analysis ###

#create dataframe of predicted values to be used in several analyses
new <- data.frame(c(1,2,3,4,5))
colnames(new) <- "weekdayNum"

## Predict Pounds of Food based on Weekday
poundsDay <- lm(totPounds ~ as.factor(weekdayNum), dailyVals)

#homogeneity of variance
leveneTest(totPounds ~ as.factor(weekday), data = dailyVals)
#variance not homogeneous so use pairwise t-tests without pooled standard error

#Test for friday significance
pairwise.t.test(dailyVals$totPounds, dailyVals$weekday,
                p.adjust.method = "BH", pool.sd = FALSE)
#Borderline significant, will mark as significant on plot

predicted <- data.frame(c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"), predict(poundsDay, new, interval="confidence", level=.95))
colnames(predicted) <- c("Weekday", "Predicted", "Lower", "Upper")

lbsPlot <- ggplot(predicted, aes(x=Weekday, y=Predicted))+
  geom_bar(stat="identity", fill="lightseagreen", color="black")+
  geom_errorbar(aes(ymin=Lower, ymax=Upper), width=.2)+
  theme_classic()+
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=16),
        plot.title = element_text(size = 18)) +
  annotate(geom="text", x=5, y=410, label="*")+
  scale_x_discrete(limits=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))+
  labs(title="Predicted Pounds of Food Needed by Day of the Week", y="Pounds of Food", x="Day of the Week")
lbsPlot

ggsave("predictedPounds.png", lbsPlot, width=7, height=4)

#############

## predict number of clothing items based on weekday
clothes <- lm(totclothes ~ as.factor(weekdayNum), dailyVals)

#homogeneity of variance
leveneTest(totclothes ~ as.factor(weekday), data = dailyVals)
#variance homogeneous between groups, so can use tukey test

#Test for friday significance
anova <- aov(clothes)
TukeyHSD(anova)
#Significant

predClothes <- data.frame(c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"), predict(clothes, new, interval="confidence", level=.95))
colnames(predClothes) <- c("Weekday", "Predicted", "Lower", "Upper")

clothesPlot <- ggplot(predClothes, aes(x=Weekday, y=Predicted, group=1))+
  geom_bar(stat="identity", fill="plum4", color="black")+
  theme_classic()+
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=16),
        plot.title = element_text(size = 18)) +
  annotate(geom="text", x=5, y=110, label="*")+
  geom_errorbar(aes(ymin=Lower, ymax=Upper), width=.2)+
  scale_x_discrete(limits=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))+
  labs(title="Predicted Clothing Items Needed by Day of the Week", x="Day of the Week", y="Number of Clothing Items")
clothesPlot

ggsave("predictedClothes.png", clothesPlot, width=7, height=4)

#################

## predict proportion of large families based on weekday
large <- lm(propBig ~ as.factor(weekdayNum), data=dailyVals)

#homogeneity of variance
leveneTest(propBig ~ as.factor(weekday), data = dailyVals)
#variance not homogeneous so use pairwise t-tests without pooled standard error

#Test for friday significance
pairwise.t.test(dailyVals$propBig, dailyVals$weekday,
                p.adjust.method = "BH", pool.sd = FALSE)
#Friday not significantly different than all others

predictBig <- data.frame(c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"), predict(large, new, interval="confidence", level=.95))
colnames(predictBig) <- c("Weekday", "Predicted", "Upper", "Lower")

bigPlot <- ggplot(predictBig, aes(x=Weekday, y=Predicted, group=1))+
  geom_bar(stat="identity", fill="tomato1", color="black")+
  theme_classic()+
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=16),
        plot.title = element_text(size = 18)) +
  geom_errorbar(aes(ymin=Lower, ymax=Upper), width=.2)+
  scale_x_discrete(limits=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))+
  labs(title="Predicted Proportion of Large Families by Day of Week", x="Day of the Week", y="Proportion of Large Families")
bigPlot

ggsave("predictedBigs.png", bigPlot, width=7, height=4)

###############

## predict number of families from day of week
visits <- lm(visitors ~ as.factor(weekdayNum), dailyVals)

#homogeneity of variance
leveneTest(totclothes ~ as.factor(weekday), data = dailyVals)
#variance homogeneous between groups, so can use tukey test

#test for friday significance
anova2 <- aov(visits)
TukeyHSD(anova2)
#Friday is significantly different from all others

predVisits <- data.frame(c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"), predict(visits, new, interval="confidence", level=.95))
colnames(predVisits) <- c("Weekday", "Predicted", "Lower", "Upper")

visitsPlot <- ggplot(predVisits, aes(x=Weekday, y=Predicted, group=1))+
  geom_bar(stat="identity", fill="tomato1", color="black")+
  theme_classic()+
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=16),
        plot.title = element_text(size = 18)) +
  annotate(geom="text", x=5, y=12, label="*")+ #Mark Friday significance
  geom_errorbar(aes(ymin=Lower, ymax=Upper), width=.2)+
  scale_x_discrete(limits=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))+
  labs(title="Predicted Number of Families by Day of the Week", x="Day of the Week", y="Number of Families")
visitsPlot

ggsave("predictedVisitors.png", visitsPlot, width=7, height=4)


#####################################################################################
### Visitors by Weekday and Year ###

meanVisitors <- dailyVals %>%
  group_by(year, weekday) %>%
  summarise(mean=mean(visitors)) %>%
  filter(year>=2008)

yearlyDist <- ggplot(meanVisitors, aes(x=as.factor(weekday), y=mean))+
  geom_bar(stat="identity")+
  facet_wrap(~year)+
  labs(title="Average Number of Visitors per Weekday for Last Several Years", x="Weekday", y="Average Number of Visitors")+
  scale_x_discrete(limits=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"), labels=c("Mon", "Tues", "Wed", "Thurs", "Fri"))
yearlyDist

ggsave("yearlyDist.png", yearlyDist, width=6.5, height=6.5)


#####################################################################################
### Redo Weekday analysis only with years 2018-2019 ###

dailyValsRestricted <- dailyVals %>% filter(year>2017)

## Clothing
clothes2 <- lm(totclothes ~ as.factor(weekdayNum), dailyValsRestricted)

predClothes2 <- data.frame(c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"), predict(clothes2, new, interval="confidence", level=.95))
colnames(predClothes2) <- c("Weekday", "Predicted", "Lower", "Upper")

clothesPlot2 <- ggplot(predClothes2, aes(x=Weekday, y=Predicted, group=1))+
  geom_bar(stat="identity", fill="plum4", color="black")+
  theme_classic()+
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=16),
        plot.title = element_text(size = 18)) +
  geom_errorbar(aes(ymin=Lower, ymax=Upper), width=.2)+
  scale_x_discrete(limits=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))+
  labs(title="Predicted Clothing Items Needed by Day of the Week", x="Day of the Week", y="Number of Clothing Items")
clothesPlot2

ggsave("updatedClothes.png", clothesPlot2, width=7, height=4)

###############

## Pounds of food
poundsDay2 <- lm(totPounds ~ as.factor(weekdayNum), dailyValsRestricted)

predicted2 <- data.frame(c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"), predict(poundsDay2, new, interval="confidence", level=.95))
colnames(predicted2) <- c("Weekday", "Predicted", "Lower", "Upper")

lbsPlot2 <- ggplot(predicted2, aes(x=Weekday, y=Predicted))+
  geom_bar(stat="identity", fill="lightseagreen", color="black")+
  geom_errorbar(aes(ymin=Lower, ymax=Upper), width=.2)+
  theme_classic()+
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=16),
        plot.title = element_text(size = 18)) +
  scale_x_discrete(limits=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))+
  labs(title="Predicted Pounds of Food Needed by Day of the Week", y="Pounds of Food", x="Day of the Week")
lbsPlot2

ggsave("updatedLbs.png", lbsPlot2, width=7, height=4)


#####################################################################################
### Plots of Items Vs. Number of Large Families ###

# foodPounds v. big families
lbsVbig <- ggplot(filter(dailyVals, totPounds<1500 & numBig <= 15), aes(x=numBig, y=totPounds))+
  geom_point(color="plum4")+
  geom_smooth(method=lm, se=F, color="plum4")+
  theme_classic()+
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=16),
        plot.title = element_text(size = 18)) +
  labs(title="Daily Pounds of Food Distributed vs.\nNumber of Large Families", y="Daily Total Pounds of Food", x="Number of Large Families")
lbsVbig

ggsave("poundsVnumLarge.png", lbsVbig, width=7, height=4)


# clothing v big families
cloVbig <- ggplot(filter(dailyVals, numBig <= 15), aes(x=numBig, y=totclothes))+
  geom_point(color="lightseagreen")+
  geom_smooth(method=lm, se=F, color="lightseagreen")+
  theme_classic()+
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=16),
        plot.title = element_text(size = 18)) +
  labs(title="Daily Number of Clothes Distributed vs.\nNumber of Large Families", x="Number of Large Families", y="Daily Total Number of Clothes")
cloVbig

ggsave("clothesVnumLarge.png", cloVbig, width=7, height=4)


#clothing v totPounds v numBig
cloVlbsVbig <- ggplot(filter(dailyVals, totPounds < 1500 & totclothes > 0 & totPounds > 0 & numBig <= 15), aes(y=totPounds, x=totclothes))+
  geom_point(aes(color=numBig))+
  theme_classic()+
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=16),
        plot.title = element_text(size = 18)) +
  scale_color_gradientn(colors=c("lightseagreen", "plum4"))+
  labs(title="Daily Number of Clothes Distributed vs.\nDaily Pounds of food Distributed", color="Number of\nLarge Families", x="Daily Total Number of Clothes", y="Daily Total Pounds of Food")
cloVlbsVbig

ggsave("clothesVpoundsVnumBig.png", cloVlbsVbig, width=7, height=4)
