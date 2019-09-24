library(ggplot2)
library(tidyverse)

### DATA IMPORT ###

df <- read_tsv("C:/Users/amyou/Documents/GitHub/bios611-projects-fall-2019-ayoung31/project_1/data/UrbanMinistriesData.tsv") %>% 
  select(c(1,6,7,8)) #only include variables of interest

colnames(df) <- c('dateChar', 'FoodProvidedFor', 'foodPounds', 'clothing')

### INITIAL DATA CLEANING AND NEW VARIABLE CREATION ###

df <- df %>%
  mutate(date = as.Date(dateChar, "%m/%d/%Y")) %>% #Properly format date variable
  select(-dateChar) %>% #drop the character format date
  mutate(year=format(date, "%Y")) %>% #extract year from date
  filter(year > 1983 & year < 2020) %>% #remove invalid years
  mutate(week=cut(date, breaks = "week")) %>% #create week variable
  filter(foodPounds < 450121) %>% #remove outlier from pounds of food
  mutate(bigFam = FoodProvidedFor > 4) %>% #create indicator for large families
  mutate(weekday = weekdays(date)) %>% #create day of week variable (mon-sun)
  mutate(weekdayNum = as.numeric(format(date, "%u"))) #create numeric day of week (1-7, mon=1)

weeklyVals <- df %>%
  group_by(week) %>%
  summarize(totPounds=sum(foodPounds, na.rm=TRUE),totclothes=sum(clothing, na.rm=TRUE))

dailyVals <- df %>% 
  group_by(date, weekday, weekdayNum) %>% 
  summarize(numBig=sum(bigFam, na.rm=TRUE), totPounds=sum(foodPounds, na.rm=TRUE),totclothes=sum(clothing, na.rm=TRUE))

### Weekly analysis ###

#On average, how many pounds of food do they distribute per week?
avgWeekLbs <- mean(dailyVals$totPounds)
confLbs <- t.test(dailyVals$totPounds, conf.level=.98)$conf.int

#On average, how many clothing items do they distribute per week?
avgWeekClo <- mean(dailyVals$totclothes)
confClo <- t.test(dailyVals$totclothes, conf.level=.98)$conf.int

### Weekday Analysis ###

#Predict Pounds of Food based on Weekday
poundsDay <- lm(totPounds ~ as.factor(weekdayNum), dailyVals)
new <- data.frame(c(1,2,3,4,5))
colnames(new) <- "weekdayNum"

predicted <- data.frame(c("Monday", "Tueday", "Wednesday", "Thursday", "Friday"), predict(poundsDay, new, interval="confidence", level=.98))
colnames(predicted) <- c("Weekday", "Predicted", "Lower", "Upper")

lbsPlot <- ggplot(predicted, aes(x=Weekday, y=Predicted))+
  geom_bar(stat="identity", fill="lightseagreen", color="black")+
  theme_classic()+
  scale_x_discrete(limits=c("Monday", "Tueday", "Wednesday", "Thursday", "Friday"))+
  labs(title="Predicted Pounds of Food Distributed on each Day of the Week", y="Pounds of Food", x="Day of the Week")

ggsave("predictedPounds.png", lbsPlot)


#predict number of large families based on weekday
large <- lm(numBig ~ as.factor(weekdayNum), data=dailyVals)
predictBig <- data.frame(c("Monday", "Tueday", "Wednesday", "Thursday", "Friday"), predict(large, new, interval="confidence", level=.98))
colnames(predictBig) <- c("Weekday", "Predicted")

bigPlot <- ggplot(predictBig, aes(x=Weekday, y=Predicted, group=1))+
  geom_bar(stat="identity", fill="lightseagreen", color="black")+
  theme_classic()+
  scale_x_discrete(limits=c("Monday", "Tueday", "Wednesday", "Thursday", "Friday"))+
  labs(title="Predicted Number of Large Families on each Day of the Week", x="Day of the Week", y="Number of Large Families (>4)")

ggsave("predictedBigs.png", bigPlot)


#predict number of clothing items based on weekday
clothes <- lm(totclothes ~ as.factor(weekdayNum), dailyVals)
predClothes <- data.frame(c("Monday", "Tueday", "Wednesday", "Thursday", "Friday"), predict(clothes, new, interval="confidence", level=.98))
colnames(predClothes) <- c("Weekday", "Predicted", "Lower", "Upper")

clothesPlot <- ggplot(predClothes, aes(x=Weekday, y=Predicted, group=1))+
  geom_bar(stat="identity", fill="lightseagreen", color="black")+
  theme_classic()+
  scale_x_discrete(limits=c("Monday", "Tueday", "Wednesday", "Thursday", "Friday"))+
  labs(title="Predicted Number Clothing Items Distributed on each Day of the Week", x="Day of the Week", y="Number of Clothing Items")

ggsave("predictedClothes.png", clothesPlot)


### Useful visualizations ###

# foodPounds v. big families
lbsVbig <- ggplot(filter(dailyVals, totPounds<1500, numBig < 15), aes(x=numBig, y=totPounds))+
  geom_point()+
  geom_smooth(method=lm, se=F)+
  labs(title="Daily Pounds of Food Distributed vs. Number of Large Families", y="Daily Total Pounds of Food", x="Number of Large Families in a Day")

ggsave("poundsVnumLarge.png", lbsVbig)


# clothing v big families
cloVbig <- ggplot(filter(dailyVals, numBig <15), aes(x=numBig, y=totclothes))+
  geom_point()+
  geom_smooth(method=lm, se=F)+
  labs(title="Daily Number of Clothes Distributed vs. Number of Large Families", x="Number of Large Families in a Day", y="Daily Total Number of Clothes")

ggsave("clothesVnumLarge.png", cloVbig)


#clothing v totPounds v numBig
cloVlbsVbig <- ggplot(filter(dailyVals, totPounds < 1500 & totclothes > 0 & totPounds > 0 & numBig <= 15), aes(x=totPounds, y=totclothes))+
  geom_point(aes(color=numBig))+
  labs(title="Daily Number of Clothes Distributed vs. Number Clothes Distributed", color="Number of\nLarge Families\nin a Day", y="Daily Total Number of Clothes", x="Daily Total Pounds of Food")

ggsave("clothesVpoundsVnumBig.png", cloVlbsVbig)
