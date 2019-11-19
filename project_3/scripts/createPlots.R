library(tidyverse)

df <- read_csv("cleaned.csv", col_names=T)
df <- df[,2:49]

#Percent of Clients by Gender
df3 <- df %>% group_by(Gender) %>% summarise(percent=n()/5299*100)
ggplot(df3, aes(x=Gender, y=percent, fill=Gender))+
  geom_bar(width=1, stat="identity")+
  coord_flip()+
  theme(legend.position="none")+
  labs(title="Percent of Clients by Gender", x="Gender", y="Percent of Clients")


#Percent of Clients by Race
df2 <- df[df$Race=="White (HUD)" | df$Race=="Black or African American (HUD)" | df$Race=="American Indian or Alaska Native (HUD)" & !is.na(df$Race),]
df3 <- df2 %>% group_by(Race) %>% summarise(percent=n()/5299*100)
ggplot(df3, aes(x=Race, y=percent, fill=Race))+
  geom_bar(width=1, stat="identity")+
  coord_flip()+
  theme(legend.position="none")+
  labs(title="Percent of Clients by Race", x="Race", y="Percent of Clients")

#Number of Clients by Age
ggplot(df, aes(x=Age_Entry))+
  geom_histogram(bins=30)+
  labs(title="Emergency Shelter Clients by Age", x="Age at Entry to Shelter", y="Number of Clients")

#Percent of Clients by Disability Status
df_dis <- df[df$Disabling_Condition=='Yes (HUD)' | df$Disabling_Condition=='No (HUD)' | is.na(df$Disabling_Condition),]
df3 <- df_dis %>% group_by(Disabling_Condition) %>% summarise(percent=n()/5299*100)
ggplot(df3, aes(x=Disabling_Condition, y=percent, fill=Disabling_Condition))+
  geom_bar(width=1, stat="identity")+
  coord_flip()+
  theme(legend.position="none")+
  labs(title="Percent of Clients by Disability Status", x="Disability Status", y="Percent of Clients")

#Percent of clients by Reason for Leaving
df3 <- df %>% group_by(Reason_for_Leaving) %>% summarise(percent=n()/5299*100)
ggplot(df3, aes(x=reorder(Reason_for_Leaving, -percent), y=percent, fill=Reason_for_Leaving))+
  geom_bar(width=1, stat="identity")+
  coord_flip()+
  theme(legend.position="none")+
  labs(title="Percent of Clients by Reason for Leaving", x="Reason for Leaving", y="Percent of Clients")


#Histogram of how long people stay at the shelter
per90 <- quantile(df$days_at_shelter, probs=.5, na.rm=T)
ggplot(df, aes(x=days_at_shelter))+
  geom_histogram()+
  geom_vline(xintercept = per90, color="red")+
  labs(title="Number of Clients by How Long They Stay", x="Number of Nights Spent at the Shelter", y="Number of Clients")+
  geom_text(aes(per90-50,2700,label = '90%'))+
  annotate("rect", xmin = -1, xmax = per90, ymin = -1, ymax = 2750,
           alpha = .2, fill="red")


##Avg Length of stay by Race
df2 <- df %>% group_by(Race) %>% summarise(mean(days_at_shelter, na.rm=T))
colnames(df2) <- c("Race", "Mean Days")
ggplot(df2, aes(x=Race, y=`Mean Days`, fill=Race))+
  geom_bar(width=1, stat="identity")+
  labs(title="Average Number of Nights at Shelter by Race",x="Race", y="Number of Nights at the shelter")+
  coord_flip()+
  theme(legend.position="none")

##Avg Length of stay by Disability Status
df2 <- df_dis %>% group_by(Disabling_Condition) %>% summarise(mean(days_at_shelter, na.rm=T))
colnames(df2) <- c("Disability Status", "Mean Days")
ggplot(df2, aes(x=`Disability Status`, y=`Mean Days`, fill=`Disability Status`))+
  geom_bar(width=1, stat="identity")+
  labs(title="Average Number of Nights at Shelter by Disability Status",x="Disability Status", y="Number of Nights at the shelter")+
  coord_flip()+
  theme(legend.position="none")


