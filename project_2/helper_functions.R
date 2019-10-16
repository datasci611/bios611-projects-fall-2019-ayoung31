
read <- function(){
  raw <- read_tsv("C:/Users/amyou/Documents/GitHub/bios611-projects-fall-2019-ayoung31/project_2/data/UrbanMinistriesData.tsv") %>% 
    select(c(1,6,7,8)) #only include variables of interest
  
  colnames(raw) <- c('dateChar', 'FoodProvidedFor', 'foodPounds', 'clothing')
  return(raw)
}

clean <- function(raw){
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
  return(df)
}

makeWeekly <- function(df){
  weeklyVals <- df %>%
    group_by(year, week) %>%
    summarize(numBig=sum(bigFam), totPounds=sum(foodPounds),totclothes=sum(clothing), visitors=n())
  return(weeklyVals)
}

makeDaily <- function(df){
  dailyVals <- df %>% 
    group_by(date, weekday, weekdayNum) %>% #weekday and weekdayNum here bc want both to be included in dailyVals
    summarize(numBig=sum(bigFam), `Pounds of Food`=sum(foodPounds),`Number of Clothes`=sum(clothing), visitors=n()) %>%
    mutate(`Proportion of Large Families` = numBig/visitors) %>%
    mutate(year=format(date, "%Y"))
  return(dailyVals)
}

weekpred <- function(){
  new <- data.frame(c(1,2,3,4,5))
  colnames(new) <- "weekdayNum"
  return(new)
}

prediction <- function(df, var, new){
  model <- lm(get(var) ~ as.factor(weekdayNum), df)
  predicted <- data.frame(c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"), predict(model, new, interval="confidence", level=.95))
  colnames(predicted) <- c("Weekday", "Predicted", "Lower", "Upper")
  return(predicted)
}
  
plotDaily <- function(df, var, new){
  model <- lm(get(var) ~ as.factor(weekdayNum), df)
  predicted <- data.frame(c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"), predict(model, new, interval="confidence", level=.95))
  colnames(predicted) <- c("Weekday", "Predicted", "Lower", "Upper")
  ggplot(predicted, aes(x=Weekday, y=Predicted))+
    geom_bar(stat="identity", fill="lightseagreen", color="black")+
    geom_errorbar(aes(ymin=Lower, ymax=Upper), width=.2)+
    theme_classic()+
    scale_x_discrete(limits=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))+
    labs(title=paste("Predicted",get("var"),"Needed by Day of the Week"), y=get("var"), x="Day of the Week")
}

