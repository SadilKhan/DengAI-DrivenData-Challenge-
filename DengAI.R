setwd("/home/sadil/Data Science Projects/DengAI")

#Insights and Questions
"There are 6533 cases in year 1994 and only 1333 cases in 1995 and then it decreased, 
need to chcek this two years and what changed during that.

After 30th Week in 1994 The cases went to a peak so need to check what happened that it went to a peak.

Until 2007 after 30th week the cases went high. Two questions arise
1. Why sudden pattern change after 2007?
2. What actually happens after 30th week. What factor contributes to this?

After 2000 there is a drop and rise situation?

The city sj has more cases than lq. Need to check what affects more"

library(tidyverse) # For Data Analysis
library(ggplot2) # For Plotting
library(ggrepel)
library(corrplot)
library(gganimate)
library(gifski)
library(mlr) # For Machine Learning
library(caTools) # For spliting Dataset
library(plyr) # Replace function
library(Hmisc) # For Missing Value Imputation


train_feature=read.csv("dengue_features_train.csv") # Total_cases are provided
train_labels=read.csv("dengue_labels_train.csv")

# Checking for missing Values
summary(train_feature)
train_labels %>% group_by(city)%>% summarise(sum=sum(total_cases))


# Merging the dataframes
train_feature=merge(train_feature,train_labels,by=c('city','year','weekofyear'),all.x = TRUE)

# Changing the year column
train_feature$year=train_feature$year-1990

# Changing the type of column Year into factor
train_feature$year = factor(train_feature$year)
test$year=factor(test$year)

# Changing the data type of the week_start_date
train_feature$week_start_date=as.Date(train_feature$week_start_date)
test$week_start_date=as.Date(train_feature$week_start_date)

# Plotting barplot of year column
ggplot(train_feature,aes(year,fill=year))+geom_bar()


#extra=data.frame(total_cases=train_labels$total_cases,week_start_date=train_feature$week_start_date)

ggplot(train_feature,aes(x=week_start_date,y=total_cases))+geom_line(color='#006994')

train_labels$year=factor(train_labels$year)
train_labels %>% group_by(year) %>% summarise(count=n())

# Plotting total cases per week
train_labels %>% group_by(year) %>% summarise(total=sum(total_cases),count=n()) %>% 
  arrange(desc(total)) %>% ggplot(aes(x=year,y=total,fill=factor(year),alpha=0.4))+geom_bar(stat = 'identity')+
  geom_label_repel(aes(label=count),hjust=0,vjust=-3,box.padding=0.35,point.padding=0.5,segment.colour='magenta',segment.alpha=0.5)+
  geom_label_repel(aes(label=total),hjust=-2,vjust=-1,box.padding=0.34,point.padding=0.3,segment.colour='navyblue',segment.alpha=0.5)+
  ggtitle("Total cases per year")


# Bar charts of total_cases per week in 1994
  train_labels %>% group_by(year) %>% filter(year==1994) %>% ggplot(aes(weekofyear,total_cases,
                                                                      alpha=0.4))+geom_line(color='red')+
  geom_label_repel(aes(label=total_cases),hjust=0,vjust=0,box.padding = 0.4,point.padding = 0.6,segment.color = 'blue')+
  geom_label_repel(aes(label=weekofyear),hjust=0,vjust=-3,box.padding = 0.4,point.padding = 0.6,segment.color = 'red')+
  ggtitle("1994 Dengue case in various weeks")


  
train_labels %>% group_by(year) %>% filter(year==1994,week==40) %>% ggplot(aes(weekofyear,total_cases,
                                                                      alpha=0.4))+geom_line(color='red')+
  geom_label_repel(aes(label=total_cases),hjust=0,vjust=0,box.padding = 0.4,point.padding = 0.6,segment.color = 'blue')+
  geom_label_repel(aes(label=weekofyear),hjust=0,vjust=-3,box.padding = 0.4,point.padding = 0.6,segment.color = 'red')+
  ggtitle("1994 Dengue case in various weeks")

train_feature %>% group_by(year) %>% filter(year==1994,weekofyear==40)


ggplot(train_labels,aes(x=weekofyear,y=total_cases,color=factor(year)))+geom_line()+
  facet_wrap(~year,scales = "free")+ggtitle("Dengue cases per week in different years")




train_labels %>% group_by(city) %>% summarise(total=sum(total_cases),count=n()) %>% 
  ggplot(aes(city,total,color='red'))+geom_point(stat='identity')+geom_text(aes(label=total),hjust=0,vjust=-1)+
  geom_text(aes(label=count),hjust=0,vjust=0)+ggtitle("City vs total cases")


                            # Reason for 1994 and 1995 dengue case drop

train_feature %>% filter(year == c(1994,1995)) %>% ggplot(aes(weekofyear,station_avg_temp_c,
                                                    color=factor(year)))+geom_point(stat='identity')

"Avg temperature is not the issue for 1994 and 1995 dengue case drop"


train_feature %>% filter(year == c(1994,1995)) %>% ggplot(aes(weekofyear,precipitation_amt_mm,
                                                              color=factor(year)))+geom_line(stat='identity')

names=colnames(train_feature)

train_feature %>% filter(year==c(1992,1993,1994,1995,1996,1998,2000)) %>% group_by(year) %>% ggplot(aes(x=year,y=ndvi_ne,fill=year))+
  geom_boxplot()+facet_wrap(~year,scales="free",ncol=2)


train_feature %>% filter(year==c(1992,1994,1995,1996,2000)) %>% group_by(year) %>% 
  ggplot(aes(x=year,y=reanalysis_dew_point_temp_k,color=year,fill=year))+
  geom_boxplot(alpha=0.2)+transition_states(year,transition_length = 3,wrap = FALSE)+shadow_mark()



train_labels %>% filter(year==1996) %>% summarise(sum=sum(total_cases))



# Data Preprocessing
test=read.csv("dengue_features_test.csv" )
test$year=test$year-1990

data=train_feature %>% select(city,year,
                              weekofyear,
                              reanalysis_sat_precip_amt_mm,
                              reanalysis_dew_point_temp_k,reanalysis_air_temp_k,
                              reanalysis_relative_humidity_percent,
                              reanalysis_tdtr_k,total_cases,ndvi_ne,ndvi_nw,ndvi_se,ndvi_sw)
test=test%>% select(city,year,
                    weekofyear,
                    reanalysis_sat_precip_amt_mm,
                    reanalysis_dew_point_temp_k,reanalysis_air_temp_k,
                    reanalysis_relative_humidity_percent,
                    reanalysis_tdtr_k,ndvi_ne,ndvi_nw,ndvi_se,ndvi_sw)
# Creating Total cases column for test
test$total_cases=rep(0,length(test$city))
temp=test %>% select(c(city,year,weekofyear,total_cases))
temp$year=1990+temp$year


test$city=mapvalues(test$city,from=c('sj','iq'),to=c(0,1))
data$city=mapvalues(data$city,from=c('sj','iq'),to=c(0,1))

# Missing Value Imputation
data$reanalysis_sat_precip_amt_mm=with(data,impute(reanalysis_sat_precip_amt_mm,mean))
data$reanalysis_dew_point_temp_k=with(data,impute(reanalysis_dew_point_temp_k,mean))
data$reanalysis_air_temp_k=with(data,impute(reanalysis_air_temp_k,mean))
data$reanalysis_relative_humidity_percent=with(data,impute(reanalysis_relative_humidity_percent,mean))
data$reanalysis_tdtr_k=with(data,impute(reanalysis_tdtr_k,mean))
data$ndvi_ne=with(data,impute(ndvi_ne,mean))
data$ndvi_nw=with(data,impute(ndvi_nw,mean))
data$ndvi_se=with(data,impute(ndvi_ne,mean))
data$ndvi_sw=with(data,impute(ndvi_sw,mean))




test$reanalysis_sat_precip_amt_mm=with(test,impute(reanalysis_sat_precip_amt_mm,mean))
test$reanalysis_dew_point_temp_k=with(test,impute(reanalysis_dew_point_temp_k,mean))
test$reanalysis_air_temp_k=with(test,impute(reanalysis_air_temp_k,mean))
test$reanalysis_relative_humidity_percent=with(test,impute(reanalysis_relative_humidity_percent,mean))
test$reanalysis_tdtr_k=with(test,impute(reanalysis_tdtr_k,mean))
test$ndvi_ne=with(test,impute(ndvi_ne,mean))
test$ndvi_nw=with(test,impute(ndvi_nw,mean))
test$ndvi_se=with(test,impute(ndvi_ne,mean))
test$ndvi_sw=with(test,impute(ndvi_sw,mean))




#data$total_cases=log(data$total_cases+1)
ggplot(data,aes(total_cases))+geom_density()

# Changing the column year into numeric
data$year=as.numeric(data$year)
test$year=as.numeric(test$year)



# Scaling
data$year=scale(data$year)
data$weekofyear=scale(data$weekofyear)
data$reanalysis_sat_precip_amt_mm=scale(data$reanalysis_sat_precip_amt_mm)
data$reanalysis_dew_point_temp_k=scale(data$reanalysis_dew_point_temp_k)
data$reanalysis_air_temp_k=scale(data$reanalysis_air_temp_k)
data$reanalysis_relative_humidity_percent=scale(data$reanalysis_relative_humidity_percent)
data$reanalysis_tdtr_k=scale(data$reanalysis_tdtr_k)


# Scaling
test$year=scale(test$year)
test$weekofyear=scale(test$weekofyear)
test$reanalysis_sat_precip_amt_mm=scale(test$reanalysis_sat_precip_amt_mm)
test$reanalysis_dew_point_temp_k=scale(test$reanalysis_dew_point_temp_k)
test$reanalysis_air_temp_k=scale(test$reanalysis_air_temp_k)
test$reanalysis_relative_humidity_percent=scale(test$reanalysis_relative_humidity_percent)
test$reanalysis_tdtr_k=scale(test$reanalysis_tdtr_k)


# Spliting Dataset
sample=sample.split(data,SplitRatio = 0.90)
x_train=subset(data,sample==TRUE)
x_test=subset(data,sample==FALSE)


            " Training ML Algorithm"
# Create A Task
task_train=makeRegrTask(data=x_train,target = 'total_cases')
task_test=makeRegrTask(data=x_test,target = 'total_cases')

task=makeRegrTask(data=test,target = 'total_cases')
# Making a Learner
learner=makeLearner(cl='regr.randomForest')
xgb=makeLearner('regr.xgboost')
learner=setHyperPars(learner=learner,ntree=30)# Hyperparameter Setting
xgb=setHyperPars(xgb,max_depth=5,colsample_bytree=0.8)

# Training
alg=train(learner,task_train)

alg_xgb=train(learner,task_train)

# Predicting on x_train and x_test
pred_train=predict(alg_xgb,task_train)
pred_test=predict(alg_xgb,task_test)


performance(pred_train,measures =mae)
performance(pred_test,measures = mae)

performance(pred_train,measures =mse)
performance(pred_test,measures = mse)


test_pred=as.data.frame(predict(alg_xgb,task))

temp$total_cases=floor(test_pred$response)

write.csv(temp,file='submission.csv',col.names = FALSE)

