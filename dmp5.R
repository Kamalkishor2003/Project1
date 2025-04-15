library(dplyr)
accidents.df <- read.csv("AccidentsFull.csv")
View(accidents.df)
accidents.df$INJURY <- ifelse(accidents.df$MAX_SEV_IR>0,
                              "yes", "no")
head(accidents.df)

#Part A
#create a table based on INJURY
inj.tbl <- table(accidents.df$INJURY)
show(inj.tbl)
#caluculate probability of injury
inj.prob = scales::percent(inj.tbl["yes"]/(inj.tbl["yes"]+inj.tbl["no"]),0.01)
inj.prob
#Ans: Since ~51% of the accidents in our data set resulted in an accident, we should predict that an accident will result in injury because it is slightly more likely.

#Part B
#convert your variables to categorical type
for (i in c(1:dim(accidents.df)[2])){
  accidents.df[,i] <- as.factor(accidents.df[,i])
}
#create a new subset with only the required records
new.df <- accidents.df[1:12,
                       c("INJURY","WEATHER_R","TRAF_CON_R")]
new.df
#B i
#Load library for pivot table
install.packages("rpivotTable")
library(rpivotTable)
rpivotTable::rpivotTable(new.df)
#B.ii
#Compute the exact Bayes conditional probabilities of an injury (INJURY = Yes) given the six possible combinations of the predictors.
#To find P(Injury=yes|WEATHER_R = 1, TRAF_CON_R =0):
numerator1 <- 2/3 * 3/12
denominator1 <- 3/12
prob1 <- numerator1/denominator1
prob1
#To find P(Injury=yes|WEATHER_R = 1, TRAF_CON_R =1):
numerator2 <- 0 * 3/12
denominator2 <- 1/12
prob2 <- numerator2/denominator2
prob2
#To find P(Injury=yes| WEATHER_R = 1, TRAF_CON_R =2):
numerator3 <- 0 * 3/12
denominator3 <- 1/12
prob3 <- numerator3/denominator3
prob3
#To find P(Injury=yes| WEATHER_R = 2, TRAF_CON_R =0):
numerator4 <- 1/3 * 3/12
denominator4 <- 6/12
prob4 <- numerator4/denominator4
prob4
#To find P(Injury=yes| WEATHER_R = 2, TRAF_CON_R =1):
numerator5 <- 0 * 3/12
denominator5 <- 1/12
prob5 <- numerator5/denominator5
prob5
#To find P(Injury=yes| WEATHER_R = 2, TRAF_CON_R =2):
numerator6 <- 0 * 3/12
denominator6 <- 0
prob6 <- numerator6/denominator6
prob6
a<-c(1,2,3,4,5,6)
b<-c(prob1,prob2,prob3,prob4,prob5,prob6)
prob.df<-data.frame(a,b)
names(prob.df)<-c('Option #','Probability')
prob.df %>% mutate_if(is.numeric, round, 3)
#NOTE: In the above 12 observations there is no observation with (Injury=yes, WEATHER_R = 2, TRAF_CON_R =2). The conditional probability here is undefined, since the denominator is zero.
#B.iii Classify the 12 accidents using these probabilities and a cutoff of 0.5.
#add probability results to you subset
new.df.prob<-new.df
head(new.df.prob)
prob.inj <- c(0.667, 0.167, 0, 0, 0.667, 0.167, 0.167, 0.667,
              0.167, 0.167,
              0.167, 0)
new.df.prob$PROB_INJURY<-prob.inj
#add a column for injury prediction based on a cutoff of 0.5
new.df.prob$PREDICT_PROB<-
  ifelse(new.df.prob$PROB_INJURY>.5,"yes","no")
new.df.prob
#B.iv
man.prob <- 2/3 * 0/3 * 3/12
man.prob
#B.v
install.packages("e1071")
library(e1071)
install.packages("klaR")
library(klaR)
library(caret)
library(ggplot2)
library(lattice)
nb<-naiveBayes(INJURY ~ ., data = new.df)
predict(nb, newdata = new.df,type = "raw")
x=new.df[,-3]
y=new.df$INJURY
model <- train(x,y,'nb', trControl = trainControl(method ='cv',number=10))
model
#Now that we have generated a classification model, we use it for prediction
model.pred<-predict(model$finalModel,x)
model.pred
##build a confusion matrix so that we can visualize the classification errors
table(model.pred$class,y)
#compare against the manually calculated results
new.df.prob$PREDICT_PROB_NB<-model.pred$class
new.df.prob

#Part c
#Let us now return to the entire dataset. Partition the data into training (60%) and validation (40%).
set.seed(22)
train.index <- sample(c(1:dim(accidents.df)[1]),
                      dim(accidents.df)[1]*0.6)
train.df <- accidents.df[train.index,]
valid.df <- accidents.df[-train.index,]

#C.i
#We can use the predictors that describe the calendar time or road
#conditions: HOUR_I_R ALIGN_I WRK_ZONE WKDY_I_R INT
#_HWY LGTCON_I_R PROFIL_I_R SPD_LIM SUR_CON TRAF
#_CON_R TRAF_WAY WEATHER_R

#C.ii
#define which variable you will be using
vars <- c("INJURY", "HOUR_I_R", "ALIGN_I" ,"WRK_ZONE",
          "WKDY_I_R",
          "INT_HWY", "LGTCON_I_R", "PROFIL_I_R",
          "SPD_LIM", "SUR_COND",
          "TRAF_CON_R", "TRAF_WAY", "WEATHER_R")
nbTotal <- naiveBayes(INJURY ~ ., data = train.df[,vars])
#generate the confusion matrix using the train.df, the prediction and the classes
confusionMatrix(train.df$INJURY, predict(nbTotal, train.df[,vars]), positive = "yes")
ner=1-.5384
nerp=scales::percent(ner,0.01)
nerp
#C.iii What is the overall error for the validation set?
confusionMatrix(valid.df$INJURY, predict(nbTotal, valid.df[,vars]),positive = "yes")
ver=1-.5354
verp=scales::percent(ver,0.01)
paste("Overall Error: ",verp)
#C.iv What is the percent improvement relative to the naive rule (using the validation set)?
imp=ver-ner
paste("The percent improvement is ",scales::percent(imp,0.01))
#C.v Examine the conditional probabilities output. Why do we get a probability of zero for P(INJURY = No j SPD_LIM = 5)?
options(digits = 2)
nbTotal
