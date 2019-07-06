install.packages("dplyr")
library(dplyr)

install.packages("plyr")
library(plyr)

library(dplyr)
install.packages("tidyr")
library(tidyr)  

install.packages("e1071")
library(e1071)

install.packages("ggplot2")
library(ggplot2)

install.packages("reshape2")
library(reshape2)
#----------------------------------------------------------------------------------------------------
# read the csv files
athelete_events_data<-data.frame(read.csv("/Users/cindykhuu/Documents/Syracuse Master Data Science Program/IST 678 Applied Data Science/Data Science Project/athlete_events.csv", header= TRUE))
View(athelete_events_data)
# Explore highlevel dataset
summary(athelete_events_data)
str(athelete_events_data)

# find which columns in the dataframe contain NAs.
colnames(athelete_events_data)[colSums(is.na(athelete_events_data)) > 0]

# Check if the NAs in column and replace them by the mean value of this column
athelete_events_data$Age[is.na(athelete_events_data$Age)] <- mean(athelete_events_data$Age, na.rm=TRUE)
athelete_events_data$Height[is.na(athelete_events_data$Height)] <- mean(athelete_events_data$Height, na.rm=TRUE)
athelete_events_data$Weight[is.na(athelete_events_data$Weight)] <- mean(athelete_events_data$Weight, na.rm=TRUE)
summary(athelete_events_data)

# Subset only the summer data,which we are focusing on
athelete_events_data_summer<-filter(athelete_events_data, Season== "Summer")
# validate new subset dataframe
View(athelete_events_data_summer)

#----------------------------------------------------------------------------------------------------
#Create a dataframe to use for plotting that has count of athletes per NOC
NumAthlete_NOC<-data.frame(count(athelete_events_data_summer, "NOC"))
str(NumAthlete_NOC)
View(NumAthlete_NOC)

#Create bar chart of # of athletes particpated by NOC thats sorted for highest to lowest
ggplotBarNumAthlete_NOC2<- ggplot(NumAthlete_NOC, aes(x=reorder(NOC, -freq), y= freq)) +geom_bar(stat="identity")
ggplotBarNumAthlete_NOC2<- ggplotBarNumAthlete_NOC2+theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 4)) + labs(x="Nations", y= "# of Athletes")
ggplotBarNumAthlete_NOC2<- ggplotBarNumAthlete_NOC2+ ggtitle( "Number of Athlete Participants by Nation") 
ggplotBarNumAthlete_NOC2

#Sort from highest to lowest the NumAthlete_NOC
NumAthlete_NOCHigh<-NumAthlete_NOC[order(-NumAthlete_NOC$freq),]
View(NumAthlete_NOCHigh)
# Create list of top 10 countries with highest athlete count
NumAthlete_NOCTop50<- NumAthlete_NOCHigh [ 1: 50,]
View(NumAthlete_NOCTop50)

#Create bar chart of top 10 countries that's sorted for highest to lowest
ggplotBarNumAthlete_NOCTop50<- ggplot(NumAthlete_NOCTop50, aes(x=reorder(NOC, -freq), y= freq)) +geom_bar(stat="identity")
#Add x and y axis title and chart title
ggplotBarNumAthlete_NOCTop50<- ggplotBarNumAthlete_NOCTop50 + labs(x="Nations", y= "# of Athletes") + ggtitle( "Top 50 Nations with Most Athlete Participants") 
# Ajust x axist label by rotating 
ggplotBarNumAthlete_NOCTop50<- ggplotBarNumAthlete_NOCTop50 + theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 10))
# Add data labels to the bar charts 
ggplotBarNumAthlete_NOCTop50<- ggplotBarNumAthlete_NOCTop50 + geom_text(aes(label= freq), position = position_dodge(width=0.9), vjust =-.50, size = 2)
ggplotBarNumAthlete_NOCTop50

#----------------------------------------------------------------------------------------------------
#Create a dataframe to use for plotting that has count of athletes per Sex per Year
NumAthlete_SexYear<-data.frame(count(athelete_events_data_summer,c("Sex" ,"Year") ))
str(NumAthlete_SexYear)

#https://plot.ly/ggplot2/geom_bar/ how to plot bar with two variables in axis Females and Males vs Year
#https://stackoverflow.com/questions/42820677/ggplot-bar-plot-side-by-side-using-two-variables?rq=1
ggplotNumAthlete_SexYear <- ggplot(NumAthlete_SexYear, aes(x = Year, y = freq ,fill = Sex)) 
ggplotNumAthlete_SexYear <- ggplotNumAthlete_SexYear +geom_bar(stat="identity",position="dodge")
ggplotNumAthlete_SexYear<-ggplotNumAthlete_SexYear+ labs(x="Year", y= "Number of Athletes") + ggtitle( "Number of Participating Athletes by Gender") 
ggplotNumAthlete_SexYear
#----------------------------------------------------------------------------------------------------
#Create a dataframe to use for plotting that has count of athletes per Sex per Year
NumAthlete_MedalsNOC<-data.frame(count(athelete_events_data_summer,c("Medal" ,"NOC") ))
str(NumAthlete_MedalsNOC)
View(NumAthlete_MedalsNOC)

#remove values with NA
NumAthlete_MedalsNOC1<-na.omit(NumAthlete_MedalsNOC)

#aggregate(x$Frequency, by=list(Category=x$Category), FUN=sum)
#aggregate(Frequency ~ Category, x, sum)
NumAthlete_MedalsNOCTotalMedal<- aggregate(freq ~ NOC, NumAthlete_MedalsNOC1, sum)
NumAthlete_MedalsNOChigh<-NumAthlete_MedalsNOCTotalMedal[order(-NumAthlete_MedalsNOCTotalMedal$freq),]
NumAthlete_MedalsNOChigh50<-NumAthlete_MedalsNOChigh[1:30, ]

#Attempt to merge gold medal 
NumAthlete_MedalsNOChigh50<-NumAthlete_MedalsNOChigh[1:30, ]
NumAthlete_MedalsNOChigh50MergeGold<- merge(NumAthlete_MedalsNOChigh50, NumAthlete_MedalsNOCGoldMedal, by = "NOC", all.x = TRUE)
#Attempt to merge bronze medal 
NumAthlete_MedalsNOChigh50MergeBronze<- merge(NumAthlete_MedalsNOChigh50, NumAthlete_MedalsNOCBronzeMedal, by = "NOC", all.x = TRUE)
#Attempt to merge silver medal 
NumAthlete_MedalsNOChigh50MergeSilver<- merge(NumAthlete_MedalsNOChigh50, NumAthlete_MedalsNOCSilverMedal, by = "NOC", all.x = TRUE)
NumAthlete_MedalsNOChigh50Dummy<- data.frame(NumAthlete_MedalsNOChigh50,"Total", NumAthlete_MedalsNOChigh50$freq) 

#Check the new dataframe
str(NumAthlete_MedalsNOChigh50MergeSilver)
str(NumAthlete_MedalsNOChigh50Dummy)

#Change column names 
names(NumAthlete_MedalsNOChigh50Dummy)<- c("NOC", "freq.x","Medal", "freq.y")
#check column names 
str(NumAthlete_MedalsNOChigh50Dummy)


# Combine all medals for total, gold, bronze, silver to one dataframe. This represents top 30 nations based on total medals won. 
NumAthlete_MedalsNOChigh50Combine<- rbind(NumAthlete_MedalsNOChigh50MergeSilver,NumAthlete_MedalsNOChigh50MergeBronze, NumAthlete_MedalsNOChigh50MergeGold  )
View(NumAthlete_MedalsNOChigh50Combine) # use this for graphhing 
names(NumAthlete_MedalsNOChigh50Combine)<- c("NOC", "TotalCount","Medal Type", "MedalCount")

#create individual dataframe for each type of medal
NumAthlete_MedalsNOCGoldMedal<- NumAthlete_MedalsNOC1[NumAthlete_MedalsNOC1$Medal== "Gold",]
NumAthlete_MedalsNOCBronzeMedal<- NumAthlete_MedalsNOC1[NumAthlete_MedalsNOC1$Medal== "Bronze",]
NumAthlete_MedalsNOCSilverMedal<- NumAthlete_MedalsNOC1[NumAthlete_MedalsNOC1$Medal== "Silver",]

#sort each individual dataframe for each type of medal from highest to lowest
NumAthlete_MedalsNOCGoldMedalhigh<- NumAthlete_MedalsNOCGoldMedal[order(-NumAthlete_MedalsNOCGoldMedal$freq),]
NumAthlete_MedalsNOCBronzeMedalhigh<-NumAthlete_MedalsNOCBronzeMedal[order(-NumAthlete_MedalsNOCBronzeMedal$freq),]
NumAthlete_MedalsNOCSilverMedalhigh<-NumAthlete_MedalsNOCSilverMedal[order(-NumAthlete_MedalsNOCSilverMedal$freq),]

#create dataframe for top 30 countries for each medal type
NumAthlete_MedalsNOCGoldMedalhigh50<- NumAthlete_MedalsNOCGoldMedalhigh[1:30,]
NumAthlete_MedalsNOCBronzeMedalhigh50<-NumAthlete_MedalsNOCBronzeMedalhigh[1:30,]
NumAthlete_MedalsNOCSilverMedalhigh50<-NumAthlete_MedalsNOCSilverMedalhigh[1:30,]


#check column names 
str(NumAthlete_MedalsNOChigh50Combine)

#plot the top 30 nations based on total medal count won. Show breakout by type of medal won by color fill. 
#https://stackoverflow.com/questions/23207878/ggplot2-group-x-axis-discrete-values-into-subgroups
ggplotNumAthlete_MedalsNOChighbreakout<- ggplot(NumAthlete_MedalsNOChigh50Combine, aes(x=reorder(NOC, -MedalCount), y=MedalCount, fill= `Medal Type`))
ggplotNumAthlete_MedalsNOChighbreakout<-ggplotNumAthlete_MedalsNOChighbreakout + geom_bar(stat = "identity")
ggplotNumAthlete_MedalsNOChighbreakout<-ggplotNumAthlete_MedalsNOChighbreakout+ labs(x="Nations", y= "Medal Count") + ggtitle( "Top 30 Nations with Most Medals Won") 
ggplotNumAthlete_MedalsNOChighbreakout
                                                                                                        

#-----------------------------------------------------------------------------------------------------------------
#Create	train	and	test	data	– one	for	training	and	one	for	testing.
#Create new dataframe of data and convert medals to numerical values in order to use for modeling
athelete_events_data_summermodel<-athelete_events_data_summer
athelete_events_data_summermodel$Medal<- gsub("Gold","1",athelete_events_data_summermodel$Medal)
athelete_events_data_summermodel$Medal<- gsub("Silver","0",athelete_events_data_summermodel$Medal)
athelete_events_data_summermodel$Medal<- gsub("Bronze","0",athelete_events_data_summermodel$Medal)
athelete_events_data_summermodel$Medal[is.na(athelete_events_data_summermodel$Medal)]<-0
athelete_events_data_summer$Medal<- as.character(athelete_events_data_summer$Medal)
#check values
str(athelete_events_data_summermodel)


#check values
str(athelete_events_data_summermodel)

#find number of observations or rows
nrows<-nrow(athelete_events_data_summermodel)

#create cutpoint for training and testing data
cutPoint<-floor(nrows/3*2)
cutPoint

# create list of random indices 
rand<-sample(1:nrows)
head(rand)

# create training data and test data based on cutpoint
traindata<- athelete_events_data_summermodel[rand[1:cutPoint],]
testdata<- athelete_events_data_summermodel[rand[(cutPoint+1):nrows],]

#validate datasets
str(traindata)
str(testdata)
head(traindata)
View(traindata)

#-----------------------------------------------------------------------------
# Check the correlation to see if numerical variables have correlation
cor(athelete_events_data_summermodel[,4:6])  # correlation of .785 between Weight and Height

#-----------------------------------------------------------------------------
# Create a new variables GoldMedal that is a binary code in order to use for modeling.Gold = 1 and everything else = 0 .
athelete_events_data_summer$GoldMedal<- athelete_events_data_summer$Medal
athelete_events_data_summer$GoldMedal<- gsub("Gold","1",athelete_events_data_summer$GoldMedal)
athelete_events_data_summer$GoldMedal<- gsub("Silver","0",athelete_events_data_summer$GoldMedal)
athelete_events_data_summer$GoldMedal<- gsub("Bronze","0",athelete_events_data_summer$GoldMedal)
athelete_events_data_summer$GoldMedal[is.na(athelete_events_data_summer$GoldMedal)]<-0

#check values
str(athelete_events_data_summer)

#find number of observations or rows
nrows<-nrow(athelete_events_data_summer)

#create cutpoint for training and testing data
cutPoint<-floor(nrows/3*2)
cutPoint

# create list of random indices 
rand<-sample(1:nrows)
head(rand)

# create training data and test data based on cutpoint
traindata<- athelete_events_data_summer[rand[1:cutPoint],]
testdata<- athelete_events_data_summer[rand[(cutPoint+1):nrows],]

#validate datasets
str(traindata)
str(testdata)
head(traindata)
View(traindata)
#------------------------------------------------------------------------------------------
# Create model with naive bayes classification method
#https://www.linkedin.com/pulse/na%C3%AFve-bayes-using-r-jeffrey-strickland-ph-d-cmsp

#Create smaller subset of the traindata with necessary columns
traindata_sub<-traindata[ , c(3:6,8,13,15, 16)]
View(traindata_sub)
str(traindata_sub)

#Change GoldMedal to be a factor mode
traindata_sub$GoldMedal<-as.factor(traindata_sub$GoldMedal)
#Combine Height and Weight variables since they are correlated
traindata_sub$HeightWeight <- traindata_sub$Height * traindata_sub$Weight
#Create smaller subset of the traindata with necessary columns
traindata_sub<-traindata_sub[ , c(1:2, 5:9)]
str(traindata_sub)

#Create smaller subset of the testdata with necessary columns
testdata_sub<-testdata[ , c(3:6,8,13,15, 16)]
View(testdata_sub)
str(testdata_sub)

#Combine Height and Weight variables since they are correlated
testdata_sub$HeightWeight <- testdata_sub$Height * testdata_sub$Weight
#Change GoldMedal to be a factor mode
testdata_sub$GoldMedal<-as.factor(testdata_sub$GoldMedal)
#Create smaller subset of the traindata with necessary columns
testdata_sub<-testdata_sub[ , c(1:2, 5:9)]
str(testdata_sub)

#Form the model with the variables using the traindata. Then, test the model using the test data to see how accurate the prediction is. 
modelNB1<-naiveBayes(traindata_sub[, c(1:5,7)], traindata_sub[ ,6])
summary(modelNB1)

predict(modelNB1)
#table(predict(modelNB1, testdata_sub[, c(1:5,7)], testdata_sub[ , 6], dnn=list('predicted', 'actual')))
PredictNBGoldMedal<-predict(modelNB1,testdata_sub[, c(1:5,7)])

#create and setup dataframe including predicted results and error
PredNBGoldMedal_testdata<- data.frame(testdata_sub, PredictNBGoldMedal)
View(PredNBGoldMedal_testdata)
str(PredNBGoldMedal_testdata)

#Caluculate the error between actual and predicted value add to dataframe
ErrorPredNBGoldMedal<- PredNBGoldMedal_testdata$GoldMedal==PredNBGoldMedal_testdata$PredictNBGoldMedal
View(ErrorPredNBGoldMedal)
PredNBGoldMedal_Errordf<-data.frame(PredNBGoldMedal_testdata,ErrorPredNBGoldMedal,testdata$Year)
colnames(PredNBGoldMedal_Errordf)<- c("Sex", "Age", "NOC", "Sport", "Medal", "GoldMedal", "HeightWeight", "Year")
View(PredNBGoldMedal_Errordf)

#Calculate accuracy of prediction to actual 
totaloutcomes<- nrow(PredNBGoldMedal_Errordf)
totalcorrectNB<- count(PredNBGoldMedal_Errordf$ErrorPredNBGoldMedal==TRUE)
PredNBAccuracy<- totalcorrectNB[2,2]/totaloutcomes 
print(PredNBAccuracy) # 99.8% accuracy in predicting gold winners

#Create dataframe summing total gold medals into one from the test data
#Change both goldmedal and predicted gold medal values to numeric in order to sum
PredNBGoldMedal_Errordf$GoldMedal<- as.numeric(PredNBGoldMedal_Errordf$GoldMedal) 
PredNBGoldMedal_Errordf$PredictNBGoldMedal<- as.numeric(PredNBGoldMedal_Errordf$PredictNBGoldMedal)
str(PredNBGoldMedal_Errordf$GoldMedal)
summary(PredNBGoldMedal_Errordf$GoldMedal)


#Sum both columns and merge the results based on NOC
ActNBGoldMedalNB<- aggregate(GoldMedal ~ NOC+ Year , PredNBGoldMedal_Errordf, sum)
PredNBGoldMedalNB<- aggregate(PredictNBGoldMedal ~ NOC + Year , PredNBGoldMedal_Errordf, sum)
help("aggregate")

#create an attribute colum for actual and predicted in the respective dataframes
ActNBGoldMedalNB$Value<- "Actual"
PredNBGoldMedalNB$Value<-"Predicted"

#Change column names so they match
names(ActNBGoldMedalNB)<- c("NOC", "Year", "Gold Medals","Value")
names(PredNBGoldMedalNB)<- c("NOC", "Year", "Gold Medals","Value")

#Combine both dataframes into one dataframe in order to plot
ActvsPredNBGoldMedalNB<- rbind(ActNBGoldMedalNB,PredNBGoldMedalNB)
View(ActvsPredNBGoldMedalNB)
str(ActvsPredNBGoldMedalNB)


ActvsPredNBGoldMedalNB2016<-filter(ActvsPredNBGoldMedalNB, Year== "2016")
ActvsPredNBGoldMedalNBorder<-ActvsPredNBGoldMedalNB2016[order(-ActvsPredNBGoldMedalNB2016$`Gold Medals`),]
ActvsPredNBGoldMedalNBtop30<-ActvsPredNBGoldMedalNBorder[1:30,]


#Plot how the actual vs predicted gold medal count was for the top 30 nations based on their actual wins
ggplotNBPredGoldMedal <- ggplot(ActvsPredNBGoldMedalNBtop30, aes(x = reorder(NOC,-`Gold Medals`) , y = `Gold Medals` ,fill = Value)) 
ggplotNBPredGoldMedal <- ggplotNBPredGoldMedal +geom_bar(stat="identity",position="dodge")
ggplotNBPredGoldMedal<-ggplotNBPredGoldMedal+ labs(x="Nations", y= "Gold Medal Count") + ggtitle( "Naïve Bayes: Actual vs Predicted Gold Medal Wins- Top 30 Nations 2016") 
ggplotNBPredGoldMedal 


#--------------------------------------------------------------------------------------------------------------------
# Compute	models	and	plot	the	results	for	‘svm’	(in	the	e1071	package)
traindata_sub2<-traindata_sub[,c(1:4,6:7)]
testdata_sub2<-testdata_sub[,c(1:4,6:7)]

# Compute model for svm using same variables
modelgoldsvm1<- svm(GoldMedal~ Age + HeightWeight + Sex+ NOC + Sport, data=traindata_sub2, gamma=10, C=10)
summary(modelgoldsvm1)

#create list of predictions using model 
predgoldsvm1 <- data.frame(predict(modelgoldsvm1,testdata_sub2))
colnames(predgoldsvm1)<- c("PredSVMGoldMedal")

#validate records
View(predgoldsvm1)
str(predgoldsvm1)
str(testdata_sub2)
View(testdata_sub)

#create new dataframe with results
actvspredgoldsvm1<- data.frame(testdata_sub2, predgoldsvm1)
View(actvspredgoldsvm1)


#caluculate the error between actual and predicted value add to dataframe
ErrorPredSVMGoldMedal<- actvspredgoldsvm1$GoldMedal==actvspredgoldsvm1$PredSVMGoldMedal
View(ErrorPredSVMGoldMedal)
PredSVMGoldMedal_Errordf<-data.frame(actvspredgoldsvm1,ErrorPredSVMGoldMedal, testdata$Year)
colnames(PredSVMGoldMedal_Errordf)<- c("Sex", "Age", "NOC", "Sport", "GoldMedal",  "HeightWeight","PredSVMGoldMedal", "ErrorPredSVMGoldMedal", "Year")
View(PredSVMGoldMedal_Errordf)


#calculate accuracy of prediction to actual 
totaloutcomesSVM<- nrow(PredSVMGoldMedal_Errordf)
totalcorrectSVM<- count(PredSVMGoldMedal_Errordf$ErrorPredSVMGoldMedal==TRUE)
PredSVMAccuracy<- totalcorrectSVM[2,2]/totaloutcomesSVM
print(PredSVMAccuracy) #  0.9504745 accuracy in predicting if athlete will win gold medal


#Create dataframe summing total gold medals into one from the test data
#Change both goldmedal and predicted gold medal values to numeric in order to sum
PredSVMGoldMedal_Errordf$GoldMedal<- as.numeric(PredSVMGoldMedal_Errordf$GoldMedal) 
PredSVMGoldMedal_Errordf$PredSVMGoldMedal<- as.numeric(PredSVMGoldMedal_Errordf$PredSVMGoldMedal)


#Sum both columns and merge the results based on NOC
ActSVMGoldMedalSVM<- aggregate(GoldMedal ~ NOC+ Year , PredSVMGoldMedal_Errordf, sum)
PredSVMGoldMedalSVM<- aggregate(PredSVMGoldMedal ~ NOC + Year , PredSVMGoldMedal_Errordf, sum)


#create an attribute colum for actual and predicted in the respective dataframes
ActSVMGoldMedalSVM$Value<- "Actual"
PredSVMGoldMedalSVM$Value<-"Predicted"

#Change column names so they match
names(ActSVMGoldMedalSVM)<- c("NOC", "Year", "Gold Medals","Value")
names(PredSVMGoldMedalSVM)<- c("NOC", "Year", "Gold Medals","Value")

#Combine both dataframes into one dataframe in order to plot
ActvsPredSVMGoldMedalSVM<- rbind(ActSVMGoldMedalSVM,PredSVMGoldMedalSVM)
View(ActvsPredSVMGoldMedalSVM)
str(ActvsPredSVMGoldMedalSVM)

#Filter to only 2016 data to compare predicted winner for that event vs actual winner. Choose only the top 30 gold medal winners for that event based on actuals. 
ActvsPredSVMGoldMedalSVM2016<-filter(ActvsPredSVMGoldMedalSVM, Year== "2016")
ActvsPredSVMGoldMedalSVMorder<-ActvsPredSVMGoldMedalSVM2016[order(-ActvsPredSVMGoldMedalSVM2016$`Gold Medals`),]
ActvsPredSVMGoldMedaltop30<-ActvsPredSVMGoldMedalSVMorder[1:30,]


#Plot how the actual vs predicted gold medal count was for the top 30 nations based on their actual wins
ggplotSVMPredGoldMedal <- ggplot(ActvsPredSVMGoldMedaltop30, aes(x = reorder(NOC,-`Gold Medals`) , y = `Gold Medals` ,fill = Value)) 
ggplotSVMPredGoldMedal <- ggplotSVMPredGoldMedal +geom_bar(stat="identity",position="dodge")
ggplotSVMPredGoldMedal<-ggplotSVMPredGoldMedal+ labs(x="Nations", y= "Gold Medal Count") + ggtitle( "Support Vector: Actual vs Predicted Gold Medal Wins- Top 30 Nations 2016") 
ggplotSVMPredGoldMedal 

