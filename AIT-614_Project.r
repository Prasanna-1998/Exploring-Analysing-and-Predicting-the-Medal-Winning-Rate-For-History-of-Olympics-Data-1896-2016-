##########  Reading the dataset  ###############
data <- read.csv(file="athlete_events.csv")

##Displays the first six rows of the dataset
head(data)

#Summary of the data
library(dplyr)
summary(data)

#Structure of the data
glimpse(data)

#checking for null values
is.na(data)
colSums(is.na(data))

#imputing the missing values in Age column with mean 
data$Age.imp.mean <- ifelse(is.na(data$Age), mean(data$Age, na.rm=TRUE), data$Age)
data$Age.imp.mean

#Calculating Mode for Height
height.freq <- table(data$Height)
height.freq <- height.freq[order(height.freq, decreasing= TRUE)]
height.freq[0:1]

#calculating mode for weight
weight.freq <- table(data$Weight)
weight.freq <- weight.freq[order(weight.freq, decreasing= TRUE)]
weight.freq[0:1]

#imputing the missing values with mode
library(Hmisc)
data$Height.imp.mode=impute(data$Height, 180) 
data$Weight.imp.mode=impute(data$Weight, 70)

#imputing missing values with None string for Medal column
data$Medal.imp.none=impute(data$Medal, 'None')

#Changing categorical variables into numeric (#4-None,3-Silver,2-Gold,1-Bronze)
data6=data %>% mutate_if(is.factor, as.numeric)
data6$Age.imp.mean <- ifelse(is.na(data6$Age), mean(data6$Age, na.rm=TRUE), data6$Age)
data6$Height.imp.mode=impute(data6$Height, 180) 
data6$Weight.imp.mode=impute(data6$Weight, 70)

#imputing missing values with None string for Medal column
data6$Medal.imp.none=impute(data6$Medal, 4)   #4-None,3-Silver,2-Gold,1-Bronze
library(dplyr)
library(tidyverse)
olympic <- data.frame(data)
olympic <- mutate(olympic, allmedals= 'Nan')
head(olympic)
df <- data.frame(data$Weight.imp.mode,data$Height.imp.mode)

################### calculating the frequency for variables in the dataset  ###########################
data.frame(table(data$City))
data.frame(table(data$NOC))
data.frame(table(data$Name))
data.frame(table(data$Sport))

###############  Apriori Rules for the total dataset  #######################

install.packages("arules",dependencies = "TRUE")
#install.packages("fdm2id")
#install.packages("factoextra")
library(arules)
data("olympic")

#Performing Apriori Rules to the Dataset
rules <- apriori(olympic,parameter = list(supp = 0.5, conf = 0.9, target = "rules"))

#Summary of the Rules
summary(rules)

#showing all significant association of rules
inspect(rules) #It gives the list of all significant association rules.

###############  Principle Component Analysis  ##############################
newdata <- subset(data, Medal.imp.none=='None',
                  select=c(Height.imp.mode,Weight.imp.mode))
#Performing Principle Component Analysis on the datset
pca = prcomp(newdata)

#Showing all the dimensions of how Height Variable got classified
plot(pca$x[,1:2], col = newdata$Height.imp.mode, main = "Reducig the dimensionality of Height variable by PCA", xlab = "First Principle Component", ylab = "Second Principle Component")

#Showing the dimensions of how weight got classified
plot(pca$x[,1:2], col = newdata$Weight.imp.mode, main = "Reducig the dimensionality of Weight variable by PCA", xlab = "First Principle Component", ylab = "Second Principle Component")


################# correlation test  ###################

#Fiding correlation between height and weight variables
cor.test(data6$Height.imp.mode, data6$Weight.imp.mode, 
         method = "pearson")

#finding correlaton for Medal and Weight
cor.test(data6$Medal.imp.none, data6$Weight.imp.mode, 
         method = "pearson")

#finding correlaton for Medal and Height
cor.test(data6$Medal.imp.none, data6$Height.imp.mode, 
         method = "pearson")

#Calculating Cofidece Intervals for age, Height and Weight variables in the dataset
library(Rmisc)
exp(CI(log(data$Age.imp.mean), ci=0.95))
exp(CI(log(data$Weight.imp.mode), ci=0.95))
exp(CI(log(data$Height.imp.mode), ci=0.95))

###################  Statistical Testing  ############################
#one-sample t-test on Age Variable

#Null Hypothesis: The Average Age of player is 24
#Alternative Hypotthesis: The average age of player > 24
t.test(data$Age, mu=24, conf.level = 0.95) #P-value is less than significance so rejecting null hypothesis

#Null Hypothesis: The Average Age of a Player is 25
#Alternative Hypothesis: The Average Age of Player is <25
t.test(data$Age, mu = 25.55, conf.level = 0.95) #p-value is greater than significance, so we can accept null hypothesis

#One Sample t test code on weight Variable
#Null Hypothesis: The Avverage Weight of all the players will be 70.53
#Alternative Hypothesis:  The Avverage Weight of all the players will be less than 70.53
t.test(data$Weight.imp.mode, mu=70.53, ci=0.95) # p-value is greater than significant, so we accept null hypothesis

#One Sample t test code on Height Variable
#Null Hypothesis: The Avverage Height of all the players will be 177cm
#Alternative Hypothesis:  The Avverage Height of all the players will be less than 177cm
t.test(data$Height.imp.mode, mu=177, ci=0.95) # p-value is less than significant, so we reject null hypothesis

###############  K-Means clustering for weight and Height Distribution of players who lost the medal ###################
library(tidyverse)  # data manipulation

############ Subset the data ###########
newdata <- subset(data, Medal.imp.none=='None',
                  select=c(Height.imp.mode,Weight.imp.mode))

#installing rattle package
data(newdata, package="rattle")

#Scaling the data based on all parameters
scale(newdata[-1])

#Calculating k-mean
k2 <- kmeans(newdata, centers = 2,nstart = 25)

#Checking the attributes of clustering
attributes(k2)

#Cluster size(how the two centers are dividing the data)
k2$size
str(k2)

#clusters
k2$cluster

#checking which data falls into which cluster by using cbind
cl <- cbind(k2$cluster)
cl

#Creating contingency tables for actual clusters and variables in column
table(newdata$Height.imp.mode, k2$cluster)
table(newdata$Weight.imp.mode,k2$cluster)

#plotting the clusters using factoextra library for better visualization
library(cluster)   # clustering algorithms
clusplot(newdata, k2$cluster, main = "Height and Weight distribution of players who lost the  medal", shade=TRUE, labels = 2, linear = 0)
library(factoextra) # clustering algorithms & visualization
#####################  Actual Plot for K-means clustering method  ###################  ( we should wait for five minutes to get the output before running the next code )
fviz_cluster(object=k2, data = newdata, geom = "point",xlab = "weight of the player", ylab = "Height of the player", main ="K-Means clustering for height and weight distriution of players who lost the Medal", stand = FALSE, frame.type = "norm") + theme_bw()


###############  K-Means clustering for weight and Height Distribution of players who won the medal ###################

newdata1 <- subset(data, Medal.imp.none=='Gold' | Medal.imp.none=='Bronze' | Medal.imp.none=='Silver',
                   select=c(Height.imp.mode,Weight.imp.mode))
#installing rattle package
data(newdata1, package="rattle")

#Scaling the data based on all parameters
scale(newdata1[-1])

#Calculating k-mean
k2 <- kmeans(newdata1, centers = 2)

#Checking the attributes of clustering
attributes(k2)

#Cluster size(how the five centers are dividing the data)
k2$size
str(k2)

#clusters
k2$cluster

#checking which data falls into which cluster by using cbind
cl <- cbind(k2$cluster)
cl

#Creating contingency tables for actual clusters and variables in column
table(newdata1$Height.imp.mode, k2$cluster)
table(newdata1$Height.imp.mode,k2$cluster)

#plotting the clusters using factoextra library for better visualization
library(cluster)   # clustering algorithms
clusplot(newdata1, k2$cluster, main = "2D representation of cluster", shade=TRUE, labels = 2, linear = 0)
library(factoextra) # clustering algorithms & visualization
#############################################  Actual plot for k-means (we should wait for five minutes to get the output )
fviz_cluster(object=k2, data = newdata1, geom = "point",xlab = "weight of the player", ylab = "Height of the player",
             main ="K-Means clustering for height and weight distriution of players who won the Medal", stand = FALSE, frame.type = "norm") + theme_bw()

##############  Heirarchical clustering for gold medal players ######################
newdata2 <- subset(data6, Medal.imp.none==2,
                   select=c(Age.imp.mean))

#Displaying First 100 observations groups
df4<-data.frame(newdata2[1:100,])

#Scaling the data
scale(df4[-1])
df4=na.omit(df4)

#Dissimilarity matrix
d <- dist(df4, method = "euclidean")

# Hierarchical clustering using Complete Linkage
hc1 <- hclust(d, method = "complete" )

# Plot the obtained dendrogram
plot(hc1, xlab="datapoints  which are clustered", ylab="Height of the tree" ,
     main = "Heirarchical clustering of players who got Gold Medals",cex = 0.6, hang = -1)

#Rectangular Boxes to seperate the Clusters
rect.hclust(hc1, k = 5, border = 3:5)

################### Heirarchical clustering for Silver medal players  ################
newdata3 <- subset(data6, Medal.imp.none==3,
                   select=c(Age.imp.mean))
#Displaying the First Hundred Observations
df5<-data.frame(newdata3[1:100,])

#Scaling the data
scale(df5[-1])
df5=na.omit(df5)

#Dissimilarity matrix
d1 <- dist(df5, method = "euclidean")

# Hierarchical clustering using Complete Linkage
hc2 <- hclust(d1, method = "complete" )

# Plot the obtained dendrogram
plot(hc2, xlab="datapoints  which are clustered", ylab="Height of the tree" ,
     main = "Heirarchical clustering of players who got Silver Medals",cex = 0.6, hang = -1)

#Rectangular boxes for seperating the clusters of every group
rect.hclust(hc2, k = 5, border = 3:5)

#####################  Heirarchical clustering for bronze medal players ###############
newdata4 <- subset(data6, Medal.imp.none==1,
                   select=c(Age.imp.mean))
df6<-data.frame(newdata4[1:100,])
scale(df6[-1])
df6=na.omit(df6)

#Dissimilarity matrix
d2 <- dist(df6, method = "euclidean")

# Hierarchical clustering using Complete Linkage
hc3 <- hclust(d2, method = "complete" )

# Plot the obtained dendrogram
plot(hc3, xlab="datapoints  which are clustered", ylab="Height of the tree" ,
main = "Heirarchical clustering of players who got Bronze Medals", cex = 0.6, hang = -1)

#Displaying the group of clusters in rectangular boxes
rect.hclust(hc1, k = 5, border = 3:5)


################ Preprocessing for Regression Techniques ##############################
################################ splitting the dataset into train test and validation ################
####################  Fitting Linear Regression #####################

random = read.csv("athlete_events.csv")
head(random)

###### subset of data with selected variables in the dataset ################
random_imp = subset(random, select = -c(ID, Name,Event,Team,Games,Year) )
head(random_imp)

#imputing the medal values with none in place of null values
library(Hmisc)
random_imp$Medal=impute(random$Medal, 'Lost')
head(random_imp)

#imputing missing values and changing into numeric
random_imp$Sex=as.numeric(random_imp$Sex)
random_imp$Season=as.numeric(random_imp$Season)
random_imp$Age=impute(random_imp$Age, 25)
random_imp$Height=impute(random_imp$Height,180)
random_imp$Weight=impute(random_imp$Weight,70)
random_imp$City=as.numeric(random_imp$City)
random_imp$Sport=as.numeric(random_imp$Sport)
random_imp$NOC=as.numeric(random_imp$NOC)
random_imp$Medal=as.numeric(random_imp$Medal)

##Splittig
ind <- sample(2, nrow(random_imp),replace = TRUE, prob = c(0.7,0.3))
trainapp <- random_imp[ind==1,]
head(trainapp)
testapp <- random_imp[ind==2,]
head(testapp)
str(random_imp)

#fitting the model
linear=lm(Medal~.,data=trainapp)

#summary
summary(linear)

#predicting the winning medal rate
li=predict(linear,testapp)
li

#confusion matrix
table(li, testapp$Medal)

##Residual Standard Error :0.768
##Adjusted R2 :0.01551
##Multiple R2 value : 0.01555

####################### Splitting for logistic regression model  #########
require(caTools) 

# splits the data in the ratio mentioned in SplitRatio. After splitting marks these rows as logical TRUE and the the remaining are marked as logical FALSE
sample = sample.split(data,SplitRatio = 0.65) 

# creates a training dataset named train1 with rows which are marked as TRUE
train1 =subset(data,sample ==TRUE) 

# creates a test dataset named test1 with rows which are marked as TRUE
test1=subset(data, sample==FALSE)

#creating the validation set from training set for frequent evaluation
smp_size <- floor(0.7 * nrow(train1))
train_ind <- sample(seq_len(nrow(train1)), size = smp_size)
validation <- train1[-train_ind, ]


##################### Fitting Logistic Regression Model ################
#Fitting the model
logistic <- glm(Medal.imp.none ~ Height.imp.mode + Weight.imp.mode + Age.imp.mean, data = train1, family = "binomial")

#description of the logistic regression model for the data
summary(logistic)

#Calculating Concordance
confint(logistic, level=.80)

#Checking performance on the validation set
val <-predict(logistic, validation, type="response") 

#combining the validation set and predicted val variables into mydf
mydf <- cbind(validation,val)
mydf
mydf$response <- as.factor(ifelse(mydf$val>0.5, 1, 0))

#Predicting the Winning rate of medal for each player in every event using logistic regression technique.
#Scoring the test data using the logistic regression model we created
predicting<-predict(logistic, test1, type="response") 
cbind(test1,predicting)

##### Printing the predicted values ######
predicting

#confusion matrix
table(predicting,test1$Medal.imp.none)

##Multicolliearity for three independent variables
car::vif(logistic)

# analysis of variance table of the fitted model for [model evaluation (anova)]
anova(logistic, test1 = "Chisq")

#### Assessing coefficients ############
install.packages("broom.mixed")
library(broom.mixed)

#coefficiet estmates
tidy(logistic)

#interpreting the medal coefficients
exp(coef(logistic))
confint(logistic)

############# While no exact equivalent to the R2 of linear regression exists, 
#the McFadden R2 index can be used to assess the model fit.
install.packages("pscl")
library(pscl)
pR2(logistic)

###Accuracy by finding mean of predicted values
mean(predicting)

#######################################################  Exploratory Analysis using SQLDF package #####################################
#############################SQLDF#######################
install.packages("sqldf")
library(sqldf)
sqldf("select Name,count(*) as coutnames from data group  by Name having count(*)>30")
sqldf("select Medal,count(*) as no_of_medals from data  group by Medal")
sqldf("select Name,Team,Season,Medal from data")

######################  Key Insights  ###################################

##1.	Which season has a greater number of athletes participating? Winter or summer
count_summerathletes <- sqldf("select COUNT(d.name) from data d where d.Season='Summer'")
count_winterathletes <- sqldf("select COUNT(d.name) from data d where d.Season='Winter'")
print(count_summerathletes)
#1    222552
print(count_winterathletes)
#1    48564

#2.	Which years had the maximum participation of athletes
max_summerpart<-sqldf("SELECT season,year,name, MAX(name_count)
FROM (SELECT season,year, name, COUNT(name)  AS name_count
FROM data 
GROUP BY year, name
ORDER BY year, name_count DESC) where season ='Summer'
GROUP BY year ORDER BY name_count DESC")
max_summerpart

max_winterpart<-sqldf("SELECT season,year,name, MAX(name_count)
FROM (SELECT season,year, name, COUNT(name)  AS name_count
FROM data 
GROUP BY year, name
ORDER BY year, name_count DESC) where season ='Winter'
GROUP BY year ORDER BY name_count DESC")
max_winterpart

#3.	What is average age of medal winners in Summer and Winter
Avgmedalwinner_age <- sqldf("select d.season,avg(d.age) as Avg_age ,COUNT(d.Medal) 
                            from data d GROUP BY d.season HAVING COUNT(d.Medal)>0")
print(Avgmedalwinner_age)
#Season  Avg_age COUNT(d.Medal)
#1 Summer 25.67405          34088
#2 Winter 25.03915           5695

#4.	What is average age of non-medal winners in Summer and Winter
Avgnonmedalwinner_age <- sqldf("select d.season,avg(d.age) as Avg_age ,COUNT(d.Medal) 
                               from data d GROUP BY d.season HAVING COUNT(d.Medal)<1")
print(Avgnonmedalwinner_age)

#5.	Compare the male vs female ratio in both Summer and Winter
malecount <- sqldf("select sex,COUNT(sex),season from data group by season,sex HAVING sex='M'")
print(malecount)
#Sex COUNT(sex) Season
#1   M     163109 Summer
#2   M      33485 Winter
femalecount <- sqldf("select sex,COUNT(sex),season from data group by season,sex HAVING sex='F'")
print(femalecount)
#Sex COUNT(sex) Season
#1   F      59443 Summer
#2   F      15079 Winter

#6.	Compare the growth of female ratio from start to recent games
female_growth <- sqldf("select sex,COUNT(sex),year from data group by year,sex HAVING sex='F' order by year asc")
print(female_growth)


