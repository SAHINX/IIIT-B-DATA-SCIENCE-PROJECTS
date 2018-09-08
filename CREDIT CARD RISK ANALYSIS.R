#Credit card risk application scoring and risk analysis

#----------------------------------------------------------#
# Process followed in this code:
# 1. Data Understanding & Preparation  
# 2. Data Preparation
# 3. Modelling
# 4. Model Evaluation
# 5. Scorecard generation
#----------------------------------------------------------#
library(ggplot2)
library(dplyr)
library(Information)
#install.packages("bit64")
#library(bit64)
#install.packages("devtools")
library(devtools)
#install_github("tomasgreif/woe")
library(woe)
#install_github("prk327/AtConP")
library(AtConP)
library(scorecard)

#-------------------------------------------------------#
### Data Understanding and Preparation ####
#-------------------------------------------------------#


dd<-read.csv("Demographic data.csv", na.strings=c("","NA"," "))
cb<-read.csv("Credit Bureau data.csv", na.strings=c("","NA"," "))

#demographic
dim(dd)#its a df with 12 variables for 71295 observations
str(dd)#many variables need to be converted to factor.
dd<-dd[-which(duplicated(dd$Application.ID)),]#removing duplicate rows
dim(dd)
length(unique(dd$Application.ID))#all records arefor unique applicants - 71292


#credit bureau
dim(cb)#its a df with 19 variables for 71295 observations
str(cb)#many variables need to be converted to factor.Some variables are redundant
cb<-cb[-which(duplicated(cb$Application.ID)),]#removing duplicate rows
dim(cb)
length(unique(cb$Application.ID))#all records are for unique applicants - 71292

setdiff(unique(dd$Application.ID),unique(cb$Application.ID))
#since there is no difference,both tables are for same applicants


#adding dd and cb and creating merged data
mm=merge(dd,cb,by="Application.ID",all=T)
dim(mm)
sum(duplicated(mm$Application.ID))

#renaming Performance.Tag.x to Performance.Tag
names(mm)[12]<-"Performance.Tag"

#removing performance.Tag.y
mm$Performance.Tag.y<-NULL
#remove Application ID
mm$Application.ID <- NULL

#Since NA in performance.Tag represents those customers who weren't issued 
#credit cards, it would be best to keep them as a separate dataset
#saving rejects to another df
Rejects<-mm%>%filter(is.na(Performance.Tag))

#removing rejects from mmerged df
mm<-mm%>%filter(!is.na(Performance.Tag))


#converting appropriate cols to factor as per data requirement
cols<-c("No.of.times.90.DPD.or.worse.in.last.6.months","No.of.times.60.DPD.or.worse.in.last.6.months","No.of.times.30.DPD.or.worse.in.last.6.months","No.of.times.90.DPD.or.worse.in.last.12.months","No.of.times.60.DPD.or.worse.in.last.12.months","No.of.times.30.DPD.or.worse.in.last.12.months","Presence.of.open.home.loan","Presence.of.open.auto.loan")
#cols<-c("Presence.of.open.home.loan","Presence.of.open.auto.loan")

mm[cols]<-lapply(mm[cols],factor)

# Checking default rate of customer
table(mm$Performance.Tag)

prop.table(table(mm$Performance.Tag))

#So percentage of deaulters is 4.22% while that of non-defaulters is 95.78%

#checking which columns have NAs/missing values in the dataset
which(sapply(mm,function(x)sum(is.na(x)))>0)#5variableshavemissingvalues
print("missing values are imputed with WOE values in next steps")

###############-----EDA--------------################################
###---------------Demographic-----#####

#-------------Univariate and Bivariate Analysis-----------------------------------
# Plotting Age histogram
ggplot(mm,aes(Age))+geom_histogram()
summary(as.factor(mm$Age))

#Here -3 might be default value while 0 might be unknown value

#Plotting boxplot for Age
boxplot(mm$Age)

# Let's check the outlier in the variables 
quantile(mm$Age,seq(0,1,0.01))

# Flooring the lower values of age with 27.
mm[(which(mm$Age < 27)),]$Age <- 27

#Plotting boxplot for Age
boxplot(mm$Age)

# Binning the age variable and store it into "binning.age".
mm$binning.age <- as.factor(cut(mm$Age, breaks = c(25, 30, 35, 40, 45, 50, 55, 60, 65, 70)))

# Check the numeric value of default rate in each bucket
agg_age <- merge(aggregate(Performance.Tag ~ binning.age, mm, mean),aggregate(Performance.Tag ~ binning.age, mm, sum),by = "binning.age")

# Adding No.of_prospect
count <- data.frame(table(mm$binning.age))
count <- count[-nrow(count),-1]
agg_age <- cbind(agg_age,count)

# changing column name of each variables in agg_age dataframe
colnames(agg_age) <- c("age", "default_rate", "count_prospects","No.of_prospect")

# Round Off the values
agg_age$default_rate <- format(round(agg_age$default_rate, 2))

agg_age

# Let's see the default rate of each age bucket in the plot

ggplot(agg_age, aes(age, No.of_prospect,label = default_rate, fill = age)) + 
  geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  geom_text(size = 3, vjust = -0.5)

# The default rate in each of the buckets is almost equal but the number of
# prospects are more in the 35-55 bracket

# Plotting bar graph for job variable.

# Writing a function "plot_default" to do the same task for each variable

plot_default <- function(cat_var, var_name, dataset){
  attach(dataset)
  a <- aggregate(Performance.Tag ~ cat_var, data = dataset, mean)
  count <- data.frame(with(dataset,table(cat_var)))
  count <- count[,-1]
  agg_default <- cbind(a, count)
  
  colnames(agg_default) <- c(var_name, "default_rate","No.of_Prospect")
  agg_default[, 2] <- format(round(agg_default[, 2], 2))
  
  ggplot(agg_default, aes(agg_default[, 1], count, label = default_rate, fill = agg_default[, 1])) + 
    geom_bar(stat = 'identity') + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
    geom_text(size = 3, vjust = -0.5) + xlab(var_name) +
    scale_fill_discrete(name =  var_name)
  #detach(dataset)
}

#------Gender------------------
#Plotting by Gender
summary(mm$Gender)

#Renaming blank level with F to get more balanced dataset
mm$Gender[which(is.na(mm$Gender))] <-  "F"
summary(mm$Gender)

plot_default(Gender, "Gender", mm)

#Since default rate is also same across both Genders, there is no difference
#between the 2 genders

#-----------Marital Status------------------------------------
#Checking Marital status at time of application
summary(mm$Marital.Status..at.the.time.of.application.)
# There are 6 NAs in the marital status whom we will merge with Single to 
# get more balanced dataset
mm$Marital.Status..at.the.time.of.application.[which(is.na(mm$Marital.Status..at.the.time.of.application.))] <- "Single"

summary(mm$Marital.Status..at.the.time.of.application.)

plot_default(Marital.Status..at.the.time.of.application., "Marital_Status", mm)

#Again there is no difference between the default rates of married and single

#-------------- Number of dependents-------------
summary(mm$No.of.dependents)
summary(as.factor(mm$No.of.dependents))

#There are only 3 NAs which we will merge with no of dependents = 5 to
#get a more balaned dataset
mm$No.of.dependents[which(is.na(mm$No.of.dependents))] <- "5"

summary(as.factor(mm$No.of.dependents))

plot_default(No.of.dependents, "No_of_dependents", mm)
#Default rate is the same across number of dependents

#---------------Income------------------------
#Plotting income histogram
ggplot(mm,aes(Income))+geom_histogram()
summary(as.factor(mm$Income))

#Income -0.5 is clearly incorrect. It might be default value stored by the
#system

#Plotting boxplot for Income
boxplot(mm$Income)

# Let's check the outlier in the variables 
quantile(mm$Income,seq(0,1,0.01))

# Flooring the lower values of Income with 0.
mm[(which(mm$Income < 0)),]$Income <- 0

boxplot(mm$Income)

# Binning the age variable and store it into "income_bin".
mm$income.bin <- as.factor(cut(mm$Income, breaks = c(0, 10, 20, 30, 40, 50, 60)))

# Check the numeric value of default rate in each bucket
#agg_income <- merge(aggregate(Performance.Tag ~ income_bin, mm, mean),aggregate(Performance.Tag ~ income_bin, mm, sum),by = "income_bin")

plot_default(income.bin, "income.bin", mm)

#Prospects in the 0,10 income bracket have twice the default rate of prospects
#in the (50,60) income bracket which would make sense
#Hence income_bracket can be an important predictor of default

#---------Education-------------------------------------
summary(mm$Education)

#Since there are 118 NAs it will be better to combine it with others
mm$Education[which(is.na(mm$Education))] <- "Others"

summary(mm$Education)

plot_default(Education, "Education", mm)
#AS can be seen from the plots, there is not much difference in the default rates
# across education levels. Hence education might not be good predictor of default

#---------------------Profession------------------------------------
summary(mm$Profession)

#since there are 13 NAs, it is better to combine them with SE,
# to give more balanced distribution across profession levels
mm$Profession[which(is.na(mm$Profession))] <- "SE"

summary(mm$Profession)

plot_default(Profession, "Profession", mm)
#Default in SE level is slightly more than other 2 levels, hence Profession
#might be weak indicator of default.

#------------------Type.of.residence--------------------------------
summary(mm$Type.of.residence)

#since there are 8 NAs in Residence, it is better to combine them with SE,
# to give more balanced distribution across types of residence levels

mm$Type.of.residence[which(is.na(mm$Type.of.residence))] <- "Others"

summary(mm$Type.of.residence)

plot_default(Type.of.residence, "Type.of.residence", mm)
#Since the rate of default in Company provided is more than twice of that
# in Others, type of residence might be an important predictor of default

#------No.of.months.in.current.residence------------------------------
#Plotting No.of.months.in.current.residence
ggplot(mm,aes(No.of.months.in.current.residence))+geom_histogram()
summary(as.factor(mm$No.of.months.in.current.residence))
summary(mm$No.of.months.in.current.residence)

#Plotting boxplot for No.of.months.in.current.residence
boxplot(mm$No.of.months.in.current.residence)

# Let's check the outlier in the variables 
quantile(mm$No.of.months.in.current.residence,seq(0,1,0.01))

# Distribution is right skewed but no outliers as such

# Binning the No.of.months.in.current.residence variable and store it into "residence.bin".
mm$residence.bin <- as.factor(cut(mm$No.of.months.in.current.residence, breaks = c(0, 20, 40, 60, 80, 100, 120, 140)))

# Check the numeric value of default rate in each bucket
#agg_income <- merge(aggregate(Performance.Tag ~ income_bin, mm, mean),aggregate(Performance.Tag ~ income_bin, mm, sum),by = "income_bin")

plot_default(residence.bin, "residence.bin", mm)
#Default rate is significantly higher in 20- 40 months bracket than in other
#bins . Hence number of months in current residence might be an important predictor
#in the default rate

#----------No.of.months.in.current.company -----------------------------
#Plotting No.of.months.in.current.company
ggplot(mm,aes(No.of.months.in.current.company))+geom_histogram()
summary(as.factor(mm$No.of.months.in.current.company))
summary(mm$No.of.months.in.current.company)

#Plotting boxplot for No.of.months.in.current.company
boxplot(mm$No.of.months.in.current.company)

# Let's check the outlier in the variables 
quantile(mm$No.of.months.in.current.company,seq(0,1,0.01))

# Distribution show a big jump from 99% to 100%. Hence, capping the 
# No.of.months.in.current.company at 99% value of 74
mm[(which(mm$No.of.months.in.current.company > 74)),]$No.of.months.in.current.company <- 74

boxplot(mm$No.of.months.in.current.company)

# Binning the No.of.months.in.current.company variable and store it into "company.bin".
mm$company.bin <- as.factor(cut(mm$No.of.months.in.current.company, breaks = c(0, 20, 40, 60, 80)))

plot_default(company.bin, "company.bin", mm)
#Default rate in the 0-20 months bin is significantly higher than in the 40-60 months
#bin, hence Number of months in current company might be an important predictor
#default rate




##########--------CredbureauEDA---------------##############


#------No.of.times.60.DPD.or.worse.in.last.6.months---------------------------
mm$No.of.times.60.DPD.or.worse.in.last.6.months <- as.factor(mm$No.of.times.60.DPD.or.worse.in.last.6.months)
summary(as.factor(mm$No.of.times.60.DPD.or.worse.in.last.6.months))
plot_default(No.of.times.60.DPD.or.worse.in.last.6.months,"60dpd6months", mm)

#Since the default rates more than triples from 0 to 3 or more 60.DPD.or.worse.in.last.6.months
# No.of.times.60.DPD.or.worse.in.last.6.months can be a significant predictor of
# default.

#-------------------No.of.times.90.DPD.or.worse.in.last.6.months---------------
summary(as.factor(mm$No.of.times.90.DPD.or.worse.in.last.6.months))
mm$No.of.times.90.DPD.or.worse.in.last.6.months <- as.factor(mm$No.of.times.90.DPD.or.worse.in.last.6.months)
plot_default(No.of.times.90.DPD.or.worse.in.last.6.months,"90dpd6months", mm)

#Since the default rates more than triples from 0 to 3 90.DPD.or.worse.in.last.6.months
# No.of.times.90.DPD.or.worse.in.last.6.months can be a significant predictor of
# default.

#-------------------Presence.of.open.home.loan---------------
summary(as.factor(mm$Presence.of.open.home.loan))
mm$Presence.of.open.home.loan <- as.factor(mm$Presence.of.open.home.loan)

#since there are  NAs in Presence.of.open.home.loan, it is better to combine them with 1,
# to give more balanced distribution across the levels of Open Home loans

mm$Presence.of.open.home.loan[which(is.na(mm$Presence.of.open.home.loan))] <- "1"

summary(mm$Presence.of.open.home.loan)
plot_default(Presence.of.open.home.loan,"Open Home loans", mm)

#Since the Default rates fall to nearly half from 0 to 1, the Presence of Open
#home loans can be a signficant predictor of default.

#-------------------Presence.of.open.auto.loan---------------
summary(as.factor(mm$Presence.of.open.auto.loan))
mm$Presence.of.open.auto.loan <- as.factor(mm$Presence.of.open.auto.loan)

plot_default(Presence.of.open.auto.loan,"Open Auto loans", mm)


#Since the default rates are same across both levels of open Auto loans
#Presence.of.open.auto.loan may not be a significant predictor of default

#-----------------Avgas.CC.Utilization.in.last.12.months-------------------
summary(mm$Avgas.CC.Utilization.in.last.12.months)
ggplot(mm,aes(Avgas.CC.Utilization.in.last.12.months))+geom_histogram()

#Average Cc utilization is a bimodal right skewed distribution

# Binning the Average CC utilization variable and store it into "Avg_util.bin".
mm$Avg_util.bin <- as.factor(cut(mm$Avgas.CC.Utilization.in.last.12.months, breaks = c(0, 20, 40, 60, 80, 100, 120)))

#Checking the default rates across Average utilization bins
plot_default(Avg_util.bin, "Avg_util.bin", mm)

#Since default rates increase significantly for more than 20 Average CC utilizations,
#Avgas.CC.Utilization.in.last.12.months can be a significant predictor of default 

#-----------------No.of.trades.opened.in.last.12.months-------------------
summary(mm$No.of.trades.opened.in.last.12.months)
summary(as.factor(mm$No.of.trades.opened.in.last.12.months))

ggplot(mm,aes(No.of.trades.opened.in.last.12.months))+geom_histogram()

#No.of.trades.opened.in.last.12.months is a right skewed distribution

# Binning the No.of.trades.opened.in.last.12.months variable and store it into "Trades_last_12.bin".
mm$Trades_last_12.bin <- as.factor(cut(mm$No.of.trades.opened.in.last.12.months, breaks = c(0, 5, 10, 15, 20, 25, 30)))

#Checking the default rates across Trades_last_12 bins
plot_default(Trades_last_12.bin, "Trades opened in last 12 months", mm)

#Since Default rates rise significantly for more than 5 trades opened in the last 12 months ,
#No.of.trades.opened.in.last.12.months can be a significant predictor of default

#---------------------No.of.PL.trades.opened.in.last.12.months-----------------
summary(mm$No.of.PL.trades.opened.in.last.12.months)

summary(as.factor(mm$No.of.PL.trades.opened.in.last.12.months))

ggplot(mm,aes(No.of.PL.trades.opened.in.last.12.months))+geom_histogram()

#No.of.PL.trades.opened.in.last.12.months is a right skewed distribution

# Binning the No.of.PL.trades.opened.in.last.12.months variable and store it into "PL_Trades_last_12.bin".
mm$PL_Trades_last_12.bin <- as.factor(cut(mm$No.of.PL.trades.opened.in.last.12.months, breaks = c(0, 3, 6, 9, 12)))

#Checking the default rates across Trades_last_12 bins
plot_default(PL_Trades_last_12.bin, "PL Trades opened in last 12 months", mm)

#Since there is significant variation in default rates from 4% to 6% across bins,
#No.of.PL.trades.opened.in.last.12.months might be a significant predictor of default.

#---------------------No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.-----------------
summary(mm$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.)

summary(as.factor(mm$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.))

ggplot(mm,aes(No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.))+geom_histogram()

#No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. is a right skewed distribution

# Binning the No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. variable and store it into "Inquiries_except_home_Auto_last_12.bin".
mm$Inquiries_except_home_Auto_last_12.bin <- as.factor(cut(mm$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans., breaks = c(0, 4, 8, 12, 16, 20)))

#Checking the default rates across Inquiries_except_home_Auto_last_12 bins
plot_default(Inquiries_except_home_Auto_last_12.bin, "Inquiries except Home Auto loans in last 12 months", mm)

#Since there is significant variation in default rates from 4% to 7% across bins,
#No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. might be a significant predictor of default.

#---------------------No.of.times.30.DPD.or.worse.in.last.6.months-----------------
summary(mm$No.of.times.30.DPD.or.worse.in.last.6.months)

summary(as.factor(mm$No.of.times.30.DPD.or.worse.in.last.6.months))

ggplot(mm,aes(No.of.times.30.DPD.or.worse.in.last.6.months))+geom_histogram(stat="count")

#No.of.times.30.DPD.or.worse.in.last.6.months is a right skewed distribution

#Converting the No.of.times.30.DPD.or.worse.in.last.6.months variable into factor.
mm$No.of.times.30.DPD.or.worse.in.last.6.months <- as.factor(mm$No.of.times.30.DPD.or.worse.in.last.6.months)

#Checking the default rates across No.of.times.30.DPD.or.worse.in.last.6.months
plot_default(mm$No.of.times.30.DPD.or.worse.in.last.6.months, "No of times 30 DPD or worse in last 6 months", mm)

#Since there is significant variation in default rates from 3% to 11% across levels,
#No.of.times.30.DPD.or.worse.in.last.6.months might be a significant predictor of default.


#---------------------Total.No.of.Trades-----------------
summary(mm$Total.No.of.Trades)

summary(as.factor(mm$Total.No.of.Trades))

ggplot(mm,aes(Total.No.of.Trades))+geom_histogram()

#Total.No.of.Trades is a right skewed distribution

# Binning the Total.No.of.Trades variable and store it into "Total_Trades.bin".
mm$Total_Trades.bin <- as.factor(cut(mm$Total.No.of.Trades, breaks = c(0, 10, 20, 30, 40, 50)))

#Checking the default rates across Total_Trades bins
plot_default(Total_Trades.bin, "Total_Trades", mm)


#Since there is significant variation in default rates from 0% to 6% across bins,
#Total.No.of.Trades might be a significant predictor of default.


#Removing all bin variables
bin_variables <- c("binning.age", "income.bin", "residence.bin", "company.bin",
                   "Avg_util.bin", "Trades_last_12.bin", "PL_Trades_last_12.bin",
                   "Inquiries_except_home_Auto_last_12.bin", "Total_Trades.bin")

mm <- mm[,!(colnames(mm) %in% bin_variables)]


#-----------------IVandWOE-----------------------------
infoTables<-create_infotables(mm,y="Performance.Tag",bins=10,parallel=T)
scorebin<-woebin(mm,"Performance.Tag", positive = "0")

#Information value graph
plotFrame<-infoTables$Summary[order(-infoTables$Summary$IV),]
plotFrame$Variable<-factor(plotFrame$Variable,
levels=plotFrame$Variable[order(-plotFrame$IV)])

ggplot(plotFrame,aes(x=Variable,y=IV))+
geom_bar(width=.35,stat="identity",color="darkblue",fill="white")+
ggtitle("InformationValue")+
theme_bw()+
theme(plot.title=element_text(size=10))+
theme(axis.text.x=element_text(angle=90))

#Replacing data with WOE values
str(mm)
woe_data<-woebin_ply(mm,scorebin)
names(woe_data)<-sapply(names(woe_data), function (x) gsub("_woe","",x=x))


#--------------------------Model building-------------------------

library(caret)
library(caTools)
library(dummies)
library(car)
library(MASS)

#considering variables with IV>0.1 as it indicates medium to strong predictive power
collist<-infoTables$Summary$Variable[infoTables$Summary$IV>0.1]

#Filtering the Dataset with variables with IV>0.1
woe_data_medstrongiv<-dplyr::select(woe_data,collist)
woe_data_medstrongiv$Performance.Tag <- as.factor(woe_data$Performance.Tag)


#Splitting data into train and test for model building.

set.seed(123)

indices = sample.split(woe_data_medstrongiv$Performance.Tag, SplitRatio = 0.7)

train = woe_data_medstrongiv[indices,]
test = woe_data_medstrongiv[!(indices),]

summary(train$Performance.Tag) 
summary(test$Performance.Tag)

#we have imbalanced traning dataset causing low accuracy

#install.packages("ROSE")
library(ROSE)
library(DMwR)

#uncomment below line to use ROSE
bal_train<-ROSE(Performance.Tag ~ ., data = train, seed=12)$data
table(bal_train$Performance.Tag)

#Logistic Model Building

model_lm <- glm (Performance.Tag ~ ., family = "binomial", data = bal_train  ) 

summary(model_lm)
sort(vif(model_lm))

# Using stepwise algorithm for removing insignificant variables 

model_1 <- stepAIC(model_lm, direction = "both")

# stepAIC has removed some variables and only the following ones remain

model_2 <- glm(Performance.Tag ~ Avgas.CC.Utilization.in.last.12.months + No.of.trades.opened.in.last.12.months + 
                 No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                 Outstanding.Balance + No.of.times.30.DPD.or.worse.in.last.6.months + 
                 Total.No.of.Trades + No.of.PL.trades.opened.in.last.6.months + 
                 No.of.times.30.DPD.or.worse.in.last.12.months + No.of.times.90.DPD.or.worse.in.last.12.months + 
                 No.of.times.60.DPD.or.worse.in.last.6.months + No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. + 
                 No.of.times.90.DPD.or.worse.in.last.6.months, family = "binomial", data=bal_train)

summary(model_2)
sort(vif(model_2))

#Removing variable based on VIF and p balue

model_3 <- glm(Performance.Tag ~ Avgas.CC.Utilization.in.last.12.months + No.of.trades.opened.in.last.12.months + 
                 No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                 Outstanding.Balance + No.of.times.30.DPD.or.worse.in.last.6.months + 
                 Total.No.of.Trades + No.of.PL.trades.opened.in.last.6.months + 
                 No.of.times.30.DPD.or.worse.in.last.12.months + No.of.times.90.DPD.or.worse.in.last.12.months + 
                 No.of.times.60.DPD.or.worse.in.last.6.months + No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.
                  , family = "binomial", data=bal_train)

summary(model_3)
sort(vif(model_3))

# removing variable based on p value and VIF

model_4 <- glm(Performance.Tag ~ Avgas.CC.Utilization.in.last.12.months + No.of.trades.opened.in.last.12.months + 
                 No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                 Outstanding.Balance + No.of.times.30.DPD.or.worse.in.last.6.months + 
                 No.of.PL.trades.opened.in.last.6.months + 
                 No.of.times.30.DPD.or.worse.in.last.12.months + No.of.times.90.DPD.or.worse.in.last.12.months + 
                 No.of.times.60.DPD.or.worse.in.last.6.months + No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.
                 , family = "binomial", data=bal_train)

summary(model_4)
sort(vif(model_4))


final_model<- model_3


#---------------------------------------------------------#    

# Predicting probabilities of responding for the test data
str(test)
pred_logit <- predict(final_model, newdata = test[, -16], type = "response")
summary(pred_logit)
#--------------------------------------------------------- 


## Model Evaluation: Logistic Regression

#---------------------------------------------------------    

# Let's find out the optimal probalility cutoff 

perform_fn <- function(cutoff) 
{
  pred_default <- factor(ifelse(pred_logit >= cutoff, 1, 0))
  conf <- confusionMatrix(pred_default, test$Performance.Tag)
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}


# Creating cutoff values from 0.01 to 0.99 for plotting and initiallizing a matrix of 1000 X 4.

s = seq(.01,.99,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

# plotting cutoffs 
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.05)]

cutoff

# Let's choose a cutoff value of 0.5247475 for final model

pred_default <- factor(ifelse(pred_logit >= 0.5247475, 1, 0))

conf_final <- confusionMatrix(pred_default, test$Performance.Tag)

conf_final

roc.curve(test$Performance.Tag,pred_default)
#Area under curve 0.620

#------------ Build the random forest------------------
library(randomForest)
data.rf <- randomForest(Performance.Tag ~ ., data=bal_train, proximity=FALSE,
                        ntree=2, mtry=10, do.trace=TRUE, na.action=na.omit)
data.rf
testPred <- predict(data.rf, newdata=test)
table(testPred, test$Performance.Tag)
conf<-confusionMatrix(testPred,test$Performance.Tag)
conf
roc.curve(testPred,test$Performance.Tag)
#Area under curve - 0.516

#Logistic regression model is giving a better Area under curve in ROC curve than Random forest

#Using results from Logistic model to generate application scorecard

#----------Application scorecard---------------
#install.packages("scorecard")
library(scorecard)

scorecard<-scorecard(scorebin,final_model, odds0 = 1/9,pdo = 20)
#performance evaluation of model using the scorecard.
scorecard::perf_eva(merged_scores$Performance.Tag,merged_scores$prob)

#generating scores for full dataset 
scores<-scorecard_ply(mm,scorecard)

#finding the cut off score based on the probablity cutoff of the LR model.
merged_scores<-mm
merged_scores$score<-scores$score
merged_scores$prob <- predict(final_model, newdata = woe_data, type = "response")
cutoff_score<-select(filter(merged_scores, prob >= cutoff, prob < cutoff + 0.0003), score)$score[1]
cutoff_score

merged_scores$score_perf<- ifelse(merged_scores$score>cutoff_score,0,1)

#confusion matrix for actual Performance tag vs derived based on cutoff score
confusionMatrix(merged_scores$Performance.Tag,merged_scores$score_perf)


## Financial benefit explained in the presentation
