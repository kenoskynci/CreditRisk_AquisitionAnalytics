############################################################################
## BFS Case Study
## By Anshul Roy, Prateek Agrawal and Rinku Kataria
## Roll No. DDA1610188, DDA1610040 and DDA1610116
############################################################################

## Clearing Objects
rm(list = ls())

# Load library
library(dplyr)
library(ggplot2)
library(Information)

################################################################################################
## Checkpoint 1: Data Understanding & Preparation of Master File
################################################################################################

## Loading the given files, Collating into single dataframe, Used stringAsfactors = False
## Use latest R studio version for using na.strings command below.
## set blank & ' ' (one space) values as NA while importing

demographic <- read.csv("Demographic data.csv", stringsAsFactors=FALSE, na.strings=c(""," ","NA"))
str(demographic)
summary(demographic)

## Check if data has duplicated records
sum(duplicated(demographic$Application.ID)) 

dup_demo <- demographic[duplicated(demographic$Application.ID) | 
                          duplicated(demographic$Application.ID, fromLast=TRUE),]
## We notice here three duplicate records as the data values are changing for the applicant with respect to age


# Load Credit Bureau dataset

credit_bureau <- read.csv("Credit Bureau data.csv", stringsAsFactors=FALSE, na.strings=c(""," ","NA"))
str(credit_bureau)
summary(credit_bureau)

## Check if data has duplicated records

sum(duplicated(credit_bureau$Application.ID))
dup_cb <- credit_bureau[duplicated(credit_bureau$Application.ID) | 
                          duplicated(credit_bureau$Application.ID, fromLast=TRUE),]

## Merging both data frames on Application ID

master_data <- merge(x = demographic, y = credit_bureau, by = "Application.ID", all = T)

## Checking structure and summary  of data
str(master_data)
summary(master_data)  

## View the duplicate records along with their original rows

sum(duplicated(master_data$Application.ID))
dup_md <- master_data[duplicated(master_data$Application.ID) | 
                        duplicated(master_data$Application.ID, fromLast=TRUE),]

## As there is no timestamp, the duplicate rows are incorrectly joined

## Remove the Applicant IDs having duplicates as they will provide false observation.
## Note that duplicate are less than 10% of data

master_data <- master_data[!(duplicated(master_data$Application.ID) | duplicated(master_data$Application.ID, fromLast = TRUE)),]
sum(duplicated(master_data$Application.ID)) 

## Checking if Performace Tag is same in files

master_data$diff <- master_data$Performance.Tag.x - master_data$Performance.Tag.y

master_data$diff <- factor(master_data$diff)


## Deleting redundant column and Changing column name

master_data <- master_data[, !(names(master_data) %in% c("diff", "Performance.Tag.x"))]

colnames(master_data)[29] <- "Performance.Tag"


################################################################################################
## Checkpoint 2: Data Preparation
################################################################################################

## Check the variable having NA's

sapply(master_data, function(x) sum(is.na(x)))

## Missing value and outlier treatment in Age variable

# Plotting Age histogram

ggplot(master_data,aes(Age))+geom_histogram()

boxplot(master_data$Age)

quantile(master_data$Age, seq(0,1,0.01))

## Typically, you must be at least 18 to apply for a credit card. 

master_data$Age[master_data$Age < 18] <- 18

#------------------------------------------------------------------------------------------------------------------
## Common function for plotting the graph

plot_response <- function(cat_var, var_name){
  a <- aggregate(Performance.Tag~cat_var, master_data, mean)
  count <- data.frame(table(cat_var))
  count <- count[,-1]
  agg_Performance <- cbind(a, count)
  
  colnames(agg_Performance) <- c(var_name, "Performance_rate","No.of_applicant")
  agg_Performance[, 2] <- format(round(agg_Performance[, 2], 2))
  var_name
  ggplot(agg_Performance, aes(agg_Performance[, 1], count, label = Performance_rate)) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + geom_text(size = 3, vjust = -0.5) + xlab(var_name)
  
}
##-----------------------------------------------------------------------------------------------------------------

# EDA
plot_response(master_data$Age, "Age")

master_data$Age <- as.factor(cut(master_data$Age, breaks = c(17, 30, 35, 38, 41, 44, 47, 50, 53, 57, 65)))
## Missing value and outlier treatment of Gender variable variable

summary(master_data$Gender)

master_data$Gender[which(is.na(master_data$Gender))] <- "M"

# EDA
plot_response(master_data$Gender, "Gender")


## Missing value and outlier treatment of Marital.Status..at.the.time.of.application. variable variable

summary(master_data$Marital.Status..at.the.time.of.application.)

master_data$Marital.Status..at.the.time.of.application.[which(is.na(master_data$Marital.Status..at.the.time.of.application.))] <- "Married"

# EDA
plot_response(master_data$Marital.Status..at.the.time.of.application., "Marital Status")


## Missing value and outlier treatment of No.of.dependents variable variable

summary(master_data$No.of.dependents)

master_data$No.of.dependents[which(is.na(master_data$No.of.dependents))] <- median(master_data$No.of.dependents, na.rm = T)

# EDA
plot_response(master_data$No.of.dependents, "No.of.dependents")


## Missing value and outlier treatment of Income variable variable

summary(master_data$Income)

# Plotting Age histogram

ggplot(master_data,aes(Income))+geom_histogram()

boxplot(master_data$Income, main = "Income", col = "lightgrey", boxwex =0.5)

quantile(master_data$Income, seq(0,1,0.01))

## Update negative income

master_data$Income[master_data$Income < 0.0] <- 0.0


# EDA
plot_response(master_data$Income, "Income")


## Missing value and outlier treatment of Income variable variable

summary(master_data$Education)

str(master_data$Education)

#master_data$Education <- character(master_data$Education)

class(master_data$Education)

# Missing value treatment

master_data$Education[which(is.na(master_data$Education))] <- "Unknown"

# EDA
plot_response(master_data$Education, "Education")


## Missing value and outlier treatment of Profession variable variable

summary(master_data$Profession)

master_data$Profession[which(is.na(master_data$Profession))] <- "SAL"

# EDA
plot_response(master_data$Profession, "Profession")


## Missing value and outlier treatment of Type.of.residence variable variable

summary(master_data$Type.of.residence)

master_data$Type.of.residence[which(is.na(master_data$Type.of.residence))] <- "Rented"

# EDA
plot_response(master_data$Type.of.residence, "Type of residence")


## Missing value and outlier treatment of No.of.months.in.current.residence variable variable

summary(master_data$No.of.months.in.current.residence)

# EDA
plot_response(master_data$No.of.months.in.current.residence, "No.of.months.in.current.residence")


## Missing value and outlier treatment of No.of.months.in.current.company variable variable

summary(master_data$No.of.months.in.current.company)

master_data$No.of.months.in.current.company <- as.factor(cut(master_data$No.of.months.in.current.company, breaks = c(0, 20, 40, 60, 80, 100, 120, 140)))

# EDA
plot_response(master_data$No.of.months.in.current.company, "No.of.months.in.current.company")


## Missing value and outlier treatment of No.of.times.90.DPD.or.worse.in.last.6.months variable variable

summary(master_data$No.of.times.90.DPD.or.worse.in.last.6.months)


## Missing value and outlier treatment of No.of.times.60.DPD.or.worse.in.last.6.months variable variable

summary(master_data$No.of.times.60.DPD.or.worse.in.last.6.months)


## Missing value and outlier treatment of No.of.times.30.DPD.or.worse.in.last.6.months variable variable

summary(master_data$No.of.times.30.DPD.or.worse.in.last.6.months)


## Missing value and outlier treatment of No.of.times.90.DPD.or.worse.in.last.12.months variable variable

summary(master_data$No.of.times.90.DPD.or.worse.in.last.12.months)


## Missing value and outlier treatment of No.of.times.60.DPD.or.worse.in.last.12.months variable variable

summary(master_data$No.of.times.60.DPD.or.worse.in.last.12.months)


## Missing value and outlier treatment of No.of.times.30.DPD.or.worse.in.last.12.months variable variable

summary(master_data$No.of.times.30.DPD.or.worse.in.last.12.months)


## Missing value and outlier treatment of Avgas.CC.Utilization.in.last.12.months variable variable

summary(master_data$Avgas.CC.Utilization.in.last.12.months)

master_data$Avgas.CC.Utilization.in.last.12.months[which(is.na(master_data$Avgas.CC.Utilization.in.last.12.months))] <- 999

master_data$Avgas.CC.Utilization.in.last.12.months <- as.factor(cut(master_data$Avgas.CC.Utilization.in.last.12.months, breaks = c(-1, 5, 7, 9, 12, 15, 22, 38, 52, 72, 113,999)))


## Missing value and outlier treatment of No.of.trades.opened.in.last.6.months variable variable

summary(master_data$No.of.trades.opened.in.last.6.months)

master_data$No.of.trades.opened.in.last.6.months[which(is.na(master_data$No.of.trades.opened.in.last.6.months))] <- median(master_data$No.of.trades.opened.in.last.6.months, na.rm = T)


## Missing value and outlier treatment of No.of.trades.opened.in.last.12.months variable variable

summary(master_data$No.of.trades.opened.in.last.12.months)

master_data$No.of.trades.opened.in.last.12.months <- as.factor(cut(master_data$No.of.trades.opened.in.last.12.months, breaks = c(-1, 1, 2, 5, 9, 13, 28)))



## Missing value and outlier treatment of No.of.PL.trades.opened.in.last.6.months variable variable

summary(master_data$No.of.PL.trades.opened.in.last.6.months)


## Missing value and outlier treatment of No.of.PL.trades.opened.in.last.12.months variable variable

summary(master_data$No.of.PL.trades.opened.in.last.12.months)


## Missing value and outlier treatment of No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. variable variable

summary(master_data$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.)


## Missing value and outlier treatment of No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. variable variable

summary(master_data$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.)


## Missing value and outlier treatment of Presence.of.open.home.loan variable variable

summary(master_data$Presence.of.open.home.loan)

master_data$Presence.of.open.home.loan[which(is.na(master_data$Presence.of.open.home.loan))] <- 999

master_data$Presence.of.open.home.loan <- as.factor(cut(master_data$Presence.of.open.home.loan, breaks = c(-1, 0, 1,999)))

## Missing value and outlier treatment of Outstanding.Balance variable variable

summary(master_data$Outstanding.Balance)

master_data$Outstanding.Balance[which(is.na(master_data$Outstanding.Balance))] <- -999

master_data$Outstanding.Balance <- as.factor(cut(master_data$Outstanding.Balance, breaks = c(-1000, -1, 6847, 25522, 386837, 585441, 774241, 972458, 1357419, 2961007, 3282409, 5218801)))

## Missing value and outlier treatment of Total.No.of.Trades variable variable

summary(master_data$Total.No.of.Trades)

master_data$Total.No.of.Trades <- as.factor(cut(master_data$Total.No.of.Trades, breaks = c(-1, 2, 3, 4, 5, 7, 9, 11, 20, 44)))

## Missing value and outlier treatment of Presence.of.open.auto.loan variable variable

summary(master_data$Presence.of.open.auto.loan)

#Binning 

master_data$No.of.dependents <- as.factor(cut(master_data$No.of.dependents, breaks = c(0, 2, 3, 4, 5)))

master_data$Income <- as.factor(cut(master_data$Income, breaks = c(-1, 4, 10, 16, 21, 26, 31, 36, 41, 48, 60)))

master_data$No.of.months.in.current.residence <- as.factor(cut(master_data$No.of.months.in.current.residence, breaks = c(5, 9, 28, 49, 72, 97, 126)))

#master_data$No.of.months.in.current.company <- as.factor(cut(master_data$No.of.months.in.current.company, breaks = c(2, 5, 12, 19, 26, 33, 40, 47, 53, 61, 133)))

master_data$No.of.times.90.DPD.or.worse.in.last.6.months <- as.factor(cut(master_data$No.of.times.90.DPD.or.worse.in.last.6.months, breaks = c(-1,0,3)))

master_data$No.of.times.60.DPD.or.worse.in.last.6.months <- as.factor(cut(master_data$No.of.times.60.DPD.or.worse.in.last.6.months, breaks = c(-1,0,5)))

master_data$No.of.times.30.DPD.or.worse.in.last.6.months <- as.factor(cut(master_data$No.of.times.30.DPD.or.worse.in.last.6.months, breaks = c(-1,0,1,7)))

master_data$No.of.times.90.DPD.or.worse.in.last.12.months <- as.factor(cut(master_data$No.of.times.90.DPD.or.worse.in.last.12.months, breaks = c(-1,0,1,5)))

master_data$No.of.times.60.DPD.or.worse.in.last.12.months <- as.factor(cut(master_data$No.of.times.60.DPD.or.worse.in.last.12.months, breaks = c(-1,0,1,7)))

master_data$No.of.times.30.DPD.or.worse.in.last.12.months <- as.factor(cut(master_data$No.of.times.30.DPD.or.worse.in.last.12.months, breaks = c(-1,0,2,9)))

#master_data$Avgas.CC.Utilization.in.last.12.months <- as.factor(cut(master_data$Avgas.CC.Utilization.in.last.12.months, breaks = c(-1, 4, 6, 8, 11, 14, 21, 37, 51, 71, 113)))

master_data$No.of.trades.opened.in.last.6.months <- as.factor(cut(master_data$No.of.trades.opened.in.last.6.months, breaks = c(-1, 0, 1, 2, 3, 4, 12)))

#master_data$No.of.trades.opened.in.last.12.months <- as.factor(cut(master_data$No.of.trades.opened.in.last.12.months, breaks = c(-1, 0, 1, 2, 3, 5, 7, 9, 12, 28)))

master_data$No.of.PL.trades.opened.in.last.6.months <- as.factor(cut(master_data$No.of.PL.trades.opened.in.last.6.months, breaks = c(-1, 0, 1, 2, 6)))

master_data$No.of.PL.trades.opened.in.last.12.months <- as.factor(cut(master_data$No.of.PL.trades.opened.in.last.12.months, breaks = c(-1, 0, 1, 2, 3, 4, 5, 12)))

master_data$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. <- as.factor(cut(master_data$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans., breaks = c(-1, 0, 1, 2, 4, 10)))

master_data$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. <- as.factor(cut(master_data$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans., breaks = c(-1, 0, 1, 2, 4, 5, 8, 20)))

#master_data$Presence.of.open.home.loan <- as.factor(cut(master_data$Presence.of.open.home.loan, breaks = c(-1, 0, 1)))

#master_data$Outstanding.Balance <- as.factor(cut(master_data$Outstanding.Balance, breaks = c(-1, 6843, 25509, 386823, 585423, 774228, 972456, 1357399, 2961005, 3282314, 5218801)))

#master_data$Total.No.of.Trades <- as.factor(cut(master_data$Total.No.of.Trades, breaks = c(-1, 1, 2, 3, 4, 5, 6, 8, 10, 19, 44)))

master_data$Presence.of.open.auto.loan <- as.factor(cut(master_data$Presence.of.open.auto.loan, breaks = c(-1, 0, 1)))

master_data$Gender <- as.factor(master_data$Gender)
master_data$Marital.Status..at.the.time.of.application. <- as.factor(master_data$Marital.Status..at.the.time.of.application.)
master_data$Education <- as.factor(master_data$Education)
master_data$Profession <- as.factor(master_data$Profession)
master_data$Type.of.residence <- as.factor(master_data$Type.of.residence)

sum(is.na(master_data))
#Performance.Tag is NA for 1425 records.

master_data <- master_data[,-1]

## Create data frame with NA's performance tag

NA.Performance.Tag <- subset(master_data, is.na(master_data$Performance.Tag))

## Remove NA performance tag form master table

master_data <- master_data[!is.na(master_data$Performance.Tag),]

#---------------- Missing value, outliers treatment and EDA on Credit Bureau data-----------------

IV <- create_infotables(master_data[-1], y="Performance.Tag", bins=10, parallel=FALSE)
IV_Value = data.frame(IV$Summary)
IV_Value

IV$Tables$Avgas.CC.Utilization.in.last.12.months$Avgas.CC.Utilization.in.last.12.months
sum(IV_Value$IV)

library(plyr)
library(dplyr)

master_data$Age_Woe <- mapvalues(master_data$Age, from = IV$Tables$Age$Age, to = IV$Tables$Age$WOE)

master_data$Gender_Woe <- mapvalues(master_data$Gender, from = IV$Tables$Gender$Gender, to = IV$Tables$Gender$WOE)

master_data$Marital.Status..at.the.time.of.application._Woe <- mapvalues(master_data$Marital.Status..at.the.time.of.application., from = IV$Tables$Marital.Status..at.the.time.of.application.$Marital.Status..at.the.time.of.application., to = IV$Tables$Marital.Status..at.the.time.of.application.$WOE)

master_data$No.of.dependents_Woe <- mapvalues(master_data$No.of.dependents, from = IV$Tables$No.of.dependents$No.of.dependents, to = IV$Tables$No.of.dependents$WOE)

master_data$Income_Woe <- mapvalues(master_data$Income, from = IV$Tables$Income$Income, to = IV$Tables$Income$WOE)

master_data$Education_Woe <- mapvalues(master_data$Education, from = IV$Tables$Education$Education, to = IV$Tables$Education$WOE)

master_data$Profession_Woe <- mapvalues(master_data$Profession, from = IV$Tables$Profession$Profession, to = IV$Tables$Profession$WOE)

master_data$Type.of.residence_Woe <- mapvalues(master_data$Type.of.residence, from = IV$Tables$Type.of.residence$Type.of.residence, to = IV$Tables$Type.of.residence$WOE)

master_data$No.of.months.in.current.residence_Woe <- mapvalues(master_data$No.of.months.in.current.residence, from = IV$Tables$No.of.months.in.current.residence$No.of.months.in.current.residence, to = IV$Tables$No.of.months.in.current.residence$WOE)

master_data$No.of.months.in.current.company_Woe <- mapvalues(master_data$No.of.months.in.current.company, from = IV$Tables$No.of.months.in.current.company$No.of.months.in.current.company, to = IV$Tables$No.of.months.in.current.company$WOE)

master_data$No.of.times.90.DPD.or.worse.in.last.6.months_Woe <- mapvalues(master_data$No.of.times.90.DPD.or.worse.in.last.6.months, from = IV$Tables$No.of.times.90.DPD.or.worse.in.last.6.months$No.of.times.90.DPD.or.worse.in.last.6.months, to = IV$Tables$No.of.times.90.DPD.or.worse.in.last.6.months$WOE)

master_data$No.of.times.60.DPD.or.worse.in.last.6.months_Woe <- mapvalues(master_data$No.of.times.60.DPD.or.worse.in.last.6.months, from = IV$Tables$No.of.times.60.DPD.or.worse.in.last.6.months$No.of.times.60.DPD.or.worse.in.last.6.months, to = IV$Tables$No.of.times.60.DPD.or.worse.in.last.6.months$WOE)

master_data$No.of.times.30.DPD.or.worse.in.last.6.months_Woe <- mapvalues(master_data$No.of.times.30.DPD.or.worse.in.last.6.months, from = IV$Tables$No.of.times.30.DPD.or.worse.in.last.6.months$No.of.times.30.DPD.or.worse.in.last.6.months, to = IV$Tables$No.of.times.30.DPD.or.worse.in.last.6.months$WOE)

master_data$No.of.times.90.DPD.or.worse.in.last.12.months_Woe <- mapvalues(master_data$No.of.times.90.DPD.or.worse.in.last.12.months, from = IV$Tables$No.of.times.90.DPD.or.worse.in.last.12.months$No.of.times.90.DPD.or.worse.in.last.12.months, to = IV$Tables$No.of.times.90.DPD.or.worse.in.last.12.months$WOE)

master_data$No.of.times.60.DPD.or.worse.in.last.12.months_Woe <- mapvalues(master_data$No.of.times.60.DPD.or.worse.in.last.12.months, from = IV$Tables$No.of.times.60.DPD.or.worse.in.last.12.months$No.of.times.60.DPD.or.worse.in.last.12.months, to = IV$Tables$No.of.times.60.DPD.or.worse.in.last.12.months$WOE)

master_data$No.of.times.30.DPD.or.worse.in.last.12.months_Woe <- mapvalues(master_data$No.of.times.30.DPD.or.worse.in.last.12.months, from = IV$Tables$No.of.times.30.DPD.or.worse.in.last.12.months$No.of.times.30.DPD.or.worse.in.last.12.months, to = IV$Tables$No.of.times.30.DPD.or.worse.in.last.12.months$WOE)

master_data$Avgas.CC.Utilization.in.last.12.months_Woe <- mapvalues(master_data$Avgas.CC.Utilization.in.last.12.months, from = IV$Tables$Avgas.CC.Utilization.in.last.12.months$Avgas.CC.Utilization.in.last.12.months, to = IV$Tables$Avgas.CC.Utilization.in.last.12.months$WOE)

master_data$No.of.trades.opened.in.last.6.months_Woe <- mapvalues(master_data$No.of.trades.opened.in.last.6.months, from = IV$Tables$No.of.trades.opened.in.last.6.months$No.of.trades.opened.in.last.6.months, to = IV$Tables$No.of.trades.opened.in.last.6.months$WOE)

master_data$No.of.trades.opened.in.last.12.months_Woe <- mapvalues(master_data$No.of.trades.opened.in.last.12.months, from = IV$Tables$No.of.trades.opened.in.last.12.months$No.of.trades.opened.in.last.12.months, to = IV$Tables$No.of.trades.opened.in.last.12.months$WOE)

master_data$No.of.PL.trades.opened.in.last.6.months_Woe <- mapvalues(master_data$No.of.PL.trades.opened.in.last.6.months, from = IV$Tables$No.of.PL.trades.opened.in.last.6.months$No.of.PL.trades.opened.in.last.6.months, to = IV$Tables$No.of.PL.trades.opened.in.last.6.months$WOE)

master_data$No.of.PL.trades.opened.in.last.12.months_Woe <- mapvalues(master_data$No.of.PL.trades.opened.in.last.12.months, from = IV$Tables$No.of.PL.trades.opened.in.last.12.months$No.of.PL.trades.opened.in.last.12.months, to = IV$Tables$No.of.PL.trades.opened.in.last.12.months$WOE)

master_data$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans._Woe <- mapvalues(master_data$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans., from = IV$Tables$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans., to = IV$Tables$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.$WOE)

master_data$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._Woe <- mapvalues(master_data$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans., from = IV$Tables$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans., to = IV$Tables$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.$WOE)

master_data$Presence.of.open.home.loan_Woe <- mapvalues(master_data$Presence.of.open.home.loan, from = IV$Tables$Presence.of.open.home.loan$Presence.of.open.home.loan, to = IV$Tables$Presence.of.open.home.loan$WOE)

master_data$Outstanding.Balance_Woe <- mapvalues(master_data$Outstanding.Balance, from = IV$Tables$Outstanding.Balance$Outstanding.Balance, to = IV$Tables$Outstanding.Balance$WOE)

master_data$Total.No.of.Trades_Woe <- mapvalues(master_data$Total.No.of.Trades, from = IV$Tables$Total.No.of.Trades$Total.No.of.Trades, to = IV$Tables$Total.No.of.Trades$WOE)

master_data$Presence.of.open.auto.loan_Woe <- mapvalues(master_data$Presence.of.open.auto.loan, from = IV$Tables$Presence.of.open.auto.loan$Presence.of.open.auto.loan, to = IV$Tables$Presence.of.open.auto.loan$WOE)

sum(is.na(master_data))

str(master_data)


response.tag <- table(master_data$Performance.Tag)

response <- response.tag[2]/(response.tag[2]+response.tag[1])
#response -> 0.04218195 which is close to 4.25%. I.e. the data is a case of Imbalanced Classification.

## As there is high covariance among features, lets examine dimensions using PCA

# WOE_data_pca = master_data[-1]
# PCA<-prcomp(WOE_data_pca, scale. = T, center = T)
# print(PCA)
# PCA_Scores <- as.data.frame(PCA$x)
# plot(PCA, type='l')

## We observe that most of the variables explains very less variability in the data set

# Combining IV Analysis With Variable Clustering
# This is to observe the variable that has the highest multiple correlation with the variables within its cluster, 
# and the lowest correlation with variables outside the cluster. 
# install.packages("ClustOfVar")
# library(ClustOfVar)
# library(reshape2)
# library(plyr)
# tree <- hclustvar(master_data[,!(names(master_data) %in% c("Performance.Tag"))])
# tree
# nvars <- length(tree[tree$height<0.7])
# part_init<-cutreevar(tree,nvars)$cluster
# kmeans<-kmeansvar(X.quanti=master_data[,!(names(master_data) %in% c("Performance.Tag"))],init=part_init)
# clusters <- cbind.data.frame(melt(kmeans$cluster), row.names(melt(kmeans$cluster)))
# names(clusters) <- c("Cluster", "Variable")
# clusters <- join(clusters, IV$Summary, by="Variable", type="left")
# clusters <- clusters[order(clusters$Cluster),]
# clusters
# clusters$Rank <- ave(-clusters$IV, clusters$Cluster, FUN=rank)
# selected_members <- subset(clusters, Rank==1)
# selected_members$Rank <- NULL
# selected_members
# nrow(selected_members)
# nrow(clusters)
# 
# rm(tree, kmeans, nvars, part_init, selected_members, clusters)


##########################################################################################
## Checkpoint 3: Model Building for Demographic vriables
##########################################################################################

library(caret)
master_data_demo <- master_data[,c(28:38)]

table(master_data_demo$Performance.Tag)

# Implement Random Forest

library(randomForest)

# Spliting the bank data in 70:30 ratio

set.seed(101)

master_data_demo$Performance.Tag <- as.factor(ifelse(master_data_demo$Performance.Tag == 1,"yes","no"))

table(master_data_demo$Performance.Tag)

train_demo.index <- createDataPartition(master_data_demo$Performance.Tag, p=0.7, list = FALSE)

train_demo <- master_data_demo[train_demo.index,]

test_demo <- master_data_demo[-train_demo.index,]


nrow(train_demo)/nrow(master_data_demo)

nrow(test_demo)/nrow(master_data_demo)

#---------------------------------------------------------    

# Building the model 

demo_rf <- randomForest(Performance.Tag ~., data = train_demo, proximity = F, do.trace = T, mtry = 5)

summary(demo_rf)

# Predict response for test data
rf_pred <- predict(demo_rf, test_demo[, -1], type = "prob")

#---------------------------------------------------------    

# Cutoff for randomforest to assign yes or no

perform_fn_rf_demo <- function(cutoff) 
{
  predicted_response <- as.factor(ifelse(rf_pred[, 2] >= cutoff, "yes", "no"))
  conf <- confusionMatrix(predicted_response, test_demo$Performance.Tag, positive = "yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  OUT_rf_demo <- t(as.matrix(c(sens, spec, acc))) 
  colnames(OUT_rf_demo) <- c("sensitivity", "specificity", "accuracy")
  return(OUT_rf_demo)
}

#---------------------------------------------------------    

# creating cutoff values from 0.01 to 0.99 for plotting and initialising a matrix of size 1000x4
s_demo = seq(.01,.99,length=100)

OUT_rf_demo = matrix(0,100,3)

# calculate the sens, spec and acc for different cutoff values

for(i in 1:100)
{
  OUT_rf_demo[i,] = perform_fn_rf_demo(s_demo[i])
} 

#---------------------------------------------------------    

# plotting cutoffs

plot(s_demo, OUT_rf_demo[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s_demo,OUT_rf_demo[,2],col="darkgreen",lwd=2)
lines(s_demo,OUT_rf_demo[,3],col=4,lwd=2)
box()

legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

cutoff_rf <- s_demo[which(abs(OUT_rf_demo[,1]-OUT_rf_demo[,2])<0.06)]

# The plot shows that cutoff value of around 22% optimises sensitivity and accuracy

predicted_response_demo <- factor(ifelse(rf_pred[, 2] >= 0.06, "yes", "no"))

conf_forest <- confusionMatrix(predicted_response_demo, test_demo[, 1], positive = "yes")

conf_forest

# Sensitivity
conf_forest$byClass[1]

# Specificity 
conf_forest$byClass[2]

# Accuracy 
conf_forest$overall[1]


# Final RF important variables
importance <- demo_rf$importance 

importance <- data.frame(importance)


#----------------------------------------------------------------------------------------------------------------------------------------

master_data_woe <- master_data[,28:55]
master_data_all <- master_data
master_data <- master_data[,c(1:28)]


set.seed(101)

library(caret)

train.index <- createDataPartition(master_data_woe$Performance.Tag, p=0.7, list = FALSE)

train <- master_data_woe[train.index,]
test <- master_data_woe[-train.index,]


library(caTools)

library(MASS)

library(car)
library(ROSE)
library(mlr)

round(prop.table(table(train$Performance.Tag))*100) #i.e. 96% value for 0 and 4% for +1. Showing Imbalanced classification of data.
round(prop.table(table(test$Performance.Tag))*100)#i.e. 96% value for 0 and 4% for +1. Showing Imbalanced classification of data.

library(randomForest)
library(caret)

#smote_train <- SMOTE(Performance.Tag ~., train, perc.over = 200, k = 5, perc.under = 200)


library("DMwR")
#create task
train.task <- makeClassifTask(data = train,target = "Performance.Tag",positive = 1)
test.task <- makeClassifTask(data=test,target = "Performance.Tag",positive =1)

train.smote <- smote(train.task,rate = 15,nn = 5)
table(getTaskTargets(train.smote))

#get variable importance chart
library("FSelector")
var_imp <- generateFilterValuesData(train.task,  method = c("information.gain","chi.squared"))
plotFilterValues(var_imp,feat.type.cols = TRUE)

#The plot will show the Important Features
# 
# Now, we'll try to make our data balanced using various techniques such as over sampling, undersampling and SMOTE. In SMOTE, the algorithm
# looks at n nearest neighbors, measures the distance between them and introduces a new observation at the center of n observations. While
# proceeding, we must keep in mind that these techniques have their own drawbacks such as:

#lets see which algorithms are available
listLearners("classif","twoclass")[c("class","package")]

#Below we used Naive Bayes, Decision Tree, Logistic Regression & Random Forest> Random Forest gave the best result.
#Commenting others so that we save time executing them. Incase of verification, please remove the comment and execute.


# #naive Bayes
# library(e1071)
# naive_learner <- makeLearner("classif.naiveBayes",predict.type = "response")
# naive_learner$par.vals <- list(laplace = 1)
# 
# 
# 
# #10fold CV - stratified
# folds <- makeResampleDesc("CV",iters=10,stratify = TRUE)
# 
# #cross validation function
# fun_cv <- function(a){
#   crv_val <- resample(naive_learner,a,folds,measures = list(acc,tpr,tnr,fpr,fp,fn))
#   crv_val$aggr
# }
# 
# 
# fun_cv(train.smote)
# 
# #acc.test.mean tpr.test.mean tnr.test.mean fpr.test.mean  fp.test.mean  fn.test.mean 
# #0.6881313     0.7256577     0.6316132     0.3683868  1145.5000000  1284.8000000 
# 
# #This package names cross validated results are test.mean. After comparing, we see that train.smote gives the highest true positive 
# #rate and true negative rate. Hence, we learn that SMOTE technique outperforms the other two sampling methods.
# 
# #Now, let's build our model SMOTE data and check our final prediction.
# 
# #train and predict
# nB_model <- train(naive_learner, train.smote)
# nB_predict <- predict(nB_model,test.task)
# #evaluate
# nB_prediction <- nB_predict$data$response
# dCM <- confusionMatrix(test$Performance.Tag,nB_prediction)
# # Accuracy : 0.7151
# # Sensitivity : 0.97040
# # Specificity : 0.07217
# 
# #calculate F measure
# precision <- dCM$byClass['Pos Pred Value']
# recall <- dCM$byClass['Sensitivity']
# 
# f_measure <- 2*((precision*recall)/(precision+recall))
# f_measure 
# 
# #Pos Pred Value 
# #0.8297897
# 
# 
# #Decision Tree
# getParamSet("classif.rpart")
# #make tree learner
#  makeatree <- makeLearner("classif.rpart", predict.type = "response")
# #set 3 fold cross validation
#  set_cv <- makeResampleDesc("CV",iters = 3L)
#   #Search for hyperparameters
#   gs <- makeParamSet(
#     makeIntegerParam("minsplit",lower = 10, upper = 50),
#     makeIntegerParam("minbucket", lower = 5, upper = 50),
#     makeNumericParam("cp", lower = 0.001, upper = 0.2)
#   )
# 
#   #do a grid search
#   gscontrol <- makeTuneControlGrid()
#   
#   #hypertune the parameters
#    stune <- tuneParams(learner = makeatree, resampling = set_cv, task = train.smote, par.set = gs, control = gscontrol, measures = acc)
#    
#    #check best parameter
#     # stune$x
#     # $minsplit
#     # [1] 37
#     # 
#     # $minbucket
#     # [1] 30
#     # 
#     # $cp
#     # [1] 0.001
#    
#   
#      
#      #cross validation result
#     stune$y
#     #acc.test.mean 
#     #0.8888576 
#    #using hyperparameters for modeling
#     t.tree <- setHyperPars(makeatree, par.vals = stune$x)
#    
#    #train the model
#     t.rpart <- train(t.tree, train.smote)
#    getLearnerModel(t.rpart)
#    
#    #make predictions
#  tpmodel <- predict(t.rpart, test.task)
#    conf_dt<-confusionMatrix(test$Performance.Tag,tpmodel$data$response)
#    #Accuracy : 0.919 
#    #Sensitivity : 0.95910         
#    #Specificity : 0.05889 
#   

#Random Forest
  getParamSet("classif.randomForest")
 
 #create a learner
  rf <- makeLearner("classif.randomForest", predict.type = "response", par.vals = list(ntree = 100, mtry = 3))
  rf$par.vals <- list(
   importance = TRUE
 )
 
 #set tunable parameters
 #grid search to find hyperparameters
  rf_param <- makeParamSet(
   makeIntegerParam("ntree",lower = 50, upper = 100),
   makeIntegerParam("mtry", lower = 3, upper = 10),
   makeIntegerParam("nodesize", lower = 10, upper = 25)
 )
 
 #let's do random search for 50 iterations
  rancontrol <- makeTuneControlRandom(maxit = 50L)
 
 #set 3 fold cross validation
 set_cv <- makeResampleDesc("CV",iters = 3L)
 
 #hypertuning
  rf_tune <- tuneParams(learner = rf, resampling = set_cv, task = train.smote, par.set = rf_param, control = rancontrol, measures = acc)
 
 
 #cv accuracy
 rf_tune$y
 #acc.test.mean 
 #0.9691891 
 
 #best parameters
 rf_tune$x
 #$ntree
 #[1] 319
 
 #$mtry
 #[1] 10
 
 #$nodesize
 #[1] 10
 
 #using hyperparameters for modeling
  rf.tree <- setHyperPars(rf, par.vals = rf_tune$x)
 
 #train a model
  rforest <- train(rf.tree, train.smote)
  #getLearnerModel(t.rpart)
 
 #make predictions
  rfmodel <- predict(rforest, test.task)
  conf_rf<-confusionMatrix(test$Performance.Tag,rfmodel$data$response)
  #Accuracy : 0.9554 
  #Sensitivity : 0.958222        
  #Specificity : 0.015873  
 
  
  
# #Logistic Regression
# logistic_1 <- glm(formula = Performance.Tag ~ ., family = "binomial", data = train)
# 
# summary(logistic_1)
# 
# #step <-stepAIC(logistic_1,direction="both")
# summary(step)
# 
# model_2 <- glm(formula = Performance.Tag ~ No.of.times.30.DPD.or.worse.in.last.6.months + 
#                  No.of.trades.opened.in.last.6.months + 
#                  No.of.PL.trades.opened.in.last.12.months + No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. + 
#                  No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
#                  Gender_woe + Avgas.CC.Utilization.in.last.12.months_woe + 
#                  No.of.trades.opened.in.last.12.months_woe + No.of.PL.trades.opened.in.last.6.months, family = "binomial", 
#                data = train)
# vif(model_2)
# summary(model_2)
# 
# #Removing No.of.PL.trades.opened.in.last.6.months as it has VIF of 6 & least significant
# model_3 <- glm(formula = Performance.Tag ~ No.of.times.30.DPD.or.worse.in.last.6.months + 
#                  No.of.trades.opened.in.last.6.months + 
#                  No.of.PL.trades.opened.in.last.12.months + No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. + 
#                  No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
#                  Gender_woe + Avgas.CC.Utilization.in.last.12.months_woe + 
#                  No.of.trades.opened.in.last.12.months_woe, family = "binomial", 
#                data = train)
# vif(model_3)
# summary(model_3)
# 
# #Removing No.of.trades.opened.in.last.6.months
# model_4 <- glm(formula = Performance.Tag ~ No.of.times.30.DPD.or.worse.in.last.6.months + 
#                  No.of.PL.trades.opened.in.last.12.months + No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. + 
#                  No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
#                  Gender_woe + Avgas.CC.Utilization.in.last.12.months_woe + 
#                  No.of.trades.opened.in.last.12.months_woe, family = "binomial", 
#                data = train)
# vif(model_4)
# summary(model_4)
# 
# #Removing Gender_woe
# model_5 <- glm(formula = Performance.Tag ~ No.of.times.30.DPD.or.worse.in.last.6.months + 
#                  No.of.PL.trades.opened.in.last.12.months + No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. + 
#                  No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
#                  Avgas.CC.Utilization.in.last.12.months_woe + 
#                  No.of.trades.opened.in.last.12.months_woe, family = "binomial", 
#                data = train)
# vif(model_5)
# summary(model_5)
# 
# logistic_final <- model_5
# 
# #All the variables are significant and hence cannot remove any further.
# #train$prediction  <- predict( model_5, newdata = train , type = "response" )
# #test$prediction  <- predict( model_5, newdata = test , type = "response" )
# #conf_matrix<-confusionMatrix(test$prediction,test$Performance.Tag)
# 
# # 
# # library("ggthemes")
# # # distribution of the prediction score grouped by known outcome
# # ggplot( train, aes( prediction, color = as.factor(Performance.Tag) ) ) + 
# #   geom_density( size = 1 ) +
# #   ggtitle( "Training Set's Predicted Score" ) + 
# #   scale_color_economist( name = "data", labels = c( "negative", "positive" ) ) + 
# #   theme_economist()
# 
# # Predicting probabilities of responding for the test data
# 
# predictions_logit <- predict(logistic_final, newdata = test[, -14], type = "response")
# summary(predictions_logit)
# 
# 
# 
# ## Model Evaluation: Logistic Regression
# 
# # Let's use the probability cutoff of 20%.
# 
# predicted_response <- factor(ifelse(predictions_logit >= 0.20, "yes", "no"))
# 
# summary(predicted_response)
# 
# # Creating confusion matrix for identifying the model evaluation.
# 
# test$Performance.Tag <- factor(ifelse(test$Performance.Tag == 1, "yes", "no"))
# 
# conf <- confusionMatrix(predicted_response, test$Performance.Tag, positive = "yes")
# 
# conf
# 
# acc <- conf$overall[1]
# sens <- conf$byClass[1]
# spec <- conf$byClass[2]
# 
# 
# # Let's find out the optimal probalility cutoff 
# perform_fn <- function(cutoff) 
# {
#   predicted_response <- factor(ifelse(predictions_logit >= cutoff, "yes", "no"))
#   conf <- confusionMatrix(predicted_response, test$Performance.Tag, positive = "yes")
#   acc <- conf$overall[1]
#   sens <- conf$byClass[1]
#   spec <- conf$byClass[2]
#   out <- t(as.matrix(c(sens, spec, acc))) 
#   colnames(out) <- c("sensitivity", "specificity", "accuracy")
#   return(out)
# }
# 
# #---------------------------------------------------------    
# 
# # Creating cutoff values from 0.01 to 0.99 for plotting and initiallizing a matrix of 1000 X 4.
# 
# 
# s = round(seq(.01,.99,length=100),digits = 2) 
# 
# OUT = matrix(0,100,3)
# 
# 
# for(i in 1:100)
# {
#   OUT[i,] = perform_fn(s[i])
# } 
# 
# #---------------------------------------------------------    
# 
# # plotting cutoffs 
# plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
# axis(1,round(seq(0,1,length=20), digits = 3),round(seq(0,1,length=20), digits = 3),cex.lab=1.5)
# axis(2,round(seq(0,1,length=20), digits = 2),round(seq(0,1,length=20), digits = 2),cex.lab=1.5)
# lines(s,OUT[,2],col="darkgreen",lwd=2)
# lines(s,OUT[,3],col=4,lwd=2)
# box()
# legend(0.5,.75,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))
# 
# ## Threshold 0.53
# 
# predicted_response <- factor(ifelse(predictions_logit >= 0.2, "yes", "no"))
# 
# summary(predicted_response)
# 
# conf <- confusionMatrix(predicted_response, test$Performance.Tag, positive = "yes")
# 
# conf

#Very poor sensitivity for Logistic Regression. Here we want to ensure that we predict the defaulters accurately. That is, we need good
#Accuracy & Sensitivity (TP prediction should be good).
#Based on the below input, we considering the Random FOrest Model as the best model.
#Naive Bayes
# Accuracy : 0.7151
# Sensitivity : 0.97040
# Specificity : 0.07217

#Decision Tree
#Accuracy : 0.919 
#Sensitivity : 0.95910         
#Specificity : 0.05889 

#Random Forest
#Accuracy : 0.956 
#Sensitivity : 0.958423        
#Specificity : 0.086207 

#Logistic Regression
#Accuracy -0.9581564 
#Sensitivity -0.001144165 
#Specificity -0.9998008 


########################################################################################
###Process Repeat
###We have 1425 records in NA.Performance.Tag Data Frame having No Performance Tag. We need to predict the performance value for these records
#by passing through the best model created above. Then we will club the complete data and create the best model from that.
#This requires the entire process we implemented above to repeat.
  
#Initially, we need to find the WOE Values of the NA.Performance.Tag. For this, we need to map it to IV Values created above.
  
  NA.Performance.Tag$Age_Woe <- mapvalues(NA.Performance.Tag$Age, from = IV$Tables$Age$Age, to = IV$Tables$Age$WOE)
  
  NA.Performance.Tag$Gender_Woe <- mapvalues(NA.Performance.Tag$Gender, from = IV$Tables$Gender$Gender, to = IV$Tables$Gender$WOE)
  
  NA.Performance.Tag$Marital.Status..at.the.time.of.application._Woe <- mapvalues(NA.Performance.Tag$Marital.Status..at.the.time.of.application., from = IV$Tables$Marital.Status..at.the.time.of.application.$Marital.Status..at.the.time.of.application., to = IV$Tables$Marital.Status..at.the.time.of.application.$WOE)
  
  NA.Performance.Tag$No.of.dependents_Woe <- mapvalues(NA.Performance.Tag$No.of.dependents, from = IV$Tables$No.of.dependents$No.of.dependents, to = IV$Tables$No.of.dependents$WOE)
  
  NA.Performance.Tag$Income_Woe <- mapvalues(NA.Performance.Tag$Income, from = IV$Tables$Income$Income, to = IV$Tables$Income$WOE)
  
  NA.Performance.Tag$Education_Woe <- mapvalues(NA.Performance.Tag$Education, from = IV$Tables$Education$Education, to = IV$Tables$Education$WOE)
  
  NA.Performance.Tag$Profession_Woe <- mapvalues(NA.Performance.Tag$Profession, from = IV$Tables$Profession$Profession, to = IV$Tables$Profession$WOE)
  
  NA.Performance.Tag$Type.of.residence_Woe <- mapvalues(NA.Performance.Tag$Type.of.residence, from = IV$Tables$Type.of.residence$Type.of.residence, to = IV$Tables$Type.of.residence$WOE)
  
  NA.Performance.Tag$No.of.months.in.current.residence_Woe <- mapvalues(NA.Performance.Tag$No.of.months.in.current.residence, from = IV$Tables$No.of.months.in.current.residence$No.of.months.in.current.residence, to = IV$Tables$No.of.months.in.current.residence$WOE)
  
  NA.Performance.Tag$No.of.months.in.current.company_Woe <- mapvalues(NA.Performance.Tag$No.of.months.in.current.company, from = IV$Tables$No.of.months.in.current.company$No.of.months.in.current.company, to = IV$Tables$No.of.months.in.current.company$WOE)
  
  NA.Performance.Tag$No.of.times.90.DPD.or.worse.in.last.6.months_Woe <- mapvalues(NA.Performance.Tag$No.of.times.90.DPD.or.worse.in.last.6.months, from = IV$Tables$No.of.times.90.DPD.or.worse.in.last.6.months$No.of.times.90.DPD.or.worse.in.last.6.months, to = IV$Tables$No.of.times.90.DPD.or.worse.in.last.6.months$WOE)
  
  NA.Performance.Tag$No.of.times.60.DPD.or.worse.in.last.6.months_Woe <- mapvalues(NA.Performance.Tag$No.of.times.60.DPD.or.worse.in.last.6.months, from = IV$Tables$No.of.times.60.DPD.or.worse.in.last.6.months$No.of.times.60.DPD.or.worse.in.last.6.months, to = IV$Tables$No.of.times.60.DPD.or.worse.in.last.6.months$WOE)
  
  NA.Performance.Tag$No.of.times.30.DPD.or.worse.in.last.6.months_Woe <- mapvalues(NA.Performance.Tag$No.of.times.30.DPD.or.worse.in.last.6.months, from = IV$Tables$No.of.times.30.DPD.or.worse.in.last.6.months$No.of.times.30.DPD.or.worse.in.last.6.months, to = IV$Tables$No.of.times.30.DPD.or.worse.in.last.6.months$WOE)
  
  NA.Performance.Tag$No.of.times.90.DPD.or.worse.in.last.12.months_Woe <- mapvalues(NA.Performance.Tag$No.of.times.90.DPD.or.worse.in.last.12.months, from = IV$Tables$No.of.times.90.DPD.or.worse.in.last.12.months$No.of.times.90.DPD.or.worse.in.last.12.months, to = IV$Tables$No.of.times.90.DPD.or.worse.in.last.12.months$WOE)
  
  NA.Performance.Tag$No.of.times.60.DPD.or.worse.in.last.12.months_Woe <- mapvalues(NA.Performance.Tag$No.of.times.60.DPD.or.worse.in.last.12.months, from = IV$Tables$No.of.times.60.DPD.or.worse.in.last.12.months$No.of.times.60.DPD.or.worse.in.last.12.months, to = IV$Tables$No.of.times.60.DPD.or.worse.in.last.12.months$WOE)
  
  NA.Performance.Tag$No.of.times.30.DPD.or.worse.in.last.12.months_Woe <- mapvalues(NA.Performance.Tag$No.of.times.30.DPD.or.worse.in.last.12.months, from = IV$Tables$No.of.times.30.DPD.or.worse.in.last.12.months$No.of.times.30.DPD.or.worse.in.last.12.months, to = IV$Tables$No.of.times.30.DPD.or.worse.in.last.12.months$WOE)
  
  NA.Performance.Tag$Avgas.CC.Utilization.in.last.12.months_Woe <- mapvalues(NA.Performance.Tag$Avgas.CC.Utilization.in.last.12.months, from = IV$Tables$Avgas.CC.Utilization.in.last.12.months$Avgas.CC.Utilization.in.last.12.months, to = IV$Tables$Avgas.CC.Utilization.in.last.12.months$WOE)
  
  NA.Performance.Tag$No.of.trades.opened.in.last.6.months_Woe <- mapvalues(NA.Performance.Tag$No.of.trades.opened.in.last.6.months, from = IV$Tables$No.of.trades.opened.in.last.6.months$No.of.trades.opened.in.last.6.months, to = IV$Tables$No.of.trades.opened.in.last.6.months$WOE)
  
  NA.Performance.Tag$No.of.trades.opened.in.last.12.months_Woe <- mapvalues(NA.Performance.Tag$No.of.trades.opened.in.last.12.months, from = IV$Tables$No.of.trades.opened.in.last.12.months$No.of.trades.opened.in.last.12.months, to = IV$Tables$No.of.trades.opened.in.last.12.months$WOE)
  
  NA.Performance.Tag$No.of.PL.trades.opened.in.last.6.months_Woe <- mapvalues(NA.Performance.Tag$No.of.PL.trades.opened.in.last.6.months, from = IV$Tables$No.of.PL.trades.opened.in.last.6.months$No.of.PL.trades.opened.in.last.6.months, to = IV$Tables$No.of.PL.trades.opened.in.last.6.months$WOE)
  
  NA.Performance.Tag$No.of.PL.trades.opened.in.last.12.months_Woe <- mapvalues(NA.Performance.Tag$No.of.PL.trades.opened.in.last.12.months, from = IV$Tables$No.of.PL.trades.opened.in.last.12.months$No.of.PL.trades.opened.in.last.12.months, to = IV$Tables$No.of.PL.trades.opened.in.last.12.months$WOE)
  
  NA.Performance.Tag$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans._Woe <- mapvalues(NA.Performance.Tag$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans., from = IV$Tables$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans., to = IV$Tables$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.$WOE)
  
  NA.Performance.Tag$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._Woe <- mapvalues(NA.Performance.Tag$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans., from = IV$Tables$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans., to = IV$Tables$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.$WOE)
  
  NA.Performance.Tag$Presence.of.open.home.loan_Woe <- mapvalues(NA.Performance.Tag$Presence.of.open.home.loan, from = IV$Tables$Presence.of.open.home.loan$Presence.of.open.home.loan, to = IV$Tables$Presence.of.open.home.loan$WOE)
  
  NA.Performance.Tag$Outstanding.Balance_Woe <- mapvalues(NA.Performance.Tag$Outstanding.Balance, from = IV$Tables$Outstanding.Balance$Outstanding.Balance, to = IV$Tables$Outstanding.Balance$WOE)
  
  NA.Performance.Tag$Total.No.of.Trades_Woe <- mapvalues(NA.Performance.Tag$Total.No.of.Trades, from = IV$Tables$Total.No.of.Trades$Total.No.of.Trades, to = IV$Tables$Total.No.of.Trades$WOE)
  
  NA.Performance.Tag$Presence.of.open.auto.loan_Woe <- mapvalues(NA.Performance.Tag$Presence.of.open.auto.loan, from = IV$Tables$Presence.of.open.auto.loan$Presence.of.open.auto.loan, to = IV$Tables$Presence.of.open.auto.loan$WOE)
  
  sum(is.na(NA.Performance.Tag))
  str(NA.Performance.Tag)
  
#Now lets use this data frame to predict performance tag using our best model.
  
NA.Performance.Tag_woe <- NA.Performance.Tag[,28:55]
NA.Performance.Tag_all <- NA.Performance.Tag
NA.Performance.Tag <- NA.Performance.Tag[,1:28]
##Assign junk values to Performance.Tag so that we can create classifier Task for that and this will be removed later on
NA.Performance.Tag_woe$Performance.Tag <- '1'
NA.Performance.Tag_woe$Performance.Tag[as.character(NA.Performance.Tag_woe$No.of.dependents) >=0.05] <- '0'
NA.Performance.Tag_woe$Performance.Tag <- as.factor(NA.Performance.Tag_woe$Performance.Tag)

#There are Empty levels in NA.Performance.Tag_woe. Lets add some records from Train for these missing Factors.
#And do the prediction. Thereafter, we can simply drop those added records.


Outstanding.Balance_woe <- unique(as.character(NA.Performance.Tag_woe$Outstanding.Balance_woe))
Total.No.of.Trades_woe <- unique(as.character(NA.Performance.Tag_woe$Total.No.of.Trades_woe))
No.of.months.in.current.company_woe <- unique(as.character(NA.Performance.Tag_woe$No.of.months.in.current.company_woe))
Presence.of.open.home.loan_woe <- unique(as.character(NA.Performance.Tag_woe$Presence.of.open.home.loan_woe))

Outstanding.Balance_woe_train <- unique(as.character(train$Outstanding.Balance_woe))
Total.No.of.Trades_woe_train <- unique(as.character(train$Total.No.of.Trades_woe))
No.of.months.in.current.company_woe_train <- unique(as.character(train$No.of.months.in.current.company_woe))
Presence.of.open.home.loan_woe_train <- unique(as.character(train$Presence.of.open.home.loan_woe))

#Now, using above values, we know which factor is not there in NA.Performance.Tag_woe. Add them from train & predict. Once predicted,
#remove the records >= 1426 index position

temp <- train[train$Outstanding.Balance_woe %in% setdiff(Outstanding.Balance_woe_train,Outstanding.Balance_woe) | train$Total.No.of.Trades_woe %in% setdiff(Total.No.of.Trades_woe_train,Total.No.of.Trades_woe) | train$No.of.months.in.current.company_woe %in% setdiff(No.of.months.in.current.company_woe_train,No.of.months.in.current.company_woe) | train$Presence.of.open.home.loan_woe %in% setdiff(Presence.of.open.home.loan_woe_train,Presence.of.open.home.loan_woe),]
NA.Performance.Tag_woe_1 <- as.data.frame(rbind(NA.Performance.Tag_woe,temp))

#Predict for the Performance.Tag column, values from the Best Model selected above
test.task_NA <- makeClassifTask(data=NA.Performance.Tag_woe_1,target = "Performance.Tag",positive = 1)
test.task_NA <- removeConstantFeatures(test.task_NA)
#make predictions
rfmodel_NA <- predict(rforest, test.task_NA)
x <- as.data.frame(rfmodel_NA$data$response)
#We have predicted correctly. Lets fetch only the records from 1:1425
x <- x[1:1425,]
NA.Performance.Tag_all$Performance.Tag_Predict <- x
NA.Performance.Tag_all$Performance.Tag <- NA.Performance.Tag_all$Performance.Tag_Predict
NA.Performance.Tag_all$Performance.Tag_Predict <- NULL
#Merge the master_data_all & NA.Performance.Tag_all
data_complete <- as.data.frame(rbind(master_data_all, NA.Performance.Tag_all))
data_complete$Performance.Tag <- as.factor(data_complete$Performance.Tag)

data_complete_woe <- data_complete[,28:55]
data_complete_all<- data_complete
data_complete<- data_complete[,1:28]

#write.csv(data_complete_all,"Complete_After_Predict_1425.csv")

#Lets create new model on data_complete_woe. This has all the records from source, woe done, missing value and outliers treated.
#We will create Logistic Regression Model & Random Forest only.



############################################################################
################New Model Creation with complete data set
############################################################################

set.seed(111)
train_new.index <- createDataPartition(data_complete_woe$Performance.Tag, p=0.7, list = FALSE)
train_new <- data_complete_woe[train_new.index,]
test_new <- data_complete_woe[-train_new.index,]
round(prop.table(table(train_new$Performance.Tag))*100) #i.e. 96% value for 0 and 4% for +1. Showing Imbalanced classification of data.
round(prop.table(table(test_new$Performance.Tag))*100)#i.e. 96% value for 0 and 4% for +1. Showing Imbalanced classification of data.

#create task
train_new.task <- makeClassifTask(data = train_new,target = "Performance.Tag")
test_new.task <- makeClassifTask(data=test_new,target = "Performance.Tag")


#remove zero variance features
train_new.task<- removeConstantFeatures(train_new.task)
test_new.task <- removeConstantFeatures(test_new.task)

#get variable importance chart
library("FSelector")
var_imp <- generateFilterValuesData(train_new.task,  method = c("information.gain","chi.squared"))
plotFilterValues(var_imp,feat.type.cols = TRUE)

#The plot will show the Important Features


#Logistic Regression
logistic_new_1 <- glm(formula = Performance.Tag ~ ., family = "binomial", data = train_new)

summary(logistic_new_1)

#step <-stepAIC(logistic_new_1,direction="both")
#summary(step)

model_new_2 <- glm(formula = Performance.Tag ~ No.of.times.90.DPD.or.worse.in.last.6.months + 
                     No.of.times.60.DPD.or.worse.in.last.6.months + No.of.times.30.DPD.or.worse.in.last.6.months + 
                     No.of.PL.trades.opened.in.last.6.months + No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. + 
                     No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                     Income_woe + No.of.months.in.current.residence_woe + Avgas.CC.Utilization.in.last.12.months_woe + 
                     No.of.trades.opened.in.last.12.months_woe, family = "binomial", 
                   data = train_new)
vif(model_new_2)
summary(model_new_2)

#This is the final model. The left variables are significant.

logistic_final_new <- model_new_2

# Predicting probabilities of responding for the test_new data

predictions_logit <- predict(logistic_final_new, newdata = test_new[, -14], type = "response")
table(predictions_logit)


##########################################################
####USe from here, Add Score part & proceed.

## Model Evaluation: Logistic Regression

# Let's use the probability cutoff of 50%.

predicted_response <- factor(ifelse(predictions_logit >= 0.20, "yes", "no"))

summary(predicted_response)

# Creating confusion matrix for identifying the model evaluation.

test_new$Performance.Tag <- factor(ifelse(test_new$Performance.Tag == 1, "yes", "no"))

conf <- confusionMatrix(predicted_response, test_new$Performance.Tag, positive = "yes")

conf

acc <- conf$overall[1]
sens <- conf$byClass[1]
spec <- conf$byClass[2]


# Let's find out the optimal probalility cutoff 
perform_fn <- function(cutoff) 
{
  predicted_response <- factor(ifelse(predictions_logit >= cutoff, "yes", "no"))
  conf <- confusionMatrix(predicted_response, test_new$Performance.Tag, positive = "yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

#---------------------------------------------------------    

# Creating cutoff values from 0.01 to 0.99 for plotting and initiallizing a matrix of 1000 X 4.


s = round(seq(.01,.99,length=100),digits = 2) 

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

#---------------------------------------------------------    

# plotting cutoffs 
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,round(seq(0,1,length=20), digits = 3),round(seq(0,1,length=20), digits = 3),cex.lab=1.5)
axis(2,round(seq(0,1,length=20), digits = 2),round(seq(0,1,length=20), digits = 2),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0.5,.75,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

## Threshold 0.05

predicted_response <- factor(ifelse(predictions_logit >= 0.05, "yes", "no"))

summary(predicted_response)

conf <- confusionMatrix(predicted_response, test_new$Performance.Tag, positive = "yes")

conf

#########################################################################
#####Code for Score
#######Use complete_data_all. It has full data,both Source columns and WOE columns
#########################################################################

#########################################################################
#####Code for Application Score Card


test_new$perdict_default <- as.data.frame(predictions_logit)

test_new$predict_NonDefault <- 1 - test_new$perdict_default

test_new$odds <-  log(test_new$predict_NonDefault/test_new$perdict_default)

# Score = Offset + ( Factor * log(odds) )
# Factor = PDO/ln(2)
# Offset = Score-(Factor*log(odds))

# In our case, PDO = 20, Base Score=400 & odds = 10


Factor = 20/log(2)
#28.8539

Offset = 400 - (28.8539*log(10))

test_new$Score = 333.5614 + (28.8539*test_new$odds)

## This Score is the basis for deciding whether to aquire customer or not.
######## Lift and gain chart



# # plotting the lift chart
# 
# 
# lift <- function(labels , predicted_prob, groups=10) {
#   if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
#   if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
#   helper = data.frame(cbind(labels , predicted_prob))
#   helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
#   gaintable = helper %>% group_by(bucket)  %>%
#     summarise_at(vars(labels ), funs(total = n(),
#                                      totalresp=sum(., na.rm = TRUE))) %>%
#     mutate(Cumresp = cumsum(totalresp),
#            Gain=Cumresp/sum(totalresp)*100,
#            Cumlift=Gain/(bucket*(100/groups)),
#            cumtotal = cumsum(total)
#     )
#   
#   gaintable1 = helper %>% group_by(bucket)  %>%
#     summarise_at(vars(predicted_prob ), funs(totalresp_pred=sum(., na.rm = TRUE))) %>% mutate(totalcumresp_pred = cumsum(totalresp_pred))
#   
#   gaintable<-cbind(gaintable,gaintable1[,-1])
#   ratio_pred_actual <- gaintable[,9]/gaintable[,4]
#   gaintable <- cbind(gaintable,ratio_pred_actual)
#   return(gaintable)
#   
# }
# 
# # Create a Table of cumulative gain and lift 
# 
# test_new$response <- as.factor(ifelse(test_new$perdict_default=="yes",1,0))
# 
# LG = lift(test_predictions$response, test_predictions$predictions, groups = 10)
# 
# # Gain Chart 
# 
# plot(LG$bucket,LG$Gain,col="red",type="l",main="Gain Chart",xlab="% of total targeted",ylab = "% of positive Response")
# 
# # Lift Chart 
# 
# plot(LG$cumtotal,LG$ratio_pred_actual,col="red",type="l",main="Lift Chart",xlab="Number Of Prospect Contacted",ylab = "Ratio of response rate using the model and without using the model")
# 
# 
# 
# # Let's say if you have spent 1Re for each customer
# View(LG)
# 



####Remove unwanted variables
remove(dup_cb,dup_demo,dup_md,NA.Performance.Tag_woe_1,NA.Performance.Tag_woe_2)
remove(temp,train_new.index,train_new.smote,train_new.task,train.index,acc,No.of.trades.opened.in.last.12.months_woe,No.of.months.in.current.residence_woe,No.of.trades.opened.in.last.12.months,No.of.months.in.current.company,No.of.months.in.current.residence,No.of.months.in.current.company_woe,No.of.months.in.current.company_woe_train)
remove(rfmodel_NA,x)