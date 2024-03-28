library(tidyverse)
library(knitr)
library(frequency)
library("car")        #<-- used to get Prestige dataset; and 'symbox' function
library("EnvStats")   #<-- used to get "boxcox" function
library("dlookr")


train_data <- read.csv(file ="~/Downloads/archive (11)/CC_train.csv")
test_data <- read.csv(file ="~/Downloads/archive (11)/CC_test.csv")

#replacing the empty/blank column values with NA
train_data[train_data == ""] <- NA

# Finding the % of NAs in each column
NA_DF <- data.frame(matrix(ncol = 3, nrow = 0))

#provide column names
for(i in 1:ncol(train_data)){
  num_na = sum(is.na(train_data[[i]]))
  if(num_na > 0){
    NA_DF <- rbind(NA_DF, c(colnames(train_data)[i], num_na, round(num_na/nrow(train_data)*100 , 2)))
  }
}

colnames(NA_DF) <- c('column_Name', 'Num_NA', '%_NA')
NA_DF


#---------------------------------
# Numeric Data Quality Report
#---------------------------------

# fetching the numeric data from the train set
train_dataNumeric<- train_data %>%  dplyr::select(where(is.numeric))

glimpse(train_dataNumeric)
summary(train_dataNumeric)


#As R does not have a method for extracting only Q1 or Q3, user-defined function is written to extract them.
# A function “quantile” in R is used in the above code where 2nd element/value of quantile is
#being stored in Q1 and 4th element/value of quantile is being stored in Q3.

Q1<-function(x,na.rm=TRUE) {
  quantile(x,na.rm=na.rm)[2]
}
Q3<-function(x,na.rm=TRUE) {
  quantile(x,na.rm=na.rm)[4]
}

# This code accepts a numerical vector x as an input parameter and then returns a vector where the
#first element is the length of the input vector (i.e., the number of observations), the second element
#is the number of unique values, the third is the number of missing values, the fourth is the mean
#value of non-missing numeric, etc
myNumericSummary <- function(x){
  c(length(x), n_distinct(x), sum(is.na(x)), mean(x, na.rm=TRUE),
    min(x,na.rm=TRUE), Q1(x,na.rm=TRUE), median(x,na.rm=TRUE), Q3(x,na.rm=TRUE),
    max(x,na.rm=TRUE), sd(x,na.rm=TRUE))
}

numericSummary <- train_dataNumeric %>%dplyr::summarise(across(everything(),myNumericSummary))
glimpse(numericSummary)

# column bind some labels to our summary statistics
numericSummary <-cbind(
  stat=c("n","unique","missing","mean","min","Q1","median","Q3","max","sd"),
  numericSummary)
glimpse(numericSummary)

# pivoting the data and adding a couple more computed values: percent missing and percent unique fields
numericSummaryFinal <- numericSummary %>%
  pivot_longer("ID":"Total_EMI_per_month", names_to = "variable", values_to = "value") %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  mutate(missing_pct = 100*missing/n,
         unique_pct = 100*unique/n) %>%
  select(variable, n, missing, missing_pct, unique, unique_pct, everything())
numericSummaryFinal

options(digits=3)
options(scipen=99)
numericSummaryFinal %>% knitr::kable()

#---------------------------------
# Categorical Data Quality Report
#---------------------------------

train_dataFactor<- train_data %>% dplyr::select(where(is.character)) %>% transmute_all(as.factor)

glimpse(train_dataFactor)

mode <- function(v) {
  uniqv <- na.omit(unique(v))
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

factorCount <- function(v){
  count <- fct_count(v)
  return(length(count$f))
}

myFactorSummary <- function(x){
  c(length(x), n_distinct(x), sum(is.na(x)),mode(x),factorCount(x))
}

factorSummary <- train_dataFactor %>% dplyr::summarise(across(everything(),~myFactorSummary(.)))
glimpse(factorSummary)

factorSummary <-cbind(
  stat=c("n","unique","missing","Mode", "Factor Count"),
  factorSummary)
glimpse(factorSummary)

factorySummaryFinal <- factorSummary %>%
  pivot_longer("Customer_ID":"Credit_Score", names_to = "variable", values_to = "value") %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  mutate(missing_pct = 100*as.numeric(missing)/as.numeric(n),
         unique_pct = 100*as.numeric(unique)/as.numeric(n)) %>%
  select(variable, n, missing, missing_pct, unique, unique_pct, everything())
factorySummaryFinal


factorySummaryFinal %>% kable()

#--------------------------------------DQR ends

# Visualization of  % of NAs in each column
train_data %>%
  select(Customer_ID:Credit_Score) %>%
  plot_na_pareto(col = "blue", main = "Pareto Chart for Finding the % of NAs in each column")


# Visualize specific numerical variables with an outlier ratio of 10% or higher
li <- train_data %>% dplyr::select(c("Num_Bank_Accounts","Num_Credit_Card","Interest_Rate",
                                     "Delay_from_due_date","Num_Credit_Inquiries","Total_EMI_per_month"))
train_data %>%
  plot_outlier(diagnose_outlier(train_data) %>%
                 filter(outliers_ratio >= 10) %>%          
                 select(variables) %>%
                 unlist())


# Function to add correlation coefficients
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...) {
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  Cor <- abs(cor(x, y)) 
  txt <- paste0(prefix, format(c(Cor, 0.123456789), digits = digits)[1])
  if(missing(cex.cor)) {
    cex.cor <- 0.4 / strwidth(txt)
  }
  text(0.5, 0.5, txt,
       cex = 1 + cex.cor * Cor) # Resize the text by level of correlation
}

# Plotting the correlation matrix
pairs(train_dataNumeric,
      upper.panel = panel.cor,    # Correlation panel
      lower.panel = panel.smooth) # Smoothed regression lines



#---------------------Pre-processing Begins from here

train_data <- read.csv(file ="C:/Users/deepi/Desktop/Fall 2022/IDA/Project/Train.csv")
test_data <- read.csv(file ="C:/Users/deepi/Desktop/Fall 2022/IDA/Project/Test.csv")

colSums(is.na(train_data))

dtype<-sapply(train_data, typeof)
dtype

#Removing "_" from certain columns data 
conversion<-function(data){
  cc<-c('Age','Annual_Income','Num_of_Loan','Num_of_Delayed_Payment','Outstanding_Debt','Amount_invested_monthly','Monthly_Balance')
  for (i in 1:length(cc)){
    x<-data[,cc[i]]
    g <- grep("[[:digit:]]_",x)
    for(j in 1:length(g)){
      dd<-as.numeric(gsub("_", "", data[g[j],cc[i]]))
      data[g[j],cc[i]]<-dd
    }
  }
  
  return(data)
}

# Converting Credit_History_Age column data
c_h_conversion<-function(data){
  for( i in 1:nrow(data)){
    c<-data[i,"Credit_History_Age"]
    if(is.na(c)==FALSE){
      r<-regmatches(c, gregexpr("[[:digit:]]+", c))
      p<-r[[1]]
      data[i,"Credit_History_Age"]<-as.numeric(p[1])+(as.numeric(p[2])/12)
    }
  }
  return(data)
}

dtype<-sapply(train_data, typeof)
dtype

train_data <- conversion(train_data)
train_data <- c_h_conversion(train_data)

dtype<-sapply(train_data, typeof)
dtype

getmode<-function(train_data){
  d<-unique(train_data)
  return(d[which.max(tabulate(match(train_data, d)))])
  
}

train_data$Annual_Income<-as.numeric(train_data$Annual_Income)

train_data$Num_of_Delayed_Payment<-as.numeric(train_data$Num_of_Delayed_Payment)
train_data$Changed_Credit_Limit<-as.numeric(train_data$Changed_Credit_Limit)

train_data$Outstanding_Debt<-as.numeric(train_data$Outstanding_Debt)

#train_data$Payment_of_Min_Amount<-as.numeric(train_data$Payment_of_Min_Amount)
train_data$Amount_invested_monthly<-as.numeric(train_data$Amount_invested_monthly)

train_data$Monthly_Balance<-as.numeric(train_data$Monthly_Balance)

grouping_by_CustId <- function(dataset){
  data<- dataset %>% group_by(Customer_ID) %>%dplyr::summarize(
    age=getmode(Age),
    Occupation=getmode(Occupation),
    Annual_Income=mean(Annual_Income),
    Monthly_Inhand_Salary=median(Monthly_Inhand_Salary,na.rm=T),
    Num_Bank_Accounts=median(Num_Bank_Accounts,na.rm=T),
    Num_Credit_Card=median(Num_Credit_Card,na.rm=T),
    Interest_Rate=mean(Interest_Rate,na.rm=T),
    Num_of_Loan=max(Num_of_Loan),
    Type_of_Loan=getmode(Type_of_Loan),
    Delay_from_due_date=median(Delay_from_due_date,na.rm = T),
    Num_of_Delayed_Payment=median(Num_of_Delayed_Payment,na.rm=T),
    Changed_Credit_Limit=mean(Changed_Credit_Limit,na.rm=T),
    Num_Credit_Inquiries=median(Num_Credit_Inquiries,na.rm = T),
    Credit_Mix=getmode(Credit_Mix),
    Outstanding_Debt=mean(Outstanding_Debt,na.rm=T),
    Credit_Utilization_Ratio=mean(Credit_Utilization_Ratio,na.rm=T),
    Credit_History_Age=max(Credit_History_Age,na.rm = T),
    Payment_of_Min_Amount=getmode(Payment_of_Min_Amount),
    Total_EMI_per_month=mean(Total_EMI_per_month,na.rm=T),
    Amount_invested_monthly=mean(Amount_invested_monthly,na.rm=T),
    Payment_Behaviour=getmode(Payment_Behaviour),
    Monthly_Balance=mean(Monthly_Balance,na.rm=T),
    Credit_Score=getmode(Credit_Score)
  )
  return(data)
}

train_data <- grouping_by_CustId(train_data)

colSums(is.na(train_data))

#table(train_data$Occupation,train_data$Credit_Score)

train_dataNumeric<- train_data %>%  dplyr::select(where(is.numeric))

par(mfrow=c(3,4))
for(i in colnames(train_dataNumeric)){
  skewnessVal <- skewness(train_dataNumeric[[i]],na.rm=TRUE)
  hist(train_dataNumeric[[i]],main= skewnessVal,xlab =i)
}

#li <- c('Annual_Income','Monthly_Inhand_Salary','Interest_Rate','Outstanding_Debt','Total_EMI_per_month','Amount_invested_monthly','Monthly_Balance')

par(mfrow=c(2,2))
hist(train_dataNumeric$Annual_Income,main="Annual_Income before transformation",xlab="Annual_Income")
symbox(train_dataNumeric$Annual_Income, data = train_dataNumeric,powers=c(3,2,1,0,-0.5,-1,-2), main="Symbox of Annual_Income",ylab="Annual_Income")
hist(log(train_dataNumeric$Annual_Income),main="Histogram of Log(Annual_Income)",xlab="Log(Annual_Income)")
res <- boxcox(train_dataNumeric$Annual_Income + 0.05,optimize = TRUE,lambda = c(-3,3))
train_dataNumeric$Annual_Income <- (train_dataNumeric$Annual_Income^res$lambda -1)/res$lambda
hist(train_dataNumeric$Annual_Income,main="Annual_Income after transformation",xlab="Annual_Income")

par(mfrow=c(2,2))
hist(train_dataNumeric$Monthly_Inhand_Salary,main="Monthly_Inhand_Salary before transformation",xlab="Monthly_Inhand_Salary")
symbox(train_dataNumeric$Monthly_Inhand_Salary, data = train_dataNumeric,powers=c(3,2,1,0,-0.5,-1,-2), main="Symbox of Monthly_Inhand_Salary",ylab="Monthly_Inhand_Salary")
hist(log(train_dataNumeric$Monthly_Inhand_Salary),main="Histogram of Log(Monthly_Inhand_Salary)",xlab="Log(Monthly_Inhand_Salary)")
res <- boxcox(train_dataNumeric$Monthly_Inhand_Salary + 0.05,optimize = TRUE,lambda = c(-3,3))
train_dataNumeric$Monthly_Inhand_Salary <- (train_dataNumeric$Monthly_Inhand_Salary^res$lambda -1)/res$lambda
hist(train_dataNumeric$Monthly_Inhand_Salary,main="Monthly_Inhand_Salary after transformation",xlab="Monthly_Inhand_Salary")

par(mfrow=c(2,2))
hist(train_dataNumeric$Interest_Rate,main="Interest_Rate before transformation",xlab="Interest_Rate")
symbox(train_dataNumeric$Interest_Rate, data = train_dataNumeric,powers=c(3,2,1,0,-0.5,-1,-2), main="Symbox of Interest_Rate",ylab="Interest_Rate")
hist(log(train_dataNumeric$Interest_Rate),main="Histogram of Log(Interest_Rate)",xlab="Log(Interest_Rate)")
res <- boxcox(train_dataNumeric$Interest_Rate + 0.05,optimize = TRUE,lambda = c(-3,3))
train_dataNumeric$Interest_Rate <- (train_dataNumeric$Interest_Rate^res$lambda -1)/res$lambda
hist(train_dataNumeric$Interest_Rate,main="Interest_Rate after transformation",xlab="Interest_Rate")

par(mfrow=c(2,2))
hist(train_dataNumeric$Outstanding_Debt,main="Outstanding_Debt before transformation",xlab="Outstanding_Debt")
symbox(train_dataNumeric$Outstanding_Debt, data = train_dataNumeric,powers=c(3,2,1,0,-0.5,-1,-2), main="Symbox of Outstanding_Debt",ylab="Outstanding_Debt")
hist(log(train_dataNumeric$Outstanding_Debt),main="Histogram of Log(Outstanding_Debt)",xlab="Log(Outstanding_Debt)")
res <- boxcox(train_dataNumeric$Outstanding_Debt + 0.05,optimize = TRUE,lambda = c(-3,3))
train_dataNumeric$Outstanding_Debt <- (train_dataNumeric$Outstanding_Debt^res$lambda -1)/res$lambda
hist(train_dataNumeric$Outstanding_Debt,main="Outstanding_Debt after transformation",xlab="Outstanding_Debt")


par(mfrow=c(2,2))
hist(train_dataNumeric$Total_EMI_per_month,main="Total_EMI_per_month before transformation",xlab="Total_EMI_per_month")
symbox(train_dataNumeric$Total_EMI_per_month, data = train_dataNumeric,powers=c(3,2,1,0,-0.5,-1,-2), main="Symbox of Total_EMI_per_month",ylab="Total_EMI_per_month")
hist(log(train_dataNumeric$Total_EMI_per_month),main="Histogram of Log(Total_EMI_per_month)",xlab="Log(Total_EMI_per_month)")
res <- boxcox(train_dataNumeric$Total_EMI_per_month + 0.05,optimize = TRUE,lambda = c(-3,3))
train_dataNumeric$Total_EMI_per_month <- (train_dataNumeric$Total_EMI_per_month^res$lambda -1)/res$lambda
hist(train_dataNumeric$Total_EMI_per_month,main="Total_EMI_per_month after transformation",xlab="Total_EMI_per_month")

par(mfrow=c(2,2))
hist(train_dataNumeric$Amount_invested_monthly,main="Amount_invested_monthly before transformation",xlab="Amount_invested_monthly")
symbox(train_dataNumeric$Amount_invested_monthly, data = train_dataNumeric,powers=c(3,2,1,0,-0.5,-1,-2), main="Symbox of Amount_invested_monthly",ylab="Amount_invested_monthly")
hist(log(train_dataNumeric$Amount_invested_monthly),main="Histogram of Log(Amount_invested_monthly)",xlab="Log(Amount_invested_monthly)")
res <- boxcox(train_dataNumeric$Amount_invested_monthly + 0.05,optimize = TRUE,lambda = c(-3,3))
train_dataNumeric$Amount_invested_monthly <- (train_dataNumeric$Amount_invested_monthly^res$lambda -1)/res$lambda
hist(train_dataNumeric$Amount_invested_monthly,main="Amount_invested_monthly after transformation",xlab="Amount_invested_monthly")


colSums(is.na(train_data))




