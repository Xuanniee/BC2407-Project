# Group Project Analysis for Churn Data

library(Hmisc)
library(data.table)
library(ggplot2)
library(dplyr)
library(caret)
library(nnet)
library(randomForest)
library(smotefamily)
library(car)
library(glmnet)

# Importing of Data
setwd("C:/Users/Siah Wee Hung/Desktop/BC2407-Project")
churnData <- read.csv("telecom_customer_churn.csv", stringsAsFactors = TRUE, na.strings = c('NULL'))

##################################### Variables with Wrong Classification after Import #############################################
# 1. CustomerID ~ Char instead of Categorical
# 2. Number of Dependents - Factor instead of Integer
# 3. City ~ Char instead of Factor


# ||=================================||
# ||                                 ||
# ||          Data Cleaning          ||
# ||                                 ||
# ||=================================||

churnData$City <- factor(churnData$City)
churnData$Number.of.Dependents <- factor(churnData$Number.of.Dependents)
churnData$Customer.ID <- as.character(churnData$Customer.ID)

# Dropping Variables that are not relevant for our analysis
churnData$Latitude  <- NULL
churnData$Longitude <- NULL
churnData$Zip.Code  <- NULL

## Convert all Whitespaces into NA
churnData[churnData==""] <- NA

## NA Values
sum(is.na(churnData))

# Check the Variables that have Missing Values
churnDataVariables <- colnames(churnData) # list of column names
numVars = length(churnDataVariables)
varTable <- matrix(nrow = numVars, ncol = 1, byrow = FALSE)
colnames(varTable) <- c("Number of Missing Values") # set column name
rownames(varTable) <- churnDataVariables

# Populate the Table with Missing Values
for (index in 1:numVars){
  # Fill in the Variable & Missing Values
  varTable[index, 1] <- sum(is.na(churnData[churnDataVariables[index]]))
}

# Determine the Variables with Missing Values
varTable

## 14 Variables with Missing Values. They are:
# Avg.Monthly.Long.Distance.Charges, Multiple.Lines, Internet.Type, Avg.Monthly.GB.Download, Online.Security,
# Online.Backup, Device.Protection.Plan, Premium.Tech.Support, Streaming.TV, Streaming.Movies, Streaming.Music,
# Unlimited.Data, Churn.Category, Churn.Reason

# View the statistical spread and various point statistics for missing variables
summary(churnData)

## 1. Impute Avg.Monthly.Long.Distance.Charges
# According to Data Dictionary, 0 if not subscribed to Home Phone
churnData$Avg.Monthly.Long.Distance.Charges[churnData$Phone.Service == "No"] <- 0

# Ensure Missing Values are imputed
sum(is.na(churnData$Avg.Monthly.Long.Distance.Charges))


## 2. Impute Avg.Monthly.GB.Download
# 0 if not subscribed to Internet Plan
churnData$Avg.Monthly.GB.Download[churnData$Internet.Service == "No"] <- 0

# Ensure no more Missing Value
sum(is.na(churnData$Avg.Monthly.GB.Download))

## 3. Impute Churn.Category
# Increase the Number of Factors to allow for Different Factors
levels(churnData$Churn.Category) <- c(levels(churnData$Churn.Category), "Staying Customer")
levels(churnData$Churn.Category) <- c(levels(churnData$Churn.Category), "New Customer")


# The high missing values in Churn.Category and Churn.Reason can be attributed to New & Loyal Customers
churnData$Churn.Category[churnData$Customer.Status == "Stayed"] <- "Staying Customer"
churnData$Churn.Category[churnData$Customer.Status == "Joined"] <- "New Customer"
# Determine the True Number of Missing for Churn.Category
sum(is.na(churnData$Churn.Category))
# Drop unused Factor Levels in Churn.Category
churnData$Churn.Category <- droplevels(churnData$Churn.Category)

## 4. Impute Churn.Reason
# Fixing the Factor Labels for Churn.Reason
levels(churnData$Churn.Reason) <- c(levels(churnData$Churn.Reason), "Staying Customer", "New Customer")
# Rationale for Missing Values is identical to Churn.Category
churnData$Churn.Reason[churnData$Customer.Status == "Stayed"] <- "Staying Customer"
churnData$Churn.Reason[churnData$Customer.Status == "Joined"] <- "New Customer"
# Ensure no more Missing Values
sum(is.na(churnData$Churn.Reason))
# Drop the Empty String Factor
churnData$Churn.Reason <- droplevels(churnData$Churn.Reason)

## 5. Impute Internet.Type
# Data appears to be missing for Consumers who did not subscribe to Internet
levels(churnData$Internet.Type) <- c(levels(churnData$Internet.Type), "No Internet Service")
churnData$Internet.Type[churnData$Internet.Service == "No"] <- "No Internet Service"

churnData$Internet.Type <- droplevels(churnData$Internet.Type)
# No Missing Values Check
sum(is.na(churnData$Internet.Type))

## 6. Impute Online.Security {Security Service Provided by Company}
# Missing Values appears to be for the same reason as Type, no Internet
levels(churnData$Online.Security) <- c(levels(churnData$Online.Security), "No Internet Service")
churnData$Online.Security[churnData$Internet.Service == "No"] <- "No Internet Service"
# Verify Missing Values
sum(is.na(churnData$Online.Security))
churnData$Online.Security <- droplevels(churnData$Online.Security)

## 7. Impute Online.Backup
# No Internet
levels(churnData$Online.Backup) <- c(levels(churnData$Online.Backup), "No Internet Service")
churnData$Online.Backup[churnData$Internet.Service == "No"] <- "No Internet Service"
# Verify Missing Values
sum(is.na(churnData$Online.Backup))
churnData$Online.Backup <- droplevels(churnData$Online.Backup)

## 8. Impute Device Protection Plan {For Internet Equipment}
# No Internet
levels(churnData$Device.Protection.Plan) <- c(levels(churnData$Device.Protection.Plan), "No Internet Equipment")
churnData$Device.Protection.Plan[churnData$Internet.Service == "No"] <- "No Internet Equipment"
# Verify Missing Values
sum(is.na(churnData$Device.Protection.Plan))
churnData$Device.Protection.Plan <- droplevels(churnData$Device.Protection.Plan)

## 9. Impute Premium Technical Support 
# {According to Data Dict, No if not subscribed to Internet Service}
churnData$Premium.Tech.Support[churnData$Internet.Service == "No"] <- "No"
# Verify Missing Values
sum(is.na(churnData$Premium.Tech.Support))
churnData$Premium.Tech.Support <- droplevels(churnData$Premium.Tech.Support)

## Impute Streaming Services {If Customer uses their Internet Plan to stream}
# According to Data Dict, will be No if not subscribed to Internet

# 10. Streaming TV
churnData$Streaming.TV[churnData$Internet.Service == "No"] <- "No"
# Verify Missing Values
sum(is.na(churnData$Streaming.TV))
churnData$Streaming.TV <- droplevels(churnData$Streaming.TV)

# 11. Streaming Movies
churnData$Streaming.Movies[churnData$Internet.Service == "No"] <- "No"
# Verify Missing Values
sum(is.na(churnData$Streaming.Movies))
churnData$Streaming.Movies <- droplevels(churnData$Streaming.Movies)

# 12. Streaming Music
churnData$Streaming.Music[churnData$Internet.Service == "No"] <- "No"
# Verify Missing Values
sum(is.na(churnData$Streaming.Music))
churnData$Streaming.Music <- droplevels(churnData$Streaming.Music)

## 13. Impute Unlimited Data
# Unlimited Data for Internet Plan, so No if Not Subscribed
churnData$Unlimited.Data[churnData$Internet.Service == "No"] <- "No"
# Verify Missing Values
sum(is.na(churnData$Unlimited.Data))
churnData$Unlimited.Data <- droplevels(churnData$Unlimited.Data)

## 14. Impute Multiple.Lines 
# According to Data Dictionary, will be No if not subscribed to Home Phone Service.
churnData$Multiple.Lines[churnData$Phone.Service == "No"] <- "No"
# Verify Missing Values
sum(is.na(churnData$Multiple.Lines))
churnData$Multiple.Lines <- droplevels(churnData$Multiple.Lines)
# Not necessary to impute with Dependents since no more Missing Values

# Verify no more Missing for entire dataset
sum(is.na(churnData))

# Remove the Whitespace in Offer A/B/C/D/E
levels(churnData$Offer) <- c(levels(churnData$Offer), "Offer.A", "Offer.B", "Offer.C", "Offer.D", "Offer.E")
churnData$Offer[churnData$Offer == "Offer A"] <- "Offer.A"
churnData$Offer[churnData$Offer == "Offer B"] <- "Offer.B"
churnData$Offer[churnData$Offer == "Offer C"] <- "Offer.C"
churnData$Offer[churnData$Offer == "Offer D"] <- "Offer.D"
churnData$Offer[churnData$Offer == "Offer E"] <- "Offer.E"

# Remove the Old Labels
churnData$Offer <- droplevels(churnData$Offer)



# ||==============================================================||
# ||                                                              ||
# ||          Data Visualisation: Univariate Categorical          ||
# ||                                                              ||
# ||==============================================================||

# Gender
ggplot(data = churnData, aes(x = Gender, fill = Gender)) +
  geom_bar() +
  scale_fill_manual("Gender", values = c("Male" = "Light Blue", "Female" = "Pink")) +
  labs(title = "Proportion of Customer by Gender")

# Married
ggplot(data = churnData, aes(x = Married, fill = Married)) +
  geom_bar() +
  scale_fill_manual("Married", values = c("No" = "Light Blue", "Yes" = "Pink")) +
  labs(title = "Proportion of Customer who is Married")

# Number.of.Dependents
ggplot(data = churnData, aes(x = Number.of.Dependents, fill = Number.of.Dependents)) +
  geom_bar() +
  scale_fill_manual("Number.of.Dependents", values = c(rep(1:10))) +
  labs(title = "Proportion of Customer by Number of Dependents")

# Offer
## Identifies the last marketing offer that the customer accepted: None, Offer A, Offer B, Offer C, Offer D, Offer E
ggplot(data = churnData, aes(x = Offer, fill = Offer)) +
  geom_bar() +
  scale_fill_manual("Offer", values = c("None" = "Red", 
                                        "Offer.A" = "Orange",
                                        "Offer.B" = "Yellow",
                                        "Offer.C" = "Green",
                                        "Offer.D" = "Blue",
                                        "Offer.E" = "Purple")) +
  labs(title = "Proportion of Offer accepted by Customers")

# Phone.Service
## Indicates if the customer subscribes to home phone service with the company: Yes, No
ggplot(data = churnData, aes(x = Phone.Service, fill = Phone.Service)) +
  geom_bar() +
  scale_fill_manual("Phone.Service", values = c("No" = "Light Blue", "Yes" = "Pink")) +
  labs(title = "Proportion of Customer who subscribed to Home Phone Service")

# Multiple.Lines
## Indicates if the customer subscribes to multiple telephone lines with the company: Yes, No 
ggplot(data = churnData, aes(x = Multiple.Lines, fill = Multiple.Lines)) +
  geom_bar() +
  scale_fill_manual("Multiple.Lines", values = c("No" = "Light Blue", "Yes" = "Pink")) +
  labs(title = "Proportion of Customer who are subscribed to multiple lines with the Company")

# Internet.Service
## Indicates if the customer subscribes to multiple telephone lines with the company: Yes, No 
ggplot(data = churnData, aes(x = Internet.Service, fill = Internet.Service)) +
  geom_bar() +
  scale_fill_manual("Internet.Service", values = c("No" = "Light Blue", "Yes" = "Pink")) +
  labs(title = "Proportion of Customer who subscribed to Internet Service with Company")

# Internet.Type
## Indicates the customer's type of internet connection: DSL, Fiber Optic, Cable, No Internet
ggplot(data = churnData, aes(x = Internet.Type, fill = Internet.Type)) +
  geom_bar() +
  scale_fill_manual("Internet.Type", values = c("No Internet Service" = "Pink", 
                                                "Cable" = "Light Blue",
                                                "DSL" = "Yellow",
                                                "Fiber Optic" = "Orange")) +
  labs(title = "Proportion of Customer by Internet Connection Type")

# Online.Security
## Indicates if the customer subscribes to an additional online security service provided by the company
ggplot(data = churnData, aes(x = Online.Security, fill = Online.Security)) +
  geom_bar() +
  scale_fill_manual("Online.Security", values = c("No Internet Service" = "Pink", 
                                                  "Yes" = "Light Blue",
                                                  "No" = "Yellow")) +
  labs(title = "Proportion of Customer who subscribed to Security Plan")

# Online.Backup
## Indicates if the customer subscribes to an additional online backup service provided by the company
ggplot(data = churnData, aes(x = Online.Backup, fill = Online.Backup)) +
  geom_bar() +
  scale_fill_manual("Online.Backup", values = c("No Internet Service" = "Pink", 
                                                "Yes" = "Light Blue",
                                                "No" = "Yellow")) +
  labs(title = "Proportion of Customer who subscribed to Backup Plan")

# Device.Protection.Plan
## Indicates if the customer subscribes to an additional device protection plan for their Internet equipment provided by the company
ggplot(data = churnData, aes(x = Device.Protection.Plan, fill = Device.Protection.Plan)) +
  geom_bar() +
  scale_fill_manual("Device.Protection.Plan", values = c("No Internet Equipment" = "Pink", 
                                                         "Yes" = "Light Blue",
                                                         "No" = "Yellow")) +
  labs(title = "Proportion of Customer who subscribed to Device Protection Plan")

# Premium.Tech.Support
## Indicates if the customer subscribes to an additional technical support plan from the company with reduced wait times
ggplot(data = churnData, aes(x = Premium.Tech.Support, fill = Premium.Tech.Support)) +
  geom_bar() +
  scale_fill_manual("Premium.Tech.Support", values = c("No" = "Light Blue", "Yes" = "Pink")) +
  labs(title = "Proportion of Customer who subscribed to Premium Tech Support")

# Streaming.TV
## Indicates if the customer uses their Internet service to stream television programing from a third party provider at no additional fee
ggplot(data = churnData, aes(x = Streaming.TV, fill = Streaming.TV)) +
  geom_bar() +
  scale_fill_manual("Streaming.TV", values = c("No" = "Light Blue", "Yes" = "Pink")) +
  labs(title = "Proportion of Customer who streams Television")

# Streaming.Movies
## Indicates if the customer uses their Internet service to stream movies from a third party provider at no additional fee
ggplot(data = churnData, aes(x = Streaming.Movies, fill = Streaming.Movies)) +
  geom_bar() +
  scale_fill_manual("Streaming.Movies", values = c("No" = "Light Blue", "Yes" = "Pink")) +
  labs(title = "Proportion of Customer who streams Movies")

# Streaming.Music
## Indicates if the customer uses their Internet service to stream music from a third party provider at no additional fee
ggplot(data = churnData, aes(x = Streaming.Music, fill = Streaming.Music)) +
  geom_bar() +
  scale_fill_manual("Streaming.Music", values = c("No" = "Light Blue", "Yes" = "Pink")) +
  labs(title = "Proportion of Customer who streams Music")

# Unlimited.Data
## Indicates if the customer has paid an additional monthly fee to have unlimited data downloads/uploads
ggplot(data = churnData, aes(x = Unlimited.Data, fill = Unlimited.Data)) +
  geom_bar() +
  scale_fill_manual("Unlimited.Data", values = c("No" = "Light Blue", "Yes" = "Pink")) +
  labs(title = "Proportion of Customer who have Unlimited Data")

# Contract
## Indicates the customer’s current contract type
ggplot(data = churnData, aes(x = Contract, fill = Contract)) +
  geom_bar() +
  scale_fill_manual("Contract", values = c("Month-to-Month" = "Pink", 
                                           "One Year" = "Light Blue",
                                           "Two Year" = "Yellow")) +
  labs(title = "Proportion of Customer by Current Contract Type")

# Paperless.Billing
## Indicates if the customer has chosen paperless billing
ggplot(data = churnData, aes(x = Paperless.Billing, fill = Paperless.Billing)) +
  geom_bar() +
  scale_fill_manual("Paperless.Billing", values = c("No" = "Light Blue", "Yes" = "Pink")) +
  labs(title = "Proportion of Customer who opted for Paperless Billing")

# Payment.Method
## Indicates how the customer pays their bill
ggplot(data = churnData, aes(x = Payment.Method, fill = Payment.Method)) +
  geom_bar() +
  scale_fill_manual("Payment.Method", values = c("Bank Withdrawal" = "Pink", 
                                                 "Credit Card" = "Light Blue",
                                                 "Mailed Check" = "Yellow")) +
  labs(title = "Proportion of Customer by Payment Methods")

# Customer.Status
## Indicates the status of the customer at the end of the quarter
ggplot(data = churnData, aes(x = Customer.Status, fill = Customer.Status)) +
  geom_bar() +
  scale_fill_manual("Customer.Status", values = c("Churned" = "Pink", 
                                                  "Stayed" = "Light Blue",
                                                  "Joined" = "Yellow")) +
  labs(title = "Proportion of Customer by Status at end of quarter")

# Churn.Category
## A high-level category for the customer’s reason for churning, which is asked when they leave the company
############################################# Dont know if shud remove the Staying and New Customer
ggplot(data = churnData, aes(x = Churn.Category, fill = Churn.Category)) +
  geom_bar() +
  scale_fill_manual("Churn.Category", values = c(rep(1:7))) +
  labs(title = "Proportion of Customer by Category for Churn")

# Churn.Reason
## A customer’s specific reason for leaving the company, which is asked when they leave the company (directly related to Churn Category)
############################################# Dont know if shud remove the Staying and New Customer
edaData <- churnData[-which(churnData$Churn.Reason == "Staying Customer" | churnData$Churn.Reason == "New Customer"), ]
ggplot(data = edaData, aes(x = Churn.Reason, fill = Churn.Reason)) +
  geom_bar() +
  scale_fill_manual("Churn.Reason", values = c(rep(1:22))) +
  labs(title = "Proportion of Customer by Reason for Churn")


# ||==========================================================||
# ||                                                          ||
# ||          Data Visualisation: Univariate Numeric          ||
# ||                                                          ||
# ||==========================================================||

# Age
## The customer’s current age, in years, at the time the fiscal quarter ended (Q2 2022)
ggplot(churnData, aes(x = Age)) +
  geom_histogram(binwidth = 1, alpha=0.8, fill="red", color="black") +
  labs(x = "Age",
       y = "Count",
       title = "Histogram of the Age of Customers")

# Number.of.Referrals
## Indicates the number of times the customer has referred a friend or family member to this company to date
ggplot(churnData, aes(x = Number.of.Referrals)) +
  geom_histogram(binwidth = 1, alpha=0.8, fill="red", color="black") +
  labs(x = "Number.of.Referrals",
       y = "Count",
       title = "Histogram of the Number.of.Referrals of Customers")

# Tenure.in.Months
## Indicates the total amount of months that the customer has been with the company by the end of the quarter specified above
ggplot(churnData, aes(x = Tenure.in.Months)) +
  geom_histogram(binwidth = 1, alpha=0.8, fill="red", color="black") +
  labs(x = "Tenure.in.Months",
       y = "Count",
       title = "Histogram of the Tenure.in.Months of Customers by quarter end")

# Avg.Monthly.Long.Distance.Charges
## Indicates the customer’s average long distance charges, calculated to the end of the quarter specified above
ggplot(churnData, aes(x = Avg.Monthly.Long.Distance.Charges)) +
  geom_histogram(binwidth = 1, alpha=0.8, fill="red", color="black") +
  labs(x = "Avg.Monthly.Long.Distance.Charges",
       y = "Count",
       title = "Histogram of the Avg.Monthly.Long.Distance.Charges of Customers by quarter end")

# Avg.Monthly.GB.Download
## Indicates the customer’s average download volume in gigabytes, calculated to the end of the quarter specified above (if the customer is not subscribed to internet service, this will be 0)
ggplot(churnData, aes(x = Avg.Monthly.GB.Download)) +
  geom_histogram(binwidth = 1, alpha=0.8, fill="red", color="black") +
  labs(x = "Avg.Monthly.GB.Download",
       y = "Count",
       title = "Histogram of the Avg.Monthly.GB.Download of Customers by quarter end")

# Monthly.Charge
## Indicates the customer’s current total monthly charge for all their services from the company
ggplot(churnData, aes(x = Monthly.Charge)) +
  geom_histogram(binwidth = 1, alpha=0.8, fill="red", color="black") +
  labs(x = "Monthly.Charge",
       y = "Count",
       title = "Histogram of the Total Monthly Charges of Customers")

# Total.Charges
## Indicates the customer’s total charges, calculated to the end of the quarter specified above
ggplot(churnData, aes(x = Total.Charges)) +
  geom_histogram(binwidth = 1, alpha=0.8, fill="red", color="black") +
  labs(x = "Total.Charges",
       y = "Count",
       title = "Histogram of the Total Charges of Customers by End of Quarters")

# Total.Refunds
## Indicates the customer’s total refunds, calculated to the end of the quarter specified above
ggplot(churnData, aes(x = Total.Refunds)) +
  geom_histogram(binwidth = 1, alpha=0.8, fill="red", color="black") +
  labs(x = "Total.Refunds",
       y = "Count",
       title = "Histogram of the Total Refunds of Customers by End of Quarter")


# Total.Extra.Data.Charges
## Indicates the customer’s total charges for extra data downloads above those specified in their plan, by the end of the quarter specified above
ggplot(churnData, aes(x = Total.Extra.Data.Charges)) +
  geom_histogram(binwidth = 1, alpha=0.8, fill="red", color="black") +
  labs(x = "Total.Extra.Data.Charges",
       y = "Count",
       title = "Histogram of the Total Charges for Extra Data Downloads of Customers by End of Quarter")

# Total.Long.Distance.Charges
## Indicates the customer’s total charges for long distance above those specified in their plan, by the end of the quarter specified above
ggplot(churnData, aes(x = Total.Long.Distance.Charges)) +
  geom_histogram(binwidth = 1, alpha=0.8, fill="red", color="black") +
  labs(x = "Total.Long.Distance.Charges",
       y = "Count",
       title = "Histogram of the Total Charges for Exceeded Long Distance of Customers by End of Quarter")

# Total.Revenue
## Indicates the company's total revenue from this customer, calculated to the end of the quarter specified above (Total Charges - Total Refurnds + Total Extra Data Charges + Total Lond Distance Charges)
ggplot(churnData, aes(x = Total.Revenue)) +
  geom_histogram(binwidth = 1, alpha=0.8, fill="red", color="black") +
  labs(x = "Total.Revenue",
       y = "Count",
       title = "Histogram of the Total Revenue from Customers by End of Quarter")


# ||==============================================================================||
# ||                                                                              ||
# ||          Data Visualisation: Bivariate [Categorical vs Categorical]          ||
# ||                                                                              ||
# ||==============================================================================||

# Gender against Customer.Status
ggplot(churnData, aes(x = Gender, fill = Customer.Status)) +
  geom_bar(position = "stack") +
  labs(x = "Gender",
       y = "Count",
       title = "Relationship between Gender and Customer Churn") +
  theme_minimal()

# Married against Customer.Status
ggplot(churnData, aes(x = Married, fill = Customer.Status)) +
  geom_bar(position = "stack") +
  labs(x = "Married",
       y = "Count",
       title = "Relationship between Married and Customer Churn") +
  theme_minimal()

# Number of Dependents against Customer.Status
ggplot(churnData, aes(x = Number.of.Dependents, fill = Customer.Status)) +
  geom_bar(position = "stack") +
  labs(x = "Number.of.Dependents",
       y = "Count",
       title = "Relationship between Number.of.Dependents and Customer Churn") +
  theme_minimal()

# Offer against Customer.Status
ggplot(churnData, aes(x = Offer, fill = Customer.Status)) +
  geom_bar(position = "stack") +
  labs(x = "Offer",
       y = "Count",
       title = "Relationship between Offer Accepted and Customer Churn") +
  theme_minimal()

# Phone.Service against Customer.Status
ggplot(churnData, aes(x = Phone.Service, fill = Customer.Status)) +
  geom_bar(position = "stack") +
  labs(x = "Phone.Service",
       y = "Count",
       title = "Relationship between Home Phone Subscription and Customer Churn") +
  theme_minimal()

# Multiple.Lines against Customer.Status
ggplot(churnData, aes(x = Multiple.Lines, fill = Customer.Status)) +
  geom_bar(position = "stack") +
  labs(x = "Multiple.Lines",
       y = "Count",
       title = "Relationship between Multiple Line Subscriptions and Customer Churn") +
  theme_minimal()

# Internet.Service against Customer.Status
ggplot(churnData, aes(x = Internet.Service, fill = Customer.Status)) +
  geom_bar(position = "stack") +
  labs(x = "Internet.Service",
       y = "Count",
       title = "Relationship between Internet Service Subscriptions and Customer Churn") +
  theme_minimal()

# Internet.Type against Customer.Status
ggplot(churnData, aes(x = Internet.Type, fill = Customer.Status)) +
  geom_bar(position = "stack") +
  labs(x = "Internet.Type",
       y = "Count",
       title = "Relationship between Internet Equipment Type and Customer Churn") +
  theme_minimal()

# Online.Security against Customer.Status
ggplot(churnData, aes(x = Online.Security, fill = Customer.Status)) +
  geom_bar(position = "stack") +
  labs(x = "Online.Security",
       y = "Count",
       title = "Relationship between Online Security Plan Subscription and Customer Churn") +
  theme_minimal()

# Online.Backup against Customer.Status
ggplot(churnData, aes(x = Online.Backup, fill = Customer.Status)) +
  geom_bar(position = "stack") +
  labs(x = "Online.Backup",
       y = "Count",
       title = "Relationship between Online Backup Plan Subscription and Customer Churn") +
  theme_minimal()

# Device.Protection.Plan against Customer.Status
ggplot(churnData, aes(x = Device.Protection.Plan, fill = Customer.Status)) +
  geom_bar(position = "stack") +
  labs(x = "Device.Protection.Plan",
       y = "Count",
       title = "Relationship between Device Protection Plan Subscription and Customer Churn") +
  theme_minimal()

# Premium.Tech.Support against Customer.Status
ggplot(churnData, aes(x = Premium.Tech.Support, fill = Customer.Status)) +
  geom_bar(position = "stack") +
  labs(x = "Premium.Tech.Support",
       y = "Count",
       title = "Relationship between Premium Tech Support Plan Subscription and Customer Churn") +
  theme_minimal()

# Streaming.TV against Customer.Status
ggplot(churnData, aes(x = Streaming.TV, fill = Customer.Status)) +
  geom_bar(position = "stack") +
  labs(x = "Streaming.TV",
       y = "Count",
       title = "Relationship between Streaming TV and Customer Churn") +
  theme_minimal()

# Streaming.Movies against Customer.Status
ggplot(churnData, aes(x = Streaming.Movies, fill = Customer.Status)) +
  geom_bar(position = "stack") +
  labs(x = "Streaming.Movies",
       y = "Count",
       title = "Relationship between Streaming Movies and Customer Churn") +
  theme_minimal()

# Streaming.Music against Customer.Status
ggplot(churnData, aes(x = Streaming.Music, fill = Customer.Status)) +
  geom_bar(position = "stack") +
  labs(x = "Streaming.Music",
       y = "Count",
       title = "Relationship between Streaming Music and Customer Churn") +
  theme_minimal()

# Unlimited.Data against Customer.Status
ggplot(churnData, aes(x = Unlimited.Data, fill = Customer.Status)) +
  geom_bar(position = "stack") +
  labs(x = "Unlimited.Data",
       y = "Count",
       title = "Relationship between Unlimited Data and Customer Churn") +
  theme_minimal()

# Contract against Customer.Status
ggplot(churnData, aes(x = Contract, fill = Customer.Status)) +
  geom_bar(position = "stack") +
  labs(x = "Contract",
       y = "Count",
       title = "Relationship between Contract Type and Customer Churn") +
  theme_minimal()

# Paperless.Billing against Customer.Status
ggplot(churnData, aes(x = Paperless.Billing, fill = Customer.Status)) +
  geom_bar(position = "stack") +
  labs(x = "Paperless.Billing",
       y = "Count",
       title = "Relationship between opting for Paperless Billing and Customer Churn") +
  theme_minimal()

# Payment.Method against Customer.Status
ggplot(churnData, aes(x = Payment.Method, fill = Customer.Status)) +
  geom_bar(position = "stack") +
  labs(x = "Payment.Method",
       y = "Count",
       title = "Relationship between Payment Methods and Customer Churn") +
  theme_minimal()


# ||==========================================================================||
# ||                                                                          ||
# ||          Data Visualisation: Bivariate [Categorical vs Numeric]          ||
# ||                                                                          ||
# ||==========================================================================||

# Age vs Customer.Status
ggplot(churnData, aes(x = Customer.Status, y = Age, fill = Customer.Status)) +
  geom_violin() +
  geom_boxplot(width = 0.5,
               fill = "Orange",
               outlier.colour = "Orange",
               outlier.size = 1.5) +
  labs(x = "Customer.Status",
       y = "Age",
       title = "Relationship between Age & Customer.Status") 

# Number.of.Referrals vs Customer.Status
ggplot(churnData, aes(x = Customer.Status, y = Number.of.Referrals, fill = Customer.Status)) +
  geom_violin() +
  geom_boxplot(width = 0.5,
               fill = "Orange",
               outlier.colour = "Orange",
               outlier.size = 1.5) +
  labs(x = "Customer.Status",
       y = "Number.of.Referrals",
       title = "Relationship between Number.of.Referrals & Customer.Status")

# Tenure.in.Months vs Customer.Status
ggplot(churnData, aes(x = Customer.Status, y = Tenure.in.Months, fill = Customer.Status)) +
  geom_violin() +
  geom_boxplot(width = 0.1,
               fill = "Orange",
               outlier.colour = "Orange",
               outlier.size = 1.5,
               alpha = 0.3) +
  labs(x = "Customer.Status",
       y = "Tenure.in.Months",
       title = "Relationship between Tenure.in.Months & Customer.Status")

# Avg.Monthly.Long.Distance.Charges vs Customer.Status
ggplot(churnData, aes(x = Customer.Status, y = Avg.Monthly.Long.Distance.Charges, fill = Customer.Status)) +
  geom_violin() +
  geom_boxplot(width = 0.1,
               fill = "Orange",
               outlier.colour = "Orange",
               outlier.size = 1.5) +
  labs(x = "Customer.Status",
       y = "Avg.Monthly.Long.Distance.Charges",
       title = "Relationship between Avg.Monthly.Long.Distance.Charges & Customer.Status")

# Avg.Monthly.GB.Download vs Customer.Status
ggplot(churnData, aes(x = Customer.Status, y = Avg.Monthly.GB.Download, fill = Customer.Status)) +
  geom_violin() +
  geom_boxplot(width = 0.5,
               fill = "Orange",
               outlier.colour = "Orange",
               outlier.size = 1.5) +
  labs(x = "Customer.Status",
       y = "Avg.Monthly.GB.Download",
       title = "Relationship between Avg.Monthly.GB.Download & Customer.Status")

# Monthly.Charge vs Customer.Status
ggplot(churnData, aes(x = Customer.Status, y = Monthly.Charge, fill = Customer.Status)) +
  geom_violin() +
  geom_boxplot(width = 0.5,
               fill = "Orange",
               outlier.colour = "Orange",
               outlier.size = 1.5,
               alpha = 0.5) +
  labs(x = "Customer.Status",
       y = "Monthly.Charge",
       title = "Relationship between Monthly.Charge & Customer.Status")

# Total.Charges vs Customer.Status
ggplot(churnData, aes(x = Customer.Status, y = Total.Charges, fill = Customer.Status)) +
  geom_violin() +
  geom_boxplot(width = 0.5,
               fill = "Orange",
               outlier.colour = "Orange",
               outlier.size = 1.5) +
  labs(x = "Customer.Status",
       y = "Total.Charges",
       title = "Relationship between Total.Charges & Customer.Status")

# Total.Refunds vs Customer.Status
ggplot(churnData, aes(x = Customer.Status, y = Total.Refunds, fill = Customer.Status)) +
  geom_violin() +
  geom_boxplot(width = 0.5,
               fill = "Orange",
               outlier.colour = "Orange",
               outlier.size = 1.5) +
  labs(x = "Customer.Status",
       y = "Total.Refunds",
       title = "Relationship between Total.Refunds & Customer.Status")

# Total.Extra.Data.Charges vs Customer.Status
ggplot(churnData, aes(x = Customer.Status, y = Total.Extra.Data.Charges, fill = Customer.Status)) +
  geom_violin() +
  geom_boxplot(width = 0.5,
               fill = "Orange",
               outlier.colour = "Orange",
               outlier.size = 1.5) +
  labs(x = "Customer.Status",
       y = "Total.Extra.Data.Charges",
       title = "Relationship between Total.Extra.Data.Charges & Customer.Status")

# Total.Long.Distance.Charges vs Customer.Status
ggplot(churnData, aes(x = Customer.Status, y = Total.Long.Distance.Charges, fill = Customer.Status)) +
  geom_violin() +
  geom_boxplot(width = 0.5,
               fill = "Orange",
               outlier.colour = "Orange",
               outlier.size = 1.5) +
  labs(x = "Customer.Status",
       y = "Total.Long.Distance.Charges",
       title = "Relationship between Total.Long.Distance.Charges & Customer.Status")

# Total.Revenue vs Customer.Status
ggplot(churnData, aes(x = Customer.Status, y = Total.Revenue, fill = Customer.Status)) +
  geom_violin() +
  geom_boxplot(width = 0.5,
               fill = "Orange",
               outlier.colour = "Orange",
               outlier.size = 1.5) +
  labs(x = "Customer.Status",
       y = "Total.Revenue",
       title = "Relationship between Total.Revenue & Customer.Status")



# ||===============================================================================||
# ||                                                                               ||
# ||          Data Visualisation: Identify Churn.Reason-specific Patterns          ||
# ||                                                                               ||
# ||===============================================================================||

# make copy of original data so we do not mess it up
df.eda <- churnData

# create subset of dataset where churn reason is something that can be addressed by offering optimal bundles
actionable.reasons = c('Competitor had better devices', 
                       'Product dissatisfaction', 
                       'Limited range of services', 
                       'Competitor made better offer',
                       'Long distance charges',
                       'Lack of affordable download/upload speed',
                       'Competitor offered more data',
                       'Price too high',
                       'Extra data charges')

df.actionable.churn = subset(churnData, Churn.Reason %in% actionable.reasons)
nrow.actionable = nrow(df.actionable.churn)

# downsample such that number of churn matches number of joined / stayed
df.stayed.downsample = sample_n(subset(churnData, Churn.Reason=='Staying Customer'), nrow.actionable)
df.combine = rbind(df.actionable.churn, df.stayed.downsample)
nrow(df.combine)

# Preprocessing: Feature selection, and change Customer.Status to binary. 1 = churned, 0 = not churned
set.seed(123)
df.combine$Y                    <- factor(ifelse(df.combine$Customer.Status=='Churned', 1, 0))
df.combine$Customer.ID          <- NULL
df.combine$Customer.Status      <- NULL
df.combine$Churn.Reason         <- NULL
df.combine$Churn.Category       <- NULL
df.combine$Number.of.Referrals  <- NULL
df.combine$Offer                <- NULL
df.combine$Payment.Method       <- NULL
df.combine$Paperless.Billing    <- NULL
str(df.combine)

# train a logistic regression model on the subset of data. No need train test split since we just want to find variable importance
model <- glm(Y ~ ., data=df.combine, family='binomial') #control = list(maxit = 100, epsilon = 1)
options(max.print=5000)
summary(model)

# Based on LR model, statistical significant variables in predicting churn:
# - Monthly.Charge
# - Contract
# - Streaming.TV
# - Premium.Tech.Support
# - Online.Security
# - Tenure.in.Months
# - City
# - Number.of.Dependents
# - Age


# relationship between unlimited data and churn reasons related to data ['competitor made better offer', 'competitor offered more data', 'extra data charges']
df.eda$Count <- rep(1, nrow(df.eda))

ggplot(subset(df.eda, Churn.Reason %in% c('Staying Customer', 'New Customer', 'Competitor made better offer', 'Extra data charges', 'Competitor offered more data')), aes(x=Churn.Reason, y=Count, fill=Unlimited.Data)) +
  geom_bar(position = "fill",stat = "identity") +
  labs(x = "Churn.Reason",
       y = "Proportion",
       title = "Proportion of Unlimited.Data for each Churn.Reason")

# relationship between tenure and customer status
ggplot(subset(df.eda, Customer.Status != 'Joined'), aes(x = Customer.Status, y = Tenure.in.Months, fill = Customer.Status)) +
  geom_violin() +
  geom_boxplot(width = 0.1,
               fill = "Orange",
               outlier.colour = "Orange",
               outlier.size = 1.5,
               alpha = 0.3) +
  labs(x = "Customer.Status",
       y = "Tenure.in.Months",
       title = "Relationship between Tenure.in.Months & Customer.Status")


# distribution of usage charge ratio for customers that churned because of "price too high" and customers that didn't churn / churned for other reasons (histogram)
df.eda$Usage.Charge.Ratio <- pmax(df.eda$Monthly.Charge / df.eda$Avg.Monthly.GB.Download, rep(0, nrow(df.eda)))
df.eda$Customer.Status.Temp <- factor(ifelse(df.eda$Churn.Reason=='Price too high', 'Price too high', 'Not Churned / Other reasons'))

ggplot(df.eda, aes(x=log(1+Usage.Charge.Ratio), fill=Customer.Status.Temp)) +
  geom_histogram(aes(y = after_stat(c(
    count[group==1] / sum(count[group==1]),
    count[group==2] / sum(count[group==2])
  ))),
  alpha=.5, 
  position="identity",
  bins=60
  ) +
  scale_fill_manual(values=c('#00BFC4', '#F9766E')) +
  labs(x = "log(1+Usage.Charge.Ratio)",
       y = "Proportion",
       title = "Relationship between Usage.Charge.Ratio & Customer.Status")

# distribution of usage charge ratio for customers that churned because of "price too high" and customers that didn't churn / churned for other reasons (boxplot)
# graph too right skewed, need log transformation
ggplot(df.eda, aes(x = Customer.Status, y = Usage.Charge.Ratio, fill = Customer.Status)) +
  geom_violin() +
  geom_boxplot(width = 0.5,
               fill = "Orange",
               outlier.colour = "Orange",
               outlier.size = 1.5) +
  labs(x = "Customer.Status",
       y = "Usage.Charge.Ratio",
       title = "Relationship between Usage.Charge.Ratio & Customer.Status")


ggplot(df.eda, aes(x=Customer.Status.Temp, y=log(1+Usage.Charge.Ratio), fill = Customer.Status.Temp)) +
  geom_violin() +
  geom_boxplot(width = 0.5,
               fill = "Orange",
               outlier.colour = "Orange",
               outlier.size = 1.5) +
  scale_fill_manual(values=c('#00BFC4', '#F9766E')) +
  labs(x = "Customer.Status",
       y = "Usage.Charge.Ratio",
       title = "Relationship between Usage.Charge.Ratio & Customer.Status")

# distribution of monthly data usage for customers that churned because "competitor offered more data" and customers that didn't churn / churned for other reasons
df.eda$Customer.Status.Temp <- factor(ifelse(churnData$Churn.Reason=='Competitor offered more data', 'Competitor offered more data', 'Not Churned / Other reasons'))

ggplot(df.eda, aes(x = Customer.Status.Temp, y = Avg.Monthly.GB.Download, fill = Customer.Status.Temp)) +
  geom_violin() +
  geom_boxplot(width = 0.5,
               fill = "Orange",
               outlier.colour = "Orange",
               outlier.size = 1.5,
               alpha = 0.3) +
  labs(x = "Customer.Status",
       y = "Avg.Monthly.GB.Download",
       title = "Relationship between Avg.Monthly.GB.Download & Customer.Status")


# Proportion of churn for each of the different service count categories 
df.eda$Customer.Status.Temp <- factor(ifelse(df.eda$Customer.Status=='Churned', 'Churned', 'Not Churned'))

# make a new col Service.Count which is just the number of services the customer has subscribed to
df.eda$Service.Count = factor(
    ifelse(df.eda$Streaming.Music=='Yes', 1, 0) + 
    ifelse(df.eda$Streaming.Movies=='Yes', 1, 0) +
    ifelse(df.eda$Streaming.TV=='Yes', 1, 0) +
    ifelse(df.eda$Premium.Tech.Support=='Yes', 1, 0) +
    ifelse(df.eda$Device.Protection.Plan=='Yes', 1, 0) +
    ifelse(df.eda$Online.Backup=='Yes', 1, 0) +
    ifelse(df.eda$Online.Security=='Yes', 1, 0)
)

ggplot(df.eda, aes(x = Service.Count, y=Count, fill = Customer.Status.Temp)) +
  geom_bar(stat = "identity", position="fill")

# relationship between age and Avg.Monthly.GB.Download for customers that churned and those that did not churn
ggplot(df.eda, aes(x = Age, y = Avg.Monthly.GB.Download)) +
  geom_jitter(aes(color=Customer.Status.Temp), width=0.1, alpha=0.5) +
  scale_color_manual(values=c('Churned'='#F9766E', 'Not Churned'='#00BFC4'))

# distribution of Avg.Monthly.GB.Download for customers of Age<30 that churned and those that did not churn
ggplot(subset(df.eda, Age<30), aes(y=Avg.Monthly.GB.Download, x=Customer.Status.Temp, fill=Customer.Status.Temp)) +
  geom_violin() +
  geom_boxplot(width = 0.5,
               fill = "Orange",
               outlier.colour = "Orange",
               outlier.size = 1.5,
               alpha = 0.3) +
  labs(x = "Customer.Status",
       y = "Avg.Monthly.GB.Download",
       title = "Relationship between Avg.Monthly.GB.Download & Customer.Status for Age<30")

# Number of Dependents against Customer.Status
df.eda$Number.of.Dependents.Bin <- as.numeric(df.eda$Number.of.Dependents)-1
df.eda$Number.of.Dependents.Bin = ifelse(df.eda$Number.of.Dependents.Bin < 3, df.eda$Number.of.Dependents.Bin, '>=3')
ggplot(df.eda, aes(x = factor(Number.of.Dependents.Bin, levels=c('0','1','2', '>=3')), y=Count, fill = Customer.Status)) +
  geom_bar(position = "fill",stat = "identity") +
  labs(x = "Number.of.Dependents",
       y = "Proportion",
       title = "Proportion of Unlimited.Data for each Churn.Reason")
theme_minimal()


# ||===========================================||
# ||                                           ||
# ||          Further Data Processing          ||
# ||                                           ||
# ||===========================================||

# +----------------------------------+
# |    Prelimary bundle assignment   |
# +----------------------------------+
# Setting the Threshold for Avg Monthly Data Usage
quantile(churnData$Avg.Monthly.GB.Download, probs = c(0.25, 0.50, 0.75))
lowDataThreshold  <- quantile(churnData$Avg.Monthly.GB.Download, probs = 0.25)
highDataThreshold <- quantile(churnData$Avg.Monthly.GB.Download, probs = 0.75)

# Setting the Threshold for Avg Monthly Long Distance Charges
quantile(churnData$Avg.Monthly.Long.Distance.Charges, probs = c(0.25, 0.50, 0.75))
lowLongDistanceThreshold  <- quantile(churnData$Avg.Monthly.Long.Distance.Charges, probs = 0.25)
highLongDistanceThreshold <- quantile(churnData$Avg.Monthly.Long.Distance.Charges, probs = 0.75)

# Setting for Threshold for Extra Data Charges
## All 0, which is why we have to fall back on the earlier thresholds
quantile(churnData$Total.Extra.Data.Charges, probs = c(0.25, 0.50, 0.75))

# Setting the Threshold for Total Long Distance Charges
quantile(churnData$Total.Long.Distance.Charges, probs = c(0.25, 0.50, 0.75))
lowTotalLongDistThreshold  <- quantile(churnData$Total.Long.Distance.Charges, probs = 0.25)
highTotalLongDistThreshold <- quantile(churnData$Total.Long.Distance.Charges, probs = 0.75)

# Creating a New Independent Variable, Customer's Assigned Bundle
churnData$Assigned.Bundle <- NA
# Creating 6 Mobile Bundles
levels(churnData$Assigned.Bundle) <- c("Young Adults", 
                                       "Pioneer Generation", 
                                       "Family Bundle", 
                                       "Gen Z Streamers", 
                                       "Frequent Fliers", 
                                       "Average Bundle")

# Initial Number of People without Bundle
sum(is.na(churnData$Assigned.Bundle))

# Pioneer Generation - Discounts due to Age
churnData$Assigned.Bundle[churnData$Age > 60 & churnData$Avg.Monthly.GB.Download < lowDataThreshold] <- "Pioneer Generation"

# Family Bundle
churnData$Assigned.Bundle[churnData$Multiple.Lines == "Yes" & churnData$Number.of.Dependents != "0" & churnData$Married == "Yes"] <- "Family Bundle"

# Gen Z
churnData$Assigned.Bundle[churnData$Avg.Monthly.GB.Download > highDataThreshold & (churnData$Streaming.Movies == "Yes" | churnData$Streaming.Music == "Yes" | churnData$Streaming.TV == "Yes")] <- "Gen Z Streamers"

# Young Adults - Also with Unlimited Data
churnData$Assigned.Bundle[churnData$Avg.Monthly.GB.Download > highDataThreshold & churnData$Avg.Monthly.Long.Distance.Charges < lowLongDistanceThreshold & churnData$Unlimited.Data == "Yes"] <- "Young Adults"

# Frequent Fliers
churnData$Assigned.Bundle[churnData$Total.Long.Distance.Charges > highTotalLongDistThreshold] <- "Frequent Fliers"

# Average Bundle - For now, if no Special Terms
churnData$Assigned.Bundle[is.na(churnData$Assigned.Bundle)] <- "Average Bundle"

# Convert the New Dependent Variable into a Factor Variable
churnData$Assigned.Bundle <- as.factor(churnData$Assigned.Bundle)
# Verify all Customers should have an Mobile Bundle that is Compatible with them!!
sum(is.na(churnData$Assigned.Bundle))


# +--------------------------------------------------+
# |    Re-asigning bundles for Customer who churn    |
# +--------------------------------------------------+

# Create the derived Dependent Variable, Churn
churnData$Churn[churnData$Customer.Status == "Stayed"] <- "Not Churned"
churnData$Churn[churnData$Customer.Status == "Joined"] <- "Not Churned"
churnData$Churn[churnData$Customer.Status == "Churned"] <- "Churned"
churnData$Churn <- as.factor(churnData$Churn)

# Assign Bundles for Churned Customers
levels(churnData$Churn.Reason)

# Better Offer - Should add 311 to YA
sum(churnData$Assigned.Bundle == "Young Adults")
churnData$Assigned.Bundle[churnData$Churn.Reason == "Competitor made better offer"] <- "Young Adults"
sum(churnData$Assigned.Bundle == "Young Adults")

# Higher Download Speeds - should add 100 to Family [added 99 instead]
sum(churnData$Assigned.Bundle == "Family Bundle")
churnData$Assigned.Bundle[churnData$Churn.Reason == "Competitor offered higher download speeds"] <- "Family Bundle"
sum(churnData$Assigned.Bundle == "Family Bundle")

# Competitors offered more Data - Should add 117 to Average {Added less}
sum(churnData$Assigned.Bundle == "Average Bundle")
churnData$Assigned.Bundle[churnData$Churn.Reason == "Competitor offered more data"] <- "Average Bundle"
sum(churnData$Assigned.Bundle == "Average Bundle")

# Extra Data Charges - Should add 39 {Added Less}
sum(churnData$Assigned.Bundle == "Average Bundle")
  churnData$Assigned.Bundle[churnData$Churn.Reason == "Extra data charges"] <- "Average Bundle"
sum(churnData$Assigned.Bundle == "Average Bundle")

# Lack of Affordable Upload/Downloads - Should add 30
sum(churnData$Assigned.Bundle == "Pioneer Generation")
churnData$Assigned.Bundle[churnData$Churn.Reason == "Lack of affordable download/upload speed"] <- "Pioneer Generation"
sum(churnData$Assigned.Bundle == "Pioneer Generation")

# Long Distance Charges - Should add 64
sum(churnData$Assigned.Bundle == "Gen Z Streamers")
churnData$Assigned.Bundle[churnData$Churn.Reason == "Long distance charges"] <- "Gen Z Streamers"
sum(churnData$Assigned.Bundle == "Gen Z Streamers")

# Price too High - Should add 78
sum(churnData$Assigned.Bundle == "Frequent Fliers")
churnData$Assigned.Bundle[churnData$Churn.Reason == "Price too high"] <- "Frequent Fliers"
sum(churnData$Assigned.Bundle == "Frequent Fliers")

table(churnData$Assigned.Bundle)


# ||===========================================||
# ||                                           ||
# ||          Machine Learning Models          ||
# ||                                           ||
# ||===========================================||

# Final data pre-processing
set.seed(4)

churnData2 <- churnData

churnData2$City <- NULL

# Remove Whitespace
names(churnData2) <- make.names(names(churnData2))
churnData2$Churn <- ifelse(churnData2$Customer.Status=='Churned', 1, 0) # stayed / joined -> 0, churned -> 1. Churn is our label.

customerID <- churnData2$Customer.ID # store customer ID first so we can re-attach in the final test dataframe

# prevent look-ahead bias
churnData2$Customer.Status     <- NULL
churnData2$Churn.Category      <- NULL
churnData2$Churn.Reason        <- NULL
churnData2$Customer.ID         <- NULL

# irrelevant
churnData2$Offer               <- NULL  
churnData2$Payment.Method      <- NULL    
churnData2$Paperless.Billing   <- NULL
churnData2$Number.of.Referrals <- NULL

# 2-factor to binary
churnData2$Male                 <- ifelse(churnData2$Gender=='Male', 1, 0) # change from "Gender" to "Male" for better readibility
churnData2$Gender               <- NULL # drop "Gender" since we have new column "Male"
churnData2$Married              <- ifelse(churnData2$Married             =='Yes', 1, 0)
churnData2$Streaming.Movies     <- ifelse(churnData2$Streaming.Movies    =='Yes', 1, 0)
churnData2$Streaming.Music      <- ifelse(churnData2$Streaming.Music     =='Yes', 1, 0)
churnData2$Streaming.TV         <- ifelse(churnData2$Streaming.TV        =='Yes', 1, 0)
churnData2$Unlimited.Data       <- ifelse(churnData2$Unlimited.Data      =='Yes', 1, 0)
churnData2$Premium.Tech.Support <- ifelse(churnData2$Premium.Tech.Support=='Yes', 1, 0)
churnData2$Internet.Service     <- ifelse(churnData2$Internet.Service    =='Yes', 1, 0)
churnData2$Multiple.Lines       <- ifelse(churnData2$Multiple.Lines      =='Yes', 1, 0)
churnData2$Phone.Service        <- ifelse(churnData2$Phone.Service       =='Yes', 1, 0)

# Prerequisite of SMOTE is a numeric dataset, so categorical must be converted to numbers. 
# This can be done via one-hot encoding
bundles <- churnData2$Assigned.Bundle # store the Assigned.Bundle column first before removing it from the dataframe to be over sampled
churnData2.no.bundle <- churnData2[, !names(churnData2) %in% c('Assigned.Bundle')] # dataframe w/o Assigned.Bundle column so that the Assigned.Bundle column does not get encoded

# one-hot encoding
dummy <- dummyVars(" ~ .", data=churnData2.no.bundle)
churnData2 <- as.data.frame(predict(dummy, newdata=churnData2.no.bundle))
churnData2$Assigned.Bundle <- bundles # insert Assigned.Bundle back into churnData2 after one-hot encoding

names(churnData2) <- make.names(names(churnData2))
churnData2 <- cbind(customerID, churnData2)

# Train-Test Split; list = F to return a vector of indices
trainIndex <- createDataPartition(churnData2$Assigned.Bundle, p = 0.7, list = FALSE)
trainChurnData <- churnData2[trainIndex, ]
testChurnData <- churnData2[-trainIndex, ]

str(trainChurnData)

# subset train set by Assigned.Bundle
trainChurnData.average <- subset(trainChurnData, Assigned.Bundle=='Average Bundle')
trainChurnData.family  <- subset(trainChurnData, Assigned.Bundle=='Family Bundle')
trainChurnData.fliers  <- subset(trainChurnData, Assigned.Bundle=='Frequent Fliers')
trainChurnData.genz    <- subset(trainChurnData, Assigned.Bundle=='Gen Z Streamers')
trainChurnData.pioneer <- subset(trainChurnData, Assigned.Bundle=='Pioneer Generation')
trainChurnData.adults  <- subset(trainChurnData, Assigned.Bundle=='Young Adults')

# stash away customer IDs first so we can drop them during training and testing, then re-attach during demo
avgCustomerID         <- trainChurnData.average$customerID
familyCustomerID      <- trainChurnData.family$customerID
fliersCustomerID      <- trainChurnData.fliers$customerID
genzCustomerID        <- trainChurnData.genz$customerID
pioneerCustomerID     <- trainChurnData.pioneer$customerID
youngAdultsCustomerID <- trainChurnData.adults$customerID

trainChurnData.average$customerID <- NULL
trainChurnData.family$customerID  <- NULL
trainChurnData.fliers$customerID  <- NULL
trainChurnData.genz$customerID    <- NULL
trainChurnData.pioneer$customerID <- NULL
trainChurnData.adults$customerID  <- NULL

# over-sample using SMOTE
averageTrainChurnData     <- SMOTE(X=trainChurnData.average[, !names(trainChurnData.average) %in% c('Assigned.Bundle')], target=trainChurnData.average$Churn)$data
familyTrainChurnData      <- SMOTE(X=trainChurnData.family[, !names(trainChurnData.family) %in% c('Assigned.Bundle')], target=trainChurnData.family$Churn)$data
fliersTrainChurnData      <- SMOTE(X=trainChurnData.fliers[, !names(trainChurnData.fliers) %in% c('Assigned.Bundle')], target=trainChurnData.fliers$Churn)$data
genZTrainChurnData        <- SMOTE(X=trainChurnData.genz[, !names(trainChurnData.genz) %in% c('Assigned.Bundle')], target=trainChurnData.genz$Churn)$data
pioneerTrainChurnData     <- SMOTE(X=trainChurnData.pioneer[, !names(trainChurnData.pioneer) %in% c('Assigned.Bundle')], target=trainChurnData.pioneer$Churn)$data
youngAdultsTrainChurnData <- SMOTE(X=trainChurnData.adults[, !names(trainChurnData.adults) %in% c('Assigned.Bundle')], target=trainChurnData.adults$Churn)$data

# remove automatically-generated column "class" from SMOTE-ing
averageTrainChurnData$class     <- NULL
familyTrainChurnData$class      <- NULL
fliersTrainChurnData$class      <- NULL
genZTrainChurnData$class        <- NULL
pioneerTrainChurnData$class     <- NULL
youngAdultsTrainChurnData$class <- NULL

# Subset test set by Assigned.Bundle
averageTestChurnData     <- subset(testChurnData, Assigned.Bundle == "Average Bundle")
familyTestChurnData      <- subset(testChurnData, Assigned.Bundle == "Family Bundle")
fliersTestChurnData      <- subset(testChurnData, Assigned.Bundle == "Frequent Fliers")
genZTestChurnData        <- subset(testChurnData, Assigned.Bundle == "Gen Z Streamers")
pioneerTestChurnData     <- subset(testChurnData, Assigned.Bundle == "Pioneer Generation")
youngAdultsTestChurnData <- subset(testChurnData, Assigned.Bundle == "Young Adults")

# Making Churn a Factor for RF
averageTrainChurnData$Churn     <- factor(averageTrainChurnData$Churn)
familyTrainChurnData$Churn      <- factor(familyTrainChurnData$Churn)
fliersTrainChurnData$Churn      <- factor(fliersTrainChurnData$Churn)
genZTrainChurnData$Churn        <- factor(genZTrainChurnData$Churn)
pioneerTrainChurnData$Churn     <- factor(pioneerTrainChurnData$Churn)
youngAdultsTrainChurnData$Churn <- factor(youngAdultsTrainChurnData$Churn)

averageTestChurnData$Churn      <- factor(averageTestChurnData$Churn)
familyTestChurnData$Churn       <- factor(familyTestChurnData$Churn)
fliersTestChurnData$Churn       <- factor(fliersTestChurnData$Churn)
genZTestChurnData$Churn         <- factor(genZTestChurnData$Churn)
pioneerTestChurnData$Churn      <- factor(pioneerTestChurnData$Churn)
youngAdultsTestChurnData$Churn  <- factor(youngAdultsTestChurnData$Churn)


# ||===========================================================||
# ||                                                           ||
# ||          Machine Learning Model 1: Random Forest          ||
# ||                                                           ||
# ||===========================================================||

# +----------------------+
# |    Average bundle    |
# +----------------------+
# Fit the Random Forest Model to Predict Churn
rfModelAvg <- randomForest(Churn ~ ., data = averageTrainChurnData, importance = TRUE,
                           ntree = 500, mtry = 2)
# Identify the Churn Probability
predict(rfModelAvg, newdata = averageTestChurnData, type = "prob")[,]
# Make Predictions and Obtain the Probability of Churning
averageTestChurnData$ProbChurn <- predict(rfModelAvg, newdata = averageTestChurnData, type = "prob")[, 2] * 100
rfModelAvg.predict <- predict(rfModelAvg, newdata = averageTestChurnData)

# Verify & view the probability of churning for the first 10 customers
head(select(averageTestChurnData, Churn, ProbChurn), 10)

# Results
summary(rfModelAvg)
# Variable Importance of Random Forest
rf.var.impt <- importance(rfModelAvg)
varImpPlot(rfModelAvg)
# Evaluate Performance of Model
confusionMatrix(rfModelAvg.predict, averageTestChurnData$Churn)
rfModelAvg.accuracy <- mean(averageTestChurnData$Churn == rfModelAvg.predict)
cat("Accuracy of Random Forest (No Feature Selection)", rfModelAvg.accuracy * 100, "%.\n")

# +---------------------+
# |    Family bundle    |
# +---------------------+
# Fit the Random Forest Model to Predict Churn
rfModelFamily <- randomForest(Churn ~ ., data = familyTrainChurnData, importance = TRUE,
                           ntree = 500, mtry = 2)
# Identify the Churn Probability
predict(rfModelFamily, newdata = familyTestChurnData, type = "prob")[,]
# Make Predictions and Obtain the Probability of Churning
familyTestChurnData$ProbChurn <- predict(rfModelFamily, newdata = familyTestChurnData, type = "prob")[, 2] * 100
rfModelFamily.predict <- predict(rfModelFamily, newdata = familyTestChurnData)
# Verify & view the probability of churning for the first 10 customers
head(select(familyTestChurnData, Churn, ProbChurn), 10)

# Results
summary(rfModelFamily)
# Variable Importance of Random Forest
rf.var.impt <- importance(rfModelFamily)
varImpPlot(rfModelFamily)
# Evaluate Performance of Model
confusionMatrix(rfModelFamily.predict, familyTestChurnData$Churn)
rfModelFamily.accuracy <- mean(familyTestChurnData$Churn == rfModelFamily.predict)
cat("Accuracy of Random Forest (No Feature Selection)", rfModelFamily.accuracy * 100, "%.\n")

# +-----------------------+
# |    Frequent fliers    |
# +-----------------------+
# Fit the Random Forest Model to Predict Churn
rfModelFliers <- randomForest(Churn ~ ., data = fliersTrainChurnData, importance = TRUE,
                              ntree = 500, mtry = 2)
# Identify the Churn Probability
predict(rfModelFliers, newdata = fliersTestChurnData, type = "prob")[,]
# Make Predictions and Obtain the Probability of Churning
fliersTestChurnData$ProbChurn <- predict(rfModelFliers, newdata = fliersTestChurnData, type = "prob")[, 2] * 100
rfModelFliers.predict <- predict(rfModelFliers, newdata = fliersTestChurnData)
# Verify & view the probability of churning for the first 10 customers
head(select(fliersTestChurnData, Churn, ProbChurn), 10)

# Results
summary(rfModelFliers)
# Variable Importance of Random Forest
rf.var.impt <- importance(rfModelFliers)
varImpPlot(rfModelFliers)
# Evaluate Performance of Model
confusionMatrix(rfModelFliers.predict, fliersTestChurnData$Churn)
rfModelFliers.accuracy <- mean(fliersTestChurnData$Churn == rfModelFliers.predict)
cat("Accuracy of Random Forest (No Feature Selection)", rfModelFliers.accuracy * 100, "%.\n")

# +-----------------------+
# |    Gen Z streamers    |
# +-----------------------+
# Fit the Random Forest Model to Predict Churn
rfModelGenZ <- randomForest(Churn ~ ., data = genZTrainChurnData, importance = TRUE,
                              ntree = 500, mtry = 2)
# Identify the Churn Probability
predict(rfModelGenZ, newdata = genZTestChurnData, type = "prob")[,]
# Make Predictions and Obtain the Probability of Churning
genZTestChurnData$ProbChurn <- predict(rfModelGenZ, newdata = genZTestChurnData, type = "prob")[, 2] * 100
rfModelGenZ.predict <- predict(rfModelGenZ, newdata = genZTestChurnData)
# Verify & view the probability of churning for the first 10 customers
head(select(genZTestChurnData, Churn, ProbChurn), 10)

# Results
summary(rfModelGenZ)
# Variable Importance of Random Forest
rf.var.impt <- importance(rfModelGenZ)
varImpPlot(rfModelGenZ)
# Evaluate Performance of Model
confusionMatrix(rfModelGenZ.predict, genZTestChurnData$Churn)
rfModelGenZ.accuracy <- mean(genZTestChurnData$Churn == rfModelGenZ.predict)
cat("Accuracy of Random Forest (No Feature Selection)", rfModelGenZ.accuracy * 100, "%.\n")

# +--------------------------+
# |    Pioneer generation    |
# +--------------------------+
# Fit the Random Forest Model to Predict Churn
rfModelPioneer <- randomForest(Churn ~ ., data = pioneerTrainChurnData, importance = TRUE,
                            ntree = 500, mtry = 2)
# Identify the Churn Probability
predict(rfModelPioneer, newdata = pioneerTestChurnData, type = "prob")[,]
# Make Predictions and Obtain the Probability of Churning
pioneerTestChurnData$ProbChurn <- predict(rfModelPioneer, newdata = pioneerTestChurnData, type = "prob")[, 2] * 100
rfModelPioneer.predict <- predict(rfModelPioneer, newdata = pioneerTestChurnData)
# Verify & view the probability of churning for the first 10 customers
head(select(pioneerTestChurnData, Churn, ProbChurn), 10)

# Results
summary(rfModelPioneer)
# Variable Importance of Random Forest
rf.var.impt <- importance(rfModelPioneer)
varImpPlot(rfModelPioneer)
# Evaluate Performance of Model
confusionMatrix(rfModelPioneer.predict, pioneerTestChurnData$Churn)
rfModelPioneer.accuracy <- mean(pioneerTestChurnData$Churn == rfModelPioneer.predict)
cat("Accuracy of Random Forest (No Feature Selection)", rfModelPioneer.accuracy * 100, "%.\n")

# +------------------+
# |   Young Adults   |
# +------------------+
# Fit the Random Forest Model to Predict Churn
rfModelYoungAdults <- randomForest(Churn ~ ., data = youngAdultsTrainChurnData, importance = TRUE,
                               ntree = 500, mtry = 2)
# Identify the Churn Probability
predict(rfModelYoungAdults, newdata = youngAdultsTestChurnData, type = "prob")[,]
# Make Predictions and Obtain the Probability of Churning
youngAdultsTestChurnData$ProbChurn <- predict(rfModelYoungAdults, newdata = youngAdultsTestChurnData, type = "prob")[, 2] * 100
rfModelYoungAdults.predict <- predict(rfModelYoungAdults, newdata = youngAdultsTestChurnData)
# Verify & view the probability of churning for the first 10 customers
head(select(youngAdultsTestChurnData, Churn, ProbChurn), 10)

# Results
summary(rfModelYoungAdults)
# Variable Importance of Random Forest
rf.var.impt <- importance(rfModelYoungAdults)
varImpPlot(rfModelYoungAdults)
# Evaluate Performance of Model
confusionMatrix(rfModelYoungAdults.predict, youngAdultsTestChurnData$Churn)
rfModelYoungAdults.accuracy <- mean(youngAdultsTestChurnData$Churn == rfModelYoungAdults.predict)
cat("Accuracy of Random Forest (No Feature Selection)", rfModelYoungAdults.accuracy * 100, "%.\n")

# Determine the Number of People whose Churn Probs > 80%
churnThreshold   <- 80
rfModelAvg.numChurn <- length(averageTestChurnData$ProbChurn[averageTestChurnData$ProbChurn > churnThreshold])
rfModelAvg.size <- nrow(averageTestChurnData)
rfModelAvg.percentageChurn <- (rfModelAvg.numChurn/rfModelAvg.size) * 100

rfModelFamily.numChurn <- length(familyTestChurnData$ProbChurn[familyTestChurnData$ProbChurn > churnThreshold])
rfModelFamily.size <- nrow(familyTestChurnData)
rfModelFamily.percentageChurn <- (rfModelFamily.numChurn/rfModelFamily.size) * 100

rfModelFliers.numChurn <- length(fliersTestChurnData$ProbChurn[fliersTestChurnData$ProbChurn > churnThreshold])
rfModelFliers.size <- nrow(fliersTestChurnData)
rfModelFliers.percentageChurn <- (rfModelFliers.numChurn/rfModelFliers.size) * 100

rfModelGenZ.numChurn <- length(genZTestChurnData$ProbChurn[genZTestChurnData$ProbChurn > churnThreshold])
rfModelGenZ.size <- nrow(genZTestChurnData)
rfModelGenZ.percentageChurn <- (rfModelGenZ.numChurn/rfModelGenZ.size) * 100

rfModelPioneer.numChurn <- length(pioneerTestChurnData$ProbChurn[pioneerTestChurnData$ProbChurn > churnThreshold])
rfModelPioneer.size <- nrow(pioneerTestChurnData)
rfModelPioneer.percentageChurn <- (rfModelPioneer.numChurn/rfModelPioneer.size) * 100

rfModelYoungAdults.numChurn <- length(youngAdultsTestChurnData$ProbChurn[youngAdultsTestChurnData$ProbChurn > churnThreshold])
rfModelYoungAdults.size <- nrow(unique(youngAdultsTestChurnData))
rfModelYoungAdults.percentageChurn <- (rfModelYoungAdults.numChurn/rfModelYoungAdults.size) * 100

# Table for Accuracies
modelNames <- c('rfModelAvg', 'rfModelFamily', 'rfModelFliers', 'rfModelGenZ', 'rfModelPioneer', 'rfModelYoungAdults')
modelAccuracies <- c(rfModelAvg.accuracy, rfModelFamily.accuracy, rfModelFliers.accuracy, rfModelGenZ.accuracy, rfModelPioneer.accuracy, rfModelYoungAdults.accuracy)
modelChurnNums <- c(rfModelAvg.numChurn, rfModelFamily.numChurn, rfModelFliers.numChurn, rfModelGenZ.numChurn, rfModelPioneer.numChurn, rfModelYoungAdults.numChurn)
modelSize <- c(rfModelAvg.size, rfModelFamily.size, rfModelFliers.size, rfModelGenZ.size, rfModelPioneer.size, rfModelYoungAdults.size)
modelPercentage <- c(rfModelAvg.percentageChurn, rfModelFamily.percentageChurn, rfModelFliers.percentageChurn, rfModelGenZ.percentageChurn, rfModelPioneer.percentageChurn, rfModelYoungAdults.percentageChurn)

accuracyRFTable <- data.frame(Model = modelNames, Accuracy = modelAccuracies, ChurnNumbers = modelChurnNums, TestSize = modelSize, ChurnPercentage = modelPercentage)
accuracyRFTable

# Subset the People who churned from all the 6 Test Datasets
churnAvg         <- averageTestChurnData[averageTestChurnData$ProbChurn         > churnThreshold, ]
churnFamily      <- familyTestChurnData[familyTestChurnData$ProbChurn           > churnThreshold, ]
churnFliers      <- fliersTestChurnData[fliersTestChurnData$ProbChurn           > churnThreshold, ]
churnGenZ        <- genZTestChurnData[genZTestChurnData$ProbChurn               > churnThreshold, ]
churnPioneer     <- pioneerTestChurnData[pioneerTestChurnData$ProbChurn         > churnThreshold, ]
churnYoungAdults <- youngAdultsTestChurnData[youngAdultsTestChurnData$ProbChurn > churnThreshold, ]

# Combine the Subsets into 1 Dataset
combinedChurnOnly <- rbind(churnAvg, churnFamily, churnFliers, churnGenZ, churnPioneer, churnYoungAdults)

## Run it for the other 5 Models
# +----------------------+
# |    Average bundle    |
# +----------------------+
predict(rfModelAvg, newdata = combinedChurnOnly, type = "prob")[,]
# Make Predictions and Obtain the Probability of Churning
combinedChurnOnly$AverageProbChurn <- predict(rfModelAvg, newdata = combinedChurnOnly, type = "prob")[, 2] * 100
rfModelAvg.predict2 <- predict(rfModelAvg, newdata = combinedChurnOnly)
# Evaluate Performance of Model
confusionMatrix(rfModelAvg.predict2, combinedChurnOnly$Churn)
rfModelAvg.accuracy2 <- mean(combinedChurnOnly$Churn == rfModelAvg.predict2)
cat("Accuracy: ", rfModelAvg.accuracy2 * 100, "%.\n")

# +---------------------+
# |    Family bundle    |
# +---------------------+
predict(rfModelFamily, newdata = combinedChurnOnly, type = "prob")[,]
# Make Predictions and Obtain the Probability of Churning
combinedChurnOnly$FamilyProbChurn <- predict(rfModelFamily, newdata = combinedChurnOnly, type = "prob")[, 2] * 100
rfModelFamily.predict2 <- predict(rfModelFamily, newdata = combinedChurnOnly)
# Evaluate Performance of Model
confusionMatrix(rfModelFamily.predict2, combinedChurnOnly$Churn)
rfModelFamily.accuracy2 <- mean(combinedChurnOnly$Churn == rfModelFamily.predict2)
cat("Accuracy: ", rfModelFamily.accuracy2 * 100, "%.\n")

# +-----------------------+
# |    Frequent fliers    |
# +-----------------------+
predict(rfModelFliers, newdata = combinedChurnOnly, type = "prob")[,]
# Make Predictions and Obtain the Probability of Churning
combinedChurnOnly$FliersProbChurn <- predict(rfModelFliers, newdata = combinedChurnOnly, type = "prob")[, 2] * 100
rfModelFliers.predict2 <- predict(rfModelFliers, newdata = combinedChurnOnly)
# Evaluate Performance of Model
confusionMatrix(rfModelFliers.predict2, combinedChurnOnly$Churn)
rfModelFliers.accuracy2 <- mean(combinedChurnOnly$Churn == rfModelFliers.predict2)
cat("Accuracy: ", rfModelFliers.accuracy2 * 100, "%.\n")

# +-----------------------+
# |    Gen Z streamers    |
# +-----------------------+
predict(rfModelGenZ, newdata = combinedChurnOnly, type = "prob")[,]
# Make Predictions and Obtain the Probability of Churning
combinedChurnOnly$GenZProbChurn <- predict(rfModelGenZ, newdata = combinedChurnOnly, type = "prob")[, 2] * 100
rfModelGenZ.predict2 <- predict(rfModelGenZ, newdata = combinedChurnOnly)
# Evaluate Performance of Model
confusionMatrix(rfModelGenZ.predict2, combinedChurnOnly$Churn)
rfModelGenZ.accuracy2 <- mean(combinedChurnOnly$Churn == rfModelGenZ.predict2)
cat("Accuracy: ", rfModelGenZ.accuracy2 * 100, "%.\n")

# +--------------------------+
# |    Pioneer generation    |
# +--------------------------+
predict(rfModelPioneer, newdata = combinedChurnOnly, type = "prob")[,]
# Make Predictions and Obtain the Probability of Churning
combinedChurnOnly$PioneerProbChurn <- predict(rfModelPioneer, newdata = combinedChurnOnly, type = "prob")[, 2] * 100
rfModelPioneer.predict2 <- predict(rfModelPioneer, newdata = combinedChurnOnly)
# Evaluate Performance of Model
confusionMatrix(rfModelPioneer.predict2, combinedChurnOnly$Churn)
rfModelPioneer.accuracy2 <- mean(combinedChurnOnly$Churn == rfModelPioneer.predict2)
cat("Accuracy: ", rfModelPioneer.accuracy2 * 100, "%.\n")

# +--------------------+
# |    Young adults    |
# +--------------------+
predict(rfModelYoungAdults, newdata = combinedChurnOnly, type = "prob")[,]
# Make Predictions and Obtain the Probability of Churning
combinedChurnOnly$YoungAdultsProbChurn <- predict(rfModelYoungAdults, newdata = combinedChurnOnly, type = "prob")[, 2] * 100
rfModelYoungAdults.predict2 <- predict(rfModelYoungAdults, newdata = combinedChurnOnly)
# Evaluate Performance of Model
confusionMatrix(rfModelYoungAdults.predict2, combinedChurnOnly$Churn)
rfModelYoungAdults.accuracy2 <- mean(combinedChurnOnly$Churn == rfModelYoungAdults.predict2)
cat("Accuracy: ", rfModelYoungAdults.accuracy2 * 100, "%.\n")

modelAccuracy2 <- c(rfModelAvg.accuracy2, rfModelFamily.accuracy2, rfModelFliers.accuracy2, rfModelGenZ.accuracy2, rfModelPioneer.accuracy2, rfModelYoungAdults.accuracy2)
subsettedCombinedChurnOnly <- subset(combinedChurnOnly, select = c("ProbChurn", "AverageProbChurn", "FamilyProbChurn", "FliersProbChurn", "GenZProbChurn", "PioneerProbChurn", "YoungAdultsProbChurn"))

# Finding Better Bundles based on lowest Churn Probs
minCol <- apply(subsettedCombinedChurnOnly, 1, function(x) {
  names(subsettedCombinedChurnOnly)[which.min(x)]
})

# Add Recommendation to Table
subsettedCombinedChurnOnly$Recommended_Bundle <- minCol
subsettedCombinedChurnOnly
table(subsettedCombinedChurnOnly$Recommended_Bundle)

# Get original bundle
OriginalBundle <- c(rep('Average Bundle',     nrow(churnAvg)),
                    rep('Family Bundle',      nrow(churnFamily)),
                    rep('Frequent Fliers',    nrow(churnFliers)),
                    rep('Gen Z Streamers',    nrow(churnGenZ)),
                    rep('Pioneer Generation', nrow(churnPioneer)),
                    rep('Young Adults',       nrow(churnYoungAdults)))


# verify that number of rows match
length(OriginalBundle) == nrow(subsettedCombinedChurnOnly)
                         
## Append the columns together into a new table
accuracyRFTable2 <- cbind(CustomerID = combinedChurnOnly$customerID, OriginalBundle, subsettedCombinedChurnOnly)

# Verify Correct Customer ID
nrow(accuracyRFTable2) == nrow(subsettedCombinedChurnOnly)
accuracyRFTable2

table(accuracyRFTable2$Recommended_Bundle)

# Export to CSV
write.csv(accuracyRFTable2, "accuracyRFTable2.csv", row.names = FALSE)



# ||=================================================================||
# ||                                                                 ||
# ||          Machine Learning Model 2: Logistic Regression          ||
# ||                                                                 ||
# ||=================================================================||

set.seed(123)

# +----------------------+
# |    Average bundle    |
# +----------------------+
cv <- cv.glmnet(x=as.matrix(averageTrainChurnData[, !names(averageTrainChurnData) %in% c('Churn')]), y=averageTrainChurnData$Churn, family='binomial', alpha=1)
model.average <- glmnet(x=averageTrainChurnData[, !names(averageTrainChurnData) %in% c('Churn')], y=averageTrainChurnData$Churn, family='binomial', alpha=1, lambda=cv$lambda.1se)

coef.df <- data.frame(rownames(coef(model.average)), as.vector(coef(model.average)[, 1]))
colnames(coef.df) <- c('Variable', 'Coefficient')
coef.df <- subset(coef.df, Coefficient != 0 & Variable != '(Intercept)')
coef.df <- arrange(coef.df, desc(Coefficient))

ggplot(data=coef.df, aes(x=factor(Variable, levels=Variable), y=Coefficient)) + 
  geom_bar(stat='identity') +
  coord_flip() +
  labs(title='Variable Coefficients', y='Coefficient', x='Variable')

# +---------------------+
# |    Family bundle    |
# +---------------------+
cv <- cv.glmnet(x=as.matrix(familyTrainChurnData[, !names(familyTrainChurnData) %in% c('Churn')]), y=familyTrainChurnData$Churn, family='binomial', alpha=1)
model.family <- glmnet(x=familyTrainChurnData[, !names(familyTrainChurnData) %in% c('Churn')], y=familyTrainChurnData$Churn, family='binomial', alpha=1, lambda=cv$lambda.1se)

coef.df <- data.frame(rownames(coef(model.family)), as.vector(coef(model.family)[, 1]))
colnames(coef.df) <- c('Variable', 'Coefficient')
coef.df <- subset(coef.df, Coefficient != 0 & Variable != '(Intercept)')
coef.df <- arrange(coef.df, desc(Coefficient))

ggplot(data=coef.df, aes(x=factor(Variable, levels=Variable), y=Coefficient)) + 
  geom_bar(stat='identity') +
  coord_flip() +
  labs(title='Variable Coefficients', y='Coefficient', x='Variable')

# +-----------------------+
# |    Frequent fliers    |
# +-----------------------+
cv <- cv.glmnet(x=as.matrix(fliersTrainChurnData[, !names(fliersTrainChurnData) %in% c('Churn')]), y=fliersTrainChurnData$Churn, family='binomial', alpha=1)
model.fliers <- glmnet(x=fliersTrainChurnData[, !names(fliersTrainChurnData) %in% c('Churn')], y=fliersTrainChurnData$Churn, family='binomial', alpha=1, lambda=cv$lambda.1se)

coef.df <- data.frame(rownames(coef(model.fliers)), as.vector(coef(model.fliers)[, 1]))
colnames(coef.df) <- c('Variable', 'Coefficient')
coef.df <- subset(coef.df, Coefficient != 0 & Variable != '(Intercept)')
coef.df <- arrange(coef.df, desc(Coefficient))

ggplot(data=coef.df, aes(x=factor(Variable, levels=Variable), y=Coefficient)) + 
  geom_bar(stat='identity') +
  coord_flip() +
  labs(title='Variable Coefficients', y='Coefficient', x='Variable')

# +-----------------------+
# |    Gen Z streamers    |
# +-----------------------+
cv <- cv.glmnet(x=as.matrix(genZTrainChurnData[, !names(genZTrainChurnData) %in% c('Churn')]), y=genZTrainChurnData$Churn, family='binomial', alpha=1)
model.genz <- glmnet(x=genZTrainChurnData[, !names(genZTrainChurnData) %in% c('Churn')], y=genZTrainChurnData$Churn, family='binomial', alpha=1, lambda=cv$lambda.1se)

coef.df <- data.frame(rownames(coef(model.genz)), as.vector(coef(model.genz)[, 1]))
colnames(coef.df) <- c('Variable', 'Coefficient')
coef.df <- subset(coef.df, Coefficient != 0 & Variable != '(Intercept)')
coef.df <- arrange(coef.df, desc(Coefficient))

ggplot(data=coef.df, aes(x=factor(Variable, levels=Variable), y=Coefficient)) + 
  geom_bar(stat='identity') +
  coord_flip() +
  labs(title='Variable Coefficients', y='Coefficient', x='Variable')

# +--------------------------+
# |    Pioneer Generation    |
# +--------------------------+
cv <- cv.glmnet(x=as.matrix(pioneerTrainChurnData[, !names(pioneerTrainChurnData) %in% c('Churn')]), y=pioneerTrainChurnData$Churn, family='binomial', alpha=1)
model.pioneer <- glmnet(x=pioneerTrainChurnData[, !names(pioneerTrainChurnData) %in% c('Churn')], y=pioneerTrainChurnData$Churn, family='binomial', alpha=1, lambda=cv$lambda.1se)

coef.df <- data.frame(rownames(coef(model.pioneer)), as.vector(coef(model.pioneer)[, 1]))
colnames(coef.df) <- c('Variable', 'Coefficient')
coef.df <- subset(coef.df, Coefficient != 0 & Variable != '(Intercept)')
coef.df <- arrange(coef.df, desc(Coefficient))

ggplot(data=coef.df, aes(x=factor(Variable, levels=Variable), y=Coefficient)) + 
  geom_bar(stat='identity') +
  coord_flip() +
  labs(title='Variable Coefficients', y='Coefficient', x='Variable')

# +-------------------+
# |   Young Adults    |
# +-------------------+
cv <- cv.glmnet(x=as.matrix(youngAdultsTrainChurnData[, !names(youngAdultsTrainChurnData) %in% c('Churn')]), y=youngAdultsTrainChurnData$Churn, family='binomial', alpha=1)
model.adults <- glmnet(x=youngAdultsTrainChurnData[, !names(youngAdultsTrainChurnData) %in% c('Churn')], y=youngAdultsTrainChurnData$Churn, family='binomial', alpha=1, lambda=cv$lambda.1se)

coef.df <- data.frame(rownames(coef(model.adults)), as.vector(coef(model.adults)[, 1]))
colnames(coef.df) <- c('Variable', 'Coefficient')
coef.df <- subset(coef.df, Coefficient != 0 & Variable != '(Intercept)')
coef.df <- arrange(coef.df, desc(Coefficient))

ggplot(data=coef.df, aes(x=factor(Variable, levels=Variable), y=Coefficient)) + 
  geom_bar(stat='identity') +
  coord_flip() +
  labs(title='Variable Coefficients', y='Coefficient', x='Variable')


# +-----------------------+
# |   Model Evaluation    |
# +-----------------------+
threshold  <- 0.5
accuracies <- rep(0, 6)
models     <- c('Average Bundle', 
                'Family Bundle', 
                'Frequent Fliers', 
                'Gen Z Streamers', 
                'Pioneer', 
                'Young Adults')



averageTestChurnData$Assigned.Bundle     <- NULL
familyTestChurnData$Assigned.Bundle      <- NULL
fliersTestChurnData$Assigned.Bundle      <- NULL
genZTestChurnData$Assigned.Bundle        <- NULL
pioneerTestChurnData$Assigned.Bundle     <- NULL
youngAdultsTestChurnData$Assigned.Bundle <- NULL

averageTestChurnData$ProbChurn     <- predict(model.average, newx=as.matrix(averageTestChurnData[,     !names(averageTestChurnData)     %in% c('Churn', 'customerID', 'ProbChurn')]), type='response')
familyTestChurnData$ProbChurn      <- predict(model.family,  newx=as.matrix(familyTestChurnData[,      !names(familyTestChurnData)      %in% c('Churn', 'customerID', 'ProbChurn')]), type='response')
fliersTestChurnData$ProbChurn      <- predict(model.fliers,  newx=as.matrix(fliersTestChurnData[,      !names(fliersTestChurnData)      %in% c('Churn', 'customerID', 'ProbChurn')]), type='response')
genZTestChurnData$ProbChurn        <- predict(model.genz,    newx=as.matrix(genZTestChurnData[,        !names(genZTestChurnData)        %in% c('Churn', 'customerID', 'ProbChurn')]), type='response')
pioneerTestChurnData$ProbChurn     <- predict(model.pioneer, newx=as.matrix(pioneerTestChurnData[,     !names(pioneerTestChurnData)     %in% c('Churn', 'customerID', 'ProbChurn')]), type='response')
youngAdultsTestChurnData$ProbChurn <- predict(model.adults,  newx=as.matrix(youngAdultsTestChurnData[, !names(youngAdultsTestChurnData) %in% c('Churn', 'customerID', 'ProbChurn')]), type='response')

churnThreshold   <- 0.8
churnAvg         <- averageTestChurnData[averageTestChurnData$ProbChurn         > churnThreshold, ]
churnFamily      <- familyTestChurnData[familyTestChurnData$ProbChurn           > churnThreshold, ]
churnFliers      <- fliersTestChurnData[fliersTestChurnData$ProbChurn           > churnThreshold, ]
churnGenZ        <- genZTestChurnData[genZTestChurnData$ProbChurn               > churnThreshold, ]
churnPioneer     <- pioneerTestChurnData[pioneerTestChurnData$ProbChurn         > churnThreshold, ]
churnYoungAdults <- youngAdultsTestChurnData[youngAdultsTestChurnData$ProbChurn > churnThreshold, ]

# Combine the subsets into 1 dataset
combinedChurnOnly <- rbind(churnAvg, churnFamily, churnFliers, churnGenZ, churnPioneer, churnYoungAdults)

averageTestChurnData$ProbChurn      <- NULL
familyTestChurnData$ProbChurn       <- NULL
fliersTestChurnData$ProbChurn       <- NULL
genZTestChurnData$ProbChurn         <- NULL
pioneerTestChurnData$ProbChurn      <- NULL
youngAdultsTestChurnData$ProbChurn  <- NULL

averageTestChurnData$customerID     <- NULL
familyTestChurnData$customerID      <- NULL
fliersTestChurnData$customerID      <- NULL
genZTestChurnData$customerID        <- NULL
pioneerTestChurnData$customerID     <- NULL
youngAdultsTestChurnData$customerID <- NULL

# evaluate model accuracy
preds         <- predict(model.average, newx=as.matrix(averageTestChurnData[, !names(averageTestChurnData) %in% c('Churn')]), type='response')
preds         <- ifelse(preds < threshold, 0, 1)
accuracies[1] <- mean(preds==averageTestChurnData$Churn) # accuracy

preds         <- predict(model.family, newx=as.matrix(familyTestChurnData[, !names(familyTestChurnData) %in% c('Churn')]), type='response')
preds         <- ifelse(preds < threshold, 0, 1)
accuracies[2] <- mean(preds==familyTestChurnData$Churn) # accuracy

preds         <- predict(model.fliers, newx=as.matrix(fliersTestChurnData[, !names(fliersTestChurnData) %in% c('Churn')]), type='response')
preds         <- ifelse(preds < threshold, 0, 1)
accuracies[3] <- mean(preds==fliersTestChurnData$Churn) # accuracy

preds         <- predict(model.genz, newx=as.matrix(genZTestChurnData[, !names(genZTestChurnData) %in% c('Churn')]), type='response')
preds         <- ifelse(preds < threshold, 0, 1)
accuracies[4] <- mean(preds==genZTestChurnData$Churn) # accuracy

preds         <- predict(model.pioneer, newx=as.matrix(pioneerTestChurnData[, !names(pioneerTestChurnData) %in% c('Churn')]), type='response')
preds         <- ifelse(preds < threshold, 0, 1)
accuracies[5] <- mean(preds==pioneerTestChurnData$Churn) # accuracy

preds         <- predict(model.adults, newx=as.matrix(youngAdultsTestChurnData[, !names(youngAdultsTestChurnData) %in% c('Churn')]), type='response')
preds         <- ifelse(preds < threshold, 0, 1)
accuracies[6] <- mean(preds==youngAdultsTestChurnData$Churn) # accuracy

data.table(models, accuracies)

# simulate deployment
# for customers facing high probability of churn, we fit their data into the other 5 models & recommend optimal bundle
combined.matrix <- as.matrix(combinedChurnOnly[, !names(combinedChurnOnly) %in% c('customerID', 'Churn', 'ProbChurn')])

combinedChurnOnly$average <- predict(model.average, newx=combined.matrix, type='response')
combinedChurnOnly$family  <- predict(model.family,  newx=combined.matrix, type='response')
combinedChurnOnly$fliers  <- predict(model.fliers,  newx=combined.matrix, type='response')
combinedChurnOnly$genz    <- predict(model.genz,    newx=combined.matrix, type='response')
combinedChurnOnly$pioneer <- predict(model.pioneer, newx=combined.matrix, type='response')
combinedChurnOnly$adults  <- predict(model.adults,  newx=combined.matrix, type='response')

combinedChurnOnlyProbs <- combinedChurnOnly[, c('average', 'family', 'fliers', 'genz', 'pioneer', 'adults')]
combinedChurnOnlyProbs$recomm <- apply(combinedChurnOnlyProbs, 1, function(x) {
  names(combinedChurnOnlyProbs)[which.min(x)]
})
combinedChurnOnlyFinal <- cbind(combinedChurnOnly[, c('customerID', 'ProbChurn')], combinedChurnOnlyProbs)

table(combinedChurnOnlyFinal$recomm)

