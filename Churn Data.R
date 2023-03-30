# Group Project Analysis for Churn Data

library(Hmisc)
library(data.table)
library(ggplot2)
library(neuralnet)
library(dplyr)
library(caret)
library(nnet)
library(ggbeeswarm)

# Importing of Data
setwd("C:/Users/Siah Wee Hung/Desktop/Folders/2. School/NTU/Y2S2/BC2407 Analytics II/Project")
churnData <- read.csv("telecom_customer_churn.csv", stringsAsFactors = TRUE, na.strings = c('NULL'))

##################################### Variables with Wrong Classification after Import #############################################
# 1. CustomerID ~ Char instead of Categorical
# 2. Number of Dependents - Factor instead of Integer
# 3. City ~ Char instead of Factor

#################################################### Data Cleaning Phase ##########################################################################
churnData$City <- factor(churnData$City)
churnData$Number.of.Dependents <- factor(churnData$Number.of.Dependents)

# Dropping Variables that are not relevant for our analysis
churnData$Customer.ID <- NULL
churnData$Latitude <- NULL
churnData$Longitude <- NULL
#churnData$City <- NULL
churnData$Zip.Code <- NULL

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
  # varTable[index, 1] <- sum(is.na(churnData[,get(names(churnData)[index])]))
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
# churnData$Phone.Service <- NULL

# # Determine the Mean of Long Distance Charges, not removing NA, just not including in mean calculation
# longDistChargesMean <- mean(churnData$Avg.Monthly.Long.Distance.Charges, na.rm = TRUE)
# # Impute Long Distance Charges
# churnData$Avg.Monthly.Long.Distance.Charges <- replace(churnData$Avg.Monthly.Long.Distance.Charges, 
#                                                        is.na(churnData$Avg.Monthly.Long.Distance.Charges),
#                                                        longDistChargesMean)
# Ensure Missing Values are imputed
sum(is.na(churnData$Avg.Monthly.Long.Distance.Charges))


## 2. Impute Avg.Monthly.GB.Download
# 0 if not subscribed to Internet Plan
churnData$Avg.Monthly.GB.Download[churnData$Internet.Service == "No"] <- 0
# churnData$Internet.Service <- NULL

# Similarly, we can impute the Avg.Monthly.GB.Download as well. But instead of choosing mean, we will impute
# it with Median since the spread of GB Download is larger from 2GB to 85GB.

# Determine the Median of Avg Monthly Download
# avgMthDownloadMedian <- median(churnData$Avg.Monthly.GB.Download, na.rm = TRUE)
# # Impute Downloads with Median
# churnData$Avg.Monthly.GB.Download <- replace(churnData$Avg.Monthly.GB.Download,
#                                              is.na(churnData$Avg.Monthly.GB.Download),
#                                              avgMthDownloadMedian)
# Ensure no more Missing Value
sum(is.na(churnData$Avg.Monthly.GB.Download))

## 3. Impute Churn.Category
# Increase the Number of Factors to allow for Different Factors
# -------------------------- 
# why dont we just drop those that don't churn?
# --------------------------
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

#################################### Data Visualisation: Univariate Categorical ####################################

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
                                        "Offer A" = "Orange",
                                        "Offer B" = "Yellow",
                                        "Offer C" = "Green",
                                        "Offer D" = "Blue",
                                        "Offer E" = "Purple")) +
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
ggplot(data = churnData, aes(x = Churn.Reason, fill = Churn.Reason)) +
  geom_bar() +
  scale_fill_manual("Churn.Reason", values = c(rep(1:22))) +
  labs(title = "Proportion of Customer by Reason for Churn")


####################################### Data Visualisation: Univariate Numeric #########################################

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

#################################### Data Visualisation: Bivariate [Categorical vs Categorical] ####################################

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

## Doesn't make sense to explore with Churn Category and Reason im assuming?

#################################### Data Visualisation: Bivariate [Categorical vs Numeric] ####################################
#### Violin & Boxplots

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


### Extra EDA ####################################################################################################################################

df.actionable.churn = subset(churnData, Churn.Reason %in% actionable.reasons)
nrow.actionable = nrow(df.actionable.churn)
df.stayed.downsample = sample_n(subset(churnData, Churn.Reason=='Staying Customer'), nrow.actionable)
df.combine = rbind(df.actionable.churn, df.stayed.downsample)
nrow(df.combine)

set.seed(123)
df.combine$Y <- factor(ifelse(df.combine$Customer.Status=='Churned', 1, 0))
df.combine$Count <- NULL
df.combine$Customer.Status.Temp <- NULL
df.combine$Customer.Status <- NULL
df.combine$Churn.Reason <- NULL
df.combine$Churn.Status <- NULL
df.combine$Churn.Category <- NULL
df.combine$Usage.Charge.Ratio <- NULL
df.combine$Number.of.Referrals <- NULL
df.combine$Offer <- NULL
df.combine$Payment.Method <- NULL
df.combine$Paperless.Billing <- NULL
#df.combine$City <- NULL
df.combine$Service.Count <- as.numeric(df.combine$Service.Count)
str(df.combine)

model <- glm(Y ~ ., data=df.combine, family='binomial')
options(max.print=5000)
summary(model)


ggplot(subset(churnData, Churn.Reason %in% c('Staying Customer', 'New Customer', 'Competitor made better offer', 'Extra data charges', 'Competitor offered more data')), aes(x=Churn.Reason, y=Count, fill=Unlimited.Data)) +
  geom_bar(position = "fill",stat = "identity") +
  labs(x = "Churn.Reason",
       y = "Proportion",
       title = "Proportion of Unlimited.Data for each Churn.Reason")

ggplot(subset(churnData, Customer.Status != 'Joined'), aes(x = Customer.Status, y = Tenure.in.Months, fill = Customer.Status)) +
  geom_violin() +
  geom_boxplot(width = 0.1,
               fill = "Orange",
               outlier.colour = "Orange",
               outlier.size = 1.5,
               alpha = 0.3) +
  labs(x = "Customer.Status",
       y = "Tenure.in.Months",
       title = "Relationship between Tenure.in.Months & Customer.Status")


churnData$Usage.Charge.Ratio <- pmax(churnData$Monthly.Charge / churnData$Avg.Monthly.GB.Download, rep(0, nrow(churnData)))
churnData$Customer.Status.Temp <- factor(ifelse(churnData$Churn.Reason=='Price too high', 'Price too high', 'Not Churned / Other reasons'))

ggplot(churnData, aes(x=log(1+Usage.Charge.Ratio), fill=Customer.Status.Temp)) +
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

ggplot(churnData, aes(x=Customer.Status.Temp, y=log(1+Usage.Charge.Ratio), fill = Customer.Status.Temp)) +
  geom_violin() +
  geom_boxplot(width = 0.5,
               fill = "Orange",
               outlier.colour = "Orange",
               outlier.size = 1.5) +
  scale_fill_manual(values=c('#00BFC4', '#F9766E')) +
  labs(x = "Customer.Status",
       y = "Usage.Charge.Ratio",
       title = "Relationship between Usage.Charge.Ratio & Customer.Status")



ggplot(churnData, aes(x = Customer.Status, y = Usage.Charge.Ratio, fill = Customer.Status)) +
  geom_violin() +
  geom_boxplot(width = 0.5,
               fill = "Orange",
               outlier.colour = "Orange",
               outlier.size = 1.5) +
  labs(x = "Customer.Status",
       y = "Usage.Charge.Ratio",
       title = "Relationship between Usage.Charge.Ratio & Customer.Status")


churnData$Customer.Status.Temp <- factor(ifelse(churnData$Churn.Reason=='Competitor offered more data', 'Competitor offered more data', 'Not Churned / Other reasons'))

ggplot(churnData, aes(x = Customer.Status.Temp, y = Avg.Monthly.GB.Download, fill = Customer.Status.Temp)) +
  geom_violin() +
  geom_boxplot(width = 0.5,
               fill = "Orange",
               outlier.colour = "Orange",
               outlier.size = 1.5,
               alpha = 0.3) +
  labs(x = "Customer.Status",
       y = "Avg.Monthly.GB.Download",
       title = "Relationship between Avg.Monthly.GB.Download & Customer.Status")


churnData$Customer.Status.Temp <- factor(ifelse(churnData$Customer.Status=='Churned', 'Churned', 'Not Churned'))

churnData$Service.Count = factor(
  ifelse(churnData$Streaming.Music=='Yes', 1, 0) + 
    ifelse(churnData$Streaming.Movies=='Yes', 1, 0) +
    ifelse(churnData$Streaming.TV=='Yes', 1, 0) +
    ifelse(churnData$Premium.Tech.Support=='Yes', 1, 0) +
    ifelse(churnData$Device.Protection.Plan=='Yes', 1, 0) +
    ifelse(churnData$Online.Backup=='Yes', 1, 0) +
    ifelse(churnData$Online.Security=='Yes', 1, 0)
)


churnData$Count <- rep(1, nrow(churnData))

ggplot(churnData, aes(x = Service.Count, y=Count, fill = Customer.Status.Temp)) +
  geom_bar(stat = "identity")


ggplot(churnData, aes(x = Age, y = Avg.Monthly.GB.Download)) +
  geom_jitter(aes(color=Customer.Status.Temp), width=0.1, alpha=0.5) + 
  scale_color_manual(values=c('Churned'='#F9766E', 'Not Churned'='#00BFC4'))

ggplot(subset(churnData, Age<30), aes(y=Avg.Monthly.GB.Download, x=Customer.Status.Temp, fill=Customer.Status.Temp)) +
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
churnData$Number.of.Dependents.Bin <- as.numeric(churnData$Number.of.Dependents)-1
churnData$Number.of.Dependents.Bin = ifelse(churnData$Number.of.Dependents.Bin < 3, churnData$Number.of.Dependents.Bin, '>=3')
ggplot(churnData, aes(x = factor(Number.of.Dependents.Bin, levels=c('0','1','2', '>=3')), y=Count, fill = Customer.Status)) +
  geom_bar(position = "fill",stat = "identity") +
  labs(x = "Number.of.Dependents",
       y = "Proportion",
       title = "Proportion of Unlimited.Data for each Churn.Reason")
  theme_minimal()

actionable.reasons = c('Competitor had better devices', 
                       'Product dissatisfaction', 
                       'Limited range of services', 
                       'Competitor made better offer',
                       'Long distance charges',
                       'Lack of affordable download/upload speed',
                       'Competitor offered more data',
                       'Price too high',
                       'Extra data charges')


### Neural Networks Model ########################################################################################################################

# Setting the Threshold for Data Usage
quantile(churnData$Avg.Monthly.GB.Download, probs = c(0.25, 0.50, 0.75))
lowDataThreshold <- quantile(churnData$Avg.Monthly.GB.Download, probs = 0.25)
highDataThreshold <- quantile(churnData$Avg.Monthly.GB.Download, probs = 0.75)

# Setting the Threshold for Monthly Long Distance Charges
quantile(churnData$Avg.Monthly.Long.Distance.Charges, probs = c(0.25, 0.50, 0.75))
lowLongDistanceThreshold <- quantile(churnData$Avg.Monthly.Long.Distance.Charges, probs = 0.25)
highLongDistanceThreshold <- quantile(churnData$Avg.Monthly.Long.Distance.Charges, probs = 0.75)

# Setting for Threshold for Extra Data Charges
## All 0, which is why we have to fall back on the earlier thresholds
quantile(churnData$Total.Extra.Data.Charges, probs = c(0.25, 0.50, 0.75))

# Setting the Threshold for Total Long Distance Charges
quantile(churnData$Total.Long.Distance.Charges, probs = c(0.25, 0.50, 0.75))
lowTotalLongDistThreshold <- quantile(churnData$Total.Long.Distance.Charges, probs = 0.25)
highTotalLongDistThreshold <- quantile(churnData$Total.Long.Distance.Charges, probs = 0.75)

# Creating a New Dependent Variable, Customer's Desired Bundle, to predict Compatibility
churnData$Most.Compatible.Bundle <- NA
levels(churnData$Most.Compatible.Bundle) <- c("Young Adults", "Pioneer Generations", "Family Bundle", 
                                              "Gen Z Streamers", "Frequent Fliers", "Silver Surfers",
                                              "Average Bundle")

## Determining their Optimal Bundle ############################################################################################################

# Initial Number of People without Bundle
sum(is.na(churnData$Most.Compatible.Bundle))

# Frequent Fliers
churnData$Most.Compatible.Bundle[churnData$Total.Long.Distance.Charges > highTotalLongDistThreshold] <- "Frequent Fliers"

# Young Adults - Also with Unlimited Data
churnData$Most.Compatible.Bundle[churnData$Avg.Monthly.GB.Download > highDataThreshold & churnData$Avg.Monthly.Long.Distance.Charges < lowLongDistanceThreshold & churnData$Unlimited.Data == "Yes"] <- "Young Adults"

# Pioneer Generation - Discounts due to Age
churnData$Most.Compatible.Bundle[churnData$Age > 60 & churnData$Avg.Monthly.GB.Download < lowDataThreshold & churnData$Avg.Monthly.Long.Distance.Charges > highLongDistanceThreshold] <- "Pioneer Generation"

# Family Bundle
churnData$Most.Compatible.Bundle[churnData$Multiple.Lines == "Yes" & churnData$Number.of.Dependents != "0" & churnData$Married == "Yes"] <- "Family Bundle"

# Gen Z
churnData$Most.Compatible.Bundle[churnData$Avg.Monthly.GB.Download > highDataThreshold & (churnData$Streaming.Movies == "Yes" | churnData$Streaming.Music == "Yes" | churnData$Streaming.TV == "Yes")] <- "Gen Z Streamers"

# Silver Surfers - Age not a Factor, no Discounts for Age
churnData$Most.Compatible.Bundle[churnData$Avg.Monthly.GB.Download < lowDataThreshold & churnData$Avg.Monthly.Long.Distance.Charges > highLongDistanceThreshold] <- "Silver Surfers"

# Average Bundle - For now, if no Special Terms
churnData$Most.Compatible.Bundle[is.na(churnData$Most.Compatible.Bundle)] <- "Average Bundle"

# Convert the New Dependent Variable into a Factor Variable
churnData$Most.Compatible.Bundle <- as.factor(churnData$Most.Compatible.Bundle)
# Verify all Customers should have an Mobile Bundle that is Compatible with them!!
sum(is.na(churnData$Most.Compatible.Bundle))


# # Create a Copy of the Dataframe to be used with the Neural Model
# neuralChurnData <- data.frame(churnData)
# # Ensure is a deep copy
# tracemem(neuralChurnData) == tracemem(churnData)

# Pre-processing required for NeuralNet Function ###########################################################################
set.seed(123)

## Scaling and Encoding of Variables (if necessary) for nnet
scaledChurnData <- churnData %>% mutate(across(where(is.numeric), scale))

# Train-Test Split; list = F to return a vector of indices
trainIndex <- createDataPartition(scaledChurnData$Most.Compatible.Bundle, p = 0.7, list = FALSE)
trainChurnData <- scaledChurnData[trainIndex, ]
testChurnData <- scaledChurnData[-trainIndex, ]

# NN model with 10 nodes in the hidden layer, no linear output and max iterations of 1000 in training
nnet.m1 <- nnet(Most.Compatible.Bundle ~ ., 
                data = trainChurnData, size = 10,
                linout = FALSE, maxit = 1000)

# Predictions using Neural Network Function
predict.m1 <- predict(nnet.m1, newdata = testChurnData, type = "class")
predict.m1 <- factor(predict.m1, levels = levels(testChurnData$Most.Compatible.Bundle))
# Accuracy of Predictions with all Variables
nnet.m1.accuracy <- mean(testChurnData$Most.Compatible.Bundle == predict.m1)
nnet.m1.accuracy
# Confusion Matrix
nnet.m1.cm <- confusionMatrix(testChurnData$Most.Compatible.Bundle, predict.m1)
nnet.m1.cm

## VALID TILL HERE AFTERWARDS IS EXPERIMENTATION##################################################################################################

## Processing Dataframe to try out alternative measure
scaledChurnData$dummyMostCompatibleBundle <- class.ind(scaledChurnData$Most.Compatible.Bundle)
scaledChurnData$Most.Compatible.Bundle <- NULL

trainChurnData2 <- scaledChurnData[trainIndex, ]
testChurnData2 <- scaledChurnData[-trainIndex, ]

nnet.m2 <- nnet(dummyMostCompatibleBundle ~ ., 
                data = trainChurnData2, size = 10, softmax = TRUE, entropy = TRUE,
                linout = FALSE, maxit = 1000)

# Predictions using Neural Network Function
predict.m2 <- predict(nnet.m2, newdata = testChurnData2, type = "class")
predict.m2 <- factor(predict.m2, levels = levels(testChurnData$Most.Compatible.Bundle))
# Accuracy of Predictions with all Variables
nnet.m2.accuracy <- mean(testChurnData2$dummyMostCompatibleBundle == predict.m2)
nnet.m2.accuracy
# Confusion Matrix
nnet.m2.cm <- confusionMatrix(testChurnData2$dummyMostCompatibleBundle, predict.m2)
nnet.m2.cm






# Convert Most.Compatible.Bundle to a Numeric for Neuralnet
churnData$Most.Compatible.Bundle <- as.numeric(churnData$Most.Compatible.Bundle)

neuralnet.m1 <- neuralnet(Most.Compatible.Bundle ~ ., 
                          data = churnData, hidden = 3, act.fct = "logistic", linear.output = FALSE)








































# Normalise the Continuous Variables in ChurnData, without touching the 
scaledChurnData <- churnData %>% mutate(across(where(is.numeric), scale))
# maxs <- apply(churnData, 2, max) 
# mins <- apply(churnData, 2, min)
# scaledChurnData <- as.data.frame(scale(churnData, center = mins, scale = maxs - mins))

# Identify the Categorical Variables
churnCategoricalVar <- c("Gender", "Married", "Number.of.Dependents", "Offer", "Phone.Service", "Multiple.Lines",
                         "Internet.Service", "Internet.Type", "Online.Security", "Online.Backup", "Device.Protection.Plan",
                         "Premium.Tech.Support", "Streaming.TV", "Streaming.Movies", "Streaming.Music", "Unlimited.Data",
                         "Contract", "Paperless.Billing", "Payment.Method", "Customer.Status", "Churn.Category",
                         "Churn.Reason", "Most.Compatible.Bundle")

# Convert Categorical to Dummy Variables
# Convert the predictor variables into dummy variables
predictors <- scaledChurnData[, !(names(scaledChurnData) %in% c("Most.Compatible.Bundle", "Churn"))]
neuralChurnData <- dummyVars(" ~ .", data = predictors)
neuralChurnData <- as.data.frame(predict(neuralChurnData, newdata = predictors))


neuralChurnData$Most.Compatible.Bundle <- NA
levels(neuralChurnData$Most.Compatible.Bundle) <- c("Young Adults", "Pioneer Generations", "Family Bundle", 
                                                    "Gen Z Streamers", "Frequent Fliers", "Silver Surfers",
                                                    "Average Bundle")

## Determine their Optimal Bundle

# Initial Number of People without Bundle
sum(is.na(neuralChurnData$Most.Compatible.Bundle))
# Young Adults - Also with Unlimited Data
neuralChurnData$Most.Compatible.Bundle[churnData$Avg.Monthly.GB.Download > highDataThreshold & churnData$Avg.Monthly.Long.Distance.Charges < lowLongDistanceThreshold & churnData$Unlimited.Data == "Yes"] <- "Young Adults"

# Pioneer Generation - Discounts due to Age
neuralChurnData$Most.Compatible.Bundle[churnData$Age > 60 & churnData$Avg.Monthly.GB.Download < lowDataThreshold & churnData$Avg.Monthly.Long.Distance.Charges > highLongDistanceThreshold] <- "Pioneer Generation"

# Family Bundle
neuralChurnData$Most.Compatible.Bundle[churnData$Multiple.Lines == "Yes" & churnData$Number.of.Dependents != "0" & churnData$Married == "Yes"] <- "Family Bundle"

# Gen Z
neuralChurnData$Most.Compatible.Bundle[churnData$Avg.Monthly.GB.Download > highDataThreshold & (churnData$Streaming.Movies == "Yes" | churnData$Streaming.Music == "Yes" | churnData$Streaming.TV == "Yes")] <- "Gen Z Streamers"


# Silver Surfers - Age not a Factor, no Discounts for Age
neuralChurnData$Most.Compatible.Bundle[churnData$Avg.Monthly.GB.Download < lowDataThreshold & churnData$Avg.Monthly.Long.Distance.Charges > highLongDistanceThreshold] <- "Silver Surfers"

# Average Bundle - For now, if no Special Terms
neuralChurnData$Most.Compatible.Bundle[is.na(neuralChurnData$Most.Compatible.Bundle)] <- "Average Bundle"

# Convert the New Dependent Variable into a Factor Variable
neuralChurnData$Most.Compatible.Bundle <- as.factor(neuralChurnData$Most.Compatible.Bundle)

# Verify all Customers should have an Mobile Bundle that is Compatible with them!!
sum(is.na(neuralChurnData$Most.Compatible.Bundle))

## Since the neuralnet library does not help us encode the categorical factors to be used in the training of our mode
## it is necessary to encode all of the variables of type factor into a binary dummy variable via One-Hot Encoding
## before using it in the training of our model
# neuralNetData <- model.matrix(~ Churn.Reason + Churn.Category + Customer.Status,
#                               data = churnData,
#                               constrasts.arg = list(Churn.Reason=diag(nlevels(churnData$Churn.Reason)),
#                                                     Churn.Category=diag(nlevels(churnData$Churn.Category)),
#                                                     Customer.Status=diag(nlevels(churnData$Customer.Status))))
# # Add the Most.Compatible.Bundle Feature
# neuralnet$Most.Compatible.Bundle <- churnData$Most.Compatible.Bundle

# Train & Test Split
set.seed(1)
trainIndex <- createDataPartition(neuralChurnData$Most.Compatible.Bundle, p = .7, list = FALSE)
training <- neuralChurnData[trainIndex, ]
testing <- neuralChurnData[-trainIndex, ]

# Relabel the Variables so that there is no Whitespace
neuralChurnData <- neuralChurnData %>% rename(
  `Contract.Month.to.Month` = `Contract.Month-to-Month`,
  `Contract.One.Year` = `Contract.One Year`,
  `Contract.Two.Year` = `Contract.Two Year`,
  `Payment.Method.Bank.Withdrawal` = `Payment.Method.Bank Withdrawal`,
  `Payment.Method.Credit.Card` = `Payment.Method.Credit Card`,
  `Payment.Method.Mailed.Check` = `Payment.Method.Mailed Check`,
  `Churn.Category.Staying.Customer` = `Churn.Category.Staying Customer`,
  `Churn.Category.New.Customer` = `Churn.Category.New Customer`,
  `Churn.Reason.Attitude.of.service.provider` = `Churn.Reason.Attitude of service provider`,
  `Churn.Reason.Attitude.of.support.person` = `Churn.Reason.Attitude of support person`,
  `Churn.Reason.Competitor.had.better.devices` = `Churn.Reason.Competitor had better devices`,
  `Churn.Reason.Competitor.made.better.offer` = `Churn.Reason.Competitor made better offer`,
  `Churn.Reason.Competitor.offered.higher.download.speeds` = `Churn.Reason.Competitor offered higher download speeds`,
  `Churn.Reason.Competitor.offered.more.data` = `Churn.Reason.Competitor offered more data`,
  `Churn.Reason.Dont.know` = `Churn.Reason.Don't know`,
  `Churn.Reason.Extra.data.charges` = `Churn.Reason.Extra data charges`,
  `Churn.Reason.Lack.of.affordable.download.upload.speed` = `Churn.Reason.Lack of affordable download/upload speed`,
  `Churn.Reason.Lack.of.self.service.on.Website` = `Churn.Reason.Lack of self-service on Website`,
  `Churn.Reason.Limited.range.of.services` = `Churn.Reason.Limited range of services`,
  `Churn.Reason.Long.distance.charges` = `Churn.Reason.Long distance charges`,
  `Churn.Reason.Network.reliability` = `Churn.Reason.Network reliability`,
  `Churn.Reason.Poor.expertise.of.online.support` = `Churn.Reason.Poor expertise of online support`,
  `Churn.Reason.Poor.expertise.of.phone.support` = `Churn.Reason.Poor expertise of phone support`,
  `Churn.Reason.Price.too.high` = `Churn.Reason.Price too high`,
  `Churn.Reason.Product.dissatisfaction` = `Churn.Reason.Product dissatisfaction`,
  `Churn.Reason.Service.dissatisfaction` = `Churn.Reason.Service dissatisfaction`,
  `Churn.Reason.Staying.Customer` = `Churn.Reason.Staying Customer`,
  `Churn.Reason.New.Customer` = `Churn.Reason.New Customer`,
  `Device.Protection.Plan.No.Internet.Equipment` = `Device.Protection.Plan.No Internet Equipment`,
  `Online.Backup.No.Internet.Service` = `Online.Backup.No Internet Service`,
  `Online.Security.No.Internet.Service` = `Online.Security.No Internet Service`,
  `Internet.Type.Fiber.Optic` = `Internet.Type.Fiber Optic`
)

# Train the Neural Network to help predict the Optimum Bundle
neuralVars <- names(neuralChurnData)
neuralnet.m1 <- neuralnet(Most.Compatible.Bundle ~ Gender.Female + Gender.Male + Age + Married.No + Married.Yes
                          + Number.of.Dependents.0 + Number.of.Dependents.1 + Number.of.Dependents.2 + Number.of.Dependents.3
                          + Number.of.Dependents.4 + Number.of.Dependents.5 + Number.of.Dependents.6 + Number.of.Dependents.7
                          + Number.of.Dependents.8 + Number.of.Dependents.9 + Number.of.Referrals + Tenure.in.Months
                          + Offer.None + Offer.Offer.A + Offer.Offer.B + Offer.Offer.C + Offer.Offer.D + Offer.Offer.E
                          + Phone.Service.No + Phone.Service.Yes + Avg.Monthly.Long.Distance.Charges + Multiple.Lines.No
                          + Multiple.Lines.Yes + Churn.Category.Attitude, 
                          data = training, hidden = 3, err.fct = "ce", linear.output = FALSE)

neuralnet.m1 <- neuralnet(Most.Compatible.Bundle ~ ., 
                          data = training, hidden = 3, err.fct = "ce", linear.output = FALSE)
predictions <- predict(neuralnet.m1, )


# Linear Regression
m1 <- glm(Customer.Status ~ . - Churn.Category - Churn.Reason - Zip.Code - Latitude - Longitude - City, 
          data = churnData, family = binomial)


m2 <- glm(Customer.Status ~ . - Churn.Category - Churn.Reason, 
          data = churnData, family = binomial)

summary(m1)

summary(m2)

competitorChurnData <- churnData[churnData$Churn.Category == "Competitor", ]
summary(competitorChurnData)

# ggplot(competitorChurnData, aes(x = Customer.Status, y = Churn.Reason, fill = Customer.Status)) +
#   # geom_violin() +
#   geom_boxplot(width = 0.5,
#                fill = "Orange",
#                outlier.colour = "Orange",
#                outlier.size = 1.5) +
#   labs(x = "Customer.Status",
#        y = "Churn.Reason",
#        title = "Relationship between Churn.Reason & Customer.Status")


# Phone.Service against Customer.Status
ggplot(competitorChurnData, aes(x = Churn.Reason, fill = Customer.Status)) +
  geom_bar(position = "stack") +
  labs(x = "Churn.Reason",
       y = "Count",
       title = "Relationship between Churn.Reason and Customer Churn") +
  theme_minimal()

# Threshold > 75th Percentile --> Gives Data
# Score based on percetile, >75 means 3, 50th is 2, 25th is 1!!

## Not sure how to do EDA with City since Char







