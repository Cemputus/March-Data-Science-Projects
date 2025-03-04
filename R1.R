## setting  WORKING DIRECTORY ----- 

setwd("~/Data_Science/year2/semester 2/Big Data -R/revision")

getwd()


## importing the necessary libraries----- 
library(tidyselect)
library(tidyverse)
library(readr)
library(dplyr)
library(ggplot2)
library(readxl)
library(data.table)


## Acquiring data ----

vegetable <- read_excel("Vegetable_Sales-1.xlsx")
view(vegetable)

dim(vegetable)

names(vegetable)

str(vegetable)
glimpse(vegetable)
view(glimpse(vegetable))

view(vegetable$`Quantity Sold (kg)`)


# summary statistics 
summary(vegetable)
view(summary(vegetable))

view(table(vegetable$`Item Name`))

view(table(vegetable$`Discount (Yes/No)`))


## using a function

vegetable1 <- mutate(vegetable, Total_price = vegetable$`Quantity Sold (kg)` * vegetable$`Unit Selling Price ($/kg)`)

view(vegetable1)


## using pipelines

vegetable2 <- vegetable %>%
  mutate(Total_price = `Quantity Sold (kg)` * `Unit Selling Price ($/kg)`)

# 1. best selling product basing on total price ----
## using which.max
#which.min

which.max(vegetable1$Total_price)


#concanting
Top_selling <- vegetable1[which.max(vegetable1$Total_price) , c("Item Name","Quantity Sold (kg)","Wholesale Price ($/kg)", "Sale or Return", "Discount (Yes/No)", "Total_price")]
Top_selling
view(Top_selling)

# worst selling product
which.min((vegetable1$Total_price))
Worst_selling <- vegetable1[which.min((vegetable1$Total_price)) , c("Item Name","Quantity Sold (kg)","Wholesale Price ($/kg)", "Sale or Return", "Discount (Yes/No)", "Total_price")]
view(Worst_selling)

## 2. filter the data for Discount -- discount= yes

Discount_filter <- filter(vegetable1, vegetable1$`Discount (Yes/No)`== "yes")
view(Discount_filter)

Discount_filter1 <- vegetable1 %>% 
  filter(`Discount (Yes/No)`== "yes")


view(Discount_filter1)

# without discounts
without_discounts <- filter(vegetable1, vegetable1$`Discount (Yes/No)`== "No")
view(without_discounts)

without_Discount1 <- vegetable1 %>% 
  filter(`Discount (Yes/No)`== "No")

view(without_Discount1)


# 3. how many were sold and how mant were returned -----

Sale_Return_table <- view(table(vegetable1$`Sale or Return`))

# 244 products were sold
# 7 products were returned


# 4. find the total revenue made out of the  ----
sold <- filter(vegetable1, vegetable1$`Sale or Return` == "sale")
view(sold)

Total_revenue <- sum(sold$Total_price)
Total_revenue


# total amount for the return
returned <- filter(vegetable1, vegetable1$`Sale or Return` == "return")
view(returned)

Total_revenue_returned <- sum(returned$Total_price)
Total_revenue_returned



# Handling missing Values ----


## 1. check for missing values ----

missing_values <- is.na(vegetable1)
sum(missing_values)


## 2. remove missing values ----
vegetable1_no_missing <- vegetable1 %>%
  drop_na()


## new dataset with missing values ----


diabetes <-  read.csv("Diabetes Missing Data.csv")
view(diabetes)


## 1. checking for missing values
missing_values1 <- sum(is.na(diabetes))
missing_values1



## use piping
diabetes %>% 
  filter(!complete.cases(.)) %>% 
  view()



glimpse(diabetes)

hist(diabetes$Skin_Fold)
hist(diabetes$Serum_Insulin)
hist(diabetes$Diabetes_Pedigree)
hist(diabetes$Age)
hist(diabetes$Pregnant)
hist(diabetes$BMI)
hist(diabetes$Glucose)
  






## imputint for missing values with median ---- 
diabetes$Skin_Fold[is.na(diabetes$Skin_Fold)] <- median(diabetes$Skin_Fold, na.rm = TRUE)

diabetes$Serum_Insulin[is.na(diabetes$Serum_Insulin)] <- median(diabetes$Serum_Insulin, na.rm = TRUE)

diabetes$Glucose[is.na(diabetes$Glucose)] <- median(diabetes$Glucose, na.rm = TRUE)


diabetes$Diastolic_BP[is.na(diabetes$Diastolic_BP)] <- median(diabetes$Diastolic_BP, na.rm = TRUE)


diabetes$BMI[is.na(diabetes$BMI)] <- median(diabetes$BMI, na.rm = TRUE)


# checking 
sum(is.na(diabetes$Skin_Fold))
sum(is.na(diabetes$Serum_Insulin))

sum(is.na(diabetes$Glucose))

sum(is.na(diabetes$Diastolic_BP))

sum(is.na(diabetes$BMI))


sum(is.na(diabetes))



















change  <- function(diabetes){
  for (col in c("Pregnant", "Glucose", "Diastolic_BP", "Skin_Fold", "Serum_Insulin", "BMI", "Diabetes_Pedigree", "Age","Class")){
    diabetes[[col]] <- is.numeric(diabetes[[col]])
  }
}

change(diabetes)

missing <- function(diabetes){
  for (col in c("Pregnant", "Glucose", "Diastolic_BP", "Skin_Fold", "Serum_Insulin", "BMI", "Diabetes_Pedigree", "Age","Class")){
    
    if(is.numeric(diabetes[[col]])){
      print(hist(diabetes[[col]],main = col))
    }
  }
}

missing(diabetes)











# Function to plot histograms for numeric columns
missing <- function(diabetes) {
  numeric_cols <- names(diabetes)[sapply(diabetes, is.numeric)]  # Select numeric columns
  
  par(mfrow = c(2, 2))  # Adjust grid for multiple plots
  
  for (col in numeric_cols) {
    hist(diabetes[[col]], main = paste("Histogram of", col), 
         xlab = col, col = "skyblue", border = "black")
  }
  
  par(mfrow = c(1, 1))  # Reset plot layout
}

# Call the function
missing(diabetes)





### using data.table
library(data.table)
library(tidyverse)
library(tidyr)


salaries <- fread("Salaries.csv")
view(salaries)


## select league id
salaries[lgID == 'AL',]

salaries[yearID == 2010, ]
#DT[I,J,by]
#subsetting by indexing
#getting the first row
salaries[1,]
#getting the first 10 rows
salaries[1:10,]
salaries[1:10,1] #first column only
salaries[1:10,1:4] #first 4 columns
salaries[1:10,lgID:salary]
salaries[1:10,list(lgID,salary)]
salaries[yearID>2000,][head(yearID,5)] #rows above 2000 for all columns
#first five rows above 2000

#Salaries from a specific year
salaries[yearID==2010,salary]
salaries[yearID==2010,] 

#select league id
salaries[lgID=="AL",]
# DATA WHERE LEAGUE ID IS AL AND YEARID IS 1990
salaries[lgID=="AL" & yearID==1990,]
#teamid=bal,lgid=AL, head=4
salaries[teamID=="BAL"& lgID=="AL",][1:10]



## summary stat -----
salaries[, mean(salary)]


salaries[, table(lgID)]
salaries[, sum(salary)]

## DT[i,j,by]
# find the total salaries of leagueID = AL
salaries[, sum(salary), by = lgID]

salaries[,.(total = sum(salary)), by =lgID]

salaries[,.(Average = mean(salary),median = median(salary)),by = "teamID"]

## count 
salaries[,.(count = .N),by = "teamID"]


library(data.table)
## stting diabetes to data.table -----
diabetes <- fread("Diabetes Missing Data.csv") 

## checking for missing values
missing_values4 <- sapply(diabetes, function(x) sum(is.na(x)))
missing_values4

## checking for missing values  using diabetes
diabetes[, lapply(.SD, function(x) sum(is.na(x)))]

incomplete_cases <- diabetes[!complete.cases(diabetes)]
view(incomplete_cases)

diabetes[!complete.cases(diabetes)] %>% 
  view()


# imputing for missing values using zeros(0)
diabetes[, Skin_Fold = fifelse(is.na(Skin_Fold), 0, Skin_Fold)]

diabetes[, Serum_Insulin = ifelse(is.na(Serum_Insulin), 0, Serum_Insulin)]


# visualizing the Glucose ----
hist(diabetes$Glucose)

diabetes[, Glucose := fifelse(is.na(Glucose), median(Glucose, na.rm = TRUE), Glucose)]

names(diabetes)

hist(diabetes$Diastolic_BP)
diabetes[, Diastolic_BP := fifelse(is.na(Diastolic_BP), median(Diastolic_BP, na.rm = TRUE), Diastolic_BP)]

hist(diabetes$Skin_Fold)
diabetes[, Skin_Fold := fifelse(is.na(Skin_Fold), median(Skin_Fold, na.rm = TRUE), Skin_Fold)]

hist(diabetes$Serum_Insulin)
diabetes[, Serum_Insulin := fifelse(is.na(Serum_Insulin), median(Serum_Insulin, na.rm = TRUE), Serum_Insulin)]


missing_values4


# forward fill 
diabetes[, BMI:= nafill(BMI, type = 'locf')]

# backwaerd fill
diabetes[, BMI := nafill(BMI, type = "nocb")]



# checking for duplicates
diabetes[duplicated(diabetes)]

diabetes[duplicated(diabetes) | duplicated(diabetes, fromLast = TRUE)]

# handling dupliccates  -----
diabetes_new <- unique(diabetes)


# checking foe numeric ----
numerical <- diabetes %>% 
  select_if(is.numeric)


names(numerical)
### checking for outliers -----

boxplot(diabetes$Pregnant)

boxplot(diabetes$Glucose)
boxplot(diabetes$Diastolic_BP)
boxplot(diabetes$Skin_Fold)
boxplot(diabetes$Serum_Insulin)
boxplot(diabetes$BMI)
boxplot(diabetes$Diabetes_Pedigree)
boxplot(diabetes$Age)
boxplot(diabetes$Class)


## setting subplots ----
# Function to plot histograms for numeric columns -----
Histogram <- function(diabetes) {
  numeric_cols <- names(diabetes)[sapply(diabetes, is.numeric)]  # Select numeric columns
  
  par(mfrow = c(2,3 ))  # Adjust grid for multiple plots
  
  for (col in numeric_cols) {
    hist(diabetes[[col]], main = paste("Histogram of", col), 
         xlab = col, col = "skyblue", border = "black")
  }
  
  par(mfrow = c(1, 1))  # Reset plot layout
}


## calling the function
Histogram(diabetes)


library(ggplot2)
library(reshape2) # For melting data into long format

# Convert data to long format
diabetes_melted <- melt(diabetes, measure.vars = c("Pregnant", "Glucose", "Diastolic_BP", 
                                                   "Skin_Fold", "Serum_Insulin", "BMI", 
                                                   "Diabetes_Pedigree", "Age"))

# Create a faceted boxplot
ggplot(diabetes_melted, aes(x = variable, y = value)) +
  geom_boxplot() +
  facet_wrap(~ variable, scales = "free") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Boxplots of Diabetes Dataset Features")


## handling outliers ----
q1 <- diabetes[quantile(Glucose, 0.25)]
q3 <- diabetes[quantile(Glucose, 0.75)]

IQR <- q3 - q1

lower_bound <- q1-1.5*IQR
upper_bound <- q3+1.5*IQR

## checking for outlier ----
outlier <- diabetes[Glucose<lower_bound|Glucose >upper_bound]

# removing outlier
removing_outlier <- diabetes[Glucose >= lower_bound & Glucose <= upper_bound]




## Handling outliers ----
q1 <- quantile(diabetes$Glucose, 0.25, na.rm = TRUE)
q3 <- quantile(diabetes$Glucose, 0.75, na.rm = TRUE)

IQR <- q3 - q1

lower_bound <- q1 - 1.5 * IQR
upper_bound <- q3 + 1.5 * IQR

## Checking for outliers ----
outliers <- diabetes[Glucose < lower_bound | Glucose > upper_bound]

## Removing outliers ----
diabetes_no_outliers <- diabetes[Glucose >= lower_bound & Glucose <= upper_bound]

Boxplots <- function(diabetes) {
  numeric_cols <- names(diabetes)[sapply(diabetes, is.numeric)]  # Select numeric columns
  
  par(mfrow = c(2,2 ))  # Adjust grid for multiple plots
  
  for (col in numeric_cols) {
    boxplot(diabetes[[col]], main = paste("Boxplots of", col), 
         xlab = col, col = "skyblue", border = "black")
  }
  
  par(mfrow = c(1, 1))  # Reset plot layout
}


## calling the function
Boxplots(diabetes)



# Alternative Approach: Remove Outliers for All Numeric Columns ----

remove_outliers <- function(diabetes, cols) {
  for (col in cols) {
    q1 <- quantile(diabetes[[col]], 0.25, na.rm = TRUE)
    q3 <- quantile(diabetes[[col]], 0.75, na.rm = TRUE)
    IQR <- q3 - q1
    lower_bound <- q1 - 1.5 * IQR
    upper_bound <- q3 + 1.5 * IQR
    diabetes <- diabetes[diabetes[[col]] >= lower_bound & diabetes[[col]] <= upper_bound, ]
  }
  return(diabetes)
}

# Apply to numeric columns
numeric_cols <- names(diabetes)[sapply(diabetes, is.numeric)]
diabetes_cleaned <- remove_outliers(diabetes, numeric_cols)



