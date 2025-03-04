### SEETING HE WPRKING DIRECTORATE ###
setwd("H:\\2024-25-R-Analysis\\2025-Analysis\\Practise")

## Loading the libraries ###
library(tidyverse)
library(tidyr)
library(tidyselect)
library(ggplot2)
library(data.table)
library(readxl)
library(readr)

### Loading the data set ##
df <- read_excel("H:/2024-25-R-Analysis/Data_set/Vegetable_Sales-1.xlsx")
df %>% 
  view()

### Understanding the structure of my data set ###
## Shape of the data
dim(df) #Rows -------> 251  columns --------> 8

## Structure
glimpse(df)

## Counting some categorical columns
df$`Discount (Yes/No)` %>% 
  table() %>% 
  view()

## Add a new variable call Total_Price
df2 <- mutate(df,Total_Price = df$`Quantity Sold (kg)` * df$`Unit Selling Price ($/kg)`)
df2
dim(df)
dim(df2)
df2 %>% 
  view()

## Find the best selling vegetable based on Total_Price
which.max(df2$Total_Price)
Top_selling <- df2[which.max(df2$Total_Price),c("Item Name","Loss Rate (%)","Wholesale Price ($/kg)",
                                                    "Quantity Sold (kg)","Unit Selling Price ($/kg)",
                                                    "Sale or Return","Discount (Yes/No)","Total_Price")]

Top_selling %>% 
  view()

## Find the least selling vegetable ##
Least_selling <- df2[which.min(df2$Total_Price),c("Item Name","Loss Rate (%)","Wholesale Price ($/kg)",
                                                "Quantity Sold (kg)","Unit Selling Price ($/kg)",
                                                "Sale or Return","Discount (Yes/No)","Total_Price")]

Least_selling %>% 
  view()

##  Which vegetable do you advise the business owner not to sell ?
High_loss <- df2[which.max(df2$`Loss Rate (%)`),c("Item Name","Loss Rate (%)","Wholesale Price ($/kg)",
                                              "Quantity Sold (kg)","Unit Selling Price ($/kg)",
                                              "Sale or Return","Discount (Yes/No)","Total_Price")]
High_loss %>% 
  view()

## How many vegetables were sold on discount and those that were given discount ?
Discount_table <- table(df2$`Discount (Yes/No)`)
Discount_table %>%     ## Discount ---------------> 6
  view()               ## No discount ------------> 245

## How man Vegetables were sold and how many were returned ?
Sale_or_Return <- table(df2$`Sale or Return`)
Sale_or_Return %>%     ## 7 ---------------> were returned
  view()               ## 244 -------------> were sold 

### Find the total revenue made out of the sales
Sold_Vegetable <- filter(df2, df2$`Sale or Return` == "sale")
Sold_Vegetable %>% 
  view()
## Total revenue made
Total_Revenue <- sum(Sold_Vegetable$Total_Price)
Total_Revenue

## Find the amount of revenue in the goods returned ##
Returned_Vegetables <- filter(df2,df2$`Sale or Return` == "return")
Returned_Vegetables %>%  ## All the vegetables that were returned were not given discount
  view()

# Total Revenue in returns
Total_Revenue_in_returns <- sum(Returned_Vegetables$Total_Price)
Total_Revenue_in_returns

## Find those that were given discount and returned the goods
Returned_and_Discount <- filter(df2,df2$`Sale or Return` == "return" & df2$`Discount (Yes/No)` == "yes")
Returned_and_Discount %>% 
  view()  ## Those that were given discount did not return the vegetables

## Find those were given discount and did returned vegetables
Sale_and_Discounted <- filter(df2,df2$`Sale or Return`=="sale" & df2$`Discount (Yes/No)`=="yes")
Sale_and_Discounted %>% 
  view()

### Carring out some summary statistics
summary(df2)

summary(df2$Total_Price)
columns <- names(df2)
## Dealing with missing values
Stats <- function(data){
  for (col in columns){
    if (is.numeric(data[[col]])){
      print(summary(data[[col]]))
    }
  }
}
Stats(df2)

## Plotting only numeric columns
Box_plot <- function(data){
  for (col in columns) {
    if (is.double(data[[col]])){
      boxplot(df[[col]], main = col)
    }
    
  }
}
Box_plot(df2)

### Creating my own function
box <- function(data){
  ## Get the columns in the data
  columns <- names(data)
  ## setting the columns in the plot
  num_cols <- 3
  ## Setting the number of rows
  num_rows <- ceiling(length(columns) / num_cols)
  # Set up the plotting area
  par(mfrow = c(num_rows,num_cols))
  for (col in columns){
    if(is.numeric(data[[col]])){
      boxplot(data[[col]],main = col,xlab = col,col = "blue")
    }
  }
  par(mfrow = c(1,1))  # Reset plotting area to defualt
}
box(df2)

## Function to plot a multiple histogram
hist_plot <- function(data){
  # Finding the columns in the data set
  columns <- names(data)
  # Setting the number of columns in the data set
  num_cols <- 3
  # Calculating the number of rows in the plot
  num_rows <- ceiling(length(columns) / num_cols)
  # Setting the plotting area
  par(mfrow = c(num_rows,num_cols))
  for (col in columns){
    if (is.numeric(data[[col]])){
      hist(data[[col]],main = col,xlab = col,col = "lightblue")
    }
  }
  par(mfrow = c(1,1))
}
hist_plot(df2)

## Checking for missing values
sum(is.na(df2))

## Checking for outliers ##
box_plot <- function(data){
  ## Finding the columns in the data
  columns <- names(data)
  # Setting the nimber of columns in the plot
  num_cols <- 3
  # Calculating the number of rows to be in my plot
  num_rows <- ceiling(length(data)/ num_cols)
  # Setting the plotting area
  par(mfrow = c(num_rows,num_cols))
  for (col in columns){
    if (is.numeric(data[[col]])){
      boxplot(data[[col]],main = col, xlab = col)
    }
  }
  par(mfrow = c(1,1))
}
box_plot(df2)

## Dealing with the outliers
# Remove outlies in Loss Rate
summary(df2$`Loss Rate (%)`)
Q1 <- 8.225
Q3 <- 11.130
IQR <- Q3 - Q1

## Setting the boundaries
Tmin <- Q1 - 1.5 * IQR
Tmax <- Q3 + 1.5 * IQR

## Dealing with the outliers
no_outliers <- df2$`Loss Rate (%)`[df2$`Loss Rate (%)` <= Tmin & df$`Loss Rate (%)` >= Tmax]
boxplot(no_outliers)

## Function for removing the outliers
outliers <- function(data){
  columns <- names(data)
  for (col in columns){
    if (is.numeric(data[[col]])){
      ## Calculate first and third quantile
      Q1 <- quantile(data[[col]], 0.25, na.rm = TRUE)
      Q3 <- quantile(data[[col]], 0.75, na.rm = TRUE)
    }
  }
}