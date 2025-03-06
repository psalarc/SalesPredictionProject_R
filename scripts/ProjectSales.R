# Load of libraries
library(readr) # Read cvs
library(dplyr) # Handle data and use of pipes
library(tidyr) # Handle dataframes
library(ggplot2) # Useful library to visualizations
library(lubridate) # Handle with dates
library(randomForest) # Random Forest
library(Metrics) # Use of RMSE metric
library(ggcorrplot) # Correlation plot
library(caret) # ML library
library(scales) # To rescale output values
library(zoo)
library(xts)
library(tseries)
library(tictoc)

rm(list = ls(all.names = TRUE))

setwd("/Users/pablosalarcarrera/Desktop/Master Classes/DS636/")

# Load datasets
sales_train <- read.csv("sales_train.csv", header=T)
items <- read.csv("items.csv", header=T)
items_categories <- read.csv("item_categories.csv", header=T)
shops <- read.csv("shops.csv", header=T)

# Print first 5 rows of each dataset
head(sales_train)
head(items)
head(items_categories)
head(shops)

# Merge datasets into a unique dataset 
sales <- sales_train %>% left_join(items,by = "item_id") %>% left_join(items_categories,by = "item_category_id") %>% left_join(shops,by = "shop_id")
# Fix the format of date column
sales$date <- as.Date(sales$date,"%d.%m.%Y")

# We may want to know sales in terms of day, month, weekday and year, so create four new columns.
sales$day = day(sales$date)
sales$month = month(sales$date)
sales$weekday = wday(sales$date)
sales$year = year(sales$date)
# Transform IDs into categorical variables
sales$item_id = factor(sales$item_id)
sales$item_category_id = factor(sales$item_category_id)
sales$shop_id = factor(sales$shop_id)

# Print first rows of the new dataset
head(sales)

# Check missing values
sum(is.na(sales))
sum(duplicated(sales))
which(duplicated(sales))

# Eliminate duplicated values
sales <- sales[!duplicated(sales), ]

# Check lowest value in item price
min(sales$item_price)
# Price cannot be negative 
sales <- sales[sales$item_price>=0, ]

boxplot(sales$`item_price`,
        ylab = "Item price"
)
# Deal with outliers
sales <- sales[sales$item_price < 100000, ]
boxplot(sales$`item_price`,
        ylab = "Item price"
)
sales <- sales[sales$item_price < 40000, ]
boxplot(sales$`item_price`,
        ylab = "Item price"
)

glimpse(sales)

# Remove the columns with the names
sales <- sales %>% select(-item_name, -item_category_name, -shop_name)
glimpse(sales)
library(Hmisc)
describe(sales)

# Check how many different items
sales %>% select(item_id) %>% distinct() %>% count()

# We can extract the numbers of shops, products and categories
print(paste0("numbers of shops: ",length(unique(sales$shop_id))))
print(paste0("numbers of products: ",length(unique(sales$item_id))))
print(paste0("numbers of categories: ",length(unique(sales$item_category_id))))

library(plotly)
library(reactable)
library(htmlwidgets)
library('IRdisplay')
library(htmltools)

product_sold_Year<-sales%>%group_by(year)%>%summarise(Product_sold_year=sum(item_cnt_day))
product_sold_Year

v1 <- plot_ly(product_sold_Year, x = ~year, y = ~Product_sold_year, color = ~year, colors = c("#4C3A51", "#774360", "#B25068"), type = 'bar') %>%
  layout(title = 'Total Product Sold Per Year', yaxis = list(title = 'Total Item Sold'))

v1

sales_price_Year<-sales%>%group_by(year)%>%summarise(Sales_value_year=sum(item_price*item_cnt_day))
sales_price_Year

v4<-plot_ly(sales_price_Year, x=~year, y=~Sales_value_year, color = ~year, colors = c("#764AF1","#9772FB","#F2F2F2"), type='bar')%>% layout(title='Total Sales Value Per Year', yaxis=list(title='Total Sales Value'))

v4

data_aggr1 <- aggregate(cbind(item_price, item_cnt_day) ~ month + year, sales, FUN = sum)
head(data_aggr1)
# Convert the revenue per month to millions
data_aggr1$item_price <- round(data_aggr1$item_price/1000000)
# Convert the items sold per month to thousands
data_aggr1$item_cnt_day <- round(data_aggr1$item_cnt_day/1000)

colnames(data_aggr1) <- c('month','year','Revenue (in millions $)', 'Number of items sold (thousands)')
head(data_aggr1)
tail(data_aggr1)

forecasting_revenue <- data_aggr1 %>% select(`Revenue (in millions $)`)

df1 <- ts(forecasting_revenue,frequency=12,start=c(2013,1),end=c(2015,10))

head(df1)

library(ggfortify)
library(forecast)
revenue_yearly_plot <- autoplot(df1)+geom_smooth(method='loess') + ggtitle('Revenue of Sales of Products Yearly')+xlab('Year') +ylab('$ Millions')

revenue_seasonal_plot <- ggseasonplot(df1, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("$ million") +
  ggtitle("Seasonal plot: Product Sales Revenue")

revenue_seasonal_plot2 <- ggseasonplot(df1, polar=TRUE) +
  ylab("$ million") +
  ggtitle("Seasonal plot: Sale of Products Revenue")

forecasting_items_sold <- data_aggr1 %>% select(`Number of items sold (thousands)`)

df2 <- ts(forecasting_items_sold,frequency=12,start=c(2013,1),end=c(2015,10))

head(df2)
items_soldyearly_plot <- autoplot(df2)+geom_smooth(method='loess') + ggtitle('Sales of Products Yearly')+xlab('Year') +ylab('Items sold (Thousand)')
items_soldseasonal_plot <- ggseasonplot(df2, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("Items sold (Thousand)") +
  ggtitle("Seasonal plot: Product Sales")

items_soldseasonal_plot2 <- ggseasonplot(df2, polar=TRUE) +
  ylab("Items sold (Thousand)") +
  ggtitle("Seasonal plot: Sale of Products")

options(repr.plot.width = 14, repr.plot.height = 8)
library(gridExtra)
grid.arrange(revenue_yearly_plot, items_soldyearly_plot, nrow=2, ncol=1)
grid.arrange(revenue_seasonal_plot, items_soldseasonal_plot, nrow=2, ncol=1)
grid.arrange(revenue_seasonal_plot2, items_soldseasonal_plot2, nrow=2, ncol=1)

# Check how many years we have in the data
sales %>% select(year) %>% distinct() %>% count()

year_sales = sales%>%group_by(year)%>%
  summarise(sales_per_year = sum(item_price*item_cnt_day)) %>%
  arrange(year)%>%
  ungroup()
year_sales_plot <- ggplot(data=year_sales, aes(x = year, y = sales_per_year, fill = as.factor(year))) + 
  geom_bar(stat = "identity") + 
  labs(title= "Revenue Sales per Year", x= "Year", y = "Total Sales per Year", fill = "Year")


year_month_sales = sales%>%group_by(year, month)%>%
  summarise(total_sales = sum(item_price*item_cnt_day)) %>%
  arrange(year)%>%
  ungroup()
year_month_sales_plot <- ggplot(data=year_month_sales, aes(x = year, y = total_sales, fill = as.factor(month))) + 
  geom_bar(stat = "identity") + 
  labs(title= "Year-Month Revenue Sales", x= "Year", y = "Year-Month Revenue Sales", fill = "Month")
# We want to forecast sales for month 11 and 12


highest_sales_day = sales%>%group_by(date)%>%
  summarise(sales_per_day = sum(item_price*item_cnt_day)) %>%
  arrange(desc(sales_per_day))%>%
  ungroup()
highest_sales_day_plot <- ggplot(data=highest_sales_day, aes(x = date, y = sales_per_day)) + 
  geom_point(na.rm = TRUE, color = "blue", size = 0.3) + 
  (scale_x_date(breaks = date_breaks("7 months"), labels = date_format("%b %y"))) +
  labs(title= "Count of Items Sold per Day", x= "Date(s)", y = "Total sales per day")


grid.arrange(year_sales_plot, year_month_sales_plot, nrow=2, ncol=1)
highest_sales_day_plot


# Decomposing the time series
df_revenue <- decompose(df1)
plot(df_revenue)

df_n_sales <- decompose(df2)
plot(df_n_sales)

# Testing whether the time series is stationary or not
# Augmented Dickey-Fuller Test (adf test). A p-Value of less than 0.05 in adf.test() indicates that it is stationary.
adf.test(df1)
adf.test(df2)


# Load test data
sales_test <- read.csv("test.csv", header=T)

head(sales_test)

sales_test %>% select(item_id) %>% distinct() %>% count()

# Function to retrieve item_category_id based on item_id
get_item_category_id <- function(item_id, items) {
  category_id <- items$item_category_id[items$item_id == item_id]
  if (length(category_id) == 0) {
    return(NA) # Return NA if item_name not found
  } else {
    return(category_id)
  }
}

# Apply the function to the 'sales_test' dataframe
sales_test$item_category_id <- sapply(sales_test$item_id, get_item_category_id, items = items)

head(sales_test)
sales_test$shop_id <- factor(sales_test$shop_id)
sales_test$item_category_id <- factor(sales_test$item_category_id)
head(sales_test)

## Linear Regression Model for Number of Sales####
lr_items_model <- lm(item_cnt_day ~ shop_id + item_category_id, data = sales)

# Make predictions on sales_test data
sales_test_predictions <- predict(lr_items_model, newdata = sales_test)
sales_test_predictions

total_sales <- sum(sales_test_predictions)
total_sales <- total_sales/1000
total_sales

## Linear Regression Model for Revenue ####
lr_revenue_model <- lm(item_price ~ shop_id + item_category_id, data = sales)

# Make predictions on sales_test data
sales_test_predictions_revenue <- predict(lr_revenue_model, newdata = sales_test)
total_revenue <- sum(sales_test_predictions_revenue)
total_revenue <- total_revenue/1000000
total_revenue

library(lightgbm)

# Lightgbm model for revenue prediction
# Define the training data
train_data_revenue <- lgb.Dataset(data = as.matrix(sales[, c("shop_id", "item_category_id")]),
                          label = sales$item_price)

# Set LightGBM parameters
params <- list(objective = "regression",
               metric = "rmse",
               num_leaves = 31,
               learning_rate = 0.05,
               feature_fraction = 0.9,
               bagging_fraction = 0.8,
               bagging_freq = 5,
               verbose = -1)

# Train the LightGBM model
lgb_model_revenue <- lgb.train(params = params,
                       data = train_data_revenue,
                       nrounds = 100)

# Make predictions on the sales_test dataset
revenue_test_predictions_lgb <- predict(lgb_model_revenue, as.matrix(sales_test[, c("shop_id", "item_category_id")]))
total_revenue_lgb <- sum(revenue_test_predictions_lgb)
total_revenue_lgb <- total_revenue_lgb/1000000
total_revenue_lgb

# Lightgbm model for number of sales prediction
# Define the training data
train_data_sales <- lgb.Dataset(data = as.matrix(sales[, c("shop_id", "item_category_id")]),
                          label = sales$item_cnt_day)

# Set LightGBM parameters
params <- list(objective = "regression",
               metric = "rmse",
               num_leaves = 31,
               learning_rate = 0.05,
               feature_fraction = 0.9,
               bagging_fraction = 0.8,
               bagging_freq = 5,
               verbose = -1)

# Train the LightGBM model
lgb_model_sales <- lgb.train(params = params,
                       data = train_data_sales,
                       nrounds = 100)

# Make predictions on the sales_test dataset
sales_test_predictions_lgb <- predict(lgb_model_sales, as.matrix(sales_test[, c("shop_id", "item_category_id")]))
total_sales_lgb <- sum(sales_test_predictions_lgb)
total_sales_lgb <- total_sales_lgb/1000
total_sales_lgb


# 1 year Time Series Predictions for Revenue 
df1_train <- head(df1, length(df1) - 9)
df1_test <- tail(df1, 9)

df1_test

df1_train_ts <- ts(df1_train, start=c(2013, 1), end=c(2015, 10), frequency=12)

df1_test_ts <- ts(df1_test,start=c(2013, 1), end=c(2016, 10), frequency=12)

df1_naive_mod <- naive(df1_train_ts, h = 12)
summary(df1_naive_mod)

df1_test$naive <- 73

df1_test$naive<- as.integer(df1_test$naive)
df1_res_naive <- mape(df1_test$naive, df1_test_ts) * 100
df1_res_naive

plot(df1,  main="Forecast for Yearly/Monthly Revenue", xlab="Time", ylab="Millions of Dollars")

lines(df1_naive_mod$mean, col=4) #Naive method prediction



df1_se_model <- ses(df1_train_ts, h = 12)
summary(df1_se_model)

df1_test$se <- 68.94

df1_test$se <- as.integer(df1_test$se)
df1_res_se <- mape(df1_test$se, df1_test_ts)*100

df1_res_se

autoplot(df1_se_model) +
  autolayer(fitted(df1_se_model), series="Fitted") +
  ylab("Revenue (millions of Dollars)") + xlab("Year")

df1_holt_model <- holt(df1,h=12)
df1_holt_model
df1_holt_model$mean

df1_holt <- as.data.frame(df1_holt_model)

df1_test$holt <- df1_holt$`Point Forecast`
df1_res_holt <- mape(df1_test$holt, df1_test_ts)*100
df1_res_holt

df1_holt_model_damp <- holt(df1,damped = TRUE ,phi = 0.9,h=15)

df1_holt_model_damp

autoplot(df1) +
  autolayer(df1_holt_model, series="Holt's method", PI=FALSE) +
  autolayer(df1_holt_model_damp, series="Damped Holt's method", PI=FALSE) +
  ggtitle("Revenue forecasts from Holt's method") + xlab("Year") +
  ylab("Revenue in Millions $)") +
  guides(colour=guide_legend(title="Forecast"))

library(DT)

Model_df1 <- c('Naive','Simple Exponential Smoothing','Holt Trend Method')

MAPE_df1 <- c(df1_res_naive,df1_res_se,df1_res_holt)

df1_sales <- data.frame(Model_df1, MAPE_df1)

datatable(df1_sales)


# 1 year Time Series Predictions of number of items sold
df2_train <- head(df2, length(df2) - 12)
df2_test <- tail(df2, 12)

df2_test

df2_train_ts <- ts(df2_train, start=c(2013, 1), end=c(2015, 10), frequency=12)

df2_test_ts <- ts(df2_test,start=c(2013, 1), end=c(2016, 10), frequency=12)

df2_naive_mod <- naive(df2_train_ts, h = 12)
summary(df2_naive_mod)

df2_test$naive <- 133

df2_test$naive<- as.integer(df2_test$naive)
df2_res_naive <- mape(df2_test$naive, df2_test_ts) * 100
df2_res_naive

plot(df2,  main="Forecast for Yearly/Monthly Items Sold", xlab="Time", ylab="Thousands of Sales")

lines(df2_naive_mod$mean, col=4) #Naive method prediction


df2_se_model <- ses(df2_train_ts, h = 12)
summary(df2_se_model)

df2_test$se <- 120.97

df2_test$se <- as.integer(df2_test$se)
df2_res_se <- mape(df2_test$se, df2_test_ts)*100

df2_res_se

autoplot(df2_se_model) +
  autolayer(fitted(df2_se_model), series="Fitted") +
  ylab("Thousands of Sales") + xlab("Year")

df2_holt_model <- holt(df2,h=12)
df2_holt_model
df2_holt_model$mean

df2_holt <- as.data.frame(df2_holt_model)

df2_test$holt <- df2_holt$`Point Forecast`
df2_res_holt <- mape(df2_test$holt, df2_test_ts)*100
df2_res_holt

df2_holt_model_damp <- holt(df2,damped = TRUE ,phi = 0.9,h=15)

df2_holt_model_damp

autoplot(df2) +
  autolayer(df2_holt_model, series="Holt's method", PI=FALSE) +
  autolayer(df2_holt_model_damp, series="Damped Holt's method", PI=FALSE) +
  ggtitle("Sales forecasts from Holt's method") + xlab("Year") +
  ylab("Thousands of Sales") +
  guides(colour=guide_legend(title="Forecast"))


Model_df2 <- c('Naive','Simple Exponential Smoothing','Holt Trend Method')

MAPE_df2 <- c(df2_res_naive,df2_res_se,df2_res_holt)

df2_sales <- data.frame(Model_df2, MAPE_df2)

datatable(df2_sales)

summary(lgb_model_sales)

