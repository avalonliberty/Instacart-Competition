# Calling required packages
library(data.table)
library(pipeR)
library(plyr)
library(mlr)


# Read in data
FilePath <- "/Users/Avalon/Downloads/InstaCartProject/"
aisles <- paste0(FilePath, "aisles.csv") %>>% fread(nThread = 4)
department <- paste0(FilePath, "departments.csv") %>>% fread(nThread = 4)
orders <- paste0(FilePath, "orders.csv") %>>% fread(nThread = 4)
products <- paste0(FilePath, "products.csv") %>>% fread(nThread = 4)
prior <- paste0(FilePath, "order_products__prior.csv") %>>% fread(nThread = 4)
train <- paste0(FilePath, "order_products__train.csv") %>>% fread(nThread = 4)


# Reshaping and preprocessing data -------------------------------------------
products <- products[department, on = "department_id"][aisles, on = "aisle_id"]
products[, `:=`(aisle_id = NULL, department_id = NULL)]
products[, `:=`(department = as.factor(department),
                aisle = as.factor(aisle))]
prior_products <- orders[prior, on = "order_id"]
train$user_id <- orders$user_id[match(train$order_id, orders$order_id)]
rm(department, aisles, FilePath, prior)
gc()


# Generating products information --------------------------------------------
prior_products <- prior_products[order(user_id, order_number, product_id)]
prior_products[, Purchase_Times := rowid(product_id), user_id]
products_info <- 
  prior_products[, .(order_times = .N,
                     reordered_times = sum(reordered),
                     first_purchase = sum(Purchase_Times == 1),
                     second_purchase = sum(Purchase_Times == 2)),
                 product_id]
products_info[, `:=`(reorder_ratio = reordered_times / order_times,
                     reorder_probability = second_purchase / first_purchase,
                     reorder_times = 1 + reordered_times / first_purchase)]
products_info <- products_info[, .(product_id, order_times, reorder_ratio,
                                   reorder_probability, reorder_times)]
rm(products)
gc()


# Generating Users information -----------------------------------------------
User_Info <- orders[eval_set == "prior"] %>>% {
  .[, .(User_Purchase_Times = max(order_number),
        Purchase_Stint = sum(days_since_prior_order, na.rm = TRUE),
        Mean_Purchase_Period = mean(days_since_prior_order, na.rm = TRUE),
        Largest_Purchase_day = max(days_since_prior_order, na.rm = TRUE),
        Smallest_Purchase_day = min(days_since_prior_order, na.rm = TRUE)),
    user_id]
}
user <- prior_products[, .(TotalOrder = .N,
                           User_Reorder_Ratio = sum(reordered == 1) /
                             sum(order_number > 1),
                           User_dinstint_Item = unique(product_id) %>>% length,
                           User_Unique_Account_Rate = 
                             (unique(product_id) %>>% length) / length(product_id)),
                       user_id]
User_Info <- user[User_Info, on = "user_id"]
User_Info[, Average_Item_Purchase := TotalOrder / Purchase_Times]
user <- orders[eval_set != "prior"][, .(eval_set), .(user_id, order_id)]
User_Info <- user[User_Info, on = "user_id"]
rm(user)
gc()


# Generating training and testing set ----------------------------------------
Final_DataSet <- prior_products[, .(Average_order_in_cart =
                                      mean(add_to_cart_order),
                                    Product_Purchase_ByUser = .N,
                                    Product_Purchase_FirstOrderNum = min(order_number),
                                    Product_Purchase_LastOrderNum = max(order_number)),
                                .(user_id, product_id)]
Final_DataSet <- products_info[Final_DataSet, on = "product_id", nomatch = 0]
Final_DataSet <- User_Info[Final_DataSet, on = "user_id", nomatch = 0]
train[, `:=`(add_to_cart_order = NULL,
             order_id = NULL)]
Final_DataSet[, `:=`(Product_Account_Rate_ByUser = Product_Purchase_ByUser /
                       User_Purchase_Times,
                     Item_Last_Purchase_Dinstinction = User_Purchase_Times -
                       Product_Purchase_LastOrderNum,
                     Purchase_Rate_Since_First_Purchase = Product_Purchase_ByUser /
                       (User_Purchase_Times - Product_Purchase_FirstOrderNum + 1),
                     Product_Purchase_Mean_Stint = Purchase_Stint / 
                       Product_Purchase_ByUser
                       )]
Final_DataSet <- Final_DataSet %>>% 
  dplyr::left_join(train, by = c("user_id", "product_id")) %>>% setDT
Final_DataSet <- 
  orders[, .(order_id)][Final_DataSet, on = "order_id"]

## Generating whether purchasing these prodocuts or not since last few times
LastOneTemp <- prior_products[, .(order_number = max(order_number)), user_id]
LastOnePurchase <- prior_products[, .(user_id, order_number, product_id)] %>>% {
  .[LastOneTemp, on = c("user_id", "order_number"), nomatch = 0]
}
LastTwoTemp <- prior_products[, .(order_number = max(order_number) - 1), user_id]
LastTwoPurchase <- prior_products[, .(user_id, order_number, product_id)] %>>% {
  .[LastTwoTemp, on = c("user_id", "order_number"), nomatch = 0]
}
LastThreeTemp <- prior_products[, .(order_number = max(order_number) - 2), user_id]
LastThreePurchase <- prior_products[, .(user_id, order_number, product_id)] %>>% {
  .[LastThreeTemp, on = c("user_id", "order_number"), nomatch = 0]
}
LastOnePurchase[, `:=`(order_number = NULL, LastOnePurchaseOrNot = 1)]
LastTwoPurchase[, `:=`(order_number = NULL, LastTwoPurchaseOrNot = 1)]
LastThreePurchase[, `:=`(order_number = NULL, LastThreePurchaseOrNot = 1)]
Final_DataSet <- merge(Final_DataSet, LastOnePurchase, all.x = TRUE) %>>%
  merge(LastTwoPurchase, all.x = TRUE) %>>%
  merge(LastThreePurchase, all.x = TRUE)
Final_DataSet[is.na(LastOnePurchaseOrNot), LastOnePurchaseOrNot := 0]
Final_DataSet[is.na(LastTwoPurchaseOrNot), LastTwoPurchaseOrNot := 0]
Final_DataSet[is.na(LastThreePurchaseOrNot), LastThreePurchaseOrNot := 0]
Final_DataSet <- orders[, .(order_id, days_since_prior_order)][Final_DataSet,
                                                               on = "order_id",
                                                               nomatch = 0]
Final_DataSet[, `:=`(Product_Purchase_Stint_Ratio = days_since_prior_order / 
                       Product_Purchase_Mean_Stint,
                     Product_Purchase_Mean_Stint = NULL)]
Final_DataSet[is.nan(Product_Purchase_Stint_Ratio) | 
                is.infinite(Product_Purchase_Stint_Ratio),
              Product_Purchase_Stint_Ratio := 0]
training_set <- Final_DataSet[eval_set == "train"]
testing_set <- Final_DataSet[eval_set == "test"]
rm(Final_DataSet, orders, prior_products, products_info, train, User_Info,
   LastOnePurchase, LastTwoPurchase, LastThreePurchase, LastOneTemp,
   LastTwoTemp, LastThreeTemp)
gc()


# Preprocessing the traing and testing sets ----------------------------------
training_set[, `:=`(user_id = NULL,
                    order_id = NULL,
                    product_id = NULL,
                    eval_set = NULL)][is.na(reordered), reordered := 0]
testing_set[, `:=`(user_id = NULL,
                   reordered = NULL,
                   eval_set = NULL)]


# deploying model ------------------------------------------------------------
h2o::h2o.init(nthreads = 3)
Task <- makeClassifTask(data = training_set,
                        target = "reordered",
                        positive = "TRUE")
Learner <- makeLearner("classif.h2o.gbm")
Model <- train(task = Task, learner = Learner)
Pred <- predict(Model, newdata = testing_set[, -c("order_id", "product_id"),
                                             with = FALSE])
h2o::h2o.shutdown()


# Generating submit file -----------------------------------------------------
submit <- data.table(order_id = testing_set$order_id,
                     product_id = testing_set$product_id,
                     Purchase = Pred$data$response)
submit <- submit[Purchase != 0] %>>% {
  .[, .(products = paste(product_id, collapse = " ")), order_id]
}
submit <- rbind(submit,
    data.table(order_id = testing_set$order_id[!testing_set$order_id %in% submit$order_id] %>>% unique,
               products = "None"))
submit <- submit[order(order_id)]
fwrite(submit, "D:/submit.csv", row.names = FALSE)
