#Week 6: Final Project
#Dataset used: Bigmart.csv


#Checking for and installing required packages

if(!require(onehot)){
  install.packages('onehot')
  library(onehot)
}

if(!require(glmnet)){
  install.packages('glmnet')
  library(glmnet)
}

#Loading the dataset bigmart.csv

dataset <- read.csv(file = file.choose(),                     #Choosing file
                    header = TRUE,                            #Columns in loaded file have names
                    na.strings = c("", " ", "?", "NA", NA))   #Treat "", " ", "?", "NA" values in file as NA


#CLEANING THE DATASET

dataset_clean <- dataset                #Initiating the process of cleaning the dataset

str(dataset_clean)                        #Displaying the structure of the dataset

column <- c(3, 5, 7, 9, 10, 11)           #Selecting the columns which are factors

for(i in column){                         #Displaying the count of unique values in each column
  cat("\n\n",
      colnames(dataset_clean[i]),         #Displaying the column name
      "- Frequency of Unique Values:\n")
  
  print(table(dataset_clean[, i]))        #Generating a contigency table and displaying the values
}



fats <- dataset_clean$Item_Fat_Content    #Selecting the 'Item_Fat_Content' column

unique(fats)                              #Displaying the names of unique values in 'Item_Fat_Content' column

dataset_clean$Item_Fat_Content[fats == "LF"] <- "Low Fat"       #Replacing 'LF' with 'Low Fat' in 'Item_Fat_Content' column

dataset_clean$Item_Fat_Content[fats == "low fat"] <- "Low Fat"  #Replacing 'low fat' with 'Low Fat' in 'Item_Fat_Content' column

dataset_clean$Item_Fat_Content[fats == "reg"] <- "Regular"      #Replacing 'reg' with 'Regular' in 'Item_Fat_Content' column

dataset_clean$Item_Fat_Content <- droplevels(dataset_clean$Item_Fat_Content)      #Removing the unused levels for 'LF', 'low fat', 'reg'

unique(dataset_clean$Item_Fat_Content)                          #Displaying the names of unique values in 'Item_Fat_Content' column

mis_val <- function(data){                                      #Function to count missing values in each column
  
  mis_val_count <- colSums(is.na(data))                         #Counting the nubmber of missing values in each column
  
  mis_val_percent <- (mis_val_count / nrow(data)) * 100         #Computing the percent of missing values for each column
  
  print(cbind(mis_val_count, mis_val_percent))                  #Displaying the count and percent of missing values
  
}
mis_val(dataset_clean)                              #Displaying the missing values for dataset using the defined function



weight <- dataset_clean$Item_Weight                 #Selecting the 'Item_Weight' column

summary(weight)                                     #Displaying statistical summary for 'Item_Weight' to show count of missing values and mean

con_1 <- is.na(weight)                              #Condition checking count of missing values in 'Item_Weight' column

weight_mean <- mean(weight[!con_1])                 #Computing the weight of available values in 'Item_Weight' column

dataset_clean$Item_Weight[con_1] <- weight_mean     #Replacing missing values with computed 'mean'

summary(dataset_clean$Item_Weight)                  #Displaying statistical summary for 'Item_Weight' column to show count of missing values and mean



size <- dataset_clean$Outlet_Size                   #Selecting the 'Outlet_Size' column

summary(size)                                       #Displaying the statistical summary for 'weight'Outlet_Type' to show count of missing values

type <- dataset_clean$Outlet_Type                   #Selecting the 'Outlet_Type' column

type_uniq <- sort(unique(type), decreasing = FALSE) #Displaying the sorted names of unique values in 'Outlet_Type' column

mode <- rep(" ", length(type_uniq))                 #Initializing 'mode' vector to contain mode of values

for(i in 1:length(type_uniq)){                      #Looping to find mode of different values in 'Outlet_Type' column
  
  con_2 <- type == type_uniq[i]                     #Condition to count
  
  con_3 <- is.na(size)                              #Condition for missing values
  
  size_nna <- size[con_2 & !con_3]                  #Counting values which are available
  
  size_uniq <- unique(size_nna)                     #Counting number of missing values
  
  count <- 1:length(size_uniq)                      #Initializing 'count' vector to contain count of values
  
  for(j in 1:length(size_uniq))                                      #Looping to find mode of each unique value in 'Outlet_Type' column
    
    count[j] <- length(size_nna[size_nna == size_uniq[j]])           #Counting number of each unique value
  
  mode_index <- which(max(count) == count)                           #Finding the mode
  
  dataset_clean$Outlet_Size[con_2 & con_3] <- size_uniq[mode_index]  #Replacing missing values in 'Outlet_Size' with mode
  
  mode[i] <- as.character(size_uniq[mode_index])                     #'mode' contains mode for all unique values
  
}
mode_data <- cbind(as.character(type_uniq), mode)                    #Adjusting values and mode

colnames(mode_data) <- c("Outlet_Type", "Outlet_Size - Mode")        #Setting names for columns

print(mode_data)                                                     #Displaying modes

summary(dataset_clean$Outlet_Size)                                   #Displaying count of unique values in 'Outlet_Size' column

mis_val(dataset_clean)                                               #Displaying count to verify that all missing values have been replaced



vis <- dataset_clean$Item_Visibility                        #Selecting the 'Item_Visibility' column

con_4 <- vis == 0                                           #Condition to check zero visibility

cat("Frequency of <zero> Item_Visibility:",
    length(vis[con_4]))                                     #Displaying count of zero values

vis_mean_nz <- mean(vis[!con_4])                            #Computing mean of non-zero values in 'Item_Visibility' column

dataset_clean$Item_Visibility[con_4] <- vis_mean_nz         #Replacing zero vales with mean of non-zero values

cat("Frequency of <zero> Item_Visibility:",
    length(dataset_clean$Item_Visibility[dataset_clean$Item_Visibility == 0]))  #Displaying count of zero values after replacing



Item_Type_New <- substr(as.character(dataset_clean$Item_Identifier), 0, 2)          #Getting the first two letters of 'Item_Identifier'

dataset_clean <- cbind(dataset_clean, Item_Type_New)                                #Creating new column with with the obtained two letters

type_new <- dataset_clean$Item_Type_New                                             #Selecting the 'Item_Type_New' column

levels(dataset_clean$Item_Fat_Content) <- c(levels(dataset_clean$Item_Fat_Content), #Adding new level "Non-Consumable"
                                            "Non-Consumable")

dataset_clean$Item_Fat_Content[type_new == "NC"] <- "Non-Consumable"                #Adding 'Non-Consumable' for items with 'NC' in newly generated column

summary(dataset_clean$Item_Fat_Content)                        #Displaying count of unique values in 'Item_Fat_Content' column 


walmart_new <- dataset_clean   #changing the dataset name


#Exploratory Data Analysis

install.packages("tidyverse")
library(tidyverse)



#bargraph for outlet type
ggplot(walmart_new,aes(x=walmart_new$Outlet_Type,fill=walmart_new$Outlet_Type)) +geom_bar() + xlab("Outlet Type")+ylab('Frequency')  + labs(title="Number of items sold for each outlet type")

#bargraph for location type 
ggplot(walmart_new,aes(x=walmart_new$Outlet_Location_Type,fill=walmart_new$Outlet_Location_Type))+geom_bar() + xlab("Location Type")+ylab('Frequency') + labs(title="Number of items sold for each location")

#bargraph for outlet size 
ggplot(walmart_new,aes(x=walmart_new$Outlet_Size,fill=walmart_new$Outlet_Size))+geom_bar() + xlab("Outlet size")+ylab('Frequency') + labs(title="Number of items sold for each outlet size")

#bargraph for item type
ggplot(walmart_new,aes(x=walmart_new$Item_Type,fill=walmart_new$Item_Type, horizontal = FALSE))+geom_bar()+ xlab("Item Type")+ylab('Number of Purchases') +
  coord_flip() + labs(title="Number of items sold for each item type")

#bar graph for fat content
ggplot(walmart_new,aes(x=walmart_new$Item_Fat_Content,fill=walmart_new$Item_Fat_Content))+geom_bar()+ xlab("Fat Content")+ylab('Frequency') + labs(title="Number of items sold as per fat content")





#creating function to perform two sample z-test
z_test2 = function(a, b, var_a, var_b){
  n.a = length(a)
  n.b = length(b)
  z = (mean(a) - mean(b)) / (sqrt((var_a)/n.a + (var_b)/n.b))
  return(z)
}


#Perform Z-test 
z_test2(walmart_new$Item_Outlet_Sales[walmart_new$Item_Fat_Content=="Regular"],walmart_new$Item_Outlet_Sales[walmart_new$Item_Fat_Content=="Low Fat"],var(walmart_new$Item_Outlet_Sales[walmart_new$Item_Fat_Content=="Regular"]),var(walmart_new$Item_Outlet_Sales[walmart_new$Item_Fat_Content=="Low Fat"]))


#plot the z-statistic
ggplot(data.frame(x = c(-3, 3)), aes(x)) + 
  stat_function(fun = dnorm, geom = "area",  fill = "blue", 
                args = list(mean = 0, sd = 1), xlim = c(-1.96, 1.96), alpha = 0.5) +
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1), size = 1.25) + 
  geom_vline(xintercept = -1.96, linetype = "dashed", color = "red", size = 1) +
  geom_vline(xintercept = 1.96, linetype = "dashed", color = "red", size = 1) + 
  theme_light() +
  geom_vline(xintercept = 1.72121, color = "blue", size = 1) +
  xlab('') +
  ylab('')




#correlation between numeric fields
walmart_cr <- walmart_new[c(2,4,6,8,12)]
install.packages("DataExplorer")
library(DataExplorer)
plot_correlation(walmart_cr)





#Preparing the data for clustering. 
walmart_clust <- walmart_new[c(6,12)]

# Now we are trying to estimate K using Elbow approach 
# we are trying to find K means starting with K value =1 to 10  
# Creating a for loop from 1:10 
kvalue<-vector("numeric",length=10)
for (i in 1:10) {
  kdet<-kmeans(walmart_clust,i)
  kvalue[i] <- kdet$tot.withinss
}

# With the availabe Total within-cluster sum of squares we are ploting them to find the best number of clusters 
ggplot(as.data.frame(kvalue), aes(x=seq(1,10),y= kvalue)) + geom_point() +geom_line() + labs(title="Finding the best K value")

#It can be seen from the graph above that a reasonable selection for the K value would be the k = 5. 
#Hence, we are going to create 5 clusters to generate our segments.

#sample created for clustering 
set.seed(42)
sample1 <- sample_n(walmart_clust,1000)

# Base R kmeans and plot ------------------------------------------------------------
mykmeansObject <- kmeans(sample1, centers = 5)

# Base R plot
plot(sample1, col = mykmeansObject$cluster)

# adding the cluster package -------------------------------------------------------
install.packages("cluster")
library(cluster)

# plotting the data set from above
clusplot(sample1, mykmeansObject$cluster, main = "Cluster plot") # using cluster package
#-----------------------------------------------------------------------------------





#plot sales vs item visibility
ggplot(data = walmart_new) +
  geom_point(mapping = aes(x = Item_Visibility, y = Item_Outlet_Sales) ,
             col = "BROWN") + xlab('Visibility') + ylab('Sales') + labs(title="Visibility-sales scatter plot")




Years <- 2013 - dataset_clean$Outlet_Establishment_Year        #Computing age of each outlet

dataset_clean <- cbind(dataset_clean, Years)                   #Adding new column for age of outlet

par(mfrow = c(1, 1))                                           #Resetting graphical layout


hist(dataset_clean[, 4],                                       #Plotting histogram for 'Visibility Area of Products'
     xlab = "Percent of Visibility Area of Products",        #Label of x axis
     ylab = "Frequency",                                                #Label of y axis
     main = "Distribution of Percent of Visibility Area of Products")   #Title of histogram

hist(dataset_clean[, 12],                                      #Plotting histogram for 'Total Sales of Products'
     xlab = "Sales of Products (in $ 1k USD)",                 #Label of x axis
     ylab = "Frequency",                                       #Label of y axis
     main = "Distribution of Total Sales of Products")         #Title of histogram

type_new_uniq <- unique(type_new)                              #Selecting unique new items according to 'Item_Fat_content' column

type_new_sales <- 1:length(type_new_uniq)                      #Selecting sales according to 'Item_Fat_content' column

for(i in 1:length(type_new_uniq)){                                                #Looping for each new item type
  
  tmp <- dataset_clean[ dataset_clean[, "Item_Type_New"] == type_new_uniq[i], ]   #Selecting matching unique new items
  
  type_new_sales[i] <- sum(tmp$Item_Outlet_Sales)              #Computing total sales for each new item type
  
}

names(type_new_sales) <- type_new_uniq                         #Setting names

barplot(type_new_sales,                                        #Displaying barplot for sales of products based on 'Item_Fat_Content'
        main = "Sales of Categories of Products",              #Title of barplot
        xlab = "Category of Products",                         #Label of x axis
        ylab = "Sales (in $ 1000k USD)",                       #Label of y axis
        names.arg=c("Food", "Drinks", "Non-Consumables"))      #Labels of each bars


#calculating mean by boxplots
#boxplot for item fat content and sales
boxplot(Item_Outlet_Sales ~ Item_Fat_Content, data = walmart_new)
#boxplot for outlet type and sales
boxplot(Item_Outlet_Sales ~ Outlet_Type, data = walmart_new)
#boxplot for location type and sales
boxplot(Item_Outlet_Sales ~ Outlet_Location_Type, data = walmart_new)


#Generating One Hot Encoder for Linear Regression

#Supressing warning for removed columns
options(warn = -1)
encoder <- onehot(dataset_clean, max_levels = 11)
options(warn = 0)

data <- predict(encoder, dataset_clean)

data_sales <- data[, 28]                              #Response variable, which is 'Sales'

data <- data[, -28]                                   #Predictors



cross_val <- cv.glmnet(data,                          #Performing cross-validation linear regression
                       data_sales,                    #Specifying response variable
                       nlambda = 500)                 #Number of lambda values to be used

#Building linear regression model

lmd_1se_cv <- glmnet(data,                            #Using greater lambda value within one standard error from the minimum value
                     data_sales,                      #Specifying response variable
                     nlambda = cross_val$lambda.1se)  #Specifying lambda value

plot.glmnet(lmd_1se_cv,                               #Plotting curve for coefficients for Lasso regularization
            xvar = "norm",                            #Specifying 'norm'
            label = TRUE,                             #Displaying labels for each coefficient curve
            main = "Lasso Regularization  - Coefficients")  #Specifying title of plot

