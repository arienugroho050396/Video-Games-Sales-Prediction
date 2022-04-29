
#=========================
#INSTALL AND IMPORT LIBRARY
#=========================

# Installing Packages
install.packages("dplyr")
install.packages("tidyr")
install.packages("stringr")
install.packages("ggplot2")
install.packages("corrplot")
install.packages("dummies")
install.packages("broom")
install.packages("mice")
install.packages("DataExplorer")
install.packages("gridExtra")
install.packages("caret")
install.packages("RColorBrewer")
install.packages("missForest")
install.packages("caTools")
install.packages("jtools")
install.packages("randomForest")
install.packages("e1071")
install.packages("ROCR")
install.packages("klaR")

# Importing Libraries
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(corrplot)
library(dummies)
library(broom)
library(mice)
library(DataExplorer)
library(gridExtra)
library(caret)
library(RColorBrewer)
library(missForest)
library(caTools)
library(jtools)
library(randomForest)
library(e1071)
library(ROCR)
library(klaR)

#=========================
#READING DATASET
#=========================

#After importing libraries, the dataset will be imported as well. A glimpse of imported datasets will also shown in this section.
sales_dataset <- read.csv('Video_Games_Sales_as_at_22_Dec_2016.csv')
glimpse(sales_dataset)

#=========================
#INITIAL DATA EXPLORATION
#=========================

# Changing 'character' columns to factor/numeric
sales_dataset$Platform=as.factor(sales_dataset$Platform)
sales_dataset$Year_of_Release=as.numeric(sales_dataset$Year_of_Release)
sales_dataset$Genre=as.factor(sales_dataset$Genre)
sales_dataset$Publisher=as.factor(sales_dataset$Publisher)
sales_dataset$Developer=as.factor(sales_dataset$Developer)
sales_dataset$Rating=as.factor(sales_dataset$Rating)

## 1. Categorical Variable
# The first type of variable that I will explore is categorical variable.

# 1.1 Name
sales_dataset %>% summarise(n_distinct(Name))
# - There are 11563 unique video games title in the dataset.

# 1.2 Platform
sales_dataset %>%
  group_by(Platform) %>%
  summarise(Count = n(),Perc=round(n()/nrow(.)*100,2)) %>%
  arrange(desc(Count))
# - There are 31 platforms of video games. Platform distribution can be seen above.
# - In this dataset, PS2 have the highest percentage from all platforms (12.93%).

# 1.3 Genre
sales_dataset %>%
  group_by(Genre) %>%
  summarise(Count = n(),Perc=round(n()/nrow(.)*100,2)) %>%
  arrange(desc(Count))
# - There are 13 different genres and 'Action' is the most popular genre (20.16%).

# 1.4 Publisher
sales_dataset %>%
  group_by(Publisher) %>%
  summarise(Count = n(),Perc=round(n()/nrow(.)*100,2)) %>%
  arrange(desc(Count))
# - There are 582 publishers of video games. 'Electronic Arts' is the most popular publisher (8.11%).

# 1.5 Developer
sales_dataset %>%
  group_by(Developer) %>%
  summarise(Count = n(),Perc=round(n()/nrow(.)*100,2)) %>%
  arrange(desc(Count))
# - There are 1697 developers of video games. In this dataset, 'Ubisoft' is the most popular developer (39.61%).

# 1.6 Rating
sales_dataset %>%
  group_by(Rating) %>%
  summarise(Count = n(),Perc=round(n()/nrow(.)*100,2)) %>%
  arrange(desc(Count))
# - There are 9 ratings of video games based on ESRB ratings.
# - In this dataset, 'E' (everyone) rating is the highest rating in dataset (23.87%).
# - There are also some null value in Rating column, this will be imputed in data manipulation process.

## 2. Numerical Variable
# The second variable that I will explore is numerical variable.

# 2.1 NA_Sales
sales_dataset %>%
  summarise(is_NULL=sum(is.na(NA_Sales)==1),
            is_NOT_NULL=sum(!is.na(NA_Sales)==1)
  )

sales_dataset%>%
  filter(!is.na(NA_Sales))%>%
  summarise(
    Max=max(NA_Sales),
    Min=min(NA_Sales),
    Mean=mean(NA_Sales),
    Median=median(NA_Sales),
    QUA1=quantile(NA_Sales,1/4),
    QUA3=quantile(NA_Sales,3/4),
    IQR=IQR(NA_Sales)
  )
# - There is no null value on NA_Sales column. However, for minimum value, there are "0" values on column.

# 2.2 EU_Sales
sales_dataset %>%
  summarise(is_NULL=sum(is.na(EU_Sales)==1),
            is_NOT_NULL=sum(!is.na(EU_Sales)==1)
  )

sales_dataset%>%
  filter(!is.na(EU_Sales))%>%
  summarise(
    Max=max(EU_Sales),
    Min=min(EU_Sales),
    Mean=mean(EU_Sales),
    Median=median(EU_Sales),
    QUA1=quantile(EU_Sales,1/4),
    QUA3=quantile(EU_Sales,3/4),
    IQR=IQR(EU_Sales)
  )
# - There is no null value on EU_Sales column but for minimum value, there are "0" values on column.

# 2.3 JP_Sales
sales_dataset %>%
  summarise(is_NULL=sum(is.na(JP_Sales)==1),
            is_NOT_NULL=sum(!is.na(JP_Sales)==1)
  )

sales_dataset %>%
  filter(!is.na(JP_Sales))%>%
  summarise(
    Max=max(JP_Sales),
    Min=min(JP_Sales),
    Mean=mean(JP_Sales),
    Median=median(JP_Sales),
    QUA1=quantile(JP_Sales,1/4),
    QUA3=quantile(JP_Sales,3/4),
    IQR=IQR(JP_Sales)
  )
# - There is no null value on JP_Sales column.
# - However, for minimum value, there is "0" values on column.

# 2.4 Other_Sales
sales_dataset %>%
  summarise(is_NULL=sum(is.na(Other_Sales)==1),
            is_NOT_NULL=sum(!is.na(Other_Sales)==1)
  )

sales_dataset %>%
  filter(!is.na(Other_Sales))%>%
  summarise(
    Max=max(Other_Sales),
    Min=min(Other_Sales),
    Mean=mean(Other_Sales),
    Median=median(Other_Sales),
    QUA1=quantile(Other_Sales,1/4),
    QUA3=quantile(Other_Sales,3/4),
    IQR=IQR(Other_Sales)
  )
# - There is no null value on Other_Sales column but for minimum value, there are "0" values on column.

# 2.5  Global Sales
sales_dataset %>%
  summarise(is_NULL=sum(is.na(Global_Sales)==1),
            is_NOT_NULL=sum(!is.na(Global_Sales)==1)
  )

sales_dataset%>%
  filter(!is.na(Global_Sales))%>%
  summarise(
    Max=max(Global_Sales),
    Min=min(Global_Sales),
    Mean=mean(Global_Sales),
    Median=median(Global_Sales),
    QUA1=quantile(Global_Sales,1/4),
    QUA3=quantile(Global_Sales,3/4),
    IQR=IQR(Global_Sales)
  )
# - There is no null or "0" values on Global_Sales column. The max sales is 82.53 and min sales is 0.01.

# 2.6 Critic_Score
sales_dataset %>%
  summarise(is_NULL=sum(is.na(Critic_Score)==1),
            is_NOT_NULL=sum(!is.na(Critic_Score)==1)
  )

sales_dataset %>%
  filter(!is.na(Critic_Score))%>%
  summarise(
    Max=max(Critic_Score),
    Min=min(Critic_Score),
    Mean=mean(Critic_Score),
    Median=median(Critic_Score),
    QUA1=quantile(Critic_Score,1/4),
    QUA3=quantile(Critic_Score,3/4),
    IQR=IQR(Critic_Score)
  )
# - The result show that Critic_Score column has 5952 null values.
# - When excluding the null values, the max score is 98 and the minimum score is 13.

# 2.7 Critic_Count
sales_dataset %>%
  summarise(is_NULL=sum(is.na(Critic_Count)==1),
            is_NOT_NULL=sum(!is.na(Critic_Count)==1)
  )

sales_dataset %>%
  filter(!is.na(Critic_Count))%>%
  summarise(
    Max=max(Critic_Count),
    Min=min(Critic_Count),
    Mean=mean(Critic_Count),
    Median=median(Critic_Count),
    QUA1=quantile(Critic_Count,1/4),
    QUA3=quantile(Critic_Count,3/4),
    IQR=IQR(Critic_Count)
  )
# - The result show that Critic_Count column has 5952 null values.
# - When excluding the null values, the max number of media give critics is 113 and the min number of media give critics is 3.

# 2.8 User_Score
sales_dataset %>%
  summarise(is_NULL=sum(is.na(User_Score)==1),
            is_NOT_NULL=sum(!is.na(User_Score)==1),
            is_tbd=sum(User_Score=="tbd")
  )

sales_dataset %>%
  mutate(User_Score = as.numeric(User_Score)) %>%
  filter(!is.na(User_Score) & User_Score!="tbd") %>%
  summarise(
    Max=max(User_Score),
    Min=min(User_Score),
    Mean=mean(User_Score),
    Median=median(User_Score),
    QUA1=quantile(User_Score,1/4),
    QUA3=quantile(User_Score,3/4),
    IQR=IQR(User_Score)
  )
# - There is no null value on User_Score column, however there are "0" values and "tbd" values on column.

# 2.9 User_Count
sales_dataset %>%
  summarise(is_NULL=sum(is.na(User_Count)==1),
            is_NOT_NULL=sum(!is.na(User_Count)==1)
  )

sales_dataset %>%
  filter(!is.na(User_Count))%>%
  summarise(
    Max=max(User_Count),
    Min=min(User_Count),
    Mean=mean(User_Count),
    Median=median(User_Count),
    QUA1=quantile(User_Count,1/4),
    QUA3=quantile(User_Count,3/4),
    IQR=IQR(User_Count)
  )
# - The result show that User_Count column has 6327 null values.
# - When excluding the null values, the max number of users give critics is 10665 and the min number of users give critics is 4.

## 3. Dataset Summary

summary(sales_dataset)

## 4. Finding Outliers in Global_Sales
#Since this notebook focuses on implementing the models to predict Global_Sales, it requires outliers detection in this column.

outlier_global_sales <- qplot(y = Global_Sales, ylab = "Outliers Global Sales", data = sales_dataset, geom = "boxplot", fill=I("tomato")) + theme_minimal() + theme(axis.text.x = element_text(angle = 90))
outlier_global_sales
# - It can be seen that there are outliers in the 'Global_Sales' column

#=========================
# DATA PREPROCESSING
#=========================
# This section focuses on cleaning/preparing the dataset before implement it to the models.

## 1. Creating and Dropping Features and Observations

# 1.1 Dropping Unrelated Features 
# Unrelated variables (such as NA_Sales, EU_Sales, JP_Sales, Other_Sales)
sales_dataset = subset(sales_dataset, select = -c(NA_Sales,EU_Sales,JP_Sales,Other_Sales))

# 1.2 Delete "PCFX" and "GG" in Platform
# Since PCFX and GG only have 1 observations, these observations will be deleted to simplify the sampling process.
sales_dataset <- subset(sales_dataset, sales_dataset$Platform != "PCFX")
sales_dataset <- subset(sales_dataset, sales_dataset$Platform != "GG")

# 1.3 Creating New Features
sales_dataset$Year_of_Release <- 2021 - sales_dataset$Year_of_Release
names(sales_dataset)[3] <- "Age"

## 2. Data Cleaning

# 2.1 Deleting Null Rows
# In the name and genre column, there are 2 null values. In this case, the data/rows will be deleted.
sales_dataset <- subset(sales_dataset, sales_dataset$Name != "" | sales_dataset$Genre != "")

# 2.2 Fill Blank/Null Values with NA
# The dataset contains some blank/null values. 
# In order to make good analysis, the null values should be filled with NA.
is.na(sales_dataset) <- sales_dataset == ''

# 2.3 Cleaning Dirty Data
# In user score column, there are some "tbd" values.
# The "tbd" values will be replaced with NA to make good analysis.
sales_dataset$User_Score <- as.character(sales_dataset$User_Score)
sales_dataset$User_Score[sales_dataset$User_Score == "tbd"] <- ""

# 2.4 Plot Missing Values
plot_missing(sales_dataset)
# After deleting, filling blank values with NA values, it still shows high percentages of missing data in the video games sales dataset.

## 3. Data Imputation

# 3.1 Imputation w/ MICE 
# This section will impute the missing values with MICE package.
# In order to fill missing categorical value in dataset, it is necessary to convert the categorical values into numbers before calling MICE package.
# Convert categorical to numbers
sales_dataset$User_Score=as.double(sales_dataset$User_Score)

df_imputation <- data.frame(sales_dataset$Age, sales_dataset$Rating, sales_dataset$Critic_Score, sales_dataset$Critic_Count, sales_dataset$User_Score, sales_dataset$User_Count)
names(df_imputation) <- c("Age_imp","Rating_imp", "Critic_Score_imp", "Critic_Count_imp", "User_Score_imp", "User_Count_imp")

df_imputation$Rating_imp = factor(df_imputation$Rating_imp, 
                                  levels = c('AO', 'E', 'E10+', 'EC', 'K-A', 'M', 'RP', 'T'), labels = c(1, 2, 3, 4, 5, 6, 7, 8))

# Imputation w/ MICE
md.pattern(df_imputation)
df_imputation_imp <- mice(df_imputation, m=5, seed = 123)
df_imputation <- complete(df_imputation_imp, 1)
plot_missing(df_imputation)

# Revert numbers to categorical
df_imputation$Rating_imp = factor(df_imputation$Rating_imp, 
                                  levels = c(1, 2, 3, 4, 5, 6, 7, 8), labels = c('AO', 'E', 'E10+', 'EC', 'K-A', 'M', 'RP', 'T'))
sales_dataset <- sales_dataset %>% mutate(ID = row_number())
df_imputation <- df_imputation %>% mutate(ID = row_number())
sales_dataset <- merge(sales_dataset,df_imputation,by="ID")
sales_dataset = subset(sales_dataset, select = -c(ID, Rating, Age, Critic_Score, Critic_Count, User_Score, User_Count))

#===========================
# EXPLORATORY DATA ANALYSIS
#===========================

## 1. Top 10 Publisher
publisher_data <- sales_dataset %>%
  group_by(Publisher) %>%
  summarise(count = n()) %>%
  top_n(n = 10, wt = count)

publisher_charts <- ggplot(publisher_data, aes(x = Publisher, y = count, fill=count)) + geom_col(fill="orange")+
  labs(title = "Top 10 Publisher", x="Publisher",y="Total") + coord_flip()
publisher_charts
# - Electronic Arts became the publisher with the most released games on the market, followed by Activision and Namco Bandai Games.

## 2. Top 5 Genre
genre_data <- sales_dataset %>%
  group_by(Genre) %>%
  summarise(count = n()) %>%
  top_n(n = 5, wt = count)

genre_charts <- ggplot(genre_data, aes(x = Genre, y = count, fill=count)) + geom_col(fill="red")+
  labs(title = "Top 5 Genres", x="Genre",y="Total")
genre_charts
# - Action is the most popular video game genre on the market, followed by Sports and Misc.

## 3. Top 5 Platform
platform_data <- sales_dataset %>%
  group_by(Platform) %>%
  summarise(count = n()) %>%
  top_n(n = 5, wt = count)

platform_charts <- ggplot(platform_data, aes(x = Platform, y = count, fill=count)) + geom_col(fill="yellow")+
  labs(title = "Top 5 Platform", x="Platform",y="Total")
platform_charts
# - PS2 became the platform that had the most video games, followed by DS and PS3.

## 4. Genre based on Rating
qplot(x = Genre, data = sales_dataset) + geom_bar(fill = "purple") + coord_flip()  + facet_wrap(~Rating_imp, nrow = 2)

# - Most of the video games on the market are rated E and the majority of genres rated E are the Sports genre.
# - Moreover, the T and M ratings are other ratings that have the most video games on the market, with Action being the majority of the genres for the T and M ratings.

## 5. Platform based on Genre
qplot(x=Platform,data=sales_dataset)+ geom_bar(fill="navy") + theme(axis.text = element_text) + coord_flip() + theme_minimal() + facet_wrap(~Genre,nrow=1)
# - Most of the video games on the market are video games for PS2, DS, PS3 and X360.
# - The majority of game genres for those platforms are Action and Sports.

## 6. Platform based on Rating
qplot(x=Platform,data=sales_dataset)+ geom_bar(fill="brown") + theme(axis.text = element_text) + coord_flip() + theme_minimal() + facet_wrap(~Rating_imp,nrow=1)
# - The DS platform has video games rated E followed by PS2 and Wii the most.
# - As for video games with a T rating, PS2 became the platform that had the most video games for that rating, followed by PS and PS3.

## 7. Correlation Matrix
cor_df <- data.frame(sales_dataset$Age_imp, sales_dataset$Critic_Score_imp, sales_dataset$Critic_Count_imp, sales_dataset$User_Score_imp, sales_dataset$User_Count_imp, sales_dataset$Global)
cor_matrix<-cor(cor_df)
corrplot(cor_matrix, diag = FALSE, order = "FPC", tl.pos = "td", tl.cex = 0.5, method = "circle",type="upper")
# - It can be seen that the ‘Critic_Count_imp’ column and ‘User_Score_imp’ column have a high correlation (close to 1).

#==========================
# DATA PREPARATION
#==========================
sales_dataset1 = subset(sales_dataset, select = -c(Name, Publisher, Developer))

# Exp 1 - train:test 70:30, seed 555
set.seed(555)
split = sample.split(sales_dataset1$Global, SplitRatio = 0.7)
train_dataset1 = subset(sales_dataset1, split == TRUE)
test_dataset1 = subset(sales_dataset1, split == FALSE)

# Exp 2 - train:test 70:30, seed 555, data scaling
set.seed(555)
split = sample.split(sales_dataset1$Global, SplitRatio = 0.7)
train_dataset2 = subset(sales_dataset1, split == TRUE)
test_dataset2 = subset(sales_dataset1, split == FALSE)

train_dataset2[,3] = scale(train_dataset2[,3], center = TRUE, scale = TRUE)
test_dataset2[,3] = scale(test_dataset2[,3], center = TRUE, scale = TRUE)
train_dataset2[,6:9] = scale(train_dataset2[,6:9], center = TRUE, scale = TRUE)
test_dataset2[,6:9] = scale(test_dataset2[,6:9], center = TRUE, scale = TRUE)

# Exp 3 - train:test 70:30, seed 555, outliers removed, data scaling
Q <- quantile(sales_dataset1$Global_Sales, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(sales_dataset1$Global_Sales)
sales_dataset1_clean<- subset(sales_dataset1, sales_dataset1$Global_Sales > (Q[1] - 1.5*iqr) & sales_dataset1$Global_Sales < (Q[2]+1.5*iqr))

set.seed(555)
split = sample.split(sales_dataset1_clean$Global, SplitRatio = 0.7)
train_dataset3 = subset(sales_dataset1_clean, split == TRUE)
test_dataset3 = subset(sales_dataset1_clean, split == FALSE)

train_dataset3[,3] = scale(train_dataset3[,3], center = TRUE, scale = TRUE)
test_dataset3[,3] = scale(test_dataset3[,3], center = TRUE, scale = TRUE)
train_dataset3[,6:9] = scale(train_dataset3[,6:9], center = TRUE, scale = TRUE)
test_dataset3[,6:9] = scale(test_dataset3[,6:9], center = TRUE, scale = TRUE)

# Exp 4 - train:test 80:20, seed 555, outlier removed, data scaling
set.seed(555)
split = sample.split(sales_dataset1_clean$Global, SplitRatio = 0.8)
train_dataset4 = subset(sales_dataset1_clean, split == TRUE)
test_dataset4 = subset(sales_dataset1_clean, split == FALSE)

train_dataset4[,3] = scale(train_dataset4[,3], center = TRUE, scale = TRUE)
test_dataset4[,3] = scale(test_dataset4[,3], center = TRUE, scale = TRUE)
train_dataset4[,6:9] = scale(train_dataset4[,6:9], center = TRUE, scale = TRUE)
test_dataset4[,6:9] = scale(test_dataset4[,6:9], center = TRUE, scale = TRUE)

#=========================
# MACHINE LEARNING MODEL
#=========================

# 1. Multiple Linear Regression

### Exp 1 - Multiple Linear Regression
regressor1 = lm(formula = Global_Sales ~ .,
                data = train_dataset1)
summary(regressor1)
summ(regressor1, confint = TRUE)


y1 = predict(regressor1, train_dataset1)
table(y1, train_dataset1$Global_Sales)

y_pred1 = predict(regressor1, test_dataset1)
table(y_pred1, test_dataset1$Global_Sales)

# RMSE on training set
RMSE(y1, train_dataset1$Global_Sales)

# RMSE on test set
RMSE(y_pred1, test_dataset1$Global_Sales)

# MAE train dataset
MAE(y1, train_dataset1$Global_Sales)

# MAE test dataset
MAE(y_pred1, test_dataset1$Global_Sales)

### Exp 1.1 - Multiple Linear Regression - Backward Elimination
regressor11 = lm(formula = Global_Sales ~ .,
                 data = train_dataset1)
regressor12 = lm(formula = Global_Sales ~ Platform + Genre + Age_imp + Rating_imp + Critic_Score_imp + 
                   Critic_Count_imp + User_Score_imp + User_Count_imp,
                 data = train_dataset1)
regressor13 = lm(formula = Global_Sales ~ Platform + Genre + Age_imp + Rating_imp + Critic_Score_imp + 
                   Critic_Count_imp + User_Score_imp,
                 data = train_dataset1)
regressor14 = lm(formula = Global_Sales ~ Platform + Genre + Age_imp + Rating_imp + Critic_Score_imp + 
                   Critic_Count_imp,
                 data = train_dataset1)
regressor15 = lm(formula = Global_Sales ~ Platform + Genre + Age_imp + Rating_imp + Critic_Score_imp,
                 data = train_dataset1)
regressor16 = lm(formula = Global_Sales ~ Platform + Genre + Age_imp + Rating_imp,
                 data = train_dataset1)
regressor17 = lm(formula = Global_Sales ~ Platform + Genre + Age_imp,
                 data = train_dataset1)
regressor18 = lm(formula = Global_Sales ~ Platform + Genre,
                 data = train_dataset1)
regressor19 = lm(formula = Global_Sales ~ Platform,
                 data = train_dataset1)

y11 = predict(regressor11, train_dataset1)
#table(y11, train_dataset1$Global_Sales)
y12 = predict(regressor12, train_dataset1)
#table(y12, train_dataset1$Global_Sales)
y13 = predict(regressor13, train_dataset1)
#table(y13, train_dataset1$Global_Sales)
y14 = predict(regressor14, train_dataset1)
#table(y14, train_dataset1$Global_Sales)
y15 = predict(regressor15, train_dataset1)
#table(y15, train_dataset1$Global_Sales)
y16 = predict(regressor16, train_dataset1)
#table(y16, train_dataset1$Global_Sales)
y17 = predict(regressor17, train_dataset1)
#table(y17, train_dataset1$Global_Sales)
y18 = predict(regressor18, train_dataset1)
#table(y18, train_dataset1$Global_Sales)
y19 = predict(regressor19, train_dataset1)
#table(y19, train_dataset1$Global_Sales)

y_pred11 = predict(regressor11, test_dataset1)
#table(y_pred11, test_dataset1$Global_Sales)
y_pred12 = predict(regressor12, test_dataset1)
#table(y_pred12, test_dataset1$Global_Sales)
y_pred13 = predict(regressor13, test_dataset1)
#table(y_pred12, test_dataset1$Global_Sales)
y_pred14 = predict(regressor14, test_dataset1)
#table(y_pred14, test_dataset1$Global_Sales)
y_pred15 = predict(regressor15, test_dataset1)
#table(y_pred12, test_dataset1$Global_Sales)
y_pred16 = predict(regressor16, test_dataset1)
#table(y_pred12, test_dataset1$Global_Sales)
y_pred17 = predict(regressor17, test_dataset1)
#table(y_pred12, test_dataset1$Global_Sales)
y_pred18 = predict(regressor18, test_dataset1)
#table(y_pred12, test_dataset1$Global_Sales)
y_pred19 = predict(regressor19, test_dataset1)
#table(y_pred19, test_dataset1$Global_Sales)

#RMSE
cat("RMSE 11 ", RMSE(y_pred11, test_dataset1$Global_Sales), sep="\t")
cat("\n")
cat("RMSE 12 ", RMSE(y_pred12, test_dataset1$Global_Sales), sep="\t")
cat("\n")
cat("RMSE 13 ", RMSE(y_pred13, test_dataset1$Global_Sales), sep="\t")
cat("\n")
cat("RMSE 14 ", RMSE(y_pred14, test_dataset1$Global_Sales), sep="\t")
cat("\n")
cat("RMSE 15 ", RMSE(y_pred15, test_dataset1$Global_Sales), sep="\t")
cat("\n")
cat("RMSE 16 ", RMSE(y_pred16, test_dataset1$Global_Sales), sep="\t")
cat("\n")
cat("RMSE 17 ", RMSE(y_pred17, test_dataset1$Global_Sales), sep="\t")
cat("\n")
cat("RMSE 18 ", RMSE(y_pred18, test_dataset1$Global_Sales), sep="\t")
cat("\n")
cat("RMSE 19 ", RMSE(y_pred19, test_dataset1$Global_Sales), sep="\t")


#MAE
cat("MAE 11 ", MAE(y_pred11, test_dataset1$Global_Sales), sep="\t")
cat("\n")
cat("MAE 12 ", MAE(y_pred12, test_dataset1$Global_Sales), sep="\t")
cat("\n")
cat("MAE 13 ", MAE(y_pred13, test_dataset1$Global_Sales), sep="\t")
cat("\n")
cat("MAE 14 ", MAE(y_pred14, test_dataset1$Global_Sales), sep="\t")
cat("\n")
cat("MAE 15 ", MAE(y_pred15, test_dataset1$Global_Sales), sep="\t")
cat("\n")
cat("MAE 16 ", MAE(y_pred16, test_dataset1$Global_Sales), sep="\t")
cat("\n")
cat("MAE 17 ", MAE(y_pred17, test_dataset1$Global_Sales), sep="\t")
cat("\n")
cat("MAE 18 ", MAE(y_pred18, test_dataset1$Global_Sales), sep="\t")
cat("\n")
cat("MAE 19 ", MAE(y_pred19, test_dataset1$Global_Sales), sep="\t")
cat("\n")

### Exp 2 - Multiple Linear Regression
regressor2 = lm(formula = Global_Sales ~ .,
                data = train_dataset2)
summary(regressor2)
summ(regressor2, confint = TRUE)


y2 = predict(regressor2, train_dataset2)
table(y2, train_dataset2$Global_Sales)

y_pred2 = predict(regressor2, test_dataset2)
table(y_pred2, test_dataset2$Global_Sales)

# RMSE on training set
RMSE(y2, train_dataset2$Global_Sales)
# RMSE on test set
RMSE(y_pred2, test_dataset2$Global_Sales)
# MAE train dataset
MAE(y2, train_dataset2$Global_Sales)
# MAE test dataset
MAE(y_pred2, test_dataset2$Global_Sales)

### Exp 2.1 - Multiple Linear Regression - Backward Elimination
regressor21 = lm(formula = Global_Sales ~ .,
                 data = train_dataset2)
regressor22 = lm(formula = Global_Sales ~ Platform + Genre + Age_imp + Rating_imp + Critic_Score_imp + 
                   Critic_Count_imp + User_Score_imp + User_Count_imp,
                 data = train_dataset2)
regressor23 = lm(formula = Global_Sales ~ Platform + Genre + Age_imp + Rating_imp + Critic_Score_imp + 
                   Critic_Count_imp + User_Score_imp,
                 data = train_dataset2)
regressor24 = lm(formula = Global_Sales ~ Platform + Genre + Age_imp + Rating_imp + Critic_Score_imp + 
                   Critic_Count_imp,
                 data = train_dataset2)
regressor25 = lm(formula = Global_Sales ~ Platform + Genre + Age_imp + Rating_imp + Critic_Score_imp,
                 data = train_dataset2)
regressor26 = lm(formula = Global_Sales ~ Platform + Genre + Age_imp + Rating_imp,
                 data = train_dataset2)
regressor27 = lm(formula = Global_Sales ~ Platform + Genre + Age_imp,
                 data = train_dataset2)
regressor28 = lm(formula = Global_Sales ~ Platform + Genre,
                 data = train_dataset2)
regressor29 = lm(formula = Global_Sales ~ Platform,
                 data = train_dataset2)

y21 = predict(regressor21, train_dataset2)
#table(y21, train_dataset2$Global_Sales)
y22 = predict(regressor22, train_dataset2)
#table(y22, train_dataset2$Global_Sales)
y23 = predict(regressor23, train_dataset2)
#table(y23, train_dataset2$Global_Sales)
y24 = predict(regressor24, train_dataset2)
#table(y24, train_dataset2$Global_Sales)
y25 = predict(regressor25, train_dataset2)
#table(y25, train_dataset2$Global_Sales)
y26 = predict(regressor26, train_dataset2)
#table(y26, train_dataset2$Global_Sales)
y27 = predict(regressor27, train_dataset2)
#table(y27, train_dataset2$Global_Sales)
y28 = predict(regressor28, train_dataset2)
#table(y28, train_dataset2$Global_Sales)
y29 = predict(regressor29, train_dataset2)
#table(y29, train_dataset2$Global_Sales)

y_pred21 = predict(regressor21, test_dataset2)
#table(y_pred21, test_dataset2$Global_Sales)
y_pred22 = predict(regressor22, test_dataset2)
#table(y_pred22, test_dataset2$Global_Sales)
y_pred23 = predict(regressor23, test_dataset2)
#table(y_pred23, test_dataset2$Global_Sales)
y_pred24 = predict(regressor24, test_dataset2)
#table(y_pred24, test_dataset2$Global_Sales)
y_pred25 = predict(regressor25, test_dataset2)
#table(y_pred25, test_dataset2$Global_Sales)
y_pred26 = predict(regressor26, test_dataset2)
#table(y_pred26, test_dataset2$Global_Sales)
y_pred27 = predict(regressor27, test_dataset2)
#table(y_pred27, test_dataset2$Global_Sales)
y_pred28 = predict(regressor28, test_dataset2)
#table(y_pred28, test_dataset2$Global_Sales)
y_pred29 = predict(regressor29, test_dataset2)
#table(y_pred29, test_dataset2$Global_Sales)

#RMSE
cat("RMSE 21 ", RMSE(y_pred21, test_dataset2$Global_Sales), sep="\t")
cat("\n")
cat("RMSE 22 ", RMSE(y_pred22, test_dataset2$Global_Sales), sep="\t")
cat("\n")
cat("RMSE 23 ", RMSE(y_pred23, test_dataset2$Global_Sales), sep="\t")
cat("\n")
cat("RMSE 24 ", RMSE(y_pred24, test_dataset2$Global_Sales), sep="\t")
cat("\n")
cat("RMSE 25 ", RMSE(y_pred25, test_dataset2$Global_Sales), sep="\t")
cat("\n")
cat("RMSE 26 ", RMSE(y_pred26, test_dataset2$Global_Sales), sep="\t")
cat("\n")
cat("RMSE 27 ", RMSE(y_pred27, test_dataset2$Global_Sales), sep="\t")
cat("\n")
cat("RMSE 28 ", RMSE(y_pred28, test_dataset2$Global_Sales), sep="\t")
cat("\n")
cat("RMSE 29 ", RMSE(y_pred29, test_dataset2$Global_Sales), sep="\t")


#MAE
cat("MAE 21 ", MAE(y_pred21, test_dataset2$Global_Sales), sep="\t")
cat("\n")
cat("MAE 22 ", MAE(y_pred22, test_dataset2$Global_Sales), sep="\t")
cat("\n")
cat("MAE 23 ", MAE(y_pred23, test_dataset2$Global_Sales), sep="\t")
cat("\n")
cat("MAE 24 ", MAE(y_pred24, test_dataset2$Global_Sales), sep="\t")
cat("\n")
cat("MAE 25 ", MAE(y_pred25, test_dataset2$Global_Sales), sep="\t")
cat("\n")
cat("MAE 26 ", MAE(y_pred26, test_dataset2$Global_Sales), sep="\t")
cat("\n")
cat("MAE 27 ", MAE(y_pred27, test_dataset2$Global_Sales), sep="\t")
cat("\n")
cat("MAE 28 ", MAE(y_pred28, test_dataset2$Global_Sales), sep="\t")
cat("\n")
cat("MAE 29 ", MAE(y_pred29, test_dataset2$Global_Sales), sep="\t")
cat("\n")

### Exp 3 - Multiple Linear Regression
regressor3 = lm(formula = Global_Sales ~ .,
                data = train_dataset3)
summary(regressor3)
summ(regressor3, confint = TRUE)

y3 = predict(regressor3, train_dataset3)
table(y3, train_dataset3$Global_Sales)

y_pred3 = predict(regressor3, test_dataset3)
table(y_pred3, test_dataset3$Global_Sales)

# RMSE on training set
RMSE(y3, train_dataset3$Global_Sales)
# RMSE on test set
RMSE(y_pred3, test_dataset3$Global_Sales)
# MAE train dataset
MAE(y3, train_dataset3$Global_Sales)
# MAE test dataset
MAE(y_pred3, test_dataset3$Global_Sales)

### Exp 3.1 - Multiple Linear Regression - Backward Elimination
regressor31 = lm(formula = Global_Sales ~ .,
                 data = train_dataset3)
regressor32 = lm(formula = Global_Sales ~ Platform + Genre + Age_imp + Rating_imp + Critic_Score_imp + 
                   Critic_Count_imp + User_Score_imp + User_Count_imp,
                 data = train_dataset3)
regressor33 = lm(formula = Global_Sales ~ Platform + Genre + Age_imp + Rating_imp + Critic_Score_imp + 
                   Critic_Count_imp + User_Score_imp,
                 data = train_dataset3)
regressor34 = lm(formula = Global_Sales ~ Platform + Genre + Age_imp + Rating_imp + Critic_Score_imp + 
                   Critic_Count_imp,
                 data = train_dataset3)
regressor35 = lm(formula = Global_Sales ~ Platform + Genre + Age_imp + Rating_imp + Critic_Score_imp,
                 data = train_dataset3)
regressor36 = lm(formula = Global_Sales ~ Platform + Genre + Age_imp + Rating_imp,
                 data = train_dataset3)
regressor37 = lm(formula = Global_Sales ~ Platform + Genre + Age_imp,
                 data = train_dataset3)
regressor38 = lm(formula = Global_Sales ~ Platform + Genre,
                 data = train_dataset3)
regressor39 = lm(formula = Global_Sales ~ Platform,
                 data = train_dataset3)

y31 = predict(regressor31, train_dataset3)
#table(y31, train_dataset3$Global_Sales)
y32 = predict(regressor32, train_dataset3)
#table(y32, train_dataset3$Global_Sales)
y33 = predict(regressor33, train_dataset3)
#table(y33, train_dataset3$Global_Sales)
y34 = predict(regressor34, train_dataset3)
#table(y34, train_dataset3$Global_Sales)
y35 = predict(regressor35, train_dataset3)
#table(y35, train_dataset3$Global_Sales)
y36 = predict(regressor36, train_dataset3)
#table(y36, train_dataset3$Global_Sales)
y37 = predict(regressor37, train_dataset3)
#table(y37, train_dataset3$Global_Sales)
y38 = predict(regressor38, train_dataset3)
#table(y38, train_dataset3$Global_Sales)
y39 = predict(regressor39, train_dataset3)
#table(y39, train_dataset3$Global_Sales)

y_pred31 = predict(regressor31, test_dataset3)
#table(y_pred31, test_dataset3$Global_Sales)
y_pred32 = predict(regressor32, test_dataset3)
#table(y_pred32, test_dataset3$Global_Sales)
y_pred33 = predict(regressor33, test_dataset3)
#table(y_pred33, test_dataset3$Global_Sales)
y_pred34 = predict(regressor34, test_dataset3)
#table(y_pred34, test_dataset3$Global_Sales)
y_pred35 = predict(regressor35, test_dataset3)
#table(y_pred35, test_dataset3$Global_Sales)
y_pred36 = predict(regressor36, test_dataset3)
#table(y_pred36, test_dataset3$Global_Sales)
y_pred37 = predict(regressor37, test_dataset3)
#table(y_pred37, test_dataset3$Global_Sales)
y_pred38 = predict(regressor38, test_dataset3)
#table(y_pred38, test_dataset3$Global_Sales)
y_pred39 = predict(regressor39, test_dataset3)
#table(y_pred39, test_dataset3$Global_Sales)

#RMSE
cat("RMSE 31 ", RMSE(y_pred31, test_dataset3$Global_Sales), sep="\t")
cat("\n")
cat("RMSE 32 ", RMSE(y_pred32, test_dataset3$Global_Sales), sep="\t")
cat("\n")
cat("RMSE 33 ", RMSE(y_pred33, test_dataset3$Global_Sales), sep="\t")
cat("\n")
cat("RMSE 34 ", RMSE(y_pred34, test_dataset3$Global_Sales), sep="\t")
cat("\n")
cat("RMSE 35 ", RMSE(y_pred35, test_dataset3$Global_Sales), sep="\t")
cat("\n")
cat("RMSE 36 ", RMSE(y_pred36, test_dataset3$Global_Sales), sep="\t")
cat("\n")
cat("RMSE 37 ", RMSE(y_pred37, test_dataset3$Global_Sales), sep="\t")
cat("\n")
cat("RMSE 38 ", RMSE(y_pred38, test_dataset3$Global_Sales), sep="\t")
cat("\n")
cat("RMSE 39 ", RMSE(y_pred39, test_dataset3$Global_Sales), sep="\t")


#MAE
cat("MAE 31 ", MAE(y_pred31, test_dataset3$Global_Sales), sep="\t")
cat("\n")
cat("MAE 32 ", MAE(y_pred32, test_dataset3$Global_Sales), sep="\t")
cat("\n")
cat("MAE 33 ", MAE(y_pred33, test_dataset3$Global_Sales), sep="\t")
cat("\n")
cat("MAE 34 ", MAE(y_pred34, test_dataset3$Global_Sales), sep="\t")
cat("\n")
cat("MAE 35 ", MAE(y_pred35, test_dataset3$Global_Sales), sep="\t")
cat("\n")
cat("MAE 36 ", MAE(y_pred36, test_dataset3$Global_Sales), sep="\t")
cat("\n")
cat("MAE 37 ", MAE(y_pred37, test_dataset3$Global_Sales), sep="\t")
cat("\n")
cat("MAE 38 ", MAE(y_pred38, test_dataset3$Global_Sales), sep="\t")
cat("\n")
cat("MAE 39 ", MAE(y_pred39, test_dataset3$Global_Sales), sep="\t")
cat("\n")

### Exp 4 - Multiple Linear Regression

regressor4 = lm(formula = Global_Sales ~ .,
                data = train_dataset4)
summary(regressor4)
summ(regressor4, confint = TRUE)

y4 = predict(regressor4, train_dataset4)
table(y4, train_dataset4$Global_Sales)

y_pred4 = predict(regressor4, test_dataset4)
table(y_pred4, test_dataset4$Global_Sales)

# RMSE on training set
RMSE(y4, train_dataset4$Global_Sales)
# RMSE on test set
RMSE(y_pred4, test_dataset4$Global_Sales)
# MAE train dataset
MAE(y4, train_dataset4$Global_Sales)
# MAE test dataset
MAE(y_pred4, test_dataset4$Global_Sales)

### Exp 4.1 - Multiple Linear Regression - Backward Elimination
regressor41 = lm(formula = Global_Sales ~ .,
                 data = train_dataset4)
regressor42 = lm(formula = Global_Sales ~ Platform + Genre + Age_imp + Rating_imp + Critic_Score_imp + 
                   Critic_Count_imp + User_Score_imp + User_Count_imp,
                 data = train_dataset4)
regressor43 = lm(formula = Global_Sales ~ Platform + Genre + Age_imp + Rating_imp + Critic_Score_imp + 
                   Critic_Count_imp + User_Score_imp,
                 data = train_dataset4)
regressor44 = lm(formula = Global_Sales ~ Platform + Genre + Age_imp + Rating_imp + Critic_Score_imp + 
                   Critic_Count_imp,
                 data = train_dataset4)
regressor45 = lm(formula = Global_Sales ~ Platform + Genre + Age_imp + Rating_imp + Critic_Score_imp,
                 data = train_dataset4)
regressor46 = lm(formula = Global_Sales ~ Platform + Genre + Age_imp + Rating_imp,
                 data = train_dataset4)
regressor47 = lm(formula = Global_Sales ~ Platform + Genre + Age_imp,
                 data = train_dataset4)
regressor48 = lm(formula = Global_Sales ~ Platform + Genre,
                 data = train_dataset4)
regressor49 = lm(formula = Global_Sales ~ Platform,
                 data = train_dataset4)

y41 = predict(regressor41, train_dataset4)
#table(y41, train_dataset4$Global_Sales)
y42 = predict(regressor42, train_dataset4)
#table(y42, train_dataset4$Global_Sales)
y43 = predict(regressor43, train_dataset4)
#table(y43, train_dataset4$Global_Sales)
y44 = predict(regressor44, train_dataset4)
#table(y44, train_dataset4$Global_Sales)
y45 = predict(regressor45, train_dataset4)
#table(y45, train_dataset4$Global_Sales)
y46 = predict(regressor46, train_dataset4)
#table(y46, train_dataset4$Global_Sales)
y47 = predict(regressor47, train_dataset4)
#table(y47, train_dataset4$Global_Sales)
y48 = predict(regressor48, train_dataset4)
#table(y48, train_dataset4$Global_Sales)
y49 = predict(regressor49, train_dataset4)
#table(y49, train_dataset4$Global_Sales)

y_pred41 = predict(regressor41, test_dataset4)
#table(y_pred41, test_dataset4$Global_Sales)
y_pred42 = predict(regressor42, test_dataset4)
#table(y_pred42, test_dataset4$Global_Sales)
y_pred43 = predict(regressor43, test_dataset4)
#table(y_pred43, test_dataset4$Global_Sales)
y_pred44 = predict(regressor44, test_dataset4)
#table(y_pred44, test_dataset4$Global_Sales)
y_pred45 = predict(regressor45, test_dataset4)
#table(y_pred45, test_dataset4$Global_Sales)
y_pred46 = predict(regressor46, test_dataset4)
#table(y_pred46, test_dataset4$Global_Sales)
y_pred47 = predict(regressor47, test_dataset4)
#table(y_pred47, test_dataset4$Global_Sales)
y_pred48 = predict(regressor48, test_dataset4)
#table(y_pred48, test_dataset4$Global_Sales)
y_pred49 = predict(regressor49, test_dataset4)
#table(y_pred49, test_dataset4$Global_Sales)

#RMSE
cat("RMSE 41 ", RMSE(y_pred41, test_dataset4$Global_Sales), sep="\t")
cat("\n")
cat("RMSE 42 ", RMSE(y_pred42, test_dataset4$Global_Sales), sep="\t")
cat("\n")
cat("RMSE 43 ", RMSE(y_pred43, test_dataset4$Global_Sales), sep="\t")
cat("\n")
cat("RMSE 44 ", RMSE(y_pred44, test_dataset4$Global_Sales), sep="\t")
cat("\n")
cat("RMSE 45 ", RMSE(y_pred45, test_dataset4$Global_Sales), sep="\t")
cat("\n")
cat("RMSE 46 ", RMSE(y_pred46, test_dataset4$Global_Sales), sep="\t")
cat("\n")
cat("RMSE 47 ", RMSE(y_pred47, test_dataset4$Global_Sales), sep="\t")
cat("\n")
cat("RMSE 48 ", RMSE(y_pred48, test_dataset4$Global_Sales), sep="\t")
cat("\n")
cat("RMSE 49 ", RMSE(y_pred49, test_dataset4$Global_Sales), sep="\t")


#MAE
cat("MAE 41 ", MAE(y_pred41, test_dataset4$Global_Sales), sep="\t")
cat("\n")
cat("MAE 42 ", MAE(y_pred42, test_dataset4$Global_Sales), sep="\t")
cat("\n")
cat("MAE 43 ", MAE(y_pred43, test_dataset4$Global_Sales), sep="\t")
cat("\n")
cat("MAE 44 ", MAE(y_pred44, test_dataset4$Global_Sales), sep="\t")
cat("\n")
cat("MAE 45 ", MAE(y_pred45, test_dataset4$Global_Sales), sep="\t")
cat("\n")
cat("MAE 46 ", MAE(y_pred46, test_dataset4$Global_Sales), sep="\t")
cat("\n")
cat("MAE 47 ", MAE(y_pred47, test_dataset4$Global_Sales), sep="\t")
cat("\n")
cat("MAE 48 ", MAE(y_pred48, test_dataset4$Global_Sales), sep="\t")
cat("\n")
cat("MAE 49 ", MAE(y_pred49, test_dataset4$Global_Sales), sep="\t")
cat("\n")

## INSIGHT:
# - In general, this model produces quite poor performance in predicting the data, where the RMSE test values are close to 1.
# - Furthermore, it can be seen that there is no significant difference between the value obtained without backward elimination and the value obtained by performing backward elimination.
# - However, it can be concluded that Experiment_1 and Experiment_2 cause the data underfitting, where weak generalization to test data despite good results on the training data.
# - Also, it can be concluded that data scaling, outlier deletion, and differences in data splitting make the RMSE and MAE training value with the RMSE and MAE test value not have a significant difference

# 2. Random Forest Regression
### Exp 1 - Random Forest - ntreeTry 500, mtry 2
set.seed(555)
rf1 <- randomForest(Global_Sales~.,data = train_dataset1,
                    ntreeTry = 500,
                    mtry=2,
                    importance = TRUE,
                    proximity = TRUE)
print(rf1)
attributes(rf1)
p11 <- predict(rf1, train_dataset1)

p11
cm_train1 <- table (p11, train_dataset1$Global_Sales)
cm_train1
train_accuracy1 = sum(diag(cm_train1)/sum(cm_train1))
train_accuracy1

p21 <- predict(rf1, test_dataset1)
cm_test1 <- table(p21, test_dataset1$Global_Sales)
cm_test1
test_accuracy1 = sum(diag(cm_test1)/sum(cm_test1))
test_accuracy1

plot(rf1)

# RMSE on training set
RMSE(p11, train_dataset1$Global_Sales)
# RMSE on test set
RMSE(p21, test_dataset1$Global_Sales)
# MAE on training set
MAE(p11, train_dataset1$Global_Sales)
# MAE on test set
MAE(p21, test_dataset1$Global_Sales)

### Exp 2 - Random Forest - ntreeTry 500, mtry 3, data scaling
set.seed(555)
rf2 <- randomForest(Global_Sales~.,data = train_dataset2,
                    ntreeTry = 500,
                    mtry=3,
                    importance = TRUE,
                    proximity = TRUE)
print(rf2)
attributes(rf2)
p12 <- predict(rf2, train_dataset2)

p12
cm_train2 <- table (p12, train_dataset2$Global_Sales)
cm_train2
train_accuracy2 = sum(diag(cm_train2)/sum(cm_train2))
train_accuracy2

p22 <- predict(rf2, test_dataset2)
cm_test2 <- table(p22, test_dataset2$Global_Sales)
cm_test2
test_accuracy2 = sum(diag(cm_test2)/sum(cm_test2))
test_accuracy2

plot(rf2)

# RMSE on training set
RMSE(p12, train_dataset2$Global_Sales)
# RMSE on test set
RMSE(p22, test_dataset2$Global_Sales)
# MAE on training set
MAE(p12, train_dataset2$Global_Sales)
# MAE on test set
MAE(p22, test_dataset2$Global_Sales)

### Exp 3 - Random Forest - ntreeTry 600, mtry 3, data scaling, outliers removed
set.seed(555)
rf3 <- randomForest(Global_Sales~.,data = train_dataset3,
                    ntreeTry = 600,
                    mtry=3,
                    importance = TRUE,
                    proximity = TRUE)
print(rf3)
attributes(rf3)
p13 <- predict(rf3, train_dataset3)

p13
cm_train3 <- table (p13, train_dataset3$Global_Sales)
cm_train3
train_accuracy3 = sum(diag(cm_train3)/sum(cm_train3))
train_accuracy3

p23 <- predict(rf3, test_dataset3)
cm_test3 <- table(p23, test_dataset3$Global_Sales)
cm_test3
test_accuracy3 = sum(diag(cm_test3)/sum(cm_test3))
test_accuracy3

plot(rf3)

# RMSE on training set
RMSE(p13, train_dataset3$Global_Sales)
# RMSE on test set
RMSE(p23, test_dataset3$Global_Sales)
# MAE on training set
MAE(p13, train_dataset3$Global_Sales)
# MAE on test set
MAE(p23, test_dataset3$Global_Sales)

### Exp 4 - Random Forest - ntreeTry 700, mtry 4, data scaling, outliers removed
set.seed(555)
rf4 <- randomForest(Global_Sales~.,data = train_dataset4,
                    ntreeTry = 700,
                    mtry=4,
                    importance = TRUE,
                    proximity = TRUE)
print(rf4)
attributes(rf4)
p14 <- predict(rf4, train_dataset4)

p14
cm_train4 <- table (p14, train_dataset4$Global_Sales)
cm_train4
train_accuracy4 = sum(diag(cm_train4)/sum(cm_train4))
train_accuracy4

p24 <- predict(rf4, test_dataset4)
cm_test4 <- table(p24, test_dataset4$Global_Sales)
cm_test4
test_accuracy4 = sum(diag(cm_test4)/sum(cm_test4))
test_accuracy4

plot(rf4)

# RMSE on training set
RMSE(p14, train_dataset4$Global_Sales)
# RMSE on test set
RMSE(p24, test_dataset4$Global_Sales)
# MAE on training set
MAE(p14, train_dataset4$Global_Sales)
# MAE on test set
MAE(p24, test_dataset4$Global_Sales)

## INSIGHT
# - From the RMSE training and test values, it can be seen that the results of Experiment_10, Experiment_11, and Experiment_12 cause the data is overfitting, which means that good results on training data, but generalization to test data is having poor performance.
# - From the MAE training and test values, it can be seen that during training, the model had already seen the training set since most of the experiments having the MAE test values is higher than the MAE training values.
# - However, Experiment_9 had better MAE training and test results than the other experiments.
# - In addition, it can be concluded that the change in the value of ‘ntreeTry’ and ‘mtry’ does not affect the RMSE and MAE values of each experiment because these values are used to predict classification.
# - Moreover, the differences in splitting train and test, data scaling, outlier removal worsen the RMSE and MAE values of each experiment

# 3. SVM Regression
### Exp 1 - Support Vector Machine

# SVM Model using the RBF kernel
svm_rbf1 <- svm(Global_Sales~., data = train_dataset1)
summary(svm_rbf1)

pred11 = predict (svm_rbf1, train_dataset1)
pred11

pred12 = predict (svm_rbf1, test_dataset1)
pred12

summary(pred11)

table(pred12, test_dataset1$Global_Sales)

RMSE(pred11, train_dataset1$Global_Sales)
RMSE(pred12, test_dataset1$Global_Sales)
MAE(pred11, train_dataset1$Global_Sales)
MAE(pred12, test_dataset1$Global_Sales)

### Exp 2 - Support Vector Machine

# SVM Model using the RBF kernel
svm_rbf2 <- svm(Global_Sales~., data = train_dataset2)
summary(svm_rbf2)

pred21 = predict (svm_rbf2, train_dataset2)
pred21

pred22 = predict (svm_rbf2, test_dataset2)
pred22

summary(pred21)

RMSE(pred21, train_dataset2$Global_Sales)
RMSE(pred22, test_dataset2$Global_Sales)
MAE(pred21, train_dataset2$Global_Sales)
MAE(pred22, test_dataset2$Global_Sales)

### Exp 3 - Support Vector Machine

# SVM Model using the RBF kernel
svm_rbf3 <- svm(Global_Sales~., data = train_dataset3)
summary(svm_rbf3)

pred31 = predict (svm_rbf3, train_dataset3)
pred31

pred32 = predict (svm_rbf3, test_dataset3)
pred32

summary(pred31)

table(pred32, test_dataset3$Global_Sales)

RMSE(pred31, train_dataset3$Global_Sales)
RMSE(pred32, test_dataset3$Global_Sales)
MAE(pred31, train_dataset3$Global_Sales)
MAE(pred32, test_dataset3$Global_Sales)

### Exp 4 - Support Vector Machine

# SVM Model using the RBF kernel
svm_rbf4 <- svm(Global_Sales~., data = train_dataset4)
summary(svm_rbf4)

pred41 = predict (svm_rbf4, train_dataset4)
pred41

pred42 = predict (svm_rbf4, test_dataset4)
pred42

summary(pred41)

table(pred42, test_dataset4$Global_Sales)

RMSE(pred41, train_dataset4$Global_Sales)
RMSE(pred42, test_dataset4$Global_Sales)
MAE(pred41, train_dataset4$Global_Sales)
MAE(pred42, test_dataset4$Global_Sales)

# SVM RBF Kernel comparison
cat("RMSE Exp 1 RBF Kernel ", RMSE(pred12, test_dataset1$Global_Sales), sep="\t")
cat("\n")
cat("RMSE Exp 2 RBF Kernel ", RMSE(pred22, test_dataset2$Global_Sales), sep="\t")
cat("\n")
cat("RMSE Exp 3 RBF Kernel ", RMSE(pred32, test_dataset3$Global_Sales), sep="\t")
cat("\n")
cat("RMSE Exp 4 RBF Kernel ", RMSE(pred42, test_dataset4$Global_Sales), sep="\t")
cat("\n")

cat("MAE Exp 1 RBF Kernel ", MAE(pred12, test_dataset1$Global_Sales), sep="\t")
cat("\n")
cat("MAE Exp 2 RBF Kernel ", MAE(pred22, test_dataset2$Global_Sales), sep="\t")
cat("\n")
cat("MAE Exp 3 RBF Kernel ", MAE(pred32, test_dataset3$Global_Sales), sep="\t")
cat("\n")
cat("MAE Exp 4 RBF Kernel ", MAE(pred42, test_dataset4$Global_Sales), sep="\t")

## INSIGHT
# - In general, this model produces quite poor performance, where the RMSE values are close to 1.
# - In addition, it can be concluded that Experiment_13 causes the data underfitting, where weak generalization to test data despite good results on the training data.
# - From the MAE training and test values, it can be seen that during training, the model had already seen the training set since most of the experiments having the MAE test values is higher than the MAE training values, except Experiment_13.

#=========================
# MODEL COMPARISON
#=========================

# - It can be concluded that random forest regression is the most suitable model for the dataset without any treatment (Dataset_1).
# - This is because the RMSE train and test scores are not much different (not overfitting or underfitting the data).
# - However, if we look at the MAE train and test values, it can be concluded that the model has seen the training set since most of the experiments having the MAE test values is higher than the MAE training values.
# - Furthermore, for the data scaling dataset, SVM (RBF kernel) is the best model in Dataset_2.
# - This is because the difference between the RMSE train and test values is smaller than the multiple linear regression model.
# - In addition, for datasets that are carried out with outlier removal, data scaling, and differences in train and test data, multiple linear regression is the best model because the RMSE train and test values are smaller than the SVM model (RBF kernel).

