[Summary of Sales Data Analysis.docx](https://github.com/pelumiway/Sales-Data-Analysis/files/11339335/Summary.of.Sales.Data.Analysis.docx)
# Sales-Data-Analysis
This report presents an analysis of sales data collected from different regions, countries, and sales channels. 
---
title: "R Notebook"
output: html_notebook
---

This is an [R Markdown](http://rmarkdown.rstudio.com) Notebook. When you execute code within the notebook, the results appear beneath the code. 

Try executing this chunk by clicking the *Run* button within the chunk or by placing your cursor inside it and pressing *Ctrl+Shift+Enter*. 

```{r}
library(tidyverse)
sales_data <- read.csv("C:/Users/rebec/Downloads/Sales_Data.csv")
sales_data
```

```{r}
str(sales_data)
names(sales_data)
```
```{r}
summary(sales_data)

```

```{r}
sum(is.na(sales_data))

```
```{r}
library(ggplot2)
ggplot(sales_data, aes(x = Units_Sold)) +
  geom_histogram()

```

```{r}
ggplot(sales_data, aes(x =Units_Sold , y =Unit_SellingPrice )) +geom_point()

```
```{r}
ggplot(sales_data, aes(x = Item_Type)) +
  geom_bar()

```
```{r}
library(tidyverse)
library(caret)
library(neuralnet)
library(dplyr)
set.seed(123)
train_index <- createDataPartition(sales_data$Units_Sold, p = 0.7, list = FALSE)
train_data <- sales_data[train_index, ]
test_data <- sales_data[-train_index, ]

```

```{r}
train_data <- train_data %>%
  mutate(total_revenue = Units_Sold * Unit_SellingPrice)
test_data <- test_data %>%
  mutate(total_revenue = Units_Sold * Unit_SellingPrice)

```

```{r}
lm_model <- lm(Units_Sold ~ Unit_SellingPrice + Item_Type + total_revenue, data = train_data)

```

```{r}
predictions <- predict(lm_model, newdata = test_data)
RMSE <- caret::RMSE(predictions, test_data$Units_Sold)
R2 <- cor(predictions, test_data$Units_Sold)^2

```

```{r}
# Check the data types of variables
str(train_data)

# Convert variables to appropriate data types
train_data$Units_Sold <- as.integer(train_data$Units_Sold)
train_data$Unit_SellingPrice <- as.numeric(gsub(",", "", train_data$Unit_SellingPrice))

# Check for missing values
sum(is.na(train_data$Units_Sold))
sum(is.na(train_data$Unit_SellingPrice))
sum(is.na(train_data$Item_Type))
sum(is.na(train_data$total_revenue))

```
```{r}
# Check the data types of columns
class(train_data$Unit_SellingPrice)
class(train_data$Item_Type)
class(train_data$total_revenue)

# Convert columns to numeric data type
train_data$Unit_SellingPrice <- as.numeric(gsub(",", "", train_data$Unit_SellingPrice))
train_data$total_revenue <- as.numeric(gsub(",", "", train_data$total_revenue))

# Check for missing values
sum(is.na(train_data$Unit_SellingPrice))
sum(is.na(train_data$Item_Type))
sum(is.na(train_data$total_revenue))
```
```{r}
# Check for missing values
sum(is.na(train_data_scaled$Units_Sold))
sum(is.na(train_data_scaled$Unit_SellingPrice))
sum(is.na(train_data_scaled$Item_Type))
sum(is.na(train_data_scaled$total_revenue))

# Create dummy variables for categorical variables
train_data_scaled <- cbind(train_data_scaled, model.matrix(~ Item_Type - 1, data = train_data_scaled))

# Check the data types of the inputs
class(train_data_scaled$Units_Sold)
class(train_data_scaled$Unit_SellingPrice)
class(train_data_scaled$total_revenue)
class(train_data_scaled[,5:ncol(train_data_scaled)])  # check data type of dummy variables

# Check the dimensions of the input data
dim(train_data_scaled)

```



```{r}
# Define neural network architecture
nn_model <- neuralnet(Units_Sold~Unit_SellingPrice + total_revenue,data=train_data_scaled[,c(1,2,4,5,6,7,8,9,10,12,14,15,16,18,20,22,24,25)], 
  hidden = c(5,3), 
  threshold = 0.01 # set convergence threshold
  )
```
```{r}
summary(nn_model)
plot(nn_model$covariate)
```


```{r}
# Compute predicted values
nn_pred <- compute(nn_model,train_data_scaled[,c(1,2,4,5,6,7,8,9,10,12,14,15,16,18,20,22,24,25)])

# Compare predicted and actual values
plot(nn_pred$net.result, train_data_scaled$Units_Sold)
abline(0,1)

```

```{r}
nn_model <- neuralnet(Units_Sold ~ Unit_SellingPrice +  Item_Type+ total_revenue, data = train_data,hidden = c(5, 3))


```

Add a new chunk by clicking the *Insert Chunk* button on the toolbar or by pressing *Ctrl+Alt+I*.

When you save the notebook, an HTML file containing the code and output will be saved alongside it (click the *Preview* button or press *Ctrl+Shift+K* to preview the HTML file).

The preview shows you a rendered HTML copy of the contents of the editor. Consequently, unlike *Knit*, *Preview* does not run any R code chunks. Instead, the output of the chunk when it was last run in the editor is displayed.



