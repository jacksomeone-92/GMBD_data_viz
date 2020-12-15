library(dplyr)
library(ggplot2)
library(readr)


# Set the folder (path) that contains this R file as the working directory
dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(dir)

#read data
data <- read.csv("madrid_transactions.csv", header=TRUE, sep=",")
head(data)

#first EDA
#install.packages('DataExplorer')
library(DataExplorer)

create_report(data)

#Is there a particular time in which tourists are buying?
a <- ggplot(data, aes(hour)) + geom_histogram(bins = 24)
a

#compare amount and time

#convert daytime and hour to factors for boxplots
data$daytime <- as.factor(data$daytime)
data$hour <- as.factor(data$hour)

#boxplot
a<-ggplot(data, aes(x=daytime, y=amount, color = daytime))
b<-ggplot(data, aes(x=hour, y=amount, color = hour))
a + geom_boxplot(outlier.shape = NA) + scale_y_continuous(limits = quantile(data$amount, c(0.1, 0.9)))
b + geom_boxplot(outlier.shape = NA) + scale_y_continuous(limits = quantile(data$amount, c(0.1, 0.9)))

#Which nationality has the highest average spending and why?
library(dplyr)
data1 <- data %>% 
  group_by(customer_country) %>% 
  summarise(amount = mean(amount))

data1 <- data1 %>% 
  filter(amount > 150)

ggplot(data1, aes(reorder(customer_country, -amount, sum), amount)) + geom_col()

#Is there any relationship between day of the week and consumption?
data2 <- data %>% 
  group_by(weekday) %>% 
  summarise(amount = sum(amount))


ggplot(data2, aes(reorder(weekday,-amount, sum), amount)) + geom_col()

#Do high end fashion retailers need to focus more on attracting Australian visitors than on Chinese? 

data3 <- data %>% 
  filter(category == "Fashion & Shoes") %>% 
  filter(amount > 200) %>% 
  group_by(customer_country) %>% 
  summarise(amount = mean(amount))

c <- ggplot(data3, aes(reorder(customer_country, -amount), amount)) + geom_col()
c + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
