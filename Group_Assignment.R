library(dplyr)
library(ggplot2)
library(readr)


# Set the folder (path) that contains this R file as the working directory
dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(dir)

#read data
data <- read.csv("madrid_transactions.csv", header=TRUE, sep=",")
head(data)

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

#Avg spend by category
##library(data.table)
#transactions <- as.data.table(data)

Avg_Spend_CAT <- data %>%
  group_by(category) %>%
  summarise(n = n(),total_spend=sum(amount),Avg_Spend = mean(amount))

ggplot(Avg_Spend_CAT, aes(x = total_spend, y = Avg_Spend, color=category)) + 
  geom_point() + geom_text(aes(label=category),hjust=0, vjust=0)

#average spending for upper left quadrant over time
datax <- data %>% 
  filter(category %in% c("Agencias de viajes", "AutomociÃ³n", "Culture & Leisure", "Transportation", "Books & Music")) %>% 
  group_by(category,hour) %>% 
  summarise(n = n(),total_spend=sum(amount),Avg_Spend = mean(amount), hour = hour, category = category)

ggplot(datax, aes(x=hour, y=Avg_Spend, color = category)) + geom_line() + geom_point()

#volumne for upper left quadrant over time (only taking into consideration categories with more than 10 transactions)
datax <- data %>% 
  filter(category %in% c("Culture & Leisure", "Transportation", "Books & Music")) %>% 
  group_by(category, hour) %>% 
  summarise(n = n(),transactions = n, category = category, hour = hour)

ggplot(datax, aes(x=hour, y=transactions, color = category)) + geom_line() + geom_point()

#what nationalities do shop in these categories?
Country_Spending <- data %>%
  filter(category %in% c("Culture & Leisure", "Transportation", "Books & Music")) %>% 
  filter(amount != 0) %>%
  group_by(customer_country) %>%
  summarise(n = n(),avg_spend=mean(amount),transactions = n)

ggplot(Country_Spending, aes(x = transactions, y = avg_spend, color = customer_country)) + geom_point() + geom_text(aes(label=customer_country),hjust=0, vjust=0)

#one conclusion would be to target customer from countries NO, MY, SA and IN since they seems to spend a considerable amount but only have a very few transactions

#Accomodation by top 10 countries total spending

Accomodation <- data %>%
  filter(category %in% c("Accommodation")) %>% 
  group_by(customer_country) %>%
  summarise(n = n(),total_spend=sum(amount),avg_spend=mean(amount), transactions = n) %>%
  top_n(n=10, wt=total_spend) 

ggplot(Accomodation, aes(x=transactions, y=total_spend)) + 
  geom_point(aes(col=customer_country, size=avg_spend)) +
  geom_text(aes(label=customer_country),hjust=0, vjust=0)

#spending on accomodation over time - check why top_n does not work
data_over_time <- data %>% 
  filter(category %in% c("Accommodation")) %>% 
  group_by(customer_country, hour) %>% 
  summarise(n = n(),total_spend=sum(amount),avg_spend=mean(amount), hour = hour) %>%
  top_n(n=10, wt=total_spend) 

ggplot(data_over_time, aes(x=hour, y=total_spend, color = customer_country)) + geom_line() + geom_point()

#compare spending over time and day
data_day <- data %>% 
  filter(category %in% c("Accommodation"))

ggplot(data_day, aes(x=hour, fill=weekday))+geom_density(alpha=0.7)

#conclusion: higher peak on thursdays around 8am, explore if promotions would trigger more transactions on friday for example
