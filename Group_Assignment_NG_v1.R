library(dplyr)
library(ggplot2)
library(readr)


# Set the folder (path) that contains this R file as the working directory
dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(dir)

#read data
data <- read.csv("madrid_transactions.csv", header=TRUE, sep=",")
head(data)

# NEW PLOTS

#what nationalities do shop in these categories?
Country_Spending <- data %>%
  filter(category %in% c("Culture & Leisure", "Transportation", "Books & Music")) %>% 
  filter(amount != 0) %>%
  group_by(customer_country) %>%
  summarise(n = n(),avg_spend=mean(amount),transactions = n)

ggplot(Country_Spending, aes(x = transactions, y = avg_spend, color = customer_country)) + geom_point() + geom_text(aes(label=customer_country),hjust=0, vjust=0)

#transaction represents groups of peoples staying (transaction does not represent one person)
#one conclusion would be to target customer from countries NO, MY, SA and IN since they seems to spend a considerable amount but only have a very few transactions

#Accomodation by top 10 countries total spending

Accomodation <- data %>%
  filter(category %in% c("Accommodation")) %>% 
  group_by(customer_country) %>%
  summarise(n = n(),total_spend=sum(amount),avg_spend=mean(amount), transactions = n) %>%
  top_n(n=10, wt=total_spend) 

a <- ggplot(Accomodation, aes(x=transactions, y=total_spend)) + 
  geom_point(aes(col=customer_country, size=avg_spend)) +
  geom_text(aes(label=customer_country),hjust=0, vjust=0)

a + labs(title= "Top 10 nationalities Spending on Accomodation", x="Transactions",y="Total Amount Spend")

#compare spending over time and day - in
theme_set(theme_classic())

data_day <- data %>% 
  filter(category %in% c("Accommodation"))

a <- ggplot(data_day, aes(x=hour, fill=weekday))+geom_density(alpha=0.7)

a + labs(title= "Compare Spending over Time and Weekday", x="Hour",y="Density")

#conclusion: higher peak on thursdays around 8am, explore if promotions would trigger more transactions during the same time on friday for example
