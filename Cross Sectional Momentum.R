# Import Libraries
library(readxl)
library(xts)

# Set up Working Directory PLEASE CHANGE THIS TO RUN CODE 
setwd("")

# Get data
prices <- read_excel("Adjusted_price_dax.xlsx")
returns <- read_excel("returns dax.xlsx")


# Turn Date into index 
prices <- xts(prices[,-1], order.by = as.Date(prices$Date))
returns <- xts(returns[,-1], order.by = as.Date(returns$Date))

# Companies 
colnames(prices)
colnames(returns)

# Basic plot
plot(prices, main = "Price of Stocks", ylab = "Price", xlab = "Date", type = "l")
plot(returns, main = "Log Returns of Stocks", ylab = "Return", xlab = "Date", type = "l")

# Set parameters
T <- dim(returns)[1]
N <- dim(returns)[2]

# Keep account of winners
winners <- matrix(0, nrow = 10, ncol = N)
colnames(winners) <- colnames(returns)
rownames(winners) <- c("(1,1)", "(2,2)", "(3,3)", "(4,4)", "(5,5)", "(6,6)",
                       "(12,12)", "(3,1)", "(6,1)", "(12,1)")

# Cross sectional momentum (1, 1) 
profit_1_1 <- c()

for(t in 1:(T-1)){
  profit_t_1 = 0
  market_return <- as.numeric(mean(returns[t,]))
  for(comp in 1:N){
    return_i <- as.numeric(returns[t,comp])
    weight_comp <- 1/N*(return_i - market_return)
    profit_t_1 = profit_t_1 + weight_comp*returns[t+1,comp]
    if(return_i>market_return){
      winners[1, comp] <- winners[1, comp] + 1
    }
  }
  profit_1_1 <- c(profit_1_1, profit_t_1)
}

# Get info on how many times a stock was a winner stock as a percentage of everytime
winners[1, ] <- winners[1, ]/(length(profit_1_1))
sort(winners[1,])

# Show profits for each period
profit_1_1

# Create a date vector
dates <- seq.Date(from = as.Date("2003-11-30"), to = as.Date("2024-09-30"), by = "month")

# Data visualization

plot(xts(profit_1_1, order.by = dates), main="Profit of cross sectional momentum (1,1)", ylab="Profit")

cummulative_profits <- xts(cumsum(profit_1_1), order.by=dates)
plot(cummulative_profits, main="Cummulative Profits (1,1)", ylab="Cummulative profits")

# Run a t-test to check for significance

mean(profit_1_1)
t.test(profit_1_1)
quantile(profit_1_1, c(0.025, 0.05, 0.5, 0.95, 0.975))
time_trend <- 1:length(cummulative_profits)
mod <- lm(cummulative_profits~time_trend)
summary(mod)

# Graph and t-test without the Great Recession

dates <- seq.Date(from = as.Date("2010-10-30"), to = as.Date("2024-09-30"), by = "month")

plot(xts(tail(profit_1_1, 168), order.by = dates), main="Profit for each time period (1,1)", ylab="Profit")

plot(xts(cumsum(tail(profit_1_1, 168)), order.by = dates), main="Cummulative Profits (1,1)", ylab="Profit")

mean(tail(profit_1_1, 168))
t.test(tail(profit_1_1, 168))
quantile(tail(profit_1_1, 168), c(0.025, 0.05, 0.5, 0.95, 0.975))
time_trend <- 1:168
mod <- lm(cumsum(tail(profit_1_1, 168))~time_trend)
summary(mod)

# Cross sectional momentum (2, 2) 
profit_2_2 <- c()

for(t in 2:(T-2)){
  profit_t_2 = 0
  market_return <- mean(returns[(t-1):t, ])
  for(comp in 1:N){
    return_i <- mean(returns[(t-1):t, comp])
    weight_comp <- 1/N*(return_i - market_return)
    profit_t_2 = profit_t_2 + weight_comp*sum(returns[(t+1):(t+2),comp])
    if(return_i>market_return){
      winners[2, comp] <- winners[2, comp] + 1
    }
  }
  profit_2_2 <- c(profit_2_2, profit_t_2)
}

# Get info on how many times a stock was a winner stock as a percentage of everytime
winners[2, ] <- winners[2, ]/(length(profit_2_2))
sort(winners[2,])

# Show profits for each period
profit_2_2

# Create a date vector
dates <- seq.Date(from = as.Date("2003-12-31"), to = as.Date("2024-09-30"), by = "month")

# Data visualization

plot(xts(profit_2_2, order.by = dates), main="Profit of cross sectional momentum (2,2)", ylab="Profit")

cummulative_profits <- xts(cumsum(profit_2_2), order.by=dates)
plot(cummulative_profits, main="Cummulative Profits (2,2)", ylab="Cummulative profits")

# Run a t-test to check for significance

mean(profit_2_2)
t.test(profit_2_2)
quantile(profit_2_2, c(0.025, 0.05, 0.5, 0.95, 0.975))
time_trend <- 1:length(cummulative_profits)
mod <- lm(cummulative_profits~time_trend)
summary(mod)

# Graph and t-test without the Great Recession

dates <- seq.Date(from = as.Date("2010-10-30"), to = as.Date("2024-09-30"), by = "month")

plot(xts(tail(profit_2_2, 168), order.by = dates), main="Profit for each time period (2,2)", ylab="Profit")

plot(xts(cumsum(tail(profit_2_2, 168)), order.by = dates), main="Cummulative Profits (2,2)", ylab="Profit")

mean(tail(profit_2_2, 168))
t.test(tail(profit_2_2, 168))
quantile(tail(profit_2_2, 168), c(0.025, 0.05, 0.5, 0.95, 0.975))
time_trend <- 1:168
mod <- lm(cumsum(tail(profit_2_2, 168))~time_trend)
summary(mod)

# Cross sectional momentum (3, 3) 
profit_3_3 <- c()

for(t in 3:(T-3)){
  profit_t_3 = 0
  market_return <- mean(returns[(t-2):t, ])
  for(comp in 1:N){
    return_i <- mean(returns[(t-2):t, comp])
    weight_comp <- 1/N*(return_i - market_return)
    profit_t_3 = profit_t_3 + weight_comp*sum(returns[(t+1):(t+3),comp])
    if(return_i>market_return){
      winners[3, comp] <- winners[3, comp] + 1
    }
  }
  profit_3_3 <- c(profit_3_3, profit_t_3)
}

# Get info on how many times a stock was a winner stock as a percentage of everytime
winners[3, ] <- winners[3, ]/(length(profit_3_3))
sort(winners[3,])

# Show profits for each period
profit_3_3

# Create a date vector
dates <- seq.Date(from = as.Date("2004-03-02"), to = as.Date("2024-09-30"), by = "month")

# Data visualization

plot(xts(profit_3_3, order.by = dates), main="Profit of cross sectional momentum (3,3)", ylab="Profit")

cummulative_profits <- xts(cumsum(profit_3_3), order.by=dates)
plot(cummulative_profits, main="Cummulative Profits (3,3)", ylab="Cummulative profits")

# Run a t-test to check for significance

mean(profit_3_3)
t.test(profit_3_3)
quantile(profit_3_3, c(0.025, 0.05, 0.5, 0.95, 0.975))
time_trend <- 1:length(cummulative_profits)
mod <- lm(cummulative_profits~time_trend)
summary(mod)

# Graph and t-test without the Great Recession

dates <- seq.Date(from = as.Date("2010-10-30"), to = as.Date("2024-09-30"), by = "month")

plot(xts(tail(profit_3_3, 168), order.by = dates), main="Profit for each time period (3,3)", ylab="Profit")

plot(xts(cumsum(tail(profit_3_3, 168)), order.by = dates), main="Cummulative Profits (3,3)", ylab="Profit")

mean(tail(profit_3_3, 168))
t.test(tail(profit_3_3, 168))
quantile(tail(profit_3_3, 168), c(0.025, 0.05, 0.5, 0.95, 0.975))
time_trend <- 1:168
mod <- lm(cumsum(tail(profit_3_3, 168))~time_trend)
summary(mod)

# Cross sectional momentum (4, 4) 
profit_4_4 <- c()

for(t in 4:(T-4)){
  profit_t_4 = 0
  market_return <- mean(returns[(t-3):t, ])
  for(comp in 1:N){
    return_i <- mean(returns[(t-3):t, comp])
    weight_comp <- 1/N*(return_i - market_return)
    profit_t_4 = profit_t_4 + weight_comp*sum(returns[(t+1):(t+4),comp])
    if(return_i>market_return){
      winners[4, comp] <- winners[4, comp] + 1
    }
  }
  profit_4_4 <- c(profit_4_4, profit_t_4)
}

# Get info on how many times a stock was a winner stock as a percentage of everytime
winners[4, ] <- winners[4, ]/(length(profit_4_4))
sort(winners[4,])

# Show profits for each period
profit_4_4

# Create a date vector
dates <- seq.Date(from = as.Date("2004-05-01"), to = as.Date("2024-09-30"), by = "month")

# Data visualization

plot(xts(profit_4_4, order.by = dates), main="Profit of cross sectional momentum (4,4)", ylab="Profit")

cummulative_profits <- xts(cumsum(profit_4_4), order.by=dates)
plot(cummulative_profits, main="Cummulative Profits (4,4)", ylab="Cummulative profits")

# Run a t-test to check for significance

mean(profit_4_4)
t.test(profit_4_4)
quantile(profit_4_4, c(0.025, 0.05, 0.5, 0.95, 0.975))
time_trend <- 1:length(cummulative_profits)
mod <- lm(cummulative_profits~time_trend)
summary(mod)

# Graph and t-test without the Great Recession

dates <- seq.Date(from = as.Date("2010-10-30"), to = as.Date("2024-09-30"), by = "month")

plot(xts(tail(profit_4_4, 168), order.by = dates), main="Profit for each time period (4,4)", ylab="Profit")

plot(xts(cumsum(tail(profit_4_4, 168)), order.by = dates), main="Cummulative Profits (4,4)", ylab="Profit")

mean(tail(profit_4_4, 168))
t.test(tail(profit_4_4, 168))
quantile(tail(profit_4_4, 168), c(0.025, 0.05, 0.5, 0.95, 0.975))
time_trend <- 1:168
mod <- lm(cumsum(tail(profit_4_4, 168))~time_trend)
summary(mod)

# Cross sectional momentum (5, 5) 
profit_5_5 <- c()

for(t in 5:(T-5)){
  profit_t_5 = 0
  market_return <- mean(returns[(t-4):t, ])
  for(comp in 1:N){
    return_i <- mean(returns[(t-4):t, comp])
    weight_comp <- 1/N*(return_i - market_return)
    profit_t_5 = profit_t_5 + weight_comp*sum(returns[(t+1):(t+5),comp])
    if(return_i>market_return){
      winners[5, comp] <- winners[5, comp] + 1
    }
  }
  profit_5_5 <- c(profit_5_5, profit_t_5)
}

# Get info on how many times a stock was a winner stock as a percentage of everytime
winners[5, ] <- winners[5, ]/(length(profit_5_5))
sort(winners[5,])

# Show profits for each period
profit_5_5

# Create a date vector
dates <- seq.Date(from = as.Date("2004-07-01"), to = as.Date("2024-09-30"), by = "month")

# Data visualization

plot(xts(profit_5_5, order.by = dates), main="Profit of cross sectional momentum (5,5)", ylab="Profit")

cummulative_profits <- xts(cumsum(profit_5_5), order.by=dates)
plot(cummulative_profits, main="Cummulative Profits (5,5)", ylab="Cummulative profits")

# Run a t-test to check for significance

mean(profit_5_5)
t.test(profit_5_5)
quantile(profit_5_5, c(0.025, 0.05, 0.5, 0.95, 0.975))
time_trend <- 1:length(cummulative_profits)
mod <- lm(cummulative_profits~time_trend)
summary(mod)

# Graph and t-test without the Great Recession

dates <- seq.Date(from = as.Date("2010-10-30"), to = as.Date("2024-09-30"), by = "month")

plot(xts(tail(profit_5_5, 168), order.by = dates), main="Profit for each time period (5,5)", ylab="Profit")

plot(xts(cumsum(tail(profit_5_5, 168)), order.by = dates), main="Cummulative Profits (5,5)", ylab="Profit")

mean(tail(profit_5_5, 168))
t.test(tail(profit_5_5, 168))
quantile(tail(profit_5_5, 168), c(0.025, 0.05, 0.5, 0.95, 0.975))
time_trend <- 1:168
mod <- lm(cumsum(tail(profit_5_5, 168))~time_trend)
summary(mod)

# Cross sectional momentum (6, 6) 
profit_6_6 <- c()

for(t in 6:(T-6)){
  profit_t_6 = 0
  market_return <- mean(returns[(t-5):t, ])
  for(comp in 1:N){
    return_i <- mean(returns[(t-5):t, comp])
    weight_comp <- 1/N*(return_i - market_return)
    profit_t_6 = profit_t_6 + weight_comp*sum(returns[(t+1):(t+6),comp])
    if(return_i>market_return){
      winners[6, comp] <- winners[6, comp] + 1
    }
  }
  profit_6_6 <- c(profit_6_6, profit_t_6)
}

# Get info on how many times a stock was a winner stock as a percentage of everytime
winners[6, ] <- winners[6, ]/(length(profit_6_6))
sort(winners[6,])

# Show profits for each period
profit_6_6

# Create a date vector
dates <- seq.Date(from = as.Date("2004-09-01"), to = as.Date("2024-09-30"), by = "month")

# Data visualization

plot(xts(profit_6_6, order.by = dates), main="Profit of cross sectional momentum (6,6)", ylab="Profit")

cummulative_profits <- xts(cumsum(profit_6_6), order.by=dates)
plot(cummulative_profits, main="Cummulative Profits (6,6)", ylab="Cummulative profits")

# Run a t-test to check for significance

mean(profit_6_6)
t.test(profit_6_6)
quantile(profit_6_6, c(0.025, 0.05, 0.5, 0.95, 0.975))
time_trend <- 1:length(cummulative_profits)
mod <- lm(cummulative_profits~time_trend)
summary(mod)

# Graph and t-test without the Great Recession

dates <- seq.Date(from = as.Date("2010-10-30"), to = as.Date("2024-09-30"), by = "month")

plot(xts(tail(profit_6_6, 168), order.by = dates), main="Profit for each time period (6,6)", ylab="Profit")

plot(xts(cumsum(tail(profit_6_6, 168)), order.by = dates), main="Cummulative Profits (6,6)", ylab="Profit")

mean(tail(profit_6_6, 168))
t.test(tail(profit_6_6, 168))
quantile(tail(profit_6_6, 168), c(0.025, 0.05, 0.5, 0.95, 0.975))
time_trend <- 1:168
mod <- lm(cumsum(tail(profit_6_6, 168))~time_trend)
summary(mod)

# Cross sectional momentum (12, 12) 
profit_12_12 <- c()

for(t in 12:(T-12)){
  profit_t_12 = 0
  market_return <- mean(returns[(t-11):t, ])
  for(comp in 1:N){
    return_i <- mean(returns[(t-11):t, comp])
    weight_comp <- 1/N*(return_i - market_return)
    profit_t_12 = profit_t_12 + weight_comp*sum(returns[(t+1):(t+12),comp])
    if(return_i>market_return){
      winners[7, comp] <- winners[7, comp] + 1
    }
  }
  profit_12_12 <- c(profit_12_12, profit_t_12)
}

# Get info on how many times a stock was a winner stock as a percentage of everytime
winners[7, ] <- winners[7, ]/(length(profit_12_12))
sort(winners[7,])

# Show profits for each period
profit_12_12

# Create a date vector
dates <- seq.Date(from = as.Date("2005-09-30"), to = as.Date("2024-09-30"), by = "month")

# Data visualization

plot(xts(profit_12_12, order.by = dates), main="Profit of cross sectional momentum (12,12)", ylab="Profit")

cummulative_profits <- xts(cumsum(profit_12_12), order.by=dates)
plot(cummulative_profits, main="Cummulative Profits (12,12)", ylab="Cummulative profits")

# Run a t-test to check for significance

mean(profit_12_12)
t.test(profit_12_12)
quantile(profit_12_12, c(0.025, 0.05, 0.5, 0.95, 0.975))
time_trend <- 1:length(cummulative_profits)
mod <- lm(cummulative_profits~time_trend)
summary(mod)


# Graph and t-test without the Great Recession

dates <- seq.Date(from = as.Date("2010-10-30"), to = as.Date("2024-09-30"), by = "month")

plot(xts(tail(profit_12_12, 168), order.by = dates), main="Profit for each time period (12,12)", ylab="Profit")

plot(xts(cumsum(tail(profit_12_12, 168)), order.by = dates), main="Cummulative Profits (12,12)", ylab="Profit")

mean(tail(profit_12_12, 168))
t.test(tail(profit_12_12, 168))
quantile(tail(profit_12_12, 168), c(0.025, 0.05, 0.5, 0.95, 0.975))
time_trend <- 1:168
mod <- lm(cumsum(tail(profit_12_12, 168))~time_trend)
summary(mod)

# Cross sectional momentum (3, 1) 
profit_3_1 <- c()

for(t in 3:(T-1)){
  profit_3t_1 = 0
  market_return <- mean(returns[(t-2):t, ])
  for(comp in 1:N){
    return_i <- mean(returns[(t-2):t, comp])
    weight_comp <- 1/N*(return_i - market_return)
    profit_3t_1 = profit_3t_1 + weight_comp*sum(returns[t+1,comp])
    if(return_i>market_return){
      winners[8, comp] <- winners[8, comp] + 1
    }
  }
  profit_3_1 <- c(profit_3_1, profit_3t_1)
}

# Get info on how many times a stock was a winner stock as a percentage of everytime
winners[8, ] <- winners[8, ]/(length(profit_3_1))
sort(winners[8,])

# Show profits for each period
profit_3_1

# Create a date vector
dates <- seq.Date(from = as.Date("2004-01-30"), to = as.Date("2024-09-30"), by = "month")

# Data visualization

plot(xts(profit_3_1, order.by = dates), main="Profit of cross sectional momentum (3,1)", ylab="Profit")

cummulative_profits <- xts(cumsum(profit_3_1), order.by=dates)
plot(cummulative_profits, main="Cummulative Profits (3,1)", ylab="Cummulative profits")

# Run a t-test to check for significance

mean(profit_3_1)
t.test(profit_3_1)
quantile(profit_3_1, c(0.025, 0.05, 0.5, 0.95, 0.975))
time_trend <- 1:length(cummulative_profits)
mod <- lm(cummulative_profits~time_trend)
summary(mod)


# Graph and t-test without the Great Recession

dates <- seq.Date(from = as.Date("2010-10-30"), to = as.Date("2024-09-30"), by = "month")

plot(xts(tail(profit_3_1, 168), order.by = dates), main="Profit for each time period (3,1)", ylab="Profit")

plot(xts(cumsum(tail(profit_3_1, 168)), order.by = dates), main="Cummulative Profits (3,1)", ylab="Profit")

mean(tail(profit_3_1, 168))
t.test(tail(profit_3_1, 168))
quantile(tail(profit_3_1, 168), c(0.025, 0.05, 0.5, 0.95, 0.975))
time_trend <- 1:168
mod <- lm(cumsum(tail(profit_3_1, 168))~time_trend)
summary(mod)

# Cross sectional momentum (6, 1) 
profit_6_1 <- c()

for(t in 6:(T-1)){
  profit_6t_1 = 0
  market_return <- mean(returns[(t-5):t, ])
  for(comp in 1:N){
    return_i <- mean(returns[(t-5):t, comp])
    weight_comp <- 1/N*(return_i - market_return)
    profit_6t_1 = profit_6t_1 + weight_comp*sum(returns[t+1,comp])
    if(return_i>market_return){
      winners[9, comp] <- winners[9, comp] + 1
    }
  }
  profit_6_1 <- c(profit_6_1, profit_6t_1)
}

# Get info on how many times a stock was a winner stock as a percentage of everytime
winners[9, ] <- winners[9, ]/(length(profit_6_1))
sort(winners[9,])

# Show profits for each period
profit_6_1

# Create a date vector
dates <- seq.Date(from = as.Date("2004-04-30"), to = as.Date("2024-09-30"), by = "month")

# Data visualization

plot(xts(profit_6_1, order.by = dates), main="Profit of cross sectional momentum (6,1)", ylab="Profit")

cummulative_profits <- xts(cumsum(profit_6_1), order.by=dates)
plot(cummulative_profits, main="Cummulative Profits (6,1)", ylab="Cummulative profits")

# Run a t-test to check for significance

mean(profit_6_1)
t.test(profit_6_1)
quantile(profit_6_1, c(0.025, 0.05, 0.5, 0.95, 0.975))
time_trend <- 1:length(cummulative_profits)
mod <- lm(cummulative_profits~time_trend)
summary(mod)

# Graph and t-test without the Great Recession

dates <- seq.Date(from = as.Date("2010-10-30"), to = as.Date("2024-09-30"), by = "month")

plot(xts(tail(profit_6_1, 168), order.by = dates), main="Profit for each time period (6,1)", ylab="Profit")

plot(xts(cumsum(tail(profit_6_1, 168)), order.by = dates), main="Cummulative Profits (6,1)", ylab="Profit")

mean(tail(profit_6_1, 168))
t.test(tail(profit_6_1, 168))
quantile(tail(profit_6_1, 168), c(0.025, 0.05, 0.5, 0.95, 0.975))
time_trend <- 1:168
mod <- lm(cumsum(tail(profit_6_1, 168))~time_trend)
summary(mod)

# Cross sectional momentum (12, 1) 
profit_12_1 <- c()

for(t in 12:(T-1)){
  profit_12t_1 = 0
  market_return <- mean(returns[(t-11):t, ])
  for(comp in 1:N){
    return_i <- mean(returns[(t-11):t, comp])
    weight_comp <- 1/N*(return_i - market_return)
    profit_12t_1 = profit_12t_1 + weight_comp*sum(returns[t+1,comp])
    if(return_i>market_return){
      winners[10, comp] <- winners[10, comp] + 1
    }
  }
  profit_12_1 <- c(profit_12_1, profit_12t_1)
}

# Get info on how many times a stock was a winner stock as a percentage of everytime
winners[10, ] <- winners[10, ]/(length(profit_12_1))
sort(winners[10,])

# Show profits for each period
profit_12_1

# Create a date vector
dates <- seq.Date(from = as.Date("2004-10-30"), to = as.Date("2024-09-30"), by = "month")

# Data visualization

plot(xts(profit_12_1, order.by = dates), main="Profit of cross sectional momentum (12,1)", ylab="Profit")

cummulative_profits <- xts(cumsum(profit_12_1), order.by=dates)
plot(cummulative_profits, main="Cummulative Profits (12,1)", ylab="Cummulative profits")

# Run a t-test to check for significance

mean(profit_12_1)
t.test(profit_12_1)
quantile(profit_12_1, c(0.025, 0.05, 0.5, 0.95, 0.975))
time_trend <- 1:length(cummulative_profits)
mod <- lm(cummulative_profits~time_trend)
summary(mod)

# Graph and t-test without the Great Recession

dates <- seq.Date(from = as.Date("2010-10-30"), to = as.Date("2024-09-30"), by = "month")

plot(xts(tail(profit_12_1, 168), order.by = dates), main="Profit for each time period (12,1)", ylab="Profit")

plot(xts(cumsum(tail(profit_12_1, 168)), order.by = dates), main="Cummulative Profits (12,1)", ylab="Profit")

mean(tail(profit_12_1, 168))
t.test(tail(profit_12_1, 168))
quantile(tail(profit_12_1, 168), c(0.025, 0.05, 0.5, 0.95, 0.975))
time_trend <- 1:168
mod <- lm(cumsum(tail(profit_12_1, 168))~time_trend)
summary(mod)

# check the winners
sort(colMeans(winners))

# Exclude Covid
profits <- data.frame(tail(profit_1_1, 168), tail(profit_2_2, 168), tail(profit_3_3, 168),
                      tail(profit_4_4, 168), tail(profit_5_5, 168), tail(profit_6_6, 168),
                      tail(profit_12_12,168), tail(profit_3_1, 168), tail(profit_6_1,168), 
                      tail(profit_12_1, 168))
colnames(profits) <- c("p(1,1)", "p(2,2)", "p(3,3)", "p(4,4)", "p(5,5)", "p(6,6)", "p(12,12)", "p(3,1)", "p(6,1)", "p(12,1)")

cummulative_profits <- data.frame(
  cumsum(tail(profit_1_1, 168)), cumsum(tail(profit_2_2, 168)), 
  cumsum(tail(profit_3_3, 168)), cumsum(tail(profit_4_4, 168)),
  cumsum(tail(profit_5_5, 168)), cumsum(tail(profit_6_6, 168)),
  cumsum(tail(profit_12_12, 168)), cumsum(tail(profit_3_1, 168)),
  cumsum(tail(profit_6_1, 168)), cumsum(tail(profit_12_1, 168)))

colnames(cummulative_profits) <- c("p(1,1)", "p(2,2)", "p(3,3)", "p(4,4)", "p(5,5)", "p(6,6)", "p(12,12)", "p(3,1)", "p(6,1)", "p(12,1)")

dates <- seq.Date(from = as.Date("2010-10-30"), to = as.Date("2024-09-30"), by = "month")

profits <- xts(profits, order.by = dates)
cummulative_profits <- xts(cummulative_profits, order.by = dates)

plot(profits, main="Profit for Each Time Period", col=1:10)
legend("topleft", legend=colnames(profits), lty=1, ncol=5)

plot(cummulative_profits, main="Cummulative Profits")
legend("topright", legend = c("p(1,1)", "p(2,2)", "p(3,3)", "p(4,4)", "p(5,5)", "p(6,6)", "p(12,12)", "p(3,1)", "p(6,1)", "p(12,1)"), 
       col = c("black", "red", "blue", "green", "yellow", "gray", "brown", "purple", "cyan", "pink"), lwd=2)

# Does price trends affects chances of being a winner stock?

# Get the trends
trends <- c()
time <- 1:dim(prices)[1]
alpha = 0.05
for(company in colnames(prices)){
  mod <- lm(prices[,company]~time)
  trends <- c(trends, as.numeric(mod$coefficients[2]))
  p_value <- summary(mod)$coefficients[2,4]
  if(p_value<alpha){
    print(paste(company, " has a significant trend of ", as.numeric(mod$coefficients[2])))
  }else{
    print(paste(company, " has an unsignificant trend of ", as.numeric(mod$coefficients[2])))
  }
}

# Answer the question

for(strategy in rownames(winners)){
  mod <- lm(winners[strategy,]~trends)
  coeff <- round(as.numeric(mod$coefficients[2]), 4)
  p_value <- round(summary(mod)$coefficients[2,4], 4)
  print(paste("For Strategy ", strategy, " the coefficient = ", coeff, " and p-value  ≈", p_value))
}

# Kernel Density Estimation of the profits

# Create a 5x2 layout

plot(density(profit_1_1), main="Momentum Strategies", xlab="Profit", ylim=c(0, 1200))
lines(density(profit_2_2), lwd=2, col="red")
lines(density(profit_3_3), lwd=2, col="blue")
lines(density(profit_4_4), lwd=2, col="green")
lines(density(profit_5_5),  lwd=2, col="yellow")
lines(density(profit_6_6), lwd=2, col="gray")
lines(density(profit_12_12), lwd=2, col="brown")
lines(density(profit_3_1), lwd=2, col="purple")
lines(density(profit_6_1), lwd=2, col="cyan")
lines(density(profit_12_1), lwd=2, col="pink")
legend("topright", legend = c("p(1,1)", "p(2,2)", "p(3,3)", "p(4,4)", "p(5,5)", "p(6,6)", "p(12,12)", "p(3,1)", "p(6,1)", "p(12,1)"), 
       col = c("black", "red", "blue", "green", "yellow", "gray", "brown", "purple", "cyan", "pink"), lwd=2)


plot(density(profit_1_1), main="Momentum Strategies", xlab="Profit", ylim=c(0, 500))
lines(density(profit_3_3), lwd=2, col="blue")
lines(density(profit_6_6), lwd=2, col="red")
lines(density(profit_12_12), lwd=2, col="yellow")
legend("topright", legend = c("p(1,1)", "p(3,3)",  "p(6,6)", "p(12,12)"), 
       col = c("black", "blue", "red", "yellow"), lwd=2)

