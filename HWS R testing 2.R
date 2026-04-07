setwd("C:/Users/colef/Documents/GitHub/Housing-Web-Scraping")
set.seed(4)


library(tidyverse)
library(forcats)
library(car)
library(rstatix)
## Polishing and Sampling dataframe:
houses.df <- read_csv(file = "housing_df.csv", show_col_types = FALSE)
houses.df <- houses.df %>% drop_na() %>% distinct()
houses.df$`Property Type` <- factor(houses.df$`Property Type`)
houses.df <- houses.df %>% mutate(`Property Type` = fct_reorder(
  `Property Type`, Price, .fun="length", .desc = TRUE))
houses.df <- houses.df %>% rename(PropertyType = `Property Type`)
houses.df <- houses.df %>% rename(SquareFeet = `Square Feet`)
## Creating first model:
houses.glm1 <- glm(formula = Price~Bedrooms+Bathrooms+SquareFeet+PropertyType+Province+Population,
                   data=houses.df)
#' Prints a concise summary of model
#'
#' @param model A linear model
#' @param name String
my_summary <- function(model, name="Unnamed Model"){
  cat("\"",name,"\"\n")
  cat("Formula: ",as.character(model$formula)[c(2,1,3)],"\n \n")
  printCoefmat(summary(model)$coefficients)
  cat(paste("\nAIC:", model$aic))}
my_summary(houses.glm1, "Naive Model")


houses.df <- houses.df %>%
  select(-new)


#' Calculates percent error between predictions and actual values
#' 
#' @param preds predicted values
#' @param reals actual values
error <- function(preds, reals){
  return(mean(abs(preds - reals) / reals))
}
pred <- predict(houses.glm1, houses.df)
error(pred, houses.df$Price)

houses.glm1.res <- rstandard(houses.glm1)
qqnorm(houses.glm1.res)
qqline(houses.glm1.res)


#' Creates a distribution plot object with relative normal distribution
#' 
#' @param df A Dataframe
#' @param column A string representing a column name
#' @param datatype A string denoting plot type
#' @return distribution plot
distribution_graph <- function(df, column, datatype="bar") {
  y <- if(datatype=="density"){
    geom_density(aes(colour=paste(column,"Distribution")))
  } else {geom_bar(aes(colour=paste(column,"Distribution")))}
  p <- ggplot(data=df, aes(x=.data[[column]])) + 
    ggtitle(paste("Distribution of", column)) + y +
    stat_function(fun=dnorm, aes(colour = "Relative Normal Distribution"),
                  args=list(mean=mean(df[[column]]), sd=sd(df[[column]]))) +
    scale_colour_manual("Legend title", values = c("blue", "red")) +
    theme(legend.position="inside", legend.position.inside = c(0.8, 0.6))
  return(p)
}
# Create the plot
distribution_graph(houses.df, "Price", datatype="density")


## Create new "log(Price)" column for dataset and replot:
houses.df$logPrice <- log(houses.df$Price)
distribution_graph(houses.df, "logPrice", datatype="density")


# Removing outlying logPrices, and making second model:
q99 <- quantile(houses.df$logPrice, probs=c(0.005,0.995))
houses.df <- houses.df %>% filter(logPrice > q99[1]) %>% filter(logPrice < q99[2])
houses.glm2 <- glm(formula = logPrice~Bedrooms+Bathrooms+SquareFeet+PropertyType+Province+Population,
                   data=houses.df)
my_summary(houses.glm2, "Model 2")


pred <- exp(predict(houses.glm2, houses.df))
error(pred, houses.df$Price)

houses.glm2.res <- rstandard(houses.glm2)
qqnorm(houses.glm2.res)
qqline(houses.glm2.res)


houses.df %>% group_by(PropertyType) %>% summarise(count=n())
ggplot(data=houses.df, aes(x=logPrice, color=PropertyType)) +
  geom_boxplot() + ggtitle("Boxplot Distribution for Each Property Type")


# removing 'Land/Lot' and 'Recreational' listings
houses.df <- houses.df %>%
  filter(!(PropertyType %in% c("Land/Lot","Recreational")))
houses.df$PropertyType <- droplevels(houses.df$PropertyType)


## ANOVA test
anova_model <- aov(logPrice ~ PropertyType, data = houses.df)
summary(anova_model)


## Tukey HCD test
tukey_results <- TukeyHSD(anova_model,conf.level = 0.95)
par(mar=c(3,8,3,3))
plot(tukey_results, las=1, cex.axis = 0.6)


# Merging categories
houses.df$PropertyType <- 
  fct_collapse(houses.df$PropertyType, 
               `Tri/Four/Multi` = c("Triplex","Fourplex","Multifamily"))
## Tukey HCD test
anova_model <- aov(logPrice ~ PropertyType, data = houses.df)
tukey_results <- TukeyHSD(anova_model,conf.level = 0.95)
par(mar=c(3,8,3,3))
plot(tukey_results, las=1, cex.axis = 0.6)


# Making model with House and Duplex separate
houses.glm3.1 <- glm(formula = logPrice~Bedrooms+Bathrooms+SquareFeet+PropertyType+Province+Population,
                     data=houses.df)
# Making model with House and Duplex merged
houses.df.2 <- houses.df
houses.df.2$PropertyType <- 
  fct_collapse(houses.df.2$PropertyType, 
               `House/Duplex` = c("House","Duplex"))
houses.glm3.2 <- glm(formula = logPrice~Bedrooms+Bathrooms+SquareFeet+PropertyType+Province+Population,
                     data=houses.df.2)
my_summary(houses.glm3.1, "Separate House/Duplex")
my_summary(houses.glm3.2, "Merged House/Duplex")


pred.1 <- exp(predict(houses.glm3.1, houses.df))
error(pred.1, houses.df$Price)
pred.2 <- exp(predict(houses.glm3.2, houses.df.2))
error(pred.2, houses.df$Price)

par(mar=c(2,2,1,1), mfrow = c(1, 2))
houses.glm3.1.res <- rstandard(houses.glm3.1)
qqnorm(houses.glm3.1.res, ylab = "", xlab = "", main="Separate Q-Q Plot")
qqline(houses.glm3.1.res)

houses.glm3.2.res <- rstandard(houses.glm3.1)
qqnorm(houses.glm3.2.res, ylab = "", xlab = "", main="Merged Q-Q Plot")
qqline(houses.glm3.2.res)


# Choosing official 3rd model
houses.glm3 <- houses.glm3.1


houses.df %>% group_by(Province) %>% summarise(count=n())
ggplot(data=houses.df, aes(x=logPrice, color=Province)) +
  geom_boxplot() + ggtitle("Boxplot Distribution for Each Property Type")


anova_model <- aov(logPrice ~ Province, data = houses.df)
summary(anova_model)


## Tukey HCD test
tukey_results <- TukeyHSD(anova_model,conf.level = 0.95)
par(mar=c(3,8,3,3))
plot(tukey_results, las=1, cex.axis = 0.6)


# Merging categories
houses.df$Province <- 
  fct_collapse(houses.df$Province, 
               `Saskatchewan/Manitoba` = c("Saskatchewan","Manitoba"))
houses.glm4 <- glm(formula = logPrice~Bedrooms+Bathrooms+SquareFeet+PropertyType+Province+Population,
                   data=houses.df)
my_summary(houses.glm4, "Model 4")
pred <- exp(predict(houses.glm4, houses.df))
error(pred, houses.df$Price)



























#############
## TESTING ##
#############
test <- houses.df
test$BB <- test$Bedrooms + test$Bathrooms
cor(test[,3:6])


q99 <- quantile(test$SquareFeet, probs=c(0.005,0.995))
test <- houses.df %>% filter(SquareFeet > q99[1]) %>% filter(SquareFeet < q99[2])

plot(test$SquareFeet, test$logPrice)
plot(sqrt(test$SquareFeet), test$logPrice)

test.glm3 <- glm(formula = logPrice~Bedrooms+Bathrooms+SquareFeet+PropertyType,data=test)

test$Feet <- sqrt(test$SquareFeet)
test$logFeet <- log(test$SquareFeet)
test.glm3.1 <- glm(formula = logPrice~Bedrooms+Bathrooms+Feet+PropertyType,data=test)
test.glm3.2 <- glm(formula = logPrice~Bedrooms+Bathrooms+logFeet+PropertyType,data=test)

cor(test[,c(4:6,8)])
cor(test[,c(4:5,8:9)])
pairs(test[,c(4:6,8)])
pairs(test[,c(4:5,8:9)])

plot(test$SquareFeet, test$logPrice)
abline(lm(test$logPrice ~ test$SquareFeet), col = "red", lwd = 2)
plot(test$logFeet, test$logPrice)
abline(lm(test$logPrice ~ test$logFeet), col = "red", lwd = 2)
plot(test$Feet, test$logPrice)
abline(lm(test$logPrice ~ test$Feet), col = "red", lwd = 2)

pred <- exp(predict(test.glm3.1, test))
error(pred, test$Price)

pred <- exp(predict(test.glm3, test))
error(pred, test$Price)


plot(density(test.glm3.1$residuals))
curve(dnorm(x,mean(test.glm3.1$residuals), sd(test.glm3.1$residuals)), add=TRUE,
      col="red")

plot(density(houses.glm3$residuals))
curve(dnorm(x,mean(houses.glm3$residuals), sd(houses.glm3$residuals)), add=TRUE,
      col="red")

plot(density(houses.glm2$residuals))
curve(dnorm(x,mean(houses.glm2$residuals), sd(houses.glm2$residuals)), add=TRUE,
      col="red")

plot(test.glm3.1$residuals ~ test$SquareFeet)

test.glm3.3 <- glm(formula = logPrice~Bedrooms+Bathrooms+Feet+PropertyType+Province,data=test)

means <- c()
cityList <- levels(test$City)
pops <- c()
for (city in cityList){
  t <- test %>%
    filter(City == city)
  means <- c(means, mean(t$logPrice))
  pops <- c(pops, t$Population[1])
}
test.df <- tibble(
  cities = cityList,
  mean = means,
  pop = pops
)
plot(test.df$pop, test.df$mean)
text(test.df$pop, test.df$mean, labels=test.df$cities)


test %>%
  filter(Bedrooms >= 10)



