setwd("C:/Users/colef/Documents/GitHub/Housing-Web-Scraping")
library(tidyverse)
library(ggsci)
library(car)

# Read in csv file as a tibble,
houses.df <- read_csv(file = "housing_df.csv", show_col_types = FALSE)
# remove any data with invalid values,
houses.df <- houses.df %>%
  drop_na()
# get rid of duplicates (some listings appeared multiple times),
houses.df <- houses.df %>%
  distinct()
# make Property Type a factor, and order it
houses.df$`Property Type` <- factor(houses.df$`Property Type`)
houses.df <- houses.df %>%
  mutate(`Property Type` = fct_reorder(`Property Type`, Price, 
                                       .fun="length", .desc = TRUE))

houses.glm1 <- glm(formula = Price~Bedrooms+Bathrooms+`Square Feet`+`Property Type`,
                data=houses.df)
summary(houses.glm1)



#' Creates quantiles of middle 99% range of data and distribution plot
#' 
#' @param df A Dataframe
#' @param column A string representing a column name
#' @param datatype A string denoting plot type
#' @return list of quantile values
#' @return distribution plot
distribution_graph <- function(df, column, datatype="bar") {
  q <- quantile(df[[column]], probs=c(0,0.005,0.995,1))
  y <- if(datatype=="density"){geom_density()}else{geom_bar()}
  p <- ggplot(data=df, aes(x=.data[[column]])) + y +
    ggtitle(paste("Distribution of", column)) +
    geom_vline(xintercept=q[c(2,3)], linetype="dashed", color="red") + 
    geom_vline(xintercept=q[c(1,4)], linetype="dashed", color="blue")
  return(list(quantiles=q,plot=p))
}

# filter out outlying prices and redo plotting:
houses.df <- houses.df %>%
  filter(Price <= 5000000) %>%
  filter(Price >= 80000)
houses.df <- houses.df %>%
  filter(`Square Feet` <= 10000)

# multi-family merging
houses.df[houses.df$`Property Type` %in% 
            c("Triplex","Fourplex"),]$`Property Type` <- 
  "Multifamily"

# removing categories
houses.df <- houses.df %>%
  filter(!(`Property Type` %in% c("Land/Lot","Recreational")))

houses.glm2 <- glm(formula = Price~Bedrooms+Bathrooms+`Square Feet`+`Property Type`, 
                  data=houses.df, family=gaussian())
summary(houses.glm2)



houses.glm2 <- glm(formula = log(Price)~Bedrooms+Bathrooms+`Square Feet`+`Property Type`, 
                   data=houses.df)
summary(houses.glm2)




houses.df$logPrice <- log(houses.df$Price)
ggplot(data=houses.df, aes(x=logPrice)) + geom_density()



plot(houses.df$Price, rstandard(houses.glm2))

houses.glm3 <- glm(formula = Price~Bedrooms+Bathrooms+`Square Feet`+`Property Type`, 
                   data=houses.df, family=gaussian(link="log"))

houses.glm4 <- glm(formula = log(Price)~Bedrooms+Bathrooms+`Square Feet`+`Property Type`, 
                   data=houses.df, family=gaussian(link="log"))


res <- rstandard(houses.glm2)
plot(houses.df$Price, res)
abline(-3,0,col="red",lty=2)
abline(3,0,col="red",lty=2)


qqnorm(res)
qqline(res)
plot(density(res))
curve(dnorm(x,mean=0))






vif(houses.glm2)

houses.glm2 <- glm(formula = Price~Bedrooms+Bathrooms+`Square Feet`+`Property Type`+`Province`, 
                   data=houses.df, family = Gamma(link = "log"))



houses.df.numeric <- houses.df %>%
  select(c(Price, Bedrooms, Bathrooms, `Square Feet`))












new_data <- tibble(
  Province = c("Ontario"),
  City = c("waterloo-on"),
  Bedrooms = c(3),
  Bathrooms = c(3),
  `Square Feet` = c(1913),
  `Property Type` = c("Townhouse")
)
exp(predict(houses.glm, newdata = new_data))
predict(houses.lm, newdata=new_data)

houses.df$Price - houses.lm$fitted.values

obs <- slice_sample(houses.df, n=100)
obs <- obs %>%
  arrange(Price)
obs <- obs %>%
  mutate(index = row_number())

houses.df$PredLM <- predict(houses.lm, newdata=houses.df)
houses.df$PredGLM <- exp(predict(houses.glm, newdata=houses.df))

houses.df$difLM <- abs(houses.df$PredLM - houses.df$Price)
houses.df$difGLM <- abs(houses.df$PredGLM - houses.df$Price)

long <- houses.df %>%
  pivot_longer(
    cols = c(difLM, difGLM),
    names_to="Model"
  )



ggplot(data=obsLong, aes(x=index, y=value, color=Model)) +
  geom_point() + scale_color_d3()


residuals(houses.glm, type="deviance")








houses.df <- houses.df %>%
  filter(Price <= 5000000) %>%
  filter(Price >= 80000)

houses.df <- houses.df %>%
  filter(`Square Feet` <= 10000)


houses.df[houses.df$`Property Type` %in% 
            c("Triplex","Fourplex"),]$`Property Type` <- 
  "Multifamily"

houses.df <- houses.df %>%
  filter(!(`Property Type` %in% c("Land/Lot","Recreational")))


houses.lm2 <- lm(formula = Price~Bedrooms+Bathrooms+`Square Feet`+`Property Type`, 
                 data=houses.df)
summary(houses.lm2)






houses.df.numeric <- houses.df %>%
  select(-c(Province, City, `Property Type`))
c <- cor.test(houses.df.numeric)
corrplot(c, type="upper")






ggplot(data=houses.df[houses.df$Price<=3000000,], 
       aes(x=Price, fill=`Province`, color=`Province`)) +
  geom_density(linewidth=1,alpha=0.1) + ggtitle("Price Boxplot for Each Property Type") +
  theme(legend.position="inside", legend.position.inside = c(0.8, 0.6)) +
  scale_color_d3() + scale_fill_d3()

houses.glm = glm(formula = Price~Bedrooms+Bathrooms+`Square Feet`+`Property Type`,
                 data = houses.df, family=)










test <- distribution_graph(houses.df, "Price","density")

test$plot +
  stat_function(fun=dgamma(x=Price,shape=5))
curve(dgamma(x, shape=5), add=TRUE, from=0, to=5000000)

test$plot + 
  stat_function(fun=dgamma, args=list(shape=4,scale=180000), 
                color="green", show.legend = TRUE) +
  labs(color="Legend")

ggplot(data = houses.df, aes(x=Price)) +
  geom_density() +
  stat_function(fun=dgamma, args=list(shape=4,scale=180000), color="red")



houses.df.numeric <- houses.df %>%
  select(-c(Province, City, `Property Type`))

h



houses.x <- model.matrix(Price~Bedrooms+Bathrooms+`Square Feet`+`Property Type`, 
                         data = houses.df)
houses.y <- houses.df$Price
houses.fit <- cv.glmnet(houses.x, houses.y, family = Gamma(link="log"), alpha = 0)
       
glmnet(formula = Price ~ Bedrooms+Bathrooms+`Square Feet`+`Property Type`,
       data=houses.df)
       
coef_min = coef(houses.fit, s = "lambda.min")
lambda.min <- houses.fit$lambda.min

houses.glm <- glmnet(houses.x, houses.y, c 
                     alpha = 0, lambda=lambda.min)

ggplot(data=houses.df[houses.df$Price<=3000000,], 
       aes(x=Price, fill=Province, color=Province)) +
  geom_density(linewidth=1,alpha=0.1) + ggtitle("Price Boxplot for Each Property Type") +
  theme(legend.position="inside", legend.position.inside = c(0.8, 0.6)) +
  scale_color_d3() + scale_fill_d3()
















## Set aside for sample

## use log link
# Check normality











#################
setwd("C:/Users/colef/Documents/GitHub/Housing-Web-Scraping")
set.seed(4)

library(tidyverse)
library(forcats)
library(ggsci)
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
houses.sample <- slice_sample(houses.df, prop=0.1)
houses.df <- anti_join(houses.df, houses.sample)
## Creating first model:
houses.glm1 <- glm(formula = Price~Bedrooms+Bathrooms+SquareFeet+PropertyType,
                   data=houses.df)
#' Prints a concise summary of model
#'
#' @param model A linear model
#' @param name String
my_summary <- function(model, name="Unnamed Model"){
  cat("\"",name,"\"\n")
  cat("Formula: ",as.character(model$formula)[c(2,1,3)],"\n \n")
  printCoefmat(summary(model)$coefficients)
  cat(paste("\nAIC:", model$aic))
}

#' Calculates percent error between predictions and actual values
#' 
#' @param preds predicted values
#' @param reals actual values
error <- function(preds, reals){
  return(mean(abs(preds - reals) / reals))
}
pred <- predict(houses.glm1, houses.sample)
error(pred, houses.sample$Price)

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
houses.glm2 <- glm(formula = logPrice~Bedrooms+Bathrooms+SquareFeet+PropertyType,
                   data=houses.df)
my_summary(houses.glm2)

pred <- exp(predict(houses.glm2, houses.sample))
error(pred, houses.sample$Price)

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

# Merging categories
houses.df$PropertyType <- 
  fct_collapse(houses.df$PropertyType, 
               `Tri/Four/Multi` = c("Triplex","Fourplex","Multifamily"))


############
anova_model <- aov(logPrice ~ PropertyType, data = houses.df)
summary(anova_model)

tukey_results <- TukeyHSD(anova_model,conf.level = 0.95)
par(mar=c(3,8,3,3))
plot(tukey_results, las=1, cex.axis = 0.6)


houses.glm3 <- glm(formula = logPrice~Bedrooms+Bathrooms+SquareFeet+PropertyType,
                   data=houses.df)
my_summary(houses.glm3)

# Merging categories
houses.df$PropertyType <- 
  fct_collapse(houses.df$PropertyType, 
               `House/Duplex` = c("House","Duplex"))
houses.glm4 <- glm(formula = logPrice~Bedrooms+Bathrooms+SquareFeet+PropertyType,
                   data=houses.df)
my_summary(houses.glm4)






# removing 'Land/Lot' and 'Recreational' listings
houses.df <- houses.df %>%
  filter(!(PropertyType %in% c("Tri/Four/Multi")))
houses.df$PropertyType <- droplevels(houses.df$PropertyType)

houses.glm5 <- glm(formula = logPrice~Bedrooms+Bathrooms+SquareFeet+PropertyType,
                   data=houses.df)
my_summary(houses.glm5)













mean(abs(houses.glm2$fitted.values - houses.df$logPrice) / houses.df$logPrice)

