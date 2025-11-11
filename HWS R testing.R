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

houses.glm1 <- glm(formula = Price~Bedrooms+Bathrooms+`Square Feet`+`Property Type`+`Province`,
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

houses.glm2 <- glm(formula = Price~Bedrooms+Bathrooms+`Square Feet`+`Property Type`+`Province`, 
                  data=houses.df, family = Gamma(link = "log"))
summary(houses.glm2)

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

