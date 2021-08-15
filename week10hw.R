library(tidyverse) 
library(data.table)
library(rstudioapi)
library(skimr)
library(inspectdf)
library(mice)
library(plotly)
#install.packages('highcharter', dependencies = TRUE)
library(highcharter)
#install.packages("recipes", dependencies = TRUE)
library(recipes)
#install.packages("caret")
library(caret) 
library(purrr) 
library(graphics) 
library(Hmisc) 
library(glue) # devtools::install_github("tidyverse/glue")
library(hrbrthemes)

data <- ggplot2::mpg
data %>% view()

#if ("package:h2o" %in% search()) { detach("package:h2o", unload=TRUE) }
#if ("h2o" %in% rownames(installed.packages())) { remove.packages("h2o") }
#pkgs <- c("RCurl","jsonlite")
#for (pkg in pkgs) {
#  if (! (pkg %in% rownames(installed.packages()))) { install.packages(pkg) }}
#install.packages("h2o", type="source", repos="http://h2o-release.s3.amazonaws.com/h2o/rel-yates/5/R")
library(h2o)  




data %>% names()  #adlarda herhansi bir problem yoxdur.Sadece year factor olmalidir.

data$year <- as.character(data$year)

data %>% skim()

# datada her hansi bir null yoxdur doldurmaq üçün

data.num <- data %>%
  select_if(is.numeric) %>% select(cty,everything())

data.char <- data %>%
  select_if(is.character)


p1 <- data %>% 
  ggplot(aes(y=cty, colour='red')) + 
  geom_boxplot(size=1) + #coord_flip() +
  theme_ft_rc()

p1 %>% ggplotly() 

p2 <- data %>% 
  ggplot(aes(y=displ, colour='red')) + 
  geom_boxplot(size=1) + #coord_flip() +
  theme_ft_rc()

p2 %>% ggplotly()     #outliter yoxdur



p3 <- data %>% 
  ggplot(aes(y=hwy, colour='red')) + 
  geom_boxplot(size=1) + #coord_flip() +
  theme_ft_rc()

p3 %>% ggplotly()    # 1 2 dene outliter var



p4 <- data %>% 
  ggplot(aes(y=cyl, colour='red')) + 
  geom_boxplot(size=1) + #coord_flip() +
  theme_ft_rc()

p4 %>% ggplotly()   #outliter yoxdur

# Outliter az olduguna göre heç ne etmirem


# One Hote Encoding ----
data.char <- dummyVars(" ~ .", data = data.char) %>% 
  predict(newdata = data.char) %>% 
  as.data.frame()

data <- cbind(data.char,data.num) %>%
  select(cty,everything())

data %>% skim()

data %>% names()

names(data) <- names(data) %>% 
  str_replace_all(" ","_") %>%
  str_replace_all("-","_") %>%
  str_replace_all("\\(","") %>% 
  str_replace_all("\\)","") %>% 
  str_replace_all("\\'","")

data %>% names()

target <- 'cty'
features <- data %>% select(-cty) %>% names()

features

f <- as.formula(paste(target, paste(features, collapse = " + "), sep = " ~ "))
glm <- glm(f, data = data)

data %>% names()

glm %>% summary()

coef_na <- attributes(alias(glm)$Complete)$dimnames[[1]]
features <- features[!features %in% coef_na]

f <- as.formula(paste(target, paste(features, collapse = " + "), sep = " ~ "))
glm <- glm(f, data = data)

glm %>% summary()

#install.packages('faraway', repos='http://cran.us.r-project.org')

# VIF (Variance Inflation Factor) ----
while(glm %>% faraway::vif() %>% sort(decreasing = T) %>% .[1] >= 1.9){
  afterVIF <- glm %>% faraway::vif() %>% sort(decreasing = T) %>% .[-1] %>% names()
  f <- as.formula(paste(target, paste(afterVIF, collapse = " + "), sep = " ~ "))
  glm <- glm(f, data = data)
}

glm %>% faraway::vif() %>% sort(decreasing = T) %>% names() -> features 

data <- data %>% select(cty,features)


# Standardize (Normalize) ----
data %>% glimpse()

data[,-1] <- data[,-1] %>% scale() %>% as.data.frame()



h2o.init()

h2o_data <- data %>% as.h2o()


# Splitting the data ----
h2o_data <- h2o_data %>% h2o.splitFrame(ratios = 0.73, seed = 123)
train <- h2o_data[[1]]
test <- h2o_data[[2]]

target <- 'cty'
features <- data %>% select(-cty) %>% names()

# Fitting h2o model ----
model <- h2o.glm(
  x = features, y = target,
  training_frame = train,
  validation_frame = test,
  nfolds = 10, seed = 123,
  lambda = 0, compute_p_values = T)

model@model$coefficients_table %>%
  as.data.frame() %>%
  dplyr::select(names,p_value) %>%
  mutate(p_value = round(p_value,3)) %>%
  .[-1,] %>%
  arrange(desc(p_value))

# Significance levels of P_value:
# 0     < p_val < 0.001 ***
# 0.001 < p_val < 0.05  **
# 0.05  < p_val < 0.01  *
# 0.01  < p_val < 0.1   .


# Stepwise Backward Elimination ----
while(model@model$coefficients_table %>%
      as.data.frame() %>%
      dplyr::select(names,p_value) %>%
      mutate(p_value = round(p_value,3)) %>%
      .[-1,] %>%
      arrange(desc(p_value)) %>%
      .[1,2] > 0.05) {
  model@model$coefficients_table %>%
    as.data.frame() %>%
    dplyr::select(names,p_value) %>%
    mutate(p_value = round(p_value,3)) %>%
    filter(!is.nan(p_value)) %>%
    .[-1,] %>%
    arrange(desc(p_value)) %>%
    .[1,1] -> v
  features <- features[features!=v]
  
  train_h2o <- train %>% as.data.frame() %>% select(target,features) %>% as.h2o()
  test_h2o <- test %>% as.data.frame() %>% select(target,features) %>% as.h2o()
  
  model <- h2o.glm(
    x = features, y = target,
    training_frame = train,
    validation_frame = test,
    nfolds = 10, seed = 123,
    lambda = 0, compute_p_values = T)
}

warnings()

model@model$coefficients_table %>%
  as.data.frame() %>%
  dplyr::select(names,p_value) %>%
  mutate(p_value = round(p_value,3))

y_pred <- model %>% h2o.predict(newdata = test) %>% as.data.frame()
y_pred$predict




test_set <- test %>% as.data.frame()
residuals = test_set$cty - y_pred$predict

# Calculate RMSE (Root Mean Square Error) ----
RMSE = sqrt(mean(residuals^2))

# Calculate Adjusted R2 (R Squared) ----
y_test_mean = mean(test_set$cty)

tss = sum((test_set$cty - y_test_mean)^2) #total sum of squares
rss = sum(residuals^2) #residual sum of squares

R2 = 1 - (rss/tss); R2

n <- test_set %>% nrow() #sample size
k <- features %>% length() #number of independent variables
Adjusted_R2 = 1-(1-R2)*((n-1)/(n-k-1))

tibble(RMSE = round(RMSE,1),
       R2, Adjusted_R2)





# Plotting actual & predicted ----
my_data <- cbind(predicted = y_pred$predict,
                 observed = test_set$cty) %>% 
  as.data.frame()

g <- my_data %>% 
  ggplot(aes(predicted, observed)) + 
  geom_point(color = "darkred") + 
  geom_smooth(method=lm) + 
  labs(x="Predecited Power Output", 
       y="Observed Power Output",
       title=glue('Test: Adjusted R2 = {round(enexpr(Adjusted_R2),2)}')) +
  theme(plot.title = element_text(color="darkgreen",size=16,hjust=0.5),
        axis.text.y = element_text(size=12), 
        axis.text.x = element_text(size=12),
        axis.title.x = element_text(size=14), 
        axis.title.y = element_text(size=14))

g %>% ggplotly()


# Check overfitting ----
y_pred_train <- model %>% h2o.predict(newdata = train) %>% as.data.frame()

train_set <- train %>% as.data.frame()
residuals = train_set$cty - y_pred_train$predict

RMSE_train = sqrt(mean(residuals^2))
y_train_mean = mean(train_set$cty)

tss = sum((train_set$cty - y_train_mean)^2)
rss = sum(residuals^2)

R2_train = 1 - (rss/tss); R2_train

n <- train_set %>% nrow() #sample size
k <- features %>% length() #number of independent variables
Adjusted_R2_train = 1-(1-R2_train)*((n-1)/(n-k-1))


# Plotting actual & predicted
my_data_train <- cbind(predicted = y_pred_train$predict,
                       observed = train_set$cty) %>% 
  as.data.frame()

g_train <- my_data_train %>% 
  ggplot(aes(predicted, observed)) + 
  geom_point(color = "darkred") + 
  geom_smooth(method=lm) + 
  labs(x="Predecited Power Output", 
       y="Observed Power Output",
       title=glue('Train: Adjusted R2 = {round(enexpr(Adjusted_R2_train),2)}')) +
  theme(plot.title = element_text(color="darkgreen",size=16,hjust=0.5),
        axis.text.y = element_text(size=12), 
        axis.text.x = element_text(size=12),
        axis.title.x = element_text(size=14), 
        axis.title.y = element_text(size=14))

g_train %>% ggplotly()


# Compare 
library(patchwork)
g_train + g

tibble(RMSE_train = round(RMSE_train,1),
       RMSE_test = round(RMSE,1),
       
       Adjusted_R2_train,
       Adjusted_R2_test = Adjusted_R2)



