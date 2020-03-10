# Import the libraries ----
library(tidyverse)
library(caTools)
library(car)
library(e1071)
library(glue)
library(highcharter)
library(plotly)

# Import the dataset ----
trial <- ggplot2::mpg
trial %>% glimpse()

# names(trial) <- gsub(names(trial), replacement = '_',pattern = ' ')   BU YEQIN KI BIZE LAZIM DEYIL



# Data preparation ----

# Outliers
num_trial <- trial %>% 
  select(-trial$cty) %>% 
  select_if(is.numeric) %>%
  names()                        ############   (bu vacibdi ki? burda men names yaratmadigim ucun bura atmiram) 
num_trial

for_trial <- c()
for (b in 2:length(num_trial)) {
  Outtrial <- boxplot(trial[[num_trial[b]]])$out           ############out nedi burda?
  if(length(Outtrial)>0){
    for_trial[b] <- num_trial[b]
  }
}
for_trial <- for_trial %>% as.data.frame() %>% drop_na() %>% pull(.) %>% as.character()
for_trial %>% length()

for (o in for_trial) {
Outtrial <- boxplot(trial[[o]])$out
trialmean <- mean(trial[[o]],na.rm=T)

o3 <- ifelse(Outtrial>trialmean,Outtrial,NA) %>% na.omit() %>% as.matrix() %>% t() %>% .[1,]
o1 <- ifelse(Outtrial<trialmean,Outtrial,NA) %>% na.omit() %>% as.matrix() %>% t() %>% .[1,]

val3 <- quantile(trial[[o]],0.75,na.rm = T) + 1.5*IQR(trial[[o]],na.rm = T)
trial[which(trial[[o]] %in% o3),o] <- val3

val1 <- quantile(trial[[o]],0.25,na.rm = T) - 1.5*IQR(trial[[o]],na.rm = T)
trial[which(trial[[o]] %in% o1),o] <- val1
}

# Check normality 
num_trial <- trial %>% 
  select_if(is.numeric) %>% 
  names()
num_trial

par(mfrow=c(2, 2))  # divide graph area in 2columns & 2rows (number of variables)

for (p in 1:length(num_trial)) {
  var.name = num_trial[p]
  plot(density(trial[[num_trial[p]]]),
       main=glue('{enexpr(var.name)}'), 
       ylab="Frequency", 
       sub=paste("Skewness:", round(e1071::skewness(raw[[num_trial[p]]]), 2)))  
  polygon(density(trial[[num_trial[p]]]), col="red")
}

# Correlation
hchart(cor(trial %>% 
             mutate_if(is.character,as.factor) %>% 
             mutate_if(is.factor,as.numeric)) %>% 
         round(.,2),label = T)

# # Transforming Data
# raw <- cbind(scale(raw[-4]),raw[4]) %>% as.data.frame()


# Linear Regression Diagnostics ----
glm <- glm(cty ~ year + cyl + displ, data = trial)

glm %>% summary()

glm %>% plot()

# Multicollinrarity
vif <- glm %>% vif() %>% as.data.frame()  
vif$Variable <- vif %>% rownames()
vif <- vif %>% rename (GVIF = ".")
vif %>% 
  select(Variable,GVIF) %>% 
  arrange(desc(GVIF)) 

# df <- trial %>% select(-Marketing_Spend)

# Splitting the df into the Train set and Test set
set.seed(123)
split <- trial$cty %>% sample.split(SplitRatio = 0.8)
train <- trial %>% subset(split == TRUE)
test <- trial %>% subset(split == FALSE)


# GLM ----        #Coefficients table
glm <- glm(cty ~ hwy + model + year, data = trial)
glm %>% summary()


# Select a significant variables with Stepwise Algorithm
step <- glm %>% step()
step$call # copy past

# glm2 <- glm(Profit ~ `R&D_Spend`, data = train)
# glm2 %>% summary()

# Predicting the Test set results
y_pred <- glm %>% predict(test %>% select(hwy,model,year))


# Model evaluation ----
residuals = test$cty - y_pred

#Calculate Root Mean Square Error
RMSE = sqrt(mean(residuals^2))

y_test_mean = mean(test$cty)

#Calculate total sum of squares
tss =  sum((test$cty - y_test_mean)^2)

#Calculate residual sum of squares
rss =  sum(residuals^2)

#Calculate R-squared
R2  =  1 - (rss/tss)

#Calculate Adjusted R-squared
n <- 234 #sample size
k <- 3 #number of independent variables
Adjusted_R2 = 1-(1-R2)*((n-1)/(n-k-1))
Adjusted_R2 <- paste0(round(Adjusted_R2*100, 1),"%")

tibble(RMSE = round(RMSE,2),
       Adjusted_R2)

# 
# RMSE Adjusted_R2
# <dbl> <chr>      
#   1  0.74 96.6%      



# Plotting actual & predicted ----
my_data <- as.data.frame(cbind(predicted = y_pred,
                               observed = test$cty))


# Plot predictions & test data
g <- my_data %>% ggplot(aes(predicted, observed)) + 
  geom_point(color = "darkred") + 
  geom_smooth(method=lm) + 
  ggtitle(glue('Multiple Linear Regression --> Adjusted R2 = {enexpr(Adjusted_R2)}')) +
  xlab("Predecited Power Output ") + 
  ylab("Observed Power Output") + 
  theme(plot.title = element_text(color="darkgreen",size=16,hjust=0.5),
        axis.text.y = element_text(size=12), 
        axis.text.x = element_text(size=12),
        axis.title.x = element_text(size=14), 
        axis.title.y = element_text(size=14))

g %>% ggplotly()
