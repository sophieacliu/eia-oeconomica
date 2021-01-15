##-----##
# Sample code for some common regression methods in economics
##-----##

# Load the following packages (you may have to install these and some dependencies)
library(AER) # loads data
library(plm) # working with panel data (and fixed effects)
library(stargazer) # makes regression tables pretty

##-----##
# Simple linear regression and multiple regression
##-----##

data(Fatalities)
str(Fatalities)
?Fatalities

# This regression reveals the relationship between two continuous variables
lm_1 <- lm(fatal ~ pop, data = Fatalities)

# We can add in more predictor variables to control for additional factors
lm_2 <- lm(fatal ~ pop + miles + dry + income, data = Fatalities)

# We can also change the response variable to another continuous numeric variable
lm_3 <- lm(afatal ~ pop + miles + beertax + income, data = Fatalities)

# If we think two predictor variables var1 and var2 are related, 
# add an interaction term var1*var2
lm_4 <- lm(afatal ~ pop + miles + beertax*income, data = Fatalities)
lm_5 <- lm(afatal ~ pop + miles + dry + beertax*income + spirits, data = Fatalities)

stargazer(lm_1, lm_2, lm_3, lm_4, lm_5, type = "text",
          title = "Simple linear regression and multiple regression",
          omit.stat = c("ser", "f"))

##-----##
# Adding dummy variables for categorical data
##-----##

# We can also add categorical variables (variables of type "factor") as predictors
# Dummy variables are indicator variables that take on a value of 
# 1 if a condition is TRUE and 0 if a condition is FALSE 

# Note how a factor that has p levels results in p-1 coefficients added to the regression
# the p-th case (the "base case") is, conceptually, "rolled into" the intercept coefficient
# For a dummy variable this means just one additional coefficient is added to the regression

lm_6 <- lm(afatal ~ breath, data = Fatalities)
lm_7 <- lm(afatal ~ jail + beertax*income, data = Fatalities)
lm_8 <- lm(afatal ~ pop + breath + jail + miles + beertax*income, data = Fatalities)

stargazer(lm_6, lm_7, lm_8, type = "text",
          title = "Adding dummy variables for categorical data",
          omit.stat = c("ser", "f"))

##-----##
# Logistic regression
##-----##

data(SmokeBan)
str(SmokeBan)
?SmokeBan

# This regression lets us examine how the probability someone is a smoker depends on
# a number of other variables

# Note how for multilevel categorical variables, dummy variables are created for each level
# with p-1 dummy variables added for p levels

logit_1 <- glm(smoker ~ ban + age + education + afam, 
                  data = SmokeBan, family = 'binomial')

stargazer(logit_1, type = "text",
          title = "Logistic regression, 1",
          omit.stat = c("ser", "f"))

# We can also create a binary output variable out of either a continuous numeric variable
# or a multileveled factor variable by establishing a cutoff
# For example, we can say that if a person engaged in extramarital sexual intercourse 
# at least once in the past year that they had an affair
# And now we can use logistic regression to examine relationship between predictors and 
# a new response, had_affair

# Note how we can change the order of levels of categorical variables with relevel()

data(Affairs)
str(Affairs)
?Affairs

affairs_clean <- Affairs %>%
  mutate(had_affair = factor(as.factor(ifelse(affairs > 0, 1, 0)), 
                                     labels = c('no','yes')),
         children = relevel(children, 'no'),
         gender = relevel(gender, 'female'),
         college_graduate = relevel(factor(as.factor(ifelse(education > 14, 1, 0)), 
                                           labels = c('no','yes')), 'no'))

logit_4 <- glm(had_affair ~ children + gender, 
               data = affairs_clean, family = 'binomial')
logit_5 <- glm(had_affair ~ children + gender + college_graduate + rating, 
               data = affairs_clean, family = 'binomial')
logit_6 <- glm(had_affair ~  gender + college_graduate + yearsmarried*children + rating, 
               data = affairs_clean, family = 'binomial')

stargazer(logit_4, logit_5, logit_6, type = "text",
          title = "Logistic regression, 2",
          omit.stat = c("ser", "f"))

##-----##
# Fixed effects
##-----##

?Fatalities

# When we have panel data (each observation/row is, for example, a time x place), we can control for
# general trends that happen over time and in a particular place by using fixed effects
# First we define our data as panel data and then run a fixed effects regression 

pdata_Fatalities <- pdata.frame(x = Fatalities, index = c('state','year'))

fe_1 <- plm(afatal ~ spirits + beertax,
            data = pdata_Fatalities, model = 'within', effect = 'twoways')
fe_2 <- plm(fatal ~ spirits + dry + beertax,
            data = pdata_Fatalities, model = 'within', effect = 'twoways')
fe_3 <- plm(fatal ~ spirits + jail + service,
            data = pdata_Fatalities, model = 'within', effect = 'twoways')
fe_4 <- plm(fatal ~ miles + income + emppop,
            data = pdata_Fatalities, model = 'within', effect = 'twoways')

stargazer(fe_1, fe_2, fe_3, fe_4, type = "text",
          title = "Fixed effects",
          omit.stat = c("ser", "f"))

##-----##
# Differences-in-differences
##-----##

# When we want to mimic an experimental design with a control and treatment group then we may want to
# use a diff-in-diff approach
# What we do is add an interaction term that contains a dummy variable that is TRUE for the treatment
# interacting with another dummy variable that is TRUE for treatment group

##-----##
# Instrumental variables
##-----##

#https://www.econometrics-with-r.org/12-ivr.html 
#...
