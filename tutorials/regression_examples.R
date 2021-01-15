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

lm_1 <- lm()
lm_2 <- lm()
lm_3 <- lm()

##-----##
# Adding dummy variables for categorical data
##-----##

add breathalizer 

#notice that has p levels, p-1 coefficients are added to the regression
# the p-th case (called the base case) is "rolled into" the intercept coefficient

clean_df <-
  clean_df %>%
  mutate(sex = relevel(as.factor(sex), "Male"),
         race = relevel(as.factor(race), "White"),
         marst = relevel(as.factor(marst), "Single"))


##-----##
# Logistic regression
##-----##

data(SmokeBan)
str(SmokeBan)

logit_1 <- glm(smoker ~ ban + age + education + afam, 
                  data = SmokeBan, family = 'binomial')

stargazer(logit_1, type = "text",
          title = "Base Specification",
          omit.stat = c("ser", "f"))

##-----##
# Fixed effects
##-----##

data(Fatalities)
pdata_Fatalities <- pdata.frame(x = Fatalities, index = c('state','year'))

fe_1 <- plm(fatal ~ spirits,
            data = pdata_Fatalities, model = 'within', effect = 'twoways')
fe_2 <- plm(fatal ~ spirits,
            data = pdata_Fatalities, model = 'within', effect = 'twoways')
fe_3 <- plm(fatal ~ spirits,
            data = pdata_Fatalities, model = 'within', effect = 'twoways')
fe_4 <- plm(fatal ~ spirits,
            data = pdata_Fatalities, model = 'within', effect = 'twoways')

stargazer(fe_1, fe_2, fe_3, fe_4, type = "text",
          title = "Fixed effects",
          omit.stat = c("ser", "f"))

##-----##
# Differences-in-differences
##-----##



data("PSID1982")

df <-
  PSID1982 %>%
  mutate(any_college = if_else(education > 12, 1, 0))

PSID1982 %>%
  ggplot(aes(x = education, y = wage, color = occupation)) +
  geom_jitter(alpha = .3) +
  facet_wrap(~gender) +
  labs(title = "Hmm")

mod1 <- lm(wage ~ education, data = df)
mod2 <- lm(wage ~ education + any_college, data = df)
mod3 <- lm(wage ~ education * any_college, data = df)
mod4 <- lm(wage ~ education * any_college + occupation, data = df)

stargazer(mod1,mod2,mod3,mod4, type = "text",
          title = "Trying to Model Prior Figure",
          omit.stat = c("ser", "f"))

dd1 <- lm(wage ~ any_college, data = df)
dd2 <- lm(wage ~ occupation, data = df)
dd3 <- lm(wage ~ any_college + occupation, data = df)
dd4 <- lm(wage ~ any_college * occupation, data = df)

stargazer(dd1,dd2,dd3,dd4, type = "text",
          title = "Trying to Model Different Part of Prior Figure",
          omit.stat = c("ser", "f"))

##-----##
# Instrumental variables
##-----##

#...
