##-----##
# Some basic examples of data cleaning and visualization with dplyr and ggplot2
##-----##

# This snippet of code is a little loop that makes my code work on your computer
# It creates an object called `root` that is the filepath to your projects directory
root <- getwd()
while(basename(root) != "eia-oeconomica") { # this is the name of your project directory you want to use
  root <- dirname(root)
}

# This line runs the script in your data.R file so that each person can have
# their data in a different place because everyone's file structure will be 
# a little different
# It creates a data directory object called `ddir` as specified in your data.R file
source(file.path(root, "data.R"))

# This is the specific folder we want to access data from
ddir <- file.path(ddir, 'dplyr_ggplot2_examples')

# Load necessary packages
if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)
if (!require(scales)) install.packages('scales')
library(scales)
if (!require(reshape2)) install.packages('reshape2')
library(reshape2)

# We will be looking at a healthcare panel dataset of Census data
# See the data folder for a full data dictionary
df_full <- read_csv(file.path(ddir,'All_years.csv'))

# Each row corresponds to an individual x year
# perwt := the number of individuals with the same characteristics in the population
# that the individual in the sample represents
str(df_full)
View(df_full)

# We can add new variables dependent on the values of existing variables
# For example, hcovany == 2 => the individual has some form of healthcare
# hinscare == 1 => the individual does not have Medicare
# hinscaid == 1 => the individual does not have Medicaid
# So, if all three of these conditions are satisfied, we can create a new dummy variable 
# healthcare_other that indicates whether the individual has healthcare other than 
# Medicare or Medicaid
# We also want subtract 1 from hcovany, hinscare, and hinscaid so that they are all equal
# to 1 when their condition is true
df_full_plus <- df_full %>%
  mutate(healthcare_other = ifelse(hcovany==2 & hinscare==1 & hinscaid==1, 1, 0),
         hcovany = hcovany - 1, 
         hinscare = hinscare - 1,
         hinscaid = hinscaid - 1)
View(df_full_plus)

# Using the group_by() and summarise() functions together allow us to create new dataframes
# that are similar to Pivot Tables in Excel, i.e., we can observe some variable, say the average age
# by each year
# We want to get a weighted average by using the perwt to account for the sampling design
df_by_year_1 <- df_full_plus %>% 
  group_by(year) %>%
  summarise(wtavg_age = sum(age * perwt) / sum(perwt), 
            wtcount_any = sum(hcovany * perwt),
            wtcount_care = sum(hinscare * perwt),
            wtcount_caid = sum(hinscaid * perwt),
            wtcount_other = sum(healthcare_other * perwt),
            wtn = sum(perwt))

ggplot(df_by_year_1, aes(x=year,y=wtn/1000000)) +
  geom_line() +
  xlab('Year') +
  ylab('Count (millions of individuals)') +
  scale_x_continuous(breaks= pretty_breaks()) +
  ggtitle('Weighted sample size')

ggplot(df_by_year_1, aes(x=year,y=wtavg_age)) +
  geom_line() +
  xlab('Year') +
  ylab('Age') +
  scale_x_continuous(breaks= pretty_breaks()) +
  ggtitle('Average age')

ggplot(df_by_year_1, aes(x=year,y=wtcount_any/1000000)) +
  geom_line() +
  xlab('Year') +
  ylab('Count with any coverage (millions of individuals)') +
  scale_x_continuous(breaks= pretty_breaks()) +
  ggtitle('Individuals with healthcare')

# We need to reformat the data to plot data over time using a bar plot
# We will first select only the variables we will need and then "melt" the dataframe
df_by_year_1_melted <- df_by_year_1 %>%
  select(c(year, wtcount_other, wtcount_care, wtcount_caid)) %>%
  melt(id.vars='year')
View(df_by_year_1_melted)

ggplot(df_by_year_1_melted, aes(x=year, y=value/1000000, fill=variable)) +
  geom_col(position='dodge') +
  xlab('Year') +
  ylab('Count') +
  ggtitle('Healthcare type breakdown') +
  scale_x_continuous(breaks= pretty_breaks()) +
  theme(axis.line = element_line(color = "black"), 
        panel.background = element_blank(),
        legend.position = 'bottom')

rm(df_by_year_1_melted)

# This is helpful, but it might actually be more useful to visualize how the *share* of
# the sample with a given type of health coverage changes over time
df_by_year_2 <- df_by_year_1 %>%
  mutate(wtshare_any = wtcount_any / wtn,
         wtshareany_care = wtcount_care / wtcount_any,
         wtshareany_caid = wtcount_caid / wtcount_any,
         wtshareany_other = wtcount_other / wtcount_any)

ggplot(df_by_year_2, aes(year, wtshare_any)) +
  geom_line() +
  xlab('Year') +
  ylab('Share of weighted sample') +
  ggtitle('Percentage with any healthcare coverage') +
  scale_x_continuous(breaks= pretty_breaks())

ggplot(data = df_by_year_2) + 
  geom_line(aes(x=year,y=wtshareany_other, color = 'red')) +
  geom_line(aes(x=year,y=wtshareany_care, color = 'light blue')) +
  geom_line(aes(x=year,y=wtshareany_caid, color = 'dark blue')) +
  scale_x_continuous(breaks= pretty_breaks()) +
  xlab('Year') +
  ylab('Fraction of share with any insurance') +
  scale_color_manual(name = "Insurance type:", labels = c('Medicaid','Medicare','Other'), 
                     values = c('dark blue','light blue','red')) +
  ggtitle('Healthcare type breakdown') +
  theme_bw() +
  theme(legend.position = 'bottom') 

rm(list = c('df_by_year_1', 'df_by_year_2'))

# We should also briefly touch on the filter() function before wrapping up
# Suppose we just want to look at data from California in 2012
# Then we filter down to the statefip code 06 
df_ca_2012 <- df_full_plus %>%
  filter(statefip == 06, year == 2012)

# Now let's calculate a weighted average of age in California in 2012
wtavg_age_ca_2012 <- sum(df_ca_2012$perwt * df_ca_2012$age) / sum(df_ca_2012$perwt)
wtavg_age_ca_2012
