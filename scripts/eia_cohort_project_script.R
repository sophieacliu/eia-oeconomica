# Oeconomica's Entrepreneurship, Innovation & Antitrust Cohort, 2020–2021

# Everything you need to run this code (data and instructions) is here:
# https://github.com/gelkouh/eia-oeconomica

# This snippet of code is a little loop that makes my code work on your computer
root <- getwd()
while(basename(root) != "eia-oeconomica") { # this is the name of your project directory you want to use
  root <- dirname(root)
}

# This line runs the script in your data.R file so that each person can have
# their data in a different place because everyone's file structure will be 
# a little different
source(file.path(root, "data.R"))

# Loading the packages we want
if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)