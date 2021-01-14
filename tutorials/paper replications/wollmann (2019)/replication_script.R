##-----##
##-----##
# Replication script for Wollmann (2019)
##-----##
##-----##

# This is a "translation" into R of the Stata replication code available online

# This snippet of code is a little loop that makes my code work on your computer
# It creates an object called `root` that is the filepath to your projects directory
root <- getwd()
while(basename(root) != "eia-oeconomica") { # this is the name of your project directory you want to use
  root <- dirname(root)
}

# This line runs the script in your data.R file so that each person can have
# their data in a different place because everyone's file structure will be 
# a little different
# It creates a data directory object called `ddir`
source(file.path(root, "data.R"))

# This is the specific folder we want to access data from
ddir <- file.path(ddir, 'paper replications/wollmann (2019)/replication files')

# Load necessary packages
if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)
library(dplyr)
library(readxl)

# Make sure that you do not have any packages attached such that functions are masked from the 
# packages loaded above
# In particular, having the `plyr` package loaded may cause issues when using `dplyr`
# To detach it, run the following line
#detach(package:plyr)

##-----##
# Import data
##-----##

# Import SIC code translation
# This links four-digit SIC codes to the names of the industries to which they refer. 
sic_codes_sheets <- excel_sheets(file.path(ddir,'sic_codes.xlsx'))
sic_codes <- lapply(sic_codes_sheets, function(x) read_excel(file.path(ddir,'sic_codes.xlsx'),sheet=x,col_names=c('sic','sic_des')))
sic_codes_df <- bind_rows(sic_codes) %>%
  slice(-1) %>%
  mutate(sic_des = tolower(sic_des))
sic_codes_df = sic_codes_df[!duplicated(sic_codes_df$sic),]

# Import data related to notification/enforcement data
# This comprises premerger notifications, investigations, and blocked mergers by year. 
notif_and_invest_and_block_df <- read_excel(file.path(ddir,'notif_and_invest_and_block.xlsx'))

# Import (year,size,SIC type) merger data 
# This provides the number of mergers by year, by whether it involves a target and acquirer in the 
# same four-digit SIC code, by whether the merger occurred in the "pre-packaged software" industry,
# and by whether the adjusted value of the merger exceed $150MM. 
# The latter two are required for tables reported in the Appendix. 
mergers_by_year_size_sic_type_df <- read_excel(file.path(ddir,'mergers_by_year_size_sic_type.xls'))

# Import and save (stealth type,SIC) merger data 
mergers_by_stealth_type_sic_df <- read_excel(file.path(ddir,'mergers_by_stealth_type_sic.xls'))

##-----##
# Merge and format data
##-----##
mergers_by_year_size_sic_type_df <- mergers_by_year_size_sic_type_df %>%
  group_by(year_e, ex_grp) %>%
  summarise(diff_4sic = sum(diff_4sic), same_4sic = sum(same_4sic), 
         diff_4sic_dollars = sum(diff_4sic_dollars), same_4sic_dollars = sum(same_4sic_dollars))

# Log variables that are in levels. Also, reduce by value the resulting 
# variables take in 2001, which is the year of the Amendment. 
# This is needed for Figure III.
mergers_by_year_size_sic_type_df <- mergers_by_year_size_sic_type_df %>%
  mutate(log_diff_4sic = log(diff_4sic), log_same_4sic = log(same_4sic),
         log_diff_4sic_dollars = log(diff_4sic_dollars), log_same_4sic_dollars = log(same_4sic_dollars)) %>%
  rename(norm_diff_4sic = diff_4sic, norm_same_4sic = same_4sic,
         norm_diff_4sic_dollars = diff_4sic_dollars, norm_same_4sic_dollars = same_4sic_dollars) 

mergers_by_year_size_sic_type_df <- mergers_by_year_size_sic_type_df %>%
  mutate(log_diff_4sic = log_diff_4sic - filter(mergers_by_year_size_sic_type_df, year_e == 2001,ex_grp==ex_grp)$log_diff_4sic, 
         log_same_4sic = log_same_4sic - filter(mergers_by_year_size_sic_type_df, year_e == 2001,ex_grp==ex_grp)$log_same_4sic,
         log_diff_4sic_dollars = log_diff_4sic_dollars - filter(mergers_by_year_size_sic_type_df, year_e == 2001,ex_grp==ex_grp)$log_diff_4sic_dollars, 
         log_same_4sic_dollars = log_same_4sic_dollars - filter(mergers_by_year_size_sic_type_df, year_e == 2001,ex_grp==ex_grp)$log_same_4sic_dollars, 
         norm_diff_4sic = norm_diff_4sic - filter(mergers_by_year_size_sic_type_df, year_e == 2001,ex_grp==ex_grp)$norm_diff_4sic, 
         norm_same_4sic = norm_same_4sic - filter(mergers_by_year_size_sic_type_df, year_e == 2001,ex_grp==ex_grp)$norm_same_4sic,
         norm_diff_4sic_dollars = norm_diff_4sic_dollars - filter(mergers_by_year_size_sic_type_df, year_e == 2001,ex_grp==ex_grp)$norm_diff_4sic_dollars, 
         norm_same_4sic_dollars = norm_same_4sic_dollars - filter(mergers_by_year_size_sic_type_df, year_e == 2001,ex_grp==ex_grp)$norm_same_4sic_dollars)

# Merge into merger data the notifications, investigations, and blocked transactions
merged_data_df <- mergers_by_year_size_sic_type_df %>%
  merge(notif_and_invest_and_block_df, by = 'year_e')

# Rename, format, and save variables
merged_data_df <- merged_data_df %>%
  rename(trans_hsr_a_and_b = trans_hsr) %>%
  mutate(trans_hsr = ifelse(ex_grp == 1, trans_hsr_below, trans_hsr_above)) %>%
  select(-c('trans_hsr_below','trans_hsr_above')) %>%
  mutate(all_4sic = diff_4sic + same_4sic) %>%
  ungroup()

# Eliminate interim files that will not be used again
rm(sic_codes)
rm(sic_codes_sheets)
rm(notif_and_invest_and_block_df)
