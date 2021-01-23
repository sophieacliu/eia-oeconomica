##-----##
##-----##
# Replication script for Wollmann (2019)
##-----##
##-----##

# This is a "translation" into R of the Stata replication code available online 
# for Figures 1, 2, 3 and Table 1 in main paper (no appendix material or Table 2 in this script)

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
ddir <- file.path(ddir, 'paper replications/wollmann (2019)/replication files')

# Load necessary packages
if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)
library(readxl)
if (!require(reshape2)) install.packages('reshape2')
library(reshape2)
if (!require(ggthemes)) install.packages('ggthemes')
library(ggthemes)
if (!require(gridExtra)) install.packages('gridExtra')
library(gridExtra)
if (!require(estimatr)) install.packages('estimatr')
library(estimatr)
if (!require(texreg)) install.packages('texreg')
library(texreg)

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
  mutate(norm_diff_4sic = diff_4sic, norm_same_4sic = same_4sic,
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
rm(sic_codes_sheets)
rm(sic_codes)
rm(notif_and_invest_and_block_df)

##-----##
# Create figure1
##-----##

# Figure 1. Notifications Drop Sharply when the Amendment Takes Effect
# Notes: This graph plots the number of transactions for which premerger notifications were filed in the United States 
# over time. Filings are required pursuant to the Hart-Scott-Rodino Antitrust Improvements Act and reviewed by the 
# Department of Justice and Federal Trade Commission. A vertical line marks 2001, the year the Act was amended to 
# exempt deals valued at less than $50 million.

figure1 <- ggplot(merged_data_df,aes(x = year_e, y = trans_hsr_a_and_b)) +
  geom_vline(xintercept = 2001, color = 'red', size = 0.5) +
  geom_point(color = '#014d64', size = 2) + 
  expand_limits(x = c(1993,2011), y = 4200) +
  xlab('Year') +
  ylab('Count') +
  theme(axis.line = element_line(color = "black"), panel.background = element_blank(),
        axis.text.y = element_text(angle = 90, hjust = 0.5, size = 10, color = 'black'),
        axis.text.x = element_text(size = 10, color = 'black'),
        axis.title = element_text(size = 10))

ggsave(file.path(root, 'tutorials/paper replications/wollmann (2019)', 'figure1.pdf'), 
       plot = figure1, width = 8/1.5, height = 6/1.5)

##-----##
# Create figure2 — TRY IT YOURSELF!!
##-----##

# Figure 2. Nearly all of the Decline in Notifications and Investigations Occurs among Newly-Exempt Mergers
# Notes: Panels A and B plot notifications and mergers over time. Panels C and D plot investigations and mergers 
# over time, with the primary and secondary y-axes counting the former and latter, respectively. Panels A and C are
# based on never-exempt mergers, while panels B and D are based on newly-exempt mergers. In each, a vertical line 
# marks 2001, the year the Act was amended to raise the size-of-transactions threshold. Note that the vertical distance 
# between the dashed lines in panel D represents non-HSR-related investigations (of which there are few).

# Hints: 
# Colors to use: 'darkred', '#014d64' (blue), 'black', 'red', '#506b2f' (green)
# Use grid.arrange() from the gridExtra package with ncol = 2

##-----##
# Create figure3 — TRY IT YOURSELF!! 
##-----##

# Figure 3. Newly-Exempt Horizontal Mergers Increase following the Amendment
# Notes: These graphs plot the log of the number of horizontal and non-horizontal mergers over time. Panel A is based 
# on never-exempt mergers, while panel B is based on newly-exempt mergers. In both, a vertical line marks 2001,
# the year the Act was amended to raise the size-of-transactions threshold. To facilitate comparisons, all plotted val- 
# ues are reduced by the value they take in that year (so that the lines that connect them intersect y = 0 in 2001).

##-----##
# Table 1 ("diffs") — TRY TO FINISH THE TABLE YOURSELF!! (i.e., reg4, reg5, reg6)
##-----##

table1_df <- merged_data_df %>%
  select(c('ex_grp', 'year_e', 'same_4sic', 'diff_4sic')) %>%
  rename(levels_1 = same_4sic, levels_2 = diff_4sic)

# Format for reg
table1_df <- table1_df %>%
  mutate(logs_1 = log(levels_1), logs_2 = log(levels_2))

levels_interim_df <- table1_df %>%
  select(c('ex_grp', 'year_e', 'levels_1', 'levels_2')) %>%
  melt(id.vars=c('ex_grp', 'year_e'),
       measure.vars=c('levels_1', 'levels_2'),
       variable.name = 'num',
       value.name = 'levels') %>%
  mutate(num = ifelse(num == 'levels_1', 1, 2))
logs_interim_df <- table1_df %>%
  select(c('ex_grp', 'year_e', 'logs_1', 'logs_2')) %>%
  melt(id.vars=c('ex_grp', 'year_e'),
       measure.vars=c('logs_1', 'logs_2'),
       variable.name = 'num',
       value.name = 'logs') %>%
  mutate(num = ifelse(num == 'logs_1', 1, 2))

table1_df <- merge(levels_interim_df, logs_interim_df, by = c('ex_grp', 'year_e', 'num')) %>%
  mutate(post = ifelse(year_e > 2001, 1, 0), below = ifelse(ex_grp == 1, 1, 0), hor = ifelse(num == 1, 1, 0), 
         iii = post*hor*below, ii1 = post*hor, ii2 = post*hor)

rm(levels_interim_df)
rm(logs_interim_df)

# in logs
reg1 <- lm_robust(logs ~ ii2 + factor(year_e) + as.numeric(hor), 
                  data = filter(table1_df, below == 1), se_type = "stata")

reg2 <- lm_robust(logs ~ iii + as.numeric(below) + factor(year_e) + as.numeric(hor) + factor(year_e):as.numeric(below) +
                    factor(year_e):as.numeric(hor) + as.numeric(below):as.numeric(hor),
                  data = table1_df, se_type = "stata")

reg3 <- lm_robust(logs ~ ii1 + factor(year_e) + as.numeric(hor), 
                  data = filter(table1_df, below == 0), se_type = "stata")

# in levels
# repeat the above for reg4, reg5, reg6 but with `levels` as the response variable

table1 <- texreg(list(reg1, reg2, reg3), include.ci = FALSE, stars = numeric(0), 
                 custom.coef.map = list('ii2'= " $ I_i^H \\cdot I_s^{Exempted} \\cdot I_t^{Post} $ ",
                                        'iii' = " $ I_i^H \\cdot I_t^{Post} $ \\textit{(Never--exempt)} \\quad \\quad \\quad \\quad",
                                        'ii1' = " $ I_i^H \\cdot I_t^{Post} $ \\textit{(Newly--exempt)}"))

write.table(table1, file.path(root, 'tutorials/paper replications/wollmann (2019)', 'table1.tex'), col.names = FALSE, row.names = FALSE, quote = FALSE)
