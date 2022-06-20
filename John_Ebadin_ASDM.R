#STUDY NUMBER 8802 - FAMILY RESOURCES SURVEY, 2019-2020
#Household EUL Data File (household)
#279 variables and 19210 rows

library(tidyverse)
library(skimr)
library(ggplot2)
library(mice)

frs_hh_raw <- read_tsv("D:/ASDM_Coursework/Datasets/househol.tab")
dim(frs_hh_raw)
head(frs_hh_raw)
skimr::skim(frs_hh_raw)

#Selecting the required dataframe from 'frs_hh_raw' as 'frs_hh_clean'
colnames(frs_hh_raw)
frs_hh_clean <- frs_hh_raw %>% 
  select(SERNUM, ADULTH, BEDROOM6, ROOMS10, YEARWHC, DEPCHLDH, DISCHHA1,
          DISCHHC1, HHINC, HEARNS, HSEINC, HHRPINC, HHINV, HBENINC, HHDISBEN,  
          TUHHRENT, HHRENT, PTENTYP2,  GVTREGNO, COUNTRY, HHETHGR3,
         CTBAND,  CTDISC, CTREB, HHLDR02, HHSTAT, MAINACC, MULTI, 
         NEEDHELP, SHELTER, HHINCBND)

frs_hh_clean
frs_hh_clean <- replace(frs_hh_clean, frs_hh_clean < 0, NA) %>% 
  mutate_at(c(1:17), as.integer) %>% 
  mutate_at(-c(1:17), as.factor)

skimr::skim(frs_hh_clean)
summary(frs_hh_clean)
glimpse(frs_hh_clean)

str(frs_hh_clean)

# CREATING NEW VARIABLES

# CREATING NEW INDEPENDENT VARIABLE: 'YEARNO'

# YEARNO - the number of years an individual has been living in a house.
# To determine how long an individual has lived in a particular house, 
# we subtract the year in which the individual first started living in the house 
# 'YEARWHC' from the current year.
head(frs_hh_clean$YEARWHC)

# To obtain the current year as an integer:
current_year <- as.integer(format(Sys.Date(), '%Y'))

# Subtracting 'YEARWHC' from 'current_year' to obtain 'YEARNO':
frs_hh_clean <- frs_hh_clean %>% mutate(YEARNO = current_year - YEARWHC)
head(frs_hh_clean$YEARNO)

View(frs_hh_clean)

# CREATING TWO DEPENDENT VARIABLES: 'RENTYP' and 'YEARBND'

# Two dependent variables are used in this project; 'RENTYP' and 'YEARBND'

# Dependent Variable 1: 'RENTYP' - Rent Type

# RENTYP - A factor variable denoting Rent Type
# RENTYP is created from the 'PTENTYP2' variable which is originally coded as: 
# 1='Rented from council', 2='Rented from Housing Association', 
# 3='Rented privately unfurnished', 4='Rented privately furnished', 
# 5='owned outright', 6='owned with mortgage'
# PTENTYP2 is now re-coded as: 1&2 = 1 - 'social rented housing', 
# 3&4 = 2 - 'private rented housing', 5&6 = 3 - 'owned housing' 
# into a new variable called RENTYP.

frs_hh_clean$RENTYP <-  ifelse(frs_hh_clean$PTENTYP2 == 1 | frs_hh_clean$PTENTYP2 == 2, 1, 
                               ifelse(frs_hh_clean$PTENTYP2 == 3 | frs_hh_clean$PTENTYP2 == 4, 2, 
                                      ifelse(frs_hh_clean$PTENTYP2 == 5 | frs_hh_clean$PTENTYP2 == 6, 3,
                                             NA)))
frs_hh_clean %>% count(RENTYP)

# social rented housing(1) = 3544, private rented housing(2) = 3262, 
# owned housing(3) = 12404
# Since our project focuses on rented housing, owned housing data is filtered out.

frs_hh_clean <- frs_hh_clean %>% filter(RENTYP < 3)
frs_hh_clean %>% count(RENTYP)

# Dependent Variable 2: 'YEARBND' - Year Bands (Duration of Tenancy)

# YEARBND - A factor variable for 'YEARNO'
# YEARNO is converted from integer to a factor (categorical) variable (YEARBND)
# so that it can be used as a target variable for classification algorithm.
# Recall: Task 1B is to build a model that predicts how 
# long a tenant is likely to occupy a property before quitting.

head(frs_hh_clean$YEARNO, 10)
frs_hh_clean %>% count(YEARNO, sort = TRUE)
frs_hh_clean %>% count(is.na(frs_hh_clean$YEARNO))
# the YEARNO variable has 7 missing values

# Creating 'YEARBND' as follows: 1-5years = 1 (band1), >5years = 2 (band 2)

frs_hh_clean$YEARBND <- ifelse(frs_hh_clean$YEARNO <= 5, 1, ifelse(frs_hh_clean$YEARNO > 5, 2, NA))
frs_hh_clean$YEARBND <- as.factor(frs_hh_clean$YEARBND)
head(frs_hh_clean[c('YEARNO','YEARBND')])
frs_hh_clean %>% count(YEARBND)
colnames(frs_hh_clean)

# VISUALIZING THE 3 VARIABLES NEWLY CREATED

# Creating a theme for ggplot visualization
my_theme <- function() {theme(
            plot.subtitle = element_text(hjust=0, 
                                         face='italic'),
            axis.title = element_text(face='bold', 
                                      color = ('lemonchiffon4')),
            plot.margin = margin(0.5,0.5,0.5,0.5, 'cm'),
            panel.background = element_rect(fill= 'white'),
            panel.border = element_rect(color = 'grey', fill = NA),
            panel.grid.major = element_line(color = 'grey', linetype = 2, size = 0.5),
            panel.grid.minor = element_line(color = 'grey', linetype= 3, size = 0.5)
          )}

# Visualizing RENTYP
frs_hh_clean %>% ggplot(aes(RENTYP))+
  geom_bar(alpha= 0.5, width = 0.5, fill = (c=12))+
  geom_text(aes(label = ..count..), stat = 'count', vjust = 1.5)+
  labs(title = '"RENTYP" - Rent Type', 
       subtitle = '1=social rented housing, 2=private rented housing',
       x= 'Rent Type',
       caption = 'Family Resources Survey 2019-20' )+
  theme(plot.subtitle = element_text(hjust=0, face='italic'))+
  theme(axis.title = element_text(face='bold', color = ('lemonchiffon4')))

# Visualizing YEARNO
frs_hh_clean %>% ggplot(aes(YEARNO))+
  geom_histogram(alpha = 1, binwidth = 0.5, fill = (c=13))+
  labs(title = '"YEARNO" - Number of Years lived in Accommodation',
       subtitle = 'YEARNO is numeric and will be converted to factor',
       x = 'Number of Years',
       y = 'Count',
       caption = 'Family Resources Survey 2019-20')+my_theme()

# Visualizing YEARBND
frs_hh_clean %>% ggplot(aes(YEARBND))+
  geom_bar(alpha = 0.5, width = 0.5, fill = (c=12))+
  geom_text(aes(label = ..count..), stat = 'count', vjust = 1.5)+
  labs(title = '"YEARBND" - Year Bands indicating tenancy duration',
       subtitle = '1=short-term, 2=medium-term & 3=long-term tenancy',
       x = 'Year Bands',
       y = 'Count',
       caption = 'Family Resources Survey 2019-20')+my_theme()

# HANDLING MISSING VALUES IN THE 'frs_hh_clean' DATASET
skim(frs_hh_clean)
colnames(frs_hh_clean)
frs_hh_no_miss <- mice(frs_hh_clean, m=5, seed = 111)
frs_hh_final <- complete(frs_hh_no_miss, 5)

# 10 out of 34 variables had missing values as shown below. 
# Recall that the total number of observations is 6806.
frs_hh_no_miss$nmis

# The method employed in each case were there was a missing value is shown below.
frs_hh_no_miss$method
skim(complete(frs_hh_no_miss, 5))

# After implementing mice, all the missing values where computed except for the 
# 7 missing values in (Duration of Tenancy) YEARNO and 3 missing Values 
# in rent (HHRENT). These values were omitted from the dataset. 
# Hence the final dataset had 6796 rows and 34 columns.

frs_hh_final <- na.omit(frs_hh_final)
dim(frs_hh_final)
head(frs_hh_final)

# Finally, 'frs_hh_final' is saved as 'frs_hh_data' 
# to be called and used for project tasks 1 - 3.

write_csv(frs_hh_final, 'D:/ASDM_Coursework/Datasets/frs_housing_data.csv')

