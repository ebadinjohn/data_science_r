# TASK 2: ASSOCIATION RULES MINING

# DATA IMPORT AND PREP

library(tidyverse)
library(arules)
library(arulesViz)

datatype <- c('iiiiiiiiiiiiiiiiiffffffffffffffiff')
frs_housing_data <- read_csv('D:/ASDM_Coursework/Datasets/frs_housing_data.csv', col_types = datatype)

colnames(frs_housing_data)
glimpse(frs_housing_data)

# Association rules mining only work with categorical data (factor). Therefore, 
# all the numeric variables are dropped
Assoc_rm <- frs_housing_data %>% select(GVTREGNO, COUNTRY, HHETHGR3, CTBAND, 
                                        CTDISC, CTREB, HHLDR02, HHSTAT, 
                                        MAINACC, MULTI, NEEDHELP, SHELTER,
                                        HHINCBND, RENTYP, YEARBND)
colnames(Assoc_rm)


# ASSOCIATION RULE MINING:FINDING RULES THAT EXPLAIN RENTYP (TASK 2A)
RENTYP_assoc_rules <- apriori(Assoc_rm, 
                              parameter = list(minlen = 2, 
                                               maxlen = 10, 
                                               support = 0.40, 
                                               confidence = 0.6),
                              appearance = list(rhs = c('RENTYP=social_rented',
                                                        'RENTYP=private_rented')))

RENTYP_assoc_rules
summary(RENTYP_assoc_rules)
inspect(RENTYP_assoc_rules)
plot(RENTYP_assoc_rules, method = 'grouped')

# ASSOCIATION RULE MINING:FINDING RULES THAT EXPLAIN YEARBND (TASK 2B)
YEARBND_assoc_rules <- apriori(Assoc_rm, 
                               parameter = list(minlen = 2, 
                                                maxlen = 10, 
                                                support = 0.4, 
                                                confidence = 0.7),
                              appearance = list(rhs = c('YEARBND=short_term', 
                                                        'YEARBND=long_term')))
YEARBND_assoc_rules
summary(YEARBND_assoc_rules)
inspect(YEARBND_assoc_rules)
plot(YEARBND_assoc_rules, method = 'grouped')