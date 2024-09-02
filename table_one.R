# Table One Creation
#///////////////////#

#Download required packages
library("dplyr") 
library("readr")
library("ggplot2")
library("tableone")
library("haven")

#rename child19 and child20
child19_new <- child19 %>%
  rename(MAXEDUCP_C = MAXPAREDUC_C)

child20_new <- child20 %>%
  rename(MAXEDUCP_C = MAXPAREDUC_C)

# Select variables
select1 <- child21 %>%
  select(AGEP_C,SEX_C,HISPALLP_C,NOTCOV_C,DIBEV_C,ASDEV_C,MAXEDUCP_C, ADHDEV_C)
select2 <- child22 %>%
  select(AGEP_C,SEX_C,HISPALLP_C,NOTCOV_C,DIBEV_C,ASDEV_C,MAXEDUCP_C,ADHDEV_C)
select3 <- child20_new %>%
  select(AGEP_C,SEX_C,HISPALLP_C,NOTCOV_C,DIBEV_C,ASDEV_C,MAXEDUCP_C,ADHDEV_C)
select4 <- child19_new %>%
  select(AGEP_C,SEX_C,HISPALLP_C,NOTCOV_C,DIBEV_C,ASDEV_C,MAXEDUCP_C,ADHDEV_C)


# Combine child21, child 22, child20 and child19 into all_data
all_data <- bind_rows(select1, select2,select3, select4)
View(all_data)

# Select the columns we would like to create Table One with.
childtable <- all_data %>%
  select (AGEP_C, SEX_C,HISPALLP_C,NOTCOV_C,DIBEV_C,ASDEV_C, ADHDEV_C, MAXEDUCP_C) %>%
mutate(ASDEV_C = case_when(ASDEV_C == 1 ~ "yes",
                           ASDEV_C == 2 ~ "no")) %>% 
mutate(ASDEV_C = factor(ASDEV_C, levels = c("no", "yes"))) %>%
  
mutate(DIBEV_C = case_when(DIBEV_C == 1 ~ "yes",
                           DIBEV_C == 2 ~ "no")) %>%
mutate(DIBEV_C = factor(DIBEV_C, levels = c("no", "yes"))) %>%
  
mutate(SEX_C = case_when(SEX_C == 1 ~ "male",
                         SEX_C == 2 ~ "female")) %>%
mutate(SEX_C = factor(SEX_C, levels = c("female", "male"))) %>%
  
mutate(NOTCOV_C = case_when(NOTCOV_C== 1 ~ "no",
                            NOTCOV_C== 2 ~ "yes")) %>%
mutate(NOTCOV_C= factor(NOTCOV_C, levels = c("no", "yes"))) %>%
  
mutate(MAXEDUCP_C= case_when(MAXEDUCP_C == 00 ~ "Lower than Bachelor's degree",
                             MAXEDUCP_C == 01 ~ "Lower than Bachelor's degree", 
                             MAXEDUCP_C == 02 ~ "Lower than Bachelor's degree", 
                             MAXEDUCP_C == 03 ~ "Lower than Bachelor's degree", 
                             MAXEDUCP_C == 04 ~ "Lower than Bachelor's degree", 
                             MAXEDUCP_C == 05 ~ "Lower than Bachelor's degree",
                             MAXEDUCP_C == 06 ~ "Lower than Bachelor's degree", 
                             MAXEDUCP_C == 07 ~ "Lower than Bachelor's degree",
                             MAXEDUCP_C == 08 ~ "Bachelor's degree",
                             MAXEDUCP_C == 09 ~ "Higher than Bachelor's degree",
                             MAXEDUCP_C == 10 ~ "Higher than Bachelor's degree")) %>%
mutate(MAXEDUCP_C= factor(MAXEDUCP_C, levels = c("Lower than Bachelor's degree",
                                                   "Bachelor's degree","Higher than Bachelor's degree"))) %>%
  
mutate(HISPALLP_C= case_when(HISPALLP_C== 01 ~ "Hispanic",HISPALLP_C== 02 ~ "Non-Hispanic White only", 
                             HISPALLP_C == 03 ~ "Non-Hispanic Black/African American only", 
                             HISPALLP_C == 04 ~ "Other", 
                             HISPALLP_C == 05 ~ "Other",
                             HISPALLP_C == 06 ~ "Other",
                             HISPALLP_C == 07 ~ "Other")) %>%
mutate(HISPALLP_C= factor(HISPALLP_C, levels = c("Hispanic", "Non-Hispanic White only",
                                                   "Non-Hispanic Black/African American only",
                                                   "Other"))) %>%
  
mutate(ADHDEV_C = case_when(ADHDEV_C == 1 ~ "yes", ADHDEV_C == 2 ~ "no"))%>%
  mutate(ADHDEV_C = factor(ADHDEV_C, levels = c("no", "yes")))

# Note: Age does not need to be changed, given that we are taking it as a numerical variable

# But will check if age is numeric
class(childtable$AGEP_C)
childtable$AGEP_C <- as.numeric(childtable$AGEP_C)


# Finally time to create Table One 
vars <- c("DIBEV_C", "AGEP_C", "SEX_C", "HISPALLP_C", "NOTCOV_C", "ADHDEV_C", "MAXEDUCP_C")
table1 <- CreateTableOne(vars = vars, strata = "ASDEV_C", data = childtable1, factorVars = c("DIBEV_C", "SEX_C", "HISPALLP_C", "NOTCOV_C", "ADHDEV_C", "MAXEDUCP_C"))

print(table1)

# Rafael: add to table 1: age, and parental highest level of education , can keep ADHD


table_one_printed <- print(table1,
                           # For categorical variables, how many decimal places to display?
                           catDigits = 1,
                           # For continuous variables, how many decimal places to display?
                           contDigits = 1,
                           # For the p value, how many decimal places to display?
                           pDigits = 3,
                           # Which categorical variables use the Fisher's exact test?
                           exact = c("ADHDEV_C" , "ASDEV_C", "DIBEV_C", "NOTCOV_C", "MAXEDUCP_C", "HISPALLP_C", "SEX_C"),
                           # Which continuous variables use the Kruskal-Wallis Rank Sum Test?
                           kruskal.test("AGEP_C"),
                           printToggle = F)

table_one <- as.data.frame(table_one_printed, row.names = F) %>%
  mutate(Variable = rownames(table_one_printed)) %>%
  select(Variable, everything()) %>%
  select(-test) 

all_data1 <- replace(all_data, all_data=='', NA)
childtable1 <- na.omit(childtable)

