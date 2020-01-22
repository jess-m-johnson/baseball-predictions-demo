#Set Working Directory
#setwd("G://Workspace//R")

#Load dplyr package into workspace.
library("dplyr")

#Load example dataset - diabetic_data is a dataset based on 10yrs (1999-2008) of clinical care data from 130 US hospital and integrated delivery networks.
diabetic_data <- read.csv("diabetic_data.csv", header = TRUE)

#Explore dataset to understand the data.
dim(diabetic_data) # Review dataset dimensionality.
names(diabetic_data) # Review dataset columns/factors.
glimpse(diabetic_data, width = 100) # Review sample columns and data - part of dplyr pkg
summary(diabetic_data) # Review summary level data.

#'<h3><i>Select()</i></h3>
#Select specific columns from the sample dataset (diabetic_data).
# Base R approach to select specific columns/factors
temp1 <- diabetic_data[, c("gender", "age", "readmitted", "A1Cresult", "time_in_hospital", "number_inpatient", "number_emergency", "number_outpatient")]

# dplyr approach to selecting specific columns/factors. Here we will save to a variable for future use.
readmit_data <- select(diabetic_data, gender, age, readmitted, A1Cresult, time_in_hospital, number_inpatient, number_emergency, number_outpatient)

# review select() result dataset. Limit to width of 50 for documentation readability
glimpse(readmit_data, width = 50)

# base R approach
head(readmit_data)

#Convert to local data frame or tbl to allow for more friendly printing properties, defaults 10 rows
readmit_risk_obs <- tbl_df(readmit_data)
print(readmit_risk_obs, n = 5)

#'<h3><i>Mutate()</i></h3>
# Base R computed column approach.
temp1$total_visits <- temp1$number_inpatient + temp1$number_outpatient + temp1$number_emergency
head(temp1)

# dplyr computed column approach (Total number of visits)
readmit_risk_obs <- mutate(readmit_risk_obs, total_visits = number_inpatient + number_emergency + number_outpatient)
head(readmit_risk_obs)

#' <h3><i>Filter()</i></h3>
#Base R approach to filtering
temp1 <- temp1[temp1$A1Cresult != "None" & (temp1$total_visits > 0 | temp1$readmitted != "NO"),]
head(temp1)

# dplyr Filter approach - you can use ',' or '&' for AND condition; you can use '|' for OR condition
readmit_risk_obs <- filter(readmit_risk_obs, A1Cresult != "None", total_visits > 0 | readmitted != "NO")
head(readmit_risk_obs)

#' #'<h3><i>Arrange()</i></h3>
# base R approach of ordering in decsending order.
temp1 <- temp1[order(temp1$total_visits, decreasing = TRUE), names(temp1)]
head(temp1)

#Sort the dataset by total_visits in descending order.
readmit_risk_obs <- arrange(readmit_risk_obs, desc(total_visits))
head(readmit_risk_obs)

#'<h3><i>Pipe Operator</i></h3>
# base R approach
head(temp1[order(temp1$total_visits, decreasing = TRUE), c("gender", "age", "A1Cresult", "total_visits")])

# 'dplyr' approach with no pipe operator
head(arrange(select(readmit_risk_obs, gender, age, A1Cresult, total_visits), desc(total_visits)))

# 'dplyr' pipe operator approach
readmit_risk_obs %>% select(gender, age, A1Cresult, total_visits) %>% arrange(desc(total_visits)) %>% head


#'<h3><i>Summarise() and Group_by()</i></h3>
# Base R approach to calculate the average total_visits and standard deviation for each AC1result group
aggregate(total_visits ~ A1Cresult, temp1, function(x) c(avg = mean(x), sd = sd(x)))

# dplyr approach to calcualte the average total_Visits and standard deviation for each AC1result group
readmit_risk_obs %>%
    group_by(A1Cresult) %>%
    summarise(avg_visits = mean(total_visits), sd_total_visits = sd(total_visits))
