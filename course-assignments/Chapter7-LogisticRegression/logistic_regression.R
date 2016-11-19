## Logistic Regression
## ═════════════════════════════════
is_installed <- function(mypkg) is.element(mypkg, installed.packages()[,1]) 

load_or_install<-function(package_names)  
{  
  for(package_name in package_names)  
  {  
    if(!is_installed(package_name))  
    {  
      install.packages(package_name,repos="http://lib.stat.cmu.edu/R/CRAN")  
    }  
    library(package_name,character.only=TRUE,quietly=TRUE,verbose=FALSE)  
  }  
}

load_or_install(c("dplyr","tidyr", "R.utils", "effects", "rio"))

## As an experiment ussing rio package to read the rds file.
# rio simplifies the process of importing data into R and exporting data from R

##   Load the National Health Interview Survey data:
NH11 <- import("dataSets/NatHealth2011.rds")

## Converting the NH11 data to csv for viewing (not an mandatory step for this assignment)
export(NH11, "dataSets/NatHealth2011.csv")

## Use glm to conduct a logistic regression to predict ever worked (everwrk) 
## using age (agep) and marital status (rmaritl)
nh11.wrk.age.mar <- subset(NH11, select = c("everwrk", "age_p", "r_maritl"))
summary(nh11.wrk.age.mar)

## Create a smaller data frame to work on.
NH11Data <-subset(NH11, select = c("everwrk", "age_p", "r_maritl"))

## Rename the column names to make it more understanding
NH11Data <- rename(NH11Data, HasEverworked = everwrk)
NH11Data <- rename(NH11Data, Age = age_p)
NH11Data <- rename(NH11Data, MaritalStatus = r_maritl)
View(NH11Data)

# Debug step to find missing values in embarked column
missingValue <- sum(is.na(NH11Data$HasEverworked))
print(missingValue)

# check stucture of hasEverWorked
str(NH11Data$HasEverworked)

## Print missing values in hasEverWorked factor (debug step)
missingValue <- sum(is.na(NH11cleaned$everwrk))
print(missingValue)

## Dropping unused factor levels in marital status
NH11DataCleaned <- transform(NH11Data,
                         HasEverworked = factor(HasEverworked,
                                                levels = c("1 Yes", "2 No")),
                         MaritalStatus = droplevels(MaritalStatus))

model.wk.age.mar <- glm(HasEverworked ~ Age + MaritalStatus, data = NH11DataCleaned,
                      family = "binomial")

summary(model.wk.age.mar)
plot(allEffects(model.wk.age.mar))

## Predict the probability of working for each level of marital status.
## Read the output of the fit metric to determine the log odd probability.
##  1 - Married status has a probablity of 11% of not having ever worked 
data.frame(Effect("MaritalStatus", model.wk.age.mar))
