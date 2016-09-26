###############################################################################
# Dealing with missing values for Titanic dataset
###############################################################################

###############################################################################
# Check to install and load packages that are needed for the script
# Source- http://www.vikparuchuri.com/blog/loading-andor-installing-packages/
###############################################################################
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

load_or_install(c("dplyr","tidyr", "R.utils"))

# Read in csv files; replace all values with space as NA
myData <- read.csv("titanic_original.csv", header = TRUE, sep = ",", stringsAsFactors=FALSE,na.strings=c(""," ","NA"))

# By observing output of tail(), the last passanger has missing information in all fields, except for the 
# age variable, therefore was excluded from the dataset.
myData <- myData[-1310,]

# TASK 1 - # Replace all missing values in embarked column as S (Initial missing values count = 3)

# Debug step to find missing values in embarked column
embarkedMissingValue <- sum(is.na(myData$embarked))
print(embarkedMissingValue)

# You can directly transform the column for missing value. Extra steps added for learning purpose.
embarked <- myData[,which( colnames(myData)=="embarked")]
embarked[is.na(embarked)] = "S"
myData$embarked <- embarked
# Validation check to ensure all missing values in embarked column are handled
if (sum(is.na(myData$embarked)) == 0) {
    print("There are no missing values in embarked column")
}

# TASK 2 - # Replace all missing values in age column with appropriate values. Give reasons for the same.
# (Initial missing values count = 264)

# converting sex variable from character to factor, as it has 2 distinct levels male and female 
# (no spell mistakes in state of sex)
myData$sex <- as.factor(myData$sex)

# For computing mean, median and mode for missing values replacement, compute them for male and female separately
# QUESTION -  Is computing statisitcs after seperating by gender right way or should we take it for 
# all passengers together.
# VIJAY - I seperately took the central tendency statistics as per gender. 
# Maybe final model might find some merits/de-merits in computing OR
# some good visualization technique might help us know if gender plays a role on age
# gender based statisitcs

# Function to compute mode as R does not have a readymade function
getMode <- function(x, na.rm = TRUE) {
    if(na.rm){
        x = x[!is.na(x)]
    }
    
    ux <- unique(x)
    return(ux[which.max(tabulate(match(x, ux)))])
}

# Extract fields based on sex
allMaleAges <- dplyr::filter(myData, myData$sex == "male")
allFemaleAges <- dplyr::filter(myData, myData$sex == "female")

# Compute the descriptive statistics for both sex
allMaleMean <- mean(allMaleAges$age, na.rm=TRUE)
allMaleMedian <- median(allMaleAges$age, na.rm=TRUE)
allMaleMode <- getMode(allMaleAges$age)

allFemaleMean <- mean(allFemaleAges$age, na.rm=TRUE)
allFemaleMedian <- median(allFemaleAges$age, na.rm=TRUE)
allFemaleMode <- getMode(allFemaleAges$age)

# FOR MALES
# MEAN = 30.58523; Median = 28 and Mode = 21
# Refer to the boxplot-ofAllMaleAges.png, there seem to seem some mild outliers in data set,
# hence prefer to replace missing values with Median. as outliers effect mean more than median and mode.

# REFERENCES USED - http://www.itl.nist.gov/div898/handbook/prc/section1/prc16.htm (for detecting outliers)
# https://www.mathsisfun.com/data/outliers.html (how outliers effect mean, median and mode)
ages <- allMaleAges[,5]
ages[is.na(ages)] <- allMaleMedian
allMaleAges$age = ages

# FOR FEMALES
# MEAN = 28.6900; Median = 27 and Mode = 27
# Refer to the boxplot-ofAllFemaleAges.png, there seem are no outliers in data set,
# hence prefer to replace missing values with mean.
ages <- allFemaleAges[,5]
ages[is.na(ages)] <- allFemaleMean
allFemaleAges$age = ages

myData <- dplyr::union(allMaleAges,allFemaleAges)
myData <- dplyr::arrange(myData,name)

# TASK 3 - Fill lifeboat empty slots with a dummy value e.g. the string 'None' or 'NA'
# VIJAY - Done while loading csv into dataframe using na.strings=c(""," ","NA")) option

#TASK 4: Cabin - Create a new column for cabin - 1 if cabin details are present else 0
addCabinNumber <- function(cabin) {
    ifelse(is.na(cabin), 0, 1)
}
myData <- mutate(myData, has_cabin_number = addCabinNumber(cabin))

# All done write it to output csv
write.csv(myData,"titanic_clean.csv")