###############################################################################
# R commands to refine the toy data set showing product purchases from an 
# electronics store database #
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

load_or_install(c("dplyr","tidyr", "R.utils", "RecordLinkage"))

# TODO - Check if all packages needed are installed and loaded. Else quit from R gracefully.
if (FALSE) {
    if (isPackageLoaded("dplyr") && isPackageLoaded("tidyr")) {
        writeLines("Proceeed further. \n")
        quit("yes",0)
    }
}

# Vijay Question - Pro and cons of using header = TRUE, stringsAsFactors=FALSE
# Read in csv files
myData <- read.csv("refine_original.csv", header = TRUE, stringsAsFactors=FALSE)

# Creating a working copy to preserve the original data frame
workData = myData

# TASK 1 - Clean up brand names
# VIJAY - Difficult task as the company names are not only different in case but have mixed casing and spelling mistakes
#           Used a third party package RecordLinkage to use a function that computes closest resembling word by graph algorithm
# Question - Can dplyr be used to simplify this??

# function to find nearest match for each company name
standardizeCompanyNames <- function(companyName) {
    philips_score <<- levenshteinDist(companyName, "philips")
    akzo_score <<- levenshteinDist(companyName, "akzo")
    vanhouten_score <<- levenshteinDist(companyName, "van houten")
    unilever_score <<- levenshteinDist(companyName, "unilever")
    closestDistance <<- min(philips_score, akzo_score, vanhouten_score, unilever_score)
    if (closestDistance == philips_score) {
        "philips"
    } else if (closestDistance == akzo_score) {
        "akzo"
    } else if (closestDistance == vanhouten_score) {
        "van houten"
    } else {
        "unilever"
    }
}
workData$company <- lapply(workData$company, standardizeCompanyNames)
# Unlist the company name column to write to csv
workData$company <- unlist(workData$company, use.names = FALSE)

# TASK 2 - Separate product code and number
# Vijay - Simple Task dlyr comes in handy
workData <- dplyr::rename(workData, product_details = Product.code...number)
workData <- separate(workData, product_details, c("product_code", "product_number"), sep = "-")

# TASK 3 - Add product categories
# Vijay - Medium complexity task; Using ifelse (to return a vector of product category) along with dplyr:mutate
# QUESTION -  is there a better way of doing it?
addProductCategory <- function(code) {
    ifelse(code == "p","Smartphone",
           ifelse(code == "v","TV", 
                  ifelse(code == "x","Laptop", ifelse(code == "q","Tablet"," "))))
}
workData <- mutate(workData, product_category = addProductCategory(product_code))

# TASK 4 - Add full address for geocoding
# Vijay - Simple Task dlyr comes in handy
workData <- unite(workData, "full_address", address, city, country, sep = ", ")

# TASK 5 - Create dummy variables for company and product category
# Vijay - QUESTION -  is there a better way of doing it? dply?? tidyr?? or any other library.
    # not sure if we can use factor(...) as task specifies to create explicit dummy variables.
workData <- mutate(workData, product_smartphone = 0, product_tv = 0, 
              product_laptop = 0, product_tablet = 0)

workData$product_smartphone[workData$product_category == "Smartphone"] <- 1
workData$product_tv[workData$product_category == "TV"] <- 1
workData$product_laptop[workData$product_category == "Laptop"] <- 1
workData$product_tablet[workData$product_category == "Tablet"] <- 1

# Similarly create for company
workData <- mutate(workData, company_philips = 0, company_akzo = 0, 
                   company_van_houten = 0, company_unilever = 0)

workData$company_philips[workData$company == "philips"] <- 1
workData$company_akzo[workData$company == "akzo"] <- 1
workData$company_van_houten[workData$company == "van houten"] <- 1
workData$company_unilever[workData$company == "unilever"] <- 1

# Final cleaning
# Factors in columns 1,2 and 6 (company, product code and product category )
#   can be dropped as they are modeled into other factors and will not play any role in modeling
# Leaving this out for the present to validate the results.

# All done write it to output csv
write.csv(workData,"refine_clean.csv")