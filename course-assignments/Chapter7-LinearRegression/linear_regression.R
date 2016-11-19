## Linear Regression
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
# read the states data
states.data <- import("dataSets/states.rds") 

## Converting the states data to csv for viewing (not an mandatory step for this assignment)
export(states.data, "dataSets/states.csv")

mod.en.metro.by.waste <- lm(energy ~ metro * waste, data = states.data)

mod.en.region <- lm(energy ~ metro * waste + region, data = states.data)
anova(mod.en.region)

plot(mod.en.metro.by.waste)