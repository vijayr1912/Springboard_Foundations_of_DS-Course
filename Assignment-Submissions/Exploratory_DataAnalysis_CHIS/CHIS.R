###############################################################################
# Explore by visualization the CHIS adult-response dataset
###############################################################################

###############################################################################
# Check to install and load packages that are needed for the script
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

load_or_install(c("ggplot2","tidyr", "R.utils"))

###### Non Executable code as the CHIS data set is present only in the DataCamp IDE.

# Plot 1 - Count histogram
ggplot(adult, aes (x = SRAGE_P, fill= factor(RBMI))) + 
    geom_histogram(binwidth = 1) +
    BMI_fill

# Plot 2 - Density histogram
ggplot(adult, aes (x = SRAGE_P, fill= factor(RBMI))) + 
    geom_histogram(aes(y = ..density..), binwidth = 1) +
    BMI_fill

# Plot 3 - Faceted count histogram
ggplot(adult, aes (x = SRAGE_P, fill= factor(RBMI))) + 
    geom_histogram(binwidth = 1) +
    BMI_fill + 
    facet_grid(RBMI ~ .)

# Plot 4 - Faceted density histogram
ggplot(adult, aes(x = SRAGE_P, fill= factor(RBMI))) + 
    geom_histogram(aes(y = ..density..), binwidth = 1) +
    BMI_fill + 
    facet_grid(RBMI ~ .)

# Plot 5 - Density histogram with position = "fill"
ggplot(adult, aes(x = SRAGE_P, fill= factor(RBMI))) + 
    geom_histogram(aes(y = ..density..), binwidth = 1, position = "fill") +
    BMI_fill

# Plot 6 - The accurate histogram
ggplot(adult, aes (x = SRAGE_P, fill= factor(RBMI))) + 
    geom_histogram(aes(y = ..count../sum(..count..)), binwidth = 1, position = "fill") +
    BMI_fill