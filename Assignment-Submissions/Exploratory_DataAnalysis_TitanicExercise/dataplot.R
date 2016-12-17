###############################################################################
# Explore by visualization the titanic dataset present in ggplot2 library
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

# Check out the structure and class of Titanic dataset present in ggplot2
str(Titanic)
class(Titanic)

# Printing string + variable value Example. NOT PART OF THE ACTUAL CODE
# Refered from - http://stackoverflow.com/questions/15589601/print-string-and-variable-contents-on-the-same-line-in-r
#wd <- getwd()
#print(paste0("Current working dir: ", wd))
#cat("Oh Ganga Current working dir: ", wd)
print(paste0("Class of Titanic data set is : ", class(Titanic)))

# View the Titanic table
View(Titanic)

# Reference - http://www.cookbook-r.com/ and Data Camp Data Visualization using ggplot videos.
# As ggplot2() functions work on data frames convert the Titanic table to data frame for data visualization
myData <- as.data.frame(Titanic)
print(paste0("Class of Titanic data set after converting to dataframe is : ", class(myData)))

# Look at the distribution of sexes within the classes of the ship using geom_bar()
ggplot(myData, aes(x = Class, fill = Sex)) +
            geom_bar(position = "dodge")

# Lay panels ina grid on basis of Survival.
# Reference - https://www.rdocumentation.org/packages/ggplot2/versions/2.1.0/topics/facet_grid?
ggplot(myData, aes(x = Class, fill = Sex)) +
    geom_bar(position = "dodge") + 
    facet_grid(. ~ Survived)

# Adding the Age variable to the plot and drawing a scatter plot instead of a bar plot for better visualization.
ggplot(myData, aes(x = Class, fill = Sex)) +
    geom_bar(position = "dodge") + 
    facet_grid(. ~ Survived)
