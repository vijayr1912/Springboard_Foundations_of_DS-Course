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

#################################################################################
# IMPORTANT NOTE
# The titanic dataset which the DataCamp refers and the Titanic dataset present
# in the ggplot2 package are different.

# An empty function for Comments
Comment <- function(`@Comments`) {invisible()}

#### Comments ####
Comment( `
        
        # titanic data set refered in DataCamp
        > head(titanic)
        Survived Pclass    Sex Age
        1        0      3   male  22
        2        1      1 female  38
        3        1      3 female  26
        4        1      1 female  35
        5        0      3   male  35
        6        0      1   male  54
        
        > str(titanic)
        'data.frame':	714 obs. of  4 variables:
            $ Survived: int  0 1 1 1 0 0 0 1 1 1 ...
        $ Pclass  : int  3 1 3 1 3 1 3 3 2 3 ...
        $ Sex     : chr  "male" "female" "female" "female" ...
        $ Age     : num  22 38 26 35 35 54 2 27 14 4 ...
        
        # Titanic data set refered in ggplot2 package
        > head(Titanic)
        [1]  0  0 35  0  0  0
        
        > str(Titanic)
        table [1:4, 1:2, 1:2, 1:2] 0 0 35 0 0 0 17 0 118 154 ...
        - attr(*, "dimnames")=List of 4
        ..$ Class   : chr [1:4] "1st" "2nd" "3rd" "Crew"
        ..$ Sex     : chr [1:2] "Male" "Female"
        ..$ Age     : chr [1:2] "Child" "Adult"
        ..$ Survived: chr [1:2] "No" "Yes"
        `)

# Hence the outplots are different.

# In fact the scatter plot for Titanic dataset does not make much sense as Age
# is a categorical variable, after modifying the Titanic table to a data frame.
#################################################################################


# Check out the structure and class of Titanic dataset present in ggplot2
str(Titanic)
class(Titanic)

# Printing string + variable value Example. NOT PART OF THE ACTUAL CODE
# Refered from - http://stackoverflow.com/questions/15589601/print-string-and-variable-contents-on-the-same-line-in-r
#wd <- getwd()
#print(paste0("Current working dir: ", wd))
#cat("Current working dir: ", wd)
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
ggplot(myData, aes(x = Class, y = Age, col = Sex)) +
    geom_jitter(size = 3) + 
    facet_grid(. ~ Survived)

# Adding the Age variable to the plot and drawing a scatter plot instead of a bar plot for better visualization.

# Set position jitter and alpha bending for better visualization of overlaping plots.
# Position jitter (use below)
posn.j <- position_jitter(0.5, 0)

ggplot(myData, aes(x = Class, y = Age, col = Sex)) + 
    geom_jitter(size = 3, alpha = 0.5, position = posn.j) + 
    facet_grid(. ~ Survived)
