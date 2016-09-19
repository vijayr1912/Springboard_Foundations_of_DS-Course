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

load_or_install(c("dplyr","tidyr", "R.utils"))

if (isPackageLoaded("dplyr") && isPackageLoaded("tidyr")) {
    writeLines("Proceeed further. \n")
}
