## k-means clustering analysis

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

load_or_install(c("cluster", "rattle","NbClust"))

# Now load the data and look at the first few rows
data(wine, package="rattle")
head(wine)

## standardize data
df <- scale(wine[-1])

## determine number of clusters
wssplot(df)
library(NbClust)
set.seed(1234)
nc <- NbClust(df, min.nc=2, max.nc=15, method="kmeans")
table(nc$Best.n[1,])

## k means cluster analysis
barplot(table(nc$Best.n[1,]), 
        xlab="Numer of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen by 26 Criteria")

set.seed(1234)
fit.km <- kmeans(df, 3, nstart=25)
fit.km$size

fit.km$centers

aggregate(wine[-1], by=list(cluster=fit.km$cluster), mean)

## A cross-tabulation of Type (wine varietal) and cluster membership
ct.km <- table(wine$Type, fit.km$cluster)