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

load_or_install(c("dplyr","tidyr", "R.utils", "effects"))

##   Load the National Health Interview Survey data:
NH11 <- readRDS("dataSets/NatHealth2011.rds")

## Use glm to conduct a logistic regression to predict ever worked (everwrk) 
## using age (agep) and marital status (rmaritl)
nh11.wrk.age.mar <- subset(NH11, select = c("everwrk", "age_p", "r_maritl"))
summary(nh11.wrk.age.mar)
NH11 <- transform(NH11,
                  everwrk = factor(everwrk,
                                   levels = c("1 Yes", "2 No")),
                  r_maritl = droplevels(r_maritl))

mod.wk.age.mar <- glm(everwrk ~ age_p + r_maritl, data = NH11,
                      family = "binomial")

summary(mod.wk.age.mar)

## Predict the probability of working for each level of marital status.
data.frame(Effect("r_maritl", mod.wk.age.mar))
