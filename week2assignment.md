# coursera-R-program
https://www.coursera.org/learn/r-programming/home/welcome








title: 'Coursera: R programming Week 2 Assignment'


The zip file contains 332 comma-separated-value (CSV) files containing pollution monitoring data for fine particulate matter (PM) air pollution at 332 locations in the United States. Each file contains data from a single monitor and the ID number for each monitor is contained in the file name. For example, data for monitor 200 is contained in the file "200.csv". Each file contains three variables:

Date: the date of the observation in YYYY-MM-DD format (year-month-day)
sulfate: the level of sulfate PM in the air on that date (measured in micrograms per cubic meter)
nitrate: the level of nitrate PM in the air on that date (measured in micrograms per cubic meter)
For this programming assignment you will need to unzip this file and create the directory 'specdata'. Once you have unzipped the zip file, do not make any modifications to the files in the 'specdata' directory. In each file you'll notice that there are many days where either sulfate or nitrate (or both) are missing (coded as NA). This is common with air pollution monitoring data in the United States.

#Part 1

Write a function named 'pollutantmean' that calculates the mean of a pollutant (sulfate or nitrate) across a specified list of monitors. The function 'pollutantmean' takes three arguments: 'directory', 'pollutant', and 'id'. Given a vector monitor ID numbers, 'pollutantmean' reads that monitors' particulate matter data from the directory specified in the 'directory' argument and returns the mean of the pollutant across all of the monitors, ignoring any missing values coded as NA. A prototype of the function is as follows.

```{r}
pollutantmean <- function(directory, pollutant, id = 1:332) {
  
  #path <- file.path(getwd(),directory)
  #setwd(file.path(getwd(),directory))
  
  path <- paste0(getwd(),"/", directory) #this one is more general

  data <- data.frame()
  for (i in id) {
    if (i < 10) {
      dat <- read.csv(paste(path,"/00", as.character(i),".csv", sep = ""), 
                      as.is = T, 
                      header = T)
      data <- rbind(data,dat)
    }
    else if (i < 100) {
      dat <- read.csv(paste(path,"/0", as.character(i),".csv", sep = ""), 
                      as.is = T, 
                      header = T)
      data <- rbind(data,dat)
    }
    else {
      dat <- read.csv(paste(path,"/", as.character(i),".csv", sep = ""), 
                      as.is = T, 
                      header = T)
      data <- rbind(data,dat)
    }
    
  }
  return(mean(data[,pollutant], na.rm = T))
}


pollutantmean("specdata", "sulfate", 1:10)
pollutantmean("specdata", "nitrate", 70:72)
pollutantmean("specdata", "nitrate", 23)
```



#Part 2

Write a function that reads a directory full of files and reports the number of completely observed cases in each data file. The function should return a data frame where the first column is the name of the file and the second column is the number of complete cases. A prototype of this function follows.
```{r}
complete <- function(directory, id = 1:332) {
  path <- paste0(getwd(),"/", directory)
  data <- data.frame()
  for (i in id) {
    if (i < 10) {
      dat <- read.csv(paste(path,"/00", as.character(i),".csv", sep = ""), 
                      as.is = T, 
                      header = T)
      
    }
    else if (i < 100) {
      dat <- read.csv(paste(path,"/0", as.character(i),".csv", sep = ""), 
                      as.is = T, 
                      header = T)
     
    }
    else {
      dat <- read.csv(paste(path,"/", as.character(i),".csv", sep = ""), 
                      as.is = T, 
                      header = T)
      
    }
    nobs <- sum(complete.cases(dat))
    data9 <- data.frame(i, nobs)
    data <- rbind(data,data9)
    
  }
  return(data)
}


complete("specdata", 1)
complete("specdata", c(2, 4, 8, 10, 12))
complete("specdata", 30:25)
complete("specdata", 3)



```




#Part 3

Write a function that takes a directory of data files and a threshold for complete cases and calculates the correlation between sulfate and nitrate for monitor locations where the number of completely observed cases (on all variables) is greater than the threshold. The function should return a vector of correlations for the monitors that meet the threshold requirement. If no monitors meet the threshold requirement, then the function should return a numeric vector of length 0. A prototype of this function follows

```{r}
corr <- function(directory, threshold = 0) {
  path <- paste0(getwd(),"/", directory)
  corr_vect <- NULL
  for (i in 1:332) {
    if (i < 10) {
      dat <- read.csv(paste(path,"/00", as.character(i),".csv", sep = ""), 
                      as.is = T, 
                      header = T)
    }
    else if (i < 100) {
      dat <- read.csv(paste(path,"/0", as.character(i),".csv", sep = ""), 
                      as.is = T, 
                      header = T)
    }
    else {
      dat <- read.csv(paste(path,"/", as.character(i),".csv", sep = ""), 
                      as.is = T, 
                      header = T)
    }
    
    data <- dat[complete.cases(dat),]
    if (nrow(data) > threshold) {
      corr_vect <- c(corr_vect, cor(data[,"sulfate"], data[, "nitrate"]))
    }
  }
    
    return(corr_vect)
}


```


```{r}
cr <- corr("specdata", 150)
head(cr,6)
summary(cr)

cr <- corr("specdata", 400)
head(cr,6)
summary(cr)

cr <- corr("specdata", 5000)
summary(cr)
length(cr)

cr <- corr("specdata")
summary(cr)
length(cr)


```


For this function you will need to use the 'cor' function in R which calculates the correlation between two vectors. Please read the help page for this function via '?cor' and make sure that you know how to use it.

You can see some example output from this function below. The function that you write should be able to approximately match this output. Note that because of how R rounds and presents floating point numbers, the output you generate may differ slightly from the example output. Please save your code to a file named corr.R. To run the submit script for this part, make sure your working directory has the file corr.R in it.



```{r}
pollutantmean("specdata", "sulfate", 1:10)


pollutantmean("specdata", "nitrate", 70:72)


pollutantmean("specdata", "sulfate", 34)


pollutantmean("specdata", "nitrate")


cc <- complete("specdata", c(6, 10, 20, 34, 100, 200, 310))
print(cc$nobs)


cc <- complete("specdata", 54)
print(cc$nobs)



set.seed(42)
cc <- complete("specdata", 332:1)
use <- sample(332, 10)
print(cc[use, "nobs"])


cr <- corr("specdata")                
cr <- sort(cr)                
set.seed(868)                
out <- round(cr[sample(length(cr), 5)], 4)
print(out)


cr <- corr("specdata", 129)                
cr <- sort(cr)                
n <- length(cr)                
set.seed(197)                
out <- c(n, round(cr[sample(n, 5)], 4))
print(out)


cr <- corr("specdata", 2000)                
n <- length(cr)                
cr <- corr("specdata", 1000)                
cr <- sort(cr)
print(c(n, round(cr, 4)))


```




