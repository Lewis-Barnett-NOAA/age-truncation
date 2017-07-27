# Age truncation metaanalysis
# Collaboration between Lewis Barnett, Trevor Branch, R. Anthony Ranasinghe, and Tim Essington
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

setwd("C:/Users/lewis.barnett/Documents/Postdoc/AgeTruncation/Data/Analysis")

library(readxl)    
library(colorRamps)
library(beanplot)
library(dplyr)
library(plotrix)

# function to read sheets from Excel
read_excel_allsheets <- function(filename) {
  sheets <- readxl::excel_sheets(filename)
  x <-    lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  names(x) <- sheets
  x
}

# read data into a list
N.age.WC <- read_excel_allsheets("C:/Users/lewis.barnett/Documents/Postdoc/AgeTruncation/Data/Analysis/lumped data (West Coast).xlsx")
N.age.SA <- read_excel_allsheets("C:/Users/lewis.barnett/Documents/Postdoc/AgeTruncation/Data/Analysis/lumped data (South Atlantic).xlsx")
N.age.NE <- read_excel_allsheets("C:/Users/lewis.barnett/Documents/Postdoc/AgeTruncation/Data/Analysis/lumped data (Mid Atlantic & New England).xlsx")
N.age.NA <- read_excel_allsheets("C:/Users/lewis.barnett/Documents/Postdoc/AgeTruncation/Data/Analysis/lumped data (Iceland).xlsx")
N.age.AK <- read_excel_allsheets("C:/Users/lewis.barnett/Documents/Postdoc/AgeTruncation/Data/Analysis/lumped data (Alaska).xlsx")

# remove forecast years from data
for(i in 1:length(N.age.WC)){ N.age.WC[[i]] = N.age.WC[[i]][which(N.age.WC[[i]][,2] == 0 ),] }
for(i in 1:length(N.age.NA)){ N.age.NA[[i]] = N.age.NA[[i]][which(N.age.NA[[i]][,2] == 0 ),] }
for(i in 1:length(N.age.SA)){ N.age.SA[[i]] = N.age.SA[[i]][which(N.age.SA[[i]][,2] == 0 ),] }
for(i in 1:length(N.age.NE)){ N.age.NE[[i]] = N.age.NE[[i]][which(N.age.NE[[i]][,2] == 0 ),] }
for(i in 1:length(N.age.AK)){ N.age.AK[[i]] = N.age.AK[[i]][which(N.age.AK[[i]][,2] == 0 ),] }

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# RECONSTRUCTION OF ORIGINAL AGE STRUCTURE FROM M
N.age.init.NA <- read_excel("C:/Users/lewis.barnett/Documents/Postdoc/AgeTruncation/Data/Analysis/lumped M data (Iceland).xlsx")
Prop.age.init.NA = matrix(nrow = nrow(N.age.init.NA), ncol = 200, dimnames = N.age.init.NA[,1])

for(i in 1:nrow(N.age.init.NA)){
  for(j in 2:201){
    if(j < ncol(N.age.init.NA) - sum(is.na(N.age.init.NA[i,])))
      Prop.age.init.NA[i,j-1] = exp(-sum(N.age.init.NA[i,2:j]))
    else
     Prop.age.init.NA[i,j-1] = exp(-sum(N.age.init.NA[i,2:(ncol(N.age.init.NA) - sum(is.na(N.age.init.NA[i,])))],
                                    (j - (ncol(N.age.init.NA) - sum(is.na(N.age.init.NA[i,])))) * 
                                      N.age.init.NA[i,(ncol(N.age.init.NA) - sum(is.na(N.age.init.NA[i,])))]))
  }
  # scale age structure to sum to 1
  Prop.age.init.NA[i,] = Prop.age.init.NA[i,] / sum(Prop.age.init.NA[i,])
}

# Make into list and lump older individuals into plus group
Prop.age.init.NA = as.list(data.frame(t(Prop.age.init.NA)))
for(i in 1:nrow(N.age.init.NA)){
  Prop.age.init.NA[[i]][(ncol(N.age.init.NA) - sum(is.na(N.age.init.NA[i,])) - 1)] = 
    sum(Prop.age.init.NA[[i]][(ncol(N.age.init.NA) - sum(is.na(N.age.init.NA[i,])) - 1):length(Prop.age.init.NA[[i]])])
  Prop.age.init.NA[[i]] = Prop.age.init.NA[[i]][1:(ncol(N.age.init.NA) - sum(is.na(N.age.init.NA[i,])) - 1)]
}


N.age.init.AK <- read_excel("C:/Users/lewis.barnett/Documents/Postdoc/AgeTruncation/Data/Analysis/lumped M data (Alaska).xlsx")
Prop.age.init.AK = matrix(nrow = nrow(N.age.init.AK), ncol = 200, dimnames = N.age.init.AK[,1])

for(i in 1:nrow(N.age.init.AK)){
  for(j in 2:201){
    if(j < ncol(N.age.init.AK) - sum(is.na(N.age.init.AK[i,])))
      Prop.age.init.AK[i,j-1] = exp(-sum(N.age.init.AK[i,2:j]))
    else
      Prop.age.init.AK[i,j-1] = exp(-sum(N.age.init.AK[i,2:(ncol(N.age.init.AK) - sum(is.na(N.age.init.AK[i,])))],
                                         (j - (ncol(N.age.init.AK) - sum(is.na(N.age.init.AK[i,])))) * 
                                           N.age.init.AK[i,(ncol(N.age.init.AK) - sum(is.na(N.age.init.AK[i,])))]))
  }
  # scale age structure to sum to 1
  Prop.age.init.AK[i,] = Prop.age.init.AK[i,] / sum(Prop.age.init.AK[i,])
}

# Make into list and lump older individuals into plus group
Prop.age.init.AK = as.list(data.frame(t(Prop.age.init.AK)))
for(i in 1:nrow(N.age.init.AK)){
  Prop.age.init.AK[[i]][(ncol(N.age.init.AK) - sum(is.na(N.age.init.AK[i,])) - 1)] = 
    sum(Prop.age.init.AK[[i]][(ncol(N.age.init.AK) - sum(is.na(N.age.init.AK[i,])) - 1):length(Prop.age.init.AK[[i]])])
  Prop.age.init.AK[[i]] = Prop.age.init.AK[[i]][1:(ncol(N.age.init.AK) - sum(is.na(N.age.init.AK[i,])) - 1)]
}


N.age.init.NE <- read_excel("C:/Users/lewis.barnett/Documents/Postdoc/AgeTruncation/Data/Analysis/lumped M data (Mid Atlantic & New England).xlsx")
Prop.age.init.NE = matrix(nrow = nrow(N.age.init.NE), ncol = 200, dimnames = N.age.init.NE[,1])

for(i in 1:nrow(N.age.init.NE)){
  for(j in 2:201){
    if(j < ncol(N.age.init.NE) - sum(is.na(N.age.init.NE[i,])))
      Prop.age.init.NE[i,j-1] = exp(-sum(N.age.init.NE[i,2:j]))
    else
      Prop.age.init.NE[i,j-1] = exp(-sum(N.age.init.NE[i,2:(ncol(N.age.init.NE) - sum(is.na(N.age.init.NE[i,])))],
                                         (j - (ncol(N.age.init.NE) - sum(is.na(N.age.init.NE[i,])))) * 
                                           N.age.init.NE[i,(ncol(N.age.init.NE) - sum(is.na(N.age.init.NE[i,])))]))
  }
  # scale age structure to sum to 1
  Prop.age.init.NE[i,] = Prop.age.init.NE[i,] / sum(Prop.age.init.NE[i,])
}

# Make into list and lump older individuals into plus group
Prop.age.init.NE = as.list(data.frame(t(Prop.age.init.NE)))
for(i in 1:nrow(N.age.init.NE)){
  Prop.age.init.NE[[i]][(ncol(N.age.init.NE) - sum(is.na(N.age.init.NE[i,])) - 1)] = 
    sum(Prop.age.init.NE[[i]][(ncol(N.age.init.NE) - sum(is.na(N.age.init.NE[i,])) - 1):length(Prop.age.init.NE[[i]])])
  Prop.age.init.NE[[i]] = Prop.age.init.NE[[i]][1:(ncol(N.age.init.NE) - sum(is.na(N.age.init.NE[i,])) - 1)]
}


N.age.init.WC <- read_excel("C:/Users/lewis.barnett/Documents/Postdoc/AgeTruncation/Data/Analysis/lumped M data (West Coast).xlsx")
Prop.age.init.WC = matrix(nrow = nrow(N.age.init.WC), ncol = 200, dimnames = N.age.init.WC[,1])

for(i in 1:nrow(N.age.init.WC)){
  for(j in 2:201){
    if(j < ncol(N.age.init.WC) - sum(is.na(N.age.init.WC[i,])))
      Prop.age.init.WC[i,j-1] = exp(-sum(N.age.init.WC[i,2:j]))
    else
      Prop.age.init.WC[i,j-1] = exp(-sum(N.age.init.WC[i,2:(ncol(N.age.init.WC) - sum(is.na(N.age.init.WC[i,])))],
                                         (j - (ncol(N.age.init.WC) - sum(is.na(N.age.init.WC[i,])))) * 
                                           N.age.init.WC[i,(ncol(N.age.init.WC) - sum(is.na(N.age.init.WC[i,])))]))
  }
  # scale age structure to sum to 1
  Prop.age.init.WC[i,] = Prop.age.init.WC[i,] / sum(Prop.age.init.WC[i,])
}

# Make into list and lump older individuals into plus group
Prop.age.init.WC = as.list(data.frame(t(Prop.age.init.WC)))
for(i in 1:nrow(N.age.init.WC)){
  Prop.age.init.WC[[i]][(ncol(N.age.init.WC) - sum(is.na(N.age.init.WC[i,])) - 1)] = 
    sum(Prop.age.init.WC[[i]][(ncol(N.age.init.WC) - sum(is.na(N.age.init.WC[i,])) - 1):length(Prop.age.init.WC[[i]])])
  Prop.age.init.WC[[i]] = Prop.age.init.WC[[i]][1:(ncol(N.age.init.WC) - sum(is.na(N.age.init.WC[i,])) - 1)]
}

N.age.init.SA <- read_excel("C:/Users/lewis.barnett/Documents/Postdoc/AgeTruncation/Data/Analysis/lumped M data (South Atlantic).xlsx")
Prop.age.init.SA = matrix(nrow = nrow(N.age.init.SA), ncol = 200, dimnames = N.age.init.SA[,1])

for(i in 1:nrow(N.age.init.SA)){
  for(j in 2:201){
    if(j < ncol(N.age.init.SA) - sum(is.na(N.age.init.SA[i,])))
      Prop.age.init.SA[i,j-1] = exp(-sum(N.age.init.SA[i,2:j]))
    else
      Prop.age.init.SA[i,j-1] = exp(-sum(N.age.init.SA[i,2:(ncol(N.age.init.SA) - sum(is.na(N.age.init.SA[i,])))],
                                         (j - (ncol(N.age.init.SA) - sum(is.na(N.age.init.SA[i,])))) * 
                                           N.age.init.SA[i,(ncol(N.age.init.SA) - sum(is.na(N.age.init.SA[i,])))]))
  }
  # scale age structure to sum to 1
  Prop.age.init.SA[i,] = Prop.age.init.SA[i,] / sum(Prop.age.init.SA[i,])
}

# Make into list and lump older individuals into plus group
Prop.age.init.SA = as.list(data.frame(t(Prop.age.init.SA)))
for(i in 1:nrow(N.age.init.SA)){
  Prop.age.init.SA[[i]][(ncol(N.age.init.SA) - sum(is.na(N.age.init.SA[i,])) - 1)] = 
    sum(Prop.age.init.SA[[i]][(ncol(N.age.init.SA) - sum(is.na(N.age.init.SA[i,])) - 1):length(Prop.age.init.SA[[i]])])
  Prop.age.init.SA[[i]] = Prop.age.init.SA[[i]][1:(ncol(N.age.init.SA) - sum(is.na(N.age.init.SA[i,])) - 1)]
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Compute and plot stripplot summary of changes in relative abundance accross stocks, by age class

# Summarize depletion by binned relative age class
log.change = matrix(nrow = (length(N.age.WC)+length(N.age.NA)+length(N.age.SA)+length(N.age.NE)+length(N.age.AK)), ncol = 6, 
                    dimnames = list(c(names(N.age.AK),names(N.age.WC),names(N.age.SA),names(N.age.NE),names(N.age.NA)), 
                                    c("Region","0.00-0.25","0.26-0.50","0.51-0.75","0.76-0.99","Oldest age class")))
for(i in 1:length(N.age.AK)){
  start.ages = seq(3, ncol(N.age.AK[[i]]))[1:round((0.25*(ncol(N.age.AK[[i]])-2)))]
  log.change[i,2] = log( (mean(rowSums(N.age.AK[[i]][(nrow(N.age.AK[[i]])-4):nrow(N.age.AK[[i]]),start.ages])) /
                            (mean(rowSums(N.age.AK[[i]][(nrow(N.age.AK[[i]])-4):nrow(N.age.AK[[i]]),3:ncol(N.age.AK[[i]])]))) ) /
                           (mean(rowSums(N.age.AK[[i]][1:5,start.ages])) /
                              (mean(rowSums(N.age.AK[[i]][1:5,3:ncol(N.age.AK[[i]])])))) 
                         )
  # 0.26-0.5
  second.ages = (1+tail(start.ages,1)):max((1+tail(start.ages,1)),round((0.5*(ncol(N.age.AK[[i]])-2))))
  log.change[i,3] = log( (mean(rowSums(N.age.AK[[i]][(nrow(N.age.AK[[i]])-4):nrow(N.age.AK[[i]]),second.ages])) /
                            (mean(rowSums(N.age.AK[[i]][(nrow(N.age.AK[[i]])-4):nrow(N.age.AK[[i]]),3:ncol(N.age.AK[[i]])]))) ) /
                           (mean(rowSums(N.age.AK[[i]][1:5,second.ages])) /
                              (mean(rowSums(N.age.AK[[i]][1:5,3:ncol(N.age.AK[[i]])])))) 
                         )
  # 0.51-0.75
  third.ages = (1+tail(second.ages,1)):max((1+tail(second.ages,1)),round((0.75*(ncol(N.age.AK[[i]])-2))))
  log.change[i,4] = log( (mean(rowSums(N.age.AK[[i]][(nrow(N.age.AK[[i]])-4):nrow(N.age.AK[[i]]),third.ages])) /
                            (mean(rowSums(N.age.AK[[i]][(nrow(N.age.AK[[i]])-4):nrow(N.age.AK[[i]]),3:ncol(N.age.AK[[i]])]))) ) /
                           (mean(rowSums(N.age.AK[[i]][1:5,third.ages])) /
                              (mean(rowSums(N.age.AK[[i]][1:5,3:ncol(N.age.AK[[i]])])))) 
                         )
  # 0.76-0.99
  fourth.ages = (1+tail(third.ages,1)):max((1+tail(third.ages,1)),ncol(N.age.AK[[i]])-1)
  log.change[i,5] = log( (mean(rowSums(N.age.AK[[i]][(nrow(N.age.AK[[i]])-4):nrow(N.age.AK[[i]]),fourth.ages])) /
                            (mean(rowSums(N.age.AK[[i]][(nrow(N.age.AK[[i]])-4):nrow(N.age.AK[[i]]),3:ncol(N.age.AK[[i]])]))) ) /
                           (mean(rowSums(N.age.AK[[i]][1:5,fourth.ages])) /
                              (mean(rowSums(N.age.AK[[i]][1:5,3:ncol(N.age.AK[[i]])])))) 
                         )
  # plus group (1)
  log.change[i,6] = log( ((sum(N.age.AK[[i]][(nrow(N.age.AK[[i]])-4):nrow(N.age.AK[[i]]),ncol(N.age.AK[[i]])]) / 5) /
                            (mean(rowSums(N.age.AK[[i]][(nrow(N.age.AK[[i]])-4):nrow(N.age.AK[[i]]),3:ncol(N.age.AK[[i]])]))) ) /
                           ((sum(N.age.AK[[i]][1:5,ncol(N.age.AK[[i]])]) / 5) /
                              (mean(rowSums(N.age.AK[[i]][1:5,3:ncol(N.age.AK[[i]])])))) 
                         )
}

for(i in 1:length(N.age.WC)){
  # youngest 0-0.25 rel age
  start.ages = seq(3, ncol(N.age.WC[[i]]))[1:round((0.25*(ncol(N.age.WC[[i]])-2)))]
  log.change[(i+length(N.age.AK)),2] = 
    log( (mean(rowSums(N.age.WC[[i]][(nrow(N.age.WC[[i]])-4):nrow(N.age.WC[[i]]),start.ages])) /
            (mean(rowSums(N.age.WC[[i]][(nrow(N.age.WC[[i]])-4):nrow(N.age.WC[[i]]),3:ncol(N.age.WC[[i]])]))) ) /
           (mean(rowSums(N.age.WC[[i]][1:5,start.ages])) /
              (mean(rowSums(N.age.WC[[i]][1:5,3:ncol(N.age.WC[[i]])])))) )
  # 0.26-0.5
  second.ages = (1+tail(start.ages,1)):max((1+tail(start.ages,1)),round((0.5*(ncol(N.age.WC[[i]])-2))))
  log.change[(i+length(N.age.AK)),3] = 
    log( (mean(rowSums(N.age.WC[[i]][(nrow(N.age.WC[[i]])-4):nrow(N.age.WC[[i]]),second.ages])) /
            (mean(rowSums(N.age.WC[[i]][(nrow(N.age.WC[[i]])-4):nrow(N.age.WC[[i]]),3:ncol(N.age.WC[[i]])]))) ) /
           (mean(rowSums(N.age.WC[[i]][1:5,second.ages])) /
              (mean(rowSums(N.age.WC[[i]][1:5,3:ncol(N.age.WC[[i]])])))) )
  # 0.51-0.75
  third.ages = (1+tail(second.ages,1)):max((1+tail(second.ages,1)),round((0.75*(ncol(N.age.WC[[i]])-2))))
  log.change[(i+length(N.age.AK)),4] = 
    log( (mean(rowSums(N.age.WC[[i]][(nrow(N.age.WC[[i]])-4):nrow(N.age.WC[[i]]),third.ages])) /
            (mean(rowSums(N.age.WC[[i]][(nrow(N.age.WC[[i]])-4):nrow(N.age.WC[[i]]),3:ncol(N.age.WC[[i]])]))) ) /
           (mean(rowSums(N.age.WC[[i]][1:5,third.ages])) /
              (mean(rowSums(N.age.WC[[i]][1:5,3:ncol(N.age.WC[[i]])])))) )
  # 0.76-0.99
  fourth.ages = (1+tail(third.ages,1)):max((1+tail(third.ages,1)),ncol(N.age.WC[[i]])-1)
  log.change[(i+length(N.age.AK)),5] = 
    log( (mean(rowSums(N.age.WC[[i]][(nrow(N.age.WC[[i]])-4):nrow(N.age.WC[[i]]),fourth.ages])) /
            (mean(rowSums(N.age.WC[[i]][(nrow(N.age.WC[[i]])-4):nrow(N.age.WC[[i]]),3:ncol(N.age.WC[[i]])]))) ) /
           (mean(rowSums(N.age.WC[[i]][1:5,fourth.ages])) /
              (mean(rowSums(N.age.WC[[i]][1:5,3:ncol(N.age.WC[[i]])])))) )
  # plus group (1)
  log.change[(i+length(N.age.AK)),6] = 
    log( ((sum(N.age.WC[[i]][(nrow(N.age.WC[[i]])-4):nrow(N.age.WC[[i]]),ncol(N.age.WC[[i]])]) / 5) /
            (mean(rowSums(N.age.WC[[i]][(nrow(N.age.WC[[i]])-4):nrow(N.age.WC[[i]]),3:ncol(N.age.WC[[i]])]))) ) /
           ((sum(N.age.WC[[i]][1:5,ncol(N.age.WC[[i]])]) / 5) /
              (mean(rowSums(N.age.WC[[i]][1:5,3:ncol(N.age.WC[[i]])])))) )
}

for(i in 1:length(N.age.SA)){
  # youngest 0-0.25 rel age
  start.ages = seq(3, ncol(N.age.SA[[i]]))[1:round((0.25*(ncol(N.age.SA[[i]])-2)))]
  log.change[(i+length(N.age.WC)+length(N.age.AK)),2] = 
    log( (mean(rowSums(N.age.SA[[i]][(nrow(N.age.SA[[i]])-4):nrow(N.age.SA[[i]]),start.ages])) /
            (mean(rowSums(N.age.SA[[i]][(nrow(N.age.SA[[i]])-4):nrow(N.age.SA[[i]]),3:ncol(N.age.SA[[i]])]))) ) /
           (mean(rowSums(N.age.SA[[i]][1:5,start.ages])) /
              (mean(rowSums(N.age.SA[[i]][1:5,3:ncol(N.age.SA[[i]])])))) )
  # 0.26-0.5
  second.ages = (1+tail(start.ages,1)):max((1+tail(start.ages,1)),round((0.5*(ncol(N.age.SA[[i]])-2))))
  log.change[(i+length(N.age.WC)+length(N.age.AK)),3] = 
    log( (mean(rowSums(N.age.SA[[i]][(nrow(N.age.SA[[i]])-4):nrow(N.age.SA[[i]]),second.ages])) /
            (mean(rowSums(N.age.SA[[i]][(nrow(N.age.SA[[i]])-4):nrow(N.age.SA[[i]]),3:ncol(N.age.SA[[i]])]))) ) /
           (mean(rowSums(N.age.SA[[i]][1:5,second.ages])) /
              (mean(rowSums(N.age.SA[[i]][1:5,3:ncol(N.age.SA[[i]])])))) )
  # 0.51-0.75
  third.ages = (1+tail(second.ages,1)):max((1+tail(second.ages,1)),round((0.75*(ncol(N.age.SA[[i]])-2))))
  log.change[(i+length(N.age.WC)+length(N.age.AK)),4] = 
    log( (mean(rowSums(N.age.SA[[i]][(nrow(N.age.SA[[i]])-4):nrow(N.age.SA[[i]]),third.ages])) /
            (mean(rowSums(N.age.SA[[i]][(nrow(N.age.SA[[i]])-4):nrow(N.age.SA[[i]]),3:ncol(N.age.SA[[i]])]))) ) /
           (mean(rowSums(N.age.SA[[i]][1:5,third.ages])) /
              (mean(rowSums(N.age.SA[[i]][1:5,3:ncol(N.age.SA[[i]])])))) )
  # 0.76-0.99
  fourth.ages = (1+tail(third.ages,1)):max((1+tail(third.ages,1)),ncol(N.age.SA[[i]])-1)
  log.change[(i+length(N.age.WC)+length(N.age.AK)),5] = 
    log( (mean(rowSums(N.age.SA[[i]][(nrow(N.age.SA[[i]])-4):nrow(N.age.SA[[i]]),fourth.ages])) /
            (mean(rowSums(N.age.SA[[i]][(nrow(N.age.SA[[i]])-4):nrow(N.age.SA[[i]]),3:ncol(N.age.SA[[i]])]))) ) /
           (mean(rowSums(N.age.SA[[i]][1:5,fourth.ages])) /
              (mean(rowSums(N.age.SA[[i]][1:5,3:ncol(N.age.SA[[i]])])))) )
  # plus group (1)
  log.change[(i+length(N.age.WC)+length(N.age.AK)),6] = 
    log( ((sum(N.age.SA[[i]][(nrow(N.age.SA[[i]])-4):nrow(N.age.SA[[i]]),ncol(N.age.SA[[i]])]) / 5) /
            (mean(rowSums(N.age.SA[[i]][(nrow(N.age.SA[[i]])-4):nrow(N.age.SA[[i]]),3:ncol(N.age.SA[[i]])]))) ) /
           ((sum(N.age.SA[[i]][1:5,ncol(N.age.SA[[i]])]) / 5) /
              (mean(rowSums(N.age.SA[[i]][1:5,3:ncol(N.age.SA[[i]])])))) )
}
for(i in 1:length(N.age.NE)){
  # youngest 0-0.25 rel age 
  start.ages = seq(3, ncol(N.age.NE[[i]]))[1:round((0.25*(ncol(N.age.NE[[i]])-2)))]
  log.change[(i+length(N.age.WC)+length(N.age.AK)+length(N.age.SA)),2] = 
    log( (mean(rowSums(N.age.NE[[i]][(nrow(N.age.NE[[i]])-4):nrow(N.age.NE[[i]]),start.ages])) /
            (mean(rowSums(N.age.NE[[i]][(nrow(N.age.NE[[i]])-4):nrow(N.age.NE[[i]]),3:ncol(N.age.NE[[i]])]))) ) /
         (mean(rowSums(N.age.NE[[i]][1:5,start.ages])) /
           (mean(rowSums(N.age.NE[[i]][1:5,3:ncol(N.age.NE[[i]])]))))
    )
  # 0.26-0.5
  second.ages = (1+tail(start.ages,1)):max((1+tail(start.ages,1)),round((0.5*(ncol(N.age.NE[[i]])-2))))
  log.change[(i+length(N.age.WC)+length(N.age.AK)+length(N.age.SA)),3] = 
    log( (mean(rowSums(N.age.NE[[i]][(nrow(N.age.NE[[i]])-4):nrow(N.age.NE[[i]]),second.ages])) /
            (mean(rowSums(N.age.NE[[i]][(nrow(N.age.NE[[i]])-4):nrow(N.age.NE[[i]]),3:ncol(N.age.NE[[i]])]))) ) /
         (mean(rowSums(N.age.NE[[i]][1:5,second.ages])) /
           (mean(rowSums(N.age.NE[[i]][1:5,3:ncol(N.age.NE[[i]])])))) 
    )
  # 0.51-0.75
  third.ages = (1+tail(second.ages,1)):max((1+tail(second.ages,1)),round((0.75*(ncol(N.age.NE[[i]])-2))))
  log.change[(i+length(N.age.WC)+length(N.age.AK)+length(N.age.SA)),4] = 
    log( (mean(rowSums(N.age.NE[[i]][(nrow(N.age.NE[[i]])-4):nrow(N.age.NE[[i]]),third.ages])) /
            (mean(rowSums(N.age.NE[[i]][(nrow(N.age.NE[[i]])-4):nrow(N.age.NE[[i]]),3:ncol(N.age.NE[[i]])]))) ) /
         (mean(rowSums(N.age.NE[[i]][1:5,third.ages])) /
           (mean(rowSums(N.age.NE[[i]][1:5,3:ncol(N.age.NE[[i]])])))) 
    )
  # 0.76-0.99
  fourth.ages = (1+tail(third.ages,1)):max((1+tail(third.ages,1)),ncol(N.age.NE[[i]])-1)
  log.change[(i+length(N.age.WC)+length(N.age.AK)+length(N.age.SA)),5] = 
    log( (mean(rowSums(N.age.NE[[i]][(nrow(N.age.NE[[i]])-4):nrow(N.age.NE[[i]]),fourth.ages])) /
            (mean(rowSums(N.age.NE[[i]][(nrow(N.age.NE[[i]])-4):nrow(N.age.NE[[i]]),3:ncol(N.age.NE[[i]])]))) ) /
         (mean(rowSums(N.age.NE[[i]][1:5,fourth.ages])) /
           (mean(rowSums(N.age.NE[[i]][1:5,3:ncol(N.age.NE[[i]])])))) 
    )
  # plus group (1)
  log.change[(i+length(N.age.WC)+length(N.age.AK)+length(N.age.SA)),6] = 
    log( ((sum(N.age.NE[[i]][(nrow(N.age.NE[[i]])-4):nrow(N.age.NE[[i]]),ncol(N.age.NE[[i]])]) / 5) /
            (mean(rowSums(N.age.NE[[i]][(nrow(N.age.NE[[i]])-4):nrow(N.age.NE[[i]]),3:ncol(N.age.NE[[i]])]))) ) /
         ((sum(N.age.NE[[i]][1:5,ncol(N.age.NE[[i]])]) / 5) /
           (mean(rowSums(N.age.NE[[i]][1:5,3:ncol(N.age.NE[[i]])])))) 
    )
}

for(i in 1:length(N.age.NA)){
  # youngest 0-0.25 rel age
  start.ages = seq(3, ncol(N.age.NA[[i]]))[1:round((0.25*(ncol(N.age.NA[[i]])-2)))]
  log.change[(i+length(N.age.WC)+length(N.age.AK)+length(N.age.SA)+length(N.age.NE)),2] = 
    log( (mean(rowSums(N.age.NA[[i]][(nrow(N.age.NA[[i]])-4):nrow(N.age.NA[[i]]),start.ages])) /
            (mean(rowSums(N.age.NA[[i]][(nrow(N.age.NA[[i]])-4):nrow(N.age.NA[[i]]),3:ncol(N.age.NA[[i]])]))) ) /
         (mean(rowSums(N.age.NA[[i]][1:5,start.ages])) /
           (mean(rowSums(N.age.NA[[i]][1:5,3:ncol(N.age.NA[[i]])])))) 
    )
  # 0.26-0.5
  second.ages = (1+tail(start.ages,1)):max((1+tail(start.ages,1)),round((0.5*(ncol(N.age.NA[[i]])-2))))
  log.change[(i+length(N.age.WC)+length(N.age.AK)+length(N.age.SA)+length(N.age.NE)),3] = 
    log( (mean(rowSums(N.age.NA[[i]][(nrow(N.age.NA[[i]])-4):nrow(N.age.NA[[i]]),second.ages])) /
            (mean(rowSums(N.age.NA[[i]][(nrow(N.age.NA[[i]])-4):nrow(N.age.NA[[i]]),3:ncol(N.age.NA[[i]])]))) ) /
         (mean(rowSums(N.age.NA[[i]][1:5,second.ages])) /
           (mean(rowSums(N.age.NA[[i]][1:5,3:ncol(N.age.NA[[i]])])))) 
    )
  # 0.51-0.75
  third.ages = (1+tail(second.ages,1)):max((1+tail(second.ages,1)),round((0.75*(ncol(N.age.NA[[i]])-2))))
  log.change[(i+length(N.age.WC)+length(N.age.AK)+length(N.age.SA)+length(N.age.NE)),4] = 
    log( (mean(rowSums(N.age.NA[[i]][(nrow(N.age.NA[[i]])-4):nrow(N.age.NA[[i]]),third.ages])) /
            (mean(rowSums(N.age.NA[[i]][(nrow(N.age.NA[[i]])-4):nrow(N.age.NA[[i]]),3:ncol(N.age.NA[[i]])]))) ) /
         (mean(rowSums(N.age.NA[[i]][1:5,third.ages])) /
           (mean(rowSums(N.age.NA[[i]][1:5,3:ncol(N.age.NA[[i]])])))) 
    )
  # 0.76-0.99
  fourth.ages = (1+tail(third.ages,1)):max((1+tail(third.ages,1)),ncol(N.age.NA[[i]])-1)
  log.change[(i+length(N.age.WC)+length(N.age.AK)+length(N.age.SA)+length(N.age.NE)),5] = 
    log( (mean(rowSums(N.age.NA[[i]][(nrow(N.age.NA[[i]])-4):nrow(N.age.NA[[i]]),fourth.ages])) /
            (mean(rowSums(N.age.NA[[i]][(nrow(N.age.NA[[i]])-4):nrow(N.age.NA[[i]]),3:ncol(N.age.NA[[i]])]))) ) /
         (mean(rowSums(N.age.NA[[i]][1:5,fourth.ages])) /
          (mean(rowSums(N.age.NA[[i]][1:5,3:ncol(N.age.NA[[i]])])))) 
  )
  # plus group (1)
  log.change[(i+length(N.age.WC)+length(N.age.AK)+length(N.age.SA)+length(N.age.NE)),6] = 
    log( ((sum(N.age.NA[[i]][(nrow(N.age.NA[[i]])-4):nrow(N.age.NA[[i]]),ncol(N.age.NA[[i]])]) / 5) /
            (mean(rowSums(N.age.NA[[i]][(nrow(N.age.NA[[i]])-4):nrow(N.age.NA[[i]]),3:ncol(N.age.NA[[i]])]))) ) /
         ((sum(N.age.NA[[i]][1:5,ncol(N.age.NA[[i]])]) / 5) /
           (mean(rowSums(N.age.NA[[i]][1:5,3:ncol(N.age.NA[[i]])])))) 
    )
}

# convert to data frame and add Region factor
log.change = as.data.frame(log.change)
log.change = mutate(log.change, Region = factor(c(rep("Alaska",length(N.age.AK)),rep("West Coast US",length(N.age.WC)),rep("South Atlantic US",length(N.age.SA)),rep("Northeast US",length(N.age.NE)),rep("North Atlantic",length(N.age.NA)))))









# SAME BUT USING M TO REPRESENT INTIAL AGE STRUCTURE
log.change.M = matrix(nrow = (length(N.age.WC)+length(N.age.NA)+length(N.age.SA)+length(N.age.NE)+length(N.age.AK)), ncol = 6, 
                    dimnames = list(c(names(N.age.AK),names(N.age.WC),names(N.age.SA),names(N.age.NE),names(N.age.NA)), 
                                    c("Region","0.00-0.25","0.26-0.50","0.51-0.75","0.76-0.99","Oldest age class")))
for(i in 1:length(N.age.AK)){
  start.ages = seq(3, ncol(N.age.AK[[i]]))[1:round((0.25*(ncol(N.age.AK[[i]])-2)))]
  log.change.M[i,2] = log( (mean(rowSums(N.age.AK[[i]][(nrow(N.age.AK[[i]])-4):nrow(N.age.AK[[i]]),start.ages])) /
                            (mean(rowSums(N.age.AK[[i]][(nrow(N.age.AK[[i]])-4):nrow(N.age.AK[[i]]),3:ncol(N.age.AK[[i]])]))) ) /
                           sum(Prop.age.init.AK[[i]][1:(round(0.25*(length(Prop.age.init.AK[[i]]))))])
  )
  # 0.26-0.5
  second.ages = (1+tail(start.ages,1)):max((1+tail(start.ages,1)),round((0.5*(ncol(N.age.AK[[i]])-2))))
  log.change.M[i,3] = log( (mean(rowSums(N.age.AK[[i]][(nrow(N.age.AK[[i]])-4):nrow(N.age.AK[[i]]),second.ages])) /
                            (mean(rowSums(N.age.AK[[i]][(nrow(N.age.AK[[i]])-4):nrow(N.age.AK[[i]]),3:ncol(N.age.AK[[i]])]))) ) /
                           sum(Prop.age.init.AK[[i]][(1+round(0.25*(length(Prop.age.init.AK[[i]])))):(round(0.5*(length(Prop.age.init.AK[[i]]))))])
  )
  # 0.51-0.75
  third.ages = (1+tail(second.ages,1)):max((1+tail(second.ages,1)),round((0.75*(ncol(N.age.AK[[i]])-2))))
  log.change.M[i,4] = log( (mean(rowSums(N.age.AK[[i]][(nrow(N.age.AK[[i]])-4):nrow(N.age.AK[[i]]),third.ages])) /
                            (mean(rowSums(N.age.AK[[i]][(nrow(N.age.AK[[i]])-4):nrow(N.age.AK[[i]]),3:ncol(N.age.AK[[i]])]))) ) /
                           sum(Prop.age.init.AK[[i]][(1+round(0.5*(length(Prop.age.init.AK[[i]])))):(round(0.75*(length(Prop.age.init.AK[[i]]))))])
  )
  # 0.76-0.99
  fourth.ages = (1+tail(third.ages,1)):max((1+tail(third.ages,1)),ncol(N.age.AK[[i]])-1)
  log.change.M[i,5] = log( (mean(rowSums(N.age.AK[[i]][(nrow(N.age.AK[[i]])-4):nrow(N.age.AK[[i]]),fourth.ages])) /
                            (mean(rowSums(N.age.AK[[i]][(nrow(N.age.AK[[i]])-4):nrow(N.age.AK[[i]]),3:ncol(N.age.AK[[i]])]))) ) /
                           sum(Prop.age.init.AK[[i]][(1+round(0.75*(length(Prop.age.init.AK[[i]])))):(length(Prop.age.init.AK[[i]]) - 1)])
  )
  # plus group (1)
  log.change.M[i,6] = log( ((sum(N.age.AK[[i]][(nrow(N.age.AK[[i]])-4):nrow(N.age.AK[[i]]),ncol(N.age.AK[[i]])]) / 5) /
                            (mean(rowSums(N.age.AK[[i]][(nrow(N.age.AK[[i]])-4):nrow(N.age.AK[[i]]),3:ncol(N.age.AK[[i]])]))) ) /
                           sum(Prop.age.init.AK[[i]][length(Prop.age.init.AK[[i]])])
  )
}

for(i in 1:length(N.age.WC)){
  # youngest 0-0.25 rel age
  start.ages = seq(3, ncol(N.age.WC[[i]]))[1:round((0.25*(ncol(N.age.WC[[i]])-2)))]
  log.change.M[(i+length(N.age.AK)),2] = 
    log( (mean(rowSums(N.age.WC[[i]][(nrow(N.age.WC[[i]])-4):nrow(N.age.WC[[i]]),start.ages])) /
            (mean(rowSums(N.age.WC[[i]][(nrow(N.age.WC[[i]])-4):nrow(N.age.WC[[i]]),3:ncol(N.age.WC[[i]])]))) ) /
           sum(Prop.age.init.WC[[i]][1:(round(0.25*(length(Prop.age.init.WC[[i]]))))]))
  # 0.26-0.5
  second.ages = (1+tail(start.ages,1)):max((1+tail(start.ages,1)),round((0.5*(ncol(N.age.WC[[i]])-2))))
  log.change.M[(i+length(N.age.AK)),3] = 
    log( (mean(rowSums(N.age.WC[[i]][(nrow(N.age.WC[[i]])-4):nrow(N.age.WC[[i]]),second.ages])) /
            (mean(rowSums(N.age.WC[[i]][(nrow(N.age.WC[[i]])-4):nrow(N.age.WC[[i]]),3:ncol(N.age.WC[[i]])]))) ) /
           sum(Prop.age.init.WC[[i]][(1+round(0.25*(length(Prop.age.init.WC[[i]])))):(round(0.5*(length(Prop.age.init.WC[[i]]))))]))
  # 0.51-0.75
  third.ages = (1+tail(second.ages,1)):max((1+tail(second.ages,1)),round((0.75*(ncol(N.age.WC[[i]])-2))))
  log.change.M[(i+length(N.age.AK)),4] = 
    log( (mean(rowSums(N.age.WC[[i]][(nrow(N.age.WC[[i]])-4):nrow(N.age.WC[[i]]),third.ages])) /
            (mean(rowSums(N.age.WC[[i]][(nrow(N.age.WC[[i]])-4):nrow(N.age.WC[[i]]),3:ncol(N.age.WC[[i]])]))) ) /
           sum(Prop.age.init.WC[[i]][(1+round(0.5*(length(Prop.age.init.WC[[i]])))):(round(0.75*(length(Prop.age.init.WC[[i]]))))]))
  # 0.76-0.99
  fourth.ages = (1+tail(third.ages,1)):max((1+tail(third.ages,1)),ncol(N.age.WC[[i]])-1)
  log.change.M[(i+length(N.age.AK)),5] = 
    log( (mean(rowSums(N.age.WC[[i]][(nrow(N.age.WC[[i]])-4):nrow(N.age.WC[[i]]),fourth.ages])) /
            (mean(rowSums(N.age.WC[[i]][(nrow(N.age.WC[[i]])-4):nrow(N.age.WC[[i]]),3:ncol(N.age.WC[[i]])]))) ) /
           sum(Prop.age.init.WC[[i]][(1+round(0.75*(length(Prop.age.init.WC[[i]])))):(length(Prop.age.init.WC[[i]]) - 1)]))
  # plus group (1)
  log.change.M[(i+length(N.age.AK)),6] = 
    log( ((sum(N.age.WC[[i]][(nrow(N.age.WC[[i]])-4):nrow(N.age.WC[[i]]),ncol(N.age.WC[[i]])]) / 5) /
            (mean(rowSums(N.age.WC[[i]][(nrow(N.age.WC[[i]])-4):nrow(N.age.WC[[i]]),3:ncol(N.age.WC[[i]])]))) ) /
           sum(Prop.age.init.WC[[i]][length(Prop.age.init.WC[[i]])]))
}

for(i in 1:length(N.age.SA)){
  # youngest 0-0.25 rel age
  start.ages = seq(3, ncol(N.age.SA[[i]]))[1:round((0.25*(ncol(N.age.SA[[i]])-2)))]
  log.change.M[(i+length(N.age.WC)+length(N.age.AK)),2] = 
    log( (mean(rowSums(N.age.SA[[i]][(nrow(N.age.SA[[i]])-4):nrow(N.age.SA[[i]]),start.ages])) /
            (mean(rowSums(N.age.SA[[i]][(nrow(N.age.SA[[i]])-4):nrow(N.age.SA[[i]]),3:ncol(N.age.SA[[i]])]))) ) /
           sum(Prop.age.init.SA[[i]][1:(round(0.25*(length(Prop.age.init.SA[[i]]))))]))
  # 0.26-0.5
  second.ages = (1+tail(start.ages,1)):max((1+tail(start.ages,1)),round((0.5*(ncol(N.age.SA[[i]])-2))))
  log.change.M[(i+length(N.age.WC)+length(N.age.AK)),3] = 
    log( (mean(rowSums(N.age.SA[[i]][(nrow(N.age.SA[[i]])-4):nrow(N.age.SA[[i]]),second.ages])) /
            (mean(rowSums(N.age.SA[[i]][(nrow(N.age.SA[[i]])-4):nrow(N.age.SA[[i]]),3:ncol(N.age.SA[[i]])]))) ) /
           sum(Prop.age.init.SA[[i]][(1+round(0.25*(length(Prop.age.init.SA[[i]])))):(round(0.5*(length(Prop.age.init.SA[[i]]))))]))
  # 0.51-0.75
  third.ages = (1+tail(second.ages,1)):max((1+tail(second.ages,1)),round((0.75*(ncol(N.age.SA[[i]])-2))))
  log.change.M[(i+length(N.age.WC)+length(N.age.AK)),4] = 
    log( (mean(rowSums(N.age.SA[[i]][(nrow(N.age.SA[[i]])-4):nrow(N.age.SA[[i]]),third.ages])) /
            (mean(rowSums(N.age.SA[[i]][(nrow(N.age.SA[[i]])-4):nrow(N.age.SA[[i]]),3:ncol(N.age.SA[[i]])]))) ) /
           sum(Prop.age.init.SA[[i]][(1+round(0.5*(length(Prop.age.init.SA[[i]])))):(round(0.75*(length(Prop.age.init.SA[[i]]))))]))
  # 0.76-0.99
  fourth.ages = (1+tail(third.ages,1)):max((1+tail(third.ages,1)),ncol(N.age.SA[[i]])-1)
  log.change.M[(i+length(N.age.WC)+length(N.age.AK)),5] = 
    log( (mean(rowSums(N.age.SA[[i]][(nrow(N.age.SA[[i]])-4):nrow(N.age.SA[[i]]),fourth.ages])) /
            (mean(rowSums(N.age.SA[[i]][(nrow(N.age.SA[[i]])-4):nrow(N.age.SA[[i]]),3:ncol(N.age.SA[[i]])]))) ) /
           sum(Prop.age.init.SA[[i]][(1+round(0.75*(length(Prop.age.init.SA[[i]])))):(length(Prop.age.init.SA[[i]]) - 1)]))
  # plus group (1)
  log.change.M[(i+length(N.age.WC)+length(N.age.AK)),6] = 
    log( ((sum(N.age.SA[[i]][(nrow(N.age.SA[[i]])-4):nrow(N.age.SA[[i]]),ncol(N.age.SA[[i]])]) / 5) /
            (mean(rowSums(N.age.SA[[i]][(nrow(N.age.SA[[i]])-4):nrow(N.age.SA[[i]]),3:ncol(N.age.SA[[i]])]))) ) /
           sum(Prop.age.init.SA[[i]][length(Prop.age.init.SA[[i]])]))
}
for(i in 1:length(N.age.NE)){
  # youngest 0-0.25 rel age 
  start.ages = seq(3, ncol(N.age.NE[[i]]))[1:round((0.25*(ncol(N.age.NE[[i]])-2)))]
  log.change.M[(i+length(N.age.WC)+length(N.age.AK)+length(N.age.SA)),2] = 
    log( (mean(rowSums(N.age.NE[[i]][(nrow(N.age.NE[[i]])-4):nrow(N.age.NE[[i]]),start.ages])) /
            (mean(rowSums(N.age.NE[[i]][(nrow(N.age.NE[[i]])-4):nrow(N.age.NE[[i]]),3:ncol(N.age.NE[[i]])]))) ) /
           sum(Prop.age.init.NE[[i]][1:(round(0.25*(length(Prop.age.init.NE[[i]]))))])
    )
  # 0.26-0.5
  second.ages = (1+tail(start.ages,1)):max((1+tail(start.ages,1)),round((0.5*(ncol(N.age.NE[[i]])-2))))
  log.change.M[(i+length(N.age.WC)+length(N.age.AK)+length(N.age.SA)),3] = 
    log( (mean(rowSums(N.age.NE[[i]][(nrow(N.age.NE[[i]])-4):nrow(N.age.NE[[i]]),second.ages])) /
            (mean(rowSums(N.age.NE[[i]][(nrow(N.age.NE[[i]])-4):nrow(N.age.NE[[i]]),3:ncol(N.age.NE[[i]])]))) ) /
           sum(Prop.age.init.NE[[i]][(1+round(0.25*(length(Prop.age.init.NE[[i]])))):(round(0.5*(length(Prop.age.init.NE[[i]]))))])
    )
  # 0.51-0.75
  third.ages = (1+tail(second.ages,1)):max((1+tail(second.ages,1)),round((0.75*(ncol(N.age.NE[[i]])-2))))
  log.change.M[(i+length(N.age.WC)+length(N.age.AK)+length(N.age.SA)),4] = 
    log( (mean(rowSums(N.age.NE[[i]][(nrow(N.age.NE[[i]])-4):nrow(N.age.NE[[i]]),third.ages])) /
            (mean(rowSums(N.age.NE[[i]][(nrow(N.age.NE[[i]])-4):nrow(N.age.NE[[i]]),3:ncol(N.age.NE[[i]])]))) ) /
           sum(Prop.age.init.NE[[i]][(1+round(0.5*(length(Prop.age.init.NE[[i]])))):(round(0.75*(length(Prop.age.init.NE[[i]]))))])
    )
  # 0.76-0.99
  fourth.ages = (1+tail(third.ages,1)):max((1+tail(third.ages,1)),ncol(N.age.NE[[i]])-1)
  log.change.M[(i+length(N.age.WC)+length(N.age.AK)+length(N.age.SA)),5] = 
    log( (mean(rowSums(N.age.NE[[i]][(nrow(N.age.NE[[i]])-4):nrow(N.age.NE[[i]]),fourth.ages])) /
            (mean(rowSums(N.age.NE[[i]][(nrow(N.age.NE[[i]])-4):nrow(N.age.NE[[i]]),3:ncol(N.age.NE[[i]])]))) ) /
           sum(Prop.age.init.NE[[i]][(1+round(0.75*(length(Prop.age.init.NE[[i]])))):(length(Prop.age.init.NE[[i]]) - 1)])
    )
  # plus group (1)
  log.change.M[(i+length(N.age.WC)+length(N.age.AK)+length(N.age.SA)),6] = 
    log( ((sum(N.age.NE[[i]][(nrow(N.age.NE[[i]])-4):nrow(N.age.NE[[i]]),ncol(N.age.NE[[i]])]) / 5) /
            (mean(rowSums(N.age.NE[[i]][(nrow(N.age.NE[[i]])-4):nrow(N.age.NE[[i]]),3:ncol(N.age.NE[[i]])]))) ) /
           sum(Prop.age.init.NE[[i]][length(Prop.age.init.NE[[i]])])
    )
}

for(i in 1:length(N.age.NA)){
  # youngest 0-0.25 rel age
  start.ages = seq(3, ncol(N.age.NA[[i]]))[1:round((0.25*(ncol(N.age.NA[[i]])-2)))]
  log.change.M[(i+length(N.age.WC)+length(N.age.AK)+length(N.age.SA)+length(N.age.NE)),2] = 
    log( (mean(rowSums(N.age.NA[[i]][(nrow(N.age.NA[[i]])-4):nrow(N.age.NA[[i]]),start.ages])) /
            (mean(rowSums(N.age.NA[[i]][(nrow(N.age.NA[[i]])-4):nrow(N.age.NA[[i]]),3:ncol(N.age.NA[[i]])]))) ) /
           sum(Prop.age.init.NA[[i]][1:(round(0.25*(length(Prop.age.init.NA[[i]]))))])
    )
  # 0.26-0.5
  second.ages = (1+tail(start.ages,1)):max((1+tail(start.ages,1)),round((0.5*(ncol(N.age.NA[[i]])-2))))
  log.change.M[(i+length(N.age.WC)+length(N.age.AK)+length(N.age.SA)+length(N.age.NE)),3] = 
    log( (mean(rowSums(N.age.NA[[i]][(nrow(N.age.NA[[i]])-4):nrow(N.age.NA[[i]]),second.ages])) /
            (mean(rowSums(N.age.NA[[i]][(nrow(N.age.NA[[i]])-4):nrow(N.age.NA[[i]]),3:ncol(N.age.NA[[i]])]))) ) /
           sum(Prop.age.init.NA[[i]][(1+round(0.25*(length(Prop.age.init.NA[[i]])))):(round(0.5*(length(Prop.age.init.NA[[i]]))))])
    )
  # 0.51-0.75
  third.ages = (1+tail(second.ages,1)):max((1+tail(second.ages,1)),round((0.75*(ncol(N.age.NA[[i]])-2))))
  log.change.M[(i+length(N.age.WC)+length(N.age.AK)+length(N.age.SA)+length(N.age.NE)),4] = 
    log( (mean(rowSums(N.age.NA[[i]][(nrow(N.age.NA[[i]])-4):nrow(N.age.NA[[i]]),third.ages])) /
            (mean(rowSums(N.age.NA[[i]][(nrow(N.age.NA[[i]])-4):nrow(N.age.NA[[i]]),3:ncol(N.age.NA[[i]])]))) ) /
           sum(Prop.age.init.NA[[i]][(1+round(0.5*(length(Prop.age.init.NA[[i]])))):(round(0.75*(length(Prop.age.init.NA[[i]]))))])
    )
  # 0.76-0.99
  fourth.ages = (1+tail(third.ages,1)):max((1+tail(third.ages,1)),ncol(N.age.NA[[i]])-1)
  log.change.M[(i+length(N.age.WC)+length(N.age.AK)+length(N.age.SA)+length(N.age.NE)),5] = 
    log( (mean(rowSums(N.age.NA[[i]][(nrow(N.age.NA[[i]])-4):nrow(N.age.NA[[i]]),fourth.ages])) /
            (mean(rowSums(N.age.NA[[i]][(nrow(N.age.NA[[i]])-4):nrow(N.age.NA[[i]]),3:ncol(N.age.NA[[i]])]))) ) /
           sum(Prop.age.init.NA[[i]][(1+round(0.75*(length(Prop.age.init.NA[[i]])))):(length(Prop.age.init.NA[[i]]) - 1)])
    )
  # plus group (1)
  log.change.M[(i+length(N.age.WC)+length(N.age.AK)+length(N.age.SA)+length(N.age.NE)),6] = 
    log( ((sum(N.age.NA[[i]][(nrow(N.age.NA[[i]])-4):nrow(N.age.NA[[i]]),ncol(N.age.NA[[i]])]) / 5) /
            (mean(rowSums(N.age.NA[[i]][(nrow(N.age.NA[[i]])-4):nrow(N.age.NA[[i]]),3:ncol(N.age.NA[[i]])]))) ) /
           sum(Prop.age.init.NA[[i]][length(Prop.age.init.NA[[i]])])
    )
}


# convert to data frame and add Region factor
log.change.M = as.data.frame(log.change.M)
log.change.M = mutate(log.change.M, Region = factor(c(rep("Alaska",length(N.age.AK)),rep("West Coast US",length(N.age.WC)),rep("South Atlantic US",length(N.age.SA)),rep("Northeast US",length(N.age.NE)),rep("North Atlantic",length(N.age.NA)))))







# plot one age group at a time, showing all data and Medians, adjusting x-axis to get groupings as desired
transblack = rgb(red=0, green=0, blue=0, alpha=0.4)
transblue = rgb(red=0.2, green=0.2, blue=1.0, alpha=0.4)
transgreen = rgb(red=0, green=0.9, blue=0, alpha=0.4)
transorange = rgb(red=1.0, green=0.4, blue=0, alpha=0.4)
transbrown = rgb(red=0.4, green=0.15, blue=0.15, alpha=0.4)


pdf("stripchart_takehome_5region_byRelAge_bothMethods_Meds_trunc.pdf", width = 8, height = 8.5)
par(mfrow = c(2,1))
par(mar = c(1, 4, 0, 0), oma = c(3.2, 0, 2, 0.5))

# Adjust outliers to fit on plot, designate <5 category at 5 using axis()
log.change.adj = log.change[,-1]
  log.change.adj[log.change.adj >= log(5)] = log(5)
log.change.adj = cbind(log.change[,1], log.change.adj)
  names(log.change.adj)[1] = "Region"

stripchart(exp(log.change.adj[,2]) ~ log.change.adj$Region, vertical = TRUE, method = "jitter", jitter = 0.25, yaxs="i", xpd = NA , xaxt='n', yaxt='n',
           col = c(transblack, transorange, transgreen, transblue,transbrown), pch = 16, xaxt = "n", ylab = "Ratio of prop. at age (end:begin)",
           ylim = c(0,5), xlim = c(0,36), at = c(0:4), cex = 0.7)
  meds <- tapply(exp(log.change[,2]),log.change$Region,median) #get the medians in factor level order - same order as stripchart 
  segments((0:4)-0.45, meds, (0:4)+0.45, col = "black", lwd=2.5) 
stripchart(exp(log.change.adj[,3]) ~ log.change.adj$Region, vertical = TRUE, method = "jitter", jitter = 0.25, axes = FALSE, xpd = NA,
           col = c(transblack, transorange, transgreen, transblue,transbrown), pch = 16, add = TRUE, at = c(8:12), cex = 0.7) 
  meds <- tapply(exp(log.change[,3]),log.change$Region,median) 
  segments((8:12)-0.45, meds, (8:12)+0.45, col = "black", lwd=2.5) 
stripchart(exp(log.change.adj[,4]) ~ log.change.adj$Region, vertical = TRUE, method = "jitter", jitter = 0.25, axes = FALSE, xpd = NA,
           col = c(transblack, transorange, transgreen, transblue,transbrown), pch = 16, add = TRUE, at = c(16:20), cex = 0.7) 
  meds <- tapply(exp(log.change[,4]),log.change$Region,median) 
  segments((16:20)-0.45, meds, (16:20)+0.45, col = "black", lwd=2.5) 
stripchart(exp(log.change.adj[,5]) ~ log.change.adj$Region, vertical = TRUE, method = "jitter", jitter = 0.25, axes = FALSE, xpd = NA,
           col = c(transblack, transorange, transgreen, transblue,transbrown), pch = 16, add = TRUE, at = c(24:28), cex = 0.7) 
  meds <- tapply(exp(log.change[,5]),log.change$Region,median) 
  segments((24:28)-0.45, meds, (24:28)+0.45, col = "black", lwd=2.5) 
stripchart(exp(log.change.adj[,6]) ~ log.change.adj$Region, vertical = TRUE, method = "jitter", jitter = 0.25, axes = FALSE, xpd = NA,
           col = c(transblack, transorange, transgreen, transblue,transbrown), pch = 16, add = TRUE, at = c(32:36), cex = 0.7) 
  meds <- tapply(exp(log.change[,6]),log.change$Region,median) 
  segments((32:36)-0.45, meds, (32:36)+0.45, col = "black", lwd=2.5) 
abline(h=1, lty = "dotted")
# add y-axis break and custom labels
axis(2, at=c(0:5), labels=c(0:4,">5"), cex.axis = 0.9)
mtext("A", side = 3, line = -1.3, adj = 0.02)

stripchart(exp(log.change.M[,2]) ~ log.change.M$Region, vertical = TRUE, method = "jitter", jitter = 0.25, yaxs="i", xpd = NA, cex.axis = 0.9,
           col = c(transblack, transorange, transgreen, transblue,transbrown), pch = 16, xaxt = "n", ylab = "Ratio of prop. at age (end:unfished)",
           ylim = c(0,5), xlim = c(0,36), at = c(0:4), cex = 0.7)
  meds <- tapply(exp(log.change.M[,2]),log.change$Region,median)
  segments((0:4)-0.45, meds, (0:4)+0.45, col = "black", lwd=2.5) 
stripchart(exp(log.change.M[,3]) ~ log.change.M$Region, vertical = TRUE, method = "jitter", jitter = 0.25, axes = FALSE, xpd = NA,
           col = c(transblack, transorange, transgreen, transblue,transbrown), pch = 16, add = TRUE, at = c(8:12), cex = 0.7) 
  meds <- tapply(exp(log.change.M[,3]),log.change$Region,median) 
  segments((8:12)-0.45, meds, (8:12)+0.45, col = "black", lwd=2.5) 
stripchart(exp(log.change.M[,4]) ~ log.change.M$Region, vertical = TRUE, method = "jitter", jitter = 0.25, axes = FALSE, xpd = NA,
           col = c(transblack, transorange, transgreen, transblue,transbrown), pch = 16, add = TRUE, at = c(16:20), cex = 0.7) 
  meds <- tapply(exp(log.change.M[,4]),log.change$Region,median) 
  segments((16:20)-0.45, meds, (16:20)+0.45, col = "black", lwd=2.5) 
stripchart(exp(log.change.M[,5]) ~ log.change.M$Region, vertical = TRUE, method = "jitter", jitter = 0.25, axes = FALSE, xpd = NA,
           col = c(transblack, transorange, transgreen, transblue,transbrown), pch = 16, add = TRUE, at = c(24:28), cex = 0.7) 
  meds <- tapply(exp(log.change.M[,5]),log.change$Region,median) 
  segments((24:28)-0.45, meds, (24:28)+0.45, col = "black", lwd=2.5) 
stripchart(exp(log.change.M[,6]) ~ log.change.M$Region, vertical = TRUE, method = "jitter", jitter = 0.25, axes = FALSE, xpd = NA,
           col = c(transblack, transorange, transgreen, transblue,transbrown), pch = 16, add = TRUE, at = c(32:36), cex = 0.7) 
  meds <- tapply(exp(log.change.M[,6]),log.change$Region,median) 
  segments((32:36)-0.45, meds, (32:36)+0.45, col = "black", lwd=2.5) 
abline(h=1, lty = "dotted")
mtext("B", side = 3, line = -1.3, adj = 0.02)

axis(1, at = c(2, 10, 18, 26, 34), labels = colnames(log.change)[-1], cex.axis = 0.9)
mtext("Relative age range", side = 1, cex = 1.1, line = 3)
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend("top", legend = c("Alaska","ICES","NE US","SE US","WC US"), horiz = TRUE, xpd = TRUE, inset = c(0,0.01),
       y.intersp = 0.1, col = c(transblack, transorange, transgreen, transblue,transbrown), pch = 16, cex = 1.2, bty = "n")

dev.off()





# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# STATISTICAL TESTING

# Test for difference in proportion within the plus group between the average 5 years at the end of the time period and those inferred at beginning from M-at-age (log(prop.T/prop.0))
# for each stock (H_a = begin does not equal end, inferring direction of difference based on values themselves), 
# then adjust the p-values to account for multiple comparisons.

# NEW ADJUSTMENT: given that we don't have true unfished abundance at age for some regions, and are using M to reconstruct the proportions at age FOR ALL REGIONS,
#                 we will multiply the unfished proportions at age by the average total population size from the earliest 5 years available to get sample sizes needed for test

# pre-allocate matrix to insert the region, proportion of change, pearson chi-square statistics and p-values of each test
test.stats.M = matrix(nrow = (length(N.age.WC)+length(N.age.NA)+length(N.age.SA)+length(N.age.NE)+length(N.age.AK)), ncol = 6, 
                    dimnames = list(c(names(N.age.WC),names(N.age.NA),names(N.age.SA),names(N.age.NE),names(N.age.AK)), 
                                    c("Region","LogRatio","PropChange","Statistic","P-value","AbsChange")))

# Compile test statistics and compute the log-ratio of proportion in the plus group between begin and end
for(i in 1:length(N.age.WC)){
  # multiply by units of 1000 individuals to get matrix of successes and failures 
  # (numbers in plus group and numbers in all other age groups, respectively) at before and after fishing
  conting.mat = 1000 * rbind( as.numeric( c( (sum(N.age.WC[[i]][1:5,3:ncol(N.age.WC[[i]])]) / 5) * Prop.age.init.WC[[i]][length(Prop.age.init.WC[[i]])], 
                                             (sum(N.age.WC[[i]][1:5,3:ncol(N.age.WC[[i]])]) / 5) * (1 - Prop.age.init.WC[[i]][length(Prop.age.init.WC[[i]])]) )), 
                              as.numeric( c(sum(N.age.WC[[i]][(nrow(N.age.WC[[i]])-4):nrow(N.age.WC[[i]]),ncol(N.age.WC[[i]])]) / 5, 
                                                 mean(rowSums(N.age.WC[[i]][(nrow(N.age.WC[[i]])-4):nrow(N.age.WC[[i]]),3:(ncol(N.age.WC[[i]])-1)])))) )
  test.stats.M[i,2] = log(prop.test(conting.mat, correct = FALSE)$estimate[2] / prop.test(conting.mat, correct = FALSE)$estimate[1])
  test.stats.M[i,3] = (prop.test(conting.mat, correct = FALSE)$estimate[2] - prop.test(conting.mat, correct = FALSE)$estimate[1]) / prop.test(conting.mat, correct = FALSE)$estimate[1]
  test.stats.M[i,4] = prop.test(conting.mat, correct = FALSE)$statistic
  test.stats.M[i,5] = prop.test(conting.mat, correct = FALSE)$p.value
  test.stats.M[i,6] = (conting.mat[2,1] - conting.mat[1,1]) / conting.mat[1,1]
}
for(i in 1:length(N.age.NA)){
  conting.mat = 1000 * rbind( as.numeric( c( (sum(N.age.NA[[i]][1:5,3:ncol(N.age.NA[[i]])]) / 5) * Prop.age.init.NA[[i]][length(Prop.age.init.NA[[i]])], 
                                             (sum(N.age.NA[[i]][1:5,3:ncol(N.age.NA[[i]])]) / 5) * (1 - Prop.age.init.NA[[i]][length(Prop.age.init.NA[[i]])]) )), 
                              as.numeric( c(sum(N.age.NA[[i]][(nrow(N.age.NA[[i]])-4):nrow(N.age.NA[[i]]),ncol(N.age.NA[[i]])]) / 5, 
                                            mean(rowSums(N.age.NA[[i]][(nrow(N.age.NA[[i]])-4):nrow(N.age.NA[[i]]),3:(ncol(N.age.NA[[i]])-1)])))) )
  test.stats.M[(i+length(N.age.WC)),2] = log(prop.test(conting.mat, correct = FALSE)$estimate[2] / prop.test(conting.mat, correct = FALSE)$estimate[1])
  test.stats.M[(i+length(N.age.WC)),3] = (prop.test(conting.mat, correct = FALSE)$estimate[2] - prop.test(conting.mat, correct = FALSE)$estimate[1]) / prop.test(conting.mat, correct = FALSE)$estimate[1]
  test.stats.M[(i+length(N.age.WC)),4] = prop.test(conting.mat, correct = FALSE)$statistic
  test.stats.M[(i+length(N.age.WC)),5] = prop.test(conting.mat, correct = FALSE)$p.value
  test.stats.M[(i+length(N.age.WC)),6] = (conting.mat[2,1] - conting.mat[1,1]) / conting.mat[1,1]
}
for(i in 1:length(N.age.SA)){
  conting.mat = 1000 * rbind( as.numeric( c( (sum(N.age.SA[[i]][1:5,3:ncol(N.age.SA[[i]])]) / 5) * Prop.age.init.SA[[i]][length(Prop.age.init.SA[[i]])], 
                                             (sum(N.age.SA[[i]][1:5,3:ncol(N.age.SA[[i]])]) / 5) * (1 - Prop.age.init.SA[[i]][length(Prop.age.init.SA[[i]])]) )), 
                              as.numeric( c(sum(N.age.SA[[i]][(nrow(N.age.SA[[i]])-4):nrow(N.age.SA[[i]]),ncol(N.age.SA[[i]])]) / 5, 
                                            mean(rowSums(N.age.SA[[i]][(nrow(N.age.SA[[i]])-4):nrow(N.age.SA[[i]]),3:(ncol(N.age.SA[[i]])-1)])))) )
  test.stats.M[(i+length(N.age.WC)+length(N.age.NA)),2] = log(prop.test(conting.mat, correct = FALSE)$estimate[2] / prop.test(conting.mat, correct = FALSE)$estimate[1])
  test.stats.M[(i+length(N.age.WC)+length(N.age.NA)),3] = (prop.test(conting.mat, correct = FALSE)$estimate[2] - prop.test(conting.mat, correct = FALSE)$estimate[1]) / prop.test(conting.mat, correct = FALSE)$estimate[1]
  test.stats.M[(i+length(N.age.WC)+length(N.age.NA)),4] = prop.test(conting.mat, correct = FALSE)$statistic
  test.stats.M[(i+length(N.age.WC)+length(N.age.NA)),5] = prop.test(conting.mat, correct = FALSE)$p.value
  test.stats.M[(i+length(N.age.WC)+length(N.age.NA)),6] = (conting.mat[2,1] - conting.mat[1,1]) / conting.mat[1,1]
}
for(i in 1:length(N.age.NE)){
  conting.mat = 1000 * rbind( as.numeric( c( (sum(N.age.NE[[i]][1:5,3:ncol(N.age.NE[[i]])]) / 5) * Prop.age.init.NE[[i]][length(Prop.age.init.NE[[i]])], 
                                             (sum(N.age.NE[[i]][1:5,3:ncol(N.age.NE[[i]])]) / 5) * (1 - Prop.age.init.NE[[i]][length(Prop.age.init.NE[[i]])]) )), 
                              as.numeric( c(sum(N.age.NE[[i]][(nrow(N.age.NE[[i]])-4):nrow(N.age.NE[[i]]),ncol(N.age.NE[[i]])]) / 5, 
                                            mean(rowSums(N.age.NE[[i]][(nrow(N.age.NE[[i]])-4):nrow(N.age.NE[[i]]),3:(ncol(N.age.NE[[i]])-1)])))) )
  test.stats.M[(i+length(N.age.WC)+length(N.age.NA)+length(N.age.SA)),2] = log(prop.test(conting.mat, correct = FALSE)$estimate[2] / prop.test(conting.mat, correct = FALSE)$estimate[1])
  test.stats.M[(i+length(N.age.WC)+length(N.age.NA)+length(N.age.SA)),3] = (prop.test(conting.mat, correct = FALSE)$estimate[2] - prop.test(conting.mat, correct = FALSE)$estimate[1]) / prop.test(conting.mat, correct = FALSE)$estimate[1]
  test.stats.M[(i+length(N.age.WC)+length(N.age.NA)+length(N.age.SA)),4] = prop.test(conting.mat, correct = FALSE)$statistic
  test.stats.M[(i+length(N.age.WC)+length(N.age.NA)+length(N.age.SA)),5] = prop.test(conting.mat, correct = FALSE)$p.value
  test.stats.M[(i+length(N.age.WC)+length(N.age.NA)+length(N.age.SA)),6] = (conting.mat[2,1] - conting.mat[1,1]) / conting.mat[1,1]
}
for(i in 1:length(N.age.AK)){
  conting.mat = 1000 * rbind( as.numeric( c( (sum(N.age.AK[[i]][1:5,3:ncol(N.age.AK[[i]])]) / 5) * Prop.age.init.AK[[i]][length(Prop.age.init.AK[[i]])], 
                                             (sum(N.age.AK[[i]][1:5,3:ncol(N.age.AK[[i]])]) / 5) * (1 - Prop.age.init.AK[[i]][length(Prop.age.init.AK[[i]])]) )), 
                              as.numeric( c(sum(N.age.AK[[i]][(nrow(N.age.AK[[i]])-4):nrow(N.age.AK[[i]]),ncol(N.age.AK[[i]])]) / 5, 
                                            mean(rowSums(N.age.AK[[i]][(nrow(N.age.AK[[i]])-4):nrow(N.age.AK[[i]]),3:(ncol(N.age.AK[[i]])-1)])))) )
  test.stats.M[(i+length(N.age.WC)+length(N.age.NA)+length(N.age.SA)+length(N.age.NE)),2] = log(prop.test(conting.mat, correct = FALSE)$estimate[2] / prop.test(conting.mat, correct = FALSE)$estimate[1])
  test.stats.M[(i+length(N.age.WC)+length(N.age.NA)+length(N.age.SA)+length(N.age.NE)),3] = (prop.test(conting.mat, correct = FALSE)$estimate[2] - prop.test(conting.mat, correct = FALSE)$estimate[1]) / prop.test(conting.mat, correct = FALSE)$estimate[1]
  test.stats.M[(i+length(N.age.WC)+length(N.age.NA)+length(N.age.SA)+length(N.age.NE)),4] = prop.test(conting.mat, correct = FALSE)$statistic
  test.stats.M[(i+length(N.age.WC)+length(N.age.NA)+length(N.age.SA)+length(N.age.NE)),5] = prop.test(conting.mat, correct = FALSE)$p.value
  test.stats.M[(i+length(N.age.WC)+length(N.age.NA)+length(N.age.SA)+length(N.age.NE)),6] = (conting.mat[2,1] - conting.mat[1,1]) / conting.mat[1,1]
}


# convert to data frame and add Region factor
test.stats.M = as.data.frame(test.stats.M)
test.stats.M = mutate(test.stats.M, Region = factor(c(rep("WC",length(N.age.WC)),rep("NA",length(N.age.NA)),rep("SA",length(N.age.SA)),rep("NE",length(N.age.NE)),rep("AK",length(N.age.AK)))))


# RESULTS
# print exp geometric mean and SE for ratio of proportions in plus group over time
summarise(group_by(test.stats.M, Region), exp(mean(LogRatio)))
# quantiles
summarise(group_by(test.stats.M, Region), exp(quantile(LogRatio, probs = 0.025)))
summarise(group_by(test.stats.M, Region), exp(quantile(LogRatio, probs = 0.975)))


# TESTING FOR DIFFERENCE AMONG REGIONS: ANOVA
fit = aov(LogRatio ~ Region, data = test.stats.M)
plot(fit) # diagnostic plots
summary(fit) # ANOVA table
# post-hoc testing
TukeyHSD(fit)


# COMPUTE STATISTIC OF WHAT PERCENTAGE OF STOCKS HAD 90 PERCENTAGE DECREASE (REL CHANGE) IN PROPORTIONAL ABUNDANCE OF THE PLUS GROUP
#hist(test.stats.M[,3], breaks = 20)
sum(test.stats.M[,3] < -0.9) / length(test.stats.M[,3])
# check to see if same result using ratios
sum(exp(log.change.M[,6]) < 0.1) / nrow(log.change.M) 
# SAME AS ABOVE BUT FOR PERCENTAGE DECREASE IN ABSOLUTE ABUNDANCE
#hist(test.stats.M[,6], breaks = 100, xlim = c(-1,1))
sum(test.stats.M[,6] < -0.9) / length(test.stats.M[,6])

# SAME AS ABOVE BUT FOR PERCENTAGE THAT ACTUALLY INCREASED BY THRESHOLD AMOUNT
#hist(test.stats.M[,3], breaks = 20)
sum(test.stats.M[,3] > 0.01) / length(test.stats.M[,3])
# ABSOLUTE ABUNDANCE
#hist(test.stats.M[,6], breaks = 100, xlim = c(-1,1))
sum(test.stats.M[,6] > 0.01) / length(test.stats.M[,6])







# Same as above but with empirical data for begin abundance 
# pre-allocate matrix to insert the region, proportion of change, pearson chi-square statistics and p-values of each test
test.stats = matrix(nrow = (length(N.age.WC)+length(N.age.NA)+length(N.age.SA)+length(N.age.NE)+length(N.age.AK)), ncol = 6, 
                    dimnames = list(c(names(N.age.WC),names(N.age.NA),names(N.age.SA),names(N.age.NE),names(N.age.AK)), 
                                    c("Region","LogRatio","PropChange","Statistic","P-value","AbsChange")))

# Compile test statistics and compute the log-ratio of proportion in the plus group between begin and end
for(i in 1:length(N.age.WC)){
  # multiply by units of 1000 individuals to get matrix of successes and failures 
  # (numbers in plus group and numbers in all other age groups, respectively) at before and after fishing
  conting.mat = 1000 * rbind( as.numeric( c(sum(N.age.WC[[i]][1:5,ncol(N.age.WC[[i]])]) / 5, 
                                            mean(rowSums(N.age.WC[[i]][1:5,3:(ncol(N.age.WC[[i]])-1)])) )), 
                              as.numeric( c(sum(N.age.WC[[i]][(nrow(N.age.WC[[i]])-4):nrow(N.age.WC[[i]]),ncol(N.age.WC[[i]])]) / 5, 
                                            mean(rowSums(N.age.WC[[i]][(nrow(N.age.WC[[i]])-4):nrow(N.age.WC[[i]]),3:(ncol(N.age.WC[[i]])-1)])))) )
  test.stats[i,2] = log(prop.test(conting.mat, correct = FALSE)$estimate[2] / prop.test(conting.mat, correct = FALSE)$estimate[1])
  test.stats[i,3] = (prop.test(conting.mat, correct = FALSE)$estimate[2] - prop.test(conting.mat, correct = FALSE)$estimate[1]) / prop.test(conting.mat, correct = FALSE)$estimate[1]
  test.stats[i,4] = prop.test(conting.mat, correct = FALSE)$statistic
  test.stats[i,5] = prop.test(conting.mat, correct = FALSE)$p.value
  test.stats[i,6] = (conting.mat[2,1] - conting.mat[1,1]) / conting.mat[1,1]
}
for(i in 1:length(N.age.NA)){
  conting.mat = 1000 * rbind( as.numeric( c(sum(N.age.NA[[i]][1:5,ncol(N.age.NA[[i]])]) / 5, 
                                mean(rowSums(N.age.NA[[i]][1:5,3:(ncol(N.age.NA[[i]])-1)])) )), 
                              as.numeric( c(sum(N.age.NA[[i]][(nrow(N.age.NA[[i]])-4):nrow(N.age.NA[[i]]),ncol(N.age.NA[[i]])]) / 5, 
                                            mean(rowSums(N.age.NA[[i]][(nrow(N.age.NA[[i]])-4):nrow(N.age.NA[[i]]),3:(ncol(N.age.NA[[i]])-1)])))) )
  test.stats[(i+length(N.age.WC)),2] = log(prop.test(conting.mat, correct = FALSE)$estimate[2] / prop.test(conting.mat, correct = FALSE)$estimate[1])
  test.stats[(i+length(N.age.WC)),3] = (prop.test(conting.mat, correct = FALSE)$estimate[2] - prop.test(conting.mat, correct = FALSE)$estimate[1]) / prop.test(conting.mat, correct = FALSE)$estimate[1]
  test.stats[(i+length(N.age.WC)),4] = prop.test(conting.mat, correct = FALSE)$statistic
  test.stats[(i+length(N.age.WC)),5] = prop.test(conting.mat, correct = FALSE)$p.value
  test.stats[(i+length(N.age.WC)),6] = (conting.mat[2,1] - conting.mat[1,1]) / conting.mat[1,1]
}
for(i in 1:length(N.age.SA)){
  conting.mat = 1000 * rbind( as.numeric( c(sum(N.age.SA[[i]][1:5,ncol(N.age.SA[[i]])]) / 5, 
                                            mean(rowSums(N.age.SA[[i]][1:5,3:(ncol(N.age.SA[[i]])-1)])) )), 
                              as.numeric( c(sum(N.age.SA[[i]][(nrow(N.age.SA[[i]])-4):nrow(N.age.SA[[i]]),ncol(N.age.SA[[i]])]) / 5, 
                                            mean(rowSums(N.age.SA[[i]][(nrow(N.age.SA[[i]])-4):nrow(N.age.SA[[i]]),3:(ncol(N.age.SA[[i]])-1)])))) )
  test.stats[(i+length(N.age.WC)+length(N.age.NA)),2] = log(prop.test(conting.mat, correct = FALSE)$estimate[2] / prop.test(conting.mat, correct = FALSE)$estimate[1])
  test.stats[(i+length(N.age.WC)+length(N.age.NA)),3] = (prop.test(conting.mat, correct = FALSE)$estimate[2] - prop.test(conting.mat, correct = FALSE)$estimate[1]) / prop.test(conting.mat, correct = FALSE)$estimate[1]
  test.stats[(i+length(N.age.WC)+length(N.age.NA)),4] = prop.test(conting.mat, correct = FALSE)$statistic
  test.stats[(i+length(N.age.WC)+length(N.age.NA)),5] = prop.test(conting.mat, correct = FALSE)$p.value
  test.stats[(i+length(N.age.WC)+length(N.age.NA)),6] = (conting.mat[2,1] - conting.mat[1,1]) / conting.mat[1,1]
}
for(i in 1:length(N.age.NE)){
  conting.mat = 1000 * rbind( as.numeric( c(sum(N.age.NE[[i]][1:5,ncol(N.age.NE[[i]])]) / 5, 
                                            mean(rowSums(N.age.NE[[i]][1:5,3:(ncol(N.age.NE[[i]])-1)])) )), 
                              as.numeric( c(sum(N.age.NE[[i]][(nrow(N.age.NE[[i]])-4):nrow(N.age.NE[[i]]),ncol(N.age.NE[[i]])]) / 5, 
                                            mean(rowSums(N.age.NE[[i]][(nrow(N.age.NE[[i]])-4):nrow(N.age.NE[[i]]),3:(ncol(N.age.NE[[i]])-1)])))) )
  test.stats[(i+length(N.age.WC)+length(N.age.NA)+length(N.age.SA)),2] = log(prop.test(conting.mat, correct = FALSE)$estimate[2] / prop.test(conting.mat, correct = FALSE)$estimate[1])
  test.stats[(i+length(N.age.WC)+length(N.age.NA)+length(N.age.SA)),3] = (prop.test(conting.mat, correct = FALSE)$estimate[2] - prop.test(conting.mat, correct = FALSE)$estimate[1]) / prop.test(conting.mat, correct = FALSE)$estimate[1]
  test.stats[(i+length(N.age.WC)+length(N.age.NA)+length(N.age.SA)),4] = prop.test(conting.mat, correct = FALSE)$statistic
  test.stats[(i+length(N.age.WC)+length(N.age.NA)+length(N.age.SA)),5] = prop.test(conting.mat, correct = FALSE)$p.value
  test.stats[(i+length(N.age.WC)+length(N.age.NA)+length(N.age.SA)),6] = (conting.mat[2,1] - conting.mat[1,1]) / conting.mat[1,1]
}
for(i in 1:length(N.age.AK)){
  conting.mat = 1000 * rbind( as.numeric( c(sum(N.age.AK[[i]][1:5,ncol(N.age.AK[[i]])]) / 5, 
                                            mean(rowSums(N.age.AK[[i]][1:5,3:(ncol(N.age.AK[[i]])-1)])) )),  
                              as.numeric( c(sum(N.age.AK[[i]][(nrow(N.age.AK[[i]])-4):nrow(N.age.AK[[i]]),ncol(N.age.AK[[i]])]) / 5, 
                                            mean(rowSums(N.age.AK[[i]][(nrow(N.age.AK[[i]])-4):nrow(N.age.AK[[i]]),3:(ncol(N.age.AK[[i]])-1)])))) )
  test.stats[(i+length(N.age.WC)+length(N.age.NA)+length(N.age.SA)+length(N.age.NE)),2] = log(prop.test(conting.mat, correct = FALSE)$estimate[2] / prop.test(conting.mat, correct = FALSE)$estimate[1])
  test.stats[(i+length(N.age.WC)+length(N.age.NA)+length(N.age.SA)+length(N.age.NE)),3] = (prop.test(conting.mat, correct = FALSE)$estimate[2] - prop.test(conting.mat, correct = FALSE)$estimate[1]) / prop.test(conting.mat, correct = FALSE)$estimate[1]
  test.stats[(i+length(N.age.WC)+length(N.age.NA)+length(N.age.SA)+length(N.age.NE)),4] = prop.test(conting.mat, correct = FALSE)$statistic
  test.stats[(i+length(N.age.WC)+length(N.age.NA)+length(N.age.SA)+length(N.age.NE)),5] = prop.test(conting.mat, correct = FALSE)$p.value
  test.stats[(i+length(N.age.WC)+length(N.age.NA)+length(N.age.SA)+length(N.age.NE)),6] = (conting.mat[2,1] - conting.mat[1,1]) / conting.mat[1,1]
}

# convert to data frame and add Region factor
test.stats = as.data.frame(test.stats)
test.stats = mutate(test.stats, Region = factor(c(rep("WC",length(N.age.WC)),rep("NA",length(N.age.NA)),rep("SA",length(N.age.SA)),rep("NE",length(N.age.NE)),rep("AK",length(N.age.AK)))))


# TESTING FOR DIFFERENCE AMONG REGIONS: ANOVA
fit = aov(LogRatio ~ Region, data = test.stats)
#plot(fit) # diagnostic plots
summary(fit) # ANOVA table
# post-hoc testing
TukeyHSD(fit)


# COMPUTE STATISTIC OF WHAT PERCENTAGE OF STOCKS HAD 90 PERCENTAGE DECREASE (REL CHANGE) IN PROPORTIONAL ABUNDANCE OF THE PLUS GROUP
#hist(test.stats[,3], breaks = 20)
sum(test.stats[,3] < -0.9) / length(test.stats[,3])
# check to see if same result using ratios
sum(exp(log.change[,6]) < 0.1) / nrow(log.change) 
# SAME AS ABOVE BUT FOR PERCENTAGE DECREASE IN ABSOLUTE ABUNDANCE
#hist(test.stats[,6], breaks = 100, xlim = c(-1,1))
sum(test.stats[,6] < -0.9) / length(test.stats[,6])





# Plot changes in plus group proportions, with violin plots
pdf("stripchart_takehome_5region_combined_plusgroup_4panel_vio.pdf", width = 4.48819, height = 5)
par(mfcol = c(2,2), mar = c(3.5, 2.7, 0, 0), oma = c(0, 0, 0.5, 0.5), mgp = c(1.5, 0.5, 0))

# Histograms of change in plus group proportion over all stocks
# adjust high numbers to fit on plot for > 2 category in begin/end estimate
ratio.trunc = replace(exp(test.stats[,2]), exp(test.stats[,2]) > 2, 2.05)

hist(ratio.trunc, breaks = seq(0,2.1,0.1), cex.axis=0.8, cex.lab=0.85, xaxt = "n",
     xaxs = "i", yaxs = "i", col = "grey", main = NULL, ylim = c(0,26.5), xlim = c(0,2.1),
     freq = TRUE, xlab = "Ratio of prop. at age (end:begin)")
box()
abline(v = 1, lty = "dotted", lwd = 1)
mtext("A", side = 3, line = -1.3, adj = 0.06)
# add custom labels
axis(1, at=c(0,0.5,1,1.5,2.05), labels=c("0.0",0.5,"1.0",1.5,">2.0"), cex.axis = 0.8)

hist(exp(test.stats.M[,2]), breaks = seq(0,2.1,0.1), cex.axis=0.8, cex.lab=0.85,
     xaxs = "i", yaxs = "i", col = "grey", main = NULL, ylim = c(0,26.5), xlim = c(0,2.1),
     freq = TRUE, xlab = "Ratio of prop. at age (end:unfished)")
box()
abline(v = 1, lty = "dotted", lwd = 1)
mtext("C", side = 3, line = -1.3, adj = 0.06)


# plot mean and SE change in ratio of plus group proportions
Mj <- c(mean(filter(log.change, Region == "Alaska")[,6]), 
        mean(filter(log.change, Region == "North Atlantic")[,6]),
        mean(filter(log.change, Region == "Northeast US")[,6]),
        mean(filter(log.change, Region == "South Atlantic US")[,6]),
        mean(filter(log.change, Region == "West Coast US")[,6]))
Sj.l <- c(exp(quantile(filter(log.change, Region == "Alaska")[,6], probs = 0.025)),
          exp(quantile(filter(log.change, Region == "North Atlantic")[,6], probs = 0.025)),
          exp(quantile(filter(log.change, Region == "Northeast US")[,6], probs = 0.025)),
          exp(quantile(filter(log.change, Region == "South Atlantic US")[,6], probs = 0.025)),
          exp(quantile(filter(log.change, Region == "West Coast US")[,6], probs = 0.025)))
Sj.u <- c(exp(quantile(filter(log.change, Region == "Alaska")[,6], probs = 0.975)),
          exp(quantile(filter(log.change, Region == "North Atlantic")[,6], probs = 0.975)),
          exp(quantile(filter(log.change, Region == "Northeast US")[,6], probs = 0.975)),
          exp(quantile(filter(log.change, Region == "South Atlantic US")[,6], probs = 0.975)),
          exp(quantile(filter(log.change, Region == "West Coast US")[,6], probs = 0.975)))

# Adjust outlier to fit on plot with broken axis
Sj.u[3] = 12.9


source("vioplot.R") 
# frame for plot only
plotCI(x=exp(Mj), ui=Sj.u, li=Sj.l, xlab="", col = "white", cex.axis=0.8, cex.lab=0.85, xlim = c(0.7,5.3),
       yaxs = "i", yaxt = 'n', xaxt='n', ylim = c(0,max(Sj.u)), ylab = "Ratio of prop. at age (end:begin)")
# now plot
vioplot(exp(filter(log.change, Region == "Alaska")[,6]), exp(filter(log.change, Region == "North Atlantic")[,6]),
        exp(filter(log.change, Region == "Northeast US")[-5,6]), exp(filter(log.change, Region == "South Atlantic US")[,6]),
        exp(filter(log.change, Region == "West Coast US")[,6]), col = c(transbrown, transblack, transorange, transgreen, transblue),
        add = TRUE, pchMed = "-")
points(3, 12.7, bg = transgreen, pch = 21, cex = 0.7)
abline(h=1, lty = "dotted")

# add y-axis break and custom labels
axis(2, at=c(0:11,12.7), labels=c(0:11,40), cex.axis = 0.8)
axis.break(axis = 2, breakpos = 12.2)
box()
mtext("B", side = 3, line = -1.3, adj = 0.01)


Mj <- c(mean(filter(log.change.M, Region == "Alaska")[,6]), 
        mean(filter(log.change.M, Region == "North Atlantic")[,6]),
        mean(filter(log.change.M, Region == "Northeast US")[,6]),
        mean(filter(log.change.M, Region == "South Atlantic US")[,6]),
        mean(filter(log.change.M, Region == "West Coast US")[,6]))
Sj.l <- c(exp(quantile(filter(log.change.M, Region == "Alaska")[,6], probs = 0.025)),
          exp(quantile(filter(log.change.M, Region == "North Atlantic")[,6], probs = 0.025)),
          exp(quantile(filter(log.change.M, Region == "Northeast US")[,6], probs = 0.025)),
          exp(quantile(filter(log.change.M, Region == "South Atlantic US")[,6], probs = 0.025)),
          exp(quantile(filter(log.change.M, Region == "West Coast US")[,6], probs = 0.025)))
Sj.u <- c(exp(quantile(filter(log.change.M, Region == "Alaska")[,6], probs = 0.975)),
          exp(quantile(filter(log.change.M, Region == "North Atlantic")[,6], probs = 0.975)),
          exp(quantile(filter(log.change.M, Region == "Northeast US")[,6], probs = 0.975)),
          exp(quantile(filter(log.change.M, Region == "South Atlantic US")[,6], probs = 0.975)),
          exp(quantile(filter(log.change.M, Region == "West Coast US")[,6], probs = 0.975)))

staxlab(1, 1:5, c("Alaska","ICES","Northeast US","Southeast US","West Coast US"), srt = 35, cex=.85, ticklen = 0.01)


par(mar = c(3.5, 2.7, 0, 0), new = FALSE)

# frame for plot only
plotCI(x=exp(Mj), ui=Sj.u, li=Sj.l, xlab="", col = "white", cex.axis=0.8, cex.lab=0.85, xlim = c(0.7,5.3),
       yaxs = "i", xaxt='n', ylim = c(0,max(Sj.u)), ylab = "Ratio of prop. at age (end:unfished)")
# now plot
vioplot(exp(filter(log.change.M, Region == "Alaska")[,6]), exp(filter(log.change.M, Region == "North Atlantic")[,6]),
        exp(filter(log.change.M, Region == "Northeast US")[,6]), exp(filter(log.change.M, Region == "South Atlantic US")[,6]),
        exp(filter(log.change.M, Region == "West Coast US")[,6]), col = c(transbrown, transblack, transorange, transgreen, transblue),
        add = TRUE, pchMed = "-")
abline(h=1, lty = "dotted")
box()
mtext("D", side = 3, line = -1.3, adj = 0.01)

staxlab(1, 1:5, c("Alaska","ICES","Northeast US","Southeast US","West Coast US"), srt = 35, cex=.85, ticklen = 0.01)

dev.off()



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Table of stocks
#stock.table = rbind((cbind(names(N.age.WC), rep("west coast US", length(N.age.WC)))), (cbind(names(N.age.NA), rep("ICES", length(N.age.NA)))),
#      (cbind(names(N.age.SA), rep("southeast US", length(N.age.SA)))), (cbind(names(N.age.NE), rep("northeast US", length(N.age.NE)))),
#      (cbind(names(N.age.AK), rep("Alaska", length(N.age.AK)))))
#  stock.table = cbind(stock.table, test.stats, test.stats.M)
#write.csv(stock.table, file = "StockTableRaw.csv")

#names = read_excel("StockTable.xlsx")

#write.csv(common_to_sci(names$'Common Name'), file = "ScientificNames.csv")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#R code to extract maximum age and other LH parameters for a set 
#of species listed from Fishbase
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

require(rfishbase)
options(FISHBASE_API = "https://fishbase.ropensci.org")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Species names to get LH parameters for
setwd("C:/Users/lewis.barnett/Documents/Postdoc/AgeTruncation/Data")
species <- read.csv(file="ATEspecies.csv", header=T, stringsAsFactors=F) 
#head(species)
nspecies <- length(species$Species)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#get max age
for (i in 1:nspecies) {
    pop.data <- popchar(species_list=species[i,2]) 
    print(paste("Species", i, "of 63", "scientific name", species[i,2]))
    species[i,4] <- max(pop.data$tmax, na.rm = TRUE) 
}

species

# Add maxmimum age estimate for atlantic butterfish, which does not have an estimate in fishbase
species[43,4] = 6
species[43,6] = "Max age from Draganik and Zukowski (1966)"

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#collate plus group ages from assessment data: could do just number of columns, but some start at different ages (0,1, sometimes 3 or 4)

#first reorder rows in "species" dataframe to match order of age data, by region- AK,WC,SA,NE,NA
species = species[c(47:63,1:24,31:41,42:46,25:30),]

#access each dataframe within N.age.(region) list, extracting the name of the last column, indicating the plus group age
for(i in 1:length(N.age.AK)){
  species[i, "Plus.group.age"] = as.numeric(sub("[[:punct:]]", "", 
                                                colnames(N.age.AK[[i]])[ncol(N.age.AK[[i]])]))
}
for(i in 1:length(N.age.WC)){
  species[i+length(N.age.AK), "Plus.group.age"] = as.numeric(sub("[[:punct:]]", "", 
                                                                 colnames(N.age.WC[[i]])[ncol(N.age.WC[[i]])]))
}
for(i in 1:length(N.age.SA)){
  species[i+length(N.age.AK)+length(N.age.WC), "Plus.group.age"] = as.numeric(sub("[[:punct:]]", "", 
                                                                                  colnames(N.age.SA[[i]])[ncol(N.age.SA[[i]])]))
}
for(i in 1:length(N.age.NE)){
  species[i+length(N.age.AK)+length(N.age.WC)+length(N.age.SA), "Plus.group.age"] = as.numeric(sub("[[:punct:]]", "", 
                                                                                                   colnames(N.age.NE[[i]])[ncol(N.age.NE[[i]])]))
}
for(i in 1:length(N.age.NA)){
  species[i+length(N.age.AK)+length(N.age.WC)+length(N.age.SA)+length(N.age.NE), "Plus.group.age"] = as.numeric(sub("[[:punct:]]", "", 
                                                                                                                    colnames(N.age.NA[[i]])[ncol(N.age.NA[[i]])]))
}

species

species$Region = as.factor(species$Region)
scatter.smooth(jitter(species$Plus.group.age, amount = 1), jitter(species$Maximum.age, amount = 1), 
               xlab = "Plus group age", ylab = "Maximum age", col = "darkgrey")

# Correlation test: Pearson's product moment correlation coefficient 
cor.test(species$Plus.group.age, species$Maximum.age, "two.sided", "pearson")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Comparison of time series length to age truncation in begin-end approach

# collate number of years in model output for each stock
species$TimeSeriesDuration = c(sapply(N.age.AK, nrow), sapply(N.age.WC, nrow), sapply(N.age.SA, nrow), sapply(N.age.NE, nrow), sapply(N.age.NA, nrow))

# combine with log ratio of change in plus group
species$Plus.group.log.change = log.change$`Oldest age class`
species$Plus.group.log.change.M = log.change.M$`Oldest age class`

# scatterplot with loess smoother
scatter.smooth(species$TimeSeriesDuration, species$Plus.group.log.change, 
               xlab = "Model output time series duration (yr)", ylab = "Log ratio of prop. at age (end:begin)", col = "darkgrey")
# Correlation test: Pearson's product moment correlation coefficient 
cor.test(species$TimeSeriesDuration, species$Plus.group.log.change, "two.sided", "pearson")
#abline(lm(species$Plus.group.log.change ~ species$TimeSeriesDuration))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# calculation of relative mean change in age structure between Hsieh et al. 2010 and our work

# Hsieh et al shows roughly a change in mean age (scaled from 0-1 for all species) from 0.91 to 0.68
(0.68-0.91)/0.91 # giving a relative change of -25%
# mean relative change in plus group proportion from our work
mean(test.stats.M$PropChange) # giving a mean relative change of -72%