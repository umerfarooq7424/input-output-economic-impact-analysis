# Name: Umer Farooq
# Date: 03/08/2024
#Homework Assignment 5 Aggregation of Impact of Gas Pipeline in NJ 
# Data Source: https://www.bea.gov/industry/input-output-accounts-data
# Data Source: https://apps.bea.gov/iTable/?reqid=150&step=3&isuri=1&table_list=6017&categories=makeuse&_gl=1*kp1ewh*_ga*MTAxMjQxMzUzOS4xNzA2NzE2MDEy*_ga_J4698JNNFT*MTcwOTk5ODUzMy4xMC4xLjE3MDk5OTg2MzYuNTkuMC4w.


#Clearing from previous work.

rm(list=ls())

#Set the working directory.

# setwd("<your data folder>")  # relative paths used in this repo version

#installing all required packages
install.packages("readr")
install.packages(readx)
install.packages(c("readxl", "tidyverse"))
install.packages("openxlsx")
library("openxlsx")
library("readxl")
library("tidyverse")

# Reading in aggregated matrix and setting blanks and other undefined values to zeros
# This matrix is not needed in most cases. We truly only need the dimension to which
# we want to aggregate, "n", and the labels for the sectors or commodities here the   
# labels are will be placed in the variable "USIOcode15". and row labels will be stored in rowname for final output
#Labels will be assigned t the end after finding the aggregated (15x4) matrix
#Importing excel files of US 15 Sectors Use tables
# Dimensions of rows are stored in m.

ex_file=read_excel("Use_tableUS2022-15.xlsx")
rowname=as.data.frame(ex_file[7:21,2:17])
new_file=data.matrix(ex_file[7:21,3:17])
new_file[is.nan(new_file)]<-0
new_file[is.na(new_file)]<-0
USuse2022_15=matrix(new_file, nrow=15, ncol=15)
n=dim(USuse2022_15)[1]
USIOcode15=paste(unlist(ex_file[7:21,1]))

# Reading in matrix to be aggregated (NJ_Impact_Analysis_Sheet  from assignment 4) 
#Taking the column names from the NJ analysis sheet as well 
#Formulating Matrix for NJ Impact Analysis output and naming it eco_impact
#Removed all NA and missing to 0.
#checked the dimension
#
eco_impact=read_excel("NJ_Impact_Analysis_Sheet.xlsx")
cols=read_excel("NJ_Impact_Analysis_Sheet.xlsx", sheet = "Impact_Analysis", col_names = FALSE)
colname=as.data.frame(cols)
eco_impact_matrix=data.matrix(eco_impact[1:405,3:6])
eco_impact_matrix[is.nan(eco_impact_matrix)]<-0
eco_impact_matrix[is.na(eco_impact_matrix)]<-0
nj_eco_impact_405=matrix(eco_impact_matrix, nrow=405, ncol=4)
m=dim(nj_eco_impact_405)[1]
nj_eco_impact=paste(unlist(eco_impact[1:405,1]))

# Some of the aggregate labels need to be redefined to make the process simpler.
# This could be done directly in the original Excel file instead. 
#
USIOcode15[USIOcode15=="31G"]="31"
USIOcode15[USIOcode15=="44RT"]="44"
USIOcode15[USIOcode15=="48TW"]="48"
USIOcode15[USIOcode15=="FIRE"]="52"
USIOcode15[USIOcode15=="PROF"]="54"
USIOcode15[USIOcode15=="6"]="60"
USIOcode15[USIOcode15=="7"]="70"
USIOcode15[USIOcode15=="G"]="90"
#
# Now for this script to work as I want it the first two characters of the detailed labels 
# need to comport with the aggregate labels. So I redefine them based on industries to sectors This too could be done
# directly in the original Excel file instead. 
#I assembled all industries in their respective sectors first 2 or in some cases first letter of the codes for sectors and industries matched
#the first two characters of nj_eco_impact are the same as the first two characters of USIOcode15.

nj_eco_impact[substr(nj_eco_impact, start=1, stop=2)=="11"]="11"
nj_eco_impact[substr(nj_eco_impact, start=1, stop=2)=="21"]="21"
nj_eco_impact[substr(nj_eco_impact, start=1, stop=2)=="22"]="22"
nj_eco_impact[substr(nj_eco_impact, start=1, stop=2)=="23"]="23"
nj_eco_impact[substr(nj_eco_impact, start=1, stop=2)=="31"]="31"
nj_eco_impact[substr(nj_eco_impact, start=1, stop=2)=="33"]="31"
nj_eco_impact[substr(nj_eco_impact, start=1, stop=2)=="32"]="31"
nj_eco_impact[substr(nj_eco_impact, start=1, stop=2)=="42"]="42"
nj_eco_impact[substr(nj_eco_impact, start=1, stop=2)=="44"]="44"
nj_eco_impact[substr(nj_eco_impact, start=1, stop=2)=="45"]="44"
nj_eco_impact[substr(nj_eco_impact, start=1, stop=2)=="4B"]="44"
nj_eco_impact[substr(nj_eco_impact, start=1, stop=2)=="48"]="48"
nj_eco_impact[substr(nj_eco_impact, start=1, stop=2)=="49"]="48"
nj_eco_impact[substr(nj_eco_impact, start=1, stop=2)=="51"]="51"
nj_eco_impact[substr(nj_eco_impact, start=1, stop=2)=="52"]="52"
nj_eco_impact[substr(nj_eco_impact, start=1, stop=2)=="53"]="52"
nj_eco_impact[substr(nj_eco_impact, start=1, stop=2)=="53"]="52"
nj_eco_impact[substr(nj_eco_impact, start=1, stop=2)=="54"]="54"
nj_eco_impact[substr(nj_eco_impact, start=1, stop=2)=="54"]="54"
nj_eco_impact[substr(nj_eco_impact, start=1, stop=2)=="56"]="54"
nj_eco_impact[substr(nj_eco_impact, start=1, stop=2)=="55"]="54"
nj_eco_impact[substr(nj_eco_impact, start=1, stop=2)=="61"]="60"
nj_eco_impact[substr(nj_eco_impact, start=1, stop=2)=="62"]="60"
nj_eco_impact[substr(nj_eco_impact, start=1, stop=2)=="71"]="70"
nj_eco_impact[substr(nj_eco_impact, start=1, stop=2)=="72"]="70"
nj_eco_impact[substr(nj_eco_impact, start=1, stop=2)=="72"]="70"
nj_eco_impact[substr(nj_eco_impact, start=1, stop=2)=="81"]="81"
nj_eco_impact[substr(nj_eco_impact, start=1, stop=3)=="S00"]="90"
nj_eco_impact[substr(nj_eco_impact, start=1, stop=1)=="G"]="90"


# To check the above, I made sure all labels in nj_eco_impact were assigned to one and only one
# USIOcode15 from US_BEA 15 sector . 
#
# Now I initialize the aggregation matrix S as a (m x n) matrix of zeros. I then check where 
# Where they do, the following loop sets the element in the S matrix to 1.
#
S<-matrix(0,nrow=m, ncol=n)
for(i in 1:m){
  for(j in 1:n){
    if (substr(nj_eco_impact[i], start = 1, stop = 2)==substr(USIOcode15[j], start = 1, stop = 2)){
      S[i,j]<-1
    }
  }
}
#
# Aggregate nj_eco_impact_405 by pre-multiplying by the transpose of S and call it nj_eco_impact_405to15.
#Transposing treats the error of different dimension of two matrices. This matrix is our main aggregated output. We just need to add labels here after.

nj_eco_impact_405to15=t(S)%*%nj_eco_impact_405


#I checked the difference of two aggregated (15) and dis aggregated (405) NJ Impact matrices to see if the aggregation is right. Difference is almost zero after aggregation.
count405<- colSums(nj_eco_impact_405)
count15<- colSums(nj_eco_impact_405to15)
diff = sum(abs((count405 - count15)))

#creating label for final matrix


label_row =rowname[1:15,1]#row label based on 15-sectors i.e. created in HW04 from NAICS names for respective industries.
label_col=colname[1,3:6]#col label based on impact analysis for NJ 

#assigning col and row labels to final output matrix. It will include names of Sectors and four outputs from HW04.
rownames(x=nj_eco_impact_405to15)=label_row
colnames(x=nj_eco_impact_405to15)=label_col

#write the final output matrix as CSV and storing on local drive

write.csv(x=nj_eco_impact_405to15, file="Economic Impact of Gas pipeline in New Jersey.csv")
