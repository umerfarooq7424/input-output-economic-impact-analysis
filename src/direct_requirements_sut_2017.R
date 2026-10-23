#Umer Farooq
#Global Data Analysis
#Spring 2024
#Direct and Requirement Matrix assignment
rm(list=ls())
options(prompt="R > ")
library(readxl)
UseTable <- read_excel("data/Use_SUT_Framework_2017_DET.xlsx", skip = 3, sheet = 2)  # BEA Supply-Use tables (not redistributed here) # reading in the data#
SupplyTable <- read_excel("data/Supply_2017_DET.xlsx", skip = 3, sheet = 2)  # BEA Supply-Use tables (not redistributed here)
# obtained from "https://www.bea.gov/industry/input-output-accounts-data"

US2017Usedata = as.data.frame(UseTable)
US2017Supplydata = as.data.frame(SupplyTable)

z = which(US2017Usedata[1,] == "F04000") # F04000 is the column of exports (x)
y = which(US2017Usedata[1,] == "S00203") # S00203 is the last intermediate industry
n = which(US2017Usedata[,1] == "S00900") # S00900 is the last intermediate commodity
k = which(US2017Usedata[1,] == "T019") # T019 is commodity output (q)
l = which(US2017Usedata[,1] == "T018")
a = which(US2017Supplydata[1,] == "MCIF") # MCIF is the column of imports
b = which(US2017Supplydata[1,] == "S00203") # S00203 is the last intermediate industry
c = which(US2017Supplydata[,1] == "S00900") # S00900 is the last intermediate commodity
d = which(US2017Supplydata[1,] == "T007") # T007 is total commodity output (q)
e = which(US2017Supplydata[,1] == "T013") #including imports q+m



U = US2017Usedata[2:n, 3:y] # the intermediate industry portion of the Use matrix
U[U == "---"] = 0 # Removing all --- to zero for solving
U = apply(U, 2, as.numeric)
U[which(is.na(U) == TRUE)] = 0 #treating all as numeric digit

S = US2017Supplydata[2:c, 3:b] # the intermediate industry portion of the Use matrix
S[S == "---"] = 0 #ditto for supply
S = apply(S, 2, as.numeric)
S[which(is.na(S) == TRUE)] = 0 #treating ana na as 0


g = US2017Usedata[l, 3:y] #This will make a vector from Use table till last commodity
g = apply(g, 1, as.numeric) #Again traeting as numeric
g = c(g) 
q = US2017Supplydata[2:c, d] #Now from Supply table which tells total production of the industry took vector till last commodity
q = as.numeric(q)
q[which(is.na(q) == TRUE)] = 0 #Ditto like use tables


ghat = diag(g) #Diagnoal matrix as of total production from Use table as total intermediate including value addition and others
print(dim(ghat))


ghatInv = 1/ghat 
ghatInv[is.infinite(ghatInv)]<-0 #took inverse and replace infinity by 0 (Dividing by 0 gets infinity)

qhat = diag(q)  #Ditto from above but from supply table as of total production of any sector/industry)
qhatInv = 1/(qhat) #Ditto
qhatInv[is.infinite(qhatInv)]<-0 #Ditto

B = U%*%ghatInv #Matrix inverse of diagonal matrix and multiplied with intermediate industry of use matrix
D = t(S)%*%qhatInv #Taking transpose just to make Industry by commodity for supply table and later multiplying 
A <- D %*% B #This gives us a direct matrix but includes exports and imports. Both exports and imports are foreign consumed or produced commodities by industries. To asses the domestic production we exculde it from our analysis for this we calculated rho
any(is.na(D))
m = US2017Supplydata[2:c,a]
x = US2017Usedata[2:n,z]
x = as.numeric(x)
x[which(is.na(x) == TRUE)] = 0
m = as.numeric(m)
m[which(is.na(m) == TRUE)] = 0
q = as.numeric(q)
q[which(is.na(q) == TRUE)] = 0

rho = (q-x)/(q-x+m)   #This account for domestic produced commodities but consumed foreign exports x and foreign produced consumed domestic imports m
rho[is.infinite(rho)]<-0 #Again replaced infinites with 0
rho[is.na(rho)]<-0
rhohat=diag(rho)
B = U%*%ghatInv
D = t(S)%*%qhatInv
A1 <- D %*%rhohat
A2 = A1 %*% B
A <- D %*%rhohat%*% B #this gives us the adjusted and final Direct Matrix 

rowSums(A)
colSums(A)

I <- diag(402) #this is diagnol matrix of all 402 items items and solving it for its inverse gives us the requirement matrix.
L <- solve(I-A)
I <- diag(402)
L <- solve(I-A)


#calculating output multipliers by summing the rowsums of Lointeif matrix

output_multipliers <- rowSums(L)

# calculating vector for total value added for each industry


va = US2017Usedata[408, 3:404]
va = apply(va, 1, as.numeric)
va = c(va)


# Value added coefficient
value_add_coeff <- va / g  # 'g' is the vector of total output for each industry
value_add_coeff[which(is.na(value_add_coeff) == TRUE)] = 0


# Calculate value-added multipliers
value_add_multiplers <- value_add_coeff %*% L

value_add_multiplers <- t(value_add_multiplers)



g <- as.numeric(g)
A <- data.matrix(A)
n=dim(A)[1]
I=diag(1,n,n)              ## creating the identity matrix confirming with A
f=g-rowSums(A%*%diag(g))   ## f is final demand vector???the difference between output and 
## intermediate output
delta_g=diag(0,n,n)        ## initializing the answer
##############################################
## The following is a loop. Each iteration through the loop performs moves on to the next
## industry until the operation has been performed on all n industries
##############################################
for(i in 1:n){
  ##########
  ## resetting values for A and f so they can be modified for extraction
  ##########
  Aprime=A
  fprime=f
  ##########
  ## zeroing out the industry's row and column in A, as well as its final demand
  ##########
  Aprime[i,]=0
  Aprime[,i]=0
  fprime[i]=0
  ##########
  ## Estimating the impact of extraction of the industry from the economy
  ##########
  delta_g[i,]=g-solve((I-Aprime),fprime)
}
##########
## deleting extraneous variables
##########
rm(Aprime,fprime)


#calculating output linkages extraction
output_linkages <- rowSums(delta_g)


#value added linkages extraction

value_added_linkages <-rowSums(delta_g%*%diag(value_add_multiplers))#post multiplying by change in output by diagonal of total output vector and divding by total output



summary_df <- data.frame(
  Leontief_Output_Multiplier = output_multipliers,
  Value_Added_Multiplier = value_add_multiplers,
  Output_Linkages_Hypothetical_Extraction = output_linkages,
  Value_Added_Linkages_Hypothetical_Extraction = value_added_linkages
)

Industry_names= US2017Usedata[2:403, 2] 
multiplier_comparison <- cbind(Industry_Name = Industry_names, summary_df) #putting in industry names as row names


# Extracting top 10 out of each column


library(openxlsx)

# Function for top 10
get_top_10 <- function(df, column_name) {
  sorted_df <- df[order(df[[column_name]], decreasing = TRUE), ]
  top_10_df <- head(sorted_df, 10)
  return(top_10_df)
}
#Top Columns
top_10_output_multiplier <- get_top_10(multiplier_comparison, "Leontief_Output_Multiplier")
top_10_value_multiplier <- get_top_10(multiplier_comparison, "Value_Added_Multiplier")
top_10_output_linkages <- get_top_10(multiplier_comparison, "Output_Linkages_Hypothetical_Extraction")
top_10_value_linkages <- get_top_10(multiplier_comparison, "Value_Added_Linkages_Hypothetical_Extraction")


wb <- createWorkbook()

addWorksheet(wb, "Top 10 Output Multiplier")
writeData(wb, "Top 10 Output Multiplier", top_10_output_multiplier)

addWorksheet(wb, "Top 10 Value Added Multiplier")
writeData(wb, "Top 10 Value Added Multiplier", top_10_value_multiplier)

addWorksheet(wb, "Top 10 Output Linkages")
writeData(wb, "Top 10 Output Linkages", top_10_output_linkages)

addWorksheet(wb, "Top 10 Value Added Linkages")
writeData(wb, "Top 10 Value Added Linkages", top_10_value_linkages)

# Save the workbook
saveWorkbook(wb, "Top_10_Multipliers_and_Linkages.xlsx", overwrite = TRUE)


