# Name: Umer Farooq
# Date: 02/28/2024
#Homework Assignment 4
# Data Source: Northeast_Pipeline_Upgrade.xlsx, Use Matrices and Economic Vectors provided in the HW files by professor.

# Set the working directory.
# setwd("<your data folder>")  # relative paths used in this repo version

#Clearing from previous work.

rm(list=ls())

install.packages("openxlsx")

# Load necessary libraries for Excel file operations.
library(readxl) 
library(openxlsx) 


A_NJ <- read.csv("A_nj.csv")
A_PA <- read.csv("A_pa.csv")

# Read Vectors for NJ and PA which includes the Multipliers by professor 
VectNJ <- read.csv("NJ_vectors.csv", header = TRUE)
VectPA <- read.csv("PA_vectors.csv", header = TRUE)

#extracting change in demand using Northeast_Pipeline_Upgrade.xlsx This file contains data for business and operations costs.
DeltYf <- read.csv("IO_industry_sector.csv", header = TRUE)
#It come from the changes made by economic activity of Pipeline for overall and respective states 
DeltYNJ <- DeltYf$NJ
DeltYPA <- DeltYf$PA
#Removing NA from the data to 0
DeltYNJ[which(is.na(DeltYNJ)==TRUE)] = 0
DeltYPA[which(is.na(DeltYPA)==TRUE)] = 0
#Removing the top row from the vector of respective states
A_NJ <- A_NJ[,-1]
A_PA <- A_PA[,-1]
#Removing NA and also converting all variables in numeric for respective states 
A_NJ[which(is.na(A_NJ)==TRUE)] = 0
A_PA[which(is.na(A_PA)==TRUE)] = 0

A_NJ_vector <- as.numeric(unlist(A_NJ))
A_NJ_matrix <- matrix(A_NJ_vector, nrow = 405, byrow = TRUE)
#Similarly for Pennsylvania
A_PA_vector <- as.numeric(unlist(A_PA))
A_PA_matrix <- matrix(A_PA_vector, nrow = 405, byrow = TRUE)

#This is taking values of jobsoutput ratio from our Vector of NJ, similar y output ratio and GDP for both of the states
RNJ <- VectNJ$jobs.1000output.ratio
TNJ <- VectNJ$comp.output.ratio
UNJ <- VectNJ$gdp.output.ratio
#RhO was given by professor in the last column of the Vector NJ file.
rhoNJ <- VectNJ$RPC
#Similarly for the Pennsylvania Data
RPA <- VectPA$jobs.1000output.ratio
TPA <- VectPA$comp.output.ratio
UPA <- VectPA$gdp.output.ratio
rhoPA <- VectPA$RPC

#It will take the industry names from the table of 405 industry. 
industrynames <- VectPA$description

I <- diag(1, 405, 405)
rhohaTNJ = diag(rhoNJ)
rhohaTPA = diag(rhoPA)


LNJ = solve(I - rhohaTNJ %*% A_NJ_matrix)
LPA = solve(I - rhohaTPA %*% A_PA_matrix)

delgNJ <- LNJ %*% DeltYNJ
delgPA <- LPA %*% DeltYPA


DeljobsNJ <- (delgNJ/1000)*RNJ

# Calculate the change in compensation in NJ due to pipeline upgradation. This involves multiplying the change in gross output ('delgNJ') by the compensation multiplier ('TNJ'). The compensation multiplier estimates the increase in total compensation (wages, salaries, benefits) in the economy per unit of output increase, highlighting the economic benefit to workers from the upgradation.
DelcompNJ <- (delgNJ)*TNJ

# Calculate the change in Gross Domestic Product (GDP) in NJ from the pipeline upgradation. The gross output change ('delgNJ') is multiplied by the GDP multiplier ('UNJ'), which reflects the overall economic value added to the regional economy per unit of output increase. This captures the broader economic impact of the pipeline upgradation on NJ's economy.
delGDPNJ <- (delgNJ)*UNJ

# Calculate the change in jobs in Pennsylvania (PA) due to the pipeline upgradation, using the same method as for NJ but with PA's job multiplier ('RPA'). This reflects the job creation in PA as a result of the economic activity associated with the pipeline upgradation, accounting for both direct construction jobs and possibly indirect jobs in related sectors.
DeljobsPA <- (delgPA/1000)*RPA

# Calculate the change in compensation in PA resulting from the pipeline upgradation by multiplying the change in output ('delgNJ') by PA's compensation multiplier ('TPA'). This step estimates the increase in worker compensation in PA due to the economic activity, reflecting the income benefits to the local workforce.
DelcompPA <- (delgPA)*TPA

# Calculate the change in GDP in PA from the pipeline upgradation, using PA's GDP multiplier ('UPA'). This step assesses the total economic value added to PA's economy per unit of output increase from the pipeline project, encapsulating the upgradation's contribution to economic growth in PA.
delGDPPA <- (delgPA)*UPA

# Combine all the calculated economic impacts (jobs, compensation, GDP for both NJ and PA) into a single matrix. 
economic_activity_matrix <- cbind(delgNJ,DeljobsNJ, DelcompNJ, delGDPNJ, delgPA, DeljobsPA, DelcompPA, delGDPPA)


# Assign descriptive column names to the economic impact matrix for clarity. 
colnames(economic_activity_matrix) <- c("change in output NJ", "change in jobs NJ", "change in earnings NJ", "Change in GDP NJ", "change in output PA",
                                      "change in jobs PA", "change in earnings PA", "Change in GDP PA")

# Set the row names of the economic impact matrix to industry names extracted from the PA vectors. This assumes the industry names are applicable to both NJ and PA analyses.
rownames(economic_activity_matrix) = industrynames

# Convert the economic impact matrix to a data frame. This is necessary for further manipulation and for exporting to Excel, as data frames are more flexible in handling.
Eco_impact_df <- as.data.frame(economic_activity_matrix)

# Round all numeric values in the economic impact data frame to one decimal place for uniformity and easier interpretation.
Eco_impact_df <- data.frame(lapply(Eco_impact_df, function(x) if(is.numeric(x)) round(x, 1) else x))
Eco_impact_df$change.in.jobs.NJ <- round(Eco_impact_df$change.in.jobs.NJ, digits = 0) #Round all numeric values under jobs to zero decimal points
Eco_impact_df$change.in.jobs.PA <- round(Eco_impact_df$change.in.jobs.PA, digits = 0) #Round all numeric values under jobs to zero decimal points

#Converting the dollar amounts to 1000s of dollars

Eco_impact_df[, c(1, 3, 4, 5, 7, 8)] <- Eco_impact_df[, c(1, 3, 4, 5, 7, 8)] / 1000


# Calculate the column sums of the economic impact data frame. This step aggregates the total impact across all industries for each economic indicator.
totals = colSums(Eco_impact_df)

# Set the row names of the economic impact data frame to industry names again, ensuring that the names are correctly associated with each row.
rownames(Eco_impact_df) = industrynames

# Append the totals row to the bottom of the economic impact data frame. This adds a summary row showing the overall impact.
Eco_impact_df <- rbind(Eco_impact_df, totals)

# Rename the last row as "Totals" to clearly indicate that this row contains the sum totals for each column.
rownames(Eco_impact_df)[nrow(Eco_impact_df)] <- "Totals"

# Add a new column "IndustryName" to the beginning of the data frame, containing the industry names. This converts the row names to a regular column for inclusion in the Excel export.
Eco_impact_df <- cbind(IndustryName = rownames(Eco_impact_df), Eco_impact_df)

# Write the economic impact data frame to an Excel file named "EconomicImpactAnalysis.xlsx" with a sheet named "Impact Analysis".
write.xlsx(Eco_impact_df, file = "EconomicImpactAnalysis.xlsx", sheetName = "Impact Analysis", rowNames = FALSE)

