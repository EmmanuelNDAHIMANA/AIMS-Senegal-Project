# To analyze the loss reserve data, we need to start by importing the famous package called ChainLadder and tidyverse for data manipulation
library(ChainLadder)
library(tidyverse)
# Using our datain munich chain ladder model, the data should be in triangle format.
#We start by importing our csv file into a dataframe
df <- read.csv2("finaldataP.csv")
#From the imported dataframe df, we extract the incurred data as run-off triangle.
INCURTriGL1P <-  as.triangle(df, origin="AccidentYear", dev = "Development", value="IncurredValue")
#And again, from the dataframe df, we retrieve the paid loss claims data in triangle format here below
PAIDTriGL1P <-  as.triangle(df, origin="AccidentYear", dev = "Development", value="PaidValue")
# The function MunichChainLadder was used to make our analysis, the input arguments are cumulative paid and incurred triangles, the other arguments are there to fix the ways the model will work.
#est.sigmaP defines how this parameter will be estimated on paid triangle, the same as est.sigmaI for incurred trianlge.
#tailP defines how the tail of the Paid triangle will be estimated, the sase as tailI for Incurred triangle.
MCL <- MunichChainLadder(Paid =  PAIDTriGL1P,Incurred =  INCURTriGL1P,est.sigmaP = "log-linear", est.sigmaI = "log-linear",tailP=TRUE,tailI=TRUE)
#The values or output of MunichChainLadder function.
#The function will give the table 4.1 and table 4.2 of the paid and incurred  ultimates projections and their ratios, the paid and incurred latest and their ratios. 
MCL
#The plot of the function will give us the four figures, figuew 4.9, figure 4.10, figure 4.1 and figure 4.2.
plot(MCL)
#The following two lines of codes will give us the slope of regression line between Paid development factore and Incurred/Paid residual 
#and the slope of regression line between incurred development factors and Paid/Incurred residuals respectively.
MCL$lambdaP
MCL$lambdaI
#The conditional deviations plots in figure 4.3 and figure 4.4 can be ploted like this.
plot(MCL$rhoP.sigma, type="l", col="blue", lwd=3, ylab="rhoP.sigma", xlab="development periods",
     main="Paid conditional deviations vs development periods")
plot(MCL$rhoI.sigma, type="l", col="blue", lwd=3, ylab="Incurred factors", xlab="development periods",
     main="Incurred conditional deviations vs development periods")
#The plots of chain ladder-link ag-to-age factors in figure 4.5 and figure 4.6 can be ploted like this.
plot(MCL$q.f, type="l", col="blue", lwd=3, ylab="qfactors", xlab="development periods", 
     main="chain-ladder-link age-to-age factors of the paid/incurred triangle vs development periods")
plot(MCL$qinverse.f, type="l", col="blue", lwd=3, ylab="qinverse factors", xlab="development periods", 
     main="chain-ladder-link age-to-age factors of the incurred/paid triangle vs development periods")
#The weighted average age-to-age factors in figure 4.7 and figure 4.8 can be ploted like this.
plot(MCL$MackPaid$f , type="o", col="blue", lwd=3, ylab="Paid factors", xlab="development periods")
plot(MCL$MackIncurred$f , type="o", col="blue", lwd=3, ylab="Incurred factors", xlab="development periods")
#Munich-chain-ladder forecasted full triangles on paid and incurred data in table 6.1 and table 6.2 can be obtained like this.
MCL$MCLPaid
MCL$MCLIncurred
