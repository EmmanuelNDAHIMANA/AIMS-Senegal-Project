# To analyze the loss reserve data, we need to start by importing the famous package called ChainLadder and tidyverse for data manipulation
library(ChainLadder)
library(tidyverse)
# Using our datain munich chain ladder model, the data should be in triangle format.
#We start by importing our csv file into a dataframe
df <- read.csv2("D:/Project/Rdata&codes/finaldataP.csv")
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
plot(MCL$rhoP.sigma, type="l", col="blue",lty=1, lwd=3, ylab="Deviations", xlab="development periods")
lines(MCL$rhoI.sigma, type="l", col="red",lty=2, lwd=3, ylab="Incurred factors", xlab="development periods")
legend("topright", lty = c(1,2),col = c("blue","red"), lwd = c(3,3), legend =c("Paid deviations","incurred deviations"))

#The plots of chain ladder-link ag-to-age factors in figure 4.5 and figure 4.6 can be ploted like this.
plot(MCL$qinverse.f, type="l", col="red", ylim=c(0.1,4),lwd=3, pch=2, lty=3, ylab="age-to-age factors", xlab="development periods")
lines(MCL$q.f, type="l", col="blue", lwd=3, ylab="age-to-age factor", xlab="development periods")
abline(h=1, lty=2, col="black")
legend("topright", lty = c(3,1),col = c("red","blue"), lwd = c(3,3), legend =c("Incurred/Paid age-to-age factors","Paid/Incurred age-to-age factors"))



#The weighted average age-to-age factors in figure 4.7 and figure 4.8 can be ploted like this.
plot(MCL$MackPaid$f , type="l", col="red", lty=2,lwd=3, ylab="Weighted average age-to-age factors", xlab="development periods")
lines(MCL$MackIncurred$f , type="o", col="blue", lwd=3, ylab="Incurred factors", xlab="development periods")
legend("topright", lty = c(3,1),col = c("red","blue"), lwd = c(3,3), legend =c("Paid factors","Incurred factors"))

#Munich-chain-ladder forecasted full triangles on paid and incurred data in table 6.1 and table 6.2 can be obtained like this.
MCL$MCLPaid
MCL$MCLIncurred

