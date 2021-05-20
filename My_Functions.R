##################################################
# FUNCTION: FitRegressionModel
# Fits an ordinary least squares regression model
# input: x and y vectors of numeric. Must be same length
# output: entire model summary from lm 
#------------------------------------------------- 
FitRegressionModel <- function(xVar=runif(10),yVar=runif(10)) {
  dataFrame <- data.frame(xVar,yVar)
  regModel <- lm(yVar~xVar,data=dataFrame)
  return(summary(regModel))
}
FitRegressionModel()


##################################################
# FUNCTION: SummarizeOutput
# pull elements from model summary list
# input: list from summary call of lm
# output: vector of regression residuals 
#------------------------------------------------- 
SummarizeOutput <- function(z=NULL) {
  if(is.null(z)) {
    z <- summary(lm(runif(10)~runif(10)))
  }  
  return(z$residuals)
}
SummarizeOutput()


##################################################
# FUNCTION: GraphResults
# graph data and fitted OLS line
# input: x and y vectors of numeric. Must be same length
# output: creates graph 
#------------------------------------------------- 
GraphResults <- function(xVar=runif(10), yVar=runif(10)) {
  dataFrame <- data.frame(xVar,yVar)
  # #plot(y=dataFrame$yVar,
  #      x=dataFrame$xVar,
  #      pch=21,
  #      bg="lightblue",
  #      cex=2)
  p1 <- qplot(data=dataFrame,x=xVar,y=yVar,geom=c("smooth","point"))
  print(p1)
  regModel <- lm(yVar~xVar,data=dataFrame)
  # abline(regModel)
  message("Message: Regression graph created")
}
GraphResults()
str(GraphResults())


##################################################
# FUNCTION: GetData
# read in a .csv file
# input: .csv file
# output: data frame
#------------------------------------------------- 
GetData <- function(fileName=NULL) {
  if(is.null(fileName)) {
    dataFrame <- data.frame(ID=101:110,
                            varA=runif(10),
                            varB=runif(10))
  }  else {
    dataFrame <- read.table(file=fileName,
                            header=TRUE,
                            sep=",",
                            stringsAsFactors=FALSE) 
  }
  return(dataFrame)
}


GetData()




##################################################
# FUNCTION: FunctionName
# one line description
# input: x
# output: x 
#------------------------------------------------- 
FunctionName <- function() {
  
  
  return("testing..............FunctionName")
}
# Sample program to illustrate structured programming with functions
# 9 March 2017
# NJG

# All functions must be declared at the top before they are used
##################################################
# FUNCTION: GetData
# one line description
# input: x
# output: x 
#------------------------------------------------- 
GetData <- function() {
  
  
  return("testing..............GetData")
}
##################################################
# FUNCTION: CalculateStuff
# one line description
# input: x
# output: x 
#------------------------------------------------- 
CalculateStuff <- function() {
  
  
  return("testing..............CalculateStuff")
}
##################################################
# FUNCTION: SummarizeOutput
# one line description
# input: x
# output: x 
#------------------------------------------------- 
SummarizeOutput <- function() {
  
  
  return("testing..............SummarizeOutput")
}
##################################################
# FUNCTION: GraphResults
# one line description
# input: x
# output: x 
#------------------------------------------------- 
GraphResults <- function() {
  
  
  return("testing..............GraphResults")
}
##################################################
library(ggplot2)
GetData()
CalculateStuff()
SummarizeOutput()
GraphResults()



