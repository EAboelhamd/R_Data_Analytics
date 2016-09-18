# reading line from the web .. 

con <- url("http://www.jhsph.edu", "r")
x <- readLines(con)
head(x)



## test function lapply
function (x,FUN)
{
  FUN <- match.fun(FUN)
  if(!is.vector(X) || is.object(X))
    X <- as.list(X)
  .Internal(lapply(X, FUN))
   
}

### Connect to databases
install.packages(c("dbConnect"))
library("dbConnect")
ucscDb <- dbConnect(MySQL(), user = "genome", host = "genome-mysql.cse.ucsc.edu")

mergedData2 <- merge(reviews, solutions, by.x = "solution_id", by.y = "id", all = TRUE)


dbDisconnect(ucscDb)
result


### Merge Data
#mergedData2 <- merge(reviews, solutions, by.x = "solution_id", by.y = "id", all = TRUE)


## Plotting:
install.packages(c("ggplot2"))
library(ggplot2)
qplot(displ, hwy, data = mpg, geom = c("point", "smooth"))

## scatter plot 
set.seed(1234)
par(mar = c(0,0,0,0))
x <- rnorm(12, mean = rep(1:3, each = 4, sd = 0.2))
y <- rnorm(12, mean = rep(c(1,2,3), each = 4), sd = 0.2)
plot(x,y, col = "blue", pch = 19, cex = 2)
text(x + 0.05, y + 0.05, labels = as.character(1:12))
  