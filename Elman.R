library(ggplot2)
library(lubridate)

library(Metrics) 
library(DataExplorer)
library(caret)
library(astsa)
library(tensorflow)
library(readr)
library(tseries)
data("bev")
plot ( bev )
data <- bev
data
y <- ts(log (bev),frequency = 12 )
decomp<- decompose ( y , type = "multiplicative" )
plot ( decomp )

plot ( decomp$trend ,ylab="Value" )

random<-newdata <-na.omit(decomp$random)
plot(density(random))
qqnorm (random)
acf(y)
pacf(y)
range_data<-function(x) {(x-min(x))/(max(x)-min(x))}
y <-range_data (bev)
min_data<-min(bev)
max_data<-max(bev)

require ( quantmod )
y<-as.zoo(y)
x1<-Lag ( y , k = 1 )
x2<-Lag ( y , k = 2 )
x3<-Lag ( y , k = 3 )
x4<-Lag ( y , k = 4 )
x5<-Lag ( y , k = 5 )
x6<-Lag ( y , k = 6 )
x7<-Lag ( y , k = 7 )
x8<-Lag ( y , k = 8 )
x9<-Lag ( y , k = 9 )
x10<-Lag ( y , k = 10)
x11<-Lag ( y , k = 11)
x12<-Lag ( y , k = 12)


x<- cbind ( x1 , x2 , x3 ,x4 , x5 , x6 ,x7,x8,x9,x10 , x11 , x12 )
x<-cbind ( y , x )
x <- x [ -(1 : 12 ) , ]

n=nrow ( x )
n

set.seed(2018)
n_train <- 300
train<- sample(1:n,n_train, FALSE)
test <- sample(1:n,58, FALSE)
inputs<-x[,2:13]
outputs<-x[,1]

library(RSNNS)

require(RSNNS)
fit<-elman(inputs[train],
                 outputs[train],
                 size=c(3,2),
                 learnFuncParams=c(0.1),
                 maxit=5000)

fit<-jordan(inputs[train],
            outputs[train],
            size=4,
            learnFuncParams=c(0.01),
            maxit=5000)


plotIterativeError(fit)

plotRegressionError(outputs [train ],fit$fitted.values)

round(cor(outputs[train],fit$fitted.values)^2,4)
#Not too bad for an initial model!

pred<-predict(fit,inputs[-train])
round(cor(outputs[-train], pred)^2,4)


unscale_data<-function(x,max_x,min_x){x*(max_x-min_x )+min_x}
output_actual<-unscale_data(outputs[-train], max_data , min_data)
output_actual<-as.matrix(output_actual)
rownames(output_actual) <-1:length(output_actual)
output_pred<-unscale_data(pred,max_data,min_data )


result<-cbind(as.ts(output_actual),as.ts(output_pred))
plot(result)






























