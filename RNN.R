 # install.package
# install.packages("rnn")

 # Library
library(rnn)
rm(list=ls())

set.seed(10)
f <- 5
w <- 2*pi*f
# Create sequences
t <- seq(0.005, 2, by = 0.005)
x <- sin(t*w)
y <- cos(t*w)
# Samples of 20 times series
X <- matrix(x, nrow = 40)
Y <- matrix(y, nrow = 40)
# Plot noisy waves
plot(as.vector(X), col = "blue", type = "l", ylab = "X,Y", main = "Noisy waves")
lines(as.vector(Y), col = "red")
legend("topright", c("X", "Y"), col = c("blue", "red"), lty = c(1,1), lwd = c(1,1))

# Standardize in the interval 0 - 1
X <- (X-min(X)) / (max(X)-min(X))
Y <- (Y-min(Y)) / (max(Y)-min(Y))
 # scale()는　(x-mean(x))/sd(x) 를　의미함
# Transpose
X <- t(X)
Y <- t(Y)
# Training-testing sets
train <- 1:8
test <- 9:10

# Train model. Keep out the last two sequences.
model <- trainr(Y = Y[train,],
                X = X[train,],
                learningrate = 0.05,
                hidden_dim = 16,
                numepochs = 500)

# Predicted values
Yp <- predictr(model, X)

# Plot Predicted vs acutal. Training set + testing set
plot(as.vector(t(Y)), col = 'red', type = 'l', main = 'Actual vs Predicted', ylab = "Y.Yp")
lines(as.vector(t(Yp)), type = 'l', col = 'blue')
# Plot predicted vs actual. Testing set only.
plot(as.vector(t(Y[test,])), col = 'red', type = 'l', main = "Actual vs Predicted : testing set", ylab = "Y, Yp")
lines(as.vector(t(Yp[test,])), type = 'l', col = 'blue')


####################### vignette('rnn') ###################################
library(rnn)
set.seed(1)
help('trainr')

# Create sample iuputs
X1 <- sample(0:127, 5000, replace = TRUE) # replace = TRUE : 복원추출가능 , class : integer
X2 <- sample(0:127, 5000, replace = TRUE) # class : integer

# Create sample output
Y <- X1 + X2

# Convert to binary
X1 <- int2bin(X1)
X2 <- int2bin(X2)
Y <- int2bin(Y)

# Create 3d array : dim 1 : samples; dim 2 : time ; dim 3 : variables.
X <- array(c(X1, X2), dim = c(dim(X1), 2))
Y <- array(Y, dim = c(dim(Y),1))

# Train model
model <- trainr(Y = Y[,dim(Y)[2]:1,,drop = F],
                X = X[,dim(X)[2]:1,,drop = F],
                learningrate = 0.1,
                hidden_dim = 10,
                batch_size = 100,
                numepochs = 10)

plot(colMeans(model$error), type = 'l', xlab = 'epoch', ylab = 'errors')

# Create test inputs
A1 <- int2bin(sample(0:127, 7000, replace = TRUE))
A2 <- int2bin(sample(0:127, 7000, replace = TRUE))

# Create 3d array dim 1 : samples; dim 2 : time ; dim 3 : variables
A <- array(c(A1, A2), dim = c(dim(A1), 2))

# Predict
B <- predictr(model,
              A[,dim(A)[2]:1,,drop = F])

# Convert back to integers
A1 <- bin2int(A1)
A2 <- bin2int(A2)
B <- bin2int(B)

# Plot the difference
hist(B-(A1+A2))

                  ############# application neural network data to RNN ##################

x_AAPL_rnn_data <- as.data.frame(lapply(x_AAPL[2:6], normalize))

# x1 <- as.matrix(x_AAPL_rnn_data[2])
# x2 <- as.matrix(x_AAPL_rnn_data[3])
# x3 <- as.matrix(x_AAPL_rnn_data[4])
# x4 <- as.matrix(x_AAPL_rnn_data[5])

x1 <- t(x_AAPL_rnn_data[2])
x2 <- t(x_AAPL_rnn_data[3])
x3 <- t(x_AAPL_rnn_data[4])
x4 <- t(x_AAPL_rnn_data[5])


x <- array(c(x1,x2,x3,x4), dim = c(dim(x1), 4))
y <- array(t(x_AAPL_rnn_data[1]), dim = dim(x1))

# x <- array(c(x1,x2,x3,x4), dim = c(dim(x1), 4))
# y <- array(t(scale(x_AAPL[2])), dim = dim(x1))

x_train <- 1:180
x_test <- 181:204

# x_model <- trainr(Y = y[x_train,, drop = F],
#                   X = x[x_train,,, drop = F],
#                   learningrate = 0.1,
#                   hidden_dim = 10,
#                   batch_size = 1,
#                   numepochs = 500)

x_model <- trainr(Y = y[,x_train, drop = F],
                  X = x[,x_train,, drop = F],
                  learningrate = 0.035,
                  hidden_dim = 14,
                  batch_size = 1,
                  numepochs = 3400,
                  network_type = "lstm")

x_predict_lstm <- predict_lstm(x_model, x[,x_test,, drop = F])

#     ######### 반복문으로 최적 hidden_dim 찾기 ########### learning_rate = 0.1 / numepoch = 200
# 
# search_hidden = c()
# for(i in seq(5, 30)){
#   x_model <- trainr(Y = y[,x_train, drop = F],
#                     X = x[,x_train,, drop = F],
#                     learningrate = 0.1,
#                     hidden_dim = i,
#                     batch_size = 1,
#                     numepochs = 200)
#   print(i)
#   x_predict <- predictr(x_model, x[,x_test,, drop = F])
#   search_hidden = c(search_hidden, mean(data.frame(actual = y[,x_test], predicted = t(x_predict), error = (y[,x_test])-t(x_predict))$error))
#   
# }
# search_hidden_dim = data.frame(hidden_dim = seq(5, 30), mean_error = search_hidden)
# write.csv(search_hidden_dim, "search_hidden_dim.csv")
# 
#     ######### 반복문으로 최적 learning_rate 찾기 ########### hidden_dim = 14 / numepoch = 200
# 
# search_learning = c()
# for(i in seq(0.001, 0.1, by = 0.001)){
#   x_model <- trainr(Y = y[,x_train, drop = F],
#                     X = x[,x_train,, drop = F],
#                     learningrate = i,
#                     hidden_dim = 14,
#                     batch_size = 1,
#                     numepochs = 200)
#   print(i)
#   x_predict <- predictr(x_model, x[,x_test,, drop = F])
#   search_learning = c(search_learning, mean(data.frame(actual = y[,x_test], predicted = t(x_predict), error = (y[,x_test])-t(x_predict))$error))
# }
# search_learning_rate = data.frame(learning_rate = seq(0.001, 0.1, by = 0.001), mean_error = search_learning)
# write.csv(search_learning_rate, "search_learning_rate.csv")
# 
#     ######### 반복문으로 최적 numepoch 찾기 ########### hidden_dim = 14 / learning_rate = 0.035
# 
# search_epoch = c()
# for(i in seq(100, 5200, by = 300)){
#   x_model <- trainr(Y = y[,x_train, drop = F],
#                     X = x[,x_train,, drop = F],
#                     learningrate = 0.035,
#                     hidden_dim = 14,
#                     batch_size = 1,
#                     numepochs = i)
#   print(i)
#   x_predict <- predictr(x_model, x[,x_test,, drop = F])
#   search_epoch = c(search_epoch, mean(data.frame(actual = y[,x_test], predicted = t(x_predict), error = (y[,x_test])-t(x_predict))$error))
# }
# search_numepoch = data.frame(numepoch = seq(100, 5200, by = 300), mean_error = search_epoch)
# write.csv(search_numepoch, "search_numepoch.csv")







# x_predict <- predictr(x_model, x[x_test,,, drop = F])
x_predict <- predictr(x_model, x[,x_test,, drop = F])

# data.frame(actual = y[x_test,], predicted = x_predict, error = (y[x_test,]-x_predict))
data.frame(actual = y[,x_test], predicted = t(x_predict), error = (y[,x_test])-t(x_predict))

# plot(as.vector(t(y[x_test,])), col = 'red', type = 'l', main = 'Actual vs Predicted')
plot(as.vector(t(y[,x_test])), col = 'red', type = 'l', main = 'Actual vs Predicted')
lines(as.vector(x_predict), type = 'l', col = 'blue' )


plot(as.vector(x_model$error), type = 'l', col = 'red')



# application nnet data to RNN package ------------------------------------
# 유사한 코드 주석은 input data의 형태에 대해 실험해 보고자 한 것

setwd("/home/moon/R/study")
concrete <- read.csv("concrete.csv", stringsAsFactors = F)[-1]

library(rnn)
  # 데이터 정규화
normalize <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}
concrete <- as.data.frame(lapply(concrete, normalize))
# concrete <- as.data.frame(lapply(concrete, scale)) # scale로 자료 표준화하면 예측값이 좋지 않음 : 하면안됨

# max_index <- length(concrete$strength)
# slice_index <- round(max_index*0.75)
# concrete_train <- concrete[1:slice_index,]
# concrete_test <- concrete[slice_index:max_index,]

x1 <- as.matrix(concrete[1])
x2 <- as.matrix(concrete[2])
x3 <- as.matrix(concrete[3])
x4 <- as.matrix(concrete[4])
x5 <- as.matrix(concrete[5])
x6 <- as.matrix(concrete[6])
x7 <- as.matrix(concrete[7])
x8 <- as.matrix(concrete[8])

y1 <- as.matrix(concrete[9])

# x1 <- t(concrete[1])
# x2 <- t(concrete[2])
# x3 <- t(concrete[3])
# x4 <- t(concrete[4])
# x5 <- t(concrete[5])
# x6 <- t(concrete[6])
# x7 <- t(concrete[7])
# x8 <- t(concrete[8])
# 
# y1 <- t(concrete[9])

con_x <- array(c(x1,x2,x3,x4,x5,x6,x7,x8), dim = c(dim(x1), 8))
con_y <- array(y1, dim = dim(y1))

con_train <- 1:700
con_test <- 701:1030

# con_model <- trainr(Y = con_y[,con_train, drop = F],
#                     X = con_x[,con_train,, drop = F],
#                     learningrate = 0.1,
#                     hidden_dim = 10,
#                     batch_size = 1,
#                     numepochs = 300)

con_model <- trainr(Y = con_y[con_train,, drop = F],
                    X = con_x[con_train,,, drop = F],
                    learningrate = 0.1,
                    hidden_dim = 10,
                    batch_size = 1,
                    numepochs = 200)



# con_predict <- predictr(con_model, con_x[,con_test,, drop = F])
# data.frame(actual = t(con_y[,con_test, drop= F]), predict = t(con_predict), error = t((con_y[,con_test, drop= F])-(con_predict)))

con_predict <- predictr(con_model, con_x[con_test,,, drop = F])
data.frame(actual = con_y[con_test,, drop= F], predict = con_predict, error = (con_y[con_test,, drop= F])-(con_predict))


# plot(as.vector(con_y[,con_test, drop= F]), col = 'blue', type = 'l')
# lines(as.vector(con_predict), col = 'red', type = 'l')

plot(as.vector(con_y[con_test,, drop= F]), col = 'blue', type = 'l')
lines(as.vector(con_predict), col = 'red', type = 'l')























