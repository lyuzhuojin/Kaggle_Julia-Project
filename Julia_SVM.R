setwd("~/Desktop")

#train = read.csv("resizedTrain.csv", as.is = T)
#test = read.csv("resizedTest.csv", as.is =T)
maintrain = read.csv("train.csv",as.is = T)
#test = read.csv("test.csv", as.is = T)

# Split training data into training and test
maintrain = maintrain[,c(2:401)]
set.seed(1)
train = sample(dim(maintrain)[1],dim(maintrain)[1]/2)
s_train = maintrain[train,]
test <- -sample(dim(maintrain)[1],dim(maintrain)[1]/2)
s_test <- maintrain[test,]
s_train_x <- s_train[,-1]
s_train_y <- s_train[,1]
s_test_x <- s_test[,-1]
s_test_y <- s_test[,1]

library("e1071")

# center and scale the data sets
s_train_x = scale(s_train_x,center=T, scale=T)
s_train_y = scale(s_train_y,center=T, scale=T)
s_test_x = scale(s_test_x,center=T, scale=T)
s_test_y = scale(s_test_y,center=T, scale=T)


# First, we are going to fit a support vector classifier with  cost of 10
svm.linear <- svm(s_train_x,s_train_y, data = dat.train, kernel = "linear",cost=10)
plot(svm.linear, s_train_x)

p1 = table(predict = predict(svm.linear, s_train_x), truth = s_train_y) 

#  polynomial kernel of degree 3.
svm.poly = svm(s_train_x, s_train_y, kernel = "polynomial",degree=3, cost = 10)
plot(svm.poly, s_train_x)
p2 = table(predict = predict(svm.poly, s_train_x), truth = s_train_y)

# radial kernel and a gamma of 1.
svm.radial <- svm(s_train_x, s_train_y,kernel = "radial", gamma = 1, cost = 10)
plot(svm.radial, s_train_x)
table(predict = predict(svm.radial, s_train_x), truth = s_train_y)


## The kernels above are very slow, even if the sized down data is used.
## For the first two, it stops after the message "maximum iterations reached"
## is displayed. Are you guys having the same problems?

 







