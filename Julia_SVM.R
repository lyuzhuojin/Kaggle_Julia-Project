setwd("~/Desktop")

main = read.csv("resizedTrain.csv", as.is = T)
labels = read.csv("trainLabels.csv", as.is=T)
maintrain = merge(main, labels, by = "ID")




# Split training data into training and test
#maintrain = maintrain[,c(2:101)]
set.seed(1)
train = sample(dim(maintrain)[1],dim(maintrain)[1]/2)
s_train = maintrain[train,]
test <- -sample(dim(maintrain)[1],dim(maintrain)[1]/2)
s_test <- maintrain[test,]
s_train_x <- s_train[,-1]
s_train_y <- s_train[,1]
s_test_x <- s_test[,-1]
s_test_y <- s_test[,1]

s_train_true = s_train_x$Class
s_train_true = factor(s_train_true)
s_test_true = s_test_x$Class
s_test_true = factor(s_test_true)

library("e1071")

s_train_x = s_train_x[,c(2:100)]
s_test_x = s_test_x[,c(2:100)]

# center and scale the data sets
s_train_x = scale(s_train_x,center=T, scale=T)
#s_train_y = scale(s_train_y,center=T, scale=T)
s_test_x = scale(s_test_x,center=T, scale=T)
#s_test_y = scale(s_test_y,center=T, scale=T)



# linear support vector classifier with  cost of 10
svm.linear <- svm(s_train_x,s_train_true, kernel = "linear",cost=10)


lpredict = predict(svm.linear, s_train_x)
p1 = as.data.frame(cbind(lpredict, s_train_true))
p1$diff = (p1$lpredict - p1$s_train_true)

names(p1) = c("prediction","truth","correct")

#This gives us an 84% accuracy! Now let's apply it to the real test data

test = read.csv("resizedTest.csv", as.is =T)
test = test[,c(2:101)]
test = scale(test, center=100, scale=T)
predict.test = predict(svm.linear, test)



#  polynomial kernel of degree 3.
svm.poly = svm(s_train_x, s_train_y, kernel = "polynomial",degree=3, cost = 10)
plot(svm.poly, s_train_x)

ppredict = predict(svm.poly, s_train_x)
p2 = as.data.frame(cbind(ppredict, s_train_y))
p2$diff = (p2$ppredict - p2$V2)

names(p2) = c("prediction","truth","correct")
s2 = subset(p2, p2$diff == 0)

# Again, 0% accuracy :(

#chi squared kernel

# radial kernel and a gamma of 1.
svm.radial <- svm(s_train_x, s_train_y,kernel = "radial", gamma = 1, cost = 10)
plot(svm.radial, s_train_x)

rpredict = predict(svm.radial, s_train_x)
p3 = as.data.frame(cbind(rpredict, s_train_y))
p3$diff = (p3$rpredict - p3$V2)
names(p3) = c("prediction","truth","correct")

s3 = subset(p3, p3$diff == 0)
# The bad news is, this gives us no differences of 0 which leads us to have a 
# 0% accuracy rate...



 







