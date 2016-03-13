setwd("G:/MyProject/R") #set current work directory
options() #check the configuration.
options(digits=3)
x <- runif(20) #均匀分布选出20个数
summary(x) #output x
hist(x) #直方图
savehistory()
save.image()
