#------------------------------------#
# R in Action (2nd ed): Chapter 1    #
# Introduction to R                  #
#------------------------------------#

# A sample R session
# q() is commented out so you don't accidently exit

# Listing 1.1 - A Sample R session

age <- c(1,3,5,2,11,9,3,9,12,3)
weight <- c(4.4,5.3,7.2,5.2,8.5,7.3,6.0,10.4,10.2,6.1)
mean(weight)
sd(weight)
cor(age,weight)
pdf("try1.pdf")
plot(age,weight)
# q()
dev.off()


## another example 
## illustrate the power of R
##  Ҳ???㲻֪????????ʲô,???????ǻ?ѧϰ????????????ǿ????
library(car)
scatterplotMatrix(~ income + education + prestige | type, data=Duncan)
scatterplotMatrix(~ income + education + prestige | type, data=Duncan,
                  regLine=FALSE, smooth=list(spread=FALSE))
scatterplotMatrix(~ income + education + prestige,
                  data=Duncan, id=TRUE, smooth=list(method=gamLine))

# Listing 1.2 - An example of commands used to manage the R Workspace. 

setwd("C:/myprojects/project1") # change the path to one of your directories
options()
options(digits=3)
x <- runif(20)
summary(x)
hist(x)
savehistory() #保存历史命令
save.image()  #保存工作空间
# q()


# Listing 1.3 - Working with a new package

help.start()
install.packages("vcd")
help(package="vcd")
library(vcd)
help(Arthritis)
Arthritis
example(Arthritis)
# q()


# save graphic
pdf("mygraph.pdf")
plot(age,weight)
dev.off()


