#-----------------------------------#
# R in Action (2nd ed): Chapter 2   #
# Creating a dataset                #
#-----------------------------------#
# install.packages(c(" ",""))


#  Creating vectors and Extracting elements
a <- c(1, 2, 5, 3, 6, -2, 4)
b <- c("one", "two", "three")
c <- c(TRUE, TRUE, TRUE, FALSE, TRUE, FALSE)


d <- seq(1,10,by=0.5) # = seq(from=1,to=10,by=0.5)
e <- seq(1,10,length=100) # = seq(1,10,length.out=21)
f <- rep(2:5,2) # = rep(2:5, times=2)
g <- rep(2:5,rep(2,4))
fg<- c(f,g)

h <- rep(1:3, times = 4, each = 2)
n <- 1:10


k <- a[c(2,5)]
s <- a[-6]
s <- a[a>0]

# Using vector subscripts
a <- c(1, 2, 5, 3, 6, -2, 4)
a[3]
a[c(1, 3, 5)]
a[2:6]                     


a <- c("k", "j", "h", "a", "c", "m")
a[3]
a[c(1, 3, 5)]
a[2:6]


# Listing 2.1 - Creating Matrices
y <- matrix(1:20, nrow=5)
y
cells    <- c(1,26,24,68)
rnames   <- c("R1", "R2")
cnames   <- c("C1", "C2") 
mymatrix <- matrix(cells, nrow=2, ncol=2, byrow=TRUE,
                   dimnames=list(rnames, cnames)) 
mymatrix
mymatrix <- matrix(cells, nrow=2, ncol=2, byrow=FALSE,
                   dimnames=list(rnames, cnames))
mymatrix


#一个矩阵的例子
#图片处理
setwd("E:/2.教学/1. 教学资源/1. 统计学专业课及论文指导/R 语言/R语言课程建设/R in Action(2)/R in Action（2）code and books/RiA2_SourceCode/RiA2 Code")


library(pixmap)
mt<-read.pnm("1.pgm")

mt
plot(mt)
locator() #确定要覆盖的位置
mt@grey[8:48,42:75]<-1
plot(mt)

# 马赛克
# adds random noise to img, at the range rows,cols of img; img and the
# return value are both objects of class pixmap; the parameter q
# controls the weight of the noise, with the result being 1-q times the
# original image plus q times the random noise
blurpart <- function(img,rows,cols,q) {
  lrows <- length(rows)
  lcols <- length(cols)
  newimg <- img
  randomnoise <- matrix(nrow=lrows, ncol=lcols,runif(lrows*lcols))
  newimg@grey[rows, cols] <- (1-q) * img@grey[rows, cols] + q * randomnoise
  return(newimg)
}

mt1<-blurpart(mt,8:48,42:75,0.65)
plot(mt1)




# Listing 2.2 - Using matrix subscripts
x <- matrix(1:10, nrow=2)
x
x[2,]
x[,2]
x[1,4]
x[1, c(4,5)]

# 矩阵计算
lambda<-2
A<-matrix(c(2,3,5,4),nrow=2,ncol=2)
B<-matrix(c(1,2,2,7),nrow=2,ncol=2)

##四则运算
lambda+A
A+B
A-B
## 数乘
lambda*A
## 转置
t(A)
# 矩阵对应元素乘除
A*B 
A/B
## 矩阵乘除
A%*%B

## 矩阵的拟
solve(B)

##矩阵相除
A%*%solve(B)

## A^TB
crossprod(A,B)

## 矩阵外积
x<-seq(1,4)
y<-seq(4,7)
outer(x,y,FUN="*")

## Kronecker 乘积
kronecker(A,B)


## 三角矩阵
M<-matrix(1:16,nrow=4)
lower.tri(M)
upper.tri(M)
M[lower.tri(M)]<-0

## 矩阵的行列式等
det(A)

## 矩阵的特征值
eigen(A)

## see the package "matrixcalc" for more functions.


# Listing 2.3 - Creating an array
dim1 <- c("A1", "A2")
dim2 <- c("B1", "B2", "B3")
dim3 <- c("C1", "C2", "C3", "C4")
z <- array(1:24, c(2,3,4), dimnames=list(dim1, dim2, dim3))
z


# Listing 2.4 - Creating a dataframe
patientID <- c(1, 2, 3, 4)
age <- c(25, 34, 28, 52)
diabetes <- c("Type1", "Type2", "Type1", "Type1")
status <- c("Poor", "Improved", "Excellent", "Poor")
patientdata <- data.frame(PatientIDD=patientID, age, diabetes, status)
patientdata


d# Listing 2.5 - Specifying elements of a dataframe
patientdata[1:2]
patientdata[c("diabetes","status")]
patientdata$age                       


# other functions related to dataframe
nrow(patientdata)
ncol(patientdata)
dim(patientdata)
names(patientdata)
rownames(patientdata)
head(patientdata)
tail(patientdata)


# # An example to illustrate the usage of "attach","detach()"
attach(mtcars)
summary(mpg)
plot(mpg, disp)
plot(mpg, wt)
detach(mtcars)  # The statement is optional but is good
                # programming practice and should be included routinely.

## the importance of 'detach()'
mpg <- c(25, 36, 47)
attach(mtcars)
plot(mpg, wt)
mpg

## an example for the function 'with'

with(mtcars, {
  summary(mpg, disp, wt)
  plot(mpg, disp)
  plot(mpg, wt)
})

#local variable
with(mtcars, {
  stats <- summary(mpg)
  stats
})

#
with(mtcars, {
  nokeepstats <- summary(mpg)
  keepstats <<- summary(mpg)
})



#factor 
diabetes <- c("Type1", "Type2", "Type1", "Type1") 
#as.numeric(diabetes)
diabetes <- factor(diabetes)#(1,2,1,1),1=type1,2=type2

#vectors representing ordinal variables
status <- c("Poor", "Improved", "Excellent", "Poor")
status <- factor(status, ordered=TRUE)

##Override the default by specifying a levels option
status <- factor(status, ordered=TRUE,
                 levels=c("Poor", "Improved", "Excellent"))


##
sex<-c(1,2,2,1,2)
sex <- factor(sex, levels=c(1, 2), labels=c("Male", "Female"))
sex

# 有序变量
z<-ordered(c("Small","Tall","Average","Tall","Average","Small","Small"),
           levels=c("Small","Average","Tall"))
class(z)

# gl funciton #Generate factors by specifying the pattern of their levels.
gl(n=2,k=8,labels=c("Control","Treat"))
   
# Listing 2.6 - Using factors
patientID <- c(1, 2, 3, 4)
age <- c(25, 34, 28, 52)
diabetes <- c("Type1", "Type2", "Type1", "Type1")
status <- c("Poor", "Improved", "Excellent", "Poor")
diabetes <- factor(diabetes)
status <- factor(status, order=TRUE)
patientdata <- data.frame(patientID, age, diabetes, status)
str(patientdata)                               
summary(patientdata)





# Listing 2.7 - Creating a list
g <- "My First List"
h <- c(25, 26, 18, 39)
j <- matrix(1:10, nrow=5)
k <- c("one", "two", "three")
mylist <- list(title=g, ages=h, j, k)
mylist


# Entering data interactively from the keyboard
mydata <- data.frame(age=numeric(0),
                     gender=character(0), weight=numeric(0))
mydata <- edit(mydata)



fix(mydata)


# Entering data inline
mydatatxt <- "
age gender weight
25 m 166
30 f 115
18 f 120
"


mydata <- read.table(header=TRUE, text=mydatatxt)


# Importing data from a delimited text file

# First, save the following 4 lines in a file named 
# "studentgrades.csv" in the current working director

StudentID,First,Last,Math,Science,Social Studies
011,Bob,Smith,90,80,67
012,Jane,Weary,75,,80
010,Dan,"Thornton, III",65,75,70
040,Mary,"O'Leary",90,95,92



setwd("E:\\2.教学\\1. 教学资源\\1. 统计学专业课及论文指导\\R 语言\\R语言课程建设\\R in Action(2)\\R in Action（2）code and books\\RiA2_SourceCode\\RiA2 Code")
# Next, read the data into R using the read.table() function
grades <- read.table("studentgrades.txt", header=TRUE,
                     row.names="StudentID", sep=" ")
grades # print data frame
str(grades) # view data frame structure

write.csv(grades,file="studentgrades.csv")

# Alternatively, import the data while specifying column classes 
grades <- read.csv("studentgrades.csv", header=TRUE,
                     row.names="StudentID", sep=",",
                     colClasses=c("character", "character", "character",
                                  "numeric", "numeric", "numeric"))


grades # print data frame
str(grades) # view data frame structure


# another example by read.table or read.csv
teachingratings<-read.table("teachingratings.txt",header=T)
#read.csv()

####  more controlled parameters can be seen by using "help(read.table)"

## read data from  the clipboard  

y<-read.table("clipboard",header=T,sep='')

mydata <- read.delim("clipboard")

# from internet
theUrl<-"http://www.jaredlander.com/data/Tomato%20First.csv"
tomato<-read.table(file=theUrl,header=TRUE,sep=",")


# read from spss
library(foreign)
teachingratings_sav <- read.spss("teachingratings.sav")



#另一个包
library(Hmisc)
mydataframe<-spss.get("teachingratings.sav",use.value.labels = TRUE)


# import data from SAS

library(Hmisc)
datadir <- "C:/mydata"
sasexe <- "C:/Program Files/SASHome/SASFoundation/9.4/sas.exe"
mydata <- sas.get(libraryName=datadir, member="clients", sasprog=sasexe)




#from internet
library(XML)
theURL<-"http://www.jaredlander.com/2012/02/another-kind-of-super-bowl-pool"
bowPool<-readHTMLTable(theURL,which=1,header=FALSE,stringsAsFactors=FALSE)


#scan  见R语言编程艺术

md<-scan("md.txt",what=" ")
v<-scan("")




## Some useful funtions

#  采用列命令获取文件路径
file.choose()
#  读取列联表数据
read.ftable()
#  使用R.matlab读取.mat文件
readMat()




