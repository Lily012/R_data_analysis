#R 语言练习题一 

#1. 访问http://www.r-project.org，下载R软件并安装；访问https://www.rstudio.com/， 下载 Rstudio 并安装。 

#2.回顾 R 包的几种不同安装方式
#利用 install.packages(“Rcmdr”) 命令安装 Rcmdr 包 
install.packages("Rcmdr")
#再用命令 load(“Rcmdr”) 加载程序包 Rcmdr 
load("Rcmdr") 
library(Rcmdr)
#请用命令或菜单的方法安装并加载 animation 程序包, 尝试如下两个例子 
#– buﬀon.needle(nmax = 500, interval = 0) 
#– f <- function(n) rchisq(n, 5);clt.ani(FUN = f) 
install.packages("animation")
buffon.needle(nmax = 500, interval = 0)
f <- function(n) rchisq(n, 5)
clt.ani(FUN = f)

#3. 将 1,2,…,20 构成两个 4*5 阶的矩阵，其中矩阵 A 是按列输入，矩阵 B 是按行输入， 并作如下运算。 
A <- matrix(1:20, nrow=4, ncol=5, byrow=TRUE)
A
B <- matrix(1:20, nrow=4, ncol=5, byrow=FALSE)
B
#（1）C=A+B
C <- A + B
C
#（2）D = ATB
D <- t(A)%*%B
D
#（3）E =(eij)n×n 其中eij = aij ·bij; 
E <- A%*%B
#（4）F 是有 A 的前 3 行和前 3 列构成矩阵
F <- A[0:3,0:3]
F
#（5）G 是有矩阵 B 的各列构成的矩阵，但不含 B 的第 3 列。 
G <- B[,-3]
G

# 4. 构造一个向量x, 向量是由 5 个 1，3 个 2，4 个 3 和 2 个 4 构成，注意用到 rep() 函 数。 
x <- c(rep(1:4,times=c(5,3,4,2)))
x

#5. 已知有5名学生的数据，如下表所示，用数据框的形式读入数据。
number <- c(1, 2, 3, 4, 5) 
name <- c("张三", "李四", "王五", "赵六","丁一") 
gender <- c("女", "男", "女", "男","女") 
age <- c("14", "15", "16", "14","15")
height <- c("156", "165", "157", "162","159") 
weight <- c("42", "49", "41.5", "52","45.5") 
student <- data.frame(number,name,gender,age,height,weight) 
student

#6. 将 5 题中的数据表写成一个纯文本文件，用函数 read.table() 读该文件，然后再用
#1函数 write.csv() 写成一个能用 Excel 表能打开的文件，并用 Excel 表打开。 
write.table(student,"student.txt")
data <- read.table('student.txt',sep = '\t',header = TRUE)
write.csv(data,"student.csv")

#7. 我们想要分析一组儿童样本的特征。这些来自法国东南部波尔多市的儿童在他们进 
#入到幼儿园的第一年（1996-1997）经历了一次医学检查。
#下表列出了 10 个 3~4 岁的儿童 样本数据观测数据。 
#在 R 中尝试完成下列操作： 
#1）选择适当的数据结构保存上述数据； 
name <- c("Edward", "Cynthia", "Eugene", "Elizabeth","Patrick","John","Albert","Lawrence","Joseph","Leo") 
gender <- c("G", "G", "B", "G","B","B","B","B","B","B") 
Zep <- c("Y", "Y", "Y", "Y","N","Y","N","Y","Y","Y")
Weight <- c(16, 14,13.5, 15.4,16.5,16,17,14.8,17,16.8)
Years <- c(3, 3, 3, 4,3,4,3,3,4,3) 
Months <- c(5, 10, 5, 0,8,0,11,9,1,3) 
Height <- c(100, 97, 95.5, 101,100,98.5,103,98,101.5,100) 
Children <- data.frame(name,gender,Zep,Weight,Years,Months,Height) 
Children
#2）计算各个变量（Weight、Height）的均值； 
mean(Weight)
mean(Height)
#3）计算每个个儿童的体重指数（BMI），再把所得到的体重指数结果保存在BMI的变量中； 
Height2 <- Height/100
BMI <- Weight/(Height2*Height2)
BMI
#4）将 Weight 视为 Height 的一个函数来绘制一个散点图，添加一个标题。
dotchart(Height, labels=Weight, cex=.7, 
         main="身高体重散点图", 
         xlab="Height")

















