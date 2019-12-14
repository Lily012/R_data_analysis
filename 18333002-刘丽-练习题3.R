#习题1.编写一个函数，计算向量奇数的个数.
x<-c(1,2,3,4,5,6,7,8,9)
odd_function<-function(x){
  i<-1
  y<-rev(sort(x%%2))
  while(i<=length(y)&&y[i]==1){i<-i+1}
  i-1
}
odd_function(x)

#习题2.求g(x1,x2,x3) = (x21 + x2 − x3)2 + (x1 + x2 − x3)2 达到极值时的参数值; 当x1 > 0, x2 > 0, x3 > 0 时的极值是多少，参数值是多少?(提示:optim 函 数和 constrOptim 函数).
fr <- function(x){
  x1 <- x[1]
  x2 <- x[2]
  x3 <- x[3]
  (x1^2+x2-x3)^2 + (x1+x2^2-x3)^2
}
optim(c(1,2,2),fr)

grr=function(x){
  x1=x[1]
  x2=x[2]
  x3=x[3]
  c(2*(x1^2+x2-x3)*2*x1+2*(x1+x2^2-x3),
    2*(x1^2+x2-x3)+2*(x1+x2^2-x3)*2*x2,
    -2*(x1^2+x2-x3)-2*(x1+x2^2-x3))
}
uimat=matrix(c(1,0,0,0,1,0,0,0,1),nrow=3,ncol=3)
cimat=c(0,0,0)
cop=constrOptim(c(1,1,1),fr,grr,ui=uimat,ci=cimat)
cop

#习题3.求f(x) = |x − 3.5| + (x − 2)2 的极值。
fr = function(x){
  if(x>=3.5) x-3.5+(x-2)^2 
  else 3.5-x+(x-2)^2
}
grr = function(x){
  if (x>=3.5) 1+2*(x-2)
  else -1+2*(x-2)
}
optim(0,fr,grr)

#习题4.求minC=5x1+8x2,s.t.x1+x2 ≥2,x1+2x2 ≥3,x1,x2 ≥0.
install.packages("Rglpk")
library(Rglpk)
obj <- c(5,8)
mat <-matrix(c(1,1,1,2),2,2,F)
dir <- c(">=",">=")
rhs <- c(2,3)
max <- FALSE
Rglpk_solve_LP(obj,mat,dir,rhs,max = F)

#习题5.遍历名人 Wayans 家族中的儿童列表。Wayans 家庭中每一代人有多少个儿童?
wayans <- list(
  "Dwayne Kim" = list(), 
  "Keenen Ivory" = list( 
  "Jolie Ivory Imani", 
  "Nala",
  "Keenen Ivory Jr", 
  "Bella",
  "Daphne Ivory"
  ),
Damon = list(
  "Damon Jr", 
  "Michael",
  "Cara Mia",
  "Kyla"
  ),
Kim = list(),
Shawn = list( 
  "Laila",
  "Illia", 
  "Marlon"
  ),
Marlon = list(
  "Shawn Howell", 
  "Arnai Zachary" 
  ),
Nadia = list(), 
Elvira = list(
  "Damien", 
  "Chaunté"
  ),
Diedre = list(
  "Craig", 
  "Gregg", 
  "Summer", 
  "Justin", 
  "Jamel"
  ),
Vonnie = list()
  )
length(unlist(wayans))
(vapply(wayans,length,numeric(1)))

#习题6.state.x77 是 R 提供的一个数据集，它包含了关于美国各州的人口、收入和其他信息。输入它的名字就可以看到他的值:state.x77
#(1) 使用 summary 和 str 函数浏览数据集.
summary(state.x77)
str(state.x77)
#(2) 计算出每一列的平均值和标准差。(提示:使用 apply 函数)
apply(state.x77,2,mean)
apply(state.x77,2,sd)
