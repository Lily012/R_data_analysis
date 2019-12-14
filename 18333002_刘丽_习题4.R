#****************************1*****************************
#(1)找出各州中失业人员的百分比和人们投票支持奥巴马的百分比之间的（Pearson）相关性。
install.packages('learningr')
data(obama_vs_mccain,package = "learningr")
head(obama_vs_mccain)
cor(obama_vs_mccain$Obama,obama_vs_mccain$Unemployment, method = "pearson")
#（2）画出两个变量的散点图，使用你自己所选择的图形系统。
# ggplot2作图系统
library(ggplot2)
ggplot(data = obama_vs_mccain, 
       mapping = aes(x = Obama, y = Unemployment)) +
  geom_point() +
  facet_wrap(~ State, ncol = 8)
#****************************2*****************************
#绘制出车手最快时间的分布，把是否有（涉嫌）使用药物车手分开比较。
data(alpe_d_huez2,package="learningr")
head(alpe_d_huez2)
#（1）直方图
# ggplot2作图系统
ggplot(data = alpe_d_huez2, 
       mapping = aes(NumericTime, ..density..)) +
  geom_histogram(bins = 6) +
  labs(title = "histogram of NumericTime")
#（2）箱线图
ggplot(data = alpe_d_huez2, 
       mapping = aes(x = DrugUse, y = NumericTime)) +
  geom_boxplot() +
  labs(x = "DrugUse",
       y = "NumericTime",
       title = "boxplot of NumericTime vs. DrugUse")
#****************************3*****************************
#gonorrhoea 数据集中包含了美国不同年份、年龄、种族和性别的淋病感染率。
data(gonorrhoea,package="learningr")
head(gonorrhoea)
#研究感染率随着年龄的增长如何变化。是否有一个时间趋势？
boxplot(Rate ~ Age.Group, data = gonorrhoea,
        main = "boxplot of Rate vs. Age.Group",
        xlab = "Age.Group", 
        ylab = "Rate")
#种族和性别是否会影响感染率？
#感染率与种族
ggplot(data = gonorrhoea, 
       mapping = aes(x = Ethnicity, y = Rate)) +
  geom_boxplot() +
  labs(x = "Ethnicity",
       y = "Rate",
       title = "boxplot of Rate vs. Ethnicity")
#感染率与性别
ggplot(data = gonorrhoea, 
       mapping = aes(x = Gender, y = Rate)) +
  geom_boxplot() +
  labs(x = "Gender",
       y = "Rate",
       title = "boxplot of Rate vs. Gender")
#****************************4*****************************
#画出以下函数的图形：
# y=3x+2,x ≤ 3   y=2x-0.5x^2,x > 3,x ∈ [0, 6]
f=function(x){
  if(x<=(3))
  {y<- 3*x+2}
  else if(x>3&x<=6)
  {y<- 2*x - 0.5 *x^2}
  return(f)
}
curve(3*x+2,0,3,xlim=c(0,6),ylim=c(-10,10))
curve(2*x-0.5*x^2, 3, 6, add = TRUE) 
#****************************5*****************************
#模拟得到 1000 个参数为 0.3 的贝努里分布随机数, 并用图示表示出来.
rbinom(1000,1,0.3)
plot(rbinom(1000,1,0.3))
#****************************6*****************************
#用命令rnorm()产生1000个均值为10,方差为4的正态分布随机数,用直方图呈现数据的分布并添加核密度曲线

x=rnorm(1000,mean=10,sd=2)#生成正态分布随机数
hist(x,xlim=c(min(x),max(x)),probability=T,nclass=max(x)-min(x)+1,
     col='lightblue',main='Normal distribution')#绘制直方图
lines(density(x,bw=1),col='red',lwd=3)#添加核密度曲线
#****************************7*****************************
#模拟得到三个t分布混合而成的样本, 用直方图呈现数据的分布并添加核密度曲线
x<-sample(c(rt(10,1),rt(10,2),rt(10,10)),1000,replace=T)
hist(x,xlim=c(min(x),max(x)),probability=T,
     nclass=max(x)-min(x)+1,col='lightblue',
     main=c("3个t分布混合样本直方图"))
lines(density(x,bw=1),col='red',lwd=2)
#****************************8*****************************
#由程序包 DAAG 中的数据集 possum,
#1) 利用函数 hist(possum$age) 作出负鼠年龄的直方图. 试选用两种不同的断点并作比较,说明两图的不同之处;
install.packages('DAAG')
library(DAAG)
data(possum)
head(possum)
hist(possum$age,breaks = 1+(0:10)*1,
     ylim = c(0,30),
     xlab = 'age',
     main = '1:break at 1,2,3,4,5...')
hist(possum$age,breaks = 1+(0:5)*2,
     ylim = c(0,60),
     xlab = 'age',
     main = '2:break at 1,3,5,7...')
#2) 求出负鼠年龄变量的均值、标准差、中位数以及上下四分位数
summary(possum$age)
NApossum <-subset(possum,age!="NA")
summary(NApossum$age)
mean(NApossum$age)
sd(NApossum$age)
median(NApossum$age)
quantile(NApossum$age)
#****************************9*****************************
#考虑程序包DAAG中的数据集tinting,
#1)获得变量tint和sex的列联表;
library(DAAG)
data(tinting)
head(tinting)#二维列联表是指按照两个分类变量列出的频数表：
table(tinting$tint,tinting$sex)
#2)在同一图上作出变量sex与tint的联合柱状图;
ggplot(data = tinting, 
       mapping = aes(sex, ..tint..)) +
  geom_histogram(bins = 6) +
  labs(x = "sex",
       y = "tint",
       title = "Sex and Tint")
barplot(table(tinting$tint,tinting$sex)) #联合柱状图
#3)作出age和it的散点图, 并进一步完成下面的操作:
library(ggplot2)
data(tinting)
plot(tinting$age ~ tinting$it,
     xlab = "Age",
     ylab = "It"
)
#ggplot(tinting,aes(x=age,y=it))+geom_point()
#用函数 lowness() 作出拟合线;
abline(lm(tinting$age ~ tinting$it), col="red",lwd=2, lty=1)
lines(lowess(tinting$age, tinting$it),lwd=2)#非线性拟合
#在图的两边加上更细小的刻度
rug(side=2, jitter(tinting$age, 5))
rug(side=1, jitter(tinting$it, 10))
#在图的两边加上箱型图.
par(mar=c(0,5,1,1))
barplot(tinting$age, axes=FALSE, space=0)
par(mar=c(5,0,1,1))
barplot(tinting$it, axes=FALSE,space=0, horiz=TRUE)
#4)作出age和it关于因子变量tint的条件散点图;
#ggplot(tinting, aes(x = age, y = it, colour = tint))+geom_point()
coplot(tinting$age~tinting$it|tinting$tint)
#5)作出age和it关于因子变量tint和sex的条件散点图;
#ggplot(tinting, aes(x = age, y = it, size = tint, colour = sex)) +geom_point(alpha = .5)  
coplot(tinting$age~tinting$it|tinting$tint*tinting$sex)
#6)做出it与csoa的等高线图;
install.packages('kde2d')
library(kde2d)
z<-kde2d(tinting$it,tinting$csoa)
contour(z,col="red",drawlabels=FALSE)
#7)使用matplot( )描述变量age,it和csoa.
d<-data.frame(y1=tinting$age,y2=tinting$it,y3=tinting$csoa)
matplot(d,type='l',main="matplot")
#****************************10*****************************
#假定某校100名女生的血清总蛋白含量(g/L)服从均值为75,标准差为3,
#并假定数据由下面的命令产生
options(didits=4)
db<-rnorm(100,75,9)
#1) 计算样本均值、方差、标准差、极差、四分位极差、变异系数、偏度、峰度和五数概括;
mean(db) #均值
sd(db) #方差
sqrt(sd(db))#标准差
max(db)-min(db)#极差
mad(db)#四分位极值
sd(db)/mean(db)#变异系数
install.packages("fBasics")
library(fBasics)
skewness(db)#偏度
kurtosis(db)#峰度
fivenum(db)#五数概括
#2) 画出直方图、核密度估计曲线、经验分布图和QQ图;
hist(db,xlim=c(min(db),max(db)),probability=T,
     nclass=max(db)-min(db)+1,col='lightblue',main="直方图") 
lines(density(db),col='red',lwd=3)
qqnorm(db,main="QQ图")
qqline(db,col='red')
x<-sort(db)
n<-length(x)
y<-(1:n)/n
m<-mean(db)
s<-sd(db)
plot(x,y,type='s',main="经验分布图")
#3) 画出茎叶图、框须图
curve(pnorm(x,m,s),col='red',lwd=2,add=T)#茎叶图
stem(db)
boxplot(db,main="框须图")
#****************************11*****************************
#某校测得20名学生的四项指标: 性别、年龄、身高(cm)和体重(kg),具体数据如表所示
install.packages("RODBC") #从Excel读入数据
library(RODBC)
setwd("C:\\Users\\Administrator\\Desktop\\研究生\\研二\\R语言")
student_data <- read.table("studentdata.xls",,header = TRUE)
head(student_data)
z<-odbcConnectExcel("C:/Users/Administrator/Desktop/研究生/研二/R语言/学生指标数据.xls") 
data<-sqlFetch(z,"Sheet1")
close(z)
#1) 绘制体重对身高的散点图;
plot(data$体重~data$身高,main="体重对身高散点图") 
#2) 绘制不同性别下, 体重对身高的散点图;
coplot(data$体重~data$身高|data$性别)
#3) 绘制不同年龄阶段, 体重对身高的散点图;
coplot(data$体重~data$身高|data$年龄)
#4) 绘制不同性别和不同年龄阶段, 体重对身高的散点图
coplot(data$体重~data$身高|data$性别*data$年龄)


