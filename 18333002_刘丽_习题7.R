# install.packages("ggplot2")
library(ggplot2)
head(diamonds) # 查看数据集前5列

# 1. 绘制重量 “carat” 的直方图，标题 “Carat Histogram”，x 轴标签为 “Carat”。
bp<-qplot(carat, data = diamonds, geom = "histogram")
bp + ggtitle("Carat Histogram")
# bp + labs(title="Carat Histogram")

# 2.绘制价格 “price” 和重量 “carat” 的散点图。
qplot(carat,price,data=diamonds) 
# 3. 绘制重量 “carat” 的箱线图
qplot(cut,carat, data=diamonds, geom="boxplot")
# 4. 绘制重量 “carat” 的核密度估计图，并使用 “grey50” 进行填充。
qplot(carat, data = diamonds, geom = "density",colour="grey50")
# 5. 绘制价格 “price” 和重量 “carat” 的散点图，并使用颜色 “color” 变量分组显示。
qplot(carat,price,data=diamonds,colour=color) 
# 6. 绘制价格 “price” 和重量 “carat” 的散点图
##  a.使用命令 “facet_wrap” 命令按照颜色 “color” 分面显示；
bp<-qplot(carat,price,data=diamonds,colour=color) 
bp + facet_wrap(.~ color)
## b. 使用命令 “facet_grid” 命令按照质量 “cut” 和清洁度 “clarity” 分面显示。
bp + facet_grid(.~ cut)
bp + facet_grid(.~ clarity)
# 7.绘制不同质量 “cut” 下重量 “carat” 的箱线图。
qplot(cut, carat, data = diamonds, geom = 'boxplot', fill = cut)
# 8. 绘制不同质量 “cut” 下重量 “carat” 的小提琴图。
qplot(cut, carat, data = diamonds, geom = 'violin', fill = cut)
       