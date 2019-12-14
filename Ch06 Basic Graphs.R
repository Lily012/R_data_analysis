#---------------------------------------------------------------#
# R in Action (2nd ed): Chapter 6                               #
# Basic graphs                                                  #
# requires packages vcd, plotrix, sm, vioplot to be installed   #
# install.packages(c("vcd", "plotrix", "sm", "vioplot"))        #
#---------------------------------------------------------------#

par(ask=TRUE)
opar <- par(no.readonly=TRUE) # save original parameter settings

###Graphs of probability mass functions
library(ggplot2)
### Graphs of probability mass functions#####
# Binomial
n<-20
p<-0.2
k<-seq(0,n)
plot(k,dbinom(k,n,p),type='h',main='Binomial distribution,
     n=20, p=0.2',xlab='k')

#or using ggplot

ggplot(data.frame(x=k,y=dbinom(k,n,p)))+aes(x=x,y=y)+geom_point( )+
  labs(x="k",y="Density")

# Poisson
lambda<-4.0; k<-seq(0,20)
plot(k,dpois(k,lambda),type='h',
     main="Poisson distribution, lambda=4.0",xlab="k")


# Normal
curve(dnorm(x,0,1), xlim=c(-5,5), ylim=c(0,.8),
      ylab=expression(paste("dnorm(x, ", mu, ",", sigma,")")),
      col='red', lwd=2, lty=3)
curve(dnorm(x,0,2), add=T, col='blue', lwd=2, lty=2)
curve(dnorm(x,0,1/2), add=T, lwd=2, lty=1)
title(main="Gaussian distributions")
ex.cs <- c(expression(sigma==1),
           expression(sigma==2), expression(sigma==1/2))
legend(par('usr')[2], par('usr')[4], xjust=1, ex.cs,
       lwd=c(2,2,2),lty=c(3,2,1),col=c('red', 'blue', par("fg")))


## See 'Rcmdr' for more distribution plots.

library(vcd)
counts <- table(Arthritis$Improved)
counts

## 四种画图系统： base, grid, lattice, ggplot2

# Listing 6.1 - Simple bar plot
# vertical barplot
barplot(counts, 
        main="Simple Bar Plot",
        xlab="Improvement", ylab="Frequency")
# horizontal bar plot   
barplot(counts, 
        main="Horizontal Bar Plot", 
        xlab="Frequency", ylab="Improvement", 
        horiz=TRUE,las=2)


# obtain 2-way frequency table
library(vcd)
counts <- table(Arthritis$Improved, Arthritis$Treatment)
counts


# Listing 6.2 - Stacked and grouped bar plots 
# stacked barplot
barplot(counts, 
        main="Stacked Bar Plot",
        xlab="Treatment", ylab="Frequency", 
        col=c("red", "yellow","green"),            
        legend=rownames(counts)) 

# grouped barplot                       
barplot(counts, 
        main="Grouped Bar Plot", 
        xlab="Treatment", ylab="Frequency",
        legend=rownames(counts), beside=TRUE)


# Listing 6.3 - Bar plot for sorted mean values
states <- data.frame(state.region, state.x77)
means <- aggregate(states$Illiteracy, by=list(state.region), 
                   FUN=mean)
means

means <- means[order(means$x),]  
means

barplot(means$x, names.arg=means$Group.1) 
title("Mean Illiteracy Rate")  


## see an example for a barplot with Ci
library(gplots)
data(VADeaths, package = "datasets")
#Death rates per 1000 in Virginia in 1940

mp <- barplot2(VADeaths) # default
hh <- t(VADeaths)[, 5:1]
mybarcol <- "gray20"
ci.l <- hh * 0.85
ci.u <- hh * 1.15
mp <- barplot2(hh, beside = TRUE,
               col = c("lightblue", "mistyrose",
                       "lightcyan", "lavender"),
               legend = colnames(VADeaths), ylim = c(0, 100),
               main = "Death Rates in Virginia", font.main = 4,
               sub = "Faked 95 percent error bars", col.sub = mybarcol,
               cex.names = 1.5, plot.ci = TRUE, ci.l = ci.l, ci.u = ci.u,
               plot.grid = TRUE)
mtext(side = 1, at = colMeans(mp), line = -2,
      text = paste("Mean", formatC(colMeans(hh))), col = "red")
box()

## in ggplot
## error bars

library(gcookbook) # For the data set
# Take a subset of the cabbage_exp data for this example
ce <- subset(cabbage_exp, Cultivar == "c39")
# With a bar graph
ggplot(ce, aes(x=Date, y=Weight)) +
  geom_bar(fill="white", colour="black",stat="Identity") +
  geom_errorbar(aes(ymin=Weight-se, ymax=Weight+se), width=.2)


# Listing 6.4 - Fitting labels in bar plots
par(las=2)                # set label text perpendicular to the axis
par(mar=c(5,8,4,2))       # increase the y-axis margin
counts <- table(Arthritis$Improved) # get the data for the bars

# produce the graph
barplot(counts, 
        main="Treatment Outcome", horiz=TRUE, cex.names=0.8,
        names.arg=c("No Improvement", "Some Improvement", "Marked Improvement")
)
par(opar)


# Spinograms
library(vcd)
attach(Arthritis)
counts <- table(Treatment,Improved)
spine(counts, main="Spinogram Example")
detach(Arthritis)

## see Chapter 3 of R graphic book 
## ggplot2
## an example 
library(ggplot2)
library(gcookbook)
ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) +
  geom_bar(stat="identity")

ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) +
  geom_bar(stat="identity", width=0.5, position=position_dodge(0.7))

# Listing 6.5 - Pie charts
opar<-par(mfrow=c(2,2))                             
slices <- c(10, 12,4, 16, 8) 
lbls <- c("US", "UK", "Australia", "Germany", "France")

pie(slices, labels = lbls, 
    main="Simple Pie Chart")

pct <- round(slices/sum(slices)*100)                      
lbls <- paste(lbls, pct) 
lbls <- paste(lbls,"%",sep="")
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Pie Chart with Percentages")

library(plotrix)                                               
pie3D(slices, labels=lbls,explode=0.2,
      main="3D Pie Chart ")

mytable <- table(state.region)                                   
lbls <- paste(names(mytable), "\n", mytable, sep="")
pie(mytable, labels = lbls, 
    main="Pie Chart from a dataframe\n (with sample sizes)")

par(opar)


# Fan plots
library(plotrix)
slices <- c(10, 12,4, 16, 8) 
lbls <- c("US", "UK", "Australia", "Germany", "France")   
fan.plot(slices, labels = lbls, main="Fan Plot")


# Listing 6.6 - Histograms
par(mfrow = c(2, 2))
# simple histogram                                                        1
hist(mtcars$mpg)

# colored histogram with specified number of bins        
hist(mtcars$mpg, 
     breaks=20, 
     col="red", 
     xlab="Miles Per Gallon", 
     main="Colored histogram with 12 bins")

# colored histogram with rug plot, frame, and specified number of bins 
hist(mtcars$mpg, 
     freq=FALSE, 
     breaks=12, 
     col="red", 
     xlab="Miles Per Gallon", 
     main="Histogram, rug plot, density curve")  
rug(jitter(mtcars$mpg)) 
lines(density(mtcars$mpg), col="blue", lwd=2)

# histogram with superimposed normal curve (Thanks to Peter Dalgaard)  
x <- mtcars$mpg 
h<-hist(x, 
        breaks=12, 
        col="red", 
        xlab="Miles Per Gallon", 
        main="Histogram with normal curve and box") 
xfit<-seq(min(x),max(x),length=40) 
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="blue", lwd=2)
box()

# restore original graphic parameters
par(opar)


# using ggplot
library(ggplot2)
p<-ggplot(data=mtcars)
p+geom_histogram(aes(x=mpg),binwidth=2)
p+geom_density(aes(x=mpg))


ggplot(faithful, aes(x=waiting)) + geom_histogram()
ggplot(faithful, aes(x=waiting)) +
  geom_histogram(binwidth=4, fill="white", colour="black")


library(MASS) # For the data set
# Use smoke as the faceting variable
ggplot(birthwt, aes(x=bwt)) + geom_histogram(fill="white", colour="black") +
  facet_grid(smoke ~ .)


birthwt1 <- birthwt
birthwt1$smoke <- factor(birthwt1$smoke)
# Map smoke to fill, make the bars NOT stacked, and make them semitransparent
ggplot(birthwt1, aes(x=bwt, fill=smoke)) +
  geom_histogram(position="identity", alpha=0.4)


# Listing 6.7 - Kernel density plot
d <- density(mtcars$mpg) # returns the density data  
plot(d) # plots the results 

d <- density(mtcars$mpg)                                  
plot(d, main="Kernel Density of Miles Per Gallon")       
polygon(d, col="red", border="blue")                     
rug(mtcars$mpg, col="brown") 


# Listing 6.8 - Comparing kernel density plots
par(lwd=2)                                                       
library(sm)
attach(mtcars)

# create value labels 
cyl.f <- factor(cyl, levels= c(4, 6, 8),                               
                labels = c("4 cylinder", "6 cylinder", "8 cylinder")) 

# plot densities 
sm.density.compare(mpg, cyl, xlab="Miles Per Gallon")                
title(main="MPG Distribution by Car Cylinders")

# add legend via mouse click
colfill<-c(2:(2+length(levels(cyl.f)))) 
cat("Use mouse to place legend...","\n\n")
legend(locator(1), levels(cyl.f), fill=colfill) 
detach(mtcars)
par(lwd=1)



#use ggplots  
library(ggplot2)
qplot(mpg,data=mtcars,geom=c("density"),facets=cyl~.)
qplot(mpg,data=mtcars,geom=c("density"),fill=cyl.f)
qplot(mpg,data=mtcars,geom=c("density"),colour=cyl.f)

ggplot(data=mtcars,aes(x=mpg,group=cyl.f))+geom_density()
ggplot(data=mtcars,aes(x=mpg,fill=cyl.f))+geom_density()
ggplot(data=mtcars,aes(x=mpg,colour=cyl.f))+geom_density()

# --Section 6.5--

# parallel box plots
boxplot(mpg~cyl,data=mtcars,
        main="Car Milage Data", 
        xlab="Number of Cylinders", 
        ylab="Miles Per Gallon")


# notched box plots
boxplot(mpg~cyl,data=mtcars, 
        notch=TRUE, 
        varwidth=TRUE,
        col="red",
        main="Car Mileage Data", 
        xlab="Number of Cylinders", 
        ylab="Miles Per Gallon")
## use ggplot

ggplot(mtcars, aes(x=as.factor(cyl), y=mpg)) + geom_boxplot()
ggplot(mtcars, aes(x=as.factor(cyl), y=mpg)) + geom_boxplot(notch = TRUE)

#add means to boxplot
ggplot(mtcars, aes(x=factor(cyl), y=mpg)) + geom_boxplot() +
  stat_summary(fun.y="mean", geom="point", shape=23, size=3, fill="white")


# Listing 6.9 - Box plots for two crossed factors
# create a factor for number of cylinders
mtcars$cyl.f <- factor(mtcars$cyl,
                       levels=c(4,6,8),
                       labels=c("4","6","8"))

# create a factor for transmission type
mtcars$am.f <- factor(mtcars$am, 
                      levels=c(0,1), 
                      labels=c("auto","standard"))

# generate boxplot
boxplot(mpg ~ am.f *cyl.f, 
        data=mtcars, 
        varwidth=TRUE,
        col=c("gold", "darkgreen"),
        main="MPG Distribution by Auto Type", 
        xlab="Auto Type")

##ggplots

ggplot(mtcars, aes(x=interaction(cyl.f, am.f), y=mpg)) + geom_boxplot() +
  stat_summary(fun.y="mean", geom="point", shape=23, size=3, fill="white")

# Listing 6.10 - Violin plots

library(vioplot)
x1 <- mtcars$mpg[mtcars$cyl==4] 
x2 <- mtcars$mpg[mtcars$cyl==6]
x3 <- mtcars$mpg[mtcars$cyl==8]
vioplot(x1, x2, x3, 
        names=c("4 cyl", "6 cyl", "8 cyl"), 
        col="gold")
title("Violin Plots of Miles Per Gallon")

##ggplots
p<-ggplot(mtcars, aes(x=as.factor(cyl), y=mpg))
p+ geom_violin() + geom_boxplot(width=.1, fill="black", outlier.colour=NA) +
  stat_summary(fun.y=median, geom="point", fill="white", shape=21, size=2.5)

# dot chart
dotchart(mtcars$mpg,labels=row.names(mtcars),cex=.7,
         main="Gas Mileage for Car Models", 
         xlab="Miles Per Gallon")


# Listing 6.11 - Dot plot grouped, sorted, and colored


x <- mtcars[order(mtcars$mpg),]
x$cyl <- factor(x$cyl)
x$color[x$cyl==4] <- "red"
x$color[x$cyl==6] <- "blue"
x$color[x$cyl==8] <- "darkgreen"
dotchart(x$mpg,
         labels = row.names(x),
         cex=.7,
         groups = x$cyl,
         gcolor = "black",
         color = x$color,
         pch=19,
         main = "Gas Mileage for Car Models\ngrouped by cylinder",
         xlab = "Miles Per Gallon")


## in ggplot2

ggplot(mtcars,aes(x=mpg,y=row.names(mtcars)))+geom_point()
ggplot(mtcars,aes(x=mpg,y=reorder(row.names(mtcars),mpg)))+geom_point()+
  theme_bw()+
  theme(axis.text.x = element_text(angle=60, hjust=1),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_line(colour="grey60", linetype="dashed"))

as.factor(mtcars$cyl)
mtcars<-mtcars[order(mtcars$cyl,mtcars$mpg),]
mtcars$names<-row.names(mtcars)
nameorder<-mtcars$names[order(mtcars$cyl,mtcars$mpg)]
mtcars$names<-factor(mtcars$names,levels=nameorder)
ggplot(mtcars, aes(x=mpg, y=names)) +
  geom_segment(aes(yend=row.names(mtcars)), xend=0, colour="grey50") +
  geom_point(size=3, aes(colour=cyl))  +
  theme_bw() +
  theme(panel.grid.major.y = element_blank(), # No horizontal grid lines
        legend.position=c(1, 0.55), # Put legend inside plot area
        legend.justification=c(1, 0.5))

# 类别比较
# 其他图形见《R语言可视化之美》
#A.1 克利夫兰点图系列
#A.2 坡度图
#A.3 南丁格尔玫瑰图
#A.4 径向柱形图
#A.5 雷达图
#A.6 词云


# 数据分布图
# 其他图形见《R语言可视化之美》
# B.1 峰峦图
# B.2 云雨图
# B.3 子母图
# B.4 带显著标签的箱型图
# B.5 带连接线的双箱型图
# B.6 ...


