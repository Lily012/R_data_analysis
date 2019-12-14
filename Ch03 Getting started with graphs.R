# ----------------------------------------------------#
# R in Action (2nd ed): Chapter 3                     #
# Getting started with graphs                         #
# requires that the Hmisc and RColorBrewer  packages  #
# have been installed                                 #
# install.packages(c("Hmisc", "RColorBrewer"))        #
#-----------------------------------------------------#

par(ask=TRUE)
opar <- par(no.readonly=TRUE) # make a copy of current settings


# --Section 3.1--
attach(mtcars) # be sure to execute this line

plot(wt, mpg)
abline(lm(mpg~wt))
title("Regression of MPG on Weight")

detach(mtcars)


#save a graph
attach(mtcars)
pdf("mygraph.pdf")
plot(wt, mpg)
abline(lm(mpg~wt))
title("Regression of MPG on Weight")
detach(mtcars)
dev.off()

##  in Gui ,you  can use the code : savePlot(filename="mygraph.pdf",type="pdf",device=dev.cur()) 
# --Section 3.2--

# Input data for drug example
dose  <- c(20, 30, 40, 45, 60)
drugA <- c(16, 20, 27, 40, 60)
drugB <- c(15, 18, 25, 31, 40)

plot(dose, drugA, type="b")


# ##plot by ggplot2  
# drug.response<-data.frame(dose,drugA,drugB)
# library(ggplot2)
# qplot(x=dose,y=drugA,data=drug.response,color=dose,geom=c("line","point"))
# #or 
# ggplot(drug.response,aes(x=dose,y=drugB))+geom_line( )+geom_point(aes(color=dose))


##more details refer to <R Graphics Cookbook >


# --Section 3.3--

## the first method to set the parameters
opar <- par(no.readonly=TRUE) # make a copy of current settings
par(lty=2, pch=17)            # change line type and symbol
plot(dose, drugA, type="b")   # generate a plot
par(opar)                     # restore the original settings 

# in ggplot2
# ggplot(drug.response,aes(x=dose,y=drugA))
# +geom_line(linetype=2 )+geom_point(shape=17)

## the second method to set the parameters
plot(dose, drugA, type="b", lty=3, lwd=3, pch=15, cex=2)


# #in ggplot2
# ggplot(drug.response,aes(x=dose,y=drugA))
# +geom_line(linetype=3,size=1)+geom_point(shape=15,size=2)


# choosing colors
library(RColorBrewer)
n <- 7
mycolors <- brewer.pal(n, "Set1")
barplot(rep(1,n), col=mycolors)

# gray function
n <- 10
mycolors <- rainbow(n)
pie(rep(1, n), labels=mycolors, col=mycolors)



mygrays <- gray(0:n/n)
pie(rep(1, n), labels=mygrays, col=mygrays)


# Listing 3.1 - Using graphical parameters to control graph appearance	
dose <- c(20, 30, 40, 45, 60)
drugA <- c(16, 20, 27, 40, 60)
drugB <- c(15, 18, 25, 31, 40)
opar <- par(no.readonly=TRUE)
par(pin=c(2, 3))
par(lwd=2, cex=1.5)
par(cex.axis=.75, font.axis=3)
par(mfrow=c(1,2))
plot(dose, drugA, type="b", pch=19, lty=2, col="red")
plot(dose, drugB, type="b", pch=23, lty=6, col="blue", bg="green")
par(opar)				

# # in ggplot2
# g<-ggplot(drug.response,aes(x=dose,y=drugB))+geom_line(linetype=6,colour="blue")+
#   geom_point(shape=23,colour="red",fill="green",size=5)


# --Section 3.4--

# Adding text, lines, and symbols
plot(dose, drugA, type="b",  
     col="red", lty=2, pch=2, lwd=2,
     main="Clinical Trials for Drug A", 
     sub="This is hypothetical data", 
     xlab="Dosage", ylab="Drug Response",
     xlim=c(0, 60), ylim=c(0, 70),ann=F)

# ## in ggplot2
# 
# g+ggtitle("Clinical Trials for Drug B")
# see more in <R graphics cookbook>

#title function

title(mai="main title", sub="subtitle",
      xlab="x-axis label",ylab="y-axis label")

title(main="My Title", col.main="red",
      sub="My Subtitle", col.sub="blue",
      xlab="My X label", ylab="My Y label",
      col.lab="green", cex.lab=0.75)



# Listing 3.2 - An Example of Custom Axes
x <- c(1:10)
y <- x
z <- 10/x
opar <- par(no.readonly=TRUE)
par(mar=c(5, 4, 4, 8) + 0.1)
plot(x, y, type="b",
     pch=21, col="red",
     yaxt="n", lty=3, ann=FALSE)
lines(x, z, type="b", pch=22, col="blue", lty=2)
axis(2, at=x, labels=x, col.axis="red", las=2)
axis(4, at=z, labels=round(z, digits=2),
     col.axis="blue", las=2, cex.axis=0.7, tck=-.01)
mtext("y=1/x", side=4, line=3, cex.lab=1, las=2, col="blue")
title("An Example of Creative Axes",
      xlab="X values",
      ylab="Y=X")
par(opar)


# Listing 3.3 - Comparing Drug A and Drug B response by dose
dose <- c(20, 30, 40, 45, 60)
drugA <- c(16, 20, 27, 40, 60)
drugB <- c(15, 18, 25, 31, 40)
opar <- par(no.readonly=TRUE)
par(lwd=2, cex=1.5, font.lab=2)
plot(dose, drugA, type="b",
     pch=15, lty=1, col="red", ylim=c(0, 60),
     main="Drug A vs. Drug B",
     xlab="Drug Dosage", ylab="Drug Response")
lines(dose, drugB, type="b",
      pch=17, lty=2, col="blue")
abline(h=c(30), lwd=1.5, lty=2, col="gray")
library(Hmisc)
minor.tick(nx=3, ny=3, tick.ratio=0.5)
legend("topleft", inset=.05, title="Drug Type", c("A","B"),
       lty=c(1, 2), pch=c(15, 17), col=c("red", "blue"))
par(opar)



# # ggplot2
# library(reshape2)
# drug.response.long<-melt(drug.response,id="dose")
# library(ggplot2)
# qplot(x=dose,y=value,geom=c("line","point"),
#       data=drug.response.long,colour=variable,
#       group=variable)


# Example of labeling points
attach(mtcars)
plot(wt, mpg,
     main="Mileage vs. Car Weight",
     xlab="Weight", ylab="Mileage",
     pch=18, col="blue")
text(wt, mpg,
     row.names(mtcars),
     cex=0.6, pos=4, col="red")
detach(mtcars)


# View font families 
opar <- par(no.readonly=TRUE)
par(cex=1.5)
plot(1:7,1:7,type="n")
text(3,3,"Example of default text")
text(4,4,family="mono","Example of mono-spaced text")
text(5,5,family="serif","Example of serif text")
par(opar)

# --Section 3.5--

# combining graphs
# Combining graphs

# Figure 3.14
attach(mtcars)
opar <- par(no.readonly=TRUE)
par(mfrow=c(2,2))
plot(wt,mpg, main="Scatterplot of wt vs. mpg")
plot(wt,disp, main="Scatterplot of wt vs. disp")
hist(wt, main="Histogram of wt")
boxplot(wt, main="Boxplot of wt")
par(opar)
detach(mtcars)


# Figure 3.15
attach(mtcars)
opar <- par(no.readonly=TRUE)
par(mfrow=c(3,1))
hist(wt)
hist(mpg)
hist(disp)
par(opar)
detach(mtcars)

# Figure 3.16
attach(mtcars)
layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
hist(wt)
hist(mpg)
hist(disp)
detach(mtcars)

# Figure 3.17
attach(mtcars)
layout(matrix(c(1, 1, 2, 3), 2, 2, byrow = TRUE),
       widths=c(3, 1), heights=c(1, 2))
hist(wt)
hist(mpg)
hist(disp)
detach(mtcars)


# Listing 3.4 - Fine placement of figures in a graph
opar <- par(no.readonly=TRUE)
par(fig=c(0, 0.8, 0, 0.8))
plot(mtcars$wt, mtcars$mpg,
     xlab="Miles Per Gallon",
     ylab="Car Weight")
par(fig=c(0, 0.8, 0.55, 1), new=TRUE)
boxplot(mtcars$wt, horizontal=TRUE, axes=FALSE)
par(fig=c(0.65, 1, 0, 0.8), new=TRUE)
boxplot(mtcars$mpg, axes=FALSE)
mtext("Enhanced Scatterplot", side=3, outer=TRUE, line=-3)
par(opar)


# math expression
# see help(plotmath) for more

par(mar=c(5,4,6,5)+0.1)
x<-0;y<-0
plot(x,y,xlab=quote(alpha),
     ylab=bquote(beta), type='n', cex.lab=1.3)
title(main=substitute(paste("A function of ",
                            alpha,'and', beta)))
title(sub=expression(mu[i]==LD["50"]))
mtext(expression('p' * group("(",tau,"|")* y * group(")", ," ")),
      side=4, las=0, line=1, cex=1.4)
mtext(expression(paste("p(",tau,"|x),",
                       gamma,"=1,", gamma,"=2,", gamma,"=3")),
      side=3, las=0, line=1, cex=1.4)
text(0,0.5, expression(mu[i]),cex=2)
text(x,y,expression(italic(p)==over(1,e^-(alpha+beta*x))),cex=2)
Rsquared <- 0.9863728
text(0,-0.5,as.expression(substitute(italic(R)^2==r,
                                     list(r=round(Rsquared,3)))),cex=2)
ex.cs1 <- c(expression(gamma==1),
            expression(gamma==2), expression(gamma==3))
legend("topright",ex.cs1, col=1:3, lty=1:3,
       ncol=1, cex=1.5, lwd=2.0)



## 其他有关命令
# 图形保存函数
savePlot(filename="Rplot",
         type=c("wmf", "png", "jpeg", "jpg", "bmp",
                "ps", "pdf"),device=dev.cur())

#图形界面分割
mat <- matrix(c(2,3,0,1,0,0,0,4,0,0,0,5),4,3,byrow=TRUE)
layout(mat)
layout.show(5)

layout(mat,widths=c(1,5,14),heights=c(1,2,4,1))
layout.show(5)

# 绘图命令
plot(c(0,1),c(0,1),type="n")
segments(x0=0,y0=0,x1=1,y1=1)
lines(x=c(1,0),y=c(0,1))


 x <- runif(12); y <- runif(12)
 i <- order(x,y); x <- x[i]; y <- y[i]
 plot(x,y)
 s <- seq(length(x)-1)
 arrows(x[s], y[s], x[s+1], y[s+1], length=0.1)
 
 #颜色系统
 pie(rep(1, 200), labels = "", col = rainbow(200), border = NA)
 require("RColorBrewer")
  display.brewer.all()
 
 #创建网格
 X <- matrix(1:12,nrow=3)
 colours <- c("orange","orangered","red","lightblue",
              "blue", "white","lightgrey","grey",
               "darkgrey","yellow","green","purple")
 image(X,col=colours)
 text(rep(c(0,0.5,1),4),rep(c(0,0.3,0.7,1),each=3),1:12,cex=2)