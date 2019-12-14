#------------------------------------------------------------------------------------#
# R in Action (2nd ed): Chapter 11                                                   #
# Intermediate graphs                                                                #
# requires packages car, scatterplot3d, gclus, hexbin, IDPmisc, Hmisc,               # 
#                   corrgram, vcd, rlg to be installed                               #
#install.packages(c("car", "scatterplot3d", "gclus", "hexbin", "IDPmisc", "Hmisc",  #
#                    "corrgram", "vcd", "rld"))                                      #
#------------------------------------------------------------------------------------#

par(ask=TRUE)
opar <- par(no.readonly=TRUE) # record current settings

# Listing 11.1 - A scatter plot with best fit lines
attach(mtcars)                                                     
plot(wt, mpg, 
     main="Basic Scatterplot of MPG vs. Weight",       
     xlab="Car Weight (lbs/1000)", 
     ylab="Miles Per Gallon ", pch=19)
abline(lm(mpg ~ wt), col="red", lwd=2, lty=1)            
lines(lowess(wt, mpg), col="blue", lwd=2, lty=2)
detach(mtcars)

# Scatter plot with fit lines by group
library(car) 
scatterplot(mpg ~ wt| cyl, data=mtcars, lwd=2,
            main="Scatter Plot of MPG vs. Weight
            # by # Cylinders",
             xlab="Weight of Car (lbs/1000)",
             ylab="Miles Per Gallon", 
             id=TRUE,
            boxplots="xy")

#Enhanced scatter plot (add lines)
library(DAAG)
data(cars)
plot(cars$dist ~ cars$speed,
     xlab = "Speed (mph)",
     ylab = "Stopping distance (ft)"
)
abline(lm(cars$dist ~ cars$speed), col="red",
       lwd=2, lty=1)
lines(lowess(cars$speed, cars$dist),lwd=2)#非线性拟合
library(locfit)
lines(locfit(cars$dist ~ lp(cars$speed, nn=0.8,
                            deg=2)))

# Enhanced scatter plot (Add histograms)
layout(matrix(c(2,0,1,3), 2, 2, byrow=TRUE),
       c(3,1), c(1,3), TRUE)
layout.show(3)
par(mar=c(5,5,1,1))
xhist <- hist(cars$speed, breaks=10, plot=FALSE)
yhist <- hist(cars$dist, breaks=10, plot=FALSE)
plot(cars$dist ~ cars$speed, xlab = "Speed (mph)",
     ylab = "Stopping distance (ft)")
par(mar=c(0,5,1,1))
barplot(xhist$counts, axes=FALSE, space=0)
par(mar=c(5,0,1,1))
barplot(yhist$counts, axes=FALSE,space=0, horiz=TRUE)

#Enhanced scatter plot (Add rugs)
plot(cars$dist ~ cars$speed,
     xlab = "Speed (mph)", ylab = "Stopping distance (ft)")
rug(side=2, jitter(cars$dist, 20))
rug(side=1, jitter(cars$speed, 5))




# Scatter-plot matrices
pairs(~ mpg + disp + drat + wt, data=mtcars, 
      main="Basic Scatterplot Matrix")


library(car)
scatterplotMatrix(~ mpg + disp + drat + wt, data=mtcars,
                  spread=FALSE, smoother.args=list(lty=2),
                  main="Scatter Plot Matrix via car Package")

scatterplotMatrix(~ mpg + disp + drat + wt | cyl, data=mtcars, spread=FALSE,
                  main="Scatterplot Matrix via car package", diagonal="histogram")

cor(mtcars[c("mpg", "wt", "disp", "drat")])


## Listing 11-2 Scatter plot matrix produced
# with the gclus package
library(gclus)
mydata <- mtcars[c(1,3,5,6)]
mydata.corr <- abs(cor(mydata))
mycolors <- dmat.color(mydata.corr)
myorder <- order.single(mydata.corr)
cpairs(mydata, myorder,
       panel.colors=mycolors, gap=.5,
       main="Variables Ordered and Colored by
       Correlation")

#gpairs()
library(YaleToolkit)
library(gpairs)
gpairs(iris[1:4])

#splom()
library(lattice)
splom(iris[1:4])


#matplot() is use to plot a line matrix

matplot(iris[1:4], type = 'l', ylab = " ", main = "Matplot")



# high density scatterplots
set.seed(1234)
n <- 10000
c1 <- matrix(rnorm(n, mean=0, sd=.5), ncol=2)
c2 <- matrix(rnorm(n, mean=3, sd=2), ncol=2)
mydata <- rbind(c1, c2)
mydata <- as.data.frame(mydata)
names(mydata) <- c("x", "y")

with(mydata,
     plot(x, y, pch=19, main="Scatter Plot with 10000 Observations"))

with(mydata,
     smoothScatter(x, y, main="Scatter Plot colored by Smoothed Densities"))

library(hexbin)
with(mydata, {
  bin <- hexbin(x, y, xbins=50)
  plot(bin, main="Hexagonal Binning with 10,000 Observations")
})


# 3-D Scatterplots
library(scatterplot3d)
attach(mtcars)
scatterplot3d(wt, disp, mpg,
              main="Basic 3D Scatter Plot")

scatterplot3d(wt, disp, mpg,
              pch=16,
              highlight.3d=TRUE,
              type="h",
              main="3D Scatter Plot with Vertical Lines")

s3d <-scatterplot3d(wt, disp, mpg,
                    pch=16,
                    highlight.3d=TRUE,
                    type="h",
                    main="3D Scatter Plot with Vertical Lines and Regression Plane")
fit <- lm(mpg ~ wt+disp)
s3d$plane3d(fit)
detach(mtcars)

# spinning 3D plot
library(rgl)
attach(mtcars)
plot3d(wt, disp, mpg, col="red", size=5)

# alternative
library(car)                            
with(mtcars,
     scatter3d(wt, disp, mpg))


# bubble plots
attach(mtcars)
r <- sqrt(disp/pi)
symbols(wt, mpg, circle=r, inches=0.30,
        fg="white", bg="lightblue",
        main="Bubble Plot with point size proportional to displacement",
        ylab="Miles Per Gallon",
        xlab="Weight of Car (lbs/1000)")
text(wt, mpg, rownames(mtcars), cex=0.6)
detach(mtcars)

# #####ggplot
# library(gcookbook) # For the data set
# cdat <- subset(countries, Year==2009 &
#                  Name %in% c("Canada", "Ireland", "United Kingdom", "United States",
#                              "New Zealand", "Iceland", "Japan", "Luxembourg",
#                              "Netherlands", "Switzerland"))
# 
# p <- ggplot(cdat, aes(x=healthexp, y=infmortality, size=GDP)) +
#   geom_point(shape=21, colour="black", fill="cornsilk")
# # GDP mapped to radius (default with scale_size_continuous)
# p
# 
# # GDP mapped to area instead, and larger circles
# p + scale_size_area(max_size=15)
# 
# hec <- HairEyeColor[,,"Male"] + HairEyeColor[,,"Female"]
# # Convert to long format
# library(reshape2)
# hec <- melt(hec, value.name="count")
# ggplot(hec, aes(x=Eye, y=Hair)) +
#   geom_point(aes(size=count), shape=21, colour="black", fill="cornsilk") +
#   scale_size_area(max_size=20, guide=FALSE) +
#   geom_text(aes(y=as.numeric(Hair)-sqrt(count)/22, label=count), vjust=1,
#             colour="grey60", size=4)
# 
# 
# ####ggplot######
# 
# 
# library(gcookbook)
# 
# ggplot(heightweight, aes(x=ageYear, y=heightIn, colour=sex)) + geom_point()
# ggplot(heightweight, aes(x=ageYear, y=heightIn, shape=sex)) + geom_point()
# 
# 
# ## change colors and shapes
# 
# ggplot(heightweight, aes(x=ageYear, y=heightIn, shape=sex, colour=sex)) +
#   geom_point() +
#   scale_shape_manual(values=c(1,2)) +
#   scale_colour_brewer(palette="Set1")
# 
# ggplot(heightweight, aes(x=ageYear, y=heightIn)) + geom_point(shape=3)
# 
# ggplot(heightweight, aes(x=ageYear, y=heightIn, shape=sex)) +
#   geom_point(size=3) + scale_shape_manual(values=c(1, 4))
# 
# #将连续型变量映射到点的颜色或大小属性上
# heightweight[, c("sex", "ageYear", "heightIn", "weightLb")]
# ggplot(heightweight, aes(x=ageYear, y=heightIn, colour=weightLb)) + geom_point()
# ggplot(heightweight, aes(x=ageYear, y=heightIn, size=weightLb)) + geom_point()
# 
# 
# ggplot(heightweight, aes(x=weightLb, y=heightIn, fill=ageYear)) +
#   geom_point(shape=21, size=2.5) +
#   scale_fill_gradient(low="black", high="white")
# 
# ggplot(heightweight, aes(x=weightLb, y=heightIn, fill=ageYear)) +
#   geom_point(shape=21, size=2.5) +
#   scale_fill_gradient(low="black", high="white", breaks=12:17,
#                       guide=guide_legend())
# 
# 
# #处理重叠
# ggplot(heightweight, aes(x=ageYear, y=heightIn, size=weightLb, colour=sex)) +
#   geom_point(alpha=.5) +
#   scale_size_area() + # Make area proportional to numeric value
#   scale_colour_brewer(palette="Set1")
# 
# sp <- ggplot(diamonds, aes(x=carat, y=price))
# sp + geom_point()
# 
# sp + geom_point(alpha=.1)
# sp + geom_point(alpha=.01)
# 
# sp + stat_bin2d()
# sp + stat_bin2d(bins=50) +
#   scale_fill_gradient(low="lightblue", high="red", limits=c(0, 6000))
# 
# 
# 
# # # -----------------------------------------------------------------------
# 
# library(hexbin)
# sp + stat_binhex() +
#   scale_fill_gradient(low="lightblue", high="red",
#                       limits=c(0, 8000))
# sp + stat_binhex() +
#   scale_fill_gradient(low="lightblue", high="red",
#                       breaks=c(0, 250, 500, 1000, 2000, 4000, 6000),
#                       limits=c(0, 6000))
# 
# 
# sp1 <- ggplot(ChickWeight, aes(x=Time, y=weight))
# sp1 + geom_point()
# sp1 + geom_point(position="jitter")
# 
# ##加拟合线 
# sp <- ggplot(heightweight, aes(x=ageYear, y=heightIn))
# sp + geom_point() + stat_smooth(method=lm)
# 
# # 99% confidence region
# sp + geom_point() + stat_smooth(method=lm, level=0.99)
# # No confidence region
# sp + geom_point() + stat_smooth(method=lm, se=FALSE)
# 
# sp + geom_point(colour="grey60") +
#   stat_smooth(method=lm, se=FALSE, colour="black")
# 
# sp + geom_point(colour="grey60") + stat_smooth()
# sp + geom_point(colour="grey60") + stat_smooth(method=loess)
# 
# ##GLM
# library(MASS) # For the data set
# b <- biopsy
# b$classn[b$class=="benign"] <- 0
# b$classn[b$class=="malignant"] <- 1
# ggplot(b, aes(x=V1, y=classn)) +
#   geom_point(position=position_jitter(width=0.3, height=0.06), alpha=0.4,
#              shape=21, size=1.5) +
#   stat_smooth(method=glm,method.args=binomial)
# 
# 
# 
# ##根据已有模型向散点图添加拟合线
# # Given a model, predict values of yvar from xvar
# # This supports one predictor and one predicted variable
# # xrange: If NULL, determine the x range from the model object. If a vector with
# # two numbers, use those as the min and max of the prediction range.
# # samples: Number of samples across the x range.
# # ...: Further arguments to be passed to predict()
# predictvals <- function(model, xvar, yvar, xrange=NULL, samples=100, ...) {
#   # If xrange isn't passed in, determine xrange from the models.
#   # Different ways of extracting the x range, depending on model type
#   if (is.null(xrange)) {
#     if (any(class(model) %in% c("lm", "glm")))
#       xrange <- range(model$model[[xvar]])
#     else if (any(class(model) %in% "loess"))
#       xrange <- range(model$x)
#   }
#   newdata <- data.frame(x = seq(xrange[1], xrange[2], length.out = samples))
#   names(newdata) <- xvar
#   newdata[[yvar]] <- predict(model, newdata = newdata, ...)
#   newdata
# }
# 
# modlinear <- lm(heightIn ~ ageYear, heightweight)
# modloess <- loess(heightIn ~ ageYear, heightweight)
# 
# lm_predicted <- predictvals(modlinear, "ageYear", "heightIn")
# loess_predicted <- predictvals(modloess, "ageYear", "heightIn")
# sp + geom_line(data=lm_predicted, colour="red", size=.8) +
#   geom_line(data=loess_predicted, colour="blue", size=.8)
# 
# 
# 
# 
# make_model <- function(data) {
#   lm(heightIn ~ ageYear, data)
# }
# 
# 
# library(gcookbook) # For the data set
# library(plyr)
# models <- dlply(heightweight, "sex", .fun = make_model)
# 
# 
# 
# ### 已有模型加曲线
# 
# model <- lm(heightIn ~ ageYear + I(ageYear^2), heightweight)
# xmin <- min(heightweight$ageYear)
# xmax <- max(heightweight$ageYear)
# predicted <- data.frame(ageYear=seq(xmin, xmax, length.out=100))
# predicted$heightIn <- predict(model, predicted)
# 
# sp <- ggplot(heightweight, aes(x=ageYear, y=heightIn)) +
#   geom_point(colour="grey40")
# sp + geom_line(data=predicted, size=1)
# 
# 
# 
# 

# Listing 11.2 - Creating side by side scatter and line plots
opar <- par(no.readonly=TRUE)
par(mfrow=c(1,2))
t1 <- subset(Orange, Tree==1)
plot(t1$age, t1$circumference,
     xlab="Age (days)",
     ylab="Circumference (mm)",
     main="Orange Tree 1 Growth")
plot(t1$age, t1$circumference,
     xlab="Age (days)",
     ylab="Circumference (mm)",
     main="Orange Tree 1 Growth",
     type="b")
par(opar)

# type= options in the plot() and lines() functions
x <- c(1:5)
y <- c(1:5)
par(mfrow=c(2,4))
types <- c("p", "l", "o", "b", "c", "s", "S", "h")
for (i in types){
  plottitle <- paste("type=", i)
  plot(x,y,type=i, col="red", lwd=2, cex=1, main=plottitle)
}

# Listing 11.3 - Line chart displaying the growth of 5 Orange trees over time
Orange$Tree <- as.numeric(Orange$Tree)
ntrees <- max(Orange$Tree)
xrange <- range(Orange$age)
yrange <- range(Orange$circumference)
plot(xrange, yrange,
     type="n",
     xlab="Age (days)",
     ylab="Circumference (mm)"
)
colors <- rainbow(ntrees)
linetype <- c(1:ntrees)
plotchar <- seq(18, 18+ntrees, 1)
for (i in 1:ntrees) {
  tree <- subset(Orange, Tree==i)
  lines(tree$age, tree$circumference,
        type="b",
        lwd=2,
        lty=linetype[i],
        col=colors[i],
        pch=plotchar[i]
  )
}
title("Tree Growth", "example of line plot")
legend(xrange[1], yrange[2],
       1:ntrees,
       cex=0.8,
       col=colors,
       pch=plotchar,
       lty=linetype,
       title="Tree"
)                                          


# Correlograms
options(digits=2)



cor(mtcars)

library(corrgram)
corrgram(mtcars, order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Corrgram of mtcars intercorrelations")

corrgram(mtcars, order=TRUE, lower.panel=panel.ellipse,
         upper.panel=panel.pts, text.panel=panel.txt,
         diag.panel=panel.minmax,
         main="Corrgram of mtcars data using scatter plots
and ellipses")

cols <- colorRampPalette(c("darkgoldenrod4", "burlywood1",
                           "darkkhaki", "darkgreen"))
corrgram(mtcars, order=TRUE, col.regions=cols,
         lower.panel=panel.pie,
         upper.panel=panel.conf, text.panel=panel.txt,
         main="A Corrgram (or Horse) of a Different Color")


# Mosaic Plots
ftable(Titanic)
library(vcd)
mosaic(Titanic, shade=TRUE, legend=TRUE)

library(vcd)
mosaic(~Class+Sex+Age+Survived, data=Titanic, shade=TRUE, legend=TRUE)

example(mosaic)



##Creating a Network Graph
library(igraph)
# May need to install first, with install.packages("igraph")
library(igraph)
# Specify edges for a directed graph
gd <- graph(c(1,2, 2,3, 2,4, 1,4, 5,5, 3,6))
plot(gd)
# For an undirected graph
gu <- graph(c(1,2, 2,3, 2,4, 1,4, 5,5, 3,6), directed=FALSE)
# No labels
plot(gu, vertex.label=NA)


library(gcookbook) # For the data set

# Create a graph object from the data set
g <- graph.data.frame(madmen2, directed=TRUE)
par(mar=c(0,0,0,0))
plot(g, layout=layout.fruchterman.reingold, vertex.size=8, edge.arrow.size=0.5,
     vertex.label=NA)

g <- graph.data.frame(madmen, directed=FALSE)
par(mar=c(0,0,0,0)) # Remove unnecessary margins
plot(g, layout=layout.circle, vertex.size=8, vertex.label=NA)

###Creating a Map
library(maps) # For map data
library(mapdata)
library(ggplot2)
# Get map data for USA
states_map <- map_data("state")
ggplot(states_map, aes(x=long, y=lat, group=group)) +
  geom_polygon(fill="white", colour="black")


# geom_path (no fill) and Mercator projection
ggplot(states_map, aes(x=long, y=lat, group=group)) +
  geom_path() 




# Map region to fill color

euro<-map_data("world",region=c("UK","France","Netherlands","Belgium"))
ggplot(euro, aes(x=long, y=lat, group=group, fill=region)) +
  geom_polygon(colour="black") +
  scale_fill_brewer(palette="Set2")+
  scale_y_continuous(limits = c(40,60))+
  scale_x_continuous(limits=c(-25,25))


##wordcloud

library(wordcloud2)
library(tm)
data(crude)
crude<-tm_map(crude,removePunctuation)
crude<-tm_map(crude,function(x)removeWords(x,stopwords()))
tdm<-TermDocumentMatrix(crude)
m<-as.matrix(tdm)
v<-sort(rowSums(m),decreasing=TRUE)
d<-data.frame(word=names(v),freq=v)
wordcloud2(d)

###其他关系图形
### # 其他图形见《R语言可视化之美》

#C.1 QQ 图与PP图
#C.2 曲面拟合图
#C.3 曲面拟合图
#C.4 等高线图
#C.5 切面图
#C.6 三元相图
#C.7 瀑布图
#C.8 韦恩图
#C.9 树形图
#C.10 圆堆积图
#C.11 和弦图
#C.12 桑基图
#C.13 热力图
# ...



