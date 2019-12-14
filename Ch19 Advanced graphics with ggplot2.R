#----------------------------------------------------------#
# R in Action (2nd ed): Chapter 19                         #
# Advanced graphics with ggplot2                           #
# requires packages ggplot2, RColorBrewer, gridExtra,      #
#   and car (for datasets)                                 #
 install.packages(c("ggplot2", "gridExtra",                
      "RColorBrewer", "car"))                             
#----------------------------------------------------------#

par(ask=TRUE)

# chapter 23 in the second edition.

# the lattice package
library(lattice)
histogram(~height | voice.part, data = singer, 
          main = "Distribution of Heights by Voice Pitch", 
          xlab = "Height (inches)")

# Listing 16.1 - Lattice plot examples

library(lattice)
attach(mtcars)

gear <- factor(gear, levels = c(3, 4, 5), 
               labels = c("3 gears", "4 gears", "5 gears"))

cyl <- factor(cyl, levels = c(4, 6, 8), 
              labels = c("4 cylinders", "6 cylinders", "8 cylinders"))

densityplot(~mpg, 
            main = "Density Plot", xlab = "Miles per Gallon")

densityplot(~mpg | cyl, 
            main = "Density Plot by Number of Cylinders", 
            xlab = "Miles per Gallon")

bwplot(cyl ~ mpg | gear, 
       main = "Box Plots by Cylinders and Gears", 
       xlab = "Miles per Gallon", ylab = "Cylinders")

xyplot(mpg ~ wt | cyl * gear, 
       main = "Scatter Plots by Cylinders and Gears", 
       xlab = "Car Weight", ylab = "Miles per Gallon")

#??άɢ??ͼ
cloud(mpg ~ wt * qsec | cyl, 
      main = "3D Scatter Plots by Cylinders")

dotplot(cyl ~ mpg | gear, 
        main = "Dot Plots by Number of Gears and Cylinders", 
        xlab = "Miles Per Gallon")

#ɢ??ͼ????
splom(mtcars[c(1, 3, 4, 5, 6)], 
      main = "Scatter Plot Matrix for mtcars Data")

detach(mtcars)

# conditioning on a continuous variable
displacement <- equal.count(mtcars$disp, number = 3, 
                            overlap = 0)
xyplot(mpg ~ wt | displacement, data = mtcars, 
       main = "Miles per Gallon vs. Weight by Engine Displacement", 
       xlab = "Weight", ylab = "Mile per Gallon", 
       layout = c(3, 1), aspect = 1.5)

# Listing 16.2 - xyplot with custom panel functions

displacement <- equal.count(mtcars$disp, number = 3, 
                            overlap = 0)

mypanel <- function(x, y) {
  panel.xyplot(x, y, pch = 19)
  panel.rug(x, y)
  panel.grid(h = -1, v = -1)
  panel.lmline(x, y, col = "red", lwd = 1, lty = 2)
}

xyplot(mpg ~ wt | displacement, data = mtcars, 
       layout = c(3, 1), aspect = 1.5, 
       main = "Miles per Gallon vs. Weight by Engine Displacement", 
       xlab = "Weight", ylab = "Mile per Gallon", panel = mypanel)

# Listing 16.3 -  xyplot with custom panel functions and additional options

library(lattice)
mtcars$transmission <- factor(mtcars$am, 
                              levels = c(0, 1), labels = c("Automatic", "Manual"))

panel.smoother <- function(x, y) {
  panel.grid(h = -1, v = -1)
  panel.xyplot(x, y)
  panel.loess(x, y)
  panel.abline(h = mean(y), lwd = 2, lty = 2, col = "green")
}

xyplot(mpg ~ disp | transmission, data = mtcars, scales = list(cex = 0.8, 
                                                               col = "red"), 
       panel = panel.smoother, xlab = "Displacement", 
       ylab = "Miles per Gallon", 
       main = "MGP vs Displacement by Transmission Type", 
       sub = "Dotted lines are Group Means", aspect = 1)

# grouping variables
library(lattice)
mtcars$transmission <- factor(mtcars$am, levels = c(0, 1), 
                              labels = c("Automatic", "Manual"))
densityplot(~mpg, data = mtcars, 
            group = transmission, 
            main = "MPG Distribution by Transmission Type", 
            xlab = "Miles per Gallon", 
            auto.key = TRUE)

densityplot(~mpg, data = mtcars, 
            group = transmission, 
            main = "MPG Distribution by Transmission Type", 
            xlab = "Miles per Gallon", 
            auto.key = list(space="right",
                            columns=1,title="Transmission"))

# Listing 16.4 - kernel density plot with a group variable and customized legend

library(lattice)
mtcars$transmission <- factor(mtcars$am, levels = c(0, 1), 
                              labels = c("Automatic", "Manual"))

colors <-  c("red", "blue")
lines <-  c(1, 2)
points <-  c(16, 17)

key.trans <- list(title = "Trasmission", 
                  space = "bottom", columns = 2, 
                  text = list(levels(mtcars$transmission)), 
                  points = list(pch = points, col = colors), 
                  lines = list(col = colors, lty = lines), 
                  cex.title = 1, cex = 0.9)

densityplot(~mpg, data = mtcars, 
            group = transmission, 
            main = "MPG Distribution by Transmission Type", 
            xlab = "Miles per Gallon", 
            pch = points, lty = lines, col = colors, 
            lwd = 2, jitter = 0.005, 
            key = key.trans)

# Listing 16.5 -  xyplot with group and conditioning variables and customized legend

library(lattice)
colors <- "darkgreen"
symbols <- c(1:12)
linetype <- c(1:3)

key.species <- list(title = "Plant", 
                    space = "right", 
                    text = list(levels(CO2$Plant)), 
                    points = list(pch = symbols, col = colors))

xyplot(uptake ~ conc | Type * Treatment, data = CO2, 
       group = Plant, 
       type = "b", 
       pch = symbols, col = colors, lty = linetype, 
       main = "Carbon Dioxide Uptake\nin Grass Plants", 
       ylab = expression(paste("Uptake ", 
                               bgroup("(", italic(frac("umol", "m"^2)), ")"))), 
       xlab = expression(paste("Concentration ", 
                               bgroup("(", italic(frac(mL, L)), ")"))), 
       sub = "Grass Species: Echinochloa crus-galli", 
       key = key.species)

# graphical parameters
show.settings()
mysettings <- trellis.par.get()
mysettings$superpose.symbol
mysettings$superpose.symbol$pch <- c(1:10)
trellis.par.set(mysettings)
show.settings()

# 自定义图形条带

library(lattice)
histogram(~height | voice.part, data = singer,
          strip = strip.custom(bg="lightgrey",
                               par.strip.text=list(col="black", cex=.8, font=3)),
          main="Distribution of Heights by Voice Pitch",
          xlab="Height (inches)")

# page arrangement
library(lattice)
graph1 <- histogram(~height | voice.part, data = singer, 
                    main = "Heights of Choral Singers by Voice Part")
graph2 <- densityplot(~height, data = singer, group = voice.part, 
                      plot.points = FALSE, auto.key = list(columns = 4))
plot(graph1, split = c(1, 1, 1, 2))
plot(graph2, split = c(1, 2, 1, 2), newpage = FALSE)

library(lattice)
graph1 <- histogram(~height | voice.part, data = singer, 
                    main = "Heights of Choral Singers by Voice Part")
graph2 <- densityplot(~height, data = singer, group = voice.part, 
                      plot.points = FALSE, auto.key = list(columns = 4))
plot(graph1, position = c(0, 0.3, 1, 1))
plot(graph2, position = c(0, 0, 1, 0.3), newpage = FALSE)

##Reference：Sarkar（2008）Lattice: Multivariate Data Visualization with R
##################################ggplot2########################

# Basic scatterplot
library(ggplot2)
ggplot(data=mtcars, aes(x=wt, y=mpg)) +
  geom_point() +
  labs(title="Automobile Data", x="Weight", y="Miles Per Gallon")


# Scatter plot with additional options
library(ggplot2)
ggplot(data=mtcars, aes(x=wt, y=mpg)) +
  geom_point(pch=17, color="blue", size=2) +
  geom_smooth(method="lm", color="red", linetype=2) +
  labs(title="Automobile Data", x="Weight", y="Miles Per Gallon")


# Scatter plot with faceting and grouping
data(mtcars)
mtcars$am <- factor(mtcars$am, levels=c(0,1),
                    labels=c("Automatic", "Manual"))
mtcars$vs <- factor(mtcars$vs, levels=c(0,1),
                    labels=c("V-Engine", "Straight Engine"))
mtcars$cyl <- factor(mtcars$cyl)


library(ggplot2)
ggplot(data=mtcars, aes(x=hp, y=mpg,
                        shape=cyl, color=cyl)) +
  geom_point(size=3) +
  facet_grid(am~vs) +
  labs(title="Automobile Data by Engine Type",
       x="Horsepower", y="Miles Per Gallon")

# Using geoms
data(singer, package="lattice")
ggplot(singer, aes(x=height)) + geom_histogram()

ggplot(singer, aes(x=voice.part, y=height)) + geom_boxplot()

data(Salaries, package="carData")

library(ggplot2)
ggplot(Salaries, aes(x=rank, y=salary)) +
  geom_boxplot(fill="cornflowerblue",
               color="black", notch=TRUE)+
  geom_point(position="jitter", color="blue", alpha=.5)+
  geom_rug(sides="l", color="black")


#combination

library(ggplot2)
data(singer, package="lattice")
ggplot(singer, aes(x=voice.part, y=height)) +
  geom_violin(fill="lightblue") +
  geom_boxplot(fill="lightgreen", width=.2)


## Grouping
data(Salaries, package="carData")
library(ggplot2)
ggplot(data=Salaries, aes(x=salary, fill=rank)) +
  geom_density(alpha=.8)

ggplot(Salaries, aes(x=yrs.since.phd, y=salary, color=rank,
                     shape=sex)) + geom_point()

ggplot(Salaries, aes(x=rank, fill=sex)) +
  geom_bar(position="stack") + labs(title='position="stack"')

ggplot(Salaries, aes(x=rank, fill=sex)) +
  geom_bar(position="dodge") + labs(title='position="dodge"')

ggplot(Salaries, aes(x=rank, fill=sex)) +
  geom_bar(position="fill") + labs(title='position="fill"')


# Placing options
ggplot(Salaries, aes(x=rank, fill=sex))+ geom_bar()

ggplot(Salaries, aes(x=rank)) + geom_bar(fill="red")

ggplot(Salaries, aes(x=rank, fill="red")) + geom_bar()


# Faceting
data(singer, package="lattice")
library(ggplot2)
ggplot(data=singer, aes(x=height)) +
  geom_histogram() +
  facet_wrap(~voice.part, nrow=4)

library(ggplot2)
ggplot(Salaries, aes(x=yrs.since.phd, y=salary, color=rank,
                     shape=rank)) + geom_point() + facet_grid(.~sex)

ggplot(Salaries, aes(x=yrs.since.phd, y=salary))+
  geom_smooth() + geom_point() + facet_grid(rank~sex)


data(singer, package="lattice")
library(ggplot2)
ggplot(data=singer, aes(x=height, fill=voice.part)) +
  geom_density() +
  facet_grid(voice.part~.)


# Adding smoothed lines
data(Salaries, package="carData")
library(ggplot2)
ggplot(data=Salaries, aes(x=yrs.since.phd, y=salary)) +
  geom_smooth() + geom_point()

ggplot(data=Salaries, aes(x=yrs.since.phd, y=salary,
                          linetype=sex, shape=sex, color=sex)) +
  geom_smooth(method=lm, formula=y~poly(x,2),
              se=FALSE, size=1) +
  geom_point(size=2)


# Modifying axes
data(Salaries,package="car")
library(ggplot2)
ggplot(data=Salaries, aes(x=rank, y=salary, fill=sex)) +
  geom_boxplot() +
  scale_x_discrete(breaks=c("AsstProf", "AssocProf", "Prof"),
                   labels=c("Assistant\nProfessor",
                            "Associate\nProfessor",
                            "Full\nProfessor")) +
  scale_y_continuous(breaks=c(50000, 100000, 150000, 200000),
                     labels=c("$50K", "$100K", "$150K", "$200K")) +
  labs(title="Faculty Salary by Rank and Sex", x="", y="")


# Legends
data(Salaries,package="car")
library(ggplot2)
ggplot(data=Salaries, aes(x=rank, y=salary, fill=sex)) +
  geom_boxplot() +
  scale_x_discrete(breaks=c("AsstProf", "AssocProf", "Prof"),
                   labels=c("Assistant\nProfessor",
                            "Associate\nProfessor",
                            "Full\nProfessor")) +
  scale_y_continuous(breaks=c(50000, 100000, 150000, 200000),
                     labels=c("$50K", "$100K", "$150K", "$200K")) +
  labs(title="Faculty Salary by Rank and Gender",
       x="", y="", fill="Gender") +
  theme(legend.position=c(.1,.8))


# Scales
ggplot(mtcars, aes(x=wt, y=mpg, size=disp)) +
  geom_point(shape=21, color="black", fill="cornsilk") +
  labs(x="Weight", y="Miles Per Gallon",
       title="Bubble Chart", size="Engine\nDisplacement")

data(Salaries, package="carData")
ggplot(data=Salaries, aes(x=yrs.since.phd, y=salary, color=rank)) +
  scale_color_manual(values=c("orange", "olivedrab", "navy")) +
  geom_point(size=2)

ggplot(data=Salaries, aes(x=yrs.since.phd, y=salary, color=rank)) +
  scale_color_brewer(palette="Set1") + geom_point(size=2)

library(RColorBrewer)
display.brewer.all()


# Themes
data(Salaries, package="car")
library(ggplot2)
mytheme <- theme(plot.title=element_text(face="bold.italic",
                                         size="14", color="brown"),
                 axis.title=element_text(face="bold.italic",
                                         size=10, color="brown"),
                 axis.text=element_text(face="bold", size=9,
                                        color="darkblue"),
                 panel.background=element_rect(fill="white",
                                               color="darkblue"),
                 panel.grid.major.y=element_line(color="grey",
                                                 linetype=1),
                 panel.grid.minor.y=element_line(color="grey",
                                                 linetype=2),
                 panel.grid.minor.x=element_blank(),
                 legend.position="top")

ggplot(Salaries, aes(x=rank, y=salary, fill=sex)) +
  geom_boxplot() +
  labs(title="Salary by Rank and Sex", 
       x="Rank", y="Salary") +
  mytheme


# Multiple graphs per page
data(Salaries, package="carData")
library(ggplot2)
p1 <- ggplot(data=Salaries, aes(x=rank)) + geom_bar()
p2 <- ggplot(data=Salaries, aes(x=sex)) + geom_bar()
p3 <- ggplot(data=Salaries, aes(x=yrs.since.phd, y=salary)) + geom_point()

library(gridExtra)
grid.arrange(p1, p2, p3, ncol=3)


# Saving graphs
ggplot(data=mtcars, aes(x=mpg)) + geom_histogram()
ggsave(file="mygraph.pdf")
