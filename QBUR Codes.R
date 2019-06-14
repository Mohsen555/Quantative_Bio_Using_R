### -------------------------------------------------------------------------------------

getwd()
setwd("C:/Users/Mohsen/Desktop/Quantative Bio Using R")
# Checking
getwd()

x <- c(10, 9.5, 10.5, 0, 10, 20)
barplot(x)

### -------------------------------------------------------------------------------------

# p-value, student's t-test
# t = (Mean_A - Mean_B)/SE
set.seed(123)
x = runif(100, 0.0, 1)
y = runif(100, 0.0, 1)
t.test(x,y)

### -------------------------------------------------------------------------------------

x <- read.delim("Endoderm.txt", row.names = 1)
# rownames(x) = x[,1]
# x = x[,-1]
head(x)

v <- vector()
for(i in 1:ncol(x)){
  v[i] <- x[i,i]
}
head(x)
head(x[-1:-3,])

heatmap(as.matrix(x))
class(as.matrix(x))
class(x)

y <- matrix(0, nrow = 5, ncol = 5)
y
class(y)

z <- data.frame(y)
z
class(z)

# install.packages("pheatmap", dependencies = TRUE, repos = 'http://cran.rstudio.com/')
library(pheatmap)

pheatmap(x,fontsize_row = 8,border_color = NA)

pdf("Heatmap.pdf")
pheatmap(x,fontsize_row = 8)
dev.off()

colnames(x)
barplot(x[,"PAX4"])

x = log2(x + 1)
head(x)
max(x,na.rm = TRUE)

pheatmap(x,fontsize_row = 8,border_color = NA)

### -------------------------------------------------------------------------------------

# Student's t-test   # Assumption: data distribution is normal   # How to make normal distibution -> (Xi-mean)/variance
a <- x[1:3,1]
b <- x[4:6,1]

t.test(a,b)

t.test(1:5,c(1.9,3:6),paired = T, alternative = "less" )

### -------------------------------------------------------------------------------------

# install.packages("ggplot2", dependencies = TRUE, repos = 'http://cran.rstudio.com/')
library(ggplot2)

# transpose a matrix
x = t(x)
head(x)
x = data.frame(x)
# Scatter Plot for SC.1 and SC.2
ggplot(x, aes(x = DE.1, y = DE.2))+geom_point()

# Add a column to the data frame
x$Gene <- rownames(x)   # x <- cbind(x, Gene = rownames(x))

p <- ggplot(x, aes(x = DE.1, y = DE.2, label = Gene))+geom_point()+
  geom_text()

plot(p)

# Bar Plot
y <- x[,c("Gene","DE.1")]
y
p1 <- ggplot(y, aes(x = Gene, y = DE.1, fill = Gene)) + geom_bar(stat="identity")
p2 = p1 + ylab("Expression of genes in Definitive Endoderm log2")
p2

pdf("Barplot.pdf")
p1
p2
dev.off()

### -------------------------------------------------------------------------------------

x = data.frame(Pressure = c(11.2, 11.5, 11, 11.1, 13, 14),
               Weight = c(70, 75, 67, 73, 100, 120))
x
cor(x$Pressure, x$Weight)
ggplot(x, aes(Pressure, Weight)) + geom_point()

x$Beat <- c(70, 65, 80, 73, 58, 45)
cor(x$Pressure, x$Beat)
ggplot(x, aes(Pressure, Beat))+ geom_point()+
  geom_smooth(method = "lm")

cor(x)
cor.test(x$Pressure, x$Weight)
cor.test(x$Pressure, x$Beat)

x$Alaki = c(7,3,2,5,19,1)

cor(x$Pressure, x$Alaki)
cor.test(x$Pressure, x$Alaki)
ggplot(x, aes(Pressure, Alaki))+ geom_point()+
  geom_smooth(method = "lm")

x$Alaki = runif(min = 1, max = 10, n = 6)
cor(x$Pressure, x$Alaki)
cor.test(x$Pressure, x$Alaki)

#-------------------
x <- read.delim("Endoderm.txt", row.names = 1)
x.cor <- cor(x)
head(x.cor)
any(is.na(x))


x <- read.delim("Endoderm.txt", row.names = 1)
x <- log2(x + 1)
x <- na.omit(x)
dim(x)
x.cor <- cor(x)
head(x)

library(pheatmap)
library(ggplot2)

pheatmap(x.cor)
ggplot(x, aes(x=NEUROD1, y=NKX6.1))+geom_point()
ggplot(x, aes(x=HLXb9, y=HHEX))+geom_point()+geom_smooth(method="lm")

y <- t(x)
head(y)
y.cor <- cor(y)
pheatmap(y.cor)
y.cor <- na.omit(y.cor)
dim(y.cor)
head(y)

dim(y[,2:3])
y[,2:3] = y[,2:3] + runif(18, min=-0.001, max=0.001 )
head(y)
y.cor <- cor(y)
pheatmap(y.cor, border_color = F)

### -------------------------------------------------------------------------------------

summary(x[,1])
# Melting a data.frame or table
# install.packages("reshape", dependencies = TRUE, repos = 'http://cran.rstudio.com/')
library(reshape)

x.m <- melt(x)
head(x.m)

ggplot(x.m, aes(x=variable, y=value, fill = variable))+geom_boxplot(outlier.size = 0)

y <- na.omit(x)
y <- t(y)
y.m <- melt(y)
head(y.m)
ggplot(y.m, aes(x = X2, y = value, fill = X2))+geom_boxplot()

class(x)
class(y)

ggplot(x.m, aes(x=variable, y=value, fill = variable))+geom_violin()

### -------------------------------------------------------------------------------------

# Spearman Correlation Coefficient
a <- c(0,4,3,0,1)
b <- c(37,39,38,36.8,37.5)
cor(a,b,method = "spearman")


pheatmap(x, border_color = NA)
pheatmap(x, border_color = NA, clustering_distance_rows = "correlation", clustering_distance_cols = "correlation")

colnames(x)
rownames(x)


x <- na.omit(x)
y <- x[-2:-3,]
head(y)
dim(y)
pheatmap(y)
pheatmap(y, border_color = NA, clustering_distance_rows = "correlation", clustering_distance_cols = "correlation")


cor(y[,c("HHEX","HLXb9")])
cor(y[,c("HHEX","HLXb9")], method = "spearman")
ggplot(y,aes(HHEX,HLXb9))+geom_point()+
  geom_smooth(method = "lm")


cor(y[,c("ISL1","HLXb9")])
cor(y[,c("ISL1","HLXb9")], method = "spearman")
ggplot(y,aes(ISL1,HLXb9))+geom_point()+
  geom_smooth(method = "lm")


z <- data.frame(weight = c(45,55,65,75,85,95,105), height = c(165,170,175,180,190,200,150))
ggplot(z, aes(weight, height))+geom_point()+geom_smooth(method = "lm")
cor(z)
cor(z, method = "spearman")


# sapply and lapply
rowMeans(x)
colMeans(x)

rowMin(x)  # ????
rowVar(x)  # ????

apply(x, 1, var)  # 1 means row and 2 means column
var(x[4,])
var(as.numeric(x[4,]))

apply(x,1,min)
apply(x,2,min, na.rm = T)
min(x$HLXb9, na.rm = T)

y <- data.frame(Age=c(20,30,NA), Weight=c(45,55,65))
z <- na.omit(y)
apply(y, 2, max)
apply(z, 2, max)
apply(y, 2, max, na.rm = T)

# lapply is specific for list  # sapply is simple version of lapply
lapply(1:3, function(x) x^2)
sapply(1:3, function(x) x^2)

rownames(x)

t.test(x[4:10,1], x[11:32, 1])
t.test(x[4:10,2], x[11:32, 2])

MyTest <- function(i){
  t.test(x[4:10,i], x[11:32, i])$p.value
}

MyTest(1)
MyTest(2)

sapply(1:ncol(x), MyTest)

MyTest2 <- function(y){
  t.test(y[4:10], y[11:32])$p.value
}

apply(x, 2, MyTest2)

sapply(1:10, print)

### -------------------------------------------------------------------------------------

sd(x[4:6,1])

se <- function(x){
  e <- sd(x)/sqrt(length(x))
  return(e)
}

se(x[4:6,1])

alaki <- function(x) x + 1
alaki(1:4)

alaki <- function(x,y){
  x * y
}
alaki(3,7)
alaki(2:3, 4:5)
alaki(1:100,0.1)

sd(c(3,5,7))
se(c(3,5,7))

options("digits")
options(digits = 4)

# ---------------------
x$Sample <- rownames(x)
head(x)
x$Sample <- substr(x$Sample, 1, nchar(x$Sample)-2)
head(x)

substr("Hello", 1,3)
nchar(x$Sample)

x.m <- aggregate(.~Sample, x, mean)
x.m
ggplot(x.m, aes(Sample, HLXb9, fill = Sample))+geom_bar(stat = "identity")

### -------------------------------------------------------------------------------------

pc <- prcomp(x)
any(is.na(x))
y <- na.omit(x)
dim(x)
dim(y)

head(x)
x <- x[,-10]
head(x)
pc <- prcomp(x)
pc
plot(pc)

pcx <- data.frame(pc$x)
head(pcx)
ggplot(pcx, aes(x=PC1, y=PC2))+geom_point()
plot(x[,3:4])

pcx$Sample <- rownames(pcx)
head(pcx)
pcx$Sample <- substr(pcx$Sample, 1, nchar(pcx$Sample)-2)
head(pcx)
ggplot(pcx, aes(x=PC1, y=PC2, color = Sample))+geom_point(size = 3)

pcr <- pc$rotation
pcr[,1]

barplot(x$PDX1)
barplot(x$PAX4)

pcr[,2]
pcr <- data.frame(pcr)
pcr$Gene <- rownames(pcr)
ggplot(pcr, aes(PC1, PC2, label = Gene))+geom_point(size = 3) +
  geom_text()
#------------------------------------------------------------------------------------
library(grid)

theme_complete_bw <- function(base_size = 24, base_family = "") 
{
  theme_grey(base_size = base_size, base_family = base_family) %+replace% 
    theme(
      axis.line =         element_blank(),
      axis.text.x =       element_text(size = base_size * 0.8 , lineheight = 0.9, colour = "black", vjust = 1),
      axis.text.y =       element_text(size = base_size * 0.8, lineheight = 0.9, colour = "black", hjust = 1),
      axis.ticks =        element_line(colour = "black"),
      axis.title.x =      element_text(size = base_size, vjust = 0.5),
      axis.title.y =      element_text(size = base_size, angle = 90, vjust = 0.5),
      axis.ticks.length = unit(0.15, "cm"),
      axis.ticks.margin = unit(0.1, "cm"),
      
      legend.background = element_rect(colour=NA), 
      legend.key =        element_rect(fill =NA, colour = "black", size = 0.25),
      legend.key.size =   unit(1.5, "lines"),
      legend.text =       element_text(size = base_size * 0.7),
      legend.title =      element_text(size = base_size * 0.8),
      legend.position =   "top",
      
      panel.background = element_rect(fill = "white", colour = NA), 
      panel.border =     element_rect(fill = NA, colour = "black", size=2), 
      panel.grid.major = element_line(colour = NA, size = 0.2), #"grey"
      panel.grid.minor = element_line(colour = NA, size = 0.5), #"grey"
      panel.margin =     unit(0.25, "lines"),
      
      strip.background = element_rect(fill = NA, colour = NA), 
      strip.text.x =     element_text(colour = "black", size = base_size * 0.8),
      strip.text.y =     element_text(colour = "black", size = base_size * 0.8, angle = +90),
      
      plot.background =  element_rect(colour = NA, fill = "white"),
      plot.title =       element_text(size = base_size*.8),
      plot.margin =      unit(c(1, 1, .5, .5), "lines"))
}
# -------------------------------------------------------------------------------------
ggplot(pcr, aes(PC1, PC2, label = Gene))+geom_point(size = 3) +
  geom_text()+theme_complete_bw()

pdf("PCA.pdf")
ggplot(pcr, aes(PC1, PC2, label = Gene))+geom_point(size = 3) +
  geom_text()+theme_complete_bw()

ggplot(pcx, aes(PC1, PC2, color = Sample))+geom_point(size = 3) +
  theme_complete_bw()
dev.off()

x[,3]
x[,3,drop=F] # Don't change the data frame format

install.packages("rgl", dependencies = TRUE, repos = 'http://cran.rstudio.com/')
library(rgl)
plot3d(pcx[,1:3])

### -------------------------------------------------------------------------------------

x$Sample <- rownames(x)
x$Sample <- substr(x$Sample, 1, nchar(x$Sample)-2)
head(x)

y <- x[,c("HLXb9", "Sample")]
y
t.test(y[y$Sample == "SC",1], y[y$Sample == "DE", 1])

subset(y, Sample == "SC")[,1]
subset(y, Sample == "DE")[,1]

subset(x, HLXb9 > 1.5 * PDX1)

# paste function
alaki <- data.frame(Gene = paste("Gene", 1:100), FC = runif(100, min=-10, max=10))
head(alaki)

paste("Hello","World")
paste(1:10,"S")

subset(alaki, FC>9)
subset(alaki, abs(FC)> 9)
#----------------------
var(subset(x, Sample =="SC")[,1])
var(subset(x, Sample =="DE")[,1])

y
a <- aov(HLXb9 ~ Sample, y)
a
anova(a)
unique(y$Sample)

alaki <- anova(aov(PDX1 ~ Sample, x))
dim(alaki)
alaki
alaki[1,5]

AOV <- function(Gene){
  y <- x[,c(Gene, "Sample")]
  colnames(y)[1] = "Gene"
  anova(aov(Gene ~ Sample, y))[1,5]
}

AOV("HLXb9")

genes <- colnames(x)
genes <- genes[genes != "Sample"]
genes
sapply(genes, AOV)

ggplot(x, aes(Sample, HLXb9, fill = Sample))+geom_boxplot()
x$Sample <- factor(x$Sample, levels = unique(x$Sample))
as.numeric(x$Sample)
ggplot(x, aes(Sample, HLXb9, fill = Sample))+geom_boxplot()

### -------------------------------------------------------------------------------------
x <- x[-ncol(x)]
head(x)

x.t <- t(x)
head(x.t)
xc <- cor(x.t)
dim(xc)

x.t <- x.t[,-2:-3]
head(x.t)
xc <- cor(x.t)

pheatmap(xc,border_color = NA)

xc <- cor(x.t, method = "spearman")

pheatmap(xc,border_color = NA)

#-----------------------------
#Principal Component Analysis

pc <- prcomp(x)
dim(pc$r)
head(pc$x)

pcx <- data.frame(pc$x)
head(pcx)

pcx$Sample <- rownames(pcx)
pcx$Sample <- substr(pcx$Sample, 1, nchar(pcx$Sample)-2)
head(pcx)
ggplot(pcx, aes(PC1, PC2, color = Sample))+geom_point(size=3)+
  theme_complete_bw()

#-------
# ANOVA

head(x)
x.t <- t(x)
head(x.t)

class(x)
x.m <- melt(as.matrix(x))
head(x.m)

colnames(x.m) <- c("Sample", "Gene", "Exp")
head(x.m)
anova(aov(Exp ~ Gene+Sample , x.m))


x.m2 <- melt(as.matrix(x[4:6,]))
x.m2
colnames(x.m2) <- c("Sample", "Gene", "Exp")
anova(aov(Exp ~ Gene+Sample , x.m2))


x.m3 <- melt(as.matrix(x[4:10,]))
x.m3
colnames(x.m3) <- c("Sample", "Gene", "Exp")
anova(aov(Exp ~ Gene+Sample , x.m3))

x.m4 <- melt(as.matrix(x[3:10,]))
x.m4
colnames(x.m4) <- c("Sample", "Gene", "Exp")
anova(aov(Exp ~ Gene+Sample , x.m4))

### ------------------------------------------------------------------------------------

a <- rnorm(200)
b <- rnorm(300)
t.test(a,b)

a <- rnorm(200)
b <- rnorm(300, mean = 1)
t.test(a,b)

summary(a)
summary(b)

c <- runif(300)
d <- runif(300)
plot(c,d)

a <- rnorm(300)
b <- rnorm(300)
plot(a,b)

wilcox.test(c,d) # We can't use t.test() because our data distribution is uniform (not normal)
t.test(a,b)


c <- runif(300)
d <- runif(300, max = 1.5)
wilcox.test(c,d)

# How to find a data distribution is normal or not?
qqplot(c,d)
qqplot(a,b)

qqplot(a,c)

f <- c(5, 7, 1, 14, 25, 36, 12)
mean(f)
sd(f)
length(f)

g <- rnorm(7, mean = mean(f), sd = sd(f))
qqplot(f,g)

# Wilcox test
z <- data.frame(Person=1:7, Pressure1=runif(7,11,12), Pressure2=runif(7,10,11))
wilcox.test(z$Pressure1, z$Pressure2, paired = T)

# ggplot (bar plot with error bars)
y <- data.frame(Mean=colMeans(z[,-1]), SD = apply(z[,-1], 2, sd))
y
y$Group <- rownames(y)
y
ggplot(y, aes(Group, Mean, ymin = Mean -SD, ymax = Mean +SD, fill = Group))+ 
  geom_bar(stat = "identity")+ geom_errorbar(width = 0.1)



se <- function (x) sqrt(sd(x))/(length(x)-1)
y <- data.frame(Mean=colMeans(z[,-1]), SE = apply(z[,-1], 2, se))
y
y$Group <- rownames(y)
y
ggplot(y, aes(Group, Mean, ymin = Mean -SE, ymax = Mean +SE, fill = Group))+ 
  geom_bar(stat = "identity")+ geom_errorbar(width = 0.1)

# The End