#load excel packages into the library
library(xlsx)
library(xlsxjars)

#load Regression Analysis Packages into the library
library(MASS)
library(relaimpo)
install.packages("Rcmdr")
library(Rcmdr)
library(lmtest)

#import the data file into R
WorldSeries <- read.xlsx("MLB-WorldSeriesStats.xlsx", sheetName="WinPCTdata")
wsFinal <- read.xlsx("MLB-WorldSeriesStats.xlsx", sheetName="WPdataFINAL")

summary(wsFinal)

shapiro.test(WorldSeries$R)
shapiro.test(WorldSeries$RBI)
shapiro.test(WorldSeries$BB)
shapiro.test(WorldSeries$OBP)
shapiro.test(WorldSeries$OPS_plus)
shapiro.test(WorldSeries$P_Age)
shapiro.test(WorldSeries$W.L.)
shapiro.test(WorldSeries$ERA)
shapiro.test(WorldSeries$CG)
shapiro.test(WorldSeries$SHO)
shapiro.test(WorldSeries$ERA_Plus)
shapiro.test(WorldSeries$FIP)
shapiro.test(WorldSeries$WHIP)
shapiro.test(WorldSeries$H9)
shapiro.test(WorldSeries$BB9)
shapiro.test(WorldSeries$F_Age)
shapiro.test(WorldSeries$CG.1)
shapiro.test(WorldSeries$Rtot)
shapiro.test(WorldSeries$Rtot.yr)
shapiro.test(WorldSeries$RF.G)
shapiro.test(WorldSeries$Sept..Win.PCT)
shapiro.test(WorldSeries$Run.Diff)

#log transform
log.ws <- log(wsFinal[, 2:18])

#apply PCA - scale. = TRUE is highly advisable, but default is FALSE.
ws.pca <- prcomp(log.ws,
                 center = TRUE,
                 scale. = TRUE)

#print method
print(ws.pca)

#plot method
plot(ws.pca, type = "lines")

#summary method
summary(ws.pca)

#predict PCs
predict(ws.pca,
        newdata=tail(log.ws, 2))

library(devtools)
install_github("vqv/ggbiplot")
library(ggbiplot)
library(ggplot2)

g <- ggplot(ws.pca, obs.scale = 1, var.scale = 1,
              ellipse = TRUE,
              cirlce = TRUE)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal',
               legend.position = 'top')

print (g)

data(wsFinal, package = "alr3")

X=wsFinal[, 2:18]
Y=wsFinal[, 19]

pc.cor=princomp(X, cor = T)
pc.corLOG=princomp(log.ws, cor = T)
screeplot(pc.cor)
screeplot(pc.corLOG)
summary(pc.cor)
summary(pc.corLOG)

plot(pc.cor, type="lines", main="ScreePlot - PC Correlation (First 10 Components)")
pc.cor$scores
biplot(pc.cor)

library(FactoMineR)
#variable factor map for first two dimensions
result <- PCA(wsFinal[, 2:18]) # graphs generated automatically

#correlation matrix of all variables (including WinPCT)
install.packages("corrplot")
library(corrplot)
cor.matWP <- round(cor(wsFinal[, 2:19]),2)
par(mar = c(3,3,3,3))
corrplot(cor.matWP, type="upper", order="hclust", 
         tl.col="black", tl.srt=45)

#correlation matrix of 17 variables
cor.mat <- round(cor(X),2)
corrplot(cor.mat, type="upper", order="hclust", 
         tl.col="black", tl.srt=45)

#correlation grid of all variables with histograms
library(PerformanceAnalytics)
chart.Correlation(X, histogram=TRUE, pch=19)

res.pca <- PCA(X, graph = FALSE)
print(res.pca)

eigenvalues <- res.pca$eig
head(eigenvalues[, 1:2])

par(mar = c(3,3,3,3))
barplot(eigenvalues[, 2], names.arg=1:nrow(eigenvalues), 
        main = "Variances",
        xlab = "Principal Components",
        ylab = "Percentage of variances",
        col ="steelblue")
# Add connected line segments to the plot
lines(x = 1:nrow(eigenvalues), eigenvalues[, 2], 
      type="b", pch=19, col = "red")

fviz_pca_var(res.pca, col.var="contrib")+
  scale_color_gradient2(low="white", mid="blue", 
                        high="red", midpoint=55)+theme_bw()




PCA(wsFinal[, 2:18], scale.unit = TRUE, ncp = 17, graph = TRUE)

plot.PCA(wsFinal[, 2:18], axes = c(1,2), choix=c("ind", "var"))

head(res.pca$var$coord)
head(res.pca$var$cos2)
head(res.pca$var$contrib)

plot(res.pca, choix = "var")

head(res.pca$ind$coord)
head(res.pca$ind$cos2)
head(res.pca$ind$contrib)

plot(res.pca, choix = "ind")

fviz_screeplot(res.pca, ncp=10)

fviz_pca_ind(res.pca, col.ind="cos2", axes = c(3,4)) +
  scale_color_gradient2(low="dark red", mid="orange", 
                        high="dark green", midpoint=0.50)

fviz_pca_var(res.pca, col.var="contrib", axes= c(3,4))+
  scale_color_gradient2(low="dark red", mid="orange", 
                        high=" dark green", midpoint=55)+theme_bw()

install.packages("devtools")
library("devtools")
install_github("kassambara/factoextra")
library(factoextra)

dim(pc.cor$scores)

OffDef = pc.cor$scores[,1]
Team = pc.cor$scores[,2]
AgeDef = pc.cor$scores[,3]
OPS.ERA.CG = pc.cor$scores[,4]
WinPCT = wsFinal$Reg..WinPCT

pclm = lm( wsFinal$Wins ~ OffDef + Team + AgeDef + OPS.ERA.CG)

summary.lm(pclm, correlation = T)
residuals(pclm)

par(mfrow=c(2, 2))
plot(pclm, pch=18, lty=6, lwd=3)

par(mfrow=c(1, 1))
rawlm = lm(wsFinal$Wins ~., data = X)
summary.lm(rawlm, correlation = T)

fviz_pca_biplot(res.pca,  geom = "text")



