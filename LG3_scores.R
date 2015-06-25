library(lattice)
library(ggplot2)
library(stats)

gdays <- read.csv('/Volumes/group_dv/personal/DValenzano/Dec2014/surv-sex-logrank.csv', sep=',', header=TRUE)
# To merge the dot plot for surv-qtl sex and log-rank test values, I need to put everything in the same column
negl_qvalg <- c(gdays$neg_log.qval., gdays$neg_log.qval._sex, gdays$log.rank) 

gd0 <- rbind(gdays, gdays, gdays)
gd0$LOD <- negl_qvalg
gd0$LOD_gr <- c(rep('surv(RF)', length(gdays$neg_log.qval.)), rep('sex(RF)', length(gdays$neg_log.qval._sex)), rep('log-rank', length(gdays$log.rank)))  

g1 <- qplot(cM,LOD, data=gd0, xlab="cM", ylab="-log(p/q)", colour=LOD_gr, main="LG3, cross G Sex (RF), Survival (RF) QTL and log-rank scores", ylim=c(0,11))
dev.new()
#g1+scale_colour_brewer(palette="Set5")
g1

#### Dec04-2014 #####

gdays2 <- read.csv('/Volumes/group_dv/personal/DValenzano/Dec2014/surv-sex-logrank2.csv', sep=',' , header=TRUE)
names(gdays2)
# To merge the dot plot for surv-qtl, sex and log-rank test values, I need to put everything in the same column
negl_qvalg2 <- c(gdays2$neg_log.qval., gdays2$neg_log.qval._sex, gdays2$log.rank) 

gd02 <- rbind(gdays2, gdays2, gdays2)

gd02$LOD <- negl_qvalg2

gd02$LOD_gr <- c(rep('surv(RF)', length(gdays2$neg_log.qval.)), rep('sex(RF)', length(gdays2$neg_log.qval._sex)), rep('log-rank', length(gdays2$log.rank)))  
names(gd02)
g2 <- qplot(cM,LOD, data=gd02, xlab="cM", ylab="-log(p/q)", colour=LOD_gr, main="LG3, cross G Sex (RF), Survival (RF) QTL and log-rank scores", ylim=c(0,11))
g2


####################### Goal: combine plots

negl_qvalg3 <- c(gdays2$neg_log.qval., gdays2$neg_log.qval._sex) 

gd03 <- rbind(gdays2, gdays2)

gd03$LOD <- negl_qvalg3

gd03$LOD_gr <- c(rep('surv(RF)', length(gdays2$neg_log.qval.)), rep('sex(RF)', length(gdays2$neg_log.qval._sex)))  

g3 <- qplot(cM,LOD, data=gd03, xlab="cM", ylab="-log(q)", colour=LOD_gr, main="LG3, cross G Sex (RF) and Survival (RF) QTL", ylim=c(0,8))
g3

## Alternatively
plot(gdays2$cM, gdays2$neg_log.qval., xlab="cM", ylab="-log(q)", col=c("tomato2"), ylim=c(0,8), pch=16)
points(gdays2$cM, gdays2$neg_log.qval._sex, xlab="cM", ylab="-log(q)", col=c("mediumblue"), pch=16)
grid()

x<-gdays2$cM
y<-gdays2$log.rank
lr <- data.frame(x,y)

plot(x,y, lwd=2.5, ylim=c(0,10), pch=19, ylab="-log(p value)", xlab='cM')

layout(matrix(c(1,1,2),3,1,byrow=TRUE))
plot(gdays2$cM, gdays2$neg_log.qval., xlab="cM", ylab="-log(q value)", col=c("tomato2"), ylim=c(0,8), pch=16, main="Random-Forest Analysis")
points(gdays2$cM, gdays2$neg_log.qval._sex, xlab="cM", ylab="-log(q)", col=c("mediumblue"), pch=16)
legend(200,7.5,c("survival", "sex"), col=c("tomato2", "mediumblue"), cex=1.2, pch=c(16,16), bty="n")
grid()
plot(x,y, lwd=2.5, ylim=c(0,10), pch=19, ylab="-log(p value)", xlab='cM', main="Log-Rank Test")
#legend(200,9.3,c("survival"), col=c("black"), cex=1.2, pch=c(16), bty="n")
grid()

######### SURVIVAL ON FEMALES ONLY ###############

gdays3 <- read.csv('/Volumes/group_dv/personal/DValenzano/Dec2014/surv-sex-logrank3.csv', sep=',' , header=TRUE)

xf<-gdays3$cM
yf<-gdays3$log.rankf
lrf <- data.frame(xf,yf)

plot(xf,yf, lwd=2.5, ylim=c(0,10), pch=19, ylab="-log(p value)", xlab='cM')

######MULTIPLE PLOTS#######

layout(matrix(c(1,1,2,3),4,1,byrow=TRUE))
plot(gdays2$cM, gdays2$neg_log.qval., xlab="cM", ylab="-log(q value)", col=c("tomato2"), ylim=c(0,8), pch=16, main="Random-Forest Analysis")
points(gdays2$cM, gdays2$neg_log.qval._sex, xlab="cM", ylab="-log(q)", col=c("mediumblue"), pch=16)
legend(200,7.5,c("survival", "sex"), col=c("tomato2", "mediumblue"), cex=1.2, pch=c(16,16), bty="n")
grid()
plot(x,y, lwd=2.5, ylim=c(0,10), pch=19, ylab="-log(p value)", xlab='cM', main="Log-Rank Test (all)")
legend(200,9.3,c("survival"), col=c("black"), cex=1.2, pch=c(16), bty="n")
grid()
plot(xf,yf, lwd=2.5, ylim=c(0,10), pch=19, ylab="-log(p value)", xlab='cM', main="Log-Rank Test (females)")
legend(200,9.3,c("survival"), col=c("black"), cex=1.2, pch=c(16), bty="n")
grid()

######### SURVIVAL ON MALES ONLY ###############

gdays4 <- read.csv('/Volumes/group_dv/personal/DValenzano/Dec2014/surv-sex-logrank4.csv', sep=',' , header=TRUE)

xm<-gdays4$cM
ym<-gdays4$log.rankm
lrm <- data.frame(xm,ym)

plot(xm,ym, lwd=2.5, ylim=c(0,10), pch=19, ylab="-log(p value)", xlab='cM')

####### MULTIPLE PLOTS ################

layout(matrix(c(1,1,2,3,4),5,1,byrow=TRUE))
plot(gdays2$cM, gdays2$neg_log.qval., xlab="cM", ylab="-log(q value)", col=c("tomato2"), ylim=c(0,8), pch=16, main="Random-Forest Analysis")
points(gdays2$cM, gdays2$neg_log.qval._sex, xlab="cM", ylab="-log(q)", col=c("mediumblue"), pch=16)
legend(200,7.5,c("survival", "sex"), col=c("tomato2", "mediumblue"), cex=1.2, pch=c(16,16), bty="n")
grid()
plot(x,y, lwd=2.5, ylim=c(0,10), pch=19, ylab="-log(p value)", xlab='cM', main="Log-Rank Test (all)")
#legend(200,9.3,c("survival"), col=c("black"), cex=1.2, pch=c(16))
grid()
plot(xf,yf, lwd=2.5, ylim=c(0,10), pch=19, ylab="-log(p value)", xlab='cM', main="Log-Rank Test (females)")
#legend(200,9.3,c("survival"), col=c("black"), cex=1.2, pch=c(16))
grid()
plot(xm,ym, lwd=2.5, ylim=c(0,10), pch=19, ylab="-log(p value)", xlab='cM', main="Log-Rank Test (males)")
#legend(200,9.3,c("survival"), col=c("black"), cex=1.2, pch=c(16))
grid()

