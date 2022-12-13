

# Packages
library(ggplot2)
library(maps)
library(shape)

# Directories
outdir <- "output/raw"


################# LB #####################

# read data WI
output_WI_LB = readRDS(file.path(outdir, "N_WI_LB.rda"))
LB_arrow_WI = read.csv(file.path(outdir, "N_WI_LB_quantities.csv"))

# remove results that R_hat is greater than 1.05 
remove_output = rep(0,length(output_WI_LB[,1,1]))
for (i in 1:length(output_WI_LB[,1,1])){
  if (max(output_WI_LB[i,,6])>1.05) {
    remove_output[i] = 1
  }
}
output_WI_LB = output_WI_LB[!remove_output,,]
LB_arrow_WI = LB_arrow_WI[!remove_output,]

# read data MN
output_MN_LB = readRDS(file.path(outdir, "N_MN_LB.rda"))
LB_arrow_MN = read.csv(file.path(outdir, "N_MN_LB_quantities.csv"))

# remove results that R_hat is greater than 1.05 
remove_output = rep(0,length(output_MN_LB[,1,1]))
for (i in 1:length(output_MN_LB[,1,1])){
  if (max(output_MN_LB[i,,6])>1.05) {
    remove_output[i] = 1
  }
}
output_MN_LB = output_MN_LB[!remove_output,,]
LB_arrow_MN = LB_arrow_MN[!remove_output,]

# creat figure data frame
figure_temp_influence = data.frame(impact_E=c(output_WI_LB[,8,1],output_MN_LB[,8,1]),low=c(output_WI_LB[,8,4],output_MN_LB[,8,4]),high=c(output_WI_LB[,8,5],output_MN_LB[,8,5]))

# plot temperature influence
# jpeg("Output/Figure/ALL/N_Figure1_Temp_Influence_LB.jpeg",width=5,height=9,res=600,units = "in")
plot(1:10, 1:10, type="n", bty="n", yaxt="n", cex.axis=1,
     xlim=c(-1, 1), ylim=c(1,nrow(figure_temp_influence)),
     xlab="Impact_E", ylab="", cex.main=1)
figure_temp_influence = figure_temp_influence[order(figure_temp_influence$impact_E),]
mu_est = figure_temp_influence$impact_E/2
mu_min = figure_temp_influence$low/2
mu_max = figure_temp_influence$high/2
col_line = rep("black",nrow(figure_temp_influence))
for (i in 1:nrow(figure_temp_influence)){
  if (mu_min[i]>0){
    col_line[i] = "red"
  }
  if(mu_max[i]<0){
    col_line[i] = "blue"
  }
}
sapply(1:nrow(figure_temp_influence), function(x) lines(x=c(mu_min[x], mu_max[x]), y=c(x,x), lwd=0.6, col=col_line[x]))
sapply(1:nrow(figure_temp_influence), function(x) points(x=mu_est[x], y=c(1:nrow(figure_temp_influence))[x], col=col_line[x],pch=16))
lines(x=c(0,0), y=c(1, nrow(figure_temp_influence)), lty=3, col="black", lwd=0.8)
lines(x=c(median(mu_est),median(mu_est)), y=c(1, nrow(figure_temp_influence)), lty=3, col="red", lwd=1.6)
dev.off()

## Arrow figure showing the ratio of biomass over k for the terminal years assuming no fishing or no environmental impact
figure_arrow = data.frame(depletion=c(LB_arrow_WI$P_terminal,LB_arrow_MN$P_terminal))
figure_arrow$depletion_no_env = c(LB_arrow_WI$P_no_env,LB_arrow_MN$P_no_env)
figure_arrow$depletion_no_fish = c(LB_arrow_WI$P_no_fishing,LB_arrow_MN$P_no_fishing)
figure_arrow = figure_arrow[order(figure_arrow$depletion),]
# jpeg("Output/Figure/ALL/N_arrow_LB.jpeg",width = 4, height=length(figure_arrow$depletion)/8, units = "in", res = 600)
plot(1:10, 1:10, type="n", bty="n", yaxt="n", cex.axis=1,
     xlim=c(0, 1), ylim=c(1,nrow(figure_arrow)),
     xlab="B/K in terminal year", ylab="", cex.main=1)
mu_est = figure_arrow$depletion
mu_min = figure_arrow$depletion_no_env
mu_max = figure_arrow$depletion_no_fish
sapply(1:nrow(figure_arrow), function(x) points(x=mu_est[x], y=c(1:nrow(figure_arrow))[x], col="black",pch=16))
sapply(1:nrow(figure_arrow), function(x) Arrows(mu_est[x], c(1:nrow(figure_arrow))[x], mu_min[x], c(1:nrow(figure_arrow))[x], col="red"))
sapply(1:nrow(figure_arrow), function(x) Arrows(mu_est[x], c(1:nrow(figure_arrow))[x], mu_max[x], c(1:nrow(figure_arrow))[x], col="blue"))
abline(v = mean(figure_arrow$depletion), col="black", lwd=3, lty=2)
# dev.off()

# jpeg("Output/Figure/ALL/N_hist_no_env_LB.jpeg",width = 4.2, height=2.75, units = "in", res = 600)
distance_env = -figure_arrow$depletion_no_env+figure_arrow$depletion
distance_fishing = -figure_arrow$depletion_no_fish+figure_arrow$depletion
hist(distance_env,main="",xlab="Net differences of B/k in terminal years\n caused by temperature change",col="red",xlim=c(min(distance_fishing,distance_env),max(distance_fishing,distance_env)))
# dev.off()

# jpeg("Output/Figure/ALL/N_hist_no_fishing_LB.jpeg",width = 4.2, height=2.75, units = "in", res = 600)
distance_env = -figure_arrow$depletion_no_env+figure_arrow$depletion
distance_fishing = -figure_arrow$depletion_no_fish+figure_arrow$depletion
hist(distance_fishing,main="",xlab="Net differences of B/k in terminal years\n caused by fishing",col="blue",xlim=c(min(distance_fishing,distance_env),max(distance_fishing,distance_env)))
# dev.off()

################# BC #####################

# read data WI
output_WI_BC = readRDS(file.path(outdir, "N_WI_BC.rda"))
BC_arrow_WI = read.csv(file.path(outdir, "N_WI_BC_quantities.csv"))

# remove results that R_hat is greater than 1.05 
remove_output = rep(0,length(output_WI_BC[,1,1]))
for (i in 1:length(output_WI_BC[,1,1])){
  if (max(output_WI_BC[i,,6])>1.05) {
    remove_output[i] = 1
  }
}
output_WI_BC = output_WI_BC[!remove_output,,]
BC_arrow_WI = BC_arrow_WI[!remove_output,]

# read data MN
output_MN_BC = readRDS(file.path(outdir, "N_MN_BC.rda"))
BC_arrow_MN = read.csv(file.path(outdir, "N_MN_BC_quantities.csv"))

# remove results that R_hat is greater than 1.05 
remove_output = rep(0,length(output_MN_BC[,1,1]))
for (i in 1:length(output_MN_BC[,1,1])){
  if (max(output_MN_BC[i,,6])>1.05) {
    remove_output[i] = 1
  }
}
output_MN_BC = output_MN_BC[!remove_output,,]
BC_arrow_MN = BC_arrow_MN[!remove_output,]

# creat figure data frame
figure_temp_influence = data.frame(impact_E=c(output_WI_BC[,8,1],output_MN_BC[,8,1]),low=c(output_WI_BC[,8,4],output_MN_BC[,8,4]),high=c(output_WI_BC[,8,5],output_MN_BC[,8,5]))

# plot temperature influence
# jpeg("Output/Figure/ALL/N_Figure1_Temp_Influence_BC.jpeg",width=5,height=9,res=600,units = "in")
plot(1:10, 1:10, type="n", bty="n", yaxt="n", cex.axis=1,
     xlim=c(-1, 1), ylim=c(1,nrow(figure_temp_influence)),
     xlab="Impact_E", ylab="", cex.main=1)
figure_temp_influence = figure_temp_influence[order(figure_temp_influence$impact_E),]
mu_est = figure_temp_influence$impact_E/2
mu_min = figure_temp_influence$low/2
mu_max = figure_temp_influence$high/2
col_line = rep("black",nrow(figure_temp_influence))
for (i in 1:nrow(figure_temp_influence)){
  if (mu_min[i]>0){
    col_line[i] = "red"
  }
  if(mu_max[i]<0){
    col_line[i] = "blue"
  }
}
sapply(1:nrow(figure_temp_influence), function(x) lines(x=c(mu_min[x], mu_max[x]), y=c(x,x), lwd=0.6, col=col_line[x]))
sapply(1:nrow(figure_temp_influence), function(x) points(x=mu_est[x], y=c(1:nrow(figure_temp_influence))[x], col=col_line[x],pch=16))
lines(x=c(0,0), y=c(1, nrow(figure_temp_influence)), lty=3, col="black", lwd=0.8)
lines(x=c(median(mu_est),median(mu_est)), y=c(1, nrow(figure_temp_influence)), lty=3, col="red", lwd=1.6)
# dev.off()

## Arrow figure showing the ratio of biomass over k for the terminal years assuming no fishing or no environmental impact
figure_arrow = data.frame(depletion=c(BC_arrow_WI$P_terminal,BC_arrow_MN$P_terminal))
figure_arrow$depletion_no_env = c(BC_arrow_WI$P_no_env,BC_arrow_MN$P_no_env)
figure_arrow$depletion_no_fish = c(BC_arrow_WI$P_no_fishing,BC_arrow_MN$P_no_fishing)
figure_arrow = figure_arrow[order(figure_arrow$depletion),]
# jpeg("Output/Figure/ALL/N_arrow_BC.jpeg",width = 4, height=length(figure_arrow$depletion)/8, units = "in", res = 600)
plot(1:10, 1:10, type="n", bty="n", yaxt="n", cex.axis=1,
     xlim=c(0, 1), ylim=c(1,nrow(figure_arrow)),
     xlab="B/K in terminal year", ylab="", cex.main=1)
mu_est = figure_arrow$depletion
mu_min = figure_arrow$depletion_no_env
mu_max = figure_arrow$depletion_no_fish
sapply(1:nrow(figure_arrow), function(x) points(x=mu_est[x], y=c(1:nrow(figure_arrow))[x], col="black",pch=16))
sapply(1:nrow(figure_arrow), function(x) Arrows(mu_est[x], c(1:nrow(figure_arrow))[x], mu_min[x], c(1:nrow(figure_arrow))[x], col="red"))
sapply(1:nrow(figure_arrow), function(x) Arrows(mu_est[x], c(1:nrow(figure_arrow))[x], mu_max[x], c(1:nrow(figure_arrow))[x], col="blue"))
abline(v = mean(figure_arrow$depletion), col="black", lwd=3, lty=2)
# dev.off()

# jpeg("Output/Figure/ALL/N_hist_no_env_BC.jpeg",width = 4.2, height=2.75, units = "in", res = 600)
distance_env = -figure_arrow$depletion_no_env+figure_arrow$depletion
distance_fishing = -figure_arrow$depletion_no_fish+figure_arrow$depletion
hist(distance_env,main="",xlab="Net differences of B/k in terminal years\n caused by temperature change",col="red",xlim=c(min(distance_fishing,distance_env),max(distance_fishing,distance_env)))
# dev.off()

# jpeg("Output/Figure/ALL/N_hist_no_fishing_BC.jpeg",width = 4.2, height=2.75, units = "in", res = 600)
distance_env = -figure_arrow$depletion_no_env+figure_arrow$depletion
distance_fishing = -figure_arrow$depletion_no_fish+figure_arrow$depletion
hist(distance_fishing,main="",xlab="Net differences of B/k in terminal years\n caused by fishing",col="blue",xlim=c(min(distance_fishing,distance_env),max(distance_fishing,distance_env)))
# dev.off()

################# NP #####################
# read data WI
output_WI_NP = readRDS(file.path(outdir, "N_WI_NP.rda"))
NP_arrow_WI = read.csv(file.path(outdir, "N_WI_NP_quantities.csv"))

# remove results that R_hat is greater than 1.05 
remove_output = rep(0,length(output_WI_NP[,1,1]))
for (i in 1:length(output_WI_NP[,1,1])){
  if (max(output_WI_NP[i,,6])>1.05) {
    remove_output[i] = 1
  }
}
output_WI_NP = output_WI_NP[!remove_output,,]
NP_arrow_WI = NP_arrow_WI[!remove_output,]

# read data MN
output_MN_NP = readRDS(file.path(outdir, "N_MN_NP.rda"))
NP_arrow_MN = read.csv(file.path(outdir, "N_MN_NP_quantities.csv"))

# remove results that R_hat is greater than 1.05 
remove_output = rep(0,length(output_MN_NP[,1,1]))
for (i in 1:length(output_MN_NP[,1,1])){
  if (max(output_MN_NP[i,,6])>1.05) {
    remove_output[i] = 1
  }
}
output_MN_NP = output_MN_NP[!remove_output,,]
NP_arrow_MN = NP_arrow_MN[!remove_output,]

# creat figure data frame
figure_temp_influence = data.frame(impact_E=c(output_WI_NP[,8,1],output_MN_NP[,8,1]),low=c(output_WI_NP[,8,4],output_MN_NP[,8,4]),high=c(output_WI_NP[,8,5],output_MN_NP[,8,5]))

# plot temperature influence
# jpeg("Output/Figure/ALL/N_Figure1_Temp_Influence_NP.jpeg",width=5,height=9,res=600,units = "in")
plot(1:10, 1:10, type="n", bty="n", yaxt="n", cex.axis=1,
     xlim=c(-1, 1), ylim=c(1,nrow(figure_temp_influence)),
     xlab="Impact_E", ylab="", cex.main=1)
figure_temp_influence = figure_temp_influence[order(figure_temp_influence$impact_E),]
mu_est = figure_temp_influence$impact_E/2
mu_min = figure_temp_influence$low/2
mu_max = figure_temp_influence$high/2
col_line = rep("black",nrow(figure_temp_influence))
for (i in 1:nrow(figure_temp_influence)){
  if (mu_min[i]>0){
    col_line[i] = "red"
  }
  if(mu_max[i]<0){
    col_line[i] = "blue"
  }
}
sapply(1:nrow(figure_temp_influence), function(x) lines(x=c(mu_min[x], mu_max[x]), y=c(x,x), lwd=0.6, col=col_line[x]))
sapply(1:nrow(figure_temp_influence), function(x) points(x=mu_est[x], y=c(1:nrow(figure_temp_influence))[x], col=col_line[x],pch=16))
lines(x=c(0,0), y=c(1, nrow(figure_temp_influence)), lty=3, col="black", lwd=0.8)
lines(x=c(median(mu_est),median(mu_est)), y=c(1, nrow(figure_temp_influence)), lty=3, col="red", lwd=1.6)
# dev.off()

## Arrow figure showing the ratio of biomass over k for the terminal years assuming no fishing or no environmental impact
figure_arrow = data.frame(depletion=c(NP_arrow_WI$P_terminal,NP_arrow_MN$P_terminal))
figure_arrow$depletion_no_env = c(NP_arrow_WI$P_no_env,NP_arrow_MN$P_no_env)
figure_arrow$depletion_no_fish = c(NP_arrow_WI$P_no_fishing,NP_arrow_MN$P_no_fishing)
figure_arrow = figure_arrow[order(figure_arrow$depletion),]
# jpeg("Output/Figure/ALL/N_arrow_NP.jpeg",width = 4, height=length(figure_arrow$depletion)/8, units = "in", res = 600)
plot(1:10, 1:10, type="n", bty="n", yaxt="n", cex.axis=1,
     xlim=c(0, 1), ylim=c(1,nrow(figure_arrow)),
     xlab="B/K in terminal year", ylab="", cex.main=1)
mu_est = figure_arrow$depletion
mu_min = figure_arrow$depletion_no_env
mu_max = figure_arrow$depletion_no_fish
sapply(1:nrow(figure_arrow), function(x) points(x=mu_est[x], y=c(1:nrow(figure_arrow))[x], col="black",pch=16))
sapply(1:nrow(figure_arrow), function(x) Arrows(mu_est[x], c(1:nrow(figure_arrow))[x], mu_min[x], c(1:nrow(figure_arrow))[x], col="red"))
sapply(1:nrow(figure_arrow), function(x) Arrows(mu_est[x], c(1:nrow(figure_arrow))[x], mu_max[x], c(1:nrow(figure_arrow))[x], col="blue"))
abline(v = mean(figure_arrow$depletion), col="black", lwd=3, lty=2)
# dev.off()

# jpeg("Output/Figure/ALL/N_hist_no_env_NP.jpeg",width = 4.2, height=2.75, units = "in", res = 600)
distance_env = -figure_arrow$depletion_no_env+figure_arrow$depletion
distance_fishing = -figure_arrow$depletion_no_fish+figure_arrow$depletion
hist(distance_env,main="",xlab="Net differences of B/k in terminal years\n caused by temperature change",col="red",xlim=c(min(distance_fishing,distance_env),max(distance_fishing,distance_env)))
# dev.off()

# jpeg("Output/Figure/ALL/N_hist_no_fishing_NP.jpeg",width = 4.2, height=2.75, units = "in", res = 600)
distance_env = -figure_arrow$depletion_no_env+figure_arrow$depletion
distance_fishing = -figure_arrow$depletion_no_fish+figure_arrow$depletion
hist(distance_fishing,main="",xlab="Net differences of B/k in terminal years\n caused by fishing",col="blue",xlim=c(min(distance_fishing,distance_env),max(distance_fishing,distance_env)))
# dev.off()

################# SB #####################

# read data WI
output_WI_SB = readRDS(file.path(outdir, "N_WI_SB.rda"))
SB_arrow_WI = read.csv(file.path(outdir, "N_WI_SB_quantities.csv"))

# remove results that R_hat is greater than 1.05 
remove_output = rep(0,length(output_WI_SB[,1,1]))
for (i in 1:length(output_WI_SB[,1,1])){
  if (max(output_WI_SB[i,,6])>1.05) {
    remove_output[i] = 1
  }
}
output_WI_SB = output_WI_SB[!remove_output,,]
SB_arrow_WI = SB_arrow_WI[!remove_output,]

# read data MN
output_MN_SB = readRDS(file.path(outdir, "N_MN_SB.rda"))
SB_arrow_MN = read.csv(file.path(outdir, "N_MN_SB_quantities.csv"))

# remove results that R_hat is greater than 1.05 
remove_output = rep(0,length(output_MN_SB[,1,1]))
for (i in 1:length(output_MN_SB[,1,1])){
  if (max(output_MN_SB[i,,6])>1.05) {
    remove_output[i] = 1
  }
}
output_MN_SB = output_MN_SB[!remove_output,,]
SB_arrow_MN = SB_arrow_MN[!remove_output,]

# creat figure data frame
figure_temp_influence = data.frame(impact_E=c(output_WI_SB[,8,1],output_MN_SB[,8,1]),low=c(output_WI_SB[,8,4],output_MN_SB[,8,4]),high=c(output_WI_SB[,8,5],output_MN_SB[,8,5]))

# plot temperature influence
# jpeg("Output/Figure/ALL/N_Figure1_Temp_Influence_SB.jpeg",width=5,height=9,res=600,units = "in")
plot(1:10, 1:10, type="n", bty="n", yaxt="n", cex.axis=1,
     xlim=c(-1, 1), ylim=c(1,nrow(figure_temp_influence)),
     xlab="Impact_E", ylab="", cex.main=1)
figure_temp_influence = figure_temp_influence[order(figure_temp_influence$impact_E),]
mu_est = figure_temp_influence$impact_E/2
mu_min = figure_temp_influence$low/2
mu_max = figure_temp_influence$high/2
col_line = rep("black",nrow(figure_temp_influence))
for (i in 1:nrow(figure_temp_influence)){
  if (mu_min[i]>0){
    col_line[i] = "red"
  }
  if(mu_max[i]<0){
    col_line[i] = "blue"
  }
}
sapply(1:nrow(figure_temp_influence), function(x) lines(x=c(mu_min[x], mu_max[x]), y=c(x,x), lwd=0.6, col=col_line[x]))
sapply(1:nrow(figure_temp_influence), function(x) points(x=mu_est[x], y=c(1:nrow(figure_temp_influence))[x], col=col_line[x],pch=16))
lines(x=c(0,0), y=c(1, nrow(figure_temp_influence)), lty=3, col="black", lwd=0.8)
lines(x=c(median(mu_est),median(mu_est)), y=c(1, nrow(figure_temp_influence)), lty=3, col="red", lwd=1.6)
# dev.off()

## Arrow figure showing the ratio of biomass over k for the terminal years assuming no fishing or no environmental impact
figure_arrow = data.frame(depletion=c(SB_arrow_WI$P_terminal,SB_arrow_MN$P_terminal))
figure_arrow$depletion_no_env = c(SB_arrow_WI$P_no_env,SB_arrow_MN$P_no_env)
figure_arrow$depletion_no_fish = c(SB_arrow_WI$P_no_fishing,SB_arrow_MN$P_no_fishing)
figure_arrow = figure_arrow[order(figure_arrow$depletion),]
# jpeg("Output/Figure/ALL/N_arrow_SB.jpeg",width = 4, height=length(figure_arrow$depletion)/8, units = "in", res = 600)
plot(1:10, 1:10, type="n", bty="n", yaxt="n", cex.axis=1,
     xlim=c(0, 1), ylim=c(1,nrow(figure_arrow)),
     xlab="B/K in terminal year", ylab="", cex.main=1)
mu_est = figure_arrow$depletion
mu_min = figure_arrow$depletion_no_env
mu_max = figure_arrow$depletion_no_fish
sapply(1:nrow(figure_arrow), function(x) points(x=mu_est[x], y=c(1:nrow(figure_arrow))[x], col="black",pch=16))
sapply(1:nrow(figure_arrow), function(x) Arrows(mu_est[x], c(1:nrow(figure_arrow))[x], mu_min[x], c(1:nrow(figure_arrow))[x], col="red"))
sapply(1:nrow(figure_arrow), function(x) Arrows(mu_est[x], c(1:nrow(figure_arrow))[x], mu_max[x], c(1:nrow(figure_arrow))[x], col="blue"))
abline(v = mean(figure_arrow$depletion), col="black", lwd=3, lty=2)
# dev.off()

# jpeg("Output/Figure/ALL/N_hist_no_env_SB.jpeg",width = 4.2, height=2.75, units = "in", res = 600)
distance_env = -figure_arrow$depletion_no_env+figure_arrow$depletion
distance_fishing = -figure_arrow$depletion_no_fish+figure_arrow$depletion
hist(distance_env,main="",xlab="Net differences of B/k in terminal years\n caused by temperature change",col="red",xlim=c(min(distance_fishing,distance_env),max(distance_fishing,distance_env)))
# dev.off()

# jpeg("Output/Figure/ALL/N_hist_no_fishing_SB.jpeg",width = 4.2, height=2.75, units = "in", res = 600)
distance_env = -figure_arrow$depletion_no_env+figure_arrow$depletion
distance_fishing = -figure_arrow$depletion_no_fish+figure_arrow$depletion
hist(distance_fishing,main="",xlab="Net differences of B/k in terminal years\n caused by fishing",col="blue",xlim=c(min(distance_fishing,distance_env),max(distance_fishing,distance_env)))
# dev.off()

################# CC #####################

# read data WI
output_WI_CC = readRDS(file.path(outdir, "N_WI_CC.rda"))
CC_arrow_WI = read.csv(file.path(outdir, "N_WI_CC_quantities.csv"))

# remove results that R_hat is greater than 1.05 
remove_output = rep(0,length(output_WI_CC[,1,1]))
for (i in 1:length(output_WI_CC[,1,1])){
  if (max(output_WI_CC[i,,6])>1.05) {
    remove_output[i] = 1
  }
}
output_WI_CC = output_WI_CC[!remove_output,,]
CC_arrow_WI = CC_arrow_WI[!remove_output,]

# read data MN
output_MN_CC = readRDS(file.path(outdir, "N_MN_CC.rda"))
CC_arrow_MN = read.csv(file.path(outdir, "N_MN_CC_quantities.csv"))

# remove results that R_hat is greater than 1.05 
remove_output = rep(0,length(output_MN_CC[,1,1]))
for (i in 1:length(output_MN_CC[,1,1])){
  if (max(output_MN_CC[i,,6])>1.05) {
    remove_output[i] = 1
  }
}
output_MN_CC = output_MN_CC[!remove_output,,]
CC_arrow_MN = CC_arrow_MN[!remove_output,]

# creat figure data frame
figure_temp_influence = data.frame(impact_E=c(output_WI_CC[,8,1],output_MN_CC[,8,1]),low=c(output_WI_CC[,8,4],output_MN_CC[,8,4]),high=c(output_WI_CC[,8,5],output_MN_CC[,8,5]))

# plot temperature influence
# jpeg("Output/Figure/ALL/N_Figure1_Temp_Influence_CC.jpeg",width=5,height=9,res=600,units = "in")
plot(1:10, 1:10, type="n", bty="n", yaxt="n", cex.axis=1,
     xlim=c(-1, 1), ylim=c(1,nrow(figure_temp_influence)),
     xlab="Impact_E", ylab="", cex.main=1)
figure_temp_influence = figure_temp_influence[order(figure_temp_influence$impact_E),]
mu_est = figure_temp_influence$impact_E/2
mu_min = figure_temp_influence$low/2
mu_max = figure_temp_influence$high/2
col_line = rep("black",nrow(figure_temp_influence))
for (i in 1:nrow(figure_temp_influence)){
  if (mu_min[i]>0){
    col_line[i] = "red"
  }
  if(mu_max[i]<0){
    col_line[i] = "blue"
  }
}
sapply(1:nrow(figure_temp_influence), function(x) lines(x=c(mu_min[x], mu_max[x]), y=c(x,x), lwd=0.6, col=col_line[x]))
sapply(1:nrow(figure_temp_influence), function(x) points(x=mu_est[x], y=c(1:nrow(figure_temp_influence))[x], col=col_line[x],pch=16))
lines(x=c(0,0), y=c(1, nrow(figure_temp_influence)), lty=3, col="black", lwd=0.8)
lines(x=c(median(mu_est),median(mu_est)), y=c(1, nrow(figure_temp_influence)), lty=3, col="red", lwd=1.6)
# dev.off()

## Arrow figure showing the ratio of biomass over k for the terminal years assuming no fishing or no environmental impact
figure_arrow = data.frame(depletion=c(CC_arrow_WI$P_terminal,CC_arrow_MN$P_terminal))
figure_arrow$depletion_no_env = c(CC_arrow_WI$P_no_env,CC_arrow_MN$P_no_env)
figure_arrow$depletion_no_fish = c(CC_arrow_WI$P_no_fishing,CC_arrow_MN$P_no_fishing)
figure_arrow = figure_arrow[order(figure_arrow$depletion),]
# jpeg("Output/Figure/ALL/N_arrow_CC.jpeg",width = 4, height=3, units = "in", res = 600)
plot(1:10, 1:10, type="n", bty="n", yaxt="n", cex.axis=1,
     xlim=c(0, 1), ylim=c(1,nrow(figure_arrow)),
     xlab="B/K in terminal year", ylab="", cex.main=1)
mu_est = figure_arrow$depletion
mu_min = figure_arrow$depletion_no_env
mu_max = figure_arrow$depletion_no_fish
sapply(1:nrow(figure_arrow), function(x) points(x=mu_est[x], y=c(1:nrow(figure_arrow))[x], col="black",pch=16))
sapply(1:nrow(figure_arrow), function(x) Arrows(mu_est[x], c(1:nrow(figure_arrow))[x], mu_min[x], c(1:nrow(figure_arrow))[x], col="red"))
sapply(1:nrow(figure_arrow), function(x) Arrows(mu_est[x], c(1:nrow(figure_arrow))[x], mu_max[x], c(1:nrow(figure_arrow))[x], col="blue"))
abline(v = mean(figure_arrow$depletion), col="black", lwd=3, lty=2)
# dev.off()

# jpeg("Output/Figure/ALL/N_hist_no_env_CC.jpeg",width = 4.2, height=2.75, units = "in", res = 600)
distance_env = -figure_arrow$depletion_no_env+figure_arrow$depletion
distance_fishing = -figure_arrow$depletion_no_fish+figure_arrow$depletion
hist(distance_env,main="",xlab="Net differences of B/k in terminal years\n caused by temperature change",col="red",xlim=c(min(distance_fishing,distance_env),max(distance_fishing,distance_env)))
# dev.off()

# jpeg("Output/Figure/ALL/N_hist_no_fishing_CC.jpeg",width = 4.2, height=2.75, units = "in", res = 600)
distance_env = -figure_arrow$depletion_no_env+figure_arrow$depletion
distance_fishing = -figure_arrow$depletion_no_fish+figure_arrow$depletion
hist(distance_fishing,main="",xlab="Net differences of B/k in terminal years\n caused by fishing",col="blue",xlim=c(min(distance_fishing,distance_env),max(distance_fishing,distance_env)))
# dev.off()

################# YP #####################

# read data WI
output_WI_YP = readRDS(file.path(outdir, "N_WI_YP.rda"))
YP_arrow_WI = read.csv(file.path(outdir, "N_WI_YP_quantities.csv"))

# remove results that R_hat is greater than 1.05 
remove_output = rep(0,length(output_WI_YP[,1,1]))
for (i in 1:length(output_WI_YP[,1,1])){
  if (max(output_WI_YP[i,,6])>1.05) {
    remove_output[i] = 1
  }
}
output_WI_YP = output_WI_YP[!remove_output,,]
YP_arrow_WI = YP_arrow_WI[!remove_output,]

# read data MN
output_MN_YP = readRDS(file.path(outdir, "N_MN_YP.rda"))
output_MN_YP = output_MN_YP[1:32,,]
YP_arrow_MN = read.csv(file.path(outdir, "N_MN_YP_quantities.csv"))
YP_arrow_MN = YP_arrow_MN[1:32,]

# remove results that R_hat is greater than 1.05 
remove_output = rep(0,length(output_MN_YP[,1,1]))
for (i in 1:length(output_MN_YP[,1,1])){
  if (max(output_MN_YP[i,,6])>1.05) {
    remove_output[i] = 1
  }
}
output_MN_YP = output_MN_YP[!remove_output,,]
YP_arrow_MN = YP_arrow_MN[!remove_output,]

# creat figure data frame
figure_temp_influence = data.frame(impact_E=c(output_WI_YP[,8,1],output_MN_YP[,8,1]),low=c(output_WI_YP[,8,4],output_MN_YP[,8,4]),high=c(output_WI_YP[,8,5],output_MN_YP[,8,5]))

# plot temperature influence
# jpeg("Output/Figure/ALL/N_Figure1_Temp_Influence_YP.jpeg",width=5,height=9,res=600,units = "in")
plot(1:10, 1:10, type="n", bty="n", yaxt="n", cex.axis=1,
     xlim=c(-1, 1), ylim=c(1,nrow(figure_temp_influence)),
     xlab="Impact_E", ylab="", cex.main=1)
figure_temp_influence = figure_temp_influence[order(figure_temp_influence$impact_E),]
mu_est = figure_temp_influence$impact_E/2
mu_min = figure_temp_influence$low/2
mu_max = figure_temp_influence$high/2
col_line = rep("black",nrow(figure_temp_influence))
for (i in 1:nrow(figure_temp_influence)){
  if (mu_min[i]>0){
    col_line[i] = "red"
  }
  if(mu_max[i]<0){
    col_line[i] = "blue"
  }
}
sapply(1:nrow(figure_temp_influence), function(x) lines(x=c(mu_min[x], mu_max[x]), y=c(x,x), lwd=0.6, col=col_line[x]))
sapply(1:nrow(figure_temp_influence), function(x) points(x=mu_est[x], y=c(1:nrow(figure_temp_influence))[x], col=col_line[x],pch=16))
lines(x=c(0,0), y=c(1, nrow(figure_temp_influence)), lty=3, col="black", lwd=0.8)
lines(x=c(median(mu_est),median(mu_est)), y=c(1, nrow(figure_temp_influence)), lty=3, col="red", lwd=1.6)
# dev.off()

## Arrow figure showing the ratio of biomass over k for the terminal years assuming no fishing or no environmental impact
figure_arrow = data.frame(depletion=c(YP_arrow_WI$P_terminal,YP_arrow_MN$P_terminal))
figure_arrow$depletion_no_env = c(YP_arrow_WI$P_no_env,YP_arrow_MN$P_no_env)
figure_arrow$depletion_no_fish = c(YP_arrow_WI$P_no_fishing,YP_arrow_MN$P_no_fishing)
figure_arrow = figure_arrow[order(figure_arrow$depletion),]
# jpeg("Output/Figure/ALL/N_arrow_YP.jpeg",width = 4, height=length(figure_arrow$depletion)/8, units = "in", res = 600)
plot(1:10, 1:10, type="n", bty="n", yaxt="n", cex.axis=1,
     xlim=c(0, 1), ylim=c(1,nrow(figure_arrow)),
     xlab="B/K in terminal year", ylab="", cex.main=1)
mu_est = figure_arrow$depletion
mu_min = figure_arrow$depletion_no_env
mu_max = figure_arrow$depletion_no_fish
sapply(1:nrow(figure_arrow), function(x) points(x=mu_est[x], y=c(1:nrow(figure_arrow))[x], col="black",pch=16))
sapply(1:nrow(figure_arrow), function(x) Arrows(mu_est[x], c(1:nrow(figure_arrow))[x], mu_min[x], c(1:nrow(figure_arrow))[x], col="red"))
sapply(1:nrow(figure_arrow), function(x) Arrows(mu_est[x], c(1:nrow(figure_arrow))[x], mu_max[x], c(1:nrow(figure_arrow))[x], col="blue"))
abline(v = mean(figure_arrow$depletion), col="black", lwd=3, lty=2)
# dev.off()

jpeg("Output/Figure/ALL/N_hist_no_env_YP.jpeg",width = 4.2, height=2.75, units = "in", res = 600)
distance_env = -figure_arrow$depletion_no_env+figure_arrow$depletion
distance_fishing = -figure_arrow$depletion_no_fish+figure_arrow$depletion
hist(distance_env,main="",xlab="Net differences of B/k in terminal years\n caused by temperature change",col="red",xlim=c(min(distance_fishing,distance_env),max(distance_fishing,distance_env)))
# dev.off()

# jpeg("Output/Figure/ALL/N_hist_no_fishing_YP.jpeg",width = 4.2, height=2.75, units = "in", res = 600)
distance_env = -figure_arrow$depletion_no_env+figure_arrow$depletion
distance_fishing = -figure_arrow$depletion_no_fish+figure_arrow$depletion
hist(distance_fishing,main="",xlab="Net differences of B/k in terminal years\n caused by fishing",col="blue",xlim=c(min(distance_fishing,distance_env),max(distance_fishing,distance_env)))
# dev.off()

################# BG #####################

# read data WI
output_WI_BG = readRDS(file.path(outdir, "N_WI_BG.rda"))
BG_arrow_WI = read.csv(file.path(outdir, "N_WI_BG_quantities.csv"))

# remove results that R_hat is greater than 1.05 
remove_output = rep(0,length(output_WI_BG[,1,1]))
for (i in 1:length(output_WI_BG[,1,1])){
  if (max(output_WI_BG[i,,6])>1.05) {
    remove_output[i] = 1
  }
}
output_WI_BG = output_WI_BG[!remove_output,,]
BG_arrow_WI = BG_arrow_WI[!remove_output,]

# read data MN
output_MN_BG = readRDS(file.path(outdir, "N_MN_BG.rda"))
output_MN_BG = output_MN_BG[1:32,,]
BG_arrow_MN = read.csv(file.path(outdir, "N_MN_BG_quantities.csv"))
BG_arrow_MN = BG_arrow_MN[1:32,]

# remove results that R_hat is greater than 1.05 
remove_output = rep(0,length(output_MN_BG[,1,1]))
for (i in 1:length(output_MN_BG[,1,1])){
  if (max(output_MN_BG[i,,6])>1.05) {
    remove_output[i] = 1
  }
}
output_MN_BG = output_MN_BG[!remove_output,,]
BG_arrow_MN = BG_arrow_MN[!remove_output,]

# creat figure data frame
figure_temp_influence = data.frame(impact_E=c(output_WI_BG[,8,1],output_MN_BG[,8,1]),low=c(output_WI_BG[,8,4],output_MN_BG[,8,4]),high=c(output_WI_BG[,8,5],output_MN_BG[,8,5]))

# plot temperature influence
# jpeg("Output/Figure/ALL/N_Figure1_Temp_Influence_BG.jpeg",width=5,height=9,res=600,units = "in")
plot(1:10, 1:10, type="n", bty="n", yaxt="n", cex.axis=1,
     xlim=c(-1, 1), ylim=c(1,nrow(figure_temp_influence)),
     xlab="Impact_E", ylab="", cex.main=1)
figure_temp_influence = figure_temp_influence[order(figure_temp_influence$impact_E),]
mu_est = figure_temp_influence$impact_E/2
mu_min = figure_temp_influence$low/2
mu_max = figure_temp_influence$high/2
col_line = rep("black",nrow(figure_temp_influence))
for (i in 1:nrow(figure_temp_influence)){
  if (mu_min[i]>0){
    col_line[i] = "red"
  }
  if(mu_max[i]<0){
    col_line[i] = "blue"
  }
}
sapply(1:nrow(figure_temp_influence), function(x) lines(x=c(mu_min[x], mu_max[x]), y=c(x,x), lwd=0.6, col=col_line[x]))
sapply(1:nrow(figure_temp_influence), function(x) points(x=mu_est[x], y=c(1:nrow(figure_temp_influence))[x], col=col_line[x],pch=16))
lines(x=c(0,0), y=c(1, nrow(figure_temp_influence)), lty=3, col="black", lwd=0.8)
lines(x=c(median(mu_est),median(mu_est)), y=c(1, nrow(figure_temp_influence)), lty=3, col="red", lwd=1.6)
# dev.off()

## Arrow figure showing the ratio of biomass over k for the terminal years assuming no fishing or no environmental impact
figure_arrow = data.frame(depletion=c(BG_arrow_WI$P_terminal,BG_arrow_MN$P_terminal))
figure_arrow$depletion_no_env = c(BG_arrow_WI$P_no_env,BG_arrow_MN$P_no_env)
figure_arrow$depletion_no_fish = c(BG_arrow_WI$P_no_fishing,BG_arrow_MN$P_no_fishing)
figure_arrow = figure_arrow[order(figure_arrow$depletion),]
# jpeg("Output/Figure/ALL/N_arrow_BG.jpeg",width = 4, height=length(figure_arrow$depletion)/8, units = "in", res = 600)
plot(1:10, 1:10, type="n", bty="n", yaxt="n", cex.axis=1,
     xlim=c(0, 1), ylim=c(1,nrow(figure_arrow)),
     xlab="B/K in terminal year", ylab="", cex.main=1)
mu_est = figure_arrow$depletion
mu_min = figure_arrow$depletion_no_env
mu_max = figure_arrow$depletion_no_fish
sapply(1:nrow(figure_arrow), function(x) points(x=mu_est[x], y=c(1:nrow(figure_arrow))[x], col="black",pch=16))
sapply(1:nrow(figure_arrow), function(x) Arrows(mu_est[x], c(1:nrow(figure_arrow))[x], mu_min[x], c(1:nrow(figure_arrow))[x], col="red"))
sapply(1:nrow(figure_arrow), function(x) Arrows(mu_est[x], c(1:nrow(figure_arrow))[x], mu_max[x], c(1:nrow(figure_arrow))[x], col="blue"))
abline(v = mean(figure_arrow$depletion), col="black", lwd=3, lty=2)
# dev.off()

# jpeg("Output/Figure/ALL/N_hist_no_env_BG.jpeg",width = 4.2, height=2.75, units = "in", res = 600)
distance_env = -figure_arrow$depletion_no_env+figure_arrow$depletion
distance_fishing = -figure_arrow$depletion_no_fish+figure_arrow$depletion
hist(distance_env,main="",xlab="Net differences of B/k in terminal years\n caused by temperature change",col="red",xlim=c(min(distance_fishing,distance_env),max(distance_fishing,distance_env)))
# dev.off()

# jpeg("Output/Figure/ALL/N_hist_no_fishing_BG.jpeg",width = 4.2, height=2.75, units = "in", res = 600)
distance_env = -figure_arrow$depletion_no_env+figure_arrow$depletion
distance_fishing = -figure_arrow$depletion_no_fish+figure_arrow$depletion
hist(distance_fishing,main="",xlab="Net differences of B/k in terminal years\n caused by fishing",col="blue",xlim=c(min(distance_fishing,distance_env),max(distance_fishing,distance_env)))
#dev.off()

################# WE #####################

# read data WI
output_WI_WE = readRDS(file.path(outdir, "N_WI_WE.rda"))
WE_arrow_WI = read.csv(file.path(outdir, "N_WI_WE_quantities.csv"))

# remove results that R_hat is greater than 1.05 
remove_output = rep(0,length(output_WI_WE[,1,1]))
for (i in 1:length(output_WI_WE[,1,1])){
  if (max(output_WI_WE[i,,6])>1.05) {
    remove_output[i] = 1
  }
}
output_WI_WE = output_WI_WE[!remove_output,,]
WE_arrow_WI = WE_arrow_WI[!remove_output,]

# read data MN
output_MN_WE = readRDS(file.path(outdir, "N_MN_WE.rda"))
WE_arrow_MN = read.csv(file.path(outdir, "N_MN_WE_quantities.csv"))

# remove results that R_hat is greater than 1.05 
remove_output = rep(0,length(output_MN_WE[,1,1]))
for (i in 1:length(output_MN_WE[,1,1])){
  if (max(output_MN_WE[i,,6])>1.05) {
    remove_output[i] = 1
  }
}
output_MN_WE = output_MN_WE[!remove_output,,]
WE_arrow_MN = WE_arrow_MN[!remove_output,]

# creat figure data frame
figure_temp_influence = data.frame(impact_E=c(output_WI_WE[,8,1],output_MN_WE[,8,1]),low=c(output_WI_WE[,8,4],output_MN_WE[,8,4]),high=c(output_WI_WE[,8,5],output_MN_WE[,8,5]))

# plot temperature influence
# jpeg("Output/Figure/ALL/N_Figure1_Temp_Influence_WE.jpeg",width=5,height=9,res=600,units = "in")
plot(1:10, 1:10, type="n", bty="n", yaxt="n", cex.axis=1,
     xlim=c(-1, 1), ylim=c(1,nrow(figure_temp_influence)),
     xlab="Impact_E", ylab="", cex.main=1)
figure_temp_influence = figure_temp_influence[order(figure_temp_influence$impact_E),]
mu_est = figure_temp_influence$impact_E/2
mu_min = figure_temp_influence$low/2
mu_max = figure_temp_influence$high/2
col_line = rep("black",nrow(figure_temp_influence))
for (i in 1:nrow(figure_temp_influence)){
  if (mu_min[i]>0){
    col_line[i] = "red"
  }
  if(mu_max[i]<0){
    col_line[i] = "blue"
  }
}
sapply(1:nrow(figure_temp_influence), function(x) lines(x=c(mu_min[x], mu_max[x]), y=c(x,x), lwd=0.6, col=col_line[x]))
sapply(1:nrow(figure_temp_influence), function(x) points(x=mu_est[x], y=c(1:nrow(figure_temp_influence))[x], col=col_line[x],pch=16))
lines(x=c(0,0), y=c(1, nrow(figure_temp_influence)), lty=3, col="black", lwd=0.8)
lines(x=c(median(mu_est),median(mu_est)), y=c(1, nrow(figure_temp_influence)), lty=3, col="red", lwd=1.6)
# dev.off()

## Arrow figure showing the ratio of biomass over k for the terminal years assuming no fishing or no environmental impact
figure_arrow = data.frame(depletion=c(WE_arrow_WI$P_terminal,WE_arrow_MN$P_terminal))
figure_arrow$depletion_no_env = c(WE_arrow_WI$P_no_env,WE_arrow_MN$P_no_env)
figure_arrow$depletion_no_fish = c(WE_arrow_WI$P_no_fishing,WE_arrow_MN$P_no_fishing)
figure_arrow = figure_arrow[order(figure_arrow$depletion),]
# jpeg("Output/Figure/ALL/N_arrow_WE.jpeg",width = 4, height=length(figure_arrow$depletion)/8, units = "in", res = 600)
plot(1:10, 1:10, type="n", bty="n", yaxt="n", cex.axis=1,
     xlim=c(0, 1), ylim=c(1,nrow(figure_arrow)),
     xlab="B/K in terminal year", ylab="", cex.main=1)
mu_est = figure_arrow$depletion
mu_min = figure_arrow$depletion_no_env
mu_max = figure_arrow$depletion_no_fish
sapply(1:nrow(figure_arrow), function(x) points(x=mu_est[x], y=c(1:nrow(figure_arrow))[x], col="black",pch=16))
sapply(1:nrow(figure_arrow), function(x) Arrows(mu_est[x], c(1:nrow(figure_arrow))[x], mu_min[x], c(1:nrow(figure_arrow))[x], col="red"))
sapply(1:nrow(figure_arrow), function(x) Arrows(mu_est[x], c(1:nrow(figure_arrow))[x], mu_max[x], c(1:nrow(figure_arrow))[x], col="blue"))
abline(v = mean(figure_arrow$depletion), col="black", lwd=3, lty=2)
# dev.off()

#jpeg("Output/Figure/ALL/N_hist_no_env_WE.jpeg",width = 4.2, height=2.75, units = "in", res = 600)
distance_env = -figure_arrow$depletion_no_env+figure_arrow$depletion
distance_fishing = -figure_arrow$depletion_no_fish+figure_arrow$depletion
hist(distance_env,main="",xlab="Net differences of B/k in terminal years\n caused by temperature change",col="red",xlim=c(min(distance_fishing,distance_env),max(distance_fishing,distance_env)))
#dev.off()

#jpeg("Output/Figure/ALL/N_hist_no_fishing_WE.jpeg",width = 4.2, height=2.75, units = "in", res = 600)
distance_env = -figure_arrow$depletion_no_env+figure_arrow$depletion
distance_fishing = -figure_arrow$depletion_no_fish+figure_arrow$depletion
hist(distance_fishing,main="",xlab="Net differences of B/k in terminal years\n caused by fishing",col="blue",xlim=c(min(distance_fishing,distance_env),max(distance_fishing,distance_env)))
#dev.off()

###### figure 1 impact explained by species ######
N_LB = length(c(output_WI_LB[,8,1],output_MN_LB[,8,1]))
N_NP = length(c(output_WI_NP[,8,1],output_MN_NP[,8,1]))
N_BC = length(c(output_WI_BC[,8,1],output_MN_BC[,8,1]))
N_SB = length(c(output_WI_SB[,8,1],output_MN_SB[,8,1]))
N_CC = length(c(output_WI_CC[,8,1],output_MN_CC[,8,1]))
N_YP = length(c(output_WI_YP[,8,1],output_MN_YP[,8,1]))
N_BG = length(c(output_WI_BG[,8,1],output_MN_BG[,8,1]))
N_WE = length(c(output_WI_WE[,8,1],output_MN_WE[,8,1]))

impact_explain = data.frame(impact_mean = c(output_WI_LB[,8,1],output_MN_LB[,8,1],output_WI_NP[,8,1],output_MN_NP[,8,1],output_WI_BC[,8,1],output_MN_BC[,8,1],output_WI_SB[,8,1],output_MN_SB[,8,1],output_WI_CC[,8,1],output_MN_CC[,8,1],output_WI_YP[,8,1],output_MN_YP[,8,1],output_WI_BG[,8,1],output_MN_BG[,8,1],output_WI_WE[,8,1],output_MN_WE[,8,1]))

impact_explain$species = c(rep("Largemouth Bass",N_LB),rep("Northern pike",N_NP),rep("Black crappie",N_BC),rep("Smallmouth Bass",N_SB),rep("Cisco",N_CC),rep("Yellow perch",N_YP),rep("Bluegill",N_BG),rep("Walleye",N_WE))
                           
# remove the NAs
#impact_explain = impact_explain[complete.cases(impact_explain$impact_mean),]

#jpeg("Output/Figure/ALL/N_Warming_Influence_species.jpeg",width=5,height=4.5,res=600,units = "in")
ggplot(impact_explain, aes(x=factor(species,levels = c("Cisco","Yellow perch","Walleye","Smallmouth Bass","Northern pike","Bluegill","Black crappie","Largemouth Bass")), y=impact_mean)) + 
  geom_boxplot(notch=T)+
  theme_bw()+
  ylab("Impact of warming")+
  xlab("Species")+
  geom_hline(yintercept = 0, linetype="dashed",color = "red", size=1)+
  theme(axis.text.x = element_text(angle=90,face="bold"),axis.text.y = element_text(face="bold"))+
  ylim(-2,2)
# dev.off()

# impact VS p_initial
impact_explain$p_initial = c(output_WI_LB[,4,1],output_MN_LB[,4,1],output_WI_NP[,4,1],output_MN_NP[,4,1],output_WI_BC[,4,1],output_MN_BC[,4,1],output_WI_SB[,4,1],output_MN_SB[,4,1],output_WI_CC[,4,1],output_MN_CC[,4,1],output_WI_YP[,4,1],output_MN_YP[,4,1],output_WI_BG[,4,1],output_MN_BG[,4,1],output_WI_WE[,4,1],output_MN_WE[,4,1])

cor.test(impact_explain$p_initial,abs(impact_explain$impact_mean))

# jpeg("Output/Figure/ALL/N_Warming_Influence_p_initial_ABS.jpeg",width=5,height=4,res=600,units = "in")
ggplot(impact_explain, aes(x=p_initial, y=abs(impact_mean))) + 
  geom_point( color="#69b3a2") +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE)+
  theme_bw()+
  ylab("Absolute value of warming effect")+
  xlab("Relative biomass of the initial year")+
  theme(axis.text.x = element_text(face="bold"),axis.text.y = element_text(face="bold"))
#dev.off()
