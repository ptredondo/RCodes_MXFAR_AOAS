##### Setting working directory
setwd("~/working/directory")

##### Installing required packages
# devtools::install_github("ptredondo/mxfar")
# install.packages("ggplot2","eegkit","latex2exp","ggpubr","grid","vars","grangers")

##### Loading needed packages
library(mxfar)
library(ggplot2)
library(eegkit)
library(latex2exp)
library(ggpubr)
library(grid)
library(vars)
library(grangers)

##### User-defined function for visualization
myelectrodes <- c("FP1","FP2","T7","T8","O1","O2")
data(eegcoord)
myEEG <- eegcoord[(1:(length(rownames(eegcoord))))[rownames(eegcoord) %in% myelectrodes],]

add_arrows <- function(connect_node,lwd = 5,arr.length = 1,spc = 1.75,adj = 0.5,cut_off = c(0.5,0.5,0.75)){
  for(i in 1:5){
    for(j in (i+1):6){
      if(i == j){next}
      mycol <- rep("black",2)
      if(connect_node[i,j] >= cut_off[3]){
        mycol[1] <- "black"
      } else if(connect_node[i,j] >= cut_off[2]){
        mycol[1] <- "darkred"
      } else if(connect_node[i,j] >= cut_off[1]){
        mycol[1] <- "darkorange"
      }
      if(connect_node[j,i] >= cut_off[3]){
        mycol[2] <- "black"
      } else if(connect_node[j,i] >= cut_off[2]){
        mycol[2] <- "darkred"
      } else if(connect_node[j,i] > cut_off[1]){
        mycol[2] <- "darkorange"
      }
      
      sgn_flip1 <- 1
      sgn_flip2 <- 1
      sgn_flip3 <- 1
      
      if((i == 1 & j == 5) | (i == 2 & j == 6) | (i == 3 & j == 5) | (i == 4 & j == 6)){
        sgn_flip1 <- -1
      }
      
      if((i == 3 & j == 5) | (i == 3 & j == 6) | (i == 4 & j == 5) | (i == 4 & j == 6)){
        sgn_flip2 <- -1
      }
      
      if((i == 1 & j == 6) | (i == 1 & j == 4) | (i == 2 & j == 6) | (i == 3 & j == 5) | (i == 4 & j == 5)){
        sgn_flip3 <- -1
      }
      
      ang <- abs(atan((myEEG$yproj[j]-myEEG$yproj[i])/(myEEG$xproj[j]-myEEG$xproj[i])))
      
      if(connect_node[i,j] >= cut_off[1] & connect_node[j,i] >= cut_off[1]){
        
        if(connect_node[i,j] >= connect_node[j,i]){
          
          arrows(x0 = sign(myEEG$xproj[i])*(abs(myEEG$xproj[i])-sgn_flip1*spc*cos(ang))+sgn_flip3*adj*sin(ang),
                 y0 = sign(myEEG$yproj[i])*(abs(myEEG$yproj[i])-spc*sin(ang))-adj*cos(ang),
                 x1 = sign(myEEG$xproj[j])*(abs(myEEG$xproj[j])-spc*cos(ang))+sgn_flip3*adj*sin(ang),
                 y1 = sign(myEEG$yproj[j])*(abs(myEEG$yproj[j])-sgn_flip2*spc*sin(ang))-adj*cos(ang),
                 length = arr.length,lwd = lwd,code = 2,col = mycol[2])
          
          arrows(x0 = sign(myEEG$xproj[i])*(abs(myEEG$xproj[i])-sgn_flip1*spc*cos(ang))-sgn_flip3*adj*sin(ang),
                 y0 = sign(myEEG$yproj[i])*(abs(myEEG$yproj[i])-spc*sin(ang))+adj*cos(ang),
                 x1 = sign(myEEG$xproj[j])*(abs(myEEG$xproj[j])-spc*cos(ang))-sgn_flip3*adj*sin(ang),
                 y1 = sign(myEEG$yproj[j])*(abs(myEEG$yproj[j])-sgn_flip2*spc*sin(ang))+adj*cos(ang),
                 length = arr.length,lwd = lwd,code = 1,col = mycol[1])
          
        } else {
          
          arrows(x0 = sign(myEEG$xproj[i])*(abs(myEEG$xproj[i])-sgn_flip1*spc*cos(ang))-sgn_flip3*adj*sin(ang),
                 y0 = sign(myEEG$yproj[i])*(abs(myEEG$yproj[i])-spc*sin(ang))+adj*cos(ang),
                 x1 = sign(myEEG$xproj[j])*(abs(myEEG$xproj[j])-spc*cos(ang))-sgn_flip3*adj*sin(ang),
                 y1 = sign(myEEG$yproj[j])*(abs(myEEG$yproj[j])-sgn_flip2*spc*sin(ang))+adj*cos(ang),
                 length = arr.length,lwd = lwd,code = 1,col = mycol[1])
          
          arrows(x0 = sign(myEEG$xproj[i])*(abs(myEEG$xproj[i])-sgn_flip1*spc*cos(ang))+sgn_flip3*adj*sin(ang),
                 y0 = sign(myEEG$yproj[i])*(abs(myEEG$yproj[i])-spc*sin(ang))-adj*cos(ang),
                 x1 = sign(myEEG$xproj[j])*(abs(myEEG$xproj[j])-spc*cos(ang))+sgn_flip3*adj*sin(ang),
                 y1 = sign(myEEG$yproj[j])*(abs(myEEG$yproj[j])-sgn_flip2*spc*sin(ang))-adj*cos(ang),
                 length = arr.length,lwd = lwd,code = 2,col = mycol[2])
          
        }
        
      } else {
        
        if(connect_node[i,j] >= cut_off[1] & connect_node[j,i] < cut_off[1]){
          
          arrows(x0 = sign(myEEG$xproj[i])*(abs(myEEG$xproj[i])-sgn_flip1*spc*cos(ang)),
                 y0 = sign(myEEG$yproj[i])*(abs(myEEG$yproj[i])-spc*sin(ang)),
                 x1 = sign(myEEG$xproj[j])*(abs(myEEG$xproj[j])-spc*cos(ang)),
                 y1 = sign(myEEG$yproj[j])*(abs(myEEG$yproj[j])-sgn_flip2*spc*sin(ang)),
                 length = arr.length,lwd = lwd,code = 1,col = mycol[1])
          
        }
        
        if(connect_node[i,j] < cut_off[1] & connect_node[j,i] >= cut_off[1]){
          
          arrows(x0 = sign(myEEG$xproj[i])*(abs(myEEG$xproj[i])-sgn_flip1*spc*cos(ang)),
                 y0 = sign(myEEG$yproj[i])*(abs(myEEG$yproj[i])-spc*sin(ang)),
                 x1 = sign(myEEG$xproj[j])*(abs(myEEG$xproj[j])-spc*cos(ang)),
                 y1 = sign(myEEG$yproj[j])*(abs(myEEG$yproj[j])-sgn_flip2*spc*sin(ang)),
                 length = arr.length,lwd = lwd,code = 2,col = mycol[2])
        }
        
      }
      
    }
  }
  
}

##### Revised codes from "grangers" package due to error
gran.cond <- function (x, y, z, ic.chosen = "SC", max.lag = min(4, length(x) - 1),
                       plot = F, type.chosen = "none", p1 = 0, p2 = 0) {
  
  dd_1 <- cbind(x, z)
  dd_2 <- cbind(x, y, z)
  colnames(dd_1) <- NULL
  colnames(dd_2) <- NULL
  if (p1 == 0) {
    model1 = VAR(dd_1, ic = ic.chosen, lag.max = max.lag, 
                 type.chosen)
  }
  if (p1 > 0) {
    model1 = VAR(dd_1, p = p1, type.chosen)
  }
  freq.good = spec.pgram(y, plot = F)$freq/frequency(x)
  astr_too1 <- vector("numeric", length(freq.good))
  astr_too2 <- vector("numeric", length(freq.good))
  astr_too3 <- vector("numeric", length(freq.good))
  astr_too <- vector("numeric", length(freq.good))
  boh_too <- vector("numeric", length(freq.good))
  coef_model1 = array(0, dim = c(model1$K, model1$K, model1$p))
  add <- array(0, dim = c(model1$K, model1$K, model1$p, length(freq.good)))
  ADD = array(0, dim = c(model1$K, model1$K, length(freq.good)))
  ADD_x1 = array(0, dim = c(model1$K, model1$K, length(freq.good)))
  H1_1 = array(0, dim = c(model1$K, model1$K, length(freq.good)))
  if (p2 == 0) {
    model2 = VAR(dd_2, ic = ic.chosen, lag.max = max.lag, 
                 type.chosen)
  }
  if (p2 > 0) {
    model2 = VAR(dd_2, p = p2, type.chosen)
  }
  coef_model2 = array(0, dim = c(model2$K, model2$K, model2$p))
  add_2 <- array(0, dim = c(model2$K, model2$K, model2$p, length(freq.good)))
  ADD_2 = array(0, dim = c(model2$K, model2$K, length(freq.good)))
  ADD_x2 = array(0, dim = c(model2$K, model2$K, length(freq.good)))
  H1_2 = array(0, dim = c(model2$K, model2$K, length(freq.good)))
  G_2 <- array(0, dim = c(model2$K, model2$K, length(freq.good)))
  Q1 <- array(0, dim = c(model2$K, model2$K, length(freq.good)))
  
  for(p0 in 1:model1$K){
    for (p in 1:model1$K) {
      for (k in 1:model1$p) {
        coef_model1[p0, p, k] = coef(model1)[[p0]][p + (k - 1) * 
                                                     model1$K, 1]
      }
    }
  }
  
  for(p0 in 1:model2$K){
    for (p in 1:model2$K) {
      for (k in 1:model2$p) {
        coef_model2[p0, p, k] = coef(model2)[[p0]][p + (k - 1) * 
                                                     model2$K, 1]
      }
    }
  }
  
  for (l in 1:length(freq.good)) {
    for (k in 1:model1$p) {
      add[, , k, l] <- coef_model1[, , k] * exp(-2 * pi * 
                                                  k * (0+1i) * freq.good[l])
    }
  }
  for (l in 1:length(freq.good)) {
    for (k in 1:model1$p) {
      ADD[, , l] = ADD[, , l] + add[, , k, l]
    }
  }
  Sigma_model1 <- summary(model1)$cov
  P_model1 <- diag(rep(1,model1$K))
  P_model1[-1,1] <- -Sigma_model1[1, -1]/Sigma_model1[1,1]
  
  for (l in 1:length(freq.good)) {
    ADD_x1[, , l] = (P_model1) %*% (-ADD[, , l] + diag(rep(1,model1$K)))
  }
  for (l in 1:length(freq.good)) {
    H1_1[, , l] = solve(ADD_x1[, , l])
  }
  
  Sigma_model2 <- summary(model2)$cov
  P1_model2 <- diag(rep(1,model2$K))
  P1_model2[2,1] <- -Sigma_model2[2, 1]/Sigma_model2[1,1]
  P1_model2[-c(1,2),1] <- -Sigma_model2[-c(1,2), 1]/Sigma_model2[1,1]
  
  P2_model2 <- diag(rep(1,model2$K))
  P2_model2[-c(1,2),2] <- -(Sigma_model2[-c(1,2),2] - Sigma_model2[-c(1,2), 1]^2/Sigma_model2[1, 1])/(Sigma_model2[2, 2] - Sigma_model2[2,1]^2/Sigma_model2[1, 1])  
  
  P_model2 <- P1_model2 %*% P2_model2
  
  for (l in 1:length(freq.good)) {
    for (k in 1:model2$p) {
      add_2[, , k, l] <- coef_model2[, , k] * exp(-2 * 
                                                    pi * k * (0+1i) * freq.good[l])
    }
  }
  for (l in 1:length(freq.good)) {
    for (k in 1:model2$p) {
      ADD_2[, , l] = ADD_2[, , l] + add_2[, , k, l]
    }
  }
  for (l in 1:length(freq.good)) {
    ADD_x2[, , l] = (P_model2) %*% (-ADD_2[, , l] + diag(rep(1,model2$K)))
  }
  for (l in 1:length(freq.good)) {
    H1_2[, , l] = solve(ADD_x2[, , l])
  }
  
  for (l in 1:length(freq.good)) {
    G_2[1, 1, l] <- H1_1[1, 1, l]
    G_2[1, -c(1,2), l] <- H1_1[1, -1, l]
    G_2[-c(1,2), 1, l] <- H1_1[-1, 1, l]
    G_2[-c(1,2), -c(1,2), l] <- H1_1[-1, -1, l]
    G_2[2, 2, l] <- 1
  }
  for (l in 1:length(freq.good)) {
    Q1[, , l] <- solve(G_2[, , l]) %*% H1_2[, , l]
  }
  for (l in 1:length(freq.good)) {
    astr_too1[l] <- Q1[1, 1, l] * Sigma_model2[1, 1] * Conj(Q1[1, 1, l])
    astr_too2[l] <- Q1[1, 2, l] * Sigma_model2[2, 2] * Conj(Q1[1, 2, l])
    if(length(Q1[1, -c(1,2), l])>1){
      astr_too3[l] <- (matrix(Q1[1, -c(1,2), l],nrow=1) %*% Sigma_model2[-c(1,2), -c(1,2)] %*% t(Conj(matrix(Q1[1, -c(1,2), l],nrow=1))))[1,1]
    } else {
      astr_too3[l] <- Q1[1, -c(1,2), l] * Sigma_model2[-c(1,2), -c(1,2)] * Conj(Q1[1, -c(1,2), l])
    }
    astr_too[l] <- astr_too1[l] + astr_too2[l] + astr_too3[l]
    boh_too[l] <- log(abs(astr_too[l])/abs(astr_too1[l]))
  }
  Granger_cond_y.to.x.by.z <- boh_too
  GG <- list(freq.good, length(x), Granger_cond_y.to.x.by.z)
  names(GG) <- c("frequency", "n", "Conditional_causality_y.to.x.on.z")
  if (plot == F) {
    return(GG)
  }
  if (plot == T) {
    par(mfrow = c(1, 1))
    plot(freq.good, Granger_cond_y.to.x.by.z, type = "l", 
         main = "Conditional Granger-causality y to x on z")
  }
}

##### Plot settings
col.pal1<-colorRampPalette(c("white","orange"))
col.pal2<-colorRampPalette(c("orange","darkred"))
theme_setting <- theme(axis.text=element_text(size=14),
                       axis.title=element_text(size=16),
                       axis.text.x = element_text(margin = margin(t = -12)),
                       axis.text.y = element_text(margin = margin(r = -14)),
                       plot.title = element_text(face = "bold",size = 20,hjust = 0.5,
                                                 margin = margin(t=20)),
                       legend.title = element_text(face = "bold",size = 16),
                       legend.text=element_text(size=18),
                       legend.key.size = unit(1.25, 'cm'),
                       plot.margin=grid::unit(c(0,0,0,0), "mm"))
theme_setting2 <- theme(axis.text=element_text(size=14),
                        axis.title=element_text(size=16),
                        legend.text=element_text(size=16),
                        legend.key.size = unit(1.75, 'cm'),
                        legend.position="bottom",
                        plot.margin=grid::unit(c(0,3,0,0), "mm"))


##### Importing visual-task EEG data
### Data can be downloaded from github (ptredondo/RCodes_MXFAR_AOAS)
visualeeg1 <- readRDS("visualeeg1.rds") 
visualeeg2 <- readRDS("visualeeg2.rds")
visualeeg3 <- readRDS("visualeeg3.rds")
visualeeg4 <- readRDS("visualeeg4.rds")
visualeeg <- rbind(visualeeg1,visualeeg2,visualeeg3,visualeeg4)[c(1:4,11,12,15,16,20)] # selecting channels
head(visualeeg) # show data
ids <- unique(visualeeg$id) # all subject ids

##### Plotting some EEG series
par(mfrow = c(5,1),mar = c(3,5,1,1))
plot.ts(visualeeg$Fp2[1:640],ylab = "Fp2")
plot.ts(visualeeg$Cz[1:640],ylab = "Cz")
plot.ts(visualeeg$T8[1:640],ylab = "T8")
plot.ts(visualeeg$O1[1:640],ylab = "O1")
plot.ts(visualeeg$O2[1:640],ylab = "O2")

#######################################################
##### EEG Analysis using the MXFAR-fPDC Framework #####
#######################################################

##### Model selection
max_p <- 5
max_d <- 10
ape_all <- NULL
system.time({
for(tw in 1:17){
  sub <- NULL
  for(i in 1:104){
    sub <- rbind(sub,subset(visualeeg,id == ids[i])[2.5*(tw-1)*128 + 1:640,-c(1,2)])
  }
  for(ref in 1:7){
    for(l in 1:max_p){
      for(j in (l+1):max_d){
        try({
          entry <- data.frame(tw=tw,p=l,d=j,ref=ref,
                              APE=APE(SeriesNum = c(51,53),Tlength = 640,y = as.matrix(sub[,1:6]),
                                      u = abs(sub[,ref]),p = l,d = j,bwp = 0.2,r = 50,Q = 4,numpoints = 50))
          cat(paste("DONE: tw=",tw," p=",l," d=",j," ref=",ref,sep = "")," \r")
          ape_all <- rbind(ape_all,entry)
        },silent = TRUE)
        
      }
    }
  }
}
})
# saveRDS(ape_all,"ape_all.rds") # saving results

##### System.time
#     user    system   elapsed 
# 269507.83  57585.86  42206.02 

### Aggregating APEs for all 17 time windows 
# ape_all <- readRDS("ape_all.rds") # loading results
ape_agg <- aggregate(APE ~ p + d + ref,data = ape_all, FUN = mean,na.rm = TRUE)
ape_sort <- ape_agg[order(ape_agg$APE),]
head(ape_sort) # the best model is given by p = 4, d = 6, ref = 7 (channel Cz)

##### Testing for non-linearity
nltests <- vector("list",17) # compiling results for 17 5-second sliding windows
system.time({
for(tw in 1:17){
  sub <- NULL
  for(i in 1:104){
    sub <- rbind(sub,subset(visualeeg,id == ids[i])[2.5*(tw-1)*128 + 1:640,-c(1,2)])
  }
  nltests[[tw]] <- NLTest(SeriesNum = c(51,53),Tlength = 640,y = as.matrix(sub[,1:6]),
                       u = abs(sub[,7]),p = 4,d = 6,bwp = 0.2,numpoints = 50,maxboot = 200)
  cat(paste("DONE: tw=",tw,sep = "")," \r")
}
})
# saveRDS(nltests,"nltests.rds") # saving results

# nltests <- readRDS("nltests.rds") # loading results
nltest_pval <- rep(NA,17)
for(tw in 1:17){
  nltest_pval[tw] <- nltests[[tw]]$pval
}
nltest_pval # < 0.05 (there is non-linearity present in the EEG signals)


##### Estimation of MXFAR model and the fPDC metric
result <- vector("list",17) # compiling results for 17 5-second sliding windows
ref <- 7 # channel Cz as reference signal

for(tw in 1:17){
  sub <- NULL
  for(i in 1:104){
    sub <- rbind(sub,subset(visualeeg,id == ids[i])[2.5*(tw-1)*128 + 1:640,-c(1,2)])
  }
  
  result[[tw]] <- MXFAR.est(SeriesNum = c(51,53),Tlength = 640,y = as.matrix(sub[,1:6]),
                            u = abs(sub[,7]),p = 4,d = 6,bwp = 0.2,numpoints = 50,fpdc = TRUE)
  cat(paste("DONE: tw=",tw,sep = "")," \r")
}
# saveRDS(result,"result_mxfarfpdc.rds") # saving results

##### System.time
#    user   system  elapsed 
# 6687.869 1899.790 2105.196   

##### Aggregating results into the four regions of interests
# result <- readRDS("result_mxfarfpdc.rds") # loading results
low.u <- 1:26 # reference signal (u<1)
high.u <- 27:51 # reference signal (u>1)
dta.w <- 1:60 # frequency delta-theta-alpha
bg.w <- 61:225 # beta-gamma

qt.level <- 0.80 # quantile level
perc.sig <- 0.10 # significance threshold

### Compiling all fPDCs for all 17 time windows
fpdc_all <- array(NA,dim = c(6,6,320,51,2,17))
for(tw in 1:17){
  fpdc_all[,,,,,tw] <- result[[tw]]$fpdc$fpdc_mean
}
for(ch in 1:6){
  fpdc_all[ch,ch,,,,] <- NA
}

### Defining prominent connections
fpdc_qt <- apply(fpdc_all,MARGIN = c(3,4,5,6),FUN = stats::quantile,probs = qt.level,na.rm = TRUE)
fpdc_prom <- array(NA,dim = dim(fpdc_all))
for(i in 1:6){
  for(j in 1:6){
    if(i == j){next}
    fpdc_prom[i,j,,,,] <- fpdc_all[i,j,,,,] > fpdc_qt
  }
}

### Defining consitent connections
conn_tw <- array(NA,c(6,6,2,2,2,17)) # ch1,ch2,freq,u,group,tw
conn_tw[,,1,1,,] <- apply(fpdc_prom[,,dta.w,low.u,,],MARGIN = c(1,2,5,6),
                          FUN = sum,na.rm = TRUE) >= length(dta.w)*length(low.u)*perc.sig
conn_tw[,,1,2,,] <- apply(fpdc_prom[,,dta.w,high.u,,],MARGIN = c(1,2,5,6),FUN = sum,na.rm = TRUE) >= length(dta.w)*length(high.u)*perc.sig
conn_tw[,,2,1,,] <- apply(fpdc_prom[,,bg.w,low.u,,],MARGIN = c(1,2,5,6),FUN = sum,na.rm = TRUE) >= length(bg.w)*length(low.u)*perc.sig
conn_tw[,,2,2,,] <- apply(fpdc_prom[,,bg.w,high.u,,],MARGIN = c(1,2,5,6),FUN = sum,na.rm = TRUE) >= length(bg.w)*length(high.u)*perc.sig
conn_agg <- apply(conn_tw,MARGIN = c(1,2,3,4,5),FUN = mean,na.rm = TRUE)

### Delta-Theta-Alpha Connectivity
par(mfrow = c(1,1))
eegcap(electrodes = myelectrodes,cex.point = 7,col.point = "#BEBEBECC",
       cex.label = 1.5,col.label = "black")
mtext("Delta-Theta-Alpha : Small Ref : ADHD",side = 3,cex=2)
add_arrows(conn_agg[,,1,1,1],lwd = 4,spc = 1.5,adj = 0.5,
           arr.length = 0.2,cut_off = c(0.5,0.5,0.75))

eegcap(electrodes = myelectrodes,cex.point = 7,col.point = "#BEBEBECC",
       cex.label = 1.5,col.label = "black")
mtext("Delta-Theta-Alpha : Small Ref : Control",side = 3,cex=2)
add_arrows(conn_agg[,,1,1,2],lwd = 4,spc = 1.5,adj = 0.5,
           arr.length = 0.2,cut_off = c(0.5,0.5,0.75))

eegcap(electrodes = myelectrodes,cex.point = 7,col.point = "#BEBEBECC",
       cex.label = 1.5,col.label = "black")
mtext("Delta-Theta-Alpha : Large Ref : ADHD",side = 3,cex=2)
add_arrows(conn_agg[,,1,2,1],lwd = 4,spc = 1.5,adj = 0.5,
           arr.length = 0.2,cut_off = c(0.5,0.5,0.75))

eegcap(electrodes = myelectrodes,cex.point = 7,col.point = "#BEBEBECC",
       cex.label = 1.5,col.label = "black")
mtext("Delta-Theta-Alpha : Large Ref : Control",side = 3,cex=2)
add_arrows(conn_agg[,,1,2,2],lwd = 4,spc = 1.5,adj = 0.5,
           arr.length = 0.2,cut_off = c(0.5,0.5,0.75))

### Beta-Gamma Connectivity
eegcap(electrodes = myelectrodes,cex.point = 7,col.point = "#BEBEBECC",
       cex.label = 1.5,col.label = "black")
mtext("Beta-Gamma : Small Ref : ADHD",side = 3,cex=2)
add_arrows(conn_agg[,,2,1,1],lwd = 4,spc = 1.5,adj = 0.5,
           arr.length = 0.2,cut_off = c(0.5,0.5,0.75))

eegcap(electrodes = myelectrodes,cex.point = 7,col.point = "#BEBEBECC",
       cex.label = 1.5,col.label = "black")
mtext("Beta-Gamma : Small Ref : Control",side = 3,cex=2)
add_arrows(conn_agg[,,2,1,2],lwd = 4,spc = 1.5,adj = 0.5,
           arr.length = 0.2,cut_off = c(0.5,0.5,0.75))

eegcap(electrodes = myelectrodes,cex.point = 7,col.point = "#BEBEBECC",
       cex.label = 1.5,col.label = "black")
mtext("Beta-Gamma : Large Ref : ADHD",side = 3,cex=2)
add_arrows(conn_agg[,,2,2,1],lwd = 4,spc = 1.5,adj = 0.5,
           arr.length = 0.2,cut_off = c(0.5,0.5,0.75))

eegcap(electrodes = myelectrodes,cex.point = 7,col.point = "#BEBEBECC",
       cex.label = 1.5,col.label = "black")
mtext("Beta-Gamma : Large Ref : Control",side = 3,cex=2)
add_arrows(conn_agg[,,2,2,2],lwd = 4,spc = 1.5,adj = 0.5,
           arr.length = 0.2,cut_off = c(0.5,0.5,0.75))


#########################################################################
##### Plotting fPDC values per Time Window - Supplementary Material #####
#########################################################################

##### Plot settings
col.pal1<-colorRampPalette(c("white","orange"))
col.pal2<-colorRampPalette(c("orange","darkred"))
theme_setting <- theme(axis.text=element_blank(),axis.title=element_blank(),
                       axis.text.x = element_text(size=6,margin = margin(t = -7.5)),
                       axis.text.y = element_text(size=6,margin = margin(r = -7.5)),
                       plot.title = element_text(face = "bold",size = 20,hjust = 0.5,
                                                 margin = margin(t=20)),
                       legend.title = element_text(face = "bold",size = 14),
                       legend.text=element_text(size=10),
                       legend.key.size = unit(0.9, 'cm'),
                       plot.margin=grid::unit(c(1,1,1,1), "mm"))
lwidth <- 0.15
mychannels <- c("Fp1","Fp2","O1","O2","T7","T8")

for(tw in 1:17){

  gr <- 1
  col_lim <- c(0,max(fpdc_all[,,,,gr,tw],na.rm = TRUE) + 0.005)
  figure_list <- vector("list",length = 6)
  for(i in 1:6){
    for(j in 1:6){
      if(i != j){
        u <- result[[tw]]$fhat_points
        u_0 <- mean(diff(u)/2)
        freq <- seq(0.01,0.5,by = 0.01)
        freq_id <- round(seq(1,320,length = 50),0)
        z <- fpdc_all[i,j,freq_id,,gr,tw]
        rect_all <- fpdc_prom[i,j,freq_id,,gr,tw]
        data_melt <- NULL
        rect_melt <- NULL
        
        for(h in 1:length(u)){
          for(g in 1:length(freq)){
            entry <- data.frame(freq = freq[g],u = u[h],fPDC = z[g,h])
            data_melt <- rbind(data_melt,entry)
            if(rect_all[g,h] == 1){
              entry <- data.frame(freq = freq[g],u = u[h])
              rect_melt <- rbind(rect_melt,entry)
            }
          }
        }
        
        ggp <- ggplot(data_melt, aes(freq, u)) +  
          geom_tile(aes(fill = fPDC)) + theme_classic(base_line_size = 0) +
          scale_fill_gradientn(colours = c(col.pal1(100),col.pal2(100)),limits= col_lim,
                               name = "fPDC", labels = function(x) format(round(x, 2), nsmall = 2)) +
          theme_setting
        
        if(!is.null(rect_melt)){
          ggp <-  ggp + geom_rect(data=rect_melt, linewidth=lwidth, fill=NA, colour="darkred",
                                  aes(xmin=freq - 0.005, xmax=freq + 0.005, ymin=u - u_0, ymax=u + u_0))
        }
      } else {
        ggp <- ggplot() +  annotate("text",x = 1,y = 1,size = 10,label = mychannels[i]) + theme_void()
      }
      
      figure_list[[6*(i-1) + j]] <- ggp
      
    }
  }
  
  
  figure <- ggarrange(plotlist = figure_list, ncol = 6, nrow = 6,
                      common.legend = TRUE, legend="right")
  figure <- annotate_figure(figure,
                            left = textGrob(TeX("Reference Signal (Cz at lag 6)",bold = TRUE),
                                            rot = 90,x = 0.35,y = 0.5,gp = gpar(cex = 1.25)))
  figure <- annotate_figure(figure,
                            bottom = textGrob(TeX("Frequency $\\omega$",bold = TRUE),
                                              rot = 0,x = 0.5,y = 0.35,gp = gpar(cex = 1.25)))
  figure_a <- annotate_figure(figure,
                              top = textGrob(TeX("(a) ADHD",bold = TRUE),
                                             rot = 0,x = 0.065,y = 0.5,gp = gpar(cex = 2)))
  
  gr <- 2
  col_lim <- c(0,max(fpdc_all[,,,,gr,tw],na.rm = TRUE) + 0.005)
  figure_list <- vector("list",length = 6)
  
  for(i in 1:6){
    for(j in 1:6){
      if(i != j){
        u <- result[[tw]]$fhat_points
        u_0 <- mean(diff(u)/2)
        freq <- seq(0.01,0.5,by = 0.01)
        freq_id <- round(seq(1,320,length = 50),0)
        z <- fpdc_all[i,j,freq_id,,gr,tw]
        rect_all <- fpdc_prom[i,j,freq_id,,gr,tw]
        
        data_melt <- NULL
        rect_melt <- NULL
        
        for(h in 1:length(u)){
          for(g in 1:length(freq)){
            entry <- data.frame(freq = freq[g],u = u[h],fPDC = z[g,h])
            data_melt <- rbind(data_melt,entry)
            if(rect_all[g,h] == 1){
              entry <- data.frame(freq = freq[g],u = u[h])
              rect_melt <- rbind(rect_melt,entry)
            }
          }
        }
        
        ggp <- ggplot(data_melt, aes(freq, u)) +  
          geom_tile(aes(fill = fPDC)) + theme_classic(base_line_size = 0) +
          scale_fill_gradientn(colours = c(col.pal1(100),col.pal2(100)),limits= col_lim,
                               name = "fPDC", labels = function(x) format(round(x, 2), nsmall = 2)) +
          theme_setting
        
        if(!is.null(rect_melt)){
          ggp <-  ggp + geom_rect(data=rect_melt, linewidth=lwidth, fill=NA, colour="darkred",
                                  aes(xmin=freq - 0.005, xmax=freq + 0.005, ymin=u - u_0, ymax=u + u_0))
        }
      } else {
        ggp <- ggplot() +  annotate("text",x = 1,y = 1,size = 10,label = mychannels[i]) + theme_void()
      }
      
      figure_list[[6*(i-1) + j]] <- ggp
      
    }
  }
  
  
  figure <- ggarrange(plotlist = figure_list, ncol = 6, nrow = 6,
                      common.legend = TRUE, legend="right")
  figure <- annotate_figure(figure,
                            left = textGrob(TeX("Reference Signal (Cz at lag 6)",bold = TRUE),
                                            rot = 90,x = 0.35,y = 0.5,gp = gpar(cex = 1.25)))
  figure <- annotate_figure(figure,
                            bottom = textGrob(TeX("Frequency $\\omega$",bold = TRUE),
                                              rot = 0,x = 0.5,y = 0.35,gp = gpar(cex = 1.25)))
  figure_b <- annotate_figure(figure,
                              top = textGrob(TeX("(b) Control",bold = TRUE),
                                             rot = 0,x = 0.075,y = 0.5,gp = gpar(cex = 2)))
  figure_ab <- ggarrange(figure_a,figure_b,ncol = 1,nrow = 2)
  
  ggsave(paste("Supplementary Material Plots/FPDC_Matrix_TW",tw,".png",sep = ""),plot = figure_ab, bg = "white",
         width = 11.25, height = 18, dpi = 300, units = "in", device='png')
  
  cat(paste("DONE: tw=",tw,sep = "")," \r")
}

##########################################################################
##### Sensitivity to the Choice of Channels - Supplementary Material #####
##########################################################################

##### Estimation of MXFAR model and the fPDC metric for the Leave-One-Channel-Out Network
fpdc_less1_all <- array(NA,dim = c(5,5,length(freq),51,2,17,6))

for(f_i in 1:6){
  
  ch_ind <- (1:6)[!((1:6) %in% f_i)]
  
  for(tw in 1:17){
    sub <- NULL
    for(i in 1:104){
      sub <- rbind(sub,subset(visualeeg,id == ids[i])[2.5*(tw-1)*128 + 1:640,-c(1,2)])
    }
    
    result <- MXFAR.est(SeriesNum = c(51,53),Tlength = 640,y = as.matrix(sub[,ch_ind]),
                        u = abs(sub[,7]),p = 4,d = 6,bwp = 0.2,numpoints = 50,fpdc = TRUE)
    
    fpdc_less1_all[,,,,,tw,f_i] <-  result$fpdc$fpdc_mean
    cat(paste("DONE: f_i = ",f_i,", tw=",tw,sep = "")," \r")
  }
}

# saveRDS(fpdc_less1_all,"fpdc_less1_all.rds") # saving results

##### Summarizing all leave-one-channel-out network
# fpdc_less1_all <- readRDS("fpdc_less1_all.rds") # loading results
low.u <- 1:26 # reference signal (u<1)
high.u <- 27:51 # reference signal (u>1)
dta.w <- 1:60 # frequency delta-theta-alpha
bg.w <- 61:225 # beta-gamma

qt.level <- 0.80 # quantile level
perc.sig <- 0.10 # significance threshold

conn_less1 <- array(NA,c(6,6,2,2,2,6)) # compiling results

for(f_i in 1:6){
  
  fpdc_less1 <- fpdc_less1_all[,,,,,,f_i]
  
  for(i in 1:5){
    fpdc_less1[i,i,,,,] <- NA
  }

  fpdc_qt_less1 <- apply(fpdc_less1,MARGIN = c(3,4,5,6),FUN = quantile,probs = qt.level,na.rm = TRUE)
  fpdc_prom_less1 <- array(NA,dim = dim(fpdc_less1))
  for(i in 1:5){
    for(j in 1:5){
      if(i == j){next}
      fpdc_prom_less1[i,j,,,,] <- fpdc_less1[i,j,,,,] > fpdc_qt_less1
    }
  }

  conn_agg <- array(NA,c(5,5,2,2,2,17)) # ch1, ch2, freq, u, group, tw
  conn_agg[,,1,1,,] <- apply(fpdc_prom_less1[,,dta.w,low.u,,],MARGIN = c(1,2,5,6),FUN = sum,na.rm = TRUE) >= length(dta.w)*length(low.u)*perc.sig
  conn_agg[,,1,2,,] <- apply(fpdc_prom_less1[,,dta.w,high.u,,],MARGIN = c(1,2,5,6),FUN = sum,na.rm = TRUE) >= length(dta.w)*length(high.u)*perc.sig
  conn_agg[,,2,1,,] <- apply(fpdc_prom_less1[,,bg.w,low.u,,],MARGIN = c(1,2,5,6),FUN = sum,na.rm = TRUE) >= length(bg.w)*length(low.u)*perc.sig
  conn_agg[,,2,2,,] <- apply(fpdc_prom_less1[,,bg.w,high.u,,],MARGIN = c(1,2,5,6),FUN = sum,na.rm = TRUE) >= length(bg.w)*length(high.u)*perc.sig

  conn_agg2 <- apply(conn_agg,MARGIN = c(1,2,3,4,5),FUN = mean,na.rm = TRUE)
  ch_ind <- (1:6)[!((1:6) %in% f_i)]
  conn_less1[ch_ind,ch_ind,,,,f_i] <- conn_agg2
  cat(paste("DONE: f_i = ",f_i,sep = "")," \r")
}
conn_agg <- apply(conn_less1,MARGIN = c(1,2,3,4,5),FUN = mean,na.rm = TRUE)

### Delta-Theta-Alpha Connectivity (Leave-One-Channel-Out Network)
par(mfrow = c(1,1))
eegcap(electrodes = myelectrodes,cex.point = 7,col.point = "#BEBEBECC",
       cex.label = 1.5,col.label = "black")
mtext("Delta-Theta-Alpha : Small Ref : ADHD",side = 3,cex=2)
add_arrows(conn_agg[,,1,1,1],lwd = 4,spc = 1.5,adj = 0.5,
           arr.length = 0.2,cut_off = c(0.4999,0.499999,0.5))

eegcap(electrodes = myelectrodes,cex.point = 7,col.point = "#BEBEBECC",
       cex.label = 1.5,col.label = "black")
mtext("Delta-Theta-Alpha : Small Ref : Control",side = 3,cex=2)
add_arrows(conn_agg[,,1,1,2],lwd = 4,spc = 1.5,adj = 0.5,
           arr.length = 0.2,cut_off = c(0.4999,0.499999,0.5))

eegcap(electrodes = myelectrodes,cex.point = 7,col.point = "#BEBEBECC",
       cex.label = 1.5,col.label = "black")
mtext("Delta-Theta-Alpha : Large Ref : ADHD",side = 3,cex=2)
add_arrows(conn_agg[,,1,2,1],lwd = 4,spc = 1.5,adj = 0.5,
           arr.length = 0.2,cut_off = c(0.4999,0.499999,0.5))

eegcap(electrodes = myelectrodes,cex.point = 7,col.point = "#BEBEBECC",
       cex.label = 1.5,col.label = "black")
mtext("Delta-Theta-Alpha : Large Ref : Control",side = 3,cex=2)
add_arrows(conn_agg[,,1,2,2],lwd = 4,spc = 1.5,adj = 0.5,
           arr.length = 0.2,cut_off = c(0.4999,0.499999,0.5))


### Beta-Gamma Connectivity (Leave-One-Channel-Out Network)
eegcap(electrodes = myelectrodes,cex.point = 7,col.point = "#BEBEBECC",
       cex.label = 1.5,col.label = "black")
mtext("Beta-Gamma : Small Ref : ADHD",side = 3,cex=2)
add_arrows(conn_agg[,,2,1,1],lwd = 4,spc = 1.5,adj = 0.5,
           arr.length = 0.2,cut_off = c(0.4999,0.499999,0.5))

eegcap(electrodes = myelectrodes,cex.point = 7,col.point = "#BEBEBECC",
       cex.label = 1.5,col.label = "black")
mtext("Beta-Gamma : Small Ref : Control",side = 3,cex=2)
add_arrows(conn_agg[,,2,1,2],lwd = 4,spc = 1.5,adj = 0.5,
           arr.length = 0.2,cut_off = c(0.4999,0.499999,0.5))

eegcap(electrodes = myelectrodes,cex.point = 7,col.point = "#BEBEBECC",
       cex.label = 1.5,col.label = "black")
mtext("Beta-Gamma : Large Ref : ADHD",side = 3,cex=2)
add_arrows(conn_agg[,,2,2,1],lwd = 4,spc = 1.5,adj = 0.5,
           arr.length = 0.2,cut_off = c(0.4999,0.499999,0.5))

eegcap(electrodes = myelectrodes,cex.point = 7,col.point = "#BEBEBECC",
       cex.label = 1.5,col.label = "black")
mtext("Beta-Gamma : Large Ref : Control",side = 3,cex=2)
add_arrows(conn_agg[,,2,2,2],lwd = 4,spc = 1.5,adj = 0.5,
           arr.length = 0.2,cut_off = c(0.4999,0.499999,0.5))


###########################################################################
##### Significant and Consistent fPDC values - Supplementary Material #####
###########################################################################

k <- 6 # six channels
p <- 4 # four lags in the VAR model
Tlength <- 640 # number of time points in a single time window
freq <- seq(0,0.5,length = 321)[-1] # Nyquist frequencies

fpdc_all <- array(NA,dim = c(6,6,length(freq),51,2,17)) # aggregated PDC values for the two groups
thres_all <- array(NA,dim = c(k,k,length(freq),51,2,17))
chi_crit <- qchisq(p = 0.95,df = 1) # to control for type I error

for(tw in 1:17){

  sigma <- colMeans((result[[tw]]$resid)^2,na.rm = TRUE)
  
  y_sub <- NULL
  for(i in 1:104){
    sub <- subset(visualeeg,id == ids[i])[2.5*(tw-1)*128 + 1:640,-9]
    y_sub <- rbind(y_sub,sub)
  }
  
  R_mat_all <- array(NA,dim = c(k*p,k*p,length(ids)))
  
  for(i in 1:104){
    
    y_sub_id <- subset(y_sub,id == ids[i])
    
    for(lag1 in 1:p){
      for(lag2 in 1:p){
        for(ch1 in 1:k){
          for(ch2 in 1:k){
            
            R_mat_all[(lag1-1)*k+ch1,(lag2-1)*k+ch2,i] <-
              cov(y_sub_id[(1+max(lag1,lag2)-lag1):(640-lag1),ch1+2],
                  y_sub_id[(1+max(lag1,lag2)-lag2):(640-lag2),ch2+2],
                  use = "pairwise.complete.obs")
            
          }
        }
      }
    }
  }

  R_mat <- apply(R_mat_all,MARGIN = c(1,2),FUN = mean,na.rm = TRUE)
  H_mat <- solve(R_mat)
  C_mat <- array(NA,dim = c(k,k,length(freq)))
  
  for(g in 1:length(freq)){
    f <- freq[g]
    for(ch1 in 1:k){
      for(ch2 in 1:k){
        C_ij <- 0
        for(lag1 in 1:p){
          for(lag2 in 1:p){
            C_ij <- C_ij + sigma[ch1]*H_mat[(lag1-1)*k+ch2,(lag2-1)*k+ch2]*(cos(2*pi*f*lag1)*cos(2*pi*f*lag2)+sin(2*pi*f*lag1)*sin(2*pi*f*lag2))
            
          }
        }
        C_mat[ch1,ch2,g] <- C_ij
      }
    }
  }
  
  fhat_mean <- result[[tw]]$fhat_mean
  
  for(h in 1:51){
    for(n in 1:2){
      A <- fhat_mean[,,h,n]
      for(g in 1:length(freq)){
        f <- freq[g]
        A_f <- matrix(0,nrow = k,ncol = k)
        A_f_all <- matrix(rep(exp(-2i*pi*f*(1:p)),each = k^2),nrow = k)*A
        for(l in 1:p){
          A_f <- A_f + A_f_all[,k*(l-1)+1:k]
        }
        A_bar <- diag(rep(1,k)) -  A_f
        
        for(i in 1:k){
          for(j in 1:k){
            fpdc_all[i,j,g,h,n,tw] <- Mod(A_bar[i,j])/sqrt(sum(Mod(A_bar[,j])^2))
            thres_all[i,j,g,h,n,tw] <- sqrt((C_mat[i,j,g]*chi_crit)/(Tlength*sum(Mod(A_bar[,j])^2)))
          }
        }
      }
    }
  }
  cat(paste("DONE: ",tw,"/ 17")," \r")
}
# saveRDS(fpdc_all,"fpdc_est_all.rds") # saving results
# saveRDS(thres_all,"fpdc_thres_all.rds") # saving results

### Defining significant connections based on the frequency-specific threshold
# fpdc_all <- readRDS("fpdc_est_all.rds") # loading results
# thres_all <- readRDS("fpdc_thres_all.rds") # loading results
conn_sig <- fpdc_all > thres_all

### Aggregating significant connections for each frequency band of interest
low.u <- 1:26 # reference signal (u<1)
high.u <- 27:51 # reference signal (u>1)
dta.w <- 1:60 # frequency delta-theta-alpha
bg.w <- 61:225 # beta-gamma
perc.sig <- 0.01
conn_sig_agg <- array(NA,c(6,6,2,2,2,17)) # freq, u, group
conn_sig_agg[,,1,1,,] <- apply(conn_sig[,,dta.w,low.u,,],MARGIN = c(1,2,5,6),FUN = sum,na.rm = TRUE) >= length(dta.w)*length(low.u)*perc.sig
conn_sig_agg[,,1,2,,] <- apply(conn_sig[,,dta.w,high.u,,],MARGIN = c(1,2,5,6),FUN = sum,na.rm = TRUE) >= length(dta.w)*length(high.u)*perc.sig
conn_sig_agg[,,2,1,,] <- apply(conn_sig[,,bg.w,low.u,,],MARGIN = c(1,2,5,6),FUN = sum,na.rm = TRUE) >= length(bg.w)*length(low.u)*perc.sig
conn_sig_agg[,,2,2,,] <- apply(conn_sig[,,bg.w,high.u,,],MARGIN = c(1,2,5,6),FUN = sum,na.rm = TRUE) >= length(bg.w)*length(high.u)*perc.sig
conn_agg <- apply(conn_sig_agg,MARGIN = c(1,2,3,4,5),FUN = mean,na.rm = TRUE)

### Delta-Theta-Alpha Connectivity
par(mfrow = c(1,1))
eegcap(electrodes = myelectrodes,cex.point = 7,col.point = "#BEBEBECC",
       cex.label = 1.5,col.label = "black")
mtext("Delta-Theta-Alpha : Small Ref : ADHD",side = 3,cex=2)
add_arrows(conn_agg[,,1,1,1],lwd = 4,spc = 1.5,adj = 0.5,
           arr.length = 0.2,cut_off = c(0.5,0.5,0.75))

eegcap(electrodes = myelectrodes,cex.point = 7,col.point = "#BEBEBECC",
       cex.label = 1.5,col.label = "black")
mtext("Delta-Theta-Alpha : Small Ref : Control",side = 3,cex=2)
add_arrows(conn_agg[,,1,1,2],lwd = 4,spc = 1.5,adj = 0.5,
           arr.length = 0.2,cut_off = c(0.5,0.5,0.75))

eegcap(electrodes = myelectrodes,cex.point = 7,col.point = "#BEBEBECC",
       cex.label = 1.5,col.label = "black")
mtext("Delta-Theta-Alpha : Large Ref : ADHD",side = 3,cex=2)
add_arrows(conn_agg[,,1,2,1],lwd = 4,spc = 1.5,adj = 0.5,
           arr.length = 0.2,cut_off = c(0.5,0.5,0.75))

eegcap(electrodes = myelectrodes,cex.point = 7,col.point = "#BEBEBECC",
       cex.label = 1.5,col.label = "black")
mtext("Delta-Theta-Alpha : Large Ref : Control",side = 3,cex=2)
add_arrows(conn_agg[,,1,2,2],lwd = 4,spc = 1.5,adj = 0.5,
           arr.length = 0.2,cut_off = c(0.5,0.5,0.75))

### Beta-Gamma Connectivity
eegcap(electrodes = myelectrodes,cex.point = 7,col.point = "#BEBEBECC",
       cex.label = 1.5,col.label = "black")
mtext("Beta-Gamma : Small Ref : ADHD",side = 3,cex=2)
add_arrows(conn_agg[,,2,1,1],lwd = 4,spc = 1.5,adj = 0.5,
           arr.length = 0.2,cut_off = c(0.5,0.5,0.75))

eegcap(electrodes = myelectrodes,cex.point = 7,col.point = "#BEBEBECC",
       cex.label = 1.5,col.label = "black")
mtext("Beta-Gamma : Small Ref : Control",side = 3,cex=2)
add_arrows(conn_agg[,,2,1,2],lwd = 4,spc = 1.5,adj = 0.5,
           arr.length = 0.2,cut_off = c(0.5,0.5,0.75))

eegcap(electrodes = myelectrodes,cex.point = 7,col.point = "#BEBEBECC",
       cex.label = 1.5,col.label = "black")
mtext("Beta-Gamma : Large Ref : ADHD",side = 3,cex=2)
add_arrows(conn_agg[,,2,2,1],lwd = 4,spc = 1.5,adj = 0.5,
           arr.length = 0.2,cut_off = c(0.5,0.5,0.75))

eegcap(electrodes = myelectrodes,cex.point = 7,col.point = "#BEBEBECC",
       cex.label = 1.5,col.label = "black")
mtext("Beta-Gamma : Large Ref : Control",side = 3,cex=2)
add_arrows(conn_agg[,,2,2,2],lwd = 4,spc = 1.5,adj = 0.5,
           arr.length = 0.2,cut_off = c(0.5,0.5,0.75))



###########################################################
##### EEG Analysis using the Linear VAR-PDC Framework #####
###########################################################

##### Estimation of the linear VAR model and the PDC metric
var_est_all <- array(NA,dim = c(6,24,104,17))
var_resid_all <- array(NA,dim = c(6,636,104,17))

for(tw in 1:17){
  
  for(i in 1:104){
    sub <- subset(visualeeg,id == ids[i])[2.5*(tw-1)*128 + 1:640,-c(1,2,9)]
    var_est <- VAR(sub,p=4,type = "none")
    
    for(ch in 1:6){
      var_est_all[ch,,i,tw] <- var_est$varresult[[ch]]$coefficients
      
      var_resid_all[ch,,i,tw] <- var_est$varresult[[ch]]$residuals
    }
    
  }
  cat(paste("DONE: tw=",tw,sep = "")," \r")
}

##### Aggregating VAR coefficients for the ADHD and control groups
var_est_agg <- array(NA,dim = c(6,24,2,17))
var_est_agg[,,1,] <- apply(var_est_all[,,1:51,],MARGIN = c(1,2,4),FUN = mean,na.rm = TRUE)
var_est_agg[,,2,] <- apply(var_est_all[,,52:104,],MARGIN = c(1,2,4),FUN = mean,na.rm = TRUE)

##### Calculating linear PDC values for the ADHD and control groups
freq <- seq(0,0.5,length = 321)[-1] # Nyquist frequencies
pdc_all <- array(NA,dim = c(6,6,length(freq),2,17)) # aggregated PDC values for the two groups

for(tw in 1:17){
  
  var_est <- var_est_agg[,,,tw]
  
  for(i in 1:2){
    A <- var_est[,,i]
    for(g in 1:length(freq)){
      f <- freq[g]
      A_f <- matrix(0,nrow = 6,ncol = 6)
      A_f_all <- matrix(rep(exp(-2i*pi*f*(1:4)),each = 36),nrow = 6)*A
      for(l in 1:4){
        A_f <- A_f + A_f_all[,6*(l-1)+1:6]
      }
      A_bar <- diag(rep(1,6)) -  A_f
      
      for(ch1 in 1:6){
        for(ch2 in 1:6){
          pdc_all[ch1,ch2,g,i,tw] <- Mod(A_bar[ch1,ch2])/sqrt(sum(Mod(A_bar[,ch2])^2))
        }
      }
    }
  }
  cat(paste("DONE: tw=",tw,sep = "")," \r")
}

for(ch in 1:6){
  pdc_all[ch,ch,,,] <- NA
}

##### Aggregating results into the two frequency bands of interests
dta.w <- 1:60 # frequency delta-theta-alpha
bg.w <- 61:225 # beta-gamma

qt.level <- 0.80 # quantile level
perc.sig <- 0.10 # significance threshold

### Defining prominent connections
pdc_qt <- apply(pdc_all,MARGIN = c(3,4,5),FUN = stats::quantile,probs = qt.level,na.rm = TRUE)
pdc_prom <- array(NA,dim = dim(pdc_all))
for(i in 1:6){
  for(j in 1:6){
    if(i == j){next}
    pdc_prom[i,j,,,] <- pdc_all[i,j,,,] > pdc_qt
  }
}

### Defining consitent connections
conn_tw <- array(NA,c(6,6,2,2,17)) # ch1,ch2,freq,group,tw
conn_tw[,,1,,] <- apply(pdc_prom[,,dta.w,,],MARGIN = c(1,2,4,5),
                        FUN = sum,na.rm = TRUE) >= length(dta.w)*perc.sig
conn_tw[,,2,,] <- apply(pdc_prom[,,bg.w,,],MARGIN = c(1,2,4,5),FUN = sum,na.rm = TRUE) >= length(bg.w)*perc.sig
conn_agg <- apply(conn_tw,MARGIN = c(1,2,3,4),FUN = mean,na.rm = TRUE)

### Delta-Theta-Alpha Connectivity
par(mfrow = c(1,1))
eegcap(electrodes = myelectrodes,cex.point = 7,col.point = "#BEBEBECC",
       cex.label = 1.5,col.label = "black")
mtext("Delta-Theta-Alpha : ADHD",side = 3,cex=2)
add_arrows(conn_agg[,,1,1],lwd = 4,spc = 1.5,adj = 0.5,
           arr.length = 0.2,cut_off = c(0.5,0.5,0.75))

eegcap(electrodes = myelectrodes,cex.point = 7,col.point = "#BEBEBECC",
       cex.label = 1.5,col.label = "black")
mtext("Delta-Theta-Alpha : Control",side = 3,cex=2)
add_arrows(conn_agg[,,1,2],lwd = 4,spc = 1.5,adj = 0.5,
           arr.length = 0.2,cut_off = c(0.5,0.5,0.75))

### Beta-Gamma Connectivity
eegcap(electrodes = myelectrodes,cex.point = 7,col.point = "#BEBEBECC",
       cex.label = 1.5,col.label = "black")
mtext("Beta-Gamma : ADHD",side = 3,cex=2)
add_arrows(conn_agg[,,2,1],lwd = 4,spc = 1.5,adj = 0.5,
           arr.length = 0.2,cut_off = c(0.5,0.5,0.75))

eegcap(electrodes = myelectrodes,cex.point = 7,col.point = "#BEBEBECC",
       cex.label = 1.5,col.label = "black")
mtext("Beta-Gamma : Control",side = 3,cex=2)
add_arrows(conn_agg[,,2,2],lwd = 4,spc = 1.5,adj = 0.5,
           arr.length = 0.2,cut_off = c(0.5,0.5,0.75))


#################################################################################
##### Significant and Consistent linear PDC values - Supplementary Material #####
#################################################################################

k <- 6 # six channels
p <- 4 # four lags in the VAR model
Tlength <- 640 # number of time points in a single time window
freq <- seq(0,0.5,length = 321)[-1] # Nyquist frequencies

pdc_all <- array(NA,dim = c(6,6,length(freq),2,17)) # aggregated PDC values for the two groups
thres_all <- array(NA,dim = c(k,k,length(freq),2,17))
chi_crit <- qchisq(p = 0.95,df = 1) # to control for type I error

for(tw in 1:17){
  
  sigma <- apply(var_resid_all[,,,tw]^2,MARGIN = 1,FUN = mean,na.rm = TRUE)
  
  y_sub <- NULL
  for(i in 1:104){
    sub <- subset(visualeeg,id == ids[i])[2.5*(tw-1)*128 + 1:640,-9]
    y_sub <- rbind(y_sub,sub)
  }
  
  R_mat_all <- array(NA,dim = c(k*p,k*p,length(ids)))
  
  for(i in 1:104){
    
    y_sub_id <- subset(y_sub,id == ids[i])
    
    for(lag1 in 1:p){
      for(lag2 in 1:p){
        for(ch1 in 1:k){
          for(ch2 in 1:k){
            
            R_mat_all[(lag1-1)*k+ch1,(lag2-1)*k+ch2,i] <-
              cov(y_sub_id[(1+max(lag1,lag2)-lag1):(640-lag1),ch1+2],
                  y_sub_id[(1+max(lag1,lag2)-lag2):(640-lag2),ch2+2],
                  use = "pairwise.complete.obs")
            
          }
        }
      }
    }
  }
  
  R_mat <- apply(R_mat_all,MARGIN = c(1,2),FUN = mean,na.rm = TRUE)
  H_mat <- solve(R_mat)
  C_mat <- array(NA,dim = c(k,k,length(freq)))
  
  for(g in 1:length(freq)){
    f <- freq[g]
    for(ch1 in 1:k){
      for(ch2 in 1:k){
        C_ij <- 0
        for(lag1 in 1:p){
          for(lag2 in 1:p){
            C_ij <- C_ij + sigma[ch1]*H_mat[(lag1-1)*k+ch2,(lag2-1)*k+ch2]*(cos(2*pi*f*lag1)*cos(2*pi*f*lag2)+sin(2*pi*f*lag1)*sin(2*pi*f*lag2))
            
          }
        }
        C_mat[ch1,ch2,g] <- C_ij
      }
    }
  }
  
  var_est <- var_est_agg[,,,tw]
  
  for(n in 1:dim(var_est)[3]){
    A <- var_est[,,n]
    for(g in 1:length(freq)){
      f <- freq[g]
      A_f <- matrix(0,nrow = k,ncol = k)
      A_f_all <- matrix(rep(exp(-2i*pi*f*(1:p)),each = k^2),nrow = k)*A
      for(l in 1:p){
        A_f <- A_f + A_f_all[,k*(l-1)+1:k]
      }
      A_bar <- diag(rep(1,k)) -  A_f
      
      for(ch1 in 1:k){
        for(ch2 in 1:k){
          pdc_all[ch1,ch2,g,n,tw] <- Mod(A_bar[ch1,ch2])/sqrt(sum(Mod(A_bar[,ch2])^2))
          thres_all[ch1,ch2,g,n,tw] <- sqrt((C_mat[ch1,ch2,g]*chi_crit)/(Tlength*sum(Mod(A_bar[,ch2])^2)))
        }
      }
    }
  }
  cat(paste("DONE: ",tw,"/ 17")," \r")
}
# saveRDS(pdc_all,"pdc_est_all.rds") # saving results
# saveRDS(thres_all,"pdc_thres_all.rds") # saving results

### Defining significant connections based on the frequency-specific threshold
# pdc_all <- readRDS("pdc_est_all.rds") # loading results
# thres_all <- readRDS("pdc_thres_all.rds") # loading results
conn_sig <- pdc_all > thres_all

### Aggregating significant connections for each frequency band of interest
dta.w <- 1:60 # frequency delta-theta-alpha
bg.w <- 61:225 # beta-gamma
perc.sig <- 0.01
conn_sig_agg <- array(NA,c(6,6,2,2,17)) # ch1, ch2, freq, group, tw
conn_sig_agg[,,1,,] <- apply(conn_sig[,,dta.w,,],MARGIN = c(1,2,4,5),FUN = sum,na.rm = TRUE) >= length(dta.w)*perc.sig
conn_sig_agg[,,2,,] <- apply(conn_sig[,,bg.w,,],MARGIN = c(1,2,4,5),FUN = sum,na.rm = TRUE) >= length(bg.w)*perc.sig
conn_agg <- apply(conn_sig_agg,MARGIN = c(1,2,3,4),FUN = mean,na.rm = TRUE)

### Delta-Theta-Alpha Connectivity
par(mfrow = c(1,1))
eegcap(electrodes = myelectrodes,cex.point = 7,col.point = "#BEBEBECC",
       cex.label = 1.5,col.label = "black")
mtext("Delta-Theta-Alpha : ADHD",side = 3,cex=2)
add_arrows(conn_agg[,,1,1],lwd = 4,spc = 1.5,adj = 0.5,
           arr.length = 0.2,cut_off = c(0.5,0.5,0.75))

eegcap(electrodes = myelectrodes,cex.point = 7,col.point = "#BEBEBECC",
       cex.label = 1.5,col.label = "black")
mtext("Delta-Theta-Alpha : Control",side = 3,cex=2)
add_arrows(conn_agg[,,1,2],lwd = 4,spc = 1.5,adj = 0.5,
           arr.length = 0.2,cut_off = c(0.5,0.5,0.75))

### Beta-Gamma Connectivity
eegcap(electrodes = myelectrodes,cex.point = 7,col.point = "#BEBEBECC",
       cex.label = 1.5,col.label = "black")
mtext("Beta-Gamma : ADHD",side = 3,cex=2)
add_arrows(conn_agg[,,2,1],lwd = 4,spc = 1.5,adj = 0.5,
           arr.length = 0.2,cut_off = c(0.5,0.5,0.75))

eegcap(electrodes = myelectrodes,cex.point = 7,col.point = "#BEBEBECC",
       cex.label = 1.5,col.label = "black")
mtext("Beta-Gamma : Control",side = 3,cex=2)
add_arrows(conn_agg[,,2,2],lwd = 4,spc = 1.5,adj = 0.5,
           arr.length = 0.2,cut_off = c(0.5,0.5,0.75))


##########################################################
##### EEG Analysis using the Spectral cGGC Framework #####
##########################################################

##### Estimation of the spectral cGGC metric
ggc_all <- array(NA,dim = c(6,6,104,320,17))
freq <- seq(0,0.5,length = 321)[-1] # Nyquist frequencies

##### Estimation
system.time({
  for(tw in 1:17){
    for(i in 1:104){
      sub <- subset(visualeeg,id == ids[i])[2.5*(tw-1)*128 + 1:640,-c(1,2,9)]
      
      for(ch1 in 1:6){
        for(ch2 in 1:6){
          if(ch1 == ch2){next}
          
          GGC_ij_l <- gran.cond(x = sub[,ch1], y = sub[,ch2], z = sub[,(1:6)[!(1:6 %in% c(ch1,ch2))]],p1 = 4,p2 = 4)
          ggc_all[ch1,ch2,i,,tw] <- GGC_ij_l$Conditional_causality_y.to.x.on.z
          cat(paste("DONE: tw = ",tw,", i = ",i,", i = ",ch1,", j = ",ch2,sep=""),"\r")
        }
      }
    }
  }
})
# saveRDS(ggc_all,"result_ggc.rds") # saving results

##### System.time
#   user   system  elapsed
# 7545.616  248.241 7798.986

# ggc_all <- readRDS("result_ggc.rds") # loading results
ggc_agg <- array(NA,dim = c(6,6,320,2,17))
ggc_agg[,,,1,] <- apply(ggc_all[,,1:51,,],MARGIN = c(1,2,4,5),FUN = mean,na.rm = TRUE)
ggc_agg[,,,2,] <- apply(ggc_all[,,52:104,,],MARGIN = c(1,2,4,5),FUN = mean,na.rm = TRUE)

for(ch in 1:6){
  ggc_agg[ch,ch,,,] <- NA
}

##### Aggregating results into the two frequency bands of interests
dta.w <- 1:60 # frequency delta-theta-alpha
bg.w <- 61:225 # beta-gamma

qt.level <- 0.80 # quantile level
perc.sig <- 0.10 # significance threshold

### Defining prominent connections
ggc_qt <- apply(ggc_agg,MARGIN = c(3,4,5),FUN = stats::quantile,probs = qt.level,na.rm = TRUE)
ggc_prom <- array(NA,dim = dim(ggc_agg))
for(i in 1:6){
  for(j in 1:6){
    if(i == j){next}
    ggc_prom[i,j,,,] <- ggc_agg[i,j,,,] > ggc_qt
  }
}

### Defining consitent connections
conn_tw <- array(NA,c(6,6,2,2,17)) # ch1,ch2,freq,group,tw
conn_tw[,,1,,] <- apply(ggc_prom[,,dta.w,,],MARGIN = c(1,2,4,5),
                        FUN = sum,na.rm = TRUE) >= length(dta.w)*perc.sig
conn_tw[,,2,,] <- apply(ggc_prom[,,bg.w,,],MARGIN = c(1,2,4,5),FUN = sum,na.rm = TRUE) >= length(bg.w)*perc.sig
conn_agg <- apply(conn_tw,MARGIN = c(1,2,3,4),FUN = mean,na.rm = TRUE)

### Delta-Theta-Alpha Connectivity
par(mfrow = c(1,1))
eegcap(electrodes = myelectrodes,cex.point = 7,col.point = "#BEBEBECC",
       cex.label = 1.5,col.label = "black")
mtext("Delta-Theta-Alpha : ADHD",side = 3,cex=2)
add_arrows(conn_agg[,,1,1],lwd = 4,spc = 1.5,adj = 0.5,
           arr.length = 0.2,cut_off = c(0.5,0.5,0.75))

eegcap(electrodes = myelectrodes,cex.point = 7,col.point = "#BEBEBECC",
       cex.label = 1.5,col.label = "black")
mtext("Delta-Theta-Alpha : Control",side = 3,cex=2)
add_arrows(conn_agg[,,1,2],lwd = 4,spc = 1.5,adj = 0.5,
           arr.length = 0.2,cut_off = c(0.5,0.5,0.75))

### Beta-Gamma Connectivity
eegcap(electrodes = myelectrodes,cex.point = 7,col.point = "#BEBEBECC",
       cex.label = 1.5,col.label = "black")
mtext("Beta-Gamma : ADHD",side = 3,cex=2)
add_arrows(conn_agg[,,2,1],lwd = 4,spc = 1.5,adj = 0.5,
           arr.length = 0.2,cut_off = c(0.5,0.5,0.75))

eegcap(electrodes = myelectrodes,cex.point = 7,col.point = "#BEBEBECC",
       cex.label = 1.5,col.label = "black")
mtext("Beta-Gamma : Control",side = 3,cex=2)
add_arrows(conn_agg[,,2,2],lwd = 4,spc = 1.5,adj = 0.5,
           arr.length = 0.2,cut_off = c(0.5,0.5,0.75))


#################################################################################
##### Illustration for the fPDC Metric - Example 1 (Sigmoidal Coefficients) #####
#################################################################################

##### Functional coefficients
fc11 <- function(x){1.4*((exp(15*(x-1.5)))/(1+exp(15*(x-1.5))))-0.7}
fc12 <- function(x){1.1*((exp(4*(x-1.5)))/(1+exp(4*(x-1.5))))-0.5}
fc21 <- function(x){-1.2*((exp(5*(x-1.5)))/(1+exp(5*(x-1.5))))+0.55}
fc22 <- function(x){-1.5*((exp(15*(x-1.5)))/(1+exp(15*(x-1.5))))+0.75}

### Functional coefficients evaluated at possible values of the reference signal
u <- seq(0,3,length = 100) # values of reference signal
A_u <- array(NA,dim = c(2,2,length(u)))
A_u[1,1,] <- fc11(u)
A_u[1,2,] <- fc12(u)
A_u[2,1,] <- fc21(u)
A_u[2,2,] <- fc22(u)

k <- 2 # dimension k
p <- 1 # number of lag L
freq <- seq(0,0.5,by = 0.005) # frequencies

### Calculating the fPDC metric (true value based on actual functional coefficients)
fpdc_all <- array(NA,dim=c(k,k,length(freq),length(u)))

for(h in 1:length(u)){
  A <- A_u[,,h]  
  for(g in 1:length(freq)){
    f <- freq[g]
    A_f <- matrix(0,nrow = k,ncol = k)
    A_f_all <- matrix(rep(exp(-2i*pi*f*(1:p)),each = k^2),nrow = k)*A
    for(l in 1:p){
      A_f <- A_f + A_f_all[,k*(l-1)+1:k]
    }
    A_bar <- diag(rep(1,k)) -  A_f
    
    for(i in 1:k){
      for(j in 1:k){
        fpdc_all[i,j,g,h] <- Mod(A_bar[i,j])/sqrt(sum(Mod(A_bar[,j])^2))
      }
    }
  }
}

### Information flow Y2 to Y1 via the fPDC metric
z <- fpdc_all[1,2,,] 
data_melt <- NULL
for(h in 1:length(u)){
  for(g in 1:length(freq)){
    entry <- data.frame(freq = freq[g],u = u[h],fPDC = z[g,h])
    data_melt <- rbind(data_melt,entry)
  }
}

### Plotting results for fPDC from Y2 to Y1
ggp1 <- ggplot(data_melt, aes(freq, u)) +  xlab(TeX("Frequency $\\omega$",bold = TRUE)) + 
  ylab(TeX("Reference Signal $u$",bold = TRUE)) + 
  geom_tile(aes(fill = fPDC)) + theme_classic(base_line_size = 0) +
  scale_fill_gradientn(colours = c(col.pal1(100),col.pal2(100)),limits=c(0,1),
                       name = "fPDC / \n PDC") +
  theme_setting + 
  geom_hline(yintercept=0.5, linetype="dashed", color = "black",lwd=1) +
  geom_hline(yintercept=2.5, linetype="dashed", color = "blue",lwd=1)
ggp1

### Information flow Y1 to Y2 via the fPDC metric
z <- fpdc_all[2,1,,]
data_melt <- NULL
for(h in 1:length(u)){
  for(g in 1:length(freq)){
    entry <- data.frame(freq = freq[g],u = u[h],fPDC = z[g,h])
    data_melt <- rbind(data_melt,entry)
  }
}

### Plotting results for fPDC from Y1 to Y2
ggp2 <- ggplot(data_melt, aes(freq, u)) +  xlab(TeX("Frequency $\\omega$",bold = TRUE)) + 
  ylab(TeX("Reference Signal $u$",bold = TRUE)) + 
  geom_tile(aes(fill = fPDC)) + theme_classic(base_line_size = 0) +
  scale_fill_gradientn(colours = c(col.pal1(100),col.pal2(100)),limits=c(0,1),
                       name = "fPDC / \n PDC") +
  theme_setting + 
  geom_hline(yintercept=0.5, linetype="dashed", color = "black",lwd=1) +
  geom_hline(yintercept=2.5, linetype="dashed", color = "blue",lwd=1)
ggp2


##### Estimating linear PDC when nonlinearity is present
Fmat = function(x,r){
  f11 <- 1.4*((exp((15 + r[1])*(x-1.5)))/(1+exp((15 + r[1])*(x-1.5))))-0.7
  f12 <- 1.1*((exp((4 + r[2])*(x-1.5)))/(1+exp((4 + r[2])*(x-1.5))))-0.5
  f21 <- -1.2*((exp((5 + r[3])*(x-1.5)))/(1+exp((5 + r[3])*(x-1.5))))+0.55
  f22 <- -1.5*((exp((15 + r[4])*(x-1.5)))/(1+exp((15 + r[4])*(x-1.5))))+0.75
  return(list(matrix(c(f11,f21,f12,f22),ncol=2)))
}

### Fitting a linear VAR model on data simulated from a FAR process
set.seed(1)
A_all <- array(NA,dim = c(2,2,500))
for(j in 1:500){
  samp <- FAR.sim(Tlength=5000,d=2,Y_d=0,Fmat,reff = rep(0,4),eps.Sigma = rep(1,2))
  var_est <- VAR(samp$y,p = 1,type = "none")
  A_all[1,,j] <- var_est$varresult$y1$coefficients
  A_all[2,,j] <- var_est$varresult$y2$coefficients
  cat(paste("+ j = ",j,sep=""),"\r")
}

### Aggregating across all replicates
A <- apply(A_all,MARGIN = c(1,2),FUN = mean,na.rm = TRUE)
pdc_all <- array(NA,dim=c(k,k,length(freq),length(u)))
for(h in 1:length(u)){
  for(g in 1:length(freq)){
    f <- freq[g]
    A_f <- matrix(0,nrow = k,ncol = k)
    A_f_all <- matrix(rep(exp(-2i*pi*f*(1:p)),each = k^2),nrow = k)*A
    for(l in 1:p){
      A_f <- A_f + A_f_all[,k*(l-1)+1:k]
    }
    A_bar <- diag(rep(1,k)) -  A_f
    
    for(i in 1:k){
      for(j in 1:k){
        pdc_all[i,j,g,h] <- Mod(A_bar[i,j])/sqrt(sum(Mod(A_bar[,j])^2))
      }
    }
  }
  
}

### Information flow Y2 to Y1 via the linear PDC metric
z <- pdc_all[1,2,,]
data_melt <- NULL
for(h in 1:length(u)){
  for(g in 1:length(freq)){
    entry <- data.frame(freq = freq[g],u = u[h],fPDC = z[g,h])
    data_melt <- rbind(data_melt,entry)
  }
}

### Plotting results for linear PDC from Y2 to Y1
ggp3 <- ggplot(data_melt, aes(freq, u)) +  xlab(TeX("Frequency $\\omega$",bold = TRUE)) +
  ylab(TeX("Reference Signal $u$",bold = TRUE)) +
  geom_tile(aes(fill = fPDC)) + theme_classic(base_line_size = 0) +
  scale_fill_gradientn(colours = c(col.pal1(100),col.pal2(100)),limits=c(0,1),
                       name = "fPDC / \n PDC") + theme_setting
ggp3

### Information flow Y1 to Y2 via the linear PDC metric
z <- pdc_all[2,1,,]
data_melt <- NULL
for(h in 1:length(u)){
  for(g in 1:length(freq)){
    entry <- data.frame(freq = freq[g],u = u[h],fPDC = z[g,h])
    data_melt <- rbind(data_melt,entry)
  }
}

### Plotting results for linear PDC from Y1 to Y2
ggp4 <- ggplot(data_melt, aes(freq, u)) +  xlab(TeX("Frequency $\\omega$",bold = TRUE)) +
  ylab(TeX("Reference Signal $u$",bold = TRUE)) +
  geom_tile(aes(fill = fPDC)) + theme_classic(base_line_size = 0) +
  scale_fill_gradientn(colours = c(col.pal1(100),col.pal2(100)),limits=c(0,1),
                       name = "fPDC / \n PDC") + theme_setting
ggp4

##### Comparing fPDC and linear PDC values from Y1 to Y2 at specific level of the reference signal u
data_fdpc_low <- data.frame(freq = freq,value = fpdc_all[1,2,,18],group = "fPDC(u=0.5)")
data_fdpc_high <- data.frame(freq = freq,value = fpdc_all[1,2,,83],group = "fPDC(u=2.5)")
data_pdc <- data.frame(freq = freq,value = pdc_all[1,2,,1],group = "PDC")
data_line <- rbind(data_fdpc_high,data_fdpc_low,data_pdc)

### Plotting results
ggp5 <- ggplot(data_line, aes(x = freq, y = value, group = group)) +
  geom_line(aes(color = group,linetype=group),lwd = 1.5) +
  xlab(TeX("Frequency $\\omega$",bold = TRUE)) +
  ylab(TeX("Intensity",bold = TRUE)) +
  scale_color_manual(values = c("black", "blue", "red"),
                     labels = c(TeX("\\textit{f}PDC$(\\omega,u = 0.5)$"),
                                TeX("\\textit{f}PDC$(\\omega,u = 2.5)$"),
                                TeX("PDC$(\\omega)$")),name = "") +
  scale_linetype_manual(values=c("dashed","dashed","solid"),
                        labels = c(TeX("\\textit{f}PDC$(\\omega,u = 0.5)$"),
                                   TeX("\\textit{f}PDC$(\\omega,u = 2.5)$"),
                                   TeX("PDC$(\\omega)$")),name = "") +
  theme_classic(base_line_size = 1.25) + theme_setting2
ggp5

##### Comparing fPDC and linear PDC values from Y1 to Y2 at specific level of the reference signal u
data_fdpc_low <- data.frame(freq = freq,value = fpdc_all[2,1,,18],group = "fPDC(u=0.5)")
data_fdpc_high <- data.frame(freq = freq,value = fpdc_all[2,1,,83],group = "fPDC(u=2.5)")
data_pdc <- data.frame(freq = freq,value = pdc_all[2,1,,1],group = "PDC")
data_line <- rbind(data_fdpc_high,data_fdpc_low,data_pdc)

### Plotting results
ggp6 <- ggplot(data_line, aes(x = freq, y = value, group = group)) +
  geom_line(aes(color = group,linetype=group),lwd = 1.5) +
  xlab(TeX("Frequency $\\omega$",bold = TRUE)) +
  ylab(TeX("Intensity",bold = TRUE)) +
  scale_color_manual(values = c("black", "blue", "red"),
                     labels = c(TeX("\\textit{f}PDC$(\\omega,u = 0.5)$"),
                                TeX("\\textit{f}PDC$(\\omega,u = 2.5)$"),
                                TeX("PDC$(\\omega)$")),name = "") +
  scale_linetype_manual(values=c("dashed","dashed","solid"),
                        labels = c(TeX("\\textit{f}PDC$(\\omega,u = 0.5)$"),
                                   TeX("\\textit{f}PDC$(\\omega,u = 2.5)$"),
                                   TeX("PDC$(\\omega)$")),name = "") +
  theme_classic(base_line_size = 1.25) + theme_setting2
ggp6



###################################################################################
##### Illustration for the fPDC Metric - Example 2 (Exponential Coefficients) #####
###################################################################################

##### Functional coefficients
fc11 <- function(x){-0.7}
fc12 <- function(x){0.85*exp(-5*x^2)}
fc21 <- function(x){-0.60*exp(-4*x^2)}
fc22 <- function(x){0.75}

### Functional coefficients evaluated at possible values of the reference signal
u <- seq(-1.5,1.5,length = 100) # values of reference signal
A_u <- array(NA,dim = c(2,2,length(u)))
A_u[1,1,] <- fc11(u)
A_u[1,2,] <- fc12(u)
A_u[2,1,] <- fc21(u)
A_u[2,2,] <- fc22(u)

k <- 2 # dimension k
p <- 1 # number of lag L
freq <- seq(0,0.5,by = 0.005) # frequencies

### Calculating the fPDC metric (true value based on actual functional coefficients)
fpdc_all <- array(NA,dim=c(k,k,length(freq),length(u)))

for(h in 1:length(u)){
  A <- A_u[,,h]  
  for(g in 1:length(freq)){
    f <- freq[g]
    A_f <- matrix(0,nrow = k,ncol = k)
    A_f_all <- matrix(rep(exp(-2i*pi*f*(1:p)),each = k^2),nrow = k)*A
    for(l in 1:p){
      A_f <- A_f + A_f_all[,k*(l-1)+1:k]
    }
    A_bar <- diag(rep(1,k)) -  A_f
    
    for(i in 1:k){
      for(j in 1:k){
        fpdc_all[i,j,g,h] <- Mod(A_bar[i,j])/sqrt(sum(Mod(A_bar[,j])^2))
      }
    }
  }
}

### Information flow Y2 to Y1 via the fPDC metric
z <- fpdc_all[1,2,,] 
data_melt <- NULL
for(h in 1:length(u)){
  for(g in 1:length(freq)){
    entry <- data.frame(freq = freq[g],u = u[h],fPDC = z[g,h])
    data_melt <- rbind(data_melt,entry)
  }
}

### Plotting results for fPDC from Y2 to Y1
ggp1 <- ggplot(data_melt, aes(freq, u)) +  xlab(TeX("Frequency $\\omega$",bold = TRUE)) + 
  ylab(TeX("Reference Signal $u$",bold = TRUE)) + 
  geom_tile(aes(fill = fPDC)) + theme_classic(base_line_size = 0) +
  scale_fill_gradientn(colours = c(col.pal1(100),col.pal2(100)),limits=c(0,1),
                       name = "fPDC / \n PDC") +
  theme_setting + 
  geom_hline(yintercept=-0.1, linetype="dashed", color = "black",lwd=1) +
  geom_hline(yintercept=0.15, linetype="dashed", color = "blue",lwd=1)
ggp1

### Information flow Y1 to Y2 via the fPDC metric
z <- fpdc_all[2,1,,]
data_melt <- NULL
for(h in 1:length(u)){
  for(g in 1:length(freq)){
    entry <- data.frame(freq = freq[g],u = u[h],fPDC = z[g,h])
    data_melt <- rbind(data_melt,entry)
  }
}

### Plotting results for fPDC from Y1 to Y2
ggp2 <- ggplot(data_melt, aes(freq, u)) +  xlab(TeX("Frequency $\\omega$",bold = TRUE)) + 
  ylab(TeX("Reference Signal $u$",bold = TRUE)) + 
  geom_tile(aes(fill = fPDC)) + theme_classic(base_line_size = 0) +
  scale_fill_gradientn(colours = c(col.pal1(100),col.pal2(100)),limits=c(0,1),
                       name = "fPDC / \n PDC") +
  theme_setting + 
  geom_hline(yintercept=-0.1, linetype="dashed", color = "black",lwd=1) +
  geom_hline(yintercept=0.15, linetype="dashed", color = "blue",lwd=1)
ggp2


##### Estimating linear PDC when nonlinearity is present
Fmat = function(x,r){
  f11 <- -0.7 + r[1]
  f12 <- 0.85*exp(-(5 + r[2])*x^2)
  f21 <- -0.60*exp(-(4 + r[3])*x^2)
  f22 <- 0.75 + r[4]
  return(list(matrix(c(f11,f21,f12,f22),ncol=2)))
}

### Fitting a linear VAR model on data simulated from a FAR process
set.seed(1)
A_all <- array(NA,dim = c(2,2,500))
for(j in 1:500){
  samp <- FAR.sim(Tlength=5000,d=2,Y_d=0,Fmat,reff = rep(0,4),eps.Sigma = rep(1,2))
  var_est <- VAR(samp$y,p = 1,type = "none")
  A_all[1,,j] <- var_est$varresult$y1$coefficients
  A_all[2,,j] <- var_est$varresult$y2$coefficients
  cat(paste("+ j = ",j,sep=""),"\r")
}

### Aggregating across all replicates
A <- apply(A_all,MARGIN = c(1,2),FUN = mean,na.rm = TRUE)
pdc_all <- array(NA,dim=c(k,k,length(freq),length(u)))
for(h in 1:length(u)){
  for(g in 1:length(freq)){
    f <- freq[g]
    A_f <- matrix(0,nrow = k,ncol = k)
    A_f_all <- matrix(rep(exp(-2i*pi*f*(1:p)),each = k^2),nrow = k)*A
    for(l in 1:p){
      A_f <- A_f + A_f_all[,k*(l-1)+1:k]
    }
    A_bar <- diag(rep(1,k)) -  A_f
    
    for(i in 1:k){
      for(j in 1:k){
        pdc_all[i,j,g,h] <- Mod(A_bar[i,j])/sqrt(sum(Mod(A_bar[,j])^2))
      }
    }
  }
  
}

### Information flow Y2 to Y1 via the linear PDC metric
z <- pdc_all[1,2,,]
data_melt <- NULL
for(h in 1:length(u)){
  for(g in 1:length(freq)){
    entry <- data.frame(freq = freq[g],u = u[h],fPDC = z[g,h])
    data_melt <- rbind(data_melt,entry)
  }
}

### Plotting results for linear PDC from Y2 to Y1
ggp3 <- ggplot(data_melt, aes(freq, u)) +  xlab(TeX("Frequency $\\omega$",bold = TRUE)) +
  ylab(TeX("Reference Signal $u$",bold = TRUE)) +
  geom_tile(aes(fill = fPDC)) + theme_classic(base_line_size = 0) +
  scale_fill_gradientn(colours = c(col.pal1(100),col.pal2(100)),limits=c(0,1),
                       name = "fPDC / \n PDC") + theme_setting
ggp3

### Information flow Y1 to Y2 via the linear PDC metric
z <- pdc_all[2,1,,]
data_melt <- NULL
for(h in 1:length(u)){
  for(g in 1:length(freq)){
    entry <- data.frame(freq = freq[g],u = u[h],fPDC = z[g,h])
    data_melt <- rbind(data_melt,entry)
  }
}

### Plotting results for linear PDC from Y1 to Y2
ggp4 <- ggplot(data_melt, aes(freq, u)) +  xlab(TeX("Frequency $\\omega$",bold = TRUE)) +
  ylab(TeX("Reference Signal $u$",bold = TRUE)) +
  geom_tile(aes(fill = fPDC)) + theme_classic(base_line_size = 0) +
  scale_fill_gradientn(colours = c(col.pal1(100),col.pal2(100)),limits=c(0,1),
                       name = "fPDC / \n PDC") + theme_setting
ggp4

##### Comparing fPDC and linear PDC values from Y1 to Y2 at specific level of the reference signal u
data_fdpc_low <- data.frame(freq = freq,value = fpdc_all[1,2,,47],group = "fPDC(u=-0.1)")
data_fdpc_high <- data.frame(freq = freq,value = fpdc_all[1,2,,56],group = "fPDC(u=0.15)")
data_pdc <- data.frame(freq = freq,value = pdc_all[1,2,,1],group = "PDC")
data_line <- rbind(data_fdpc_high,data_fdpc_low,data_pdc)

### Plotting results
ggp5 <- ggplot(data_line, aes(x = freq, y = value, group = group)) +
  geom_line(aes(color = group,linetype=group),lwd = 1.5) +
  xlab(TeX("Frequency $\\omega$",bold = TRUE)) +
  ylab(TeX("Intensity",bold = TRUE)) +
  scale_color_manual(values = c("black", "blue", "red"),
                     labels = c(TeX("\\textit{f}PDC$(\\omega,u = -0.1)$"),
                                TeX("\\textit{f}PDC$(\\omega,u = 0.15)$"),
                                TeX("PDC$(\\omega)$")),name = "") +
  scale_linetype_manual(values=c("dashed","dashed","solid"),
                        labels = c(TeX("\\textit{f}PDC$(\\omega,u = -0.1)$"),
                                   TeX("\\textit{f}PDC$(\\omega,u = 0.15)$"),
                                   TeX("PDC$(\\omega)$")),name = "") +
  theme_classic(base_line_size = 1.25) + theme_setting2
ggp5

##### Comparing fPDC and linear PDC values from Y1 to Y2 at specific level of the reference signal u
data_fdpc_low <- data.frame(freq = freq,value = fpdc_all[2,1,,47],group = "fPDC(u=-0.1)")
data_fdpc_high <- data.frame(freq = freq,value = fpdc_all[2,1,,56],group = "fPDC(u=0.15)")
data_pdc <- data.frame(freq = freq,value = pdc_all[2,1,,1],group = "PDC")
data_line <- rbind(data_fdpc_high,data_fdpc_low,data_pdc)

### Plotting results
ggp6 <- ggplot(data_line, aes(x = freq, y = value, group = group)) +
  geom_line(aes(color = group,linetype=group),lwd = 1.5) +
  xlab(TeX("Frequency $\\omega$",bold = TRUE)) +
  ylab(TeX("Intensity",bold = TRUE)) +
  scale_color_manual(values = c("black", "blue", "red"),
                     labels = c(TeX("\\textit{f}PDC$(\\omega,u = -0.1)$"),
                                TeX("\\textit{f}PDC$(\\omega,u = 0.15)$"),
                                TeX("PDC$(\\omega)$")),name = "") +
  scale_linetype_manual(values=c("dashed","dashed","solid"),
                        labels = c(TeX("\\textit{f}PDC$(\\omega,u = -0.1)$"),
                                   TeX("\\textit{f}PDC$(\\omega,u = 0.15)$"),
                                   TeX("PDC$(\\omega)$")),name = "") +
  theme_classic(base_line_size = 1.25) + theme_setting2
ggp6




####################################################################
##### Run Times of the Local Linear Estimation for MXFAR model #####
####################################################################


##### Simulation settings
N_all <- c(10,20,50,100) # number of series/subjects
Tlength_all <- c(500,1000) # time series length

##### Dimension k = 2 MXFAR process with exponential functional coefficients
Fmat = function(x,r){
  f11 <- -0.3 + r[1]
  f12 <- 0.6*exp(-(0.30 + r[2])*x^2)
  f21 <- -0.2 + r[3]
  f22 <- -0.4*exp(-(0.45 + r[4])*x^2)
  return(list(matrix(c(f11,f21,f12,f22),ncol=2)))
}

runtime_k2 <- array(NA,dim = c(200,2,4)) # compiling results
for(r in 1:10){
  for(n in 1:4){
    for(t in 1:2){
      Mydata <- MXFAR.sim(SeriesNum = N_all[n],Tlength = Tlength_all[t],d = 2,Y_d = 0,Fmat,
                          reff.Sigma = rep(0.05,4),eps.Sigma = rep(1,2))
      start <- Sys.time()
      est <- MXFAR.est(SeriesNum = N_all[n],Tlength = Tlength_all[t],y = Mydata[[1]],u = Mydata[[2]],
                       p = 1,d = 2,bwp = 0.1)
      end <- Sys.time()
      runtime_k2[r,t,n] <- as.vector(end - start)
      cat(paste("+ Runtime: r = ",r,", t = ",t,", n = ",n,sep = ""),"\r")
      saveRDS(runtime_k2,"runtime_k2.rds")
    }
  }
}

##### Dimension k = 6 MXFAR process with sigmoidal functional coefficients
Fmat = function(x,r){
  
  A1 <- matrix(c(0.2,0,0,0,0,0,
                 0,0,0,0,0,0,
                 0,0.25,0,0,0,0,
                 0,0,0,0,0,0,
                 0,0,0,0,0,0,
                 0,0,0,0,0.15,0),
               ncol = 6,nrow = 6,byrow = TRUE)
  
  A2 <- matrix(c(0,0,0,0,0,0,
                 0,0.3,0,0,0,0,
                 0,0,0,0,0,0,
                 0,0,0,0,0,0.15,
                 0,0,-0.25,0,0,0,
                 0,0,0,0,0,0),
               ncol = 6,nrow = 6,byrow = TRUE)
  
  A3 <- matrix(c(0,-0.4,0,0,0,0,
                 0.5,0,0,0,0,0,
                 0,0,0,0,0.6,0,
                 0,0,0,0,0,0,
                 0,0,0.5,0,0,0,
                 0,0,0,0,-0.35,0),
               ncol = 6,nrow = 6,byrow = TRUE)
  
  A4 <- matrix(c(0,-0.45,0,0,0,0,
                 0,0,-0.5,0,0,0,
                 0,0,0.65,0,0,0,
                 0,0,0,0.3,0,0,
                 0.6,0,0,0,0,0,
                 0,0,0,0,0,0.3),
               ncol = 6,nrow = 6,byrow = TRUE)
  
  A1[2,4] <- 1.1*((exp((5 + r[1])*x))/(1+exp((5 + r[1])*x))) - 0.5
  A1[5,1] <- 0 + r[2]
  A2[1,2] <- 1.25*((exp((6 + r[3])*x))/(1+exp((6 + r[3])*x))) - 0.6
  A2[4,5] <- 0 + r[4]
  A3[3,5] <- -0.8*((exp((7 + r[5])*x))/(1+exp((7 + r[5])*x))) + 0.4
  A4[6,2] <- 0.9*((exp((5 + r[6])*x))/(1+exp((5 + r[6])*x))) - 0.4
  
  return(list(A1,A2,A3,A4))
}

runtime_k6 <- array(NA,dim = c(200,2,4)) # compiling results
for(r in 1:10){
  for(n in 1:4){
    for(t in 1:2){
      Mydata <- MXFAR.sim(SeriesNum = N_all[n],Tlength = Tlength_all[t],d = 6,Y_d = 0,Fmat,
                          reff.Sigma = rep(0.05,6),eps.Sigma = rep(1,6))
      start <- Sys.time()
      est <- MXFAR.est(SeriesNum = N_all[n],Tlength = Tlength_all[t],y = Mydata[[1]],u = Mydata[[2]],
                       p = 4,d = 6,bwp = 0.1)
      end <- Sys.time()
      runtime_k6[r,t,n] <- as.vector(end - start)
      cat(paste("+ Runtime: r = ",r,", t = ",t,", n = ",n,sep = ""),"\r")
      saveRDS(runtime_k6,"runtime_k6.rds")
    }
  }
}

##### Summarizing results
# runtime_k2 <- readRDS("Data and Results/runtime_k2.rds") # loading results
runtime_k2_agg <- apply(runtime_k2,MARGIN = c(2,3),FUN = max,na.rm = TRUE)
runtime_k2_agg[2,4] <- runtime_k2_agg[2,4]*60

# runtime_k6 <- readRDS("Data and Results/runtime_k6.rds") # loading results
runtime_k6_agg <- apply(runtime_k6,MARGIN = c(2,3),FUN = max,na.rm = TRUE)
runtime_k6_agg[2,3:4] <- runtime_k6_agg[2,3:4]*60

##### Plotting results
par(mar = c(5,6,1,1))
plot(N_all,runtime_k2_agg[1,],type = "b",pch = 19,ylim = c(0,200),cex = 1.5,lwd = 4,
     ylab = "",xlab = "",xaxt = "none",cex.axis = 2,cex.lab = 2.5)
mtext("Run Times (in seconds)",side = 2,cex = 2.5,line = 3.5)
mtext("Number of Individual Series (N)",side = 1,cex = 2.5,line = 3.5)
axis(side = 1,at = N_all,labels = N_all,lwd = 3,cex.axis = 2)
lines(N_all,runtime_k2_agg[2,],type = "b",pch = 19,col = "red",cex = 1.5,lwd = 4)
lines(N_all,runtime_k6_agg[1,],type = "b",pch = 19,col = "green",cex = 1.5,lwd = 4)
lines(N_all,runtime_k6_agg[2,],type = "b",pch = 19,col = "blue",cex = 1.5,lwd = 4)
legend(x = 7,y = 205,legend = c("k = 2, p = 1, T = 500", "k = 2, p = 1, T = 1000",
                                "k = 6, p = 4, T = 500", "k = 6, p = 4, T = 1000"),
       col = c("black","red","green","blue"),lty = 1,pch = 19,cex = 2)
box(lwd = 5)



#######################################
##### Example of Simulation Study #####
#######################################

### Exponential Functional Coefficients ###
###     Dimension k = 2, lag p = 1      ###

### Functional coefficients
Fmat = function(x,r){
  f11 <- -0.3 + r[1]
  f12 <- 0.6*exp(-(0.30 + r[2])*x^2)
  f21 <- -0.2 + r[3]
  f22 <- -0.4*exp(-(0.45 + r[4])*x^2)
  return(list(matrix(c(f11,f21,f12,f22),ncol=2)))
}

### Simulating from a bivariate MXFAR(p=1) process with exogenous reference signal (Y_d = 0)
set.seed(1)
Mydata <- MXFAR.sim(SeriesNum = 10,Tlength = 500,d = 2,Y_d = 0,Fmat,
                    reff.Sigma = rep(0.05,4),eps.Sigma = rep(1,2))

### Estimating the MXFAR model
est <- MXFAR.est(SeriesNum = 10,Tlength = 500,y = Mydata[[1]],u = Mydata[[2]],
                 p = 1,d = 2,bwp = 0.1)

### Plotting estimated vs actual functional coefficients
f11 <- function(x){rep(-0.3,length(x))} 
f12 <- function(x){0.6*exp(-0.30*x^2)}
f21 <- function(x){rep(-0.2,length(x))}
f22 <- function(x){-0.4*exp(-0.45*x^2)}

par(mfrow = c(2,2),mar = c(2,5,2,1))
plot(est$fhat_points,est$fhat_mean[1,1,,1],type = "l",ylim = c(-0.5,-0.1),ylab = "f11")
lines(est$fhat_points,f11(est$fhat_points),type = "l",col = "red")
plot(est$fhat_points,est$fhat_mean[1,2,,1],type = "l",ylim = c(0.25,0.65),ylab = "f12")
lines(est$fhat_points,f12(est$fhat_points),type = "l",col = "red")
plot(est$fhat_points,est$fhat_mean[2,1,,1],type = "l",ylim = c(-0.4,0.1),ylab = "f21")
lines(est$fhat_points,f21(est$fhat_points),type = "l",col = "red")
plot(est$fhat_points,est$fhat_mean[2,2,,1],type = "l",ylim = c(-0.5,0.1),ylab = "f22")
lines(est$fhat_points,f22(est$fhat_points),type = "l",col = "red")

### Model selection via the APE criterion
max_p <- 3
max_d <- 5
ape_all <- NULL

for(ref in 0:2){ # ref = 0 indicates an exogenous reference signal
  for(l in 1:max_p){
    for(j in (l+1):max_d){
      if(ref == 0){
        entry <- data.frame(p=l,d=j,ref=ref,
                            APE=APE(SeriesNum = 10,Tlength = 500,y = Mydata[[1]],
                                    u = Mydata[[2]],p = l,d = j,bwp = 0.2,r = 50,Q = 4,numpoints = 50))
      } else {
        entry <- data.frame(p=l,d=j,ref=ref,
                            APE=APE(SeriesNum = 10,Tlength = 500,y = Mydata[[1]],
                                    u = Mydata[[1]][,ref],p = l,d = j,bwp = 0.2,r = 50,Q = 4,numpoints = 50))
      }
      
      cat(paste("DONE: p = ",l," d = ",j," ref = ",ref,sep = "")," \r")
      ape_all <- rbind(ape_all,entry)
    }
  }
}

### Obtaining the model specification with the smallest APE value
ape_sort <- ape_all[order(ape_all$APE),]
head(ape_sort) # the best model is given by p = 1, d = 2, ref = 0 (exogenous) which is the true model

### Testing for presence of nonlinearity in the time series
set.seed(2)
nltest_res <- NLTest(SeriesNum = 10,Tlength = 500,y = Mydata[[1]],
                     u = Mydata[[2]],p = 1,d = 2,bwp = 0.2,numpoints = 50,maxboot = 200)
nltest_res$pval #pvalue < 0.05 which implies nonlinearity is present


### Sigmoidal Functional Coefficients ###
###    Dimension k = 6, lag p = 4     ###

### Functional coefficients
Fmat = function(x,r){
  
  A1 <- matrix(c(0.2,0,0,0,0,0,
                 0,0,0,0,0,0,
                 0,0.25,0,0,0,0,
                 0,0,0,0,0,0,
                 0,0,0,0,0,0,
                 0,0,0,0,0.15,0),
               ncol = 6,nrow = 6,byrow = TRUE)
  
  A2 <- matrix(c(0,0,0,0,0,0,
                 0,0.3,0,0,0,0,
                 0,0,0,0,0,0,
                 0,0,0,0,0,0.15,
                 0,0,-0.25,0,0,0,
                 0,0,0,0,0,0),
               ncol = 6,nrow = 6,byrow = TRUE)
  
  A3 <- matrix(c(0,-0.4,0,0,0,0,
                 0.5,0,0,0,0,0,
                 0,0,0,0,0.6,0,
                 0,0,0,0,0,0,
                 0,0,0.5,0,0,0,
                 0,0,0,0,-0.35,0),
               ncol = 6,nrow = 6,byrow = TRUE)
  
  A4 <- matrix(c(0,-0.45,0,0,0,0,
                 0,0,-0.5,0,0,0,
                 0,0,0.65,0,0,0,
                 0,0,0,0.3,0,0,
                 0.6,0,0,0,0,0,
                 0,0,0,0,0,0.3),
               ncol = 6,nrow = 6,byrow = TRUE)
  
  A1[2,4] <- 1.1*((exp((5 + r[1])*x))/(1+exp((5 + r[1])*x))) - 0.5
  A1[5,1] <- -0.9*((exp((7 + r[2])*x))/(1+exp((7 + r[2])*x))) + 0.5
  A2[1,2] <- 1.25*((exp((6 + r[3])*x))/(1+exp((6 + r[3])*x))) - 0.6
  A2[4,5] <- 0.7*((exp((3 + r[4])*x))/(1+exp((3 + r[4])*x))) - 0.3
  A3[3,5] <- -0.8*((exp((7 + r[5])*x))/(1+exp((7 + r[5])*x))) + 0.4
  A4[6,2] <- 0.9*((exp((5 + r[6])*x))/(1+exp((5 + r[6])*x))) - 0.4
  
  return(list(A1,A2,A3,A4))
}


### Simulating from a "k=6"-dimensional MXFAR(p=6) process with endogenous reference signal (Y_d = 1)
set.seed(3)
Mydata <- MXFAR.sim(SeriesNum = 10,Tlength = 500,d = 6,Y_d = 1,Fmat,
                    reff.Sigma = rep(0.03,6),eps.Sigma = rep(1,6))

### Estimating the MXFAR model
est <- MXFAR.est(SeriesNum = 10,Tlength = 500,y = Mydata[[1]],u = Mydata[[2]],
                 p = 4,d = 6,bwp = 0.1)

### Plotting estimated vs actual functional coefficients
f24_1 <- function(x){1.1*((exp(5*x))/(1+exp(5*x))) - 0.5}
f51_1 <- function(x){-0.9*((exp(7*x))/(1+exp(7*x))) + 0.5}
f12_2 <- function(x){1.25*((exp(6*x))/(1+exp(6*x))) - 0.6}
f45_2 <- function(x){0.7*((exp(3*x))/(1+exp(3*x))) - 0.3}
f35_3 <- function(x){-0.8*((exp(7*x))/(1+exp(7*x))) + 0.4}
f62_4 <- function(x){0.9*((exp(5*x))/(1+exp(5*x))) - 0.4}

par(mfrow = c(3,2),mar = c(2,5,2,1))
i <- 2; j <- 4; l <- 1 
plot(est$fhat_points,est$fhat_mean[i,6*(l-1)+j,,1],type = "l",ylim = c(-0.95,0.95),ylab = "f24_1")
lines(est$fhat_points,f24_1(est$fhat_points),type = "l",col = "red")
i <- 5; j <- 1; l <- 1 
plot(est$fhat_points,est$fhat_mean[i,6*(l-1)+j,,1],type = "l",ylim = c(-0.95,0.95),ylab = "f51_1")
lines(est$fhat_points,f51_1(est$fhat_points),type = "l",col = "red")
i <- 1; j <- 2; l <- 2 
plot(est$fhat_points,est$fhat_mean[i,6*(l-1)+j,,1],type = "l",ylim = c(-0.95,0.95),ylab = "f12_2")
lines(est$fhat_points,f12_2(est$fhat_points),type = "l",col = "red")
i <- 4; j <- 5; l <- 2 
plot(est$fhat_points,est$fhat_mean[i,6*(l-1)+j,,1],type = "l",ylim = c(-0.95,0.95),ylab = "f45_2")
lines(est$fhat_points,f45_2(est$fhat_points),type = "l",col = "red")
i <- 3; j <- 5; l <- 3 
plot(est$fhat_points,est$fhat_mean[i,6*(l-1)+j,,1],type = "l",ylim = c(-0.95,0.95),ylab = "f35_3")
lines(est$fhat_points,f35_3(est$fhat_points),type = "l",col = "red")
i <- 6; j <- 2; l <- 4 
plot(est$fhat_points,est$fhat_mean[i,6*(l-1)+j,,1],type = "l",ylim = c(-0.95,0.95),ylab = "f62_4")
lines(est$fhat_points,f62_4(est$fhat_points),type = "l",col = "red")


### Model selection via the APE criterion
max_p <- 5
max_d <- 10
ape_all <- NULL

for(ref in 1:6){
  for(l in 1:max_p){
    for(j in (l+1):max_d){
      entry <- data.frame(p=l,d=j,ref=ref,
                          APE=APE(SeriesNum = 10,Tlength = 500,y = Mydata[[1]],
                                  u = Mydata[[1]][,ref],p = l,d = j,bwp = 0.2,r = 50,Q = 4,numpoints = 50))
      cat(paste("DONE: p = ",l," d = ",j," ref = ",ref,sep = "")," \r")
      ape_all <- rbind(ape_all,entry)
    }
  }
}

### Obtaining the model specification with the smallest APE value
ape_sort <- ape_all[order(ape_all$APE),]
head(ape_sort) # the best model is given by p = 4, d = 6, ref = 1 (edogenous) which is the true model

### Testing for presence of nonlinearity in the time series
set.seed(4)
nltest_res <- NLTest(SeriesNum = 10,Tlength = 500,y = Mydata[[1]],
                     u = Mydata[[2]],p = 4,d = 6,bwp = 0.2,numpoints = 50,maxboot = 200)
nltest_res$pval #pvalue < 0.05 which implies nonlinearity is present





