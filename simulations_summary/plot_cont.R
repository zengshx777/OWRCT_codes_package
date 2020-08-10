plot_size=2.5
n_index=1:16
pdf("cont_MCSD.pdf",height=1.3*plot_size,width=4*plot_size)
m <- matrix(c(1,2,3,4,5,5,5,5),nrow = 2,ncol = 4,byrow = TRUE)

layout(mat = m,heights = c(0.5,0.1))
h.degree=0
pt=0.5
mis.specified=0
#s_string="correctly specified"
#if (mis.specified==1){s_string="mispecified"}
# setwd("cont/")
load(paste(h.degree,pt,mis.specified,"cont_result.RData",sep="_"))
#par(mfrow=c(1,4))
plot(MC_SD[n_index,1],(MC_SD[n_index,2]/MC_SD[n_index,3])^2,type='o',
     xlab="Sample size",lwd=1.5,
     ylab="Relative efficiency",
     main="(a)",
     cex.lab=1.4,cex.main=2,
     # main=substitute(paste("Standard deviation, ",r,"=",pt,", ",b[1],"=",h.degree,", ",s_string),
     #                 list(pt=pt,h.degree=h.degree,s_string=s_string)),
     #ylim=range(MC_SD[n_index,2]/MC_SD[n_index,3],MC_SD[n_index,2]/MC_SD[n_index,4],MC_SD[n_index,2]/MC_SD[n_index,5],MC_SD[n_index,2]/MC_SD[n_index,6]),
     ylim=c(0,4),
     col="black",pch=15)
lines(MC_SD[n_index,1],(MC_SD[n_index,2]/MC_SD[n_index,4])^2,type='o',col="red",lwd=1.5,pch=19)
lines(MC_SD[n_index,1],(MC_SD[n_index,2]/MC_SD[n_index,5])^2,type='o',col="purple",lwd=1.5,pch=17)
# lines(MC_SD[n_index,1],(MC_SD[n_index,2]/MC_SD[n_index,6])^2,type='o',col="blue",lwd=1.5,pch=3)
# lines(MC_SD[n_index,1],MC_SD[n_index,2]/MC_SD[n_index,6],type='o',col="blue",pch=6)
# legend("bottomright",col=c("black","red","purple","blue"),
#        lty=1,pch=c(1,2,3,6),legend=c("IPW","OW","LR","DR"))
# legend("bottomright",col=c("black","purple","red"),lwd=1.5,
#        lty=1,pch=c(1,3,2),legend=c("IPW","LR","OW"))
abline(h=1,lty=2,lwd=1.5)
#dev.off()
h.degree=0.75
pt=0.5
mis.specified=0
#s_string="correctly specified"
#if (mis.specified==1){s_string="mispecified"}
# setwd("cont/")
load(paste(h.degree,pt,mis.specified,"cont_result.RData",sep="_"))
#pdf(paste(h.degree,pt,mis.specified,"cont_RMSE.pdf",sep="_"),height=plot_size,width=plot_size)
plot(MC_SD[n_index,1],(MC_SD[n_index,2]/MC_SD[n_index,3])^2,type='o',
     xlab="Sample size",lwd=1.5,
     #ylab="Relative efficiency over UNADJ",
     ylab="",
     main="(b)",
     cex.lab=1.4,cex.main=2,
     # main=substitute(paste("Standard deviation, ",r,"=",pt,", ",b[1],"=",h.degree,", ",s_string),
     #                 list(pt=pt,h.degree=h.degree,s_string=s_string)),
     #ylim=range(MC_SD[n_index,2]/MC_SD[n_index,3],MC_SD[n_index,2]/MC_SD[n_index,4],MC_SD[n_index,2]/MC_SD[n_index,5],MC_SD[n_index,2]/MC_SD[n_index,6]),
     ylim=c(0,4),
     col="black",pch=15)
lines(MC_SD[n_index,1],(MC_SD[n_index,2]/MC_SD[n_index,4])^2,type='o',col="red",lwd=1.5,pch=19)
lines(MC_SD[n_index,1],(MC_SD[n_index,2]/MC_SD[n_index,5])^2,type='o',col="purple",lwd=1.5,pch=17)
# lines(MC_SD[n_index,1],(MC_SD[n_index,2]/MC_SD[n_index,6])^2,type='o',col="blue",lwd=1.5,pch=6)
# lines(MC_SD[n_index,1],MC_SD[n_index,2]/MC_SD[n_index,6],type='o',col="blue",pch=6)
# legend("bottomright",col=c("black","red","purple","blue"),
#        lty=1,pch=c(1,2,3,6),legend=c("IPW","OW","LR","DR"))
# legend("bottomright",col=c("black","purple","red"),lwd=1.5,
#        lty=1,pch=c(1,3,2),legend=c("IPW","LR","OW"))
abline(h=1,lty=2,lwd=1.5)

h.degree=0
pt=0.7
mis.specified=0
#s_string="correctly specified"
#if (mis.specified==1){s_string="mispecified"}
# setwd("cont/")
load(paste(h.degree,pt,mis.specified,"cont_result.RData",sep="_"))
#pdf(paste(h.degree,pt,mis.specified,"cont_RMSE.pdf",sep="_"),height=plot_size,width=plot_size)
plot(MC_SD[n_index,1],MC_SD[n_index,2]/MC_SD[n_index,3],type='o',
     xlab="Sample size",lwd=1.5,
     #ylab="Relative efficiency over UNADJ",
     ylab="",
     main="(c)",
     cex.lab=1.4,cex.main=2,
     # main=substitute(paste("Standard deviation, ",r,"=",pt,", ",b[1],"=",h.degree,", ",s_string),
     #                 list(pt=pt,h.degree=h.degree,s_string=s_string)),
     ylim=c(0,4),
     col="black",pch=15)
lines(MC_SD[n_index,1],(MC_SD[n_index,2]/MC_SD[n_index,4])^2,type='o',col="red",lwd=1.5,pch=19)
lines(MC_SD[n_index,1],(MC_SD[n_index,2]/MC_SD[n_index,5])^2,type='o',col="purple",lwd=1.5,pch=17)
# lines(MC_SD[n_index,1],(MC_SD[n_index,2]/MC_SD[n_index,6])^2,type='o',col="blue",lwd=1.5,pch=6)
# lines(MC_SD[n_index,1],MC_SD[n_index,2]/MC_SD[n_index,6],type='o',col="blue",pch=6)
# legend("bottomright",col=c("black","red","purple","blue"),
#        lty=1,pch=c(1,2,3,6),legend=c("IPW","OW","LR","DR"))
# legend("bottomright",col=c("black","purple","red"),lwd=1.5,
#        lty=1,pch=c(1,3,2),legend=c("IPW","LR","OW"))
abline(h=1,lty=2,lwd=1.5)

h.degree=0
pt=0.7
mis.specified=1
#s_string="correctly specified"
#if (mis.specified==1){s_string="mispecified"}
# setwd("cont/")
load(paste(h.degree,pt,mis.specified,"cont_result.RData",sep="_"))
#pdf(paste(h.degree,pt,mis.specified,"cont_RMSE.pdf",sep="_"),height=plot_size,width=plot_size)
plot(MC_SD[n_index,1],(MC_SD[n_index,2]/MC_SD[n_index,3])^2,type='o',
     xlab="Sample size",lwd=1.5,
     #ylab="Relative efficiency over UNADJ",
     ylab="",
     main="(d)",
     cex.lab=1.4,cex.main=2,
     # main=substitute(paste("Standard deviation, ",r,"=",pt,", ",b[1],"=",h.degree,", ",s_string),
     #                 list(pt=pt,h.degree=h.degree,s_string=s_string)),
     ylim=c(0,4),
     col="black",pch=15)
lines(MC_SD[n_index,1],(MC_SD[n_index,2]/MC_SD[n_index,4])^2,type='o',col="red",lwd=1.5,pch=19)
lines(MC_SD[n_index,1],(MC_SD[n_index,2]/MC_SD[n_index,5])^2,type='o',col="purple",lwd=1.5,pch=17)
# lines(MC_SD[n_index,1],(MC_SD[n_index,2]/MC_SD[n_index,6])^2,type='o',col="blue",lwd=1.5,pch=6)
abline(h=1,lty=2,lwd=1.5)
# lines(MC_SD[n_index,1],MC_SD[n_index,2]/MC_SD[n_index,6],type='o',col="blue",pch=6)
# legend("bottomright",col=c("black","red","purple","blue"),
#        lty=1,pch=c(1,2,3,6),legend=c("IPW","OW","LR","DR"))
par(mar = c(0,0,1,0))
plot(1, type = "n", axes=FALSE, xlab="", ylab="")
legend("top",inset=0,title="Relative efficiency to UNADJ", col=c("black","purple","red"),lwd=1.5,
       lty=1,pch=c(15,17,19),legend=c("IPW","LR","OW"),horiz=TRUE)

dev.off()

# 
# h.degree=0
# pt=0.5
# mis.specified=0
# #s_string="correctly specified"
# #if (mis.specified==1){s_string="mispecified"}
# # setwd("cont/")
# load(paste(h.degree,pt,mis.specified,"cont_result.RData",sep="_"))
# 
# pdf("cont_cov.pdf",height=plot_size,width=4*plot_size)
# par(mfrow=c(1,4))
# plot(CRATE[,1],CRATE[,2],type='o',
#      xlab="Sample size",lwd=1.5,
#      ylab="Coverage rate",ylim=c(min(CRATE[,-1]),1),
#      main="(a)",
#      cex.lab=1.4,cex.main=2,
#      # main=substitute(paste("Coverage rate, ",r,"=",pt,", ",b[1],"=",h.degree,", ",s_string),
#      #                 list(pt=pt,h.degree=h.degree,s_string=s_string)),
#      col="blue",pch=4)
# lines(CRATE[,1],CRATE[,3],type='o',col="black",lwd=1.5,pch=1)
# lines(CRATE[,1],CRATE[,4],type='o',col="red",lwd=1.5,pch=19)
# lines(CRATE[,1],CRATE[,5],type='o',col="purple",lwd=1.5,pch=3)
# #lines(CRATE[,1],CRATE[,6],type='o',col="blue",pch=6)
# abline(h=0.95,lty=1.5)
# # legend("bottomright",col=c("green","black","red","purple","blue"),
# #        lty=1,pch=c(5,1,2,3,6),legend=c("UNADJ","IPW","OW","LR","DR"))
# # legend("bottomright",col=c("blue","black","purple","red"),lwd=1.5,
# #        lty=1,pch=c(4,1,3,2),legend=c("UNADJ","IPW","LR","OW"))
# 
# 
# h.degree=0.75
# pt=0.5
# mis.specified=0
# #s_string="correctly specified"
# #if (mis.specified==1){s_string="mispecified"}
# # setwd("cont/")
# load(paste(h.degree,pt,mis.specified,"cont_result.RData",sep="_"))
# 
# 
# plot(CRATE[,1],CRATE[,2],type='o',
#      xlab="Sample size",lwd=1.5,
#      ylab="",ylim=c(min(CRATE[,-1]),1),
#      main="(b)",
#      cex.lab=1.4,cex.main=2,
#      # main=substitute(paste("Coverage rate, ",r,"=",pt,", ",b[1],"=",h.degree,", ",s_string),
#      #                 list(pt=pt,h.degree=h.degree,s_string=s_string)),
#      col="blue",pch=4)
# lines(CRATE[,1],CRATE[,3],type='o',col="black",lwd=1.5,pch=1)
# lines(CRATE[,1],CRATE[,4],type='o',col="red",lwd=1.5,pch=19)
# lines(CRATE[,1],CRATE[,5],type='o',col="purple",lwd=1.5,pch=3)
# #lines(CRATE[,1],CRATE[,6],type='o',col="blue",pch=6)
# abline(h=0.95,lty=1.5)
# # legend("bottomright",col=c("green","black","red","purple","blue"),
# #        lty=1,pch=c(5,1,2,3,6),legend=c("UNADJ","IPW","OW","LR","DR"))
# # legend("bottomright",col=c("blue","black","purple","red"),lwd=1.5,
# #        lty=1,pch=c(4,1,3,2),legend=c("UNADJ","IPW","LR","OW"))
# 
# h.degree=0
# pt=0.7
# mis.specified=0
# #s_string="correctly specified"
# #if (mis.specified==1){s_string="mispecified"}
# # setwd("cont/")
# load(paste(h.degree,pt,mis.specified,"cont_result.RData",sep="_"))
# 
# 
# plot(CRATE[,1],CRATE[,2],type='o',
#      xlab="Sample size",lwd=1.5,
#      ylab="",ylim=c(min(CRATE[,-1]),1),
#      main="(c)",
#      cex.lab=1.4,cex.main=2,
#      # main=substitute(paste("Coverage rate, ",r,"=",pt,", ",b[1],"=",h.degree,", ",s_string),
#      #                 list(pt=pt,h.degree=h.degree,s_string=s_string)),
#      col="blue",pch=4)
# lines(CRATE[,1],CRATE[,3],type='o',col="black",lwd=1.5,pch=1)
# lines(CRATE[,1],CRATE[,4],type='o',col="red",lwd=1.5,pch=19)
# lines(CRATE[,1],CRATE[,5],type='o',col="purple",lwd=1.5,pch=3)
# #lines(CRATE[,1],CRATE[,6],type='o',col="blue",pch=6)
# abline(h=0.95,lty=1.5)
# # legend("bottomright",col=c("green","black","red","purple","blue"),
# #        lty=1,pch=c(5,1,2,3,6),legend=c("UNADJ","IPW","OW","LR","DR"))
# # legend("bottomright",col=c("blue","black","purple","red"),lwd=1.5,
# #        lty=1,pch=c(4,1,3,2),legend=c("UNADJ","IPW","LR","OW"))
# 
# h.degree=0
# pt=0.7
# mis.specified=1
# #s_string="correctly specified"
# #if (mis.specified==1){s_string="mispecified"}
# # setwd("cont/")
# load(paste(h.degree,pt,mis.specified,"cont_result.RData",sep="_"))
# 
# 
# plot(CRATE[,1],CRATE[,2],type='o',
#      xlab="Sample size",lwd=1.5,
#      ylab="",ylim=c(min(CRATE[,-1]),1),
#      main="(d)",
#      cex.lab=1.4,cex.main=2,
#      # main=substitute(paste("Coverage rate, ",r,"=",pt,", ",b[1],"=",h.degree,", ",s_string),
#      #                 list(pt=pt,h.degree=h.degree,s_string=s_string)),
#      col="blue",pch=4)
# lines(CRATE[,1],CRATE[,3],type='o',col="black",lwd=1.5,pch=1)
# lines(CRATE[,1],CRATE[,4],type='o',col="red",lwd=1.5,pch=19)
# lines(CRATE[,1],CRATE[,5],type='o',col="purple",lwd=1.5,pch=3)
# #lines(CRATE[,1],CRATE[,6],type='o',col="blue",pch=6)
# abline(h=0.95,lty=1.5)
# # legend("bottomright",col=c("green","black","red","purple","blue"),
# #        lty=1,pch=c(5,1,2,3,6),legend=c("UNADJ","IPW","OW","LR","DR"))
# legend("bottomright",col=c("blue","black","purple","red"),lwd=1.5,
#        lty=1,pch=c(4,1,3,2),legend=c("UNADJ","IPW","LR","OW"))
# dev.off()
