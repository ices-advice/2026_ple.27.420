# --------------------------------------------------------------------------------------
# Functions for north sea plaice
#
# Author  : Chun chen
# --------------------------------------------------------------------------------------


## totalStk
## Adds up totals for stock objects - computes landings, discards, catch, stock totals and sets units
totalStk <- function(stk, Units){
  landings(stk) <- computeLandings(stk)
  discards(stk) <- computeDiscards(stk)
  catch.n(stk)  <- landings.n(stk)+discards.n(stk)
  catch.wt(stk) <-(landings.n(stk)*landings.wt(stk)+discards.n(stk)*discards.wt(stk))/catch.n(stk)
  catch(stk)    <- computeCatch(stk)
  stock(stk)    <- computeStock(stk)
  units(stk)[1:17] <- as.list(c(rep(Units,4), "NA", "NA", "f", "NA", "NA"))
  return(stk)
}

## extra_processing_data_year_2019
## manually add 2.9763 t of sweden in area 4
#############################################################################  
## special processing in WG2020, manually add 2.9763 t of sweden in area 4
## add on landing.n, landing, catch.n and catch

extra_processing_data_year_2019 <- function(mystock) {
  ## landing
  landings_old <- mystock@landings[,"2019"]  ## 48742 old landing
  landings_new <- round(landings_old + 2.9763, digit=0)
  mystock@landings[,"2019"]   <- landings_new
  mystock@landings.n[,"2019"] <- as.vector(landings_new)/as.vector(landings_old)*as.vector(mystock@landings.n[,"2019"])
  sum(mystock@landings.n[,"2019"]*mystock@landings.wt[,"2019"])
  ## discards
  ## disacards rate in area 4
  aa <- read.table("C:/Users/chen072/OneDrive - Wageningen University & Research/0_2020_WGNSSK/01_Data North Sea and Ska/Intercatch/Intercatch_submitted_files/StockOverview.txt",header=TRUE,sep="\t")
  aa <- aa[aa$Area %in% c("27.4", "27.4.a", "27.4.b", "27.4.c"),]
  #table(aa$Area)
  aa <- aa[aa$Catch.Cat. %in% c("Discards", "Landings"),]
  #table(aa$Catch.Cat.)
  temp <- aggregate(Catch..kg~Catch.Cat., FUN=sum, data=aa)
  dis_ratio_4  <- temp[1,2]/temp[2,2]
  discards_old <- round(mystock@discards[,"2019"], digit=1)  
  discards_new <- round(mystock@discards[,"2019"] + 2.9763*dis_ratio_4, digit=1)
  mystock@discards[,"2019"]   <- discards_new
  mystock@discards.n[,"2019"] <- as.vector(discards_new)/as.vector(discards_old)*as.vector(mystock@discards.n[,"2019"])
  sum(mystock@discards.n[,"2019"]*mystock@discards.wt[,"2019"])
  ## catch
  catch_old   <- mystock@catch[,"2019"]
  catch_new   <- round(catch_old + (discards_new-discards_old) + (landings_new-landings_old), digit=0)
  mystock@catch[,"2019"]      <- catch_new
  mystock@catch.n[,"2019"]    <- as.vector(catch_new)/as.vector(catch_old)*as.vector(mystock@catch.n[,"2019"])
  sum(mystock@catch.n[,"2019"]*mystock@catch.wt[,"2019"])
  
  return(mystock)
  
}

## write cn, cw, lf, dw, sw files in lowestoft format

write_to_Lowestoft <- function (mydat, file, data_index, format_identifier=1, mydigit, nam = "") 
{
  dat1 <- reshape(mydat, v.names = "value", idvar = "year",
                  timevar = "age", direction = "wide")
  dat1 <- dat1[order(dat1$year, decreasing = F),]
  cat(nam, "\n", file = file)
  cat(paste0("1 ",  data_index, " "),  "\n", file = file, append = TRUE)
  cat(range(mydat$year), "\n", file = file, 
      append = TRUE)
  cat(min(mydat$age), max(mydat$age), "\n", file = file, append = TRUE)
  cat(format_identifier, "\n", file = file, append = TRUE)
  write.table(round(dat1[,6:ncol(dat1)], digit=mydigit), file = file, row.names = FALSE, 
              col.names = FALSE, append = TRUE)
}

extra_processing_Lowestoft_format <- function(mystock, result_path) {
  ## catch number: cn
  dat <- melt(catch.n(mystock))
  write_to_Lowestoft(dat, file=paste0(result_path, "cn.dat"), 
                     data_index=2, format_identifier=1, mydigit=1, 
                     nam = paste("Plaice in IV(incl SK and VIId): Catch in numbers (thousands)", Sys.time(), sep=" ")) 
  
  ## catch mean weight: cw
  dat <- melt(catch.wt(mystock))
  write_to_Lowestoft(dat, file=paste0(result_path, "cw.dat"), 
                     data_index=3, format_identifier=1, mydigit=3,
                     nam = paste("Plaice in IV+IIIa: Mean weight of catches (kg)", Sys.time(), sep=" ")) 
  
  ## landing n: ln
  dat <- melt(landings.n(mystock))
  write_to_Lowestoft(dat, file=paste0(result_path, "ln.dat"), 
                     data_index=14, format_identifier=1, mydigit=1,
                     nam = paste("Plaice in IV+IIIa: landing number", Sys.time(), sep=" ")) 
  
  ## discards n: dn
  dat <- melt(discards.n(mystock))
  write_to_Lowestoft(dat, file=paste0(result_path, "dn.dat"), 
                     data_index=15, format_identifier=1, mydigit=1,
                     nam = paste("Plaice in IV+IIIa: discards number", Sys.time(), sep=" ")) 
  
  
  ## landing fraction in number: lf
  temp <- landings.n(stock)/catch.n(mystock)
  dat  <- melt(temp)
  write_to_Lowestoft(dat, file=paste0(result_path, "lf.dat"), 
                     data_index=4, format_identifier=1, mydigit=4,
                     nam = paste("Plaice in IV(incl SK and VIId): Landings fraction in numbers", Sys.time(), sep=" ")) 
  
  ## discards mean weight: dw
  dat <- melt(discards.wt(mystock))
  write_to_Lowestoft(dat, file=paste0(result_path, "dw.dat"), 
                     data_index=5, format_identifier=1, mydigit=3,
                     nam = paste("Plaice in IV+IIIa: Mean weight of discards (kg)", Sys.time(), sep=" ")) 
  
  ## landing mean weight: lw
  dat <- melt(landings.wt(mystock))
  write_to_Lowestoft(dat, file=paste0(result_path, "lw.dat"), 
                     data_index=6, format_identifier=1, mydigit=3,
                     nam = paste("Plaice in IV+IIIa: Mean weight of discards (kg)", Sys.time(), sep=" ")) 
  
  ## stock mean weight: sw
  dat <- melt(stock.wt(mystock))
  write_to_Lowestoft(dat, file=paste0(result_path, "sw.dat"), 
                     data_index=7, format_identifier=1, mydigit=3,
                     nam = paste("Plaice in IV+IIIa: Mean weight of discards (kg)", Sys.time(), sep=" ")) 
  
}

## extract catchability parameter:
extract_catchability <- function(myrun, myconf, surveys) {
  mytable  <- as.data.frame(partable(myrun))
  mytable1 <- mytable[grep("logFpar_", rownames(mytable)), ]
  mytable1$survey <- NA
  mytable1$age    <- NA
  fleets   <- c(names(surveys))
  mydat           <- NA
  for (isurvey in 1:length(fleets)) {
    myparind <- myconf$keyLogFpar[isurvey+1,][myconf$keyLogFpar[isurvey+1,]!=-1]
    iage <- 1
    for (ind2 in myparind){
      temp <- NA
      temp <- mytable1[rownames(mytable1) %in% paste0("logFpar_",ind2),]
      temp$survey <- fleets[isurvey]
      temp$age    <- iage
      iage        <- iage+1
      mydat       <- rbind(mydat, temp)
    }
  }
  mydat <- mydat[-1,]
  return(mydat)
}

## extract obs-process variance parameter:
extract_ob_pro_variance <- function(myrun, myconf, surveys) {
  mytable <- as.data.frame(partable(myrun))
  rownames(mytable)
  names(surveys)
  
  mytable1 <- mytable[grep("logSdLogObs_", rownames(mytable)), ]
  mytable1$survey <- NA
  mytable1$age    <- NA
  fleets <- c("catch", names(surveys))
  mydat           <- NA
  for (isurvey in 1:length(fleets)) {
    myparind <- myconf$keyVarObs[isurvey,][myconf$keyVarObs[isurvey,]!=-1]
    iage <- 1
    for (ind2 in myparind){
      temp <- NA
      temp <- mytable1[rownames(mytable1) %in% paste0("logSdLogObs_",ind2),]
      temp$survey <- fleets[isurvey]
      temp$age    <- iage
      iage        <- iage+1
      mydat       <- rbind(mydat, temp)
    }
  }
  mydat <- mydat[-1,]
  
  ## add process_N variance
  mytable1 <- mytable[grep("logSdLogN_", rownames(mytable)), ]
  mytable1$survey <- NA
  mytable1$age    <- NA
  mydat1           <- NA
  myparind <- myconf$keyVarLogN
  iage <- 1
  for (ind2 in myparind){
    temp1 <- NA
    temp1 <- mytable1[rownames(mytable1) %in% paste0("logSdLogN_",ind2),]
    temp1$survey <- "process_N"
    temp1$age    <- iage
    iage        <- iage+1
    mydat1       <- rbind(mydat1, temp1)
  }
  mydat1 <- mydat1[-1,]
  
  mydat1 <- rbind(mydat, mydat1)
  
  return(mydat1)
}

## extract process_F variance parameter:
extract_proF_variance <- function(myrun, myconf) {
  mytable  <- as.data.frame(partable(myrun))
  mytable1 <- mytable[grep("logSdLogFsta", rownames(mytable)), ]
  mytable1$survey <- NA
  mytable1$age    <- NA
  mydat1           <- NA
  myparind <- myconf$keyVarF[1,]
  iage <- 1
  for (ind2 in myparind){
    temp1 <- NA
    temp1 <- mytable1[rownames(mytable1) %in% paste0("logSdLogFsta_",ind2),]
    temp1$survey <- "process_F"
    temp1$age    <- iage
    iage        <- iage+1
    mydat1       <- rbind(mydat1, temp1)
  }
  mydat1 <- mydat1[-1,]
  return(mydat1)
  
}

## saveplot:
saveplotrun <- function(mydata,mytitle, irun, mypath, mywidth=800, myeight=600){
  png(paste0(mypath, paste0("plot_",mytitle, "_run", irun, ".png")),width = mywidth, height = myeight)
  plot(mydata, main=paste0(mytitle, " run", irun))
  dev.off()
}


## plot observational variance
sdplot<-function(fit){
  cf <- fit$conf$keyVarObs
  fn <- attr(fit$data, "fleetNames")
  ages <- fit$conf$minAge:fit$conf$maxAge
  pt <- partable(fit)
  sd <- unname(exp(pt[grep("logSdLogObs",rownames(pt)),1]))
  v<-cf
  v[] <- c(NA,sd)[cf+2]
  res<-data.frame(fleet=fn[as.vector(row(v))],name=paste0(fn[as.vector(row(v))]," age ",ages[as.vector(col(v))]), sd=as.vector(v))
  res<-res[complete.cases(res),]
  o<-order(res$sd)
  res<-res[o,]
  par(mar=c(13,6,2,1))
  barplot(res$sd, names.arg=res$name,las=2, col=colors()[as.integer(as.factor(res$fleet))*10], ylab="SD"); box()
}

##plot survey catchability

plot_catchability1 <- function(myrun) {
  mytable  <- as.data.frame(partable(myrun))
  mytable1 <- mytable[grep("logFpar_", rownames(mytable)), ]
  mytable1$survey <- NA
  mytable1$age    <- NA
  fleets   <- attr(myrun$data, "fleetNames")[2:length(attr(myrun$data, "fleetNames"))]
  mydat           <- NA
  for (isurvey in 1:length(fleets)) {
    myparind <- myrun$conf$keyLogFpar[isurvey+1,][myrun$conf$keyLogFpar[isurvey+1,]!=-1]
    iage <- 1
    for (ind2 in myparind){
      temp <- NA
      temp <- mytable1[rownames(mytable1) %in% paste0("logFpar_",ind2),]
      temp$survey <- fleets[isurvey]
      temp$age    <- iage
      iage        <- iage+1
      mydat       <- rbind(mydat, temp)
    }
  }
  mydat <- mydat[-1,]
  # plot
  f1 <- ggplot(mydat, aes(x = age, y = par)) +
    geom_path(lwd = 1.5) +
    geom_ribbon(aes(ymin = par-`sd(par)`, ymax = par+`sd(par)`), alpha = 0.2) +
    labs(y  = "", x = "",
         title = paste0("run ", irun, ": logFpar+SD, estimated survey catchability")) +
    scale_x_continuous(name="age", breaks=seq(1, 10, 1), labels=seq(1, 10, 1)) +
    theme_bw()+
    theme(plot.title = element_text(color="black", size=15, face="bold"),
          plot.subtitle = element_text(color="black", size=12, face="bold"),
          legend.position = "bottom", legend.title = element_text(colour="black", size=15, face="bold"),
          legend.text = element_text(colour="black", size=10, face="plain"),
          axis.text.x = element_text(color = "black", size = 14, angle = 0),
          axis.text.y = element_text(color = "black", size = 14, angle = 0),
          axis.title.y = element_text(color = "black", size = 14, angle = 90),
          strip.text.x = element_text(size = 12, colour = "black", angle = 0),
          strip.text.y = element_text(size = 10, colour = "black", angle = 0))
  f2 <- facet(f1, facet.by = c("survey"), ncol = 2, scales = "free_y", panel.labs.font.y = list(size = 12, angle=0), panel.labs.font.x = list(size = 12))
  print(f2)
  
}


plot_catchability2 <- function(myrun) {
  mytable  <- as.data.frame(partable(myrun))
  mytable1 <- mytable[grep("logFpar_", rownames(mytable)), ]
  mytable1$survey <- NA
  mytable1$age    <- NA
  fleets   <- attr(myrun$data, "fleetNames")[2:length(attr(myrun$data, "fleetNames"))]
  mydat           <- NA
  for (isurvey in 1:length(fleets)) {
    myparind <- myrun$conf$keyLogFpar[isurvey+1,][myrun$conf$keyLogFpar[isurvey+1,]!=-1]
    iage <- 1
    for (ind2 in myparind){
      temp <- NA
      temp <- mytable1[rownames(mytable1) %in% paste0("logFpar_",ind2),]
      temp$survey <- fleets[isurvey]
      temp$age    <- iage
      iage        <- iage+1
      mydat       <- rbind(mydat, temp)
    }
  }
  mydat <- mydat[-1,]
  # plot
  f1 <- ggplot(mydat, aes(x = age, y = `exp(par)`)) +
    geom_path(lwd = 1.5) +
    geom_ribbon(aes(ymin = Low, ymax = High), alpha = 0.2) +
    labs(y  = "", x = "",
         title = paste0("run ", irun, ": estimated survey catchability")) +
    scale_x_continuous(name="age", breaks=seq(1, 10, 1), labels=seq(1, 10, 1)) +
    theme_bw()+
    theme(plot.title = element_text(color="black", size=15, face="bold"),
          plot.subtitle = element_text(color="black", size=12, face="bold"),
          legend.position = "bottom", legend.title = element_text(colour="black", size=15, face="bold"),
          legend.text = element_text(colour="black", size=10, face="plain"),
          axis.text.x = element_text(color = "black", size = 14, angle = 0),
          axis.text.y = element_text(color = "black", size = 14, angle = 0),
          axis.title.y = element_text(color = "black", size = 14, angle = 90),
          strip.text.x = element_text(size = 12, colour = "black", angle = 0),
          strip.text.y = element_text(size = 10, colour = "black", angle = 0))
  f2 <- facet(f1, facet.by = c("survey"), ncol = 2, scales = "free_y", panel.labs.font.y = list(size = 12, angle=0), panel.labs.font.x = list(size = 12))
  print(f2)
  
}


## plot ob SD
plot_ob_SD <- function(myrun, myconf) {
  mytable <- as.data.frame(partable(myrun))
  #rownames(mytable)
  #names(surveys)
  
  mytable1 <- mytable[grep("logSdLogObs_", rownames(mytable)), ]
  mytable1$survey <- NA
  mytable1$age    <- NA
  fleets <- c("catch", names(surveys))
  mydat           <- NA
  for (isurvey in 1:length(fleets)) {
    myparind <- myconf$keyVarObs[isurvey,][myconf$keyVarObs[isurvey,]!=-1]
    iage <- 1
    for (ind2 in myparind){
      temp <- NA
      temp <- mytable1[rownames(mytable1) %in% paste0("logSdLogObs_",ind2),]
      temp$survey <- fleets[isurvey]
      temp$age    <- iage
      iage        <- iage+1
      mydat       <- rbind(mydat, temp)
    }
  }
  mydat <- mydat[-1,]
  
  ## add process_N variance
  mytable1 <- mytable[grep("logSdLogN_", rownames(mytable)), ]
  mytable1$survey <- NA
  mytable1$age    <- NA
  mydat1           <- NA
  myparind <- myconf$keyVarLogN
  iage <- 1
  for (ind2 in myparind){
    temp1 <- NA
    temp1 <- mytable1[rownames(mytable1) %in% paste0("logSdLogN_",ind2),]
    temp1$survey <- "process_N"
    temp1$age    <- iage
    iage        <- iage+1
    mydat1       <- rbind(mydat1, temp1)
  }
  mydat1 <- mydat1[-1,]
  
  mydat1 <- rbind(mydat, mydat1)
  # plot
  f1 <- ggplot(mydat1, aes(x = age, y = `exp(par)`)) +
    geom_path(lwd = 1.5) +
    geom_ribbon(aes(ymin = Low, ymax = High), alpha = 0.2) +
    labs(y  = "", x = "",
         title = paste0("run ", irun, ":SdLogObs+CI, estimated observation/process sd")) +
    scale_x_continuous(name="age", breaks=seq(1, 10, 1), labels=seq(1, 10, 1)) +
    theme_bw()+
    theme(plot.title = element_text(color="black", size=15, face="bold"),
          plot.subtitle = element_text(color="black", size=12, face="bold"),
          legend.position = "bottom", legend.title = element_text(colour="black", size=15, face="bold"),
          legend.text = element_text(colour="black", size=10, face="plain"),
          axis.text.x = element_text(color = "black", size = 14, angle = 0),
          axis.text.y = element_text(color = "black", size = 14, angle = 0),
          axis.title.y = element_text(color = "black", size = 14, angle = 90),
          strip.text.x = element_text(size = 12, colour = "black", angle = 0),
          strip.text.y = element_text(size = 10, colour = "black", angle = 0))
  f2 <- facet(f1, facet.by = c("survey"), ncol = 2, scales = "fixed", panel.labs.font.y = list(size = 12, angle=0), panel.labs.font.x = list(size = 12))
  print(f2)
  
}

## plotF@age
plot_F_at_age <- function(myrun){
  mytable <- melt(faytable(myrun))
  names(mytable) <- c("year", "age","f")
  mytable$age <- factor(mytable$age)
  f1 <- ggplot(data=mytable[mytable$age %in% c(1:10),], aes(x=year, y=f, color=age)) +
    geom_line(size=1) + 
    ylab("f")+ xlab("year") +
    scale_colour_brewer(palette = "Spectral") +
    geom_vline(xintercept = 1996, color = "gray", size=1)+
    #scale_colour_gradientn(colours =  RColorBrewer::brewer.pal(10, "Spectral")) +
    #ggtitle("XX") +
    theme_bw() +
    theme(plot.title = element_text(color="black", size=14, face="bold"), 
          legend.position = "bottom", 
          legend.title = element_text(colour="black", size=14, face="bold"),
          legend.text = element_text(colour="black", size=14, face="plain"),
          axis.text.x = element_text(color = "black", size = 14, angle = 0),
          axis.text.y = element_text(color = "black", size = 14, angle = 0),
          axis.title.x = element_text(color = "black", size = 14, angle = 0),
          axis.title.y = element_text(color = "black", size = 14, angle = 90)) 
  print(f1)
  
}

## plot F process error SD
plot_F_SD <- function(myrun, myconf) {
  mytable <- as.data.frame(partable(myrun))
  mytable1 <- mytable[grep("logSdLogFsta", rownames(mytable)), ]
  mytable1$survey <- NA
  mytable1$age    <- NA
  mydat1           <- NA
  myparind <- myconf$keyVarF[1,]
  iage <- 1
  for (ind2 in myparind){
    temp1 <- NA
    temp1 <- mytable1[rownames(mytable1) %in% paste0("logSdLogFsta_",ind2),]
    temp1$survey <- "process_F"
    temp1$age    <- iage
    iage        <- iage+1
    mydat1       <- rbind(mydat1, temp1)
  }
  mydat1 <- mydat1[-1,]
  # plot
  f1 <- ggplot(mydat1, aes(x = age, y = `exp(par)`)) +
    geom_path(lwd = 1.5) +
    geom_ribbon(aes(ymin = Low, ymax = High), alpha = 0.2) +
    labs(y  = "", x = "",
         title = paste0("run ", irun, ":SdLogF+CI, estimated observation sd")) +
    scale_x_continuous(name="age", breaks=seq(1, 10, 1), labels=seq(1, 10, 1)) +
    theme_bw()+
    theme(plot.title = element_text(color="black", size=15, face="bold"),
          plot.subtitle = element_text(color="black", size=12, face="bold"),
          legend.position = "bottom", legend.title = element_text(colour="black", size=15, face="bold"),
          legend.text = element_text(colour="black", size=10, face="plain"),
          axis.text.x = element_text(color = "black", size = 14, angle = 0),
          axis.text.y = element_text(color = "black", size = 14, angle = 0),
          axis.title.y = element_text(color = "black", size = 14, angle = 90),
          strip.text.x = element_text(size = 12, colour = "black", angle = 0),
          strip.text.y = element_text(size = 10, colour = "black", angle = 0))
  f2 <- facet(f1, facet.by = c("survey"), ncol = 2, scales = "fixed", panel.labs.font.y = list(size = 12, angle=0), panel.labs.font.x = list(size = 12))
  print(f2)
}

plot_N_at_age <- function(myrun) {
  
  aa        <- melt(myrun$plsd$logN)
  names(aa) <- c("age", "year", "SDlogN")
  aa$year   <- aa$year+1956
  bb        <- melt(ntable(myrun)  )  
  names(bb) <- c("year", "age", "N")
  bb$logN   <- log(bb$N)
  
  dat <- merge(aa, bb, by=c("year","age"))
  dat$high <- dat$logN+2*dat$SDlogN
  dat$low <- dat$logN-2*dat$SDlogN
  f1 <- ggplot(data=dat, aes(x=year, y=logN, color=factor(age))) +
    geom_line(size=1) + 
    geom_ribbon(aes(ymin = low, ymax = high,fill=factor(age)), linetype=0,alpha = 0.2) +
    #geom_vline(xintercept = 1996, color = "gray", size=1) +
    #geom_vline(xintercept = 1985, color = "gray", size=1) +
    ylab("logn")+ xlab("year") +
    #scale_colour_brewer(palette = "Set1") +
    #scale_colour_gradientn(colours =  RColorBrewer::brewer.pal(10, "Spectral")) +
    ggtitle(paste0("run ", irun, ":log(N)+/-SDlogN")) +
    theme_bw() +
    theme(plot.title = element_text(color="black", size=14, face="bold"), 
          legend.position = "bottom", 
          legend.title = element_text(colour="black", size=14, face="bold"),
          legend.text = element_text(colour="black", size=14, face="plain"),
          axis.text.x = element_text(color = "black", size = 14, angle = 0),
          axis.text.y = element_text(color = "black", size = 14, angle = 0),
          axis.title.x = element_text(color = "black", size = 14, angle = 0),
          axis.title.y = element_text(color = "black", size = 14, angle = 90)) 
  #f2 <- facet(f1, facet.by = c("age"), ncol = 4, scales = "fixed", panel.labs.font.y = list(size = 12, angle=0), panel.labs.font.x = list(size = 12))
  print(f1)
  
}


plot_retro_catchability <- function(re, surveys) {
  datall <- NA
  for (i in 1:5) {
    myrun  <- re[[i]]
    myconf <- re[[i]]$conf
    dd1     <- extract_catchability(myrun, myconf, surveys)
    dd1$run <- paste0("peel",i)
    datall <- rbind(datall, dd1)
  }
  datall <- datall[-1,]
  f1 <- ggplot(datall, aes(x = age, y = `exp(par)`, color=run)) +
    geom_path(lwd = 1.5) +
    geom_ribbon(aes(ymin = Low, ymax = High,fill=run), linetype=0,alpha = 0.2) +
    labs(y  = "", x = "",
         title = "compare catchability") +
    scale_x_continuous(name="age", breaks=seq(1, 10, 1), labels=seq(1, 10, 1)) +
    theme_bw()+
    theme(plot.title = element_text(color="black", size=15, face="bold"),
          plot.subtitle = element_text(color="black", size=12, face="bold"),
          legend.position = "bottom", legend.title = element_text(colour="black", size=15, face="bold"),
          legend.text = element_text(colour="black", size=10, face="plain"),
          axis.text.x = element_text(color = "black", size = 14, angle = 0),
          axis.text.y = element_text(color = "black", size = 14, angle = 0),
          axis.title.y = element_text(color = "black", size = 14, angle = 90),
          strip.text.x = element_text(size = 12, colour = "black", angle = 0),
          strip.text.y = element_text(size = 10, colour = "black", angle = 0))
  f2 <- facet(f1, facet.by = c("survey"), ncol = 2, scales = "free_y", panel.labs.font.y = list(size = 12, angle=0), panel.labs.font.x = list(size = 12))
  print(f2)
}

plot_retro_F_at_age <- function(re) {
  datall <- NA
  for (i in 1:5) {
    myrun  <- re[[i]]
    myconf <- re[[i]]$conf
    dd1 <- melt(faytable(myrun))
    names(dd1) <- c("year", "age","f")
    dd1$run <- paste0("peel",i)
    datall <- rbind(datall, dd1)
  }
  datall <- datall[-1,]
  
  f1 <- ggplot(data=datall, aes(x=year, y=f, color=run)) +
    geom_line(size=1) + 
    ylab("f")+ xlab("year") +
    scale_colour_brewer(palette = "Spectral") +
    #scale_colour_gradientn(colours =  RColorBrewer::brewer.pal(10, "Spectral")) +
    #ggtitle("XX") +
    theme_bw() +
    theme(plot.title = element_text(color="black", size=14, face="bold"), 
          legend.position = "bottom", 
          legend.title = element_text(colour="black", size=14, face="bold"),
          legend.text = element_text(colour="black", size=14, face="plain"),
          axis.text.x = element_text(color = "black", size = 14, angle = 0),
          axis.text.y = element_text(color = "black", size = 14, angle = 0),
          axis.title.x = element_text(color = "black", size = 14, angle = 0),
          axis.title.y = element_text(color = "black", size = 14, angle = 90)) 
  f2 <- facet(f1, facet.by = c("age"), ncol = 4, scales = "fixed", panel.labs.font.y = list(size = 12, angle=0), panel.labs.font.x = list(size = 12))
  print(f2)
}



plot_SD_Q_VS_SD_var <- function(myrun, myconf, surveys) {
  mytable  <- as.data.frame(partable(myrun))
  mytable1 <- mytable[grep("logFpar_", rownames(mytable)), ]
  mytable1$survey <- NA
  mytable1$age    <- NA
  fleets   <- c(names(surveys))
  mydat           <- NA
  for (isurvey in 1:length(fleets)) {
    myparind <- myconf$keyLogFpar[isurvey+1,][myconf$keyLogFpar[isurvey+1,]!=-1]
    iage <- 1
    for (ind2 in myparind){
      temp <- NA
      temp <- mytable1[rownames(mytable1) %in% paste0("logFpar_",ind2),]
      temp$survey <- fleets[isurvey]
      temp$age    <- iage
      iage        <- iage+1
      mydat       <- rbind(mydat, temp)
    }
  }
  mydat <- mydat[-1,]
  
  aa<- mydat
  names(aa)[2] <- "sd_logFpar"
  
  
  mytable1 <- mytable[grep("logSdLogObs_", rownames(mytable)), ]
  mytable1$survey <- NA
  mytable1$age    <- NA
  fleets <- c("catch", names(surveys))
  mydat           <- NA
  for (isurvey in 1:length(fleets)) {
    myparind <- myconf$keyVarObs[isurvey,][myconf$keyVarObs[isurvey,]!=-1]
    iage <- 1
    for (ind2 in myparind){
      temp <- NA
      temp <- mytable1[rownames(mytable1) %in% paste0("logSdLogObs_",ind2),]
      temp$survey <- fleets[isurvey]
      temp$age    <- iage
      iage        <- iage+1
      mydat       <- rbind(mydat, temp)
    }
  }
  mydat <- mydat[-1,]
  bb <- mydat
  names(bb)[3] <- "SDlogob"
  cc <- merge(aa[,c(2,6,7)], bb[,c(3,6,7)], by=c("survey", "age"), all.x=T)
  cc$nn <- paste(substr(cc$survey, 1,6), cc$age, sep="-")
  plot(cc$sd_logFpar, cc$SDlogob,type = "n")
  text(jitter(cc$sd_logFpar), jitter(cc$SDlogob),cc$nn, cex=0.8 )
}

library(flextable)

setMethod("as_flextable", signature(x="FLQuant"),
          function(x) {
            
            # CONVERT to year~age data.frame
            df <- as.data.frame(t(x[drop=TRUE]), row.names=FALSE)
            
            # ADD year column
            ft <- cbind(year=dimnames(x)$year, df)
            
            # CREATE flextable
            autofit(flextable(ft))
            
          })

style_table1 <- function(tab) {
  
  # Capitalize first letter of column, make header, last column and second
  # last row in boldface and make last row italic
  names(tab) <- pandoc.strong.return(names(tab))
  emphasize.strong.cols(ncol(tab))
  emphasize.strong.rows(nrow(tab))
  set.alignment("right")
  
  return(tab)
}
