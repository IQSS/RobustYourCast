plot.robust.yourcast <- function(x,print="device",
                                 nparam=2,
                                 screen1=list(z =-30,x=-60),
                                 screen2=list(z =-30,x=-60),
                                 screen3=list(z =-30,x=-60),
                                 screen4=list(z =-30,x=-60),
                                 filename="diagplots.pdf",...){

  
  # Need to check if any of the parameters fixed as NA
  # Extract optimal sigma combination
  opt.sigma <- as.numeric(x$par.opt)
  
  # Any not used?
  pos.na <- which(is.na(opt.sigma))
  pos.nona <- c(1,2,3)[-pos.na]
  fix.param <- list("vary","vary","vary")
  fix.param[pos.na] <- NA

  
  if(length(pos.na)==2){
    if(print=="device"){dev.new()}
    if(print=="pdf"){pdf(filename)}
    plot.diag(x,fix.param=fix.param,...)
    if(print=="pdf"){dev.off()}
  }

  
  if(length(pos.na)==1){
    if(nparam==1){
      fix.param1 <- fix.param
      fix.param1[pos.nona[2]] <- "opt"
      fix.param2 <- fix.param
      fix.param2[pos.nona[1]] <- "opt"
      if(print=="device"){dev.new(width=12,height=6)}
      if(print=="pdf"){pdf(filename,width=12,height=6)}
      par(mfrow=c(1,2))
      plot.diag(x,fix.param=fix.param1,...)
      plot.diag(x,fix.param=fix.param2,...)
      if(print=="pdf"){dev.off()}
    }

    if(nparam==2){
      par.settings <- list(par.main.text=list(cex=1),
                           par.xlab.text=list(cex=0.8),
                           par.ylab.text=list(cex=0.8),
                           par.zlab.text=list(cex=0.8))
      
      if(print=="device"){dev.new()}
      if(print=="pdf"){pdf(filename)}
      plot.diag(x,fix.param=fix.param,screen=screen1,
              par.settings=par.settings,...)
    if(print=="pdf"){dev.off()}
    }
  }
  
  

  if(length(pos.na)==0){
    # If only one parameter allowed to vary at time
    if(nparam==1){
      if(print=="device"){dev.new(width=10,height=10)}
      if(print=="pdf"){pdf(filename,width=10,height=10)}
      par(mfrow=c(2,2))
      plot.diag(x,fix.param=list("vary","opt","opt"),...)
      plot.diag(x,fix.param=list("opt","vary","opt"),...)
      plot.diag(x,fix.param=list("opt","opt","vary"),...)
      if(print=="pdf"){dev.off()}
    }

    if(nparam==2){
      par.settings <- list(par.main.text=list(cex=1),
                           par.xlab.text=list(cex=0.8),
                           par.ylab.text=list(cex=0.8),
                           par.zlab.text=list(cex=0.8))
      
      if(print=="device"){dev.new(width=12,height=12)}
      if(print=="pdf"){pdf(filename,width=12,height=12)}
      plot.diag(x,fix.param=list("vary","vary","opt"),
                screen=screen1,
              par.settings=par.settings,
                args.print.trellis=list(more=TRUE,split=c(1,1,2,2)),...)
      plot.diag(x,fix.param=list("vary","opt","vary"),
                screen=screen2,
                par.settings=par.settings,
                args.print.trellis=list(more=TRUE,split=c(2,1,2,2)),...)
      plot.diag(x,fix.param=list("opt","vary","vary"),
                screen=screen3,
                par.settings=par.settings,
                args.print.trellis=list(more=TRUE,split=c(1,2,2,2)),...)
      plot.diag(x,fix.param=list("vary","vary","vary"),
                screen=screen4,
                par.settings=par.settings,
                args.print.trellis=list(more=FALSE,split=c(2,2,2,2)),...)
    if(print=="pdf"){dev.off()}
    }
  }
}
