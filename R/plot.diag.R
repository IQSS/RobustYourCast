plot.diag <- function(x,fix.param=list("vary","vary","opt"),
                      family="obj.fun",
                      lattice.plot="wireframe",
                      screen=list(z=-30,x=-60),
                      print="device",
                      filename="objplot.pdf",
                      args.par=list(),
                      args.print.trellis=list(),...) {

  # Check fix.param
  if(!is.list(fix.param)){stop("fix.param must be list object")}

  # Create plot main lables and z-axis labels depending on type of plot
  if(family=="rss"){label <- zlabel <- "RSS"}
  if(family=="arc.age"){label <- "Age Arc Length"
                        zlabel <- "Age AL"}
  if(family=="arc.time"){label <- "Time Arc Length"
                         zlabel <- "Time AL"}
    if(family=="trend.deviate"){label <- "Trend Deviation"
                                zlabel <- "Trend Dev"}
  if(family=="obj.fun"){label <- "Objective Function"
                        zlabel <- "Obj Fun"}
    
  
  # Extract data
  data <- x$diag.valid

  # Extract optimal sigma combination
  opt.sigma <- as.numeric(x$par.opt)

  # Figure out if any sigma parameters not used
  pos.na <- which(is.na(opt.sigma))
  # Then map NAs to -999 so that can be matched more easily
  fix.param[pos.na] <- -999
  if(length(pos.na>0)){data[is.na(data)] <- -999}

  # Establish which of possible list of params to be varied
  param.tot <- c("Ha.sigma","Ht.sigma","Hat.sigma")
  pos.vary <- which(fix.param=="vary")
  if(length(pos.vary)==0){stop("At least one variable must be allowed to vary.")}
  pos.novary <- c(1,2,3)[-pos.vary]
  param <- param.tot[pos.vary]

  # Check that no more than two parameters allowed to vary
  #if(length(param)>2){stop("Only two parameters can be varied at a time.")}
  
  # Replace other elements with closest values tested with
    # robust.yourcast
  pos.other <- c()
  # Do any parameters need to be fixed?
  for(i in 1:length(fix.param)){
    other <- suppressWarnings(!any(fix.param[[i]]=="opt",fix.param[[i]]=="vary",
                                   fix.param[[i]]==-999))
    if(other){pos.other <- c(pos.other,i)}
  }
  if(length(pos.other)>0){
    if(any(!is.numeric(fix.param[[pos.other]]))){stop(paste("List item(s)",pos.other,"in fix.param are not numeric or do not take on recognized values of 'opt', 'vary' or 'NA'"))}
    for(i in pos.other){
      pos.close <- which.min(abs(fix.param[[i]]-data[,param.tot[i]]))
      fix.param[i] <- data[pos.close,param.tot[i]]
    }
  }
  
  # Replace elements of fix.param with optimum values
  pos.opt <- which(fix.param=="opt")
  if(length(pos.opt)>0){fix.param[pos.opt] <- opt.sigma[pos.opt]}
    
  # Now turn fix.param into a vector
  #fix.param <- unlist(fix.param)


  # Create plots

  # If only one parameter, plots pretty simple
  if(length(param)==1){
    # Get data from parameter at fixed value of others
    pos.fixothers <-
      which(data[,pos.novary[1]]==fix.param[[pos.novary[1]]]&
            data[,pos.novary[2]]==fix.param[[pos.novary[2]]])
    data.param <- data[pos.fixothers,c(paste(param),paste(family))]

    # Create info on fixed variables
    suppressWarnings(fix.param[fix.param==-999] <- NA)
    fix.label <- paste(param.tot[pos.novary[1]],"=",
                       round(fix.param[[pos.novary[1]]],3),", ",
                       param.tot[pos.novary[2]],"=",
                       round(fix.param[[pos.novary[2]]],3),sep="")

    # Create argument list for plotting function
    args.plot <- list(...)
    args.plot$x <- data.param
    if(is.null(args.plot$xlab)){args.plot$xlab <- param}
    if(is.null(args.plot$ylab)){args.plot$ylab <- zlabel}
    if(is.null(args.plot$main)){args.plot$main <-
                                  paste("Total Validation",
                                        label,"\n","Fixed:",fix.label)}
    if(is.null(args.plot$type)){args.plot$type <- "l"}
    
    if(print=="pdf"){pdf(filename)}
    do.call("par",args.par)
    do.call("plot",args.plot)
    if(print=="pdf"){dev.off()}
  }
  
    
    
   # If two parameters, need 3D plot from lattice library
  if(length(param)==2){
    formula <- as.formula(paste(family," ~",param.tot[pos.vary[1]],
                                "*",param.tot[pos.vary[2]]))

   # Get data from parameters at fixed value of third
    pos.fixothers <- which(data[,pos.novary]==fix.param[[pos.novary]])
    data.param <- as.data.frame(data[pos.fixothers,
                                     c(paste(param),paste(family))])

   # Create info on fixed variables
    suppressWarnings(fix.param[fix.param==-999] <- NA)
    fix.label <- paste(param.tot[pos.novary],"=",
                       round(fix.param[[pos.novary]],3),sep="")
    
    args.lattice <- list(...)
    args.lattice$x <- formula
    args.lattice$data <- data.param
    if(any(lattice.plot=="wireframe",lattice.plot=="cloud")){
      args.lattice$screen <- screen}
    if(any(lattice.plot=="wireframe",lattice.plot=="cloud")){
      if(is.null(args.lattice$scales)){args.lattice$scales <-
                                    list(arrows=FALSE)}}
    if(lattice.plot=="wireframe"){
      if(is.null(args.lattice$drape)){args.lattice$drape <- TRUE}}
    if(is.null(args.lattice$main)){args.lattice$main <-
                                  paste("Total Validation",
                                        label,"\n","Fixed:",
                                        fix.label)}
    if(any(lattice.plot=="wireframe",lattice.plot=="cloud")){
      if(is.null(args.lattice$zlab)){args.lattice$zlab <- zlabel}}
      if(is.null(args.lattice$alpha.regions)){args.lattice$alpha.regions <- 0.7}
    
    if(print=="pdf"){pdf(filename)}
    lattice.out <- do.call(lattice.plot,args.lattice)
    do.call("print",append(list(x=lattice.out),args.print.trellis))
    
    if(print=="pdf"){dev.off()}
  }


  if(length(param)==3){
    args.lattice <- list(...)
    args.lattice$x <- formula <- as.formula(paste(param.tot[1]," ~",param.tot[2],
                                "*",param.tot[3]))
    if(is.null(args.lattice$main)){args.lattice$main <-
                                     paste("Total Validation",
                                           label,"\n  ")}
    args.lattice$screen <- screen
    args.lattice$scales <- list(arrows=FALSE)
    args.lattice$data <- as.data.frame(data)
    response <- data[,family]
    response.weight <- response/max(response)
    #unique.obj.fun <- sort(unique(data$obj.fun))
    #col.obj <- heat.colors(length(unique.obj.fun))
    #pos.col <- sapply(data$obj.fun,function(x){grep(x,unique.obj.fun)})
    #args.lattice$col <- col.obj[pos.col]
    if(is.null(args.lattice$col)){args.lattice$col <-
      rgb(red=1-response.weight,green=0,blue=response.weight)}
    #args.lattice$par.settings$axis.line <- list(col = 0)
    if(is.null(args.lattice$panel)){args.lattice$panel <-
                                      function(x,response,col.points,...){
                                        panel.cloud(x,...)
                                        x1 <- -0.175
                                        y1 <- 0.18
                                        x2 <- x1+0.3
                                        ltext(x=x1,y=y1,
                                              labels=round(min(response),4),cex=0.8)
                                        ltext(x=x2,y=y1,
                                              labels=round(max(response),4),cex=0.8)
                                        col.points.use <- col.points[order(response)]
                                        npoints <- 100
                                        index.use <- trunc(seq(1,length(response),
                                                               length.out=npoints))
                                        lpoints(x=seq(x1+0.035,x2-0.035,length.out=npoints),
                                                y=y1,col=col.points.use[index.use],cex=1.25)
                                      }
                                  }
    args.lattice$response <- response
    args.lattice$col.points <- args.lattice$col
    if(print=="pdf"){pdf(filename)}
    cloud.out <- do.call("cloud",args.lattice)
    do.call("print",append(list(x=cloud.out),args.print.trellis))
    if(print=="pdf"){dev.off()}
  }
    
}

