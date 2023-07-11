# Function to plot optimal sigma combinations identified by
 # optim.yourcast and robust.yourcast

run.opt <- function(x,quant="best",plot=TRUE,
                    Ha.set=NULL,Ht.set=NULL,Hat.set=NULL,
                    create.main=TRUE,...) {

  # Take arguments for yourcast() run
  args.yourcast <- x$aux.robust$args.yourcast

  if(class(x)=="robust.yourcast"){
  # Either run the best combination or some quantile
  if(quant=="best"){q.obj.fun <- x$obj.fun[which(x$obj.fun[,"obj.fun"]==
                                     min(x$obj.fun[,"obj.fun"])),]
                  label <- "Best: "}

  else{obj.quant <- quantile(x$obj.fun[,"obj.fun"],
                             probs=c(quant),type=3)
       q.obj.fun <- x$obj.fun[which(x$obj.fun[,"obj.fun"]==
                                          obj.quant),]
       label <- paste(100*quant,"th: ",sep="")
     }


  # Take smoothing parameters from desired quantile--
  # or note they are NA
  if(is.na(q.obj.fun[1])){q.Ha <- NA}
  else{q.Ha <- as.numeric(q.obj.fun[1])}

  if(is.na(q.obj.fun[2])){q.Ht <- NA}
  else{q.Ht <- as.numeric(q.obj.fun[2])}

  if(is.na(q.obj.fun[3])){q.Hat <- NA}
  else{q.Hat <- as.numeric(q.obj.fun[3])}
}

  if(class(x)=="optim.yourcast"){
    if(quant!="best"){warning("'quant' set to \"best\" since using optim.yourcast object. Use robust.yourcast object to try non-optimal levels of quant.")}
    label <- "Best: "
    q.Ha <- x$par.opt[1]
    q.Ht <- x$par.opt[2]
    q.Hat <- x$par.opt[3]
  }

  # If user wants override, override
  if(!is.null(Ha.set)){q.Ha <- Ha.set}
  if(!is.null(Ht.set)){q.Ht <- Ht.set}
  if(!is.null(Hat.set)){q.Hat <- Hat.set}
  
  # Run yourcast with desired combination
  q.obj.out <- do.call("yourcast",
                       append(list(Ha.sigma=q.Ha,
                                   Ht.sigma=q.Ht,
                                   Hat.sigma=q.Hat),
                              args.yourcast))

  
  # Produce plots if desired
  if(plot){
    list.plot.yourcast <- append(list(x=q.obj.out),list(...))
    if(create.main){
      list.plot.yourcast$dvlabel <- paste(label,"Ha=",round(q.Ha,2),
                                          ", Ht=",round(q.Ht,2),
                                          ", Hat=",round(q.Hat,2),sep="")
    }
    do.call("plot.yourcast",list.plot.yourcast)
  }


  return(q.obj.out)

}
  
 
