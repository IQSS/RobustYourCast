sigma.plot <- function(plot.data) {
  range.y <- range(plot.data$Ha.sigma)[2]-range(plot.data$Ha.sigma)[1]
  plot(plot.data$Ha.sigma,plot.data$Ht.sigma,xlab="Ha.sigma",
       ylab="Ht.sigma",ylim=c(min(plot.data$Ht.sigma),
                         max(plot.data$Ht.sigma)*1.03),
       col=rainbow(nrow(plot.data)))
  text(plot.data$Ha.sigma,plot.data$Ht.sigma+0.03*range.y,
       labels=c(1980:2005),cex=0.8)
  arrows(plot.data$Ha.sigma[-nrow(plot.data)],
         plot.data$Ht.sigma[-nrow(plot.data)],
         plot.data$Ha.sigma[-1],
         plot.data$Ht.sigma[-1],code=2,length=0.1,lty=2)
}
