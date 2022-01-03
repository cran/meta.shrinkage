rml<-function(y, x = 1:length(y), id = 1:length(y), decreasing = FALSE, test=FALSE){
  G <- length(y)
  data <- data.frame(id = id, y = y, x = x)
  dat <- data[order(data$x, decreasing = decreasing), ]
  RML=numeric(G)
  for (i in 1:G) {
    z <- matrix(0, nrow = i, ncol = (G - i + 1))
    for (s in 1:i) {
      for (u in i:G) {
        z[s, (u - (i - 1))] <- sum(dat$y[s:u])/(u - s + 1)
      }
    }
    RML[i] <- max(apply(z, 1, min))
  }
  RML_table <- data.frame(id = dat$id, x = dat$x, RML = RML)

  if(test==TRUE){
    plot(x,y)
    lines(lowess(x,y))
    Tau=cor.test(x,y,method="kendall")
    Direct="Non-monotone"
    if((Tau$estimate>0)&(Tau$p.value<0.10)){Direct="Increasing"}
    if((Tau$estimate<0)&(Tau$p.value<0.10)){Direct="Decreasing"}
    text(x=mean(x),y=mean(y),Direct)
    text(x=mean(x),y=max(y),paste("Kendall's tau = ",round(Tau$estimate,3)))
    text(x=mean(x),y=min(y),paste("P-value = ",round(Tau$p.value,3)))
  }
  return(RML_table)
}
