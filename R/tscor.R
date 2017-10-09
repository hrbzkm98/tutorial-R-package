#'Compute correlation matrix of an n by t time series matrix
#'@param x An n by t time series matrix
#'@return An n by n correlation matrix
#'@export
tscor <- function(x) {
  n = nrow(x)
  cormat = matrix(nrow = n, ncol = n)
  for (i in 1:n){
    for (j in 1:n){
      cormat[i,j] = cor(x[i,],x[j,])
    }
  }
  return(cormat)
}

#'Return a visualization of time series
#'@param x An n by t time series matrix
#'@return A gglot with n time series on one graph
#'@export
tsplot <- function(x) {
  df <- melt(x)
  n = ncol(x)
  g = ggplot(df,aes(Var2,value,group = factor(Var1))) + geom_line(aes(color=factor(Var1))) + xlab('Time') + ylab('Value') + labs(color='Time Series Label')
  g = g + scale_x_continuous(limits=c(0, n+1),breaks = c(1:n))
  g = g + theme(plot.title = element_text(hjust = 0.5))
  g = g + ggtitle('Time Series Visualization')
  print(g)
}

#'Return a visualization of time series correlation matrix
#'@param x An n by t time series matrix
#'@return A heatmap visualizing correlation matrix
#'@export
tscorplot <- function(x) {
  n = nrow(x)
  c = melt(tscor(x))
  g = ggplot(c, aes(x=Var1, y=Var2, fill=value)) + geom_tile()
  g = g + scale_fill_continuous(limits=c(-1, 1), breaks=seq(-1,1,by=0.5))
  g = g + scale_x_continuous(name="Time Series Labels", limits=c(0, n+1),breaks = c(1:n))
  g = g + scale_y_continuous(name="Time Series Labels", limits=c(0, n+1),breaks = c(1:n))
  g = g + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  g = g + theme(plot.title = element_text(hjust = 0.5))
  g = g + ggtitle('Correlation Matrix Heat Map')
  print(g)
}
