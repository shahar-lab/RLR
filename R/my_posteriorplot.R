#' @title my_posteriorplot
#' @description This function plots a posterior
#' @param x a vector you want to create a posterior from.
#' @param myxlabel x label
#' @param mycolor color of density function
#' @param myxlim a vector of two numbers describing the limits of the x axis.
#' @param my_vline the value of the median or the mean.
#' @return Returns a posterior plot
#' @details This function plots a posterior from your bayesian outputs.
#' @examples
#' \dontrun{
#'posterior_plot=(
#'x=param,
#'myxlabel="Beta1",
#'mycolor = "gray",
#'myxlim = c(0,2),
#'my_vline= median(param)
#')
#'}
#' @seealso
#' @rdname my_posteriorplot
#' @export
#' @import ggplot2

my_posteriorplot<-function(x,myxlabel,mycolor,myxlim,my_vline){
  library(ggplot2)
  ggplot(data.frame(x=x),aes(x=x))+geom_density(alpha = .5,fill=mycolor)+
    geom_vline(xintercept = my_vline, linetype="dotted",color = "blue", size=1.5)+
    geom_segment(aes(x = hdi(x, ci = 0.95)$CI_low, y = 0, xend = hdi(x, ci = 0.95)$CI_high, yend = 0),color="darkgray",size=2,show.legend = F)+
    xlim(myxlim[1],myxlim[2])+ xlab(myxlabel)+ theme_classic()
}
