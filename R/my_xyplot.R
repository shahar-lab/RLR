#' @title my_xyplot
#' @description This function plots a nice ggplot geom_point graph
#' @param x x variable for the plot
#' @param y y variable for the plot
#' @param myxlab label of x
#' @param myylab label of y
#' @param mycolor color of points
#' @return Returns a standard plot with geom_point
#' @details This function standardizes the plotting procedure
#' @examples
#' \dontrun{
#'plot=
#'my_xyplot(
#'x=working_memory,
#'y=stay,
#'myxlab="Working_memory",
#'myylab="Probability to stay with same card",
#'mycolor = "green")
#' }
#' @seealso
#' @rdname my_xyplot
#' @export
#' @import ggplot2

my_xyplot<-function(x,y,myxlab,myylab,mycolor){
  library(ggplot2)
  p =
  ggplot(data.frame(x =x, y =y), aes(x=x,y=y))+

           geom_point(col=mycolor,alpha=0.7)+
           geom_smooth(method="lm")+
           ggtitle('',subtitle = paste('r=',round(cor(x,y),3)))+

           xlab(myxlab)+ylab(myylab)+

    #xlim(0,1)+ylim(0,1)+
    theme_classic()

return(p)

}
