#' @title raincloud_plot
#' @description This function plots a nice raincloud plot
#' @param x_str x variable for the plot as a string
#' @param y_str y variable for the plot as a string
#' @param xlab label of x
#' @param ylab label of y
#' @return Returns a raincloud plot
#' @details This function plots a raincloud plot
#' @examples
#' \dontrun{
#'raincloud_plot=
#'my_raincloud(
#'df=df,
#'x="working_memory",
#'y="stay",
#'xlab="Working_memory",
#'ylab="Probability to stay with same card")
#' }
#' @seealso
#' @rdname my_raincloud
#' @export
#' @import raincloudplots

my_raincloud = function(df,x_str,y_str,xlab,ylab) {

library(raincloudplots)
df_1x1 <- data_1x1(
  array_1 = df[,x_str],
  array_2 = df[,y_str],
  jit_distance = .09,
  jit_seed = 321)

raincloud_2 <- raincloud_1x1_repmes(
  data = df_1x1,
  colors = (c('dodgerblue', 'darkorange')),
  fills = (c('dodgerblue', 'darkorange')),
  line_color = 'gray',
  line_alpha = .3,
  size = 1,
  alpha = .6,
  align_clouds = FALSE) +

  scale_x_continuous(breaks=c(1,2), labels=c("Pre", "Post"), limits=c(0, 3)) +
  xlab(xlab) +
  ylab(ylab) +
  theme_classic()

raincloud_2
return (raincloud_2)
}
