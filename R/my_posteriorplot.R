#' @title my_posterior_plot
#' @description This function plots a posterior
#' @param model a model you want to create a posterior from.
#' @param variable_name the variable you want to model
#' @param xlabel the label for the variable
#' @param mycolor color of the density function
#' @return Returns a posterior plot
#' @details This function plots a posterior from your Bayesian outputs.
#' @examples
#' \dontrun{
#'posterior_plot=(
#'model=model,
#'variable_name="reward_oneback",
#'xlabel = "Reward Oneback"
#'mycolor = "gray",
#')
#'}
#' @seealso
#' @rdname my_posteriorplot
#' @export
#' @import ggplot2 insight bayestestR



my_posteriorplot<-function(model,variable_name,xlabel,mycolor="gray"){
  library(ggplot2)
  library(insight)
  library(bayestestR)
  params=insight::get_parameters(model)
  variable=params[,paste0("b_",variable_name)]
  ggplot(data.frame(x=variable),aes(x=variable))+geom_density(alpha = .5,fill=mycolor)+
    geom_vline(xintercept = median(variable), linetype="dotted",color = "blue", size=1.5)+
    geom_segment(aes(x = hdi(variable, ci = 0.95)$CI_low, y = 0, xend = hdi(variable, ci = 0.95)$CI_high, yend = 0),color="darkgray",size=2,show.legend = F)+
    xlab(xlabel)+ ylab("Density") + theme_classic()
}
