#' @title simulate_values
#' @description This function simulates values of 4 groups by 2 conditions
#' @param population_size Size of the population you simulate
#' @param mean_group1 mean of first condition
#' @param mean_group2 mean of second condition
#' @param sd_group1 sd of first condition
#' @param sd_group2 sd of second condition
#' @param interaction_difference the interaction in y values
#' @param independent_var_name_1 name of variable 1
#' @param independent_var_name_2 name of variable 2
#' @param dependent_var_name name of dependent variable
#' @return Returns a df showing the values of the population in all the conditions
#' @details This function is the first step in the power simulation pipeline
#' @examples
#' \dontrun{
#'df =
#'simulate_values(population_size=10000,
#'mean_group1=0.54,
#'mean_group2=0.51,
#'sd_group1=0.04,
#'sd_group2=0.02,
#'interaction_difference=0.02,
#'independent_var_name_1 = "reward",
#'independent_var_name_2 = "load",
#'dependent_var_name = "stay_key")
#' }
#' @seealso
#' @rdname simulate_values
#' @export
#' @import dplyr
simulate_values = function(population_size,
                         mean_group1,
                         mean_group2,
                         sd_group1,
                         sd_group2,
                         interaction_difference,
                         independent_var_name_1,
                         independent_var_name_2,
                         dependent_var_name) {
  library(dplyr)
  # generate values for groups a_1 and a_2 ----------------------------------

  group_a_1 = rnorm(population_size, mean_group1, sd_group1)
  group_a_2 = rnorm(population_size, mean_group2, sd_group2)
  # generate values for groups b_1 and b_2 -----------------------------------------------------------------------
  group_b_1 = rnorm(population_size, mean_group1, sd_group1)
  group_b_2 = rnorm(population_size, mean_group2 + interaction_difference, sd_group2) #interaction is when both fixed effects are present, so only group b_2 gets it.
  df = data_frame(group_a_1, group_a_2, group_b_1, group_b_2)
  colnames(df)=c(paste(independent_var_name_1,"_0_",independent_var_name_2,"_0"),paste(independent_var_name_1,"_0_",independent_var_name_2,"_1"),paste(independent_var_name_1,"_1_",independent_var_name_2,"_0"),paste(independent_var_name_1,"_1_",independent_var_name_2,"_1"))
  num_char_var1=nchar(independent_var_name_1)
  num_char_var2=nchar(independent_var_name_2)
  #build names_pattern - work in progress
  part1=paste(replicate(num_char_var1+2,"."),sep="",collapse = "")
  part2="(.)"
  part3=paste(replicate(num_char_var2+4,"."),sep="",collapse = "")
  part4="(.)"
  names_pattern = paste(part1,part2,part3,part4,sep="")
  df = df %>% pivot_longer(cols = colnames(df),
                           names_to = c(independent_var_name_1, independent_var_name_2),
                           names_pattern = names_pattern,
                           values_to = dependent_var_name)
  df =df%>%mutate(subject = rep(1:population_size,each=4))

  df[,1:2] = sapply(df[, 1:2], as.numeric)

  return (df)
}
