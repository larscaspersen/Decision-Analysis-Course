#install.packages("decisionSupport")
library(decisionSupport)

#read data
example_decision_inputs <- read.csv('data/example_decision_inputs.csv')

#define the model
example_decision_model <- function(x, additional_benefits)
{
  profit <- benefits-costs
  
  final_profits <- profit + additional_benefits
  
  return(final_profits)
  
}

#do montecarle simulations on the model
mcSimulation_results <- mcSimulation(estimate = as.estimate(example_decision_inputs),
             model_function = example_decision_model,
             numberOfModelRuns = 700,
             functionSyntax = "plainNames")

#plot the final_profits of the run
plot_distributions(mcSimulation_object = mcSimulation_results, 
                                    vars = c('output_1'),
                                    method = 'smooth_simple_overlay', 
                                    base_size = 7)

#investiagte which of the three parameters had the biggest impact on the final_profits
pls_result <- plsr.mcSimulation(object = mcSimulation_results,
                                resultName = names(mcSimulation_results$y)[1], ncomp = 1)
#plot results
plot_pls(pls_result, input_table = example_decision_inputs, threshold = 0)

#--> costs have the most important impact, additional benefits are neglectable
