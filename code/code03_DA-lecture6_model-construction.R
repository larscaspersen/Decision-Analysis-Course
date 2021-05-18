#install.packages("decisionSupport")
library(decisionSupport)


#----------------- Lecture

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


#----------------------- Seminar

library(DiagrammeR)

#add management cost to the diagram
mermaid("graph LR
        Y(Yield)-->I(Income); linkStyle 0 stroke:green, stroke-width:1.5px
        M(Market price)-->I; linkStyle 1 stroke: green, stroke-width:1.5px
        I-->F(Final result); linkStyle 2 stroke: green, stroke-width:1.5px
        CL(Labor cost)-->F; linkStyle 3 stroke: red, stroke-width:1.5px
        CM(Management Cost)-->F; linkStyle 4 stroke: red, stroke-width:1.5px")

#change orientation of diagramme to top down, increase linewidth and make cost nodes red and income nodes to green
mermaid("graph TD
        Y(Yield)-->I(Income); linkStyle 0 stroke:green, stroke-width:2px
        M(Market price)-->I; linkStyle 1 stroke: green, stroke-width:2px
        I-->F(Final result); linkStyle 2 stroke: green, stroke-width:2px
        CL(Labor cost)-->F; linkStyle 3 stroke: red, stroke-width:2px
        CM(Management Cost)-->F; linkStyle 4 stroke: red, stroke-width:2px
        style Y fill:green; style M fill:green; style I fill:green; style CL fill:red; style CM fill:red")

#add management costs to the df
input_estimates <- data.frame(variable = c("Yield", "Market_price", "Labor_cost","Management_cost"),
                              lower = c(6000, 3, 500, 100),
                              median = NA,
                              upper = c(14000, 8, 1000, 2000),
                              distribution = c("posnorm", "posnorm", "posnorm","posnorm"),
                              label = c("Yield (kg/ha)", "Price (USD/kg)", "Labor cost (USD/ha)", "Management cost (USD/ha)"),
                              Description = c("Yield in a sweet cherry farm under normal conditions",
                                              "Price of sweet cherry in a normal season",
                                              "Labor costs in a normal season","Management costs in a normal season"))

input_estimates

#model which mathematically expresses the graphical model
model_function <- function(){
  
  # Estimate the income in a normal season
  income <- Yield * Market_price
  
  #Estimate total overall costs
  overall_costs <- Labor_cost + Management_cost
  
  # Estimate the final results from the model
  final_result <- income - overall_costs
  
  # Generate the list of outputs from the Monte Carlo simulation
  return(list(final_result = final_result))
}

# Run the Monte Carlo simulation using the model function
chile_mc_simulation <- mcSimulation(estimate = as.estimate(input_estimates),
                                    model_function = model_function,
                                    numberOfModelRuns = 800,
                                    functionSyntax = "plainNames")

chile_mc_simulation

#plot mc simulation results as a histogram
plot_distributions(mcSimulation_object = chile_mc_simulation,
                   vars = "final_result",
                   method = "hist_simple_overlay",
                   old_names = "final_result",
                   new_names = "Outcome distribution for profits")

#function to draw random variables of e.g. input data set to make a run of the model
make_variables<-function(est,n=1)
{ x<-random(rho=est, n=n)
for(i in colnames(x)) assign(i,
                             as.numeric(x[1,i]),envir=.GlobalEnv)
}

#draw random variables
make_variables(as.estimate(input_estimates))
