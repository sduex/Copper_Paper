using CSV
using DataFrames
using JuMP
using Gurobi
using LinearAlgebra

# Optimization Run Function
# Inputs:
# - Demand 
# - Deposits parameters
# - Save folder 
# - degradationLimit: Set to zero for single objective optimization
# - discount rate
# - cost to not met demand (slack) - historic high price: 68000 USD per LCE
# - use an hyperbolic discount rate (default is false)
function runOptimization(demand,deposit,saveFolder, degradationLimit,discount_rate = 0.07,bigM_cost = 205,hyperbolic=true)
    
    d_size = size(deposit, 1) 
    t_size = size(demand, 1)
    
    # Extract necessary columns
    # Demand in ktons
    demand = demand[!, :Demand]./1000
    # Name
    deposit_name = deposit[!, :Deposit_Name]
    # Reserves
    reserve = deposit[!, :reserve]
    resource_demostrated = deposit[!, :resource_demonstrated]
    resource_inferred = deposit[!, :resource_inferred]
    #Grades
    grade_1 = deposit[!, :grade_reserve]
    grade_2 = deposit[!, :grade_resource]
    grade_3 = deposit[!, :grade_resource_inferred]
    #Cobalt grades
    grade_1_Co = deposit[!, :Grade_Proven_Probable_Cobalt]
    grade_2_Co = deposit[!, :Grade_Measured_Indicated_Only_Cobalt]
    grade_3_Co = deposit[!, :Grade_Inferred_Cobalt]

    grade_edb = deposit[!, :EDB_Grade]
    # Dynamics
    
    prod_rate2023 = deposit[!, :prod_rate2023]
    prod_rate2024 = deposit[!, :prod_rate2024]
    prod_rate2025 = deposit[!, :prod_rate2025]
    prod_rate2026 = deposit[!, :prod_rate2026]
    prod_rate2027 = deposit[!, :prod_rate2027]
    prod_rate2028 = deposit[!, :prod_rate2028]
    #prod_rate2030 = deposit[!, :prod_rate2030]
    
    max_prod_rate = deposit[!, :max_cap_Mt_ore]
    max_ramp_up = deposit[!, :max_ramp_Mt_ore]
    min_prod_rate = max_prod_rate ./ 4 
    # Costs, all in million USD
    cost_extraction1 = deposit[!, :Opex1_Tax]  #./ 1e3 # divide by 1e6 to million USD, multiply by 1e3 to get to kton
    cost_extraction2 = deposit[!, :Opex2_Tax]  #./ 1e3
    cost_extraction3 = deposit[!, :Opex3_Tax]  #./ 1e3
    cost_expansion = deposit[!, :Cost_Expansion]  #./ 1e3 # same as extraction
    cost_opening = deposit[!, :Cost_Open]  #./ 1e6 # million usd
    # Non Monetary Factors
    edb = deposit[!, :edb] # 0 to 100, 0 is better and 100 is worse
    
    ###Utilization
    util =  deposit[!, :Utilization]
    
    # Set big M values
    bigM_extract = maximum(max_prod_rate)
    
    # Planned capacity over time, towards 2030
    prod_rate = zeros(d_size, t_size)
    
    # Fill the matrix for the specific years
   # prod_rate[:, 1] .= prod_rate2022          # Year 2022
    prod_rate[:, 1] .= prod_rate2023          # Year 2023
    prod_rate[:, 2] .= prod_rate2024
    prod_rate[:, 3] .= prod_rate2025          # Year 2025
    prod_rate[:, 4] .= prod_rate2026
    prod_rate[:, 5] .= prod_rate2027
    prod_rate[:, 6:end] .= prod_rate2028
    #prod_rate[:, 9:end] .= prod_rate2030      # Years 2030 to 2070
    
    # Linear interpolation for 2024 (index 3)
   # prod_rate[:, 3] .= (prod_rate2023 .+ prod_rate2025) ./ 2
    
    # Linear interpolation for 2026 to 2029 (indices 5 to 8)
  #  for i in 1:4
  #      t = i / 5
 #       prod_rate[:, 4 + i] .= (1 - t) .* prod_rate2025 .+ t .* prod_rate2030
 #   end
    
    # Discount rates for costs
    if hyperbolic
        discounter = 1 .+ discount_rate .*(0:size(demand, 1) - 1) # hyperbolic discount rate
    else
        discounter = (1 .+ discount_rate) .^(0:size(demand, 1) - 1)
    end

    cost_extraction1 = cost_extraction1 .* (1 ./ discounter')
    cost_extraction2 = cost_extraction2 .* (1 ./ discounter')
    cost_extraction3 = cost_extraction3 .* (1 ./ discounter')
    cost_opening = cost_opening .* (1 ./ discounter')
    cost_expansion = cost_expansion .* (1 ./ discounter')
    # Avoid expansion of certain mines with no info
    status = deposit[!, :Status_Delay]     
    delay_years = deposit[!, :Delay_years] # delay in expansion
    for i in 1:size(cost_expansion, 1)
        if delay_years[i] > 0
            cost_expansion[i, 1:delay_years[i]] .= 1e6 # Not possible to expand, given the delay in years   
        end
    end
    
    # Big M effect, should be reduced towards the future?
    bigM_cost = bigM_cost .* (1 ./ discounter') 
    
    # Create optimization model
    model = Model(Gurobi.Optimizer)
    
    # Decision variables
    @variable(model, x[1:d_size, 1:t_size] >= 0)  # Extraction
    @variable(model, x2[1:d_size, 1:t_size] >= 0)  # Stepwise cost
    @variable(model, x3[1:d_size, 1:t_size] >= 0) 
    @variable(model, y[1:d_size, 1:t_size] >= 0)  # Additional capacity
    @variable(model, w[1:d_size, 1:t_size], Bin)  # Open or not
    @variable(model, z[1:t_size] >= 0)  # Slack to match balance
  #  @variable(model, s1[1:d_size, 1:t_size], Bin)  # Can extract from resource
   # @variable(model, s2[1:d_size, 1:t_size], Bin)  # Can extract from inferred
    
    # Objective function
    @objective(model, Min, sum(cost_extraction1[d, t] * x[d, t] +
    cost_extraction2[d, t] * x2[d, t] +  # Stepwise cost
    cost_extraction3[d, t] * x3[d, t] + 
    cost_expansion[d, t] * y[d, t] +
    cost_opening[d, t] * w[d, t] for d in 1:d_size, t in 1:t_size) +
    sum(bigM_cost[t] * z[t] for t in 1:t_size))
    
    optimize!(model)  # QUICK RUN TO AVOID WEIRD ERROR WITH MULTI OBJECTIVE CODE

    # Constraints
    # Extraction less than available production capacity considering Utilization
    @constraint(model, c1[d in 1:d_size, t in 1:t_size], x[d, t] + x2[d, t] +x3[d,t] <= sum(y[d, t1]*util[d] for t1 in 1:t) + prod_rate[d,t]*util[d])
    # Met demand ###added artificial grade to slack
    @constraint(model, c2[t in 1:t_size], sum(x[d, t]*grade_1[d]/100 + x2[d, t]*grade_2[d]/100 +x3[d,t]*grade_3[d]/100 for d in 1:d_size) + z[t]*0.01 >= demand[t]+(t > 1 ? z[t-1]*0.01 : 0))
    # Max depletion of reserves, 3 stages
    @constraint(model, c3[d in 1:d_size], sum(x[d, t] for t in 1:t_size) <= reserve[d])
    @constraint(model, c4[d in 1:d_size], sum(x2[d, t] for t in 1:t_size) <= resource_demostrated[d])
    @constraint(model, c5[d in 1:d_size], sum(x3[d, t] for t in 1:t_size) <= resource_inferred[d])        
    # Max production rate only on open mines

   #@constraint(model, force_open[d in 1:d_size, t in 1:t_size], w[d, t] == 1) ##debugging

    @constraint(model, c6[d in 1:d_size, t in 1:t_size], sum(y[d, t1] for t1 in 1:t) + prod_rate[d,t] <= sum(w[d, t1] for t1 in 1:t) * max_prod_rate[d])
    # Open mine only once
    @constraint(model, c7[d in 1:d_size], sum(w[d, t] for t in 1:t_size) <= 1)

    # Max Ramp up
    @constraint(model, c8[d in 1:d_size, t in 1:t_size], y[d, t] <= max_ramp_up[d])  





    #####Reserves before resources before inferred
  #  @constraint(model, restrict_x2[d in 1:d_size, t in 1:t_size], x2[d,t] <= s1[d,t] * max_prod_rate[d])
   # @constraint(model, restrict_x3[d in 1:d_size, t in 1:t_size], x3[d,t] <= s2[d,t] * max_prod_rate[d])

  #  @constraint(model, stage1_unlock[d in 1:d_size, t in 1:t_size], 
 #   sum(x[d, t1] for t1 in 1:t) >= reserve[d] * s1[d,t])

  #  @constraint(model, stage2_unlock[d in 1:d_size, t in 1:t_size],
  #  sum(x2[d, t1] for t1 in 1:t) >= resource_demostrated[d] * s2[d,t])

    if degradationLimit>0
        # Second objective: Non monetary Factors
        # see: https://github.com/jump-dev/Gurobi.jl/issues/294
        # https://github.com/jump-dev/Gurobi.jl/pull/295        
        
        MOI.set(model,Gurobi.NumberOfObjectives(),2)  # Multiobjective
        
        # Minimize EDB to expand capacity in countries
         f2 = @expression(model, sum(edb[d]*sum(y[d,t]*grade_edb[d]/100/discounter[t]  for t in 1:t_size) for d in 1:d_size))
        MOI.set(model, Gurobi.MultiObjectiveFunction(2), moi_function(f2))
        # Set the relative tolerance for the second objective
        # EDB is optimized within 10% of the solution based solely on costs
        MOI.set(model,Gurobi.MultiObjectiveAttribute(1,"ObjNRelTol"),degradationLimit) # index start at 0, annoying behavior
        
        # STATUS OCT 24: Code no longer work with priority, but if I run it without it and then again it works!
        # Note that set priority is needed to run the hierarchical model
        # SOLUTION SO FAR: RUN OPTIMIZE WITH NO CONSTRAINTS AND THEN CODE WORKS 

        # Priority
        MOI.set(model, Gurobi.MultiObjectivePriority(1), 10)
        MOI.set(model, Gurobi.MultiObjectivePriority(2), 5) 
    end
    optimize!(model)

    # get and store results
    x_values = [value(x[d, t]*grade_1[d]/100) for d in 1:d_size, t in 1:t_size]
    x2_values = [value(x2[d, t]*grade_2[d]/100) for d in 1:d_size, t in 1:t_size]
    x3_values = [value(x3[d, t]*grade_3[d]/100) for d in 1:d_size, t in 1:t_size]
    y_values = [value(y[d, t]) for d in 1:d_size, t in 1:t_size]
    w_values = [value(w[d, t]) for d in 1:d_size, t in 1:t_size]
    z_values = [value(z[ t]) for t in 1:t_size]

    x_values_co=[value(x[d, t]*grade_1_Co[d]/100) for d in 1:d_size, t in 1:t_size]
    x2_values_co=[value(x2[d, t]*grade_2_Co[d]/100) for d in 1:d_size, t in 1:t_size]
    x3_values_co=[value(x3[d, t]*grade_3_Co[d]/100) for d in 1:d_size, t in 1:t_size]
    x_total= [value(x[d, t]+x2[d, t]+x3[d, t]) for d in 1:d_size, t in 1:t_size]
    
    # mine open cumulative - to indicate if the mine is open in that period
    mine_open = mapslices(cumsum, w_values, dims=2)
    
    # df_results = DataFrame(d = repeat(1:d_size, outer=t_size),t = repeat(2022:(t_size+2021), inner=d_size),
    df_results = DataFrame(d = repeat(deposit_name, outer=t_size),t = repeat(2023:(t_size+2022), inner=length(deposit_name)),
    tons_extracted1 = vec(x_values),
    tons_extracted2 = vec(x2_values),
    tons_extracted3 = vec(x3_values),
    capacity_added = vec(y_values),
    mine_opened = vec(w_values),
    cobalt1= vec(x_values_co),
    cobalt2= vec(x2_values_co),
    cobalt3= vec(x3_values_co),
    total_ore_mined=vec(x_total))
    
    df_z = DataFrame(variable="demand_unmet",t = 2023:(t_size+2022),value = vec(z_values))
    
    # save results
    # create directory if not there
    if !isdir("Results/Optimization/"* saveFolder)
        mkpath("Results/Optimization/"* saveFolder)
    end
    url_file = "Results/Optimization/"* saveFolder *"/Base_Julia.csv"
    CSV.write(url_file, df_results)
    url_file = "Results/Optimization/"* saveFolder *"/Slack_Julia.csv"
    CSV.write(url_file, df_z)
    # Save optimization parameters
    url_file = "Results/Optimization/"* saveFolder *"/OptimizationInputs.csv"
    inputs_text = DataFrame([
        ("Discount rate", discount_rate),
        ("Slack cost", bigM_cost[1]),
        ("Degradation limit", degradationLimit),
        ("Time vector size", t_size),
        ("Deposit vector size", d_size)
        ], [:Parameter, :Value])
    CSV.write(url_file, inputs_text)

    
    if degradationLimit>0
         # Save final values
         final_obj_values = [objective_value(model)[i] for i in 1:2]
         url_file = "Results/Optimization/"* saveFolder *"/OptimalValue.txt"
         open(url_file, "w") do file
             for (i, val) in enumerate(final_obj_values)
                 write(file, "Objective $i: $val\n")
             end
         end 
    end

    if degradationLimit==0
         # Save final values
        final_obj_values = [objective_value(model)[i] for i in 1:1]
        url_file = "Results/Optimization/"* saveFolder *"/OptimalValue.txt"
        open(url_file, "w") do file
            for (i, val) in enumerate(final_obj_values)
                write(file, "Objective $i: $val\n")
            end
        end
    end    
end