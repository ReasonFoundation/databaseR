pullAllData <- function (FY) 
{
  
  ### SQL connection
  con <- RPostgres::dbConnect(
    RPostgres::Postgres(),
    dbname = "d629vjn37pbl3l",
    host = "ec2-3-209-200-73.compute-1.amazonaws.com",
    port = 5432,
    user = "reason_readonly",
    password = "p88088bd28ea68027ee96c65996f7ea3b56db0e27d7c9928c05edc6c23ef2bc27",
    sslmode = "require")
  
  list <- c("year",
            "plan_id",                                                                  
            "display_name",                                                             
            "state",                                                                    
            "employee_contribution_dollar",                                             
            "actuarial_funded_ratio_percentage",                                        
            "actuarially_required_contribution_dollar",                                 
            "actuarially_accrued_liabilities_dollar",                                   
            "employer_state_contribution_dollar",                                       
            "type_of_employees_covered",                                                
            "discount_rate_assumption",                                                 
            "covered_payroll_dollar",                                                   
            "actuarial_cost_method_in_gasb_reporting",                                  
            "actuarial_value_of_assets_gasb_dollar",                                    
            "number_of_years_remaining_on_amortization_schedule",                       
            "actuarially_required_contribution_paid_percentage",                        
            "market_assets_reported_for_asset_smoothing",                               
            "total_pension_liability_dollar",                                           
            "total_benefits_paid_dollar",                                               
            "fiscal_year_of_contribution",     
            "fiscal_year_end_date", 
            "asset_valuation_method_for_gasb_reporting",                                
            "employer_contribution_regular_dollar",                                     
            "total_normal_cost_percentage",                                             
            "unfunded_actuarially_accrued_liabilities_dollar",                          
            "employee_normal_cost_percentage",                                          
            "total_amortization_payment_percentage",                                    
            "total_normal_cost_dollar",                                                 
            "statutory_payment_dollar",                                                 
            "payroll_growth_assumption",                                                
            "employers_projected_actuarial_required_contribution_percentage_of_payroll",
            "amortizaton_method",                                                       
            "wage_inflation",                                                           
            "refunds_dollar",                                                           
            "market_investment_return_mva_basis",                                       
            "market_value_of_assets_dollar",                                            
            "statutory_payment_percentage",                                             
            "investment_return_assumption_for_gasb_reporting",                          
            "administrative_expense_dollar",                                            
            "benefit_payments_dollar",                                                  
            "total_contribution_dollar",                                                
            "x1_year_investment_return_percentage",                                     
            "other_contribution_dollar",                                                
            "employer_normal_cost_dollar")
  
  #### SQL query
  query <- paste("select * from pull_data_state_only()\nwhere year > '", 
                 paste(FY - 1), "'")
  result <- RPostgres::dbSendQuery(con, query)
  all_data <- RPostgres::dbFetch(result) %>% janitor::clean_names()
  RPostgres::dbClearResult(result)
  RPostgres::dbDisconnect(con)
  all_data %>% dplyr::group_by_at(dplyr::vars(-.data$attribute_value)) %>% 
    dplyr::mutate(row_id = 1:dplyr::n()) %>% dplyr::ungroup() %>% 
    tidyr::pivot_wider(names_from = attribute_name, values_from = attribute_value) %>% 
    dplyr::select(-.data$row_id) %>% dplyr::arrange(display_name, 
                                                    year) %>% janitor::clean_names() %>%
    dplyr::select(print(list))
}
