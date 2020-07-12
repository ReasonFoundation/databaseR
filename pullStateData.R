##Download all state-level data (filter columns & starting year)

con <- RPostgres::dbConnect(
  RPostgres::Postgres(),
  dbname = "d629vjn37pbl3l",
  host = "ec2-3-209-200-73.compute-1.amazonaws.com",
  port = 5432,
  user = "reason_readonly",
  password = "p88088bd28ea68027ee96c65996f7ea3b56db0e27d7c9928c05edc6c23ef2bc27",
  sslmode = "require")

query <- paste0("select * from pull_data_state_only()
where year > '2001'
and attribute_name in ('1 Year Investment Return Percentage',
'1 Year Investment Return Percentage',
'Investment Return Assumption for GASB Reporting',
'Actuarially Accrued Liabilities Dollar',
'Total Normal Cost Percentage',
'Covered Payroll Dollar',
'Payroll Growth Assumption',
'Total Benefits Paid Dollar')")

result <- RPostgres::dbSendQuery(params = NULL,con, query)
RPostgres::dbBind(result, params = NULL)
all_data <- RPostgres::dbFetch(result) %>%
  janitor::clean_names()
RPostgres::dbClearResult(result)
RPostgres::dbDisconnect(con)

state_plan_data <- all_data
all_data <- data.table(state_plan_data %>% arrange(year, display_name))
all_data$attribute_value <- as.numeric(all_data$attribute_value)

all_data <- data.table(all_data %>% 
                         pivot_wider(names_from = attribute_name, values_from = attribute_value))