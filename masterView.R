masterView <- function(source = NULL, expand = FALSE){
  con <- RPostgres::dbConnect(
    RPostgres::Postgres(),
    dbname = "d629vjn37pbl3l",
    host = "ec2-3-209-200-73.compute-1.amazonaws.com",
    port = 5432,
    user = "reason_readonly",
    password = "p88088bd28ea68027ee96c65996f7ea3b56db0e27d7c9928c05edc6c23ef2bc27",
    sslmode = "require")
  # define the query to retrieve the plan data
  
  query <- paste("select * from", 
                 if(isTRUE(expand)){paste("plan_attribute")}
                 else{paste("master_priority_view")}
  )
  
  result <- RPostgres::dbSendQuery(con, query)
  #RPostgres::dbBind(result, list(1))
  all_data <- RPostgres::dbFetch(result) %>%
    janitor::clean_names()
  RPostgres::dbClearResult(result)
  RPostgres::dbDisconnect(con)
  
  all_data <- data.table(all_data)
  
  if(is.null(source)){
    all_data <- data.table(all_data)
    
  }else if(!is.null(source) & !isTRUE(expand)){
    all_data <- all_data[ data_source_name == source]
    
  }else if(!is.null(source) & source == "Reason" & isTRUE(expand)){
    all_data <- all_data[ data_source_id == 3]}
  
  else{all_data <- data.table(all_data)
  }
  
}