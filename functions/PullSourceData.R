##pullSourceData Function
pullSourceData <- function(plan_name)
{    con <- RPostgres::dbConnect(
  RPostgres::Postgres(),
  dbname = "d629vjn37pbl3l",
  host = "ec2-3-209-200-73.compute-1.amazonaws.com",
  port = 5432,
  user = "reason_readonly",
  password = "p88088bd28ea68027ee96c65996f7ea3b56db0e27d7c9928c05edc6c23ef2bc27",
  sslmode = "require")
# define the query to retrieve the plan data
plan_id <- pl$id[pl$display_name == plan_name]
query <- paste("select * from pull_plan_data(",plan_id,")")
#paste0("select * from pull_plan_data('", str_replace(plan_name,"'", "''"), "')")

result <- RPostgres::dbSendQuery(con, query)
#RPostgres::dbBind(result, list(1))
all_data <- RPostgres::dbFetch(result) %>%
  janitor::clean_names()
RPostgres::dbClearResult(result)
RPostgres::dbDisconnect(con)

all_data %>%
  dplyr::group_by_at(dplyr::vars(-.data$attribute_value)) %>%  # group by everything other than the value column.
  dplyr::mutate(row_id = 1:dplyr::n()) %>%
  dplyr::ungroup() %>%  # build group index
  tidyr::spread(.data$attribute_name, .data$attribute_value, convert = TRUE) %>%    # spread
  dplyr::select(-.data$row_id) %>%  # drop the index
  janitor::clean_names()
}