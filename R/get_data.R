readQuery<-function(file) {
  query_text <- readLines(file)
  query_code <- query_text[-(grep("--", query_text))]
  paste(query_code, collapse = " ")
}

try(babase <- DBI::dbConnect(
  RPostgreSQL::PostgreSQL(),
  host = "localhost",
  port = 22222,
  user = "jansen1",
  dbname = "babase",
  password = "Bab00n3455"), silent = TRUE)

if(!("babase" %in% ls())) {
  print("make sure you have a vpn connection with Duke and a ssh tunnel")
  print("ssh -L 22222:localhost:5432 daj23@papio.biology.duke.edu")
  print("here daj23 should be you Duke username")
}


swerb_data_query <- readQuery("./sql/swerb_data.sql")
swerb_data_raw <- as_tibble(dbGetQuery(babase, swerb_data_query))
