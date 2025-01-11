# install.packages("dplyr")
# install.packages("RSelenium")
# install.packages("jsonlite")
# install.packages("httr")
# install.packages("pak")
# install.packages("rvest")
# install.packages("xml2")
# install.packages("DBI")
# install.packages("RMySQL")

library(dplyr)
library(RSelenium)
library(jsonlite)
library(httr)
library(pak)
library(rvest)
library(xml2)
library(DBI)
library(RMySQL)

# Send GET-anmodning for at hente siden
initial_res_hcab <- GET("https://envs2.au.dk/Luftdata/Presentation/table/Copenhagen/HCAB")

# Base URL
base_url <- "https://envs2.au.dk/"

# Parse HTML for at finde tokenet
initial_html_hcab <- read_html(content(initial_res_hcab, "text"))

# Find tokenet i HTML'en
token <- initial_html_hcab %>%
  html_element("input[name='__RequestVerificationToken']") %>%
  html_attr("value")

# Tjek, om tokenet blev fundet
if (is.null(token)) {
  stop("Kunne ikke finde __RequestVerificationToken i HTML.")
} else {
  print(token)
}
#5tKvi8Tm4IlZDkzx6xSfdNFv9UxYJZNEe_8ZWXfRSZUQrgNFnFsTkFb9KNcoWTSj0fqdLlfkfGCAcCXZQzehLSNk5VY4QZ17RDncw1OvhHU1

hcab_table_res <- POST(
  url = paste0(base_url, "/Luftdata/Presentation/table/MainTable/Copenhagen/HCAB"),
  add_headers(
    "X-Requested-With" = "XMLHttpRequest",
    "Content-Type" = "application/x-www-form-urlencoded",
    #"Referer" = url_hcab,
    "Origin" = base_url,
    "User-Agent" = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/130.0.0.0 Safari/537.36"
  ),
  body = list(
    "__RequestVerificationToken" = token
  ),
  encode = "form"
)

# Tilføj en forsinkelse på 5 sekunder
Sys.sleep(5)

# Tjek status på POST-anmodningen
if (status_code(hcab_table_res) != 200) {
  stop(paste("POST-anmodning fejlede med statuskode:", status_code(hcab_table_res)))
}

# Step 3: Parse HTML-svar for at finde tabellen
content_hcab <- content(hcab_table_res, "text")
hcab_html <- read_html(content_hcab)

# Udtræk tabellen
hcab_table <- hcab_html %>%
  html_table(fill = TRUE)



if (length(hcab_table) == 0) {
  stop("Ingen tabeller fundet i HTML-svaret.")
} else {
  hcab_dataframe <- as.data.frame(hcab_table[[1]])  # Konverter tabel til dataframe
  print("Fundet tabel som dataframe:")
  print(hcab_dataframe)
}


names(hcab_dataframe)[names(hcab_dataframe) == "Målt (starttid)"] <- "starttid"


hcab_dataframe$ScrapeTime <- Sys.time()




url_anholt <- "https://envs2.au.dk/Luftdata/Presentation/table/Rural/ANHO"

# Step 1: Send GET-anmodning for at hente siden og finde token
initial_res_anholt <- GET(url_anholt)
initial_html_anholt <- read_html(content(initial_res_anholt, "text"))

# Find __RequestVerificationToken i HTML'en
token_anholt <- initial_html_anholt %>%
  html_element("input[name='__RequestVerificationToken']") %>%
  html_attr("value")

# Tjek, om tokenet blev fundet
if (is.null(token_anholt)) {
  stop("Kunne ikke finde __RequestVerificationToken i HTML.")
} else {
  print(paste("Fundet token:", token_anholt))
}

anholt_table_res <- POST(
  url = paste0(base_url, "/Luftdata/Presentation/table/MainTable/Rural/ANHO"),
  add_headers(
    "X-Requested-With" = "XMLHttpRequest",
    "Content-Type" = "application/x-www-form-urlencoded",
    "Referer" = url_anholt,
    "Origin" = base_url,
    "User-Agent" = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/130.0.0.0 Safari/537.36"
  ),
  body = list(
    "__RequestVerificationToken" = token_anholt
  ),
  encode = "form"
)

# Tilføj en forsinkelse på 5 sekunder
Sys.sleep(5)

# Tjek status på POST-anmodningen
if (status_code(anholt_table_res) != 200) {
  stop(paste("POST-anmodning fejlede med statuskode:", status_code(anholt_table_res)))
}

# Step 3: Parse HTML-svar for at finde tabellen
content_anholt <- content(anholt_table_res, "text")
anholt_html <- read_html(content_anholt)

# Udtræk tabellen som en dataframe
anholt_table <- anholt_html %>%
  html_table(fill = TRUE)

# Step 4: Gem tabellen som dataframe
if (length(anholt_table) == 0) {
  stop("Ingen tabeller fundet i HTML-svaret.")
} else {
  anholt_dataframe <- as.data.frame(anholt_table[[1]])  # Konverter tabel til dataframe
  print("Fundet tabel som dataframe:")
  print(anholt_dataframe)
}


anholt_dataframe$ScrapeTime <- Sys.time()

names(anholt_dataframe)[names(anholt_dataframe) == "Målt (starttid)"] <- "starttid"




url_banegaard <- "https://envs2.au.dk/Luftdata/Presentation/table/Aarhus/AARH3"

# Step 1: Send GET-anmodning for at hente siden og finde token
initial_res_banegaard <- GET(url_banegaard)
initial_html_banegaard <- read_html(content(initial_res_banegaard, "text"))

# Find __RequestVerificationToken i HTML'en
token_banegaard<- initial_html_banegaard %>%
  html_element("input[name='__RequestVerificationToken']") %>%
  html_attr("value")

# Tjek, om tokenet blev fundet
if (is.null(token_banegaard)) {
  stop("Kunne ikke finde __RequestVerificationToken i HTML.")
} else {
  print(paste("Fundet token:", token_banegaard))
}

# Step 2: Send POST-anmodning med tokenet
banegaard_table_res <- POST(
  url = paste0(base_url, "/Luftdata/Presentation/table/MainTable/Aarhus/AARH3"),
  add_headers(
    "X-Requested-With" = "XMLHttpRequest",
    "Content-Type" = "application/x-www-form-urlencoded",
    "Referer" = url_banegaard,
    "Origin" = base_url,
    "User-Agent" = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/130.0.0.0 Safari/537.36"
  ),
  body = list(
    "__RequestVerificationToken" = token_banegaard
  ),
  encode = "form"
)

# Tilføj en forsinkelse på 5 sekunder
Sys.sleep(5)

# Tjek status på POST-anmodningen
if (status_code(banegaard_table_res) != 200) {
  stop(paste("POST-anmodning fejlede med statuskode:", status_code(banegaard_table_res)))
}

# Step 3: Parse HTML-svar for at finde tabellen
content_banegaard <- content(banegaard_table_res, "text")
banegaard_html <- read_html(content_banegaard)

# Udtræk tabellen som en dataframe
banegaard_table <- banegaard_html %>%
  html_table(fill = TRUE)

# Step 4: Gem tabellen som dataframe
if (length(banegaard_table) == 0) {
  stop("Ingen tabeller fundet i HTML-svaret.")
} else {
  banegaard_dataframe <- as.data.frame(banegaard_table[[1]])  # Konverter tabel til dataframe
  print("Fundet tabel som dataframe:")
  print(banegaard_dataframe)
}


banegaard_dataframe$ScrapeTime <- Sys.time()

names(banegaard_dataframe)[names(banegaard_dataframe) == "Målt (starttid)"] <- "starttid"



url_risoe <- "https://envs2.au.dk/Luftdata/Presentation/table/Rural/RISOE"

# Step 1: Send GET-anmodning for at hente siden og finde token
initial_res_risoe <- GET(url_risoe)
initial_html_risoe <- read_html(content(initial_res_risoe, "text"))

# Find __RequestVerificationToken i HTML'en
token_risoe <- initial_html_risoe %>%
  html_element("input[name='__RequestVerificationToken']") %>%
  html_attr("value")

# Tjek, om tokenet blev fundet
if (is.null(token_risoe)) {
  stop("Kunne ikke finde __RequestVerificationToken i HTML.")
} else {
  print(paste("Fundet token:", token_risoe))
}

# Step 2: Send POST-anmodning med tokenet
risoe_table_res <- POST(
  url = paste0(base_url, "/Luftdata/Presentation/table/MainTable/Rural/RISOE"),
  add_headers(
    "X-Requested-With" = "XMLHttpRequest",
    "Content-Type" = "application/x-www-form-urlencoded",
    "Referer" = url_risoe,
    "Origin" = base_url,
    "User-Agent" = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/130.0.0.0 Safari/537.36"
  ),
  body = list(
    "__RequestVerificationToken" = token_risoe
  ),
  encode = "form"
)

# Tilføj en forsinkelse på 5 sekunder
Sys.sleep(5)

# Tjek status på POST-anmodningen
if (status_code(risoe_table_res) != 200) {
  stop(paste("POST-anmodning fejlede med statuskode:", status_code(risoe_table_res)))
}

# Step 3: Parse HTML-svar for at finde tabellen
content_risoe <- content(risoe_table_res, "text")
risoe_html <- read_html(content_risoe)

# Udtræk tabellen som en dataframe
risoe_table <- risoe_html %>%
  html_table(fill = TRUE)

# Step 4: Gem tabellen som dataframe
if (length(risoe_table) == 0) {
  stop("Ingen tabeller fundet i HTML-svaret.")
} else {
  risoe_dataframe <- as.data.frame(risoe_table[[1]])  # Konverter tabel til dataframe
  print("Fundet tabel som dataframe:")
  print(risoe_dataframe)
}


risoe_dataframe$ScrapeTime <- Sys.time()

names(risoe_dataframe)[names(risoe_dataframe) == "Målt (starttid)"] <- "starttid"


# Opret forbindelse til MySQL
con <- dbConnect(
  RMySQL::MySQL(),
  dbname = "miljødata",          # Navn på databasen
  host = "localhost",        # Adresse til databasen
  port = 3306,               # Porten (standard er 3306 for MySQL)
  user = "root",       # Dit MySQL-brugernavn
  password = "1234567891" # Dit MySQL-password
)






# Her loader jeg HCandersen tabellen 


# Vælg kun de nødvendige kolonner
colnames(hcab_dataframe)
data_to_insert <- hcab_dataframe %>%
  dplyr::select(starttid = starttid, CO = CO, NO2 = NO2, NOX = NOX, SO2 = SO2,
                O3 = O3, PM10 = PM10, `PM2.5` = `PM2.5`,
                ScrapeTime = ScrapeTime)


data_to_insert$CO <- suppressWarnings(as.numeric(gsub(",", ".", as.character(data_to_insert$CO))))
data_to_insert$NO2 <- suppressWarnings(as.numeric(gsub(",", ".", as.character(data_to_insert$NO2))))
data_to_insert$NOX <- suppressWarnings(as.numeric(gsub(",", ".", as.character(data_to_insert$NOX))))
data_to_insert$SO2 <- suppressWarnings(as.numeric(gsub(",", ".", as.character(data_to_insert$SO2))))
data_to_insert$O3 <- suppressWarnings(as.numeric(gsub(",", ".", as.character(data_to_insert$O3))))
data_to_insert$PM10 <- suppressWarnings(as.numeric(gsub(",", ".", as.character(data_to_insert$PM10))))
data_to_insert$`PM2.5` <- suppressWarnings(as.numeric(gsub(",", ".", as.character(data_to_insert$`PM2.5`))))


# Loop til tabel 1: HCandersen
for (i in 1:nrow(data_to_insert)) {
  tryCatch({
    row <- data_to_insert[i, ]
    
    # Konverter datetime-felter til MySQL-kompatibelt format eller NULL
    starttid <- ifelse(is.na(row$starttid), "NULL", 
                       sprintf("'%s'", format(as.POSIXct(row$starttid, format = "%d-%m-%Y %H:%M"), "%Y-%m-%d %H:%M:%S")))
    ScrapeTime <- ifelse(is.na(row$ScrapeTime), "NULL", 
                         sprintf("'%s'", format(as.POSIXct(row$ScrapeTime, format = "%d-%m-%Y %H:%M"), "%Y-%m-%d %H:%M:%S")))
    
    # Håndter numeriske værdier
    CO <- ifelse(is.na(row$CO), "NULL", sprintf("%.2f", as.numeric(row$CO)))
    NO2 <- ifelse(is.na(row$NO2), "NULL", sprintf("%.2f", as.numeric(row$NO2)))
    NOX <- ifelse(is.na(row$NOX), "NULL", sprintf("%.2f", as.numeric(row$NOX)))
    SO2 <- ifelse(is.na(row$SO2), "NULL", sprintf("%.2f", as.numeric(row$SO2)))
    O3 <- ifelse(is.na(row$O3), "NULL", sprintf("%.2f", as.numeric(row$O3)))
    PM10 <- ifelse(is.na(row$PM10), "NULL", sprintf("%.2f", as.numeric(row$PM10)))
    `PM2.5` <- ifelse(is.na(row$`PM2.5`), "NULL", sprintf("%.2f", as.numeric(row$`PM2.5`)))
    
    # Byg SQL-query
    query <- sprintf(
      "INSERT INTO HCandersen (starttid, CO, NO2, NOX, SO2, O3, PM10, `PM2.5`, ScrapeTime) 
      VALUES (%s, %s, %s, %s, %s, %s, %s, %s, %s)",
      starttid, CO, NO2, NOX, SO2, O3, PM10, `PM2.5`, ScrapeTime
    )
    
    # Udfør query
    dbExecute(con, query)
  }, error = function(e) {
    cat("Fejl ved indsættelse i HCandersen, række:", i, "\n", e$message, "\n")
  })
}






# Her loader jeg Anholt tabellen 


# Vælg kun de nødvendige kolonner
colnames(anholt_dataframe)
data_to_insert <- anholt_dataframe %>%
  dplyr::select(starttid = starttid, NO2 = NO2, NOX = NOX,
                ScrapeTime = ScrapeTime)


data_to_insert$NO2 <- suppressWarnings(as.numeric(gsub(",", ".", as.character(data_to_insert$NO2))))
data_to_insert$NOX <- suppressWarnings(as.numeric(gsub(",", ".", as.character(data_to_insert$NOX))))


# Loop til tabel 2: anholt
for (i in 1:nrow(data_to_insert)) {
  tryCatch({
    row <- data_to_insert[i, ]
    
    # Konverter datetime-felter til MySQL-kompatibelt format eller NULL
    starttid <- ifelse(is.na(row$starttid), "NULL", 
                       sprintf("'%s'", format(as.POSIXct(row$starttid, format = "%d-%m-%Y %H:%M"), "%Y-%m-%d %H:%M:%S")))
    ScrapeTime <- ifelse(is.na(row$ScrapeTime), "NULL", 
                         sprintf("'%s'", format(as.POSIXct(row$ScrapeTime, format = "%d-%m-%Y %H:%M"), "%Y-%m-%d %H:%M:%S")))
    
    # Håndter numeriske værdier
    NO2 <- ifelse(is.na(row$NO2), "NULL", sprintf("%.2f", as.numeric(row$NO2)))
    NOX <- ifelse(is.na(row$NOX), "NULL", sprintf("%.2f", as.numeric(row$NOX)))
    
    # Byg SQL-query
    query <- sprintf(
      "INSERT INTO anholt (starttid, NO2, NOX, ScrapeTime) 
      VALUES (%s, %s, %s, %s)",
      starttid, NO2, NOX, ScrapeTime
    )
    
   
    # Udfør query
    dbExecute(con, query)
  }, error = function(e) {
    cat("Fejl ved indsættelse i anholt, række:", i, "\n", e$message, "\n")
  })
}





# Her loader jeg banegaard tabellen 

# Vælg kun de nødvendige kolonner
colnames(banegaard_dataframe)
data_to_insert <- banegaard_dataframe %>%
  dplyr::select(starttid = starttid, CO = CO, NO2 = NO2, NOX = NOX,
                ScrapeTime = ScrapeTime)


data_to_insert$CO <- suppressWarnings(as.numeric(gsub(",", ".", as.character(data_to_insert$CO))))
data_to_insert$NO2 <- suppressWarnings(as.numeric(gsub(",", ".", as.character(data_to_insert$NO2))))
data_to_insert$NOX <- suppressWarnings(as.numeric(gsub(",", ".", as.character(data_to_insert$NOX))))


# Loop til tabel 3: banegaard
for (i in 1:nrow(data_to_insert)) {
  tryCatch({
    row <- data_to_insert[i, ]
    
    # Konverter datetime-felter til MySQL-kompatibelt format eller NULL
    starttid <- ifelse(is.na(row$starttid), "NULL", 
                       sprintf("'%s'", format(as.POSIXct(row$starttid, format = "%d-%m-%Y %H:%M"), "%Y-%m-%d %H:%M:%S")))
    ScrapeTime <- ifelse(is.na(row$ScrapeTime), "NULL", 
                         sprintf("'%s'", format(as.POSIXct(row$ScrapeTime, format = "%d-%m-%Y %H:%M"), "%Y-%m-%d %H:%M:%S")))
    
    # Håndter numeriske værdier
    CO <- ifelse(is.na(row$CO), "NULL", sprintf("%.2f", as.numeric(row$CO)))
    NO2 <- ifelse(is.na(row$NO2), "NULL", sprintf("%.2f", as.numeric(row$NO2)))
    NOX <- ifelse(is.na(row$NOX), "NULL", sprintf("%.2f", as.numeric(row$NOX)))
    
    
    # Byg SQL-query
    query <- sprintf(
      "INSERT INTO banegaard (starttid, CO, NO2, NOX, ScrapeTime) 
      VALUES (%s, %s, %s, %s, %s)",
      starttid, CO, NO2, NOX, ScrapeTime
    )
    
    # Udfør query
    dbExecute(con, query)
  }, error = function(e) {
    cat("Fejl ved indsættelse i banegaard, række:", i, "\n", e$message, "\n")
  })
}





# Her loader jeg risoe tabellen 

# Vælg kun de nødvendige kolonner
colnames(risoe_dataframe)
data_to_insert <- risoe_dataframe %>%
  dplyr::select(starttid = starttid, CO = CO, NO2 = NO2, NOX = NOX, O3 = O3, PM10 = PM10,
                ScrapeTime = ScrapeTime)


data_to_insert$CO <- suppressWarnings(as.numeric(gsub(",", ".", as.character(data_to_insert$CO))))
data_to_insert$NO2 <- suppressWarnings(as.numeric(gsub(",", ".", as.character(data_to_insert$NO2))))
data_to_insert$NOX <- suppressWarnings(as.numeric(gsub(",", ".", as.character(data_to_insert$NOX))))
data_to_insert$O3 <- suppressWarnings(as.numeric(gsub(",", ".", as.character(data_to_insert$O3))))
data_to_insert$PM10 <- suppressWarnings(as.numeric(gsub(",", ".", as.character(data_to_insert$PM10))))


# Loop til tabel 4: risoe
for (i in 1:nrow(data_to_insert)) {
  tryCatch({
    row <- data_to_insert[i, ]
    
    # Konverter datetime-felter til MySQL-kompatibelt format eller NULL
    starttid <- ifelse(is.na(row$starttid), "NULL", 
                       sprintf("'%s'", format(as.POSIXct(row$starttid, format = "%d-%m-%Y %H:%M"), "%Y-%m-%d %H:%M:%S")))
    ScrapeTime <- ifelse(is.na(row$ScrapeTime), "NULL", 
                         sprintf("'%s'", format(as.POSIXct(row$ScrapeTime, format = "%d-%m-%Y %H:%M"), "%Y-%m-%d %H:%M:%S")))
    
    # Håndter numeriske værdier
    CO <- ifelse(is.na(row$CO), "NULL", sprintf("%.2f", as.numeric(row$CO)))
    NO2 <- ifelse(is.na(row$NO2), "NULL", sprintf("%.2f", as.numeric(row$NO2)))
    NOX <- ifelse(is.na(row$NOX), "NULL", sprintf("%.2f", as.numeric(row$NOX)))
    O3 <- ifelse(is.na(row$O3), "NULL", sprintf("%.2f", as.numeric(row$O3)))
    PM10 <- ifelse(is.na(row$PM10), "NULL", sprintf("%.2f", as.numeric(row$PM10)))
    
    # Byg SQL-query
    query <- sprintf(
      "INSERT INTO risoe (starttid, CO, NO2, NOX, O3, PM10, ScrapeTime) 
      VALUES (%s, %s, %s, %s, %s, %s, %s)",
      starttid, CO, NO2, NOX, O3, PM10, ScrapeTime
    )
    
    # Udfør query
    dbExecute(con, query)
  }, error = function(e) {
    cat("Fejl ved indsættelse i risoe, række:", i, "\n", e$message, "\n")
  })
}


