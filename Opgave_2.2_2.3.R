library(dplyr)
library(readr)
library(DBI)
library(RMariaDB)

#===============================================================================
# Opgave 2.2
#===============================================================================

# Opret forbindelse til DBeaver (localhost)
wc <- dbConnect(MariaDB(),
                host = "localhost",
                user = "root",
                password = "Root123",
                db = "cars",  # Indlæs ind til den nye 'cars' database
                port = 3306)

# Klargør datasættet
df_renset <- read.csv("bilbasen_renset.csv", stringsAsFactors = FALSE)
df_renset$scrapedate <- as.Date(Sys.Date()) # Sikrer at der er en dato på

# Splitter data op for at matche relationsdatabasen
df_stamdata <- df_renset |> 
  select(carid, Mærke, Model = Variant, Registrering_Måned, Registrering_År, Kilometer_kørt, Forhandler, Link)

df_prisdata <- df_renset |> 
  select(carid, Pris_DKK = Pris, scrapedate) |> 
  mutate(sold = FALSE)

# Sender data til DBeaver
dbWriteTable(wc, "Car_Details", df_stamdata, append = TRUE, row.names = FALSE)
dbWriteTable(wc, "Car_Pricing", df_prisdata, append = TRUE, row.names = FALSE)

#===============================================================================
# Opgave 2.3
#===============================================================================

df_renset <- df_renset |> 
  rename(
    Model = Variant,
    Pris_DKK = Pris
  )

# --- Laver Kørsel-II ---
koersel_II <- df_renset
koersel_II$scrapedate <- koersel_II$scrapedate + 1 # Fremrykker med en dag
koersel_II$Pris_DKK[1:3] <- koersel_II$Pris_DKK[1:3] * 0.90 # 3 biler får ændret pris
koersel_II <- koersel_II[-(4:8), ] # Sletter 5 biler

# Tilføjer 2 nye biler
nye_biler_mock <- data.frame(
  carid = c("MOCK_1", "MOCK_2"), Mærke = c("Tesla", "VW"), Model = c("Model 3", "ID.4"),
  Registrering_Måned = c(1, 2), Registrering_År = c(2025, 2024),
  Kilometer_kørt = c(1000, 5000), Pris_DKK = c(300000, 250000),
  Forhandler = c("Test", "Test"), Link = c("link", "link"),
  scrapedate = Sys.Date() + 1, stringsAsFactors = FALSE
)
# Gør carid i din oprindelige dataframe til tekst (character), så det matcher VARCHAR i SQL
koersel_II$carid <- as.character(koersel_II$carid)

koersel_II <- bind_rows(koersel_II, nye_biler_mock)

# --- Indlæser den nuværende tilstand fra databasen ---
db_tilstand <- dbGetQuery(wc, "
  SELECT p.carid, p.Pris_DKK FROM Car_Pricing p
  INNER JOIN (SELECT carid, MAX(scrapedate) as max_date FROM Car_Pricing GROUP BY carid) nyeste 
  ON p.carid = nyeste.carid AND p.scrapedate = nyeste.max_date
  WHERE p.sold = FALSE
")

# --- Opdater vores databasen systematisk ---
# A) Nye biler
nye_records <- koersel_II |> filter(!carid %in% db_tilstand$carid)
if(nrow(nye_records) > 0) {
  dbWriteTable(wc, "Car_Details", nye_records |> select(carid, Mærke, Model, Registrering_Måned, Registrering_År, Kilometer_kørt, Forhandler, Link), append = TRUE, row.names = FALSE)
  dbWriteTable(wc, "Car_Pricing", nye_records |> select(carid, Pris_DKK, scrapedate) |> mutate(sold = FALSE), append = TRUE, row.names = FALSE)
}

# B) Ændrede priser
aendrede_priser <- koersel_II |> inner_join(db_tilstand, by = "carid") |> filter(Pris_DKK.x != Pris_DKK.y)
if(nrow(aendrede_priser) > 0) {
  dbWriteTable(wc, "Car_Pricing", aendrede_priser |> select(carid, Pris_DKK = Pris_DKK.x, scrapedate) |> mutate(sold = FALSE), append = TRUE, row.names = FALSE)
}

# C) Solgte biler (manglende i Kørsel-II)
solgte_biler <- db_tilstand |> filter(!carid %in% koersel_II$carid)
if(nrow(solgte_biler) > 0) {
  ids <- paste(sprintf("'%s'", solgte_biler$carid), collapse = ", ")
  dbExecute(wc, paste0("UPDATE Car_Pricing SET sold = TRUE WHERE carid IN (", ids, ")"))
}

# Luk forbindelsen til slut
dbDisconnect(wc)