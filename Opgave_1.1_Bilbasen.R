library(rvest)
library(chromote)
library(dplyr)
library(stringr)
library(tidyr)

#===============================================================================
# Trin 1 - Web Scraping af annoncer på BilBasen
#===============================================================================

base_url_start <- "https://www.bilbasen.dk/brugt/bil?fuel=3&includeengroscvr=true&includeleasing=false&page="
sider_der_skal_scrapes <- 1:95
alle_elbiler_liste <- list()

b <- ChromoteSession$new()
b$Page$navigate(paste0(base_url_start, "1")) # Åbner side 1 som start
b$view() 

for (i in sider_der_skal_scrapes) {
  url <- paste0(base_url_start, i)
  message("Trin 1: Scraper side ", i, " - ", url)
  
  #Browseren navigerer nu til den dynamiske 'url'
  b$Page$navigate(url)
  Sys.sleep(5) 
  
  html <- b$Runtime$evaluate("document.documentElement.outerHTML")$result$value
  page <- read_html(html)
  
  annoncer <- page |> html_elements('[data-e2e="search-result-card"]')
  
  if (length(annoncer) == 0) break
  
  titler <- annoncer |> html_element('[data-e2e="search-result-make-model"]') |> html_text2()
  priser_raw <- annoncer |> html_element('[data-e2e="search-result-price"]') |> html_text2()
  detaljer_raw <- annoncer |> html_element('[class^="Listing_details"]') |> html_text2()
  features_raw <- annoncer |> html_element('[class^="Listing_properties"]') |> html_text2()
  forhandler_raw <- annoncer |> html_element('[class^="Listing_location"]') |> html_text2()
  
  links_raw <- annoncer |> html_element('[data-e2e="search-result-link"]') |> html_attr("href")
  fulde_links <- ifelse(str_starts(links_raw, "/"), paste0("https://www.bilbasen.dk", links_raw), links_raw)
  bil_id <- str_extract(links_raw, "\\d+$")
  
  df_side <- data.frame(
    carid = bil_id, Titel = titler, Pris_raw = priser_raw, 
    Detaljer_raw = detaljer_raw, Features_raw = features_raw, 
    Forhandler = forhandler_raw, Link = fulde_links, 
    scrapedate = as.Date(Sys.Date()), stringsAsFactors = FALSE
  )
  
  alle_elbiler_liste[[i]] <- df_side
  Sys.sleep(sample(3:5, 1))
}

# Samler & renser Trin 1 data
rå_data <- bind_rows(alle_elbiler_liste) |> filter(!is.na(Titel))

færdig_elbil_data <- rå_data |> 
  mutate(
    Pris = as.numeric(str_remove_all(Pris_raw, "[^0-9]")),
    Kilometer_tekst = str_extract(Detaljer_raw, "\\d{1,3}(\\.\\d{3})*(?=\\s*(?i)km(?! rækkevidde))"),
    Kilometer = as.numeric(str_remove_all(Kilometer_tekst, "\\.")),
    Registrering = str_extract(Detaljer_raw, "\\d{1,2}/\\d{4}"),
    Aargang = as.numeric(str_extract(Registrering, "\\d{4}")),
    Raekkevidde_ny = as.numeric(str_extract(Detaljer_raw, "\\d+(?=\\s*(?i)km rækkevidde)")),
    Raekkevidde_gammel = as.numeric(str_extract(Features_raw, "\\d+(?=\\s*(?i)km)")),
    Raekkevidde_km = coalesce(Raekkevidde_ny, Raekkevidde_gammel)
  ) |> 
  select(carid, Titel, Aargang, Registrering, Kilometer, Pris, Raekkevidde_km, Forhandler, Link, scrapedate)

# Bekræft at Trin 1 lykkedes ved at tjekke antallet af rækker
print(paste("Trin 1 fuldført. Fandt", nrow(færdig_elbil_data), "biler."))

saveRDS(færdig_elbil_data, file = "scrapet_biler_korrekt.rds")

færdig_elbil_data <- readRDS("scrapet_biler_korrekt.rds")

#===============================================================================
# Trin 2 - Web scraping af detaljer under hver annonce (INKL. FORHANDLER)
#===============================================================================

# 1. Opretter listen
dybe_detaljer_liste <- list()

# 2. Starter Chromote browseren
b <- ChromoteSession$new()
b$Page$navigate(bil_url)
b$view()

# 3. Kør det sikrede loop fra biler i trin 1
for (i in 1:nrow(færdig_elbil_data)) {
  
  bil_url <- færdig_elbil_data$Link[i]
  bil_id <- færdig_elbil_data$carid[i]
  
  message("Trin 2: Henter dybe specs (", i, "/", nrow(færdig_elbil_data), ") - ID: ", bil_id)
  
  # --- TRYCATCH: Undgår at scriptet crasher ved netværksfejl ---
  fejl_ved_indlæsning <- tryCatch({
    b$Page$navigate(bil_url)
    Sys.sleep(4) # Giver Bilbasen tid til at vise tabellen
    FALSE        # Bliver FALSE, hvis alt går godt
  }, error = function(e) {
    message("Fejl/Timeout ved bil ", i, " (ID: ", bil_id, "). Springer over.")
    TRUE         # Bliver TRUE, hvis netværket/siden fejlede
  })
  
  # Afbryd dette ene loop og gå videre til næste bil ved fejl
  if (fejl_ved_indlæsning) {
    next 
  }
  
  # Trækker HTML'en fra siden:
  html <- b$Runtime$evaluate("document.documentElement.outerHTML")$result$value
  page <- read_html(html)
  
  # --- 1. TRÆK TABELLEN MED SPECS ---
  tabel_html <- page |> html_element('table[data-e2e="car-facts-table-preview"]')
  
  if (!is.na(tabel_html)) {
    tabel_df <- tabel_html |> html_table()
    colnames(tabel_df) <- c("Egenskab", "Vaerdi")
    tabel_bred <- tabel_df |> pivot_wider(names_from = Egenskab, values_from = Vaerdi)
  } else {
    tabel_bred <- data.frame() 
  }
  
  # --- 2. TRÆK ANMELDELSER OG FORHANDLER ---
  stjerner <- page |> html_element('[class*="ratingsNumber"]') |> html_text2()
  anmeldelser <- page |> html_element('[class*="ratingsComponent-reviews"]') |> html_text2()
  
  # NYT: Trækker forhandlernavnet
  forhandler_navn <- page |> html_element('[data-e2e="vip-section-dealer-info"] h2') |> html_text2()
  
  # --- 3. SAML DET HELE ---
  tabel_bred <- tabel_bred |> mutate(
    carid = bil_id, 
    Forhandler = forhandler_navn, 
    Stjerner = stjerner, 
    Anmeldelser_tekst = anmeldelser,
    .before = 1 # Sikrer at disse vigtige ID- og navnekolonner ligger først
  )
  
  # Gemmer bilen på plads nummer 'i' i den eksisterende liste
  dybe_detaljer_liste[[i]] <- tabel_bred
  
  # Anti-bot pause
  Sys.sleep(sample(3:5, 1)) 
}

b$close() # Lukker browseren når alt er skrabet

# Samler listen til én dataframe
trin2_raa <- bind_rows(dybe_detaljer_liste)

# Gem den rå version lokalt med det samme
saveRDS(trin2_raa, file = "bilbasen_trin2_raa.rds")

# Samler Trin 2 dataene
dybe_data_samlet <- bind_rows(dybe_detaljer_liste)

# 3. Flet de unikke datasæt og rens tallene
endeligt_kombineret_datasæt <- færdig_elbil_data |> 
  left_join(dybe_data_samlet, by = "carid") |> 
  mutate(
    # NYT MØNSTER: Trækker kun tal (og evt. komma) ud. F.eks. "77,4" fra "77,4 kWh (netto)"
    Batterikapacitet_ren = as.numeric(str_replace(str_extract(Batterikapacitet, "\\d+(,\\d+)?"), ",", ".")),
    
    Ydelse_ren = as.numeric(str_extract(Ydelse, "\\d+(?=\\s*hk)")),
    Acceleration_ren = as.numeric(str_replace(str_extract(Acceleration, "\\d+(,\\d+)?"), ",", ".")), # Samme trick anvendt her
    Antal_anmeldelser = as.numeric(str_extract(Anmeldelser_tekst, "\\d+")),
    Stjerner_ren = as.numeric(Stjerner)
  )

# 4. Gemmer det endelige samlet datasæt
saveRDS(endeligt_kombineret_datasæt, file = "elbiler_samlet_bilbasen.rds")
write.csv(endeligt_kombineret_datasæt, file = "elbiler_bilbasen.csv", row.names = FALSE, na = "")

#===============================================================================
# Rensning af Bilbasen data
#===============================================================================

# --- Indlæser datasæt ---
df <- read.csv("elbiler_bilbasen.csv", stringsAsFactors = FALSE)

# --- Konvertere tomme strenge til ægte NA-værdier ---
df <- df |> 
  mutate(across(where(is.character), ~na_if(str_trim(.), "")))

# --- Fjerner fuldstændig de tomme kolonner ---
# Tjekker alle kolonner og sletter dem, hvor alt er NA (som for eksempel Ydelse og Acceleration)
df <- df |>
  select(where(~!all(is.na(.))))


# --- Opdeler "Titel" til Mærke og Variant ---
# Klipper ved første mellemrum. 'extra = "merge"' samler resten af navnet i Variant.
df <- df |>
  separate(Titel, into = c("Mærke", "Variant"), sep = " ", extra = "merge", fill = "right") |>
  mutate(Variant = str_squish(str_replace_all(Variant, "\r\n", " "))) # Fjerner grimme linjeskift


# --- Opdeler "Registrering" til Måned og År ---
# Klipper ved skråstregen. 'convert = TRUE' gør automatisk teksten til rigtige tal.
df <- df |>
  separate(Registrering, into = c("Registrering_Måned", "Registrering_År"), sep = "/", convert = TRUE, fill = "right")


# --- Opdeler "Forhandler Lokation" til By og Område ---
# Klipper ved kommaet for at skille f.eks. "Taastrup" fra "København".
df <- df |>
  separate(Forhandler.x, into = c("Forhandler_By", "Forhandler_Område"), sep = ",\\s*", extra = "merge", fill = "right")


# --- Omdøbning og datatyper ---
df <- df |>
  rename(
    Kilometer_kørt = Kilometer,
    Forhandler = Forhandler.y,
    Batterikapacitet_kWh = Batterikapacitet_ren,
    Registrering =  X1..registrering
  ) |>
  mutate(
    carid = as.numeric(carid),
    Aargang = as.numeric(Aargang)
  )


# --- Fjerner overflødige kolonner ---
# Fjerner de kolonner, vi ikke længere har brug for efter vores opdelinger og rensning.
df_renset <- df |>
  select(-any_of(c("Kilometertal", "Rækkevidde", "Raekkevidde_km", "Batterikapacitet", "Stjerner", "Produceret")))

# --- Gemmer det rensede datasæt ---

# Gem som RDS (Bevarer alle dine numeriske datatyper perfekt til senere brug i R)
saveRDS(df_renset, file = "bilbasen_renset.rds")

fdata <- readRDS("bilbasen_renset.rds")

# Gem som CSV-fil
# row.names = FALSE forhindrer, at R tilføjer en ubrugelig kolonne med rækkenumre.
# na = "" sørger for, at NA-værdier bliver helt tomme i filen, hvilket SQL automatisk oversætter til NULL.
write.csv(df_renset, file = "bilbasen_renset.csv", row.names = FALSE, na = "")