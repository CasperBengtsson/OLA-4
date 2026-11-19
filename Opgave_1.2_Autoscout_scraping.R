library(rvest)
library(chromote)
library(dplyr)
library(tidyr)
library(stringr)

# 1. Base URL - Husk at din base_url skal ende på "page="
base_url <- "https://www.autoscout24.de/lst/ft_elektro?cy=D&damaged_listing=exclude&desc=0&ocs_listing=include&powertype=kw&sort=standard&ustate=N%2CU&atype=C&page="

# 2. Start den usynlige browser
b <- ChromoteSession$new()
b$Page$navigate(paste0(base_url, 1))
b$view()

#=============================================================================
# Fuld scraping af elbiler fra Autoscout24
#=============================================================================
antal_sider <- 200

autoscout_liste <- list()

for (i in 1:antal_sider) {
  
  message("Henter side ", i, " af ", antal_sider)
  
  # Auto-genstart af browser for at forhindre crash
  fejl_indlaesning <- tryCatch({
    b$Page$navigate(paste0(base_url, i))
    Sys.sleep(sample(4:6, 1)) # Lidt tilfældig pause snyder bot-filteret
    FALSE
  }, error = function(e) { TRUE })
  
  if (fejl_indlaesning) {
    message("Fejl ved side ", i, ". Starter browser forfra...")
    try(b$close(), silent = TRUE)
    b <- ChromoteSession$new()
    Sys.sleep(3)
    b$Page$navigate(paste0(base_url, i))
    Sys.sleep(5)
  }
  
  # HTML træk
  html <- b$Runtime$evaluate("document.documentElement.outerHTML")$result$value
  page <- read_html(html)
  
  biler_html <- page |> html_elements("article")
  
  # Hvis der ikke findes biler på siden, stopper loopet
  if(length(biler_html) == 0) {
    message("Ingen biler fundet på side ", i, ". Stopper loopet.")
    break
  }
  
  # Udtræk af standardfelter
  titler <- biler_html |> html_element('[class*="ListItemTitle_wrapper"]') |> html_text2()
  priser <- biler_html |> html_element('[class*="ListItemPrice_wrapper"]') |> html_text2()
  detaljer <- biler_html |> html_element('[class*="ListItemVehicleDetails_pillContainer"]') |> html_text2()
  
  # Udtræk af forhandler (navn og by kombineret)
  forhandlere <- biler_html |> html_element('[class*="DeclutteredListItemDefaultView_dealer"]') |> html_text2()
  
  # --- ULTIMATIV LINK-UDTRÆK (BYG LINKET MANUELT) ---
  links_fuld <- sapply(biler_html, function(bil_node) {
    
    # 1. Træk det unikke ID (ofte kaldet data-guid eller id) ud af selve <article>
    bil_id <- bil_node |> html_attr("data-guid")
    if (is.na(bil_id)) bil_id <- bil_node |> html_attr("id")
    
    # 2. Hvis vi fangede et ID, bygger vi Autoscouts "bagdørs"-link automatisk
    if (!is.na(bil_id) && str_length(bil_id) > 10) {
      return(paste0("https://www.autoscout24.de/angebote/-", bil_id))
    }
    
    # 3. Nød-fallback: Hvis ID'et mangler, suger vi det længste link ud af kassen
    alle_links <- bil_node |> html_elements("a") |> html_attr("href")
    alle_links <- alle_links[!is.na(alle_links)]
    gode_links <- alle_links[str_length(alle_links) > 25 & !str_detect(alle_links, "kundenbewertung")]
    
    if (length(gode_links) > 0) {
      link <- gode_links[1]
      if (!str_starts(link, "http")) link <- paste0("https://www.autoscout24.de", link)
      return(str_remove(link, "\\?.*"))
    }
    
    return(NA)
  })
  
  # Gemmer siden i listen med det sikre ID
  temp_df <- data.frame(
    carid = paste0("AS_", i, "_", seq_along(titler)),
    Titel = titler, 
    Pris = priser, 
    Detaljer = detaljer,
    Forhandler = forhandlere,
    Link = links_fuld,
    stringsAsFactors = FALSE
  )
  
  autoscout_liste[[i]] <- temp_df
}

# Samler alle sider til én stor dataframe
autoscout_df <- bind_rows(autoscout_liste)

# Gemmer som RDS
saveRDS(autoscout_df, file = "autoscout_scraping.rds")

autoscout_df <- readRDS("autoscout_scraping.rds")

# 3. Gemmer som CSV
write.csv(autoscout_df, file = "autoscout_scraping.csv", row.names = FALSE, na = "")

#============================================================================
# Rensning af scrapet datasæt
#============================================================================
# --- Indlæser data og fikser tomme felter ---
df_as <- read.csv("autoscout_scraping.csv", stringsAsFactors = FALSE)

# Omdanner tomme tekstfelter til rigtige NA-værdier
df_as <- df_as |> 
  mutate(across(where(is.character), ~na_if(str_trim(.), "")))


# --- Opdeler "Titel" til Mærke og Model ---
df_as <- df_as |>
  separate(Titel, into = c("Mærke", "Model"), sep = " ", extra = "merge", fill = "right") |>
  mutate(Model = str_squish(Model)) # Fjerner ekstra usynlige mellemrum


# --- Renser og konverterer Pris (Fjerner det mærkelige ekstra 1-tal) ---
df_as <- df_as |>
  mutate(
    # Hvis der kommer et 4. tal (eks ".9411"), FJERNER den det sidste tal.
    Pris = str_replace(Pris, "(\\.\\d{3})\\d", "\\1"),
    
    # Fjerner vi bogstaver, punktummer og €-tegn
    Pris = str_remove_all(Pris, "[^0-9]"),
    
    # Gør til numerisk og kalder den Pris_euro
    Pris_euro = as.numeric(Pris)
  ) |>
  select(-Pris) # Fjerner den gamle rodede kolonne


# --- Udtrækker data fra den rodede "Detaljer" kolonne ---
# Bruger regex til at "fiske" de specifikke værdier ud uanset linjenummer
df_as <- df_as |>
  mutate(
    # Fanger formatet MM/YYYY (eks. "05/2023"). Fabriksnye biler får automatisk NA.
    Registrering = str_extract(Detaljer, "\\d{2}/\\d{4}"),
    
    # Fanger tallet (og evt. punktum) der står lige før "km"
    Kilometer_kørt = str_extract(Detaljer, "[0-9\\.]+(?=\\s*km)"),
    
    # Fanger tallet der står lige før "PS"
    Hestekræfter = str_extract(Detaljer, "\\d+(?=\\s*PS)"),
    
    # Fanger tallet der står lige før "kW"
    Batterikapacitet_kWh = str_extract(Detaljer, "\\d+(?=\\s*kW)")
  )


# --- Gør detaljerne numeriske og split dato ---
df_as <- df_as |>
  mutate(
    # Fjerner punktummet fra kilometer (eks. "31.586" bliver 31586) og gør til tal
    Kilometer_kørt = as.numeric(str_remove_all(Kilometer_kørt, "\\.")),
    Hestekræfter = as.numeric(Hestekræfter),
    Batterikapacitet_kWh = as.numeric(Batterikapacitet_kWh)
  ) |>
  # Deler Registrering i måned og år ligesom i Bilbasen-scriptet
  separate(Registrering, into = c("Registrering_Måned", "Registrering_År"), sep = "/", convert = TRUE, fill = "right")

# --- Omregnner valuta fra Euro til DKK ---
df_as <- df_as |>
  mutate(
    # Ganger Euro med 7,46 og runder af til 0 decimaler (hele kroner)
    Pris_DKK = round(Pris_euro * 7.46, 0)
  )

# --- Ryder op og fjerner de gamle kolonner ---
df_as_renset <- df_as |>
  select(-Detaljer)

# --- Gemmer det rensede Autoscout-datasæt ---

# Gem som RDS
saveRDS(df_as_renset, file = "autoscout_renset.rds")

df_as_renset <- readRDS("autoscout_renset.rds")

# Gem som CSV
# row.names = FALSE fjerner overflødige rækkenumre.
# na = "" konverterer dine NA-værdier til blanke felter, som SQL læser som NULL.
write.csv(df_as_renset, file = "autoscout_renset.csv", row.names = FALSE, na = "")