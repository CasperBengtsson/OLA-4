library(rvest)
library(chromote)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)

# 1. Start en headless browser
b <- ChromoteSession$new()

# Udvid den usynlige skærm til 1920x1080 så Angular tegner alle tilbud
b$Emulation$setDeviceMetricsOverride(width = 1920, height = 1080, deviceScaleFactor = 1, mobile = FALSE)

# 2. Gå til Power-kampagnesiden
url <- "https://www.power.dk/kampagne/aktuelt/"
b$Page$navigate(url)
b$view()

# 3. VIGTIGT: Giv Angular tid til at hente data ned og bygge produkterne på skærmen
message("Venter på at Power bygger siden...")
Sys.sleep(8) # Lidt ekstra tid for at være sikker

# 4. Træk den færdigbyggede HTML ud
html <- b$Runtime$evaluate("document.documentElement.outerHTML")$result$value
page <- read_html(html)

# 5. Find alle produkt-containere på siden
power_tilbud <- page |> html_elements("article.product-item")

# 6. Udtræk data fra de specifikke elementer
df_power_raa <- data.frame(
  Navn = power_tilbud |> html_element("h3.h6") |> html_text2(),
  
  # OPPDATERET STJERNE-UDTRÆK: Vi målretter den skjulte aria-label, der indeholder den rigtige tekst
  Stjerner = power_tilbud |> html_element("div[role='img']") |> html_attr("aria-label"),
  
  # SPAR mærkatet
  Besparelse = power_tilbud |> html_element("div.savings-label") |> html_text2(),
  
  # Den aktuelle pris
  Pris = power_tilbud |> html_element("data.main-price") |> html_text2(),
  
  # Førpris og datoperiode
  Foerpris_og_dato = power_tilbud |> html_element("pwr-product-previous-price-disclaimer div.text-truncate") |> html_text2(),
  
  # Lagerstatus 
  Lagerstatus = power_tilbud |> html_element("pwr-product-stock-label span") |> html_text2(),
  
  # NYT: Hent de tre specifikations-linjer. 
  # Vi bruger sapply for at samle de tre linjer (subtitle-2) med et komma
  Specifikationer = sapply(power_tilbud, function(node) {
    # Finder alle de små info-linjer inde i den specifikke bil
    info_linjer <- node |> html_elements(".product-item-arguments-container .subtitle-2") |> html_text2()
    # Samler dem til én pæn tekststreng adskilt af komma
    paste(info_linjer, collapse = ", ")
  }),
  
  stringsAsFactors = FALSE
)

# Luk browseren ned
b$close()

# Tjek resultatet
# View(df_power_raa)

# 7. Rens dataene så vi får rene tal og datoer
df_power_renset <- df_power_raa |>
  mutate(
    # --- 1. UDTRÆK FRA NAVN ---
    # Fanger et tal efterfulgt af "GB" (eks. "128 GB")
    Kapacitet_GB = str_extract(Navn, "\\d+\\s?GB"),
    
    # Fanger teksten EFTER det sidste komma (eks. "sort", "grå"). Giver NA hvis der intet komma er.
    Farve = str_extract(Navn, "(?<=, )[^,]+$"),
    
    # Rens selve Navn for farven (klipper alt væk fra og med det sidste komma), så navnet er pænt
    Navn = str_remove(Navn, ",\\s*[^,]+$"),
    
    # --- 2. TALKONVERTERING (Dine eksisterende) ---
    Besparelse_kr = as.numeric(str_remove_all(Besparelse, "[^0-9]")),
    Pris_kr = as.numeric(str_remove_all(Pris, "[^0-9]")),
    Foerpris_kr = as.numeric(str_remove_all(str_extract(Foerpris_og_dato, "(?<=Førpris: )[0-9\\.]+"), "\\.")),
    Startdato = str_extract(Foerpris_og_dato, "\\d{1,2}/\\d{1,2}"),
    Slutdato = str_extract(Foerpris_og_dato, "(?<=- )\\d{1,2}/\\d{1,2}"),
    
    # EKSTRA: Træk stjernetallet ud (eks. "Bedømmelse 4,4 ud af 5" -> 4.4)
    # Konverterer det danske komma til punktum, så R forstår det som et decimaltal
    Stjerner_tal = as.numeric(str_replace(str_extract(Stjerner, "\\d+,\\d+"), ",", "."))
  ) |>
  
  # --- 3. OPDELING AF SPECIFIKATIONER ---
  # Vi deler kolonnen op i 3 nye (Spec_1, Spec_2, Spec_3) der hvor den møder ", • "
  separate(Specifikationer, into = c("Spec_1", "Spec_2", "Spec_3"), sep = ", • ", extra = "merge", fill = "right") |>
  
  # Fjerner den allersidste rest af punkttegn fra den første specifikation
  mutate(Spec_1 = str_remove(Spec_1, "^• ")) |> 
  
  # Fjerner de oprindelige, rodede kolonner vi ikke længere har brug for
  select(-Besparelse, -Pris, -Foerpris_og_dato, -Stjerner) |>
  
  # --- 4. DEN PERFEKTE RÆKKEFØLGE ---
  select(
    Navn,
    Pris_kr,
    Besparelse_kr,
    Foerpris_kr,
    Startdato,
    Slutdato,
    Stjerner_tal,
    Farve,
    Kapacitet_GB,
    Lagerstatus,
    Spec_1,
    Spec_2,
    Spec_3
  )

# Gemmer en CSV-fil
write.csv(df_power_renset, file = "power_tilbud_1.3.csv", row.names = FALSE, na = "")

# Gemmer en RDS-fil
write_rds(df_power_renset, "Power_tilbud.rds")