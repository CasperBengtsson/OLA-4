library(dplyr)
library(stringr)
library(ggplot2)

df_bilbasen <- readRDS("bilbasen_renset.rds")
df_autoscout <- readRDS("autoscout_renset.rds")

# --- Klargør Bilbasen ---
df_bilbasen_renset <- df_bilbasen |> 
  mutate(Kilde = "Bilbasen") |> 
  select(
    Kilde,
    Mærke,
    Model = Variant, 
    Pris_DKK = Pris,
    Kilometer_kørt,
    Registrering_År = Aargang,
    Batterikapacitet_kWh
  )

# --- Klargør AutoScout (Renset for scraping-outliere) ---
df_autoscout_renset <- df_autoscout |> 
  mutate(Kilde = "AutoScout24") |> 
  filter(Pris_DKK < 2500000) |> # Fjerner de falske milliard-priser!
  select(
    Kilde,
    Mærke,
    Model,
    Pris_DKK,
    Kilometer_kørt,
    Registrering_År,
    Batterikapacitet_kWh
  )

# --- Stabler de to datasæt oven på hinanden ---
df_samlet <- bind_rows(df_bilbasen_renset, df_autoscout_renset)

# --- Beregner de statistiske nøgletal ---
pris_sammenligning <- df_samlet |>
  group_by(Kilde) |>
  summarise(
    Antal_biler = n(),
    Gennemsnitspris = mean(Pris_DKK, na.rm = TRUE),
    Medianpris = median(Pris_DKK, na.rm = TRUE),
    Min_pris = min(Pris_DKK, na.rm = TRUE),
    Max_pris = max(Pris_DKK, na.rm = TRUE)
  )

print(pris_sammenligning)

# --- Laver et boxplot for at sammenligne prisen ---
ggplot(df_samlet, aes(x = Kilde, y = Pris_DKK, fill = Kilde)) +
  geom_boxplot(alpha = 0.8) +
  scale_y_continuous(labels = scales::comma_format(big.mark = ".", decimal.mark = ",")) +
  labs(
    title = "Den gennemsnitlige pris på elbiler er billigere i Tyskland",
    subtitle = "Sammenligning af udbudspriser (outliere fjernet)",
    x = "Platform",
    y = "Pris (DKK)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

#===============================================================================
# Sammenligning af bilmærker
#===============================================================================
# 1. Ensret mærkenavne på tværs af de to platforme
df_samlet <- df_samlet |>
  mutate(
    Mærke = case_when(
      Mærke == "VW" ~ "Volkswagen",
      Mærke == "Mercedes" ~ "Mercedes-Benz",
      Mærke == "CUPRA" ~ "Cupra",
      Mærke == "smart" ~ "Smart",
      Mærke == "SEAT" ~ "Seat",
      TRUE ~ Mærke
    )
  )

# 2. Gruppér efter Mærke og Kilde (langt format - godt til ggplot)
maerke_long <- df_samlet |>
  group_by(Mærke, Kilde) |>
  summarise(
    Antal = n(),
    Gennemsnitspris = round(mean(Pris_DKK, na.rm = TRUE), 0),
    Medianpris = round(median(Pris_DKK, na.rm = TRUE), 0),
    .groups = "drop"
  )

# 3. Sammenligningstabel side om side (bredt format)
maerke_sammenligning <- maerke_long |>
  pivot_wider(
    names_from = Kilde,
    values_from = c(Antal, Gennemsnitspris, Medianpris)
  ) |>
  # Behold kun mærker med mindst 20 biler på begge platforme, så enkelte biler ikke skævvrider snittet
  filter(Antal_Bilbasen >= 20, Antal_AutoScout24 >= 20) |>
  mutate(
    Forskel_Gns = Gennemsnitspris_Bilbasen - Gennemsnitspris_AutoScout24
  ) |>
  arrange(desc(Antal_Bilbasen))

print(maerke_sammenligning, n = 25)

# Udvælg de 10 største fælles mærker til plottet
top_10_maerker <- maerke_sammenligning |>
  slice_head(n = 10) |>
  pull(Mærke)

maerke_long |>
  filter(Mærke %in% top_10_maerker) |>
  ggplot(aes(x = reorder(Mærke, Gennemsnitspris), y = Gennemsnitspris, fill = Kilde)) +
  geom_col(position = "dodge", alpha = 0.85) +
  coord_flip() +
  scale_y_continuous(labels = scales::comma_format(big.mark = ".", decimal.mark = ",")) +
  labs(
    title = "Gennemsnitspris pr. bilmærke: Bilbasen vs. AutoScout24",
    subtitle = "Top 10 største fælles elbilsmærker (outliere over 2,5 mio. kr. fjernet)",
    x = "Mærke",
    y = "Gennemsnitspris (DKK)",
    fill = "Platform"
  ) +
  theme_minimal()

#===============================================================================
# Dumbbell-diagram
#===============================================================================
plot_data <- maerke_sammenligning |>
  slice_head(n = 10) |>
  mutate(
    Forskel_label = ifelse(
      Forskel_Gns > 0,
      paste0("+", round(Forskel_Gns / 1000, 0), " t.kr."),
      paste0(round(Forskel_Gns / 1000, 0), " t.kr.")
    ),
    max_pris = pmax(Gennemsnitspris_Bilbasen, Gennemsnitspris_AutoScout24)
  )

ggplot(plot_data, aes(y = reorder(Mærke, Gennemsnitspris_Bilbasen))) +
  # Forbindelsesstreg der synliggør "gabet" mellem de to platforme
  geom_segment(
    aes(x = Gennemsnitspris_AutoScout24, xend = Gennemsnitspris_Bilbasen, yend = Mærke),
    color = "grey75", linewidth = 1.5
  ) +
  geom_point(aes(x = Gennemsnitspris_AutoScout24, color = "AutoScout24"), size = 4) +
  geom_point(aes(x = Gennemsnitspris_Bilbasen, color = "Bilbasen"), size = 4) +
  # Prisforskel i tusinde kr. lige til højre for den højeste prik
  geom_text(
    aes(x = max_pris + 8000, label = Forskel_label),
    hjust = 0, size = 3.5, fontface = "bold", color = "grey30"
  ) +
  scale_color_manual(values = c("AutoScout24" = "#E76F51", "Bilbasen" = "#008B8B")) +
  scale_x_continuous(
    labels = scales::comma_format(big.mark = ".", decimal.mark = ","),
    expand = expansion(mult = c(0.05, 0.18))
  ) +
  labs(
    title = "Gennemsnitspris pr. bilmærke: Bilbasen vs. AutoScout24",
    subtitle = "Tallene til højre viser forskellen (Bilbasen minus AutoScout24)",
    x = "Gennemsnitspris (DKK)",
    y = NULL,
    color = "Platform"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    panel.grid.major.y = element_blank()
  )

#===============================================================================
# Søjlediagram
#===============================================================================
maerke_sammenligning |>
  slice_head(n = 10) |>
  mutate(
    Dyrest = ifelse(Forskel_Gns > 0, "Bilbasen dyrest", "AutoScout24 dyrest"),
    label_tekst = ifelse(
      Forskel_Gns > 0,
      paste0("+", scales::comma(Forskel_Gns, big.mark = ".", decimal.mark = ","), " kr."),
      paste0(scales::comma(Forskel_Gns, big.mark = ".", decimal.mark = ","), " kr.")
    )
  ) |>
  ggplot(aes(x = reorder(Mærke, Forskel_Gns), y = Forskel_Gns, fill = Dyrest)) +
  geom_col(alpha = 0.85, width = 0.7) +
  geom_hline(yintercept = 0, color = "grey30", linewidth = 0.8) +
  geom_text(
    aes(
      label = label_tekst,
      hjust = ifelse(Forskel_Gns > 0, -0.1, 1.1)
    ),
    size = 3.5, fontface = "bold", color = "grey20"
  ) +
  coord_flip() +
  scale_fill_manual(values = c("Bilbasen dyrest" = "#008B8B", "AutoScout24 dyrest" = "#E76F51")) +
  scale_y_continuous(
    labels = scales::comma_format(big.mark = ".", decimal.mark = ","),
    expand = expansion(mult = c(0.25, 0.25))
  ) +
  labs(
    title = "Prisforskel pr. bilmærke: Bilbasen minus AutoScout24",
    subtitle = "Højre = Bilbasen dyrest i snit | Venstre = AutoScout24 dyrest i snit",
    x = NULL,
    y = "Gennemsnitlig prisforskel (DKK)",
    fill = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    panel.grid.major.y = element_blank()
  )
