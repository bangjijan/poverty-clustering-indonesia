# ============================================================
# PROYEK : Klasterisasi Kemiskinan Provinsi Indonesia
# FILE   : 03_mapping.R
# PENULIS: Muhammad Rafli Azizan | Universitas Diponegoro
# TUJUAN : Visualisasi choropleth hasil clustering ke peta
#          Indonesia menggunakan shapefile GADM level 1
#          Input : df_result.rds, scaled_matrix.rds
#          Output: peta PNG + CSV final
# ============================================================


# ── Package ──────────────────────────────────────────────────

library(tidyverse)
library(sf)
library(cluster)

cat("✓ Package loaded\n\n")


# ── Load data hasil clustering ───────────────────────────────

df_result    <- readRDS("output/df_result.rds")
profil_final <- readRDS("output/profil_klaster.rds")

cat("✓ Data clustering loaded —", nrow(df_result), "provinsi\n")
cat("  Klaster:\n")
df_result %>%
  count(cluster, label_klaster) %>%
  print()


# ── Tampilkan profilisasi final ──────────────────────────────

cat("\n=== PROFILISASI FINAL (k=3) ===\n")
df_result %>%
  group_by(cluster, label_klaster) %>%
  summarise(
    n          = n(),
    pct_miskin = round(mean(pct_miskin), 2),
    ipm        = round(mean(ipm),        2),
    tpt        = round(mean(tpt),        2),
    rls        = round(mean(rls),        2),
    ahh        = round(mean(ahh),        2),
    pdrb       = round(mean(pdrb),       2),
    .groups    = "drop"
  ) %>%
  arrange(pct_miskin) %>%
  print(n = Inf, width = Inf)

cat("\nAnggota per klaster:\n")
df_result %>%
  arrange(cluster, provinsi) %>%
  group_by(cluster, label_klaster) %>%
  summarise(daftar = paste(provinsi, collapse = ", "), .groups = "drop") %>%
  print(width = Inf)


# ============================================================
# STEP 1: Download shapefile Indonesia (GADM level 1)
# Coba geodata → fallback rnaturalearth
# ============================================================

cat("\nMendownload shapefile Indonesia (GADM level 1)...\n")

if (!dir.exists("data")) dir.create("data")

indo_sf <- tryCatch({
  pkgs_geo <- c("geodata")
  invisible(lapply(pkgs_geo, function(p) {
    if (!requireNamespace(p, quietly = TRUE))
      install.packages(p, type = "binary")
    library(p, character.only = TRUE)
  }))
  gadm_data <- gadm("IDN", level = 1, path = "data/")
  sf::st_as_sf(gadm_data)

}, error = function(e) {
  message("geodata gagal (", conditionMessage(e), "), mencoba rnaturalearth...")

  tryCatch({
    pkgs_ne <- c("rnaturalearth", "rnaturalearthdata")
    invisible(lapply(pkgs_ne, function(p) {
      if (!requireNamespace(p, quietly = TRUE))
        install.packages(p, type = "binary")
      library(p, character.only = TRUE)
    }))
    ne_states(country = "indonesia", returnclass = "sf")
  }, error = function(e2) {
    message("rnaturalearth juga gagal: ", conditionMessage(e2))
    NULL
  })
})

if (is.null(indo_sf)) {
  cat("\n⚠ Shapefile tidak bisa didownload otomatis.\n")
  cat("Download manual:\n")
  cat("  URL: https://geodata.ucdavis.edu/gadm/gadm4.1/shp/gadm41_IDN_shp.zip\n")
  cat("  Ekstrak ke folder: data/gadm/\n")
  cat("  Kemudian jalankan ulang script ini.\n")
  stop("Shapefile tidak tersedia — hentikan eksekusi.")
}

cat("✓ Shapefile berhasil di-load\n")
cat("  Kolom tersedia:", paste(names(indo_sf), collapse = ", "), "\n")


# ============================================================
# STEP 2: Deteksi kolom nama provinsi & tampilkan
# ============================================================

nama_col <- names(indo_sf)[str_detect(names(indo_sf), "NAME_1|name_1")][1]
cat("\nKolom nama provinsi:", nama_col, "\n")

provinsi_shapefile <- sort(pull(indo_sf, all_of(nama_col)))
cat("\nNama provinsi di shapefile:\n")
print(provinsi_shapefile)


# ============================================================
# STEP 3: Mapping nama shapefile → nama data BPS
#
# PERBAIKAN dari versi sebelumnya:
#   "Bangka-Belitung" (tanda hubung) → "Bangka Belitung" (spasi)
#   sesuai nama aktual di shapefile GADM.
# ============================================================

nama_mapping <- c(
  "Aceh"                = "ACEH",
  "Bali"                = "BALI",
  "Bangka Belitung"     = "KEP BANGKA BELITUNG",   # ← FIX: spasi bukan tanda hubung
  "Banten"              = "BANTEN",
  "Bengkulu"            = "BENGKULU",
  "Gorontalo"           = "GORONTALO",
  "Jakarta Raya"        = "DKI JAKARTA",
  "Jambi"               = "JAMBI",
  "Jawa Barat"          = "JAWA BARAT",
  "Jawa Tengah"         = "JAWA TENGAH",
  "Jawa Timur"          = "JAWA TIMUR",
  "Kalimantan Barat"    = "KALIMANTAN BARAT",
  "Kalimantan Selatan"  = "KALIMANTAN SELATAN",
  "Kalimantan Tengah"   = "KALIMANTAN TENGAH",
  "Kalimantan Timur"    = "KALIMANTAN TIMUR",
  "Kalimantan Utara"    = "KALIMANTAN UTARA",
  "Kepulauan Riau"      = "KEP RIAU",
  "Lampung"             = "LAMPUNG",
  "Maluku"              = "MALUKU",
  "Maluku Utara"        = "MALUKU UTARA",
  "Nusa Tenggara Barat" = "NUSA TENGGARA BARAT",
  "Nusa Tenggara Timur" = "NUSA TENGGARA TIMUR",
  "Papua"               = "PAPUA",
  "Papua Barat"         = "PAPUA BARAT",
  "Riau"                = "RIAU",
  "Sulawesi Barat"      = "SULAWESI BARAT",
  "Sulawesi Selatan"    = "SULAWESI SELATAN",
  "Sulawesi Tengah"     = "SULAWESI TENGAH",
  "Sulawesi Tenggara"   = "SULAWESI TENGGARA",
  "Sulawesi Utara"      = "SULAWESI UTARA",
  "Sumatera Barat"      = "SUMATERA BARAT",
  "Sumatera Selatan"    = "SUMATERA SELATAN",
  "Sumatera Utara"      = "SUMATERA UTARA",
  "Yogyakarta"          = "DI YOGYAKARTA"
)

indo_sf <- indo_sf %>%
  mutate(
    provinsi = recode(pull(., all_of(nama_col)), !!!nama_mapping)
  ) %>%
  left_join(
    df_result %>% select(provinsi, cluster, label_klaster,
                          pct_miskin, ipm, tpt, rls, ahh, pdrb),
    by = "provinsi"
  )

# Cek keberhasilan join
tidak_match <- indo_sf %>%
  filter(is.na(cluster)) %>%
  pull(all_of(nama_col))

if (length(tidak_match) > 0) {
  cat("\n⚠ Provinsi tidak match (perlu update nama_mapping):\n")
  cat("  ", paste(tidak_match, collapse = ", "), "\n")
} else {
  cat("✓ Semua 34 provinsi berhasil di-join ke shapefile!\n")
}


# ============================================================
# STEP 4: Plot peta choropleth utama
# ============================================================

# Palet warna per klaster — urutan: Rendah, Menengah, Tinggi
# Klaster diidentifikasi via label bukan nomor klaster mentah

label_order <- df_result %>%
  group_by(cluster, label_klaster) %>%
  summarise(pct_miskin = mean(pct_miskin), .groups = "drop") %>%
  arrange(pct_miskin)

warna_klaster <- c(
  "#2166AC",   # Biru tua   — Kemiskinan Rendah
  "#78C679",   # Hijau      — Kemiskinan Menengah
  "#D73027"    # Merah      — Kemiskinan Tinggi
)
names(warna_klaster) <- as.character(label_order$cluster)

label_legend <- label_order %>%
  mutate(
    label = sprintf("Klaster %d — %s\n(n=%d, miskin=%.1f%%)",
                    cluster, label_klaster, n(),
                    pct_miskin)
  ) %>%
  pull(label)

# Hitung rata-rata per klaster untuk label_legend yang lebih informatif
profil_label <- df_result %>%
  group_by(cluster, label_klaster) %>%
  summarise(n = n(), pct_miskin = round(mean(pct_miskin), 1), .groups="drop") %>%
  arrange(pct_miskin)

label_legend <- setNames(
  sprintf("Klaster %d — %s  (n=%d, rata-rata miskin=%.1f%%)",
          profil_label$cluster,
          profil_label$label_klaster,
          profil_label$n,
          profil_label$pct_miskin),
  as.character(profil_label$cluster)
)

peta_utama <- ggplot(indo_sf) +
  geom_sf(
    aes(fill = factor(cluster)),
    color     = "white",
    linewidth = 0.25
  ) +
  scale_fill_manual(
    values   = warna_klaster,
    name     = "Klaster Kemiskinan",
    labels   = label_legend,
    na.value = "grey80",
    na.translate = FALSE
  ) +
  labs(
    title    = "Klasterisasi Provinsi Indonesia Berdasarkan Indikator Kemiskinan",
    subtitle = paste0(
      "Hierarchical Clustering (Average Linkage) | k=3 | 34 Provinsi | BPS 2022\n",
      "Variabel: % Penduduk Miskin · IPM · TPT · Rata-rata Lama Sekolah · AHH · PDRB per Kapita"
    ),
    caption  = paste0(
      "Sumber: Badan Pusat Statistik (BPS) Indonesia 2022\n",
      "Muhammad Rafli Azizan | Universitas Diponegoro | Silhouette Coefficient = 0.430"
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(size = 15, face = "bold", margin = margin(b=5)),
    plot.subtitle    = element_text(size = 10, color = "grey40", margin = margin(b=10)),
    plot.caption     = element_text(size = 8,  color = "grey55", hjust = 0),
    legend.position  = "bottom",
    legend.title     = element_text(face = "bold", size = 10),
    legend.text      = element_text(size = 9),
    legend.key.size  = unit(0.9, "lines"),
    panel.grid       = element_blank(),
    axis.text        = element_blank(),
    axis.ticks       = element_blank(),
    plot.margin      = margin(10, 10, 10, 10)
  ) +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE))

ggsave("output/peta_klaster_indonesia.png",
       plot   = peta_utama,
       width  = 16,
       height = 9,
       dpi    = 300)
cat("✓ Peta utama tersimpan: output/peta_klaster_indonesia.png\n")


# ============================================================
# STEP 5: Plot peta % kemiskinan (continuous choropleth)
# Pelengkap — menunjukkan gradasi aktual pct_miskin
# ============================================================

peta_kontinu <- ggplot(indo_sf) +
  geom_sf(
    aes(fill = pct_miskin),
    color     = "white",
    linewidth = 0.25
  ) +
  scale_fill_distiller(
    palette  = "YlOrRd",
    direction = 1,
    name     = "% Penduduk\nMiskin",
    na.value = "grey80",
    labels   = function(x) paste0(x, "%")
  ) +
  labs(
    title    = "Distribusi Persentase Penduduk Miskin per Provinsi",
    subtitle = "Data: BPS Maret 2022 | 34 Provinsi Indonesia",
    caption  = "Muhammad Rafli Azizan | Universitas Diponegoro"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 10, color = "grey40"),
    plot.caption  = element_text(size = 8,  color = "grey55", hjust = 0),
    panel.grid    = element_blank(),
    axis.text     = element_blank(),
    axis.ticks    = element_blank()
  )

ggsave("output/peta_pct_miskin_kontinu.png",
       plot   = peta_kontinu,
       width  = 16,
       height = 9,
       dpi    = 300)
cat("✓ Peta kontinu tersimpan: output/peta_pct_miskin_kontinu.png\n")


# ============================================================
# STEP 6: Bar chart profil antar klaster
# Untuk laporan — perbandingan rata-rata per variabel
# ============================================================

profil_long <- df_result %>%
  group_by(cluster, label_klaster) %>%
  summarise(across(c(pct_miskin, ipm, tpt, rls, ahh),
                   ~round(mean(.), 2)),
            .groups = "drop") %>%
  pivot_longer(cols = c(pct_miskin, ipm, tpt, rls, ahh),
               names_to = "variabel", values_to = "nilai") %>%
  mutate(
    variabel = recode(variabel,
      pct_miskin = "% Penduduk Miskin",
      ipm        = "IPM",
      tpt        = "Tingkat Pengangguran (%)",
      rls        = "Rata-rata Lama Sekolah (th)",
      ahh        = "Angka Harapan Hidup (th)"
    ),
    label_klaster = factor(label_klaster,
                           levels = c("Kemiskinan Rendah",
                                      "Kemiskinan Menengah",
                                      "Kemiskinan Tinggi"))
  )

profil_chart <- ggplot(profil_long,
                       aes(x = label_klaster, y = nilai,
                           fill = label_klaster)) +
  geom_col(width = 0.65, show.legend = FALSE) +
  geom_text(aes(label = nilai), vjust = -0.4, size = 3.2, fontface = "bold") +
  scale_fill_manual(values = c(
    "Kemiskinan Rendah"    = "#2166AC",
    "Kemiskinan Menengah"  = "#78C679",
    "Kemiskinan Tinggi"    = "#D73027"
  )) +
  facet_wrap(~variabel, scales = "free_y", ncol = 3) +
  labs(
    title    = "Profil Rata-rata Setiap Klaster",
    subtitle = "Hierarchical Clustering (k=3) | 34 Provinsi | BPS 2022",
    x        = NULL,
    y        = "Nilai rata-rata",
    caption  = "Muhammad Rafli Azizan | Universitas Diponegoro"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title    = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(color = "grey50", size = 9),
    plot.caption  = element_text(color = "grey60", size = 8, hjust = 0),
    strip.text    = element_text(face = "bold", size = 9),
    axis.text.x   = element_text(size = 8, angle = 10, hjust = 1),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank()
  )

ggsave("output/plots/profil_klaster_chart.png",
       plot = profil_chart, width = 13, height = 8, dpi = 220)
cat("✓ Bar chart profil klaster tersimpan\n\n")


# ── Simpan data final ────────────────────────────────────────

write_csv(df_result, "output/data_cluster_result_final.csv")
saveRDS(df_result,   "output/df_result.rds")

cat("=== SEMUA SELESAI! ===\n\n")
cat("Output yang siap dipakai:\n")
cat("  output/peta_klaster_indonesia.png      ← choropleth utama (portfolio)\n")
cat("  output/peta_pct_miskin_kontinu.png     ← gradasi pct miskin (laporan)\n")
cat("  output/plots/dendrogram.png            ← dendrogram (laporan)\n")
cat("  output/plots/silhouette_plot.png       ← silhouette method (laporan)\n")
cat("  output/plots/elbow_plot.png            ← elbow method (laporan)\n")
cat("  output/plots/silhouette_individual_k3.png ← detail per provinsi\n")
cat("  output/plots/profil_klaster_chart.png  ← bar chart profil (laporan)\n")
cat("  output/data_cluster_result_final.csv   ← dataset final (GitHub)\n")
