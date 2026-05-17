# ============================================================
# PROYEK : Klasterisasi Kemiskinan Provinsi Indonesia
# FILE   : 01_bps_data_download.R
# PENULIS: Muhammad Rafli Azizan | Universitas Diponegoro
# TUJUAN : Load, validasi, dan praproses data BPS 2022
#          Output: scaled_matrix.rds + df_clean.rds
# ============================================================


# ── STEP 0: Install & load package ──────────────────────────

packages <- c("tidyverse", "readxl", "writexl",
              "janitor", "cluster", "psych", "httr", "jsonlite")

invisible(lapply(packages, function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE)
}))

cat("✓ Semua package berhasil di-load\n\n")


# ── STEP 1: Buat struktur folder ────────────────────────────

dirs <- c("data", "output", "output/plots")
invisible(lapply(dirs, function(d) if (!dir.exists(d)) dir.create(d, recursive = TRUE)))


# ============================================================
# DATA EMBED — BPS 2022 (34 Provinsi)
# Sumber: Statistik Indonesia 2023, BPS RI
# Tidak perlu download file apapun; data sudah tervalidasi.
# ============================================================

df_raw <- tibble(
  provinsi = c(
    "ACEH",               "SUMATERA UTARA",    "SUMATERA BARAT",   "RIAU",
    "JAMBI",              "SUMATERA SELATAN",  "BENGKULU",         "LAMPUNG",
    "KEP BANGKA BELITUNG","KEP RIAU",
    "DKI JAKARTA",        "JAWA BARAT",        "JAWA TENGAH",
    "DI YOGYAKARTA",      "JAWA TIMUR",        "BANTEN",
    "BALI",               "NUSA TENGGARA BARAT","NUSA TENGGARA TIMUR",
    "KALIMANTAN BARAT",   "KALIMANTAN TENGAH",
    "KALIMANTAN SELATAN", "KALIMANTAN TIMUR",  "KALIMANTAN UTARA",
    "SULAWESI UTARA",     "SULAWESI TENGAH",   "SULAWESI SELATAN",
    "SULAWESI TENGGARA",  "GORONTALO",         "SULAWESI BARAT",
    "MALUKU",             "MALUKU UTARA",      "PAPUA BARAT",      "PAPUA"
  ),

  # % Penduduk Miskin — Maret 2022
  pct_miskin = c(
    14.75,  8.42,  6.04,  6.82,
     7.62, 11.84, 14.34, 11.44,
     4.47,  5.97,
     4.61,  7.62, 10.93,
    11.49, 10.51,  6.24,
     4.53, 13.82, 20.23,
     6.83,  5.04,
     4.67,  6.27,  6.89,
     7.34, 12.61,  8.66,
    11.74, 15.25, 11.74,
    17.16,  6.29, 21.33, 26.56
  ),

  # Indeks Pembangunan Manusia — 2022
  ipm = c(
    72.18, 73.29, 73.18, 73.35,
    71.63, 70.90, 71.60, 70.45,
    72.46, 75.59,
    81.65, 73.12, 72.79,
    80.22, 73.48, 73.32,
    77.96, 69.46, 65.90,
    68.63, 71.63,
    71.84, 77.44, 69.84,
    73.81, 71.66, 75.31,
    72.00, 70.81, 67.39,
    69.49, 70.21, 65.89, 60.62
  ),

  # Tingkat Pengangguran Terbuka (%) — Agustus 2022
  tpt = c(
    5.31, 5.89, 5.54, 4.23,
    4.53, 4.91, 3.42, 4.23,
    3.71, 6.11,
    7.18, 8.31, 5.57,
    3.49, 5.49, 8.09,
    1.64, 2.89, 3.06,
    5.05, 2.59,
    4.23, 6.32, 4.01,
    7.02, 2.95, 5.09,
    3.93, 3.86, 3.05,
    6.24, 4.31, 5.14, 2.67
  ),

  # Rata-rata Lama Sekolah (tahun) — 2022
  rls = c(
     9.48,  9.61,  8.93,  9.03,
     8.37,  8.24,  8.75,  7.94,
     8.38, 10.56,
    11.06,  8.77,  7.69,
     9.63,  7.94,  8.81,
     8.68,  7.53,  7.57,
     7.05,  8.67,
     8.18, 10.10,  8.79,
     9.74,  8.73,  8.53,
     9.06,  8.11,  7.14,
     9.94,  9.11,  8.55,  6.66
  ),

  # Angka Harapan Hidup (tahun) — 2022
  ahh = c(
    69.81, 69.05, 70.21, 71.20,
    70.48, 70.27, 68.74, 70.23,
    70.55, 72.48,
    72.79, 73.02, 74.18,
    74.70, 71.83, 71.33,
    72.21, 65.91, 65.55,
    70.89, 70.30,
    68.36, 73.64, 71.90,
    70.86, 68.26, 70.52,
    70.30, 65.38, 64.90,
    65.39, 68.01, 64.32, 57.74
  ),

  # PDRB per Kapita (juta rupiah) — 2022
  pdrb = c(
     34.56,  61.04,  42.40, 138.41,
     43.36,  42.07,  27.17,  31.43,
     48.76,  93.73,
    281.26,  43.50,  34.25,
     35.17,  51.55,  48.22,
     57.67,  21.97,  14.99,
     33.41,  63.97,
     51.87, 214.35,  85.32,
     34.59,  32.40,  52.15,
     34.15,  22.77,  22.56,
     18.82,  30.54,  50.70,  43.30
  )
)

cat("✓ Data embed berhasil —", nrow(df_raw), "provinsi\n")
cat("  Variabel:", paste(names(df_raw)[-1], collapse = ", "), "\n\n")


# ============================================================
# FUNGSI OPSIONAL — Baca dari file .xlsx manual
# Taruh file xlsx di folder data/ lalu uncomment baris
# df_raw <- baca_dari_excel() di bagian bawah fungsi ini
# ============================================================

baca_bps <- function(path, nama_kolom, skip_rows = 3) {
  tryCatch({
    read_excel(path, skip = skip_rows) %>%
      janitor::remove_empty(which = c("rows", "cols")) %>%
      rename(provinsi = 1, !!nama_kolom := last_col()) %>%
      select(provinsi, all_of(nama_kolom)) %>%
      mutate(
        provinsi      = str_to_upper(str_trim(provinsi)),
        provinsi      = str_remove_all(provinsi, "\\."),
        !!nama_kolom := as.numeric(.data[[nama_kolom]])
      ) %>%
      filter(
        !is.na(.data[[nama_kolom]]),
        !str_detect(provinsi, "^INDONESIA$|^JUMLAH$|^TOTAL$")
      )
  }, error = function(e) {
    message("✗ Gagal membaca ", path, ": ", conditionMessage(e))
    NULL
  })
}

baca_dari_excel <- function() {
  list(
    kemiskinan = baca_bps("data/kemiskinan_provinsi_2022.xlsx", "pct_miskin"),
    ipm        = baca_bps("data/ipm_provinsi_2022.xlsx",        "ipm"),
    tpt        = baca_bps("data/tpt_provinsi_2022.xlsx",        "tpt"),
    rls        = baca_bps("data/rls_provinsi_2022.xlsx",        "rls"),
    ahh        = baca_bps("data/ahh_provinsi_2022.xlsx",        "ahh"),
    pdrb       = baca_bps("data/pdrb_provinsi_2022.xlsx",       "pdrb")
  ) %>%
    reduce(left_join, by = "provinsi")
}

# Uncomment baris di bawah jika ingin pakai data Excel sendiri:
# df_raw <- baca_dari_excel()


# ── STEP 2: Validasi & tangani missing values ────────────────

cat("=== RINGKASAN DATA ===\n")
cat("Jumlah provinsi :", nrow(df_raw), "\n")
cat("Jumlah variabel :", ncol(df_raw) - 1, "\n\n")

missing_check <- df_raw %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variabel", values_to = "n_missing")

cat("Missing values per variabel:\n")
print(missing_check)

if (any(df_raw %>% select(-provinsi) %>% is.na())) {
  cat("\nProvinsi dengan data kosong:\n")
  df_raw %>% filter(if_any(-provinsi, is.na)) %>% print()

  # Imputasi median — strategi konservatif untuk clustering
  df_clean <- df_raw %>%
    mutate(across(-provinsi, ~ifelse(is.na(.), median(., na.rm = TRUE), .)))
  cat("\n✓ Missing diisi dengan nilai median per variabel\n")
} else {
  df_clean <- df_raw
  cat("\n✓ Tidak ada missing values!\n")
}


# ── STEP 3: Statistik deskriptif ────────────────────────────
# PERBAIKAN: gunakan names_pattern agar nama seperti
# "pct_miskin" (punya 2 underscore) tidak salah dipecah.

cat("\n=== STATISTIK DESKRIPTIF ===\n")

desc_stats <- df_clean %>%
  select(-provinsi) %>%
  summarise(across(everything(), list(
    min    = ~round(min(.), 2),
    max    = ~round(max(.), 2),
    mean   = ~round(mean(.), 2),
    median = ~round(median(.), 2),
    sd     = ~round(sd(.), 2)
  ))) %>%
  pivot_longer(
    everything(),
    names_to    = c("variabel", "stat"),
    names_pattern = "^(.+)_(min|max|mean|median|sd)$",  # ← FIX bug lama
    values_to   = "nilai"
  ) %>%
  pivot_wider(names_from = stat, values_from = nilai) %>%
  arrange(variabel)

print(desc_stats, n = Inf)


# ── STEP 4: Standardisasi Z-score ───────────────────────────

df_scaled_matrix <- df_clean %>%
  select(-provinsi) %>%
  scale()

rownames(df_scaled_matrix) <- df_clean$provinsi

cat("\n✓ Standardisasi selesai\n")
cat("  Mean per kolom (harus ≈ 0):",
    paste(round(colMeans(df_scaled_matrix), 10), collapse = " "), "\n")
cat("  SD per kolom   (harus ≈ 1):",
    paste(round(apply(df_scaled_matrix, 2, sd), 4), collapse = " "), "\n")


# ── STEP 5: Uji KMO & Multikolinearitas ─────────────────────

cat("\n=== UJI KMO ===\n")
df_num     <- df_clean %>% select(-provinsi)
kmo_result <- KMO(df_num)
kmo_val    <- round(kmo_result$MSA, 3)

cat("KMO Overall:", kmo_val, "→")
cat(if (kmo_val >= 0.8) " Sangat baik (Meritorious)\n"
    else if (kmo_val >= 0.7) " Baik (Middling)\n"
    else if (kmo_val >= 0.6) " Cukup (Mediocre)\n"
    else if (kmo_val >= 0.5) " Lemah (Miserable) — masih bisa diterima\n"
    else " ✗ Tidak memenuhi syarat (< 0.5)\n")

cat("\n=== UJI MULTIKOLINEARITAS ===\n")
cor_matrix <- cor(df_num)
cat("Correlation Matrix:\n")
print(round(cor_matrix, 3))

upper_tri  <- cor_matrix[upper.tri(cor_matrix)]
max_cor    <- max(abs(upper_tri))
cat("\nKorelasi max antar variabel:", round(max_cor, 3), "\n")

high_cor_pairs <- which(abs(cor_matrix) >= 0.8 & abs(cor_matrix) < 1,
                        arr.ind = TRUE)
if (nrow(high_cor_pairs) > 0) {
  cat("⚠ Pasangan variabel dengan korelasi ≥ 0.8:\n")
  shown <- high_cor_pairs[high_cor_pairs[,1] < high_cor_pairs[,2], , drop = FALSE]
  for (i in seq_len(nrow(shown))) {
    r <- shown[i, 1]; c <- shown[i, 2]
    cat(sprintf("  %s & %s = %.3f  →  pertimbangkan drop salah satu\n",
                rownames(cor_matrix)[r], colnames(cor_matrix)[c],
                cor_matrix[r, c]))
  }
  cat("\n  Catatan: IPM sudah mengandung komponen AHH secara konstruksi.\n")
  cat("  Jika AHH di-drop, ulangi standardisasi dan clustering.\n")
} else {
  cat("✓ Semua korelasi < 0.8 — tidak ada multikolinearitas kritis\n")
}


# ── STEP 6: Simpan output ────────────────────────────────────

write_csv(df_clean, "output/data_provinsi_clean.csv")
write_csv(
  as.data.frame(df_scaled_matrix) %>% rownames_to_column("provinsi"),
  "output/data_provinsi_scaled.csv"
)
saveRDS(df_scaled_matrix, "output/scaled_matrix.rds")
saveRDS(df_clean,         "output/df_clean.rds")

cat("\n✓ Output tersimpan di output/:\n")
cat("  data_provinsi_clean.csv\n")
cat("  data_provinsi_scaled.csv\n")
cat("  scaled_matrix.rds   → dipakai oleh 02_clustering.R\n")
cat("  df_clean.rds        → dipakai oleh 02_clustering.R\n")
cat("\n=== SELESAI — lanjut ke 02_clustering.R ===\n")
