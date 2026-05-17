# ============================================================
# PROYEK : Klasterisasi Kemiskinan Provinsi Indonesia
# FILE   : 02_clustering.R
# PENULIS: Muhammad Rafli Azizan | Universitas Diponegoro
# TUJUAN : Hierarchical clustering + evaluasi + profilisasi
#          Input : scaled_matrix.rds, df_clean.rds
#          Output: df_result.rds, profil_klaster.rds, plots
# ============================================================


# ── Package ──────────────────────────────────────────────────

library(tidyverse)
library(cluster)

cat("✓ Package loaded\n")


# ── Load data ─────────────────────────────────────────────────

df_scaled_matrix <- readRDS("output/scaled_matrix.rds")
df_clean         <- readRDS("output/df_clean.rds")

cat("✓ Data loaded —", nrow(df_clean), "provinsi,",
    ncol(df_clean) - 1, "variabel\n\n")


# ============================================================
# STEP 1: Hitung Distance Matrix + Hierarchical Clustering
# Metode linkage: "average" (UPGMA) — robust terhadap outlier,
# cocok untuk data provinsi yang heterogen seperti Papua.
# ============================================================

dist_matrix   <- dist(df_scaled_matrix, method = "euclidean")
hclust_result <- hclust(dist_matrix, method = "average")

cat("✓ Distance matrix (Euclidean) dihitung\n")
cat("✓ Hierarchical clustering selesai (linkage: Average/UPGMA)\n\n")


# ============================================================
# STEP 2: Dendrogram
# rect.hclust ditampilkan untuk k=3 (keputusan final di Step 5)
# ============================================================

png("output/plots/dendrogram.png", width = 2800, height = 1600, res = 220)

par(mar = c(5, 4, 4, 2))
plot(
  hclust_result,
  main   = "Dendrogram Klasterisasi Kemiskinan Provinsi Indonesia",
  sub    = "Metode: Hierarchical Clustering (Average Linkage) | Jarak: Euclidean | BPS 2022",
  xlab   = "Provinsi",
  ylab   = "Jarak (Euclidean)",
  cex    = 0.70,
  hang   = -1,
  col    = "grey20",
  lwd    = 1.2
)
rect.hclust(hclust_result, k = 3,
            border = c("#1A56DB", "#1D9E75", "#D85A30"))

legend("topright",
       legend = c("Klaster 1", "Klaster 2", "Klaster 3"),
       fill   = c("#1A56DB", "#1D9E75", "#D85A30"),
       border = NA, bty = "n", cex = 0.8)

dev.off()
cat("✓ Dendrogram tersimpan: output/plots/dendrogram.png\n\n")


# ============================================================
# STEP 3: Evaluasi jumlah klaster — Silhouette + Elbow (WSS)
# Dua metode dipakai bersama untuk keputusan k yang lebih kuat.
# ============================================================

k_range <- 2:8

cat("Menghitung Silhouette dan WSS untuk k =", min(k_range),
    "sampai", max(k_range), "...\n")

eval_df <- map_dfr(k_range, function(k) {
  labels <- cutree(hclust_result, k = k)
  sil    <- silhouette(labels, dist_matrix)
  sc     <- mean(sil[, "sil_width"])

  # WSS (Within-cluster Sum of Squares)
  wss <- sum(sapply(unique(labels), function(cl) {
    members <- df_scaled_matrix[labels == cl, , drop = FALSE]
    if (nrow(members) == 1) return(0)
    sum(scale(members, scale = FALSE)^2)
  }))

  tibble(k = k, silhouette = round(sc, 4), wss = round(wss, 2))
})

cat("\n─── Nilai evaluasi per k ─────────────────────────\n")
print(eval_df, n = Inf)

k_optimal  <- eval_df$k[which.max(eval_df$silhouette)]
sc_optimal <- max(eval_df$silhouette)

interpretasi_sc <- case_when(
  sc_optimal > 0.70 ~ "Sangat kuat (Strong structure)",
  sc_optimal > 0.50 ~ "Cukup baik (Reasonable structure)",
  sc_optimal > 0.25 ~ "Lemah (Weak structure)",
  TRUE              ~ "Tidak ada struktur yang jelas"
)

cat("\n✓ k optimal (Silhouette) :", k_optimal,
    "| SC =", sc_optimal, "|", interpretasi_sc, "\n")


# ── Plot Silhouette ──────────────────────────────────────────

sil_plot <- ggplot(eval_df, aes(x = k, y = silhouette)) +
  geom_line(color = "#1A56DB", linewidth = 1.3) +
  geom_point(size = 3.5, color = "#1A56DB") +
  geom_point(
    data  = filter(eval_df, k == k_optimal),
    color = "#D85A30", size = 7
  ) +
  geom_label(
    data  = filter(eval_df, k == k_optimal),
    aes(label = paste0("k = ", k, "\nSC = ", silhouette)),
    vjust = -0.8, color = "#D85A30", size = 3.5,
    fontface = "bold", fill = "white", label.size = 0.3
  ) +
  scale_x_continuous(breaks = k_range) +
  scale_y_continuous(limits = c(0.1, 0.6)) +
  labs(
    title    = "Penentuan Jumlah Klaster — Metode Silhouette",
    subtitle = "Hierarchical Clustering (Average Linkage) | 34 Provinsi Indonesia | BPS 2022",
    x        = "Jumlah Klaster (k)",
    y        = "Rata-rata Silhouette Width",
    caption  = "Muhammad Rafli Azizan | Universitas Diponegoro"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title       = element_text(face = "bold", size = 14),
    plot.subtitle    = element_text(color = "grey50", size = 10),
    plot.caption     = element_text(color = "grey60", size = 9, hjust = 1),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey90")
  )

ggsave("output/plots/silhouette_plot.png",
       plot = sil_plot, width = 9, height = 5.5, dpi = 220)
cat("✓ Silhouette plot tersimpan\n")


# ── Plot Elbow (WSS) ─────────────────────────────────────────

elbow_plot <- ggplot(eval_df, aes(x = k, y = wss)) +
  geom_line(color = "#1D9E75", linewidth = 1.3) +
  geom_point(size = 3.5, color = "#1D9E75") +
  scale_x_continuous(breaks = k_range) +
  labs(
    title    = "Elbow Method — Within-Cluster Sum of Squares (WSS)",
    subtitle = "Hierarchical Clustering (Average Linkage) | 34 Provinsi Indonesia | BPS 2022",
    x        = "Jumlah Klaster (k)",
    y        = "WSS (Total Within-cluster SS)",
    caption  = "Muhammad Rafli Azizan | Universitas Diponegoro"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title       = element_text(face = "bold", size = 14),
    plot.subtitle    = element_text(color = "grey50", size = 10),
    plot.caption     = element_text(color = "grey60", size = 9, hjust = 1),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey90")
  )

ggsave("output/plots/elbow_plot.png",
       plot = elbow_plot, width = 9, height = 5.5, dpi = 220)
cat("✓ Elbow plot tersimpan\n\n")


# ============================================================
# STEP 4: Silhouette plot individual (k=3)
# Visualisasi silhouette width per provinsi per klaster.
# ============================================================

cluster_k3 <- cutree(hclust_result, k = 3)
sil_k3     <- silhouette(cluster_k3, dist_matrix)

sil_ind_df <- tibble(
  provinsi  = rownames(df_scaled_matrix),
  cluster   = sil_k3[, "cluster"],
  sil_width = sil_k3[, "sil_width"]
) %>%
  arrange(cluster, sil_width) %>%
  mutate(
    provinsi = factor(provinsi, levels = provinsi),
    warna    = ifelse(sil_width >= 0, "Positif", "Negatif")
  )

sil_ind_plot <- ggplot(sil_ind_df,
                       aes(x = provinsi, y = sil_width, fill = warna)) +
  geom_col(show.legend = FALSE) +
  geom_hline(yintercept = mean(sil_ind_df$sil_width),
             linetype = "dashed", color = "#D85A30", linewidth = 0.8) +
  scale_fill_manual(values = c("Positif" = "#1A56DB", "Negatif" = "#D85A30")) +
  facet_wrap(~cluster, scales = "free_y",
             labeller = labeller(cluster = c("1"="Klaster 1","2"="Klaster 2","3"="Klaster 3"))) +
  coord_flip() +
  labs(
    title    = "Silhouette Width per Provinsi (k = 3)",
    subtitle = paste0("Rata-rata SC = ",
                      round(mean(sil_ind_df$sil_width), 4),
                      " | Garis merah = rata-rata keseluruhan"),
    x        = NULL,
    y        = "Silhouette Width",
    caption  = "Muhammad Rafli Azizan | Universitas Diponegoro"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title    = element_text(face = "bold"),
    plot.subtitle = element_text(color = "grey50", size = 9),
    strip.text    = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave("output/plots/silhouette_individual_k3.png",
       plot = sil_ind_plot, width = 11, height = 8, dpi = 220)
cat("✓ Silhouette individual (k=3) tersimpan\n\n")


# ============================================================
# STEP 5: Penetapan klaster final — k = 3
#
# JUSTIFIKASI PEMILIHAN k=3 (bukan k=2 optimal Silhouette):
#   - k=2 menghasilkan klaster trivial: 1 klaster berisi
#     33 provinsi, 1 klaster hanya Papua.
#   - Informasi ini kurang berguna secara kebijakan.
#   - k=3 masih memiliki SC = 0.43 (Reasonable) dan
#     menghasilkan segmentasi yang substantif dan dapat
#     diinterpretasikan secara regional.
#   - Pendekatan ini umum dalam analisis regional BPS:
#     pemilihan k mempertimbangkan kebermaknaan substantif,
#     bukan hanya metrik statistik semata.
# ============================================================

k_final       <- 3
cluster_final <- cutree(hclust_result, k = k_final)
sc_final      <- eval_df$silhouette[eval_df$k == k_final]

df_result <- df_clean %>%
  mutate(cluster = as.integer(cluster_final))

cat("─── Distribusi provinsi per klaster (k=3) ───────────\n")
df_result %>% count(cluster) %>% print()


# ── Profilisasi klaster ──────────────────────────────────────

cat("\n=== PROFILISASI KLASTER (k=3) ===\n")

profil_klaster <- df_result %>%
  group_by(cluster) %>%
  summarise(
    n_provinsi = n(),
    pct_miskin = round(mean(pct_miskin), 2),
    ipm        = round(mean(ipm),        2),
    tpt        = round(mean(tpt),        2),
    rls        = round(mean(rls),        2),
    ahh        = round(mean(ahh),        2),
    pdrb       = round(mean(pdrb),       2),
    .groups    = "drop"
  ) %>%
  arrange(pct_miskin)

print(profil_klaster, n = Inf)

# Labelling otomatis berdasarkan urutan pct_miskin
label_map <- profil_klaster %>%
  mutate(label_klaster = case_when(
    row_number() == 1 ~ "Kemiskinan Rendah",
    row_number() == 2 ~ "Kemiskinan Menengah",
    TRUE              ~ "Kemiskinan Tinggi"
  )) %>%
  select(cluster, label_klaster)

df_result <- df_result %>%
  left_join(label_map, by = "cluster")

cat("\nAnggota setiap klaster:\n")
df_result %>%
  arrange(cluster, provinsi) %>%
  group_by(cluster, label_klaster) %>%
  summarise(
    n             = n(),
    provinsi_list = paste(provinsi, collapse = ", "),
    .groups       = "drop"
  ) %>%
  print(width = Inf)


# ── Simpan semua output ──────────────────────────────────────

write_csv(df_result,    "output/data_cluster_result.csv")
saveRDS(df_result,      "output/df_result.rds")
saveRDS(profil_klaster, "output/profil_klaster.rds")
saveRDS(hclust_result,  "output/hclust_result.rds")   # untuk keperluan lanjutan

cat("\n✓ Semua output tersimpan di output/\n")

cat("\n╔══════════════════════════════════════════════╗\n")
cat("║        RINGKASAN UNTUK PORTFOLIO / CV        ║\n")
cat("╠══════════════════════════════════════════════╣\n")
cat(sprintf("║  Metode            : Hierarchical (Average)  ║\n"))
cat(sprintf("║  Jumlah provinsi   : 34                      ║\n"))
cat(sprintf("║  Jumlah variabel   : 6                       ║\n"))
cat(sprintf("║  k optimal (SC)    : %-4d (SC = %-6.4f)     ║\n", k_optimal, sc_optimal))
cat(sprintf("║  k final dipakai   : %-4d (SC = %-6.4f)     ║\n", k_final, sc_final))
cat(sprintf("║  Interpretasi SC   : Reasonable structure    ║\n"))
cat("╚══════════════════════════════════════════════╝\n")

cat("\n✓ SELESAI — lanjut ke 03_mapping.R\n")
