# 🗺️ Klasterisasi Kemiskinan Provinsi Indonesia
### Hierarchical Clustering Berbasis Indikator Multidimensi | BPS 2022

> Pengelompokan 34 provinsi Indonesia menggunakan *Hierarchical Clustering (Average Linkage)*
> berdasarkan 6 indikator kemiskinan dari Badan Pusat Statistik tahun 2022.

---

## 📊 Hasil Utama

![Peta Klasterisasi](output/peta_klaster_indonesia.png)

| Klaster | Label | Jumlah Provinsi | Rata-rata % Miskin | IPM | PDRB/kapita |
|:-------:|-------|:---:|:---:|:---:|:---:|
| 🔵 2 | Kemiskinan Rendah | 2 | 5.44% | 79.5 | Rp 248 juta |
| 🟢 1 | Kemiskinan Menengah | 31 | 10.1% | 71.9 | Rp 45 juta |
| 🔴 3 | Kemiskinan Tinggi | 1 | 26.56% | 60.6 | Rp 43 juta |

**Silhouette Coefficient = 0.430** — *Reasonable structure* ✔️

---

## 🗂️ Struktur Proyek

```
poverty-clustering-indonesia/
│
├── 📄 01_bps_data_download.R     # Load + validasi + praproses data BPS
├── 📄 02_clustering.R            # Hierarchical clustering + evaluasi k
├── 📄 03_mapping.R               # Choropleth map + visualisasi
│
├── 📄 laporan_klasterisasi_kemiskinan.Rmd   # Laporan lengkap (R Markdown)
│
├── output/
│   ├── 🗺️  peta_klaster_indonesia.png       # Peta choropleth utama
│   ├── 📈  peta_pct_miskin_kontinu.png      # Peta gradasi % kemiskinan
│   ├── 📋  data_cluster_result_final.csv    # Dataset hasil clustering
│   │
│   └── plots/
│       ├── 🌳 dendrogram.png                # Dendrogram hierarchical
│       ├── 📉 silhouette_plot.png           # Silhouette method
│       ├── 📉 elbow_plot.png                # Elbow method (WSS)
│       ├── 📊 silhouette_individual_k3.png  # Silhouette per provinsi
│       └── 📊 profil_klaster_chart.png      # Bar chart profil klaster
│
└── data/                         # Folder data (auto-dibuat oleh script)
```

---

## 🔬 Variabel Analisis

| Variabel | Deskripsi | Satuan | Sumber |
|----------|-----------|--------|--------|
| `pct_miskin` | Persentase Penduduk Miskin | % | BPS, Maret 2022 |
| `ipm` | Indeks Pembangunan Manusia | Indeks 0–100 | BPS 2022 |
| `tpt` | Tingkat Pengangguran Terbuka | % | BPS, Agustus 2022 |
| `rls` | Rata-rata Lama Sekolah | Tahun | BPS 2022 |
| `ahh` | Angka Harapan Hidup | Tahun | BPS 2022 |
| `pdrb` | PDRB per Kapita | Juta Rupiah | BPS 2022 |

---

## ⚙️ Metodologi

```
Data BPS 2022 (34 provinsi, 6 variabel)
        ↓
Standardisasi Z-score
        ↓
Uji KMO (0.724 ✔) + Cek Multikolinearitas
        ↓
Hierarchical Clustering
  • Distance  : Euclidean
  • Linkage   : Average (UPGMA)
        ↓
Evaluasi k (Silhouette + Elbow)
  • k=2 optimal secara SC (0.503)
  • k=3 dipilih → lebih informatif substantif
        ↓
Profilisasi + Choropleth Map
```

---

## 🚀 Cara Menjalankan

**Prasyarat:** R ≥ 4.0 dan RStudio

```r
# 1. Clone repositori ini
# git clone https://github.com/username/poverty-clustering-indonesia.git

# 2. Jalankan secara berurutan di RStudio:
source("01_bps_data_download.R")   # ~30 detik
source("02_clustering.R")           # ~1 menit
source("03_mapping.R")              # ~2-5 menit (download shapefile)

# 3. Render laporan R Markdown:
rmarkdown::render("laporan_klasterisasi_kemiskinan.Rmd")
```

> **Catatan:** Package akan diinstall otomatis jika belum tersedia.
> Koneksi internet diperlukan untuk download shapefile GADM pada `03_mapping.R`.

---

## 📦 Package yang Digunakan

| Package | Fungsi |
|---------|--------|
| `tidyverse` | Manipulasi & visualisasi data |
| `cluster` | Silhouette coefficient |
| `psych` | Uji KMO |
| `sf` | Spatial data (shapefile) |
| `geodata` | Download shapefile GADM |
| `knitr` + `kableExtra` | Tabel laporan R Markdown |
| `gridExtra` | Gabungkan multiple plots |

---

## 📈 Visualisasi Pilihan

<details>
<summary><b>🌳 Dendrogram</b></summary>

![Dendrogram](output/plots/dendrogram.png)

</details>

<details>
<summary><b>📉 Silhouette & Elbow Method</b></summary>

![Silhouette](output/plots/silhouette_plot.png)
![Elbow](output/plots/elbow_plot.png)

</details>

<details>
<summary><b>📊 Profil Klaster</b></summary>

![Profil](output/plots/profil_klaster_chart.png)

</details>

---

## 🔍 Temuan Kunci

- **Papua** secara konsisten menjadi *outlier* dengan kemiskinan 26.56% — hampir 3× rata-rata nasional
- **DKI Jakarta & Kalimantan Timur** membentuk klaster unggul didorong PDRB per kapita yang sangat tinggi (Rp 281 juta dan Rp 214 juta)
- **31 dari 34 provinsi** berada di klaster menengah — menunjukkan mayoritas Indonesia masih membutuhkan perhatian kebijakan
- Korelasi tinggi antara IPM dan AHH (r = 0.825) mencerminkan konstruksi IPM yang memang mencakup dimensi kesehatan

---

## ⚠️ Catatan Metodologis

- Data mengacu pada 34 provinsi (sebelum pemekaran Papua 2022/2023)
- Pemilihan k=3 menggunakan pertimbangan **substantif** di atas statistik murni (k=2 optimal secara SC, namun trivial)
- Korelasi IPM–AHH dipertahankan karena keduanya mengukur dimensi berbeda

---

## 📚 Referensi

- Badan Pusat Statistik. (2023). *Statistik Indonesia 2023*. BPS RI.
- Kaufman, L. & Rousseeuw, P.J. (1990). *Finding Groups in Data*. Wiley.
- Rousseeuw, P.J. (1987). Silhouettes: A graphical aid to cluster analysis. *JCAM*, 20, 53–65.

---

## 👤 Tentang

**Muhammad Rafli Azizan**  
Universitas Diponegoro

[![GitHub](https://img.shields.io/badge/GitHub-100000?style=flat&logo=github&logoColor=white)](https://github.com/bangjijan)
[![LinkedIn](https://img.shields.io/badge/LinkedIn-0077B5?style=flat&logo=linkedin&logoColor=white)](https://linkedin.com/in/RafliAzizan)

---

*Data: Badan Pusat Statistik Republik Indonesia, 2022*  
*Shapefile: GADM v4.1 — [gadm.org](https://gadm.org)*
# poverty-clustering-indonesia
