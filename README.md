# Psychometrics Learning Lab — v2.0.0

Paket repository GitHub Pages berbasis **Shinylive/webR**. File `app.R` adalah **salinan bit-per-bit dari app R yang diberikan**, tanpa menulis ulang rumus, simulasi, maupun tampilan. Ada 8 modul: IRT, CTT, Reliability, Factor Analysis, DIF, Measurement Invariance, Validity, dan Scale Development. Ada mode terang/gelap, ekspor CSV, unduhan sintaks R, dan tautan untuk berbagi setelan.

**Status: SIAP UNTUK DICOBA DEPLOY, BUKAN SUDAH DIVERIFIKASI ONLINE.** Build GitHub Pages serta uji browser belum bisa dijalankan pada lingkungan pembuatan paket ini (R dan webR runtime tidak tersedia). Kode R **belum dijamin** berfungsi di browser karena dependensi `lavaan`. Pengguna harus melakukan uji pakai sebelum aplikasi dipublikasikan untuk penggunaan luas.

## 1. Deploy ke GitHub Pages

1. Ekstrak ZIP ini. Buat repository **public** bernama, misalnya, `psychometrics-learning-lab` di GitHub. Jangan centang opsi membuat README tambahan.
2. Upload **isi folder** ini, termasuk file tersembunyi `.github/workflows/deploy-app.yaml`. Jangan mengunggah ZIP sebagai satu berkas. Cara paling aman dari terminal macOS, di dalam folder hasil ekstrak:

   ```bash
   git init
   git branch -M main
   git add .
   git commit -m "Publish Psychometrics Learning Lab"
   git remote add origin https://github.com/USERNAME/psychometrics-learning-lab.git
   git push -u origin main
   ```

   Ganti `USERNAME` dengan username GitHub Anda. Anda harus sudah login GitHub melalui SSH atau credential manager (atau gunakan GitHub Desktop untuk menambahkan repository lokal ke GitHub).
3. Pada repository → **Settings → Pages → Build and deployment → Source: GitHub Actions**.
4. Periksa tab **Actions → Publish Psychometrics Learning Lab**. Jika belum terpicu otomatis, klik **Run workflow**.
5. Jika build dan deploy berhasil, GitHub Pages biasanya menampilkan URL `https://USERNAME.github.io/psychometrics-learning-lab/`. Ini contoh pola URL, **bukan tautan live**.
6. Buka app di Chrome desktop dan ikuti [CHECKLIST.md](CHECKLIST.md) untuk memastikan modul benar-benar berjalan.

### Penting: `lavaan` dan kemungkinan error webR

Aplikasi memuat `library(lavaan)` di awal; jadi **jika `lavaan` gagal dimuat di webR, keseluruhan app dapat gagal start**, walaupun build GitHub Actions terlihat hijau. Ada indikasi `lavaan` memerlukan paket WebAssembly yang belum disediakan oleh repository webR resmi pada konfigurasi tertentu. Ada build `lavaan` **eksperimental tidak resmi** pada repositori pihak ketiga, tetapi bukan pengganti stabil atau otomatis kompatibel dengan versi R/webR aplikasi ini. Karena kode asli sengaja dipertahankan, paket ini **tidak mengaktifkan repositori eksperimental** dan tidak mengklaim seluruh modul akan berfungsi di GitHub Pages.

Jika terjadi kegagalan:
- Buka **Actions** untuk melihat error build.
- Pada halaman situs, tekan **F12 → Console** untuk menemukan pesan gagal install/load paket atau error R.
- Cek kesesuaian paket webR pada `https://repo.r-wasm.org/`.
- Bila kebutuhan utama adalah **kesetaraan penuh dengan R/lavaan**, pakai **Posit Connect Cloud** (lihat bagian berikut).

## 2. Deploy aplikasi asli di Posit (pilihan paling aman)

Deploy `app.R` asli melalui RStudio/Posit Connect Cloud. Aplikasi R akan dihitung oleh server, sehingga dapat menggunakan paket `lavaan`, `DT`, dan dependensi lainnya selama paket R tersedia di lingkungan server. GitHub tetap bisa digunakan sebagai penyimpan dan pengelola versi sumber, sementara **Posit menjalankan aplikasinya** (kedua layanan tidak harus dipilih salah satu).

Info resmi:
- [Posit Connect Cloud](https://connect.posit.cloud/)
- [Panduan deploy Shiny R](https://docs.posit.co/connect-cloud/how-to/r/shiny-r.html)
- [Shinylive untuk R](https://posit-dev.github.io/r-shinylive/)
- [GitHub Pages](https://docs.github.com/en/pages)

## 3. Uji export Shinylive di Mac, bila perlu

Dari R/RStudio pada folder ini:

```r
install.packages(c("shiny", "bslib", "ggplot2", "DT", "lavaan", "shinylive", "httpuv"))
shinylive::export(".", "site")
httpuv::runStaticServer("site")
```

Catatan: instalasi paket di komputer Mac **tidak menjamin** paket tersebut juga tersedia sebagai binary WebAssembly pada browser.

## Batasan hosting

- **GitHub Pages biasa tidak menjalankan R.** Shinylive mengubah app R menjadi kumpulan file statis dan menjalankan R via webR di **komputer pengguna**.
- Hosting Pages tidak ditagih berdasarkan jam CPU R; tetap tunduk pada batas/ketentuan GitHub dan perangkat pengguna.
- Waktu pemuatan pertama, konsumsi RAM, dan responsivitas bisa berbeda dibanding Posit; khususnya simulasi Monte Carlo/CFA.
- Kode/sintaks R di Shinylive publik pada dasarnya dikirim ke browser dan **bukan rahasia**. Jangan menaruh token atau data sensitif di dalam kode.
- Untuk aplikasi publik ratusan pengguna, lakukan uji browser per modul sebelum mengumumkan bahwa seluruhnya tersedia.

## Berkas

- `app.R`: kode Shiny asli, 3813 baris, tidak diubah.
- `.github/workflows/deploy-app.yaml`: workflow resmi Posit r-shinylive untuk otomatis ekspor dan publish GitHub Pages.
- `.gitignore`: abaikan site hasil export lokal.
- `CHECKLIST.md`: daftar uji fungsional sebelum rilis.

Tidak ada lisensi baru ditambahkan ke kode R asli.
