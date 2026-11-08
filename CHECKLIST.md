# Checklist validasi setelah publish GitHub Pages

- [ ] GitHub Actions build dan deployment keduanya sukses.
- [ ] Aplikasi membuka halaman Beranda; tab, font, dan tema gelap/terang tidak rusak.
- [ ] **IRT Playground:** ganti Rasch/2PL/3PL/4PL/RSM/PCM/GRM; kurva probabilitas dan informasi berubah.
- [ ] **CTT Laboratory:** simulasi skor teramati, true score, error, SEM, dan plot ter-update.
- [ ] **Reliability Explorer:** alpha, CI Feldt, omega, tabel, dan grafik item muncul. **Omega butuh `lavaan`.**
- [ ] **Factor Analysis:** loadings, communalities, eigenvalues, parallel analysis dan interpretasinya muncul.
- [ ] **DIF Explorer:** skor matching, logistic, MH/ETS, grafik dan tabel ter-update.
- [ ] **Measurement Invariance:** configural/metric/scalar/strict, CFI/RMSEA/SRMR, chi-square difference dan partial invariance terhitung. **Wajib uji `lavaan`.**
- [ ] **Validity Lab:** attenuation dan range restriction berubah sesuai kontrol.
- [ ] **Scale Development:** corrected item-total correlation, alpha-if-deleted, tabel, dan iterative removal tampil.
- [ ] Semua modul: pengaturan seed menghasilkan simulasi konsisten, sintaks R disalin dengan benar, download R dan data CSV berjalan.
- [ ] Browser desktop dan mobile: tidak macet atau kehilangan state.
- [ ] Browser Developer Console: tidak ada error paket WebAssembly, `lavaan`, `DT`, grafik, atau download.

**Jika `lavaan` gagal start:** jangan menyatakan app berhasil dikonversi 100% ke GitHub Pages. Pertahankan deployment Posit yang mengakses R server, atau rekayasa ulang/validasi paket yang diperlukan.
