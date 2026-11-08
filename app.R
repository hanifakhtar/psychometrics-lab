# ============================================================
# PSYCHOMETRICS LEARNING LAB — v2.0.0
# Laboratorium interaktif untuk mempelajari konsep psikometrika
# melalui simulasi.
#
# Dependencies:
#   install.packages(c("shiny", "bslib", "ggplot2", "DT", "lavaan"))
#
# Jalankan:  shiny::runApp()
#
# Perubahan besar dari v1 (rincian di README):
#   REL-01  kurva test length tidak lagi bercampur urutan kolom item
#   MI-01   item non-invariant dapat dipilih dan tidak lagi berimpit
#           dengan marker variable
#   FA-01   communality memperhitungkan korelasi antar-faktor
#   REL-02  McDonald's omega dan interval Feldt untuk alpha
#   MI-02   chi-square difference, modification index, partial invariance
#   DIF-01  matching pada true theta, skor total, atau purified score
#   DIF-02  Mantel-Haenszel, Delta MH, dan klasifikasi ETS
#   Baru    Validity Lab, panel syntax R, unduhan data, tautan setelan
# ============================================================

required_packages <- c("shiny", "bslib", "ggplot2", "DT", "lavaan")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0) {
  stop(
    "Install package berikut terlebih dahulu: ",
    paste(missing_packages, collapse = ", "),
    "\nContoh: install.packages(c(",
    paste(sprintf('"%s"', missing_packages), collapse = ", "),
    "))"
  )
}

library(shiny)
library(bslib)
library(ggplot2)
library(DT)
library(lavaan)

APP_VERSION <- "2.0.0"

`%||%` <- function(x, y) if (is.null(x)) y else x
clamp <- function(x, lo, hi) pmax(lo, pmin(hi, x))

# ------------------------------------------------------------
# Tema
# ------------------------------------------------------------
app_theme <- bs_theme(
  version = 5,
  bg = "#F5F7F9",
  fg = "#16222E",
  primary = "#2F6690",
  secondary = "#5A6B7C",
  base_font = font_collection(
    font_google("Source Sans 3", local = FALSE),
    "system-ui", "-apple-system", "Segoe UI", "sans-serif"
  ),
  code_font = font_collection(
    font_google("JetBrains Mono", local = FALSE),
    "ui-monospace", "SFMono-Regular", "Menlo", "monospace"
  )
)

app_css <- "
:root{
  --ink:#16222E;
  --ink-2:#3D4E5D;
  --muted:#65798A;
  --line:#DEE5EB;
  --line-2:#C3CFD9;
  --surface:#FFFFFF;
  --surface-2:#F7F9FB;
  --page:#F5F7F9;
  --primary:#2F6690;
  --primary-dark:#245377;
  --primary-soft:#E8F0F6;
  --primary-line:#C9DCEA;
  --ok:#1D6A54;
  --ok-soft:#E9F4F0;
  --ok-line:#C4E1D6;
  --warn:#7E5A11;
  --warn-soft:#FAF3E3;
  --warn-line:#EBDCB4;
  --bad:#9C3B2A;
  --bad-soft:#FAECE8;
  --bad-line:#EFD1C8;
  --code-bg:#F1F4F7;
  --on-primary:#FFFFFF;
  --shadow-sm:0 1px 2px rgba(22,34,46,.04), 0 4px 14px rgba(22,34,46,.04);
  --shadow-md:0 2px 4px rgba(22,34,46,.05), 0 10px 26px rgba(22,34,46,.06);
}
[data-bs-theme='dark']{
  --ink:#E3EAF0;
  --ink-2:#B7C4CF;
  --muted:#8595A4;
  --line:#28343F;
  --line-2:#3A4855;
  --surface:#161F28;
  --surface-2:#1C2731;
  --page:#0F171E;
  --primary:#7FAFD4;
  --primary-dark:#95BFDF;
  --primary-soft:#182937;
  --primary-line:#28425A;
  --ok:#69BFA4;
  --ok-soft:#142D26;
  --ok-line:#245043;
  --warn:#D8B468;
  --warn-soft:#2B2415;
  --warn-line:#4A3D1E;
  --bad:#E0907C;
  --bad-soft:#301C17;
  --bad-line:#513027;
  --code-bg:#111A22;
  --on-primary:#0F171E;
  --shadow-sm:0 1px 2px rgba(0,0,0,.35), 0 4px 14px rgba(0,0,0,.25);
  --shadow-md:0 2px 4px rgba(0,0,0,.4), 0 10px 26px rgba(0,0,0,.3);
}

body{background:var(--page);color:var(--ink);-webkit-font-smoothing:antialiased;}
.container-fluid{max-width:1560px;}
.navbar{
  background:var(--surface)!important;border-bottom:1px solid var(--line);
  margin-bottom:0;position:sticky;top:0;z-index:1030;
}
.navbar-brand{font-weight:700;letter-spacing:-.02em;color:var(--ink)!important;}
.navbar-nav .nav-link{font-size:14px;font-weight:600;color:var(--muted)!important;
  padding-left:11px;padding-right:11px;}
.navbar-nav .nav-link.active{color:var(--primary)!important;}

.pl-theme-toggle{
  position:fixed;top:9px;right:14px;z-index:1100;width:34px;height:34px;
  border-radius:9px;border:1px solid var(--line);background:var(--surface);
  color:var(--ink);font-size:15px;line-height:1;cursor:pointer;
}
.pl-theme-toggle:hover{background:var(--surface-2);}

.hero{
  padding:26px 28px 20px;background:var(--surface);
  border-bottom:1px solid var(--line);
}
.hero h2{margin-top:0;font-weight:700;letter-spacing:-.02em;}
.hero p{font-size:16px;color:var(--ink-2);max-width:1050px;line-height:1.6;}

.module-card{
  background:var(--surface);border:1px solid var(--line);border-radius:12px;
  padding:18px;margin-bottom:16px;box-shadow:var(--shadow-sm);height:100%;
  display:flex;flex-direction:column;
}
.module-card h4{margin-top:0;font-weight:700;}
.module-card p{color:var(--ink-2);line-height:1.55;flex:1;}
.module-card a{font-weight:600;}

.metric-grid{display:grid;grid-template-columns:repeat(auto-fit,minmax(176px,1fr));
  gap:10px;margin-bottom:14px;align-items:stretch;}
.metric-box{background:var(--surface);border:1px solid var(--line);border-radius:10px;
  padding:12px 14px;min-height:96px;display:flex;flex-direction:column;}
.metric-title{font-size:11.5px;color:var(--muted);text-transform:uppercase;
  letter-spacing:.04em;font-weight:700;line-height:1.3;min-height:2.6em;}
.metric-value{font-size:24px;font-weight:700;margin-top:3px;color:var(--ink);
  font-variant-numeric:tabular-nums;line-height:1.15;}
.metric-subtitle{font-size:11px;color:var(--muted);margin-top:auto;padding-top:3px;
  line-height:1.4;}

.help-note{background:var(--surface-2);border-left:4px solid var(--primary);
  padding:12px 14px;border-radius:6px;color:var(--ink-2);margin:10px 0;font-size:13px;
  line-height:1.6;}
.insight{background:var(--warn-soft);border:1px solid var(--warn-line);padding:12px 14px;
  border-radius:8px;margin-top:10px;color:var(--ink-2);line-height:1.6;}
.small-muted{color:var(--muted);font-size:12px;line-height:1.5;}
.control-label{font-weight:600;}

.panel-card{background:var(--surface);border:1px solid var(--line);border-radius:12px;
  padding:18px;margin-bottom:16px;box-shadow:var(--shadow-sm);}
.panel-card h4{margin-top:0;font-weight:700;}
.panel-card p{color:var(--ink-2);line-height:1.62;}

.theta-box{background:var(--primary-soft);border:1px solid var(--primary-line);
  border-radius:12px;padding:14px 14px 8px;margin-bottom:16px;}
.theta-box h4{margin-top:0;margin-bottom:4px;color:var(--primary);font-weight:700;}
.item-card{background:var(--surface);border:1px solid var(--line);border-radius:10px;
  padding:12px 12px 4px;margin-bottom:12px;}
.item-card h5{margin-top:0;font-weight:700;color:var(--ink);}
.model-note{background:var(--surface-2);border:1px solid var(--line);border-radius:8px;
  padding:10px 12px;margin:8px 0 14px;font-size:12px;color:var(--ink-2);line-height:1.55;}

.status-ok{background:var(--ok-soft);border-left:4px solid var(--ok);padding:10px 12px;
  border-radius:6px;margin:8px 0;color:var(--ink-2);font-size:12.5px;line-height:1.55;}
.status-warn,.warning-note{background:var(--warn-soft);border-left:4px solid var(--warn);
  padding:10px 12px;border-radius:6px;margin:8px 0;color:var(--ink-2);font-size:12.5px;
  line-height:1.55;}
.status-bad{background:var(--bad-soft);border-left:4px solid var(--bad);padding:10px 12px;
  border-radius:6px;margin:8px 0;color:var(--ink-2);font-size:12.5px;line-height:1.55;}

.stat-readout{border:1px solid var(--line);border-left:3px solid var(--primary);
  background:var(--surface-2);border-radius:8px;padding:12px 14px;margin-top:10px;
  font-size:12.5px;line-height:1.62;color:var(--ink-2);}
.stat-readout strong{color:var(--ink);}
.stat-formula{font-family:var(--bs-font-monospace);font-size:11.5px;color:var(--muted);
  display:block;margin-top:6px;}

/* ---- panel syntax R ---- */
.syntax-wrap{border:1px solid var(--line);border-radius:10px;background:var(--surface);
  overflow:hidden;margin-top:8px;}
.syntax-head{display:flex;align-items:center;gap:10px;padding:9px 13px;
  background:var(--surface-2);border-bottom:1px solid var(--line);}
.syntax-head span{font-size:11.5px;font-weight:700;letter-spacing:.04em;
  text-transform:uppercase;color:var(--muted);}
.syntax-head .btn{margin-left:auto;padding:3px 10px;font-size:12px;}
.syntax-wrap pre{margin:0;border:0;border-radius:0;background:var(--code-bg);
  padding:13px 15px;max-height:520px;overflow:auto;}
.syntax-wrap code{background:none;border:0;padding:0;font-size:12px;line-height:1.6;
  color:var(--ink);white-space:pre;}

details.help-block{border:1px solid var(--line);border-radius:10px;background:var(--surface-2);
  padding:10px 13px;margin-bottom:14px;}
details.help-block > summary{cursor:pointer;font-weight:700;color:var(--ink);font-size:13px;}
details.help-block .help-body{padding-top:8px;color:var(--ink-2);font-size:12.5px;line-height:1.65;}
details.help-block .help-body p{margin-bottom:8px;}
details.help-block .help-body p:last-child{margin-bottom:0;}

.dl-row{display:flex;gap:8px;flex-wrap:wrap;margin-top:12px;}

.btn{border-radius:8px!important;font-weight:600!important;}
/* Bootstrap 5 mengatur warna teks tombol lewat --bs-btn-*-color; menimpa
   background saja membuat label hilang saat kursor di atasnya. */
.btn-primary{
  --bs-btn-color:var(--on-primary);--bs-btn-hover-color:var(--on-primary);
  --bs-btn-active-color:var(--on-primary);--bs-btn-disabled-color:var(--on-primary);
  --bs-btn-bg:var(--primary);--bs-btn-hover-bg:var(--primary-dark);
  --bs-btn-active-bg:var(--primary-dark);--bs-btn-disabled-bg:var(--primary);
  --bs-btn-border-color:var(--primary);--bs-btn-hover-border-color:var(--primary-dark);
  --bs-btn-active-border-color:var(--primary-dark);--bs-btn-disabled-border-color:var(--primary);
  color:var(--on-primary);
}
.btn-primary:hover,.btn-primary:focus,.btn-primary:active{color:var(--on-primary);}
.btn-default,.btn-secondary,.btn-light{
  --bs-btn-color:var(--ink);--bs-btn-hover-color:var(--ink);--bs-btn-active-color:var(--ink);
  --bs-btn-bg:var(--surface);--bs-btn-hover-bg:var(--surface-2);--bs-btn-active-bg:var(--surface-2);
  --bs-btn-border-color:var(--line);--bs-btn-hover-border-color:var(--line-2);
  --bs-btn-active-border-color:var(--line-2);
  background:var(--surface);border:1px solid var(--line);color:var(--ink);
}
.btn-default:hover,.btn-default:focus,.btn-default:active,
.btn-secondary:hover,.btn-light:hover{
  background:var(--surface-2);border-color:var(--line-2);color:var(--ink);
}

.form-control,.selectize-input,textarea.form-control{
  border-radius:8px!important;border-color:var(--line)!important;box-shadow:none!important;
  background:var(--surface)!important;color:var(--ink)!important;
}
.form-control:focus,.selectize-input.focus{
  border-color:var(--primary)!important;box-shadow:0 0 0 3px var(--primary-soft)!important;
}
.well,.card{background:var(--surface);border-color:var(--line);}
.tab-content{padding-top:14px;}
.nav-tabs{border-bottom:1px solid var(--line);}
.nav-tabs .nav-link{color:var(--muted);font-weight:600;border:none!important;
  border-bottom:2px solid transparent!important;}
.nav-tabs .nav-link.active{color:var(--primary)!important;background:transparent!important;
  border-bottom-color:var(--primary)!important;}

table.dataTable{color:var(--ink)!important;}
.dataTables_wrapper .dataTables_info,.dataTables_wrapper .dataTables_paginate{
  color:var(--muted)!important;
}
hr{border-color:var(--line);}
.footer{text-align:center;color:var(--muted);padding:22px 10px 28px;font-size:12px;}
.shiny-notification{border-radius:10px!important;box-shadow:var(--shadow-md)!important;}

@media(max-width:768px){
  .metric-grid{grid-template-columns:repeat(auto-fit,minmax(142px,1fr));}
  .metric-title{min-height:0;}
  .hero{padding:20px;}
}
"

app_js <- "
window.PsyLab = (function () {
  function applyTheme(t) {
    document.documentElement.setAttribute('data-bs-theme', t);
    var b = document.getElementById('pl-theme-toggle');
    if (b) b.textContent = (t === 'dark') ? '\\u2600' : '\\u25D0';
    try { localStorage.setItem('psylab_theme', t); } catch (e) {}
    reportTheme(t);
  }
  // Beri tahu server agar ggplot ikut memakai palet gelap; tanpa ini panel
  // grafik tetap putih di tengah halaman gelap.
  function reportTheme(t) {
    if (window.Shiny && Shiny.setInputValue) {
      Shiny.setInputValue('pl_theme', t, { priority: 'event' });
    }
  }
  $(document).on('shiny:connected', function () {
    reportTheme(document.documentElement.getAttribute('data-bs-theme') || 'light');
  });
  function toggleTheme() {
    var cur = document.documentElement.getAttribute('data-bs-theme');
    applyTheme(cur === 'dark' ? 'light' : 'dark');
  }
  try {
    var saved = localStorage.getItem('psylab_theme');
    if (saved) applyTheme(saved);
  } catch (e) {}

  function copySyntax(id) {
    var el = document.getElementById(id);
    if (!el) return;
    var txt = el.textContent || '';
    function done() {
      var btn = document.querySelector('[data-copy=\"' + id + '\"]');
      if (!btn) return;
      var old = btn.textContent;
      btn.textContent = 'Tersalin';
      setTimeout(function () { btn.textContent = old; }, 1400);
    }
    if (navigator.clipboard && navigator.clipboard.writeText) {
      navigator.clipboard.writeText(txt).then(done, function () {});
      return;
    }
    var ta = document.createElement('textarea');
    ta.value = txt; document.body.appendChild(ta); ta.select();
    try { document.execCommand('copy'); done(); } catch (e) {}
    document.body.removeChild(ta);
  }

  function copyText(txt) {
    if (navigator.clipboard && navigator.clipboard.writeText) {
      navigator.clipboard.writeText(txt);
    }
  }

  return { toggleTheme: toggleTheme, copySyntax: copySyntax, copyText: copyText };
})();

$(document).on('shiny:connected', function () {
  var b = document.getElementById('pl-theme-toggle');
  if (b) {
    b.textContent =
      (document.documentElement.getAttribute('data-bs-theme') === 'dark')
        ? '\\u2600' : '\\u25D0';
  }
});

Shiny.addCustomMessageHandler('psylab_copy', function (msg) {
  window.PsyLab.copyText(msg.text || '');
});
"

# ------------------------------------------------------------
# Helper UI
# ------------------------------------------------------------
metric_box <- function(title, value, subtitle = NULL) {
  div(class = "metric-box",
      div(class = "metric-title", title),
      div(class = "metric-value", value),
      if (!is.null(subtitle)) div(class = "metric-subtitle", subtitle))
}

help_block <- function(title, ...) {
  tags$details(
    class = "help-block",
    tags$summary(title),
    div(class = "help-body", ...)
  )
}

# Tabel sederhana untuk dipakai di dalam renderUI. renderTable() tidak bisa
# disisipkan ke tagList karena ia mengembalikan fungsi render, bukan tag.
simple_table <- function(df) {
  div(
    style = "overflow-x:auto;",
    tags$table(
      class = "table table-sm",
      tags$thead(tags$tr(lapply(names(df), function(h) tags$th(h)))),
      tags$tbody(lapply(seq_len(nrow(df)), function(i) {
        tags$tr(lapply(seq_along(df), function(j) tags$td(as.character(df[i, j]))))
      }))
    )
  )
}

stat_readout <- function(headline, body, formula = NULL) {
  div(class = "stat-readout",
      strong(headline), br(), body,
      if (!is.null(formula)) span(class = "stat-formula", formula))
}

# Panel syntax R: kode yang benar-benar mereproduksi perhitungan di layar,
# lengkap dengan seed yang sama, sehingga hasilnya identik ketika ditempel
# ke R Markdown.
syntax_panel <- function(id, code, download_id = NULL) {
  div(
    class = "syntax-wrap",
    div(
      class = "syntax-head",
      span("Syntax R untuk hasil di layar"),
      tags$button(
        type = "button", class = "btn btn-default",
        `data-copy` = id,
        onclick = sprintf("PsyLab.copySyntax('%s');", id),
        "Salin"
      ),
      if (!is.null(download_id)) downloadButton(download_id, "Unduh .R", class = "btn-default")
    ),
    # htmltools memberi indentasi pada anak <pre>, yang ikut terbaca sebagai
    # bagian dari kode. Bangun blok ini sebagai HTML mentah agar spasi persis.
    HTML(sprintf('<pre><code id="%s">%s</code></pre>',
                 htmltools::htmlEscape(id, attribute = TRUE),
                 htmltools::htmlEscape(code)))
  )
}

# ============================================================
# Helper psikometrika umum
# ============================================================

PSY_BLUE   <- "#2F6690"
PSY_INK    <- "#16222E"
PSY_ACCENT <- "#9C3B2A"
PSY_OK     <- "#1D6A54"
PSY_GREY   <- "#93A3B1"

# Palet grafik mengikuti tema aplikasi. Tanpa ini, panel ggplot tetap putih
# di tengah halaman mode gelap.
PSY_DARK <- list(bg = "#111A22", panel = "#16222E", grid = "#2B3B4A",
                 ink = "#E4EBF1", muted = "#9DB0C0",
                 blue = "#5FA3D4", accent = "#E08163", ok = "#4FBF9B",
                 grey = "#7C8EA0")

psy_theme <- function(base_size = 12.5, dark = FALSE) {
  base <- theme_minimal(base_size = base_size) +
    theme(
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      legend.title = element_text(size = rel(.9)),
      plot.title = element_text(face = "bold", size = rel(1.05)),
      plot.caption = element_text(size = rel(.78), hjust = 0),
      axis.title = element_text(size = rel(.92))
    )
  if (!isTRUE(dark)) {
    return(base + theme(plot.caption = element_text(colour = "#65798A",
                                                    size = rel(.78), hjust = 0)))
  }
  d <- PSY_DARK
  base + theme(
    plot.background   = element_rect(fill = d$bg, colour = NA),
    panel.background  = element_rect(fill = d$panel, colour = NA),
    legend.background = element_rect(fill = d$bg, colour = NA),
    legend.key        = element_rect(fill = d$panel, colour = NA),
    panel.grid.major  = element_line(colour = d$grid),
    strip.background  = element_rect(fill = d$panel, colour = NA),
    strip.text        = element_text(colour = d$ink),
    text              = element_text(colour = d$ink),
    axis.text         = element_text(colour = d$muted),
    axis.title        = element_text(colour = d$ink, size = rel(.92)),
    plot.title        = element_text(colour = d$ink, face = "bold", size = rel(1.05)),
    plot.caption      = element_text(colour = d$muted, size = rel(.78), hjust = 0)
  )
}

fmt_p <- function(p) {
  if (length(p) != 1 || !is.finite(p)) return("NA")
  if (p < .001) return("< .001")
  sprintf("%.3f", p)
}

fmt_n <- function(x, digits = 3) {
  if (length(x) != 1 || !is.finite(x)) return("NA")
  formatC(x, format = "f", digits = digits)
}

standardize <- function(x) {
  s <- stats::sd(x, na.rm = TRUE)
  if (!is.finite(s) || s == 0) return(rep(0, length(x)))
  (x - mean(x, na.rm = TRUE)) / s
}

# ------------------------------------------------------------
# Reliability
# ------------------------------------------------------------
cronbach_alpha <- function(x) {
  x <- as.data.frame(x)
  x <- x[, vapply(x, is.numeric, logical(1)), drop = FALSE]
  k <- ncol(x)
  if (k < 2) return(NA_real_)
  item_var <- sum(vapply(x, stats::var, numeric(1), na.rm = TRUE))
  total_var <- stats::var(rowSums(x, na.rm = TRUE), na.rm = TRUE)
  if (!is.finite(total_var) || total_var <= 0) return(NA_real_)
  k / (k - 1) * (1 - item_var / total_var)
}

mean_interitem_correlation <- function(dat) {
  r <- suppressWarnings(stats::cor(dat, use = "pairwise.complete.obs"))
  if (is.null(dim(r)) || ncol(r) < 2) return(NA_real_)
  mean(r[upper.tri(r)], na.rm = TRUE)
}

spearman_brown <- function(r, multiplier = 2) {
  if (!is.finite(r)) return(NA_real_)
  den <- 1 + (multiplier - 1) * r
  if (!is.finite(den) || abs(den) < 1e-8) return(NA_real_)
  (multiplier * r) / den
}

# REL-02a: interval kepercayaan Feldt untuk alpha.
# (1 - alpha_hat)/(1 - alpha) mengikuti distribusi F dengan
# df1 = n - 1 dan df2 = (n - 1)(k - 1).
alpha_feldt_ci <- function(alpha, n, k, conf = .95) {
  if (length(alpha) != 1 || !is.finite(alpha) || n < 3 || k < 2) {
    return(c(lo = NA_real_, hi = NA_real_))
  }
  a <- 1 - conf
  df1 <- n - 1
  df2 <- (n - 1) * (k - 1)
  c(lo = 1 - (1 - alpha) * stats::qf(1 - a / 2, df1, df2),
    hi = 1 - (1 - alpha) * stats::qf(a / 2, df1, df2))
}

# REL-02b: McDonald's omega total dari CFA satu faktor.
# omega = (sum lambda)^2 / [(sum lambda)^2 + sum theta]
# Item reverse yang belum direcode memberi lambda negatif, sehingga
# jumlahnya mengecil dan omega turun — persis pelajaran yang dituju.
omega_total <- function(dat) {
  items <- names(dat)
  if (length(items) < 3) return(NA_real_)
  model <- paste0("F =~ ", paste(items, collapse = " + "))
  fit <- tryCatch(
    suppressWarnings(lavaan::cfa(model, data = dat, std.lv = TRUE)),
    error = function(e) NULL
  )
  if (is.null(fit)) return(NA_real_)
  conv <- tryCatch(lavaan::lavInspect(fit, "converged"), error = function(e) FALSE)
  if (!isTRUE(conv)) return(NA_real_)
  
  est <- lavaan::parameterEstimates(fit)
  lam <- est$est[est$op == "=~"]
  th <- est$est[est$op == "~~" & est$lhs == est$rhs & est$lhs %in% items]
  if (!length(lam) || !length(th)) return(NA_real_)
  s <- sum(lam)
  den <- s^2 + sum(th)
  if (!is.finite(den) || den <= 0) return(NA_real_)
  s^2 / den
}

item_analysis <- function(dat) {
  k <- ncol(dat)
  total <- rowSums(dat, na.rm = TRUE)
  out <- lapply(seq_len(k), function(j) {
    corrected_total <- total - dat[[j]]
    rit <- suppressWarnings(
      stats::cor(dat[[j]], corrected_total, use = "pairwise.complete.obs")
    )
    data.frame(
      item = names(dat)[j],
      mean = mean(dat[[j]], na.rm = TRUE),
      sd = stats::sd(dat[[j]], na.rm = TRUE),
      corrected_item_total = rit,
      alpha_if_deleted = cronbach_alpha(dat[, -j, drop = FALSE]),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, out)
}

# ------------------------------------------------------------
# Simulasi skala Likert
# ------------------------------------------------------------
simulate_likert_scale <- function(n = 300, k = 12, loading = .65, reverse_n = 0,
                                  bad_n = 0, seed = 1234) {
  set.seed(seed)
  trait <- stats::rnorm(n)
  out <- matrix(NA_real_, nrow = n, ncol = k)
  loadings <- rep(loading, k)
  
  bad_idx <- if (bad_n > 0) utils::tail(seq_len(k), min(bad_n, k)) else integer(0)
  if (length(bad_idx)) loadings[bad_idx] <- stats::runif(length(bad_idx), .05, .25)
  
  for (j in seq_len(k)) {
    y <- loadings[j] * trait + sqrt(max(1 - loadings[j]^2, .05)) * stats::rnorm(n)
    out[, j] <- as.integer(cut(y, breaks = c(-Inf, -.85, -.20, .35, 1.05, Inf),
                               labels = 1:5))
  }
  
  reverse_idx <- if (reverse_n > 0) seq_len(min(reverse_n, k)) else integer(0)
  if (length(reverse_idx)) out[, reverse_idx] <- 6 - out[, reverse_idx]
  
  colnames(out) <- sprintf("Item_%02d", seq_len(k))
  list(data = as.data.frame(out), trait = trait, loadings = loadings,
       reverse_idx = reverse_idx, bad_idx = bad_idx)
}

# REL-01: kurva panjang tes.
#
# Versi lama menghitung alpha untuk k item PERTAMA. Karena simulasi menaruh
# reverse item di kolom awal dan item lemah di kolom akhir, kurva itu
# sebenarnya menggambarkan urutan kolom, bukan efek panjang tes: pada setelan
# wajar nilainya bisa turun sampai alpha negatif lalu naik lagi.
#
# Versi ini menambahkan item dalam urutan acak dan merata-ratakan banyak
# permutasi, sehingga pertanyaan yang dijawab benar-benar "apa efek menambah
# item dengan kualitas seperti ini?". Kurva urutan asli tetap dihitung agar
# keduanya dapat dibandingkan secara eksplisit.
alpha_length_curve <- function(dat, n_perm = 120, seed = 1) {
  k <- ncol(dat)
  if (k < 2) return(NULL)
  ks <- 2:k
  set.seed(seed)
  
  perms <- vapply(seq_len(n_perm), function(i) {
    ord <- sample(k)
    vapply(ks, function(j) cronbach_alpha(dat[, ord[seq_len(j)], drop = FALSE]),
           numeric(1))
  }, numeric(length(ks)))
  if (is.null(dim(perms))) perms <- matrix(perms, nrow = length(ks))
  
  data.frame(
    items = ks,
    alpha_random = rowMeans(perms, na.rm = TRUE),
    lo = apply(perms, 1, stats::quantile, probs = .10, na.rm = TRUE),
    hi = apply(perms, 1, stats::quantile, probs = .90, na.rm = TRUE),
    alpha_ordered = vapply(ks, function(j)
      cronbach_alpha(dat[, seq_len(j), drop = FALSE]), numeric(1)),
    stringsAsFactors = FALSE
  )
}

# ------------------------------------------------------------
# Classical Test Theory
# ------------------------------------------------------------
simulate_ctt_scores <- function(n = 500, reliability = .80, test_mean = 50,
                                test_sd = 10, person_z = 0, repeats = 100,
                                seed = 2026) {
  set.seed(seed)
  reliability <- clamp(reliability, .001, .999)
  sd_true <- test_sd * sqrt(reliability)
  sem <- test_sd * sqrt(1 - reliability)
  
  true <- stats::rnorm(n, test_mean, sd_true)
  error <- stats::rnorm(n, 0, sem)
  observed <- true + error
  
  person_true <- test_mean + person_z * sd_true
  repeat_scores <- person_true + stats::rnorm(repeats, 0, sem)
  
  list(
    data = data.frame(true = true, error = error, observed = observed),
    person_true = person_true,
    repeat_scores = repeat_scores,
    target_reliability = reliability,
    empirical_reliability = stats::var(true) / stats::var(observed),
    corr_sq = suppressWarnings(stats::cor(true, observed)^2),
    sem = sem,
    sd_true = sd_true,
    config = list(n = n, reliability = reliability, test_mean = test_mean,
                  test_sd = test_sd, person_z = person_z, repeats = repeats,
                  seed = seed)
  )
}

# ------------------------------------------------------------
# Factor analysis
# ------------------------------------------------------------
# FA-01: communality yang benar untuk faktor yang berkorelasi adalah
# diagonal dari Lambda %*% Phi %*% t(Lambda), bukan rowSums(Lambda^2).
# Versi lama membuat varians item melebihi 1 ketika ada cross-loading
# sekaligus factor correlation (mis. 1,294 pada cross .35 dan r .60),
# sehingga loading yang dipulihkan factanal tidak sebanding dengan angka
# yang disetel pengguna — dan arah biasnya bahkan berbalik pada setelan
# ekstrem. Di sini uniqueness diturunkan dari communality yang benar,
# dan loading yang terlalu besar diskalakan agar varians item tepat 1.
factor_sim <- function(n = 500, n_factors = 2, items_per_factor = 5,
                       loading = .70, cross_loading = .10, factor_r = .30,
                       seed = 123, max_communality = .90) {
  set.seed(seed)
  p <- n_factors * items_per_factor
  
  Rf <- matrix(factor_r, n_factors, n_factors)
  diag(Rf) <- 1
  ev <- eigen(Rf, symmetric = TRUE)$values
  if (min(ev) <= 0) Rf <- Rf + diag(abs(min(ev)) + .01, n_factors)
  
  Lambda <- matrix(0, p, n_factors)
  for (f in seq_len(n_factors)) {
    idx <- ((f - 1) * items_per_factor + 1):(f * items_per_factor)
    Lambda[idx, f] <- loading
    if (n_factors > 1 && cross_loading > 0) {
      other <- if (f == n_factors) 1 else f + 1
      Lambda[idx, other] <- cross_loading
    }
  }
  
  communal <- diag(Lambda %*% Rf %*% t(Lambda))
  too_big <- communal > max_communality
  if (any(too_big)) {
    Lambda[too_big, ] <- Lambda[too_big, , drop = FALSE] *
      sqrt(max_communality / communal[too_big])
    communal <- diag(Lambda %*% Rf %*% t(Lambda))
  }
  uniqueness <- pmax(1 - communal, .01)
  
  Lc <- chol(Rf)
  Fscores <- matrix(stats::rnorm(n * n_factors), n, n_factors) %*% Lc
  E <- matrix(stats::rnorm(n * p), n, p) %*% diag(sqrt(uniqueness), p)
  X <- Fscores %*% t(Lambda) + E
  colnames(X) <- sprintf("Item_%02d", seq_len(p))
  
  list(data = as.data.frame(X), lambda = Lambda, Rf = Rf, factors = Fscores,
       communality = communal, uniqueness = uniqueness,
       implied_var = communal + uniqueness,
       rescaled = any(too_big))
}

parallel_random_eigen <- function(n, p, reps = 100, seed = 123) {
  set.seed(seed)
  vals <- replicate(reps, {
    z <- matrix(stats::rnorm(n * p), n, p)
    eigen(stats::cor(z), symmetric = TRUE, only.values = TRUE)$values
  })
  apply(vals, 1, stats::quantile, probs = .95, na.rm = TRUE)
}

parallel_retain <- function(observed, random_cutoff) {
  above <- observed > random_cutoff
  if (!length(above) || !above[1]) return(0L)
  first_fail <- which(!above)[1]
  if (is.na(first_fail)) length(above) else first_fail - 1L
}

# ============================================================
# Item Response Theory
# ============================================================
dichotomous_models <- c("Rasch", "2PL", "3PL", "4PL")
polytomous_models <- c("RSM", "PCM", "GRM")
is_dichotomous <- function(model) model %in% dichotomous_models
is_polytomous <- function(model) model %in% polytomous_models

default_step_deviations <- function(n_categories = 4) {
  n_steps <- max(2, n_categories - 1)
  seq(-(n_steps - 1) / 2, (n_steps - 1) / 2, length.out = n_steps)
}

format_step_deviations <- function(x) {
  paste(format(round(x, 2), trim = TRUE), collapse = ", ")
}

parse_numeric_vector <- function(x) {
  if (is.null(x) || !nzchar(trimws(x))) return(numeric(0))
  parts <- unlist(strsplit(gsub(";", ",", x), "[,[:space:]]+"))
  parts <- parts[nzchar(parts)]
  suppressWarnings(as.numeric(parts))
}

prepare_step_deviations <- function(x, n_categories = 4, ordered = FALSE) {
  n_steps <- n_categories - 1
  raw <- parse_numeric_vector(x)
  valid <- length(raw) == n_steps && all(is.finite(raw))
  if (!valid) raw <- default_step_deviations(n_categories)
  centered <- raw - mean(raw)
  was_unsorted <- ordered && is.unsorted(centered, strictly = TRUE)
  if (ordered) centered <- sort(centered)
  list(values = centered, valid = valid, was_unsorted = was_unsorted)
}

irt_prob <- function(theta, a = 1, b = 0, c = 0, d = 1, model = "2PL") {
  if (model == "Rasch") { a <- 1; c <- 0; d <- 1 }
  if (model == "2PL") { c <- 0; d <- 1 }
  if (model == "3PL") d <- 1
  L <- stats::plogis(a * (theta - b))
  c + (d - c) * L
}

irt_info <- function(theta, a = 1, b = 0, c = 0, d = 1, model = "2PL") {
  if (model == "Rasch") { a <- 1; c <- 0; d <- 1 }
  if (model == "2PL") { c <- 0; d <- 1 }
  if (model == "3PL") d <- 1
  L <- stats::plogis(a * (theta - b))
  P <- c + (d - c) * L
  dP <- (d - c) * a * L * (1 - L)
  P <- clamp(P, 1e-10, 1 - 1e-10)
  (dP^2) / (P * (1 - P))
}

pcm_probability_matrix <- function(theta, b = 0, step_deviations) {
  theta <- as.numeric(theta)
  steps <- step_deviations - mean(step_deviations)
  delta <- b + steps
  k <- length(delta) + 1
  out <- matrix(NA_real_, nrow = length(theta), ncol = k)
  for (i in seq_along(theta)) {
    eta <- c(0, cumsum(theta[i] - delta))
    eta <- eta - max(eta)
    w <- exp(eta)
    out[i, ] <- w / sum(w)
  }
  colnames(out) <- paste0("P", 0:(k - 1))
  out
}

grm_probability_matrix <- function(theta, a = 1, b = 0, threshold_deviations) {
  theta <- as.numeric(theta)
  dev <- sort(threshold_deviations - mean(threshold_deviations))
  thresholds <- b + dev
  k <- length(thresholds) + 1
  ge <- sapply(thresholds, function(th) stats::plogis(a * (theta - th)))
  if (length(theta) == 1) ge <- matrix(ge, nrow = 1)
  out <- matrix(0, nrow = length(theta), ncol = k)
  out[, 1] <- 1 - ge[, 1]
  if (k > 2) for (j in 2:(k - 1)) out[, j] <- ge[, j - 1] - ge[, j]
  out[, k] <- ge[, k - 1]
  out <- pmax(out, 0)
  out <- out / rowSums(out)
  colnames(out) <- paste0("P", 0:(k - 1))
  out
}

item_probability_matrix <- function(theta, item, model, common_steps = NULL) {
  if (is_dichotomous(model)) {
    p <- irt_prob(theta, item$a, item$b, item$c, item$d, model)
    return(cbind(P0 = 1 - p, P1 = p))
  }
  if (model == "RSM") return(pcm_probability_matrix(theta, item$b, common_steps))
  if (model == "PCM") return(pcm_probability_matrix(theta, item$b, item$steps))
  if (model == "GRM") return(grm_probability_matrix(theta, item$a, item$b, item$steps))
  stop("Model IRT tidak dikenali.")
}

# Fisher information untuk respons politomus: sum[(dP_k/dtheta)^2 / P_k],
# dihitung secara numerik dengan beda terpusat.
item_information_general <- function(theta, item, model, common_steps = NULL) {
  if (is_dichotomous(model)) {
    return(irt_info(theta, item$a, item$b, item$c, item$d, model))
  }
  h <- 1e-4
  p0 <- item_probability_matrix(theta, item, model, common_steps)
  pp <- item_probability_matrix(theta + h, item, model, common_steps)
  pm <- item_probability_matrix(theta - h, item, model, common_steps)
  dp <- (pp - pm) / (2 * h)
  rowSums((dp^2) / pmax(p0, 1e-12))
}

item_expected_score <- function(theta, item, model, common_steps = NULL) {
  p <- item_probability_matrix(theta, item, model, common_steps)
  as.numeric(p %*% (0:(ncol(p) - 1)))
}

# ============================================================
# DIF, Measurement Invariance, Validity
# ============================================================

# ------------------------------------------------------------
# DIF-01: tes pendek, bukan satu item terisolasi.
#
# Versi lama mencocokkan peserta pada theta hasil simulasi — nilai yang
# dalam analisis nyata tidak pernah tersedia. Di sini dibangkitkan item
# jangkar yang berfungsi sama pada kedua group plus satu item yang
# dipelajari, sehingga matching dapat dilakukan pada true theta, skor
# total, atau rest score (skor total tanpa item yang diuji). Rest score
# adalah versi sederhana dari purification: item yang sedang diperiksa
# tidak boleh ikut menentukan kelompok pembandingnya sendiri.
# ------------------------------------------------------------
simulate_dif_test <- function(n_per_group = 600, n_anchor = 19, a = 1.4, b = 0,
                              delta_a = 0, delta_b = .5, focal_theta_mean = 0,
                              seed = 606) {
  set.seed(seed)
  a_focal <- clamp(a + delta_a, .20, 3.50)
  b_focal <- b + delta_b
  
  theta <- c(stats::rnorm(n_per_group, 0, 1),
             stats::rnorm(n_per_group, focal_theta_mean, 1))
  group <- factor(rep(c("Reference", "Focal"), each = n_per_group),
                  levels = c("Reference", "Focal"))
  
  anchor_b <- seq(-1.6, 1.6, length.out = n_anchor)
  anchor <- vapply(seq_len(n_anchor), function(j) {
    stats::rbinom(length(theta), 1, stats::plogis(1.0 * (theta - anchor_b[j])))
  }, numeric(length(theta)))
  colnames(anchor) <- sprintf("anchor_%02d", seq_len(n_anchor))
  
  p_studied <- ifelse(group == "Reference",
                      stats::plogis(a * (theta - b)),
                      stats::plogis(a_focal * (theta - b_focal)))
  studied <- stats::rbinom(length(theta), 1, p_studied)
  
  rest <- rowSums(anchor)
  dat <- data.frame(group = group, theta = theta, studied = studied,
                    rest = rest, total = rest + studied,
                    stringsAsFactors = FALSE)
  
  list(data = dat, anchor = as.data.frame(anchor),
       a_ref = a, b_ref = b, a_focal = a_focal, b_focal = b_focal,
       config = list(n_per_group = n_per_group, n_anchor = n_anchor, a = a, b = b,
                     delta_a = delta_a, delta_b = delta_b,
                     focal_theta_mean = focal_theta_mean, seed = seed))
}

dif_matching_variable <- function(dat, method = "rest") {
  switch(method,
         theta = dat$theta,
         total = dat$total,
         rest = dat$rest,
         dat$rest)
}

dif_logistic <- function(dat, method = "rest") {
  m <- dif_matching_variable(dat, method)
  d <- data.frame(response = dat$studied, match = m, group = dat$group)
  fit <- suppressWarnings(stats::glm(response ~ match * group, data = d,
                                     family = stats::binomial()))
  co <- summary(fit)$coefficients
  rows <- rownames(co)
  list(
    fit = fit,
    table = data.frame(term = rows, estimate = co[, 1], se = co[, 2],
                       z = co[, 3], p = co[, 4], row.names = NULL,
                       stringsAsFactors = FALSE),
    p_group = co[match("groupFocal", rows), 4],
    p_inter = co[match("match:groupFocal", rows), 4]
  )
}

# DIF-02: Mantel-Haenszel, Delta MH, dan klasifikasi ETS.
# alpha_MH = sum(A_k D_k / N_k) / sum(B_k C_k / N_k), dengan A = Reference
# benar, B = Reference salah, C = Focal benar, D = Focal salah.
# Delta MH = -2.35 * ln(alpha_MH); nilai negatif berarti item lebih
# menguntungkan Reference group.
mantel_haenszel <- function(resp, group, strata) {
  idx_by_stratum <- split(seq_along(resp), strata)
  num <- 0; den <- 0; sumA <- 0; sumE <- 0; sumV <- 0; used <- 0
  
  for (idx in idx_by_stratum) {
    r <- resp[idx]; g <- group[idx]
    N <- length(idx)
    nR <- sum(g == "Reference"); nF <- N - nR
    n1 <- sum(r == 1); n0 <- N - n1
    if (N < 2 || nR == 0 || nF == 0 || n1 == 0 || n0 == 0) next
    A <- sum(r == 1 & g == "Reference")
    B <- nR - A
    C <- sum(r == 1 & g == "Focal")
    D <- nF - C
    num <- num + A * D / N
    den <- den + B * C / N
    sumA <- sumA + A
    sumE <- sumE + nR * n1 / N
    sumV <- sumV + (nR * nF * n1 * n0) / (N^2 * (N - 1))
    used <- used + 1
  }
  
  if (used == 0 || den <= 0 || num <= 0 || sumV <= 0) return(NULL)
  
  alpha_mh <- num / den
  delta_mh <- -2.35 * log(alpha_mh)
  chisq <- (abs(sumA - sumE) - 0.5)^2 / sumV
  p <- stats::pchisq(chisq, df = 1, lower.tail = FALSE)
  
  # Klasifikasi ETS. Aturan penuh mensyaratkan Delta MH berbeda secara
  # signifikan dari 1,0 untuk kategori C; di sini dipakai versi ringkas
  # yang lazim pada bahan ajar dan sudah cukup menunjukkan bahwa p kecil
  # tidak otomatis berarti DIF yang besar.
  ets <- if (!is.finite(p) || p >= .05 || abs(delta_mh) < 1.0) {
    "A (dapat diabaikan)"
  } else if (abs(delta_mh) >= 1.5) {
    "C (besar)"
  } else {
    "B (sedang)"
  }
  
  list(alpha_mh = alpha_mh, delta_mh = delta_mh, chisq = chisq, p = p,
       ets = ets, n_strata = used)
}

# Strata untuk MH. Skor diskret dikelompokkan agar tidak ada sel kosong;
# theta kontinu dibagi menjadi kuantil.
dif_strata <- function(match_value, method = "rest", n_bins = 10) {
  if (method == "theta") {
    br <- stats::quantile(match_value, probs = seq(0, 1, length.out = n_bins + 1),
                          na.rm = TRUE)
    br[1] <- -Inf; br[length(br)] <- Inf
    return(cut(match_value, breaks = unique(br), include.lowest = TRUE))
  }
  br <- unique(stats::quantile(match_value, probs = seq(0, 1, length.out = n_bins + 1),
                               na.rm = TRUE, type = 1))
  if (length(br) < 3) return(factor(match_value))
  br[1] <- -Inf; br[length(br)] <- Inf
  cut(match_value, breaks = br, include.lowest = TRUE)
}

# ------------------------------------------------------------
# MI-01: item yang dimanipulasi dapat dipilih.
#
# Pada lavaan, indicator pertama menjadi marker variable dan loading-nya
# di-fix ke 1 pada kedua group. Jika item yang sengaja dibuat berbeda
# justru item itu, perbedaannya tidak dapat muncul sebagai loading yang
# berbeda: ia terserap ke skala faktor dan menyebar ke item lain.
# Verifikasi pada kode versi lama: Delta loading -.35 pada item 1 memberi
# Delta CFI metric -0,0143, sedangkan manipulasi yang sama pada item 3
# memberi -0,0215, dan modification index terbesar justru menunjuk
# item 2-6, bukan item 1.
# ------------------------------------------------------------
simulate_mi_data <- function(n_per_group = 400, n_items = 6, loading = .70,
                             target_item = 3, delta_loading = 0,
                             delta_intercept = 0, residual_ratio = 1,
                             latent_mean_diff = 0, seed = 2026) {
  set.seed(seed)
  k <- n_items
  target_item <- clamp(as.integer(target_item), 1L, k)
  
  lam_a <- rep(loading, k)
  lam_b <- lam_a
  lam_b[target_item] <- clamp(loading + delta_loading, .15, .95)
  
  int_a <- rep(0, k)
  int_b <- int_a
  int_b[target_item] <- delta_intercept
  
  resid_a <- sqrt(pmax(1 - lam_a^2, .10))
  resid_b <- sqrt(pmax(1 - lam_b^2, .10))
  resid_b[target_item] <- resid_b[target_item] * residual_ratio
  
  eta_a <- stats::rnorm(n_per_group, 0, 1)
  eta_b <- stats::rnorm(n_per_group, latent_mean_diff, 1)
  
  ya <- vapply(seq_len(k), function(j)
    int_a[j] + lam_a[j] * eta_a + stats::rnorm(n_per_group, 0, resid_a[j]),
    numeric(n_per_group))
  yb <- vapply(seq_len(k), function(j)
    int_b[j] + lam_b[j] * eta_b + stats::rnorm(n_per_group, 0, resid_b[j]),
    numeric(n_per_group))
  colnames(ya) <- colnames(yb) <- paste0("i", seq_len(k))
  
  dat <- rbind(data.frame(group = "Group A", ya, check.names = FALSE),
               data.frame(group = "Group B", yb, check.names = FALSE))
  dat$group <- factor(dat$group, levels = c("Group A", "Group B"))
  
  list(data = dat, lam_a = lam_a, lam_b = lam_b, int_a = int_a, int_b = int_b,
       resid_a = resid_a, resid_b = resid_b, target_item = target_item,
       config = list(n_per_group = n_per_group, n_items = k, loading = loading,
                     target_item = target_item, delta_loading = delta_loading,
                     delta_intercept = delta_intercept,
                     residual_ratio = residual_ratio,
                     latent_mean_diff = latent_mean_diff, seed = seed))
}

# MI-02: urutan invariance lengkap dengan chi-square difference dan
# dukungan partial invariance lewat group.partial.
fit_invariance_sequence <- function(sim, partial = character(0)) {
  items <- paste0("i", seq_len(sim$config$n_items))
  model <- paste0("F =~ ", paste(items, collapse = " + "))
  
  safe_cfa <- function(equal = NULL) {
    tryCatch(
      suppressWarnings(lavaan::cfa(
        model, data = sim$data, group = "group", meanstructure = TRUE,
        group.equal = equal,
        group.partial = if (length(partial)) partial else NULL
      )),
      error = function(e) NULL
    )
  }
  
  fits <- list(
    Configural = safe_cfa(NULL),
    Metric = safe_cfa("loadings"),
    Scalar = safe_cfa(c("loadings", "intercepts")),
    Strict = safe_cfa(c("loadings", "intercepts", "residuals"))
  )
  
  converged <- function(f) {
    !is.null(f) && isTRUE(tryCatch(lavaan::lavInspect(f, "converged"),
                                   error = function(e) FALSE))
  }
  
  rows <- lapply(names(fits), function(nm) {
    f <- fits[[nm]]
    if (!converged(f)) {
      return(data.frame(model = nm, cfi = NA_real_, rmsea = NA_real_,
                        srmr = NA_real_, chisq = NA_real_, df = NA_real_,
                        stringsAsFactors = FALSE))
    }
    fm <- tryCatch(lavaan::fitMeasures(f, c("cfi", "rmsea", "srmr", "chisq", "df")),
                   error = function(e) rep(NA_real_, 5))
    data.frame(model = nm, cfi = unname(fm["cfi"]), rmsea = unname(fm["rmsea"]),
               srmr = unname(fm["srmr"]), chisq = unname(fm["chisq"]),
               df = unname(fm["df"]), stringsAsFactors = FALSE)
  })
  tab <- do.call(rbind, rows)
  rownames(tab) <- NULL
  
  tab$delta_cfi <- c(NA, diff(tab$cfi))
  tab$delta_rmsea <- c(NA, diff(tab$rmsea))
  tab$delta_srmr <- c(NA, diff(tab$srmr))
  
  # Uji chi-square difference antar model nested berurutan.
  tab$lrt_chisq <- NA_real_
  tab$lrt_df <- NA_real_
  tab$lrt_p <- NA_real_
  seq_names <- names(fits)
  for (i in 2:length(seq_names)) {
    f0 <- fits[[seq_names[i - 1]]]
    f1 <- fits[[seq_names[i]]]
    if (!converged(f0) || !converged(f1)) next
    lr <- tryCatch(suppressWarnings(lavaan::lavTestLRT(f0, f1)),
                   error = function(e) NULL)
    if (is.null(lr) || nrow(lr) < 2) next
    tab$lrt_chisq[i] <- lr[2, "Chisq diff"]
    tab$lrt_df[i] <- lr[2, "Df diff"]
    tab$lrt_p[i] <- lr[2, "Pr(>Chisq)"]
  }
  
  list(fits = fits, table = tab, model = model, partial = partial)
}

# Modification index untuk constraint equality, diterjemahkan ke nama
# parameter yang terbaca. Di sinilah pertanyaan "item mana yang
# bermasalah" benar-benar dijawab.
invariance_score_table <- function(fit, top = 8) {
  if (is.null(fit)) return(NULL)
  st <- tryCatch(lavaan::lavTestScore(fit)$uni, error = function(e) NULL)
  if (is.null(st) || !nrow(st)) return(NULL)
  pt <- lavaan::parTable(fit)
  
  label_of <- function(plab) {
    row <- pt[pt$plabel == plab, , drop = FALSE]
    if (!nrow(row)) return(plab)
    op <- row$op[1]
    kind <- if (op == "=~") "loading" else if (op == "~1") "intercept" else
      if (op == "~~") "residual" else op
    item <- if (op == "=~") row$rhs[1] else row$lhs[1]
    paste0(kind, " ", item)
  }
  
  st$parameter <- vapply(st$lhs, label_of, character(1))
  st <- st[order(-st$X2), , drop = FALSE]
  st$constraint <- paste0(st$lhs, " == ", st$rhs)
  utils::head(st[, c("parameter", "constraint", "X2", "df", "p.value")], top)
}

# Daftar constraint yang dapat dibebaskan, dalam format group.partial.
invariance_partial_options <- function(n_items) {
  items <- paste0("i", seq_len(n_items))
  c(paste0("F=~", items), paste0(items, "~1"))
}

# ------------------------------------------------------------
# Validity Lab
#
# X = T_x + E_x dengan var(T_x) = rel_x dan var(E_x) = 1 - rel_x, sehingga
# var(X) = 1 dan cor(X, T_x) = sqrt(rel_x). Dengan konstruksi ini,
# r_xy teramati = rho_true * sqrt(rel_x * rel_y): tepat rumus attenuation.
# ------------------------------------------------------------
simulate_validity <- function(n = 1000, rho_true = .50, rel_x = .80, rel_y = .70,
                              select_top_pct = 100, seed = 2026) {
  set.seed(seed)
  rho_true <- clamp(rho_true, -.99, .99)
  rel_x <- clamp(rel_x, .05, .99)
  rel_y <- clamp(rel_y, .05, .99)
  
  z1 <- stats::rnorm(n)
  z2 <- rho_true * z1 + sqrt(1 - rho_true^2) * stats::rnorm(n)
  
  true_x <- z1 * sqrt(rel_x)
  true_y <- z2 * sqrt(rel_y)
  x <- true_x + stats::rnorm(n, 0, sqrt(1 - rel_x))
  y <- true_y + stats::rnorm(n, 0, sqrt(1 - rel_y))
  
  dat <- data.frame(true_x = true_x, true_y = true_y, x = x, y = y)
  
  r_true <- stats::cor(true_x, true_y) / sqrt(rel_x * rel_y)  # korelasi skor murni
  r_obs <- stats::cor(x, y)
  
  keep_pct <- clamp(select_top_pct, 5, 100)
  cutoff <- stats::quantile(x, probs = 1 - keep_pct / 100)
  selected <- x >= cutoff
  dat$selected <- selected
  
  sd_full <- stats::sd(x)
  sd_sel <- stats::sd(x[selected])
  r_sel <- if (sum(selected) > 3) stats::cor(x[selected], y[selected]) else NA_real_
  
  # Koreksi attenuation (Spearman)
  r_dis <- r_obs / sqrt(rel_x * rel_y)
  
  # Koreksi range restriction (Thorndike Case II) pada sampel terseleksi
  ratio <- sd_full / sd_sel
  r_rr <- if (is.finite(r_sel)) {
    (r_sel * ratio) / sqrt(1 - r_sel^2 + r_sel^2 * ratio^2)
  } else NA_real_
  
  # Kedua koreksi: perbaiki range restriction dulu, lalu attenuation
  r_both <- if (is.finite(r_rr)) r_rr / sqrt(rel_x * rel_y) else NA_real_
  
  list(
    data = dat,
    rho_true = rho_true, r_true_scores = r_true,
    r_observed = r_obs, r_disattenuated = r_dis,
    r_selected = r_sel, r_range_corrected = r_rr, r_both_corrected = r_both,
    sd_full = sd_full, sd_selected = sd_sel, n_selected = sum(selected),
    max_possible = sqrt(rel_x * rel_y),
    config = list(n = n, rho_true = rho_true, rel_x = rel_x, rel_y = rel_y,
                  select_top_pct = keep_pct, seed = seed)
  )
}

# ============================================================
# UI — helper dan halaman bagian pertama
# ============================================================

# Input yang ikut disimpan ke query string agar setelan dapat dibagikan.
SHARE_INPUTS <- c(
  "irt_theta", "irt_model", "irt_n_items", "irt_ncat", "irt_common_steps",
  "cttl_n", "cttl_rel", "cttl_mean", "cttl_sd", "cttl_person_z", "cttl_repeats", "cttl_seed",
  "rel_n", "rel_k", "rel_loading", "rel_reverse", "rel_bad", "rel_seed",
  "fa_n", "fa_nf", "fa_ipf", "fa_loading", "fa_cross", "fa_fr", "fa_seed",
  "dif_theta", "dif_a", "dif_b", "dif_db", "dif_da", "dif_mean", "dif_n",
  "dif_anchor", "dif_match", "dif_seed",
  "mi_n", "mi_k", "mi_loading", "mi_item", "mi_dload", "mi_dint", "mi_resid",
  "mi_latent_mean", "mi_seed",
  "val_n", "val_rho", "val_relx", "val_rely", "val_select", "val_seed",
  "sc_n", "sc_k", "sc_loading", "sc_bad", "sc_reverse", "sc_cut", "sc_seed"
)

share_button <- function(id) {
  tagList(
    actionButton(id, "Salin tautan setelan ini", class = "btn-default",
                 style = "width:100%;margin-top:6px;"),
    div(class = "small-muted", style = "margin-top:4px;",
        "Tautan memuat seluruh posisi slider dan seed, sehingga hasilnya identik ketika dibuka orang lain.")
  )
}

module_hero <- function(title, blurb) {
  div(class = "hero", h2(title), p(blurb))
}

home_ui <- tabPanel(
  "Beranda", value = "Beranda",
  div(
    class = "hero",
    h2("Psychometrics Learning Lab"),
    p("Laboratorium interaktif untuk mempelajari konsep inti psikometrika melalui simulasi. ",
      "Setiap modul mengajak Anda mengubah parameter, mengamati konsekuensinya pada hasil pengukuran, ",
      "lalu menghubungkan konsep matematisnya dengan interpretasi psikometrik."),
    div(class = "small-muted",
        "Setiap modul menyertakan syntax R yang mereproduksi persis perhitungan di layar, ",
        "beserta unduhan data simulasinya. Aplikasi ini untuk pembelajaran; bukan pengganti analisis psikometrik pada data empiris.")
  ),
  fluidRow(
    column(3, div(class = "module-card", h4("IRT Playground"),
                  p("Hubungan antara latent trait, item parameters, response probability, Item Information, Test Information, dan Standard Error pada model Rasch, 2PL, 3PL, 4PL, RSM, PCM, dan GRM."),
                  actionLink("go_irt", "Buka modul →"))),
    column(3, div(class = "module-card", h4("CTT Laboratory"),
                  p("Classical Test Theory lewat model X = T + E: true score, observed score, measurement error, reliability, dan Standard Error of Measurement."),
                  actionLink("go_ctt", "Buka modul →"))),
    column(3, div(class = "module-card", h4("Reliability Explorer"),
                  p("Internal consistency dan precision lewat Cronbach's alpha dengan interval Feldt, McDonald's omega, mean inter-item correlation, SEM, serta efek panjang tes dan scoring error."),
                  actionLink("go_rel", "Buka modul →"))),
    column(3, div(class = "module-card", h4("Factor Analysis Lab"),
                  p("Latent structure lewat factor loading, cross-loading, factor correlation, communality, scree plot, dan parallel analysis."),
                  actionLink("go_fa", "Buka modul →")))
  ),
  fluidRow(
    column(3, div(class = "module-card", h4("DIF Explorer"),
                  p("Apakah item berfungsi berbeda antar-group pada level kemampuan yang sama. Matching dapat memakai true theta, skor total, atau rest score, dengan Mantel-Haenszel dan klasifikasi ETS."),
                  actionLink("go_dif", "Buka modul →"))),
    column(3, div(class = "module-card", h4("Measurement Invariance"),
                  p("Configural, metric, scalar, dan strict invariance dengan chi-square difference, modification index, serta partial invariance."),
                  actionLink("go_mi", "Buka modul →"))),
    column(3, div(class = "module-card", h4("Validity Lab"),
                  p("Hubungan reliability dan validity: attenuation karena error pengukuran, range restriction karena seleksi, dan koreksi untuk keduanya."),
                  actionLink("go_val", "Buka modul →"))),
    column(3, div(class = "module-card", h4("Scale Development Lab"),
                  p("Statistical item review: corrected item-total correlation, alpha if deleted, omega, dan konsekuensi penghapusan item satu per satu."),
                  actionLink("go_scale", "Buka modul →")))
  ),
  div(class = "footer", paste0("© 2026 Hanif Akhtar · versi ", APP_VERSION))
)

# ------------------------------------------------------------
irt_ui <- tabPanel(
  "IRT Playground", value = "IRT Playground",
  sidebarLayout(
    sidebarPanel(
      width = 3,
      help_block(
        "Apa yang dipelajari modul ini?",
        p("Item Response Theory memodelkan hubungan antara latent trait peserta dan item response. ",
          "Berbeda dari pendekatan yang hanya melihat skor total, IRT memberi parameter pada item, ",
          "sehingga terlihat item mana yang mudah atau sulit, tajam membedakan peserta, dan informatif pada bagian tertentu skala theta."),
        p(strong("Theta (θ)."), " Posisi peserta pada latent trait. Nol biasanya merepresentasikan pusat skala."),
        p(strong("a, b, c, d."), " a = discrimination, b = lokasi/difficulty, c = lower asymptote, d = upper asymptote. ",
          "Pada model politomus, step atau threshold menentukan transisi antar-kategori."),
        p(strong("Test Information & SE."), " Test Information menjumlahkan information seluruh item; ",
          "SE(θ) ≈ 1/sqrt(I_test(θ)), sehingga information yang lebih tinggi berarti SE lebih kecil.")
      ),
      div(class = "theta-box", h4("Theta peserta"),
          sliderInput("irt_theta", "Theta (θ)", min = -4, max = 4, value = 0, step = .1)),
      h4("Konfigurasi model"),
      selectInput("irt_model", "Model IRT",
                  c("Rasch", "2PL", "3PL", "4PL", "RSM", "PCM", "GRM"), selected = "2PL"),
      sliderInput("irt_n_items", "Jumlah item", min = 1, max = 3, value = 3, step = 1),
      conditionalPanel(
        "input.irt_model == 'RSM' || input.irt_model == 'PCM' || input.irt_model == 'GRM'",
        sliderInput("irt_ncat", "Jumlah kategori respons", min = 3, max = 6, value = 4, step = 1)
      ),
      conditionalPanel(
        "input.irt_model == 'RSM'",
        textInput("irt_common_steps", "Common step deviations", value = "-1, 0, 1"),
        div(class = "small-muted", "Masukkan K−1 angka. Nilai dicenter agar rata-ratanya 0 dan dipakai sama untuk semua item.")
      ),
      uiOutput("irt_model_note"),
      h4("Parameter item"),
      uiOutput("irt_item_controls"),
      uiOutput("irt_step_warning"),
      share_button("share_irt")
    ),
    mainPanel(
      width = 9,
      uiOutput("irt_metrics"),
      tabsetPanel(
        tabPanel("Response Curves", plotOutput("irt_icc", height = 400)),
        tabPanel("Item Information", plotOutput("irt_iif", height = 400)),
        tabPanel("Test Information & SE",
                 plotOutput("irt_tif", height = 265), plotOutput("irt_se", height = 265),
                 div(class = "help-note",
                     "Test Information dan Standard Error ditampilkan terpisah karena skalanya berbeda. Hubungan dasarnya SE(θ) ≈ 1/sqrt(I_test(θ)).")),
        tabPanel("Test Characteristic Curve", plotOutput("irt_tcc", height = 400)),
        tabPanel("Item Summary", DTOutput("irt_summary")),
        tabPanel("Interpretasi", uiOutput("irt_interpret")),
        tabPanel("Syntax R", uiOutput("irt_syntax"))
      )
    )
  )
)

# ------------------------------------------------------------
ctt_ui <- tabPanel(
  "CTT Laboratory", value = "CTT Laboratory",
  sidebarLayout(
    sidebarPanel(
      width = 3,
      help_block(
        "Apa yang dipelajari modul ini?",
        p("Classical Test Theory memakai model X = T + E. X adalah observed score, ",
          "T adalah true score dalam pengertian skor harapan dari equivalent measurements yang berulang, dan E adalah measurement error."),
        p(strong("Population reliability."), " Menentukan seberapa besar variasi observed score yang berkaitan dengan true-score variance."),
        p(strong("SEM."), " Standard Error of Measurement = SD × sqrt(1 − reliability), dalam satuan skor tes."),
        p(strong("Repeated Measurement."), " Memperlihatkan bahwa satu orang tidak selalu memperoleh skor identik walaupun true score-nya tetap.")
      ),
      h4("Classical Test Model"),
      sliderInput("cttl_n", "Jumlah peserta", 200, 3000, 600, step = 100),
      sliderInput("cttl_rel", "Population reliability", .30, .98, .80, step = .05),
      numericInput("cttl_mean", "Mean skor tes", 50),
      numericInput("cttl_sd", "SD skor tes", 10, min = 1),
      sliderInput("cttl_person_z", "Participant true score (z)", -2.5, 2.5, 0, step = .25),
      sliderInput("cttl_repeats", "Repeated measurements", 30, 500, 120, step = 10),
      numericInput("cttl_seed", "Seed", 2026, min = 1),
      actionButton("cttl_run", "Jalankan simulasi", class = "btn-primary"),
      share_button("share_ctt")
    ),
    mainPanel(
      width = 9,
      uiOutput("cttl_metrics"),
      tabsetPanel(
        tabPanel("X = T + E", plotOutput("cttl_scatter", height = 400),
                 uiOutput("cttl_dl")),
        tabPanel("Measurement Error", plotOutput("cttl_error", height = 370)),
        tabPanel("Repeated Measurement", plotOutput("cttl_repeat", height = 380)),
        tabPanel("Interpretasi", uiOutput("cttl_interpret")),
        tabPanel("Syntax R", uiOutput("cttl_syntax"))
      )
    )
  )
)

# ------------------------------------------------------------
rel_ui <- tabPanel(
  "Reliability Explorer", value = "Reliability Explorer",
  sidebarLayout(
    sidebarPanel(
      width = 3,
      help_block(
        "Apa yang dipelajari modul ini?",
        p("Reliability adalah derajat konsistensi atau presisi skor. Reliability tinggi berarti skor relatif lebih sedikit dipengaruhi error, ",
          "tetapi tidak otomatis berarti tes valid atau unidimensional."),
        p(strong("Alpha dan omega."), " Cronbach's alpha mengasumsikan seluruh item memberi kontribusi setara. ",
          "McDonald's omega dihitung dari factor loading, sehingga tidak menuntut asumsi itu. ",
          "Jarak antara keduanya melebar justru ketika ada item lemah atau reverse item yang belum direcode."),
        p(strong("Reverse items without recoding."), " Mensimulasikan scoring error yang lazim: item yang arahnya terbalik dapat menghasilkan corrected item-total correlation negatif."),
        p(strong("Kurva panjang tes."), " Item ditambahkan dalam urutan acak lalu dirata-ratakan, sehingga yang terlihat benar-benar efek menambah item, bukan efek urutan kolom.")
      ),
      h4("Simulasi skala"),
      sliderInput("rel_n", "Jumlah responden", 100, 1500, 400, step = 50),
      sliderInput("rel_k", "Jumlah item", 4, 30, 12, step = 1),
      sliderInput("rel_loading", "Item-trait relationship", .15, .90, .65, step = .05),
      sliderInput("rel_reverse", "Reverse items without recoding", 0, 6, 0, step = 1),
      sliderInput("rel_bad", "Number of weak items", 0, 6, 1, step = 1),
      numericInput("rel_seed", "Seed", 1234, min = 1),
      actionButton("rel_run", "Jalankan simulasi", class = "btn-primary"),
      share_button("share_rel")
    ),
    mainPanel(
      width = 9,
      uiOutput("rel_metrics"),
      tabsetPanel(
        tabPanel("Trait and Observed Score", plotOutput("rel_true_obs", height = 370),
                 uiOutput("rel_dl")),
        tabPanel("Item Analysis", DTOutput("rel_items")),
        tabPanel("Test Length", plotOutput("rel_alpha_curve", height = 380),
                 uiOutput("rel_curve_note")),
        tabPanel("Interpretasi", uiOutput("rel_interpret")),
        tabPanel("Syntax R", uiOutput("rel_syntax"))
      )
    )
  )
)

# ------------------------------------------------------------
fa_ui <- tabPanel(
  "Factor Analysis Lab", value = "Factor Analysis Lab",
  sidebarLayout(
    sidebarPanel(
      width = 3,
      help_block(
        "Apa yang dipelajari modul ini?",
        p("Factor Analysis mempelajari latent structure di balik inter-item correlations. ",
          "Item yang mengukur construct yang sama diharapkan berbagi common variance yang dapat direpresentasikan satu atau beberapa latent factor."),
        p(strong("Cross-loading dan factor correlation."), " Keduanya membuat simple structure lebih sulit terlihat. ",
          "Karena faktor boleh berkorelasi, solusi multi-faktor memakai oblique rotation (promax)."),
        p(strong("Parallel analysis."), " Membandingkan eigenvalue data dengan eigenvalue data acak untuk membantu menentukan jumlah faktor yang layak dipertahankan."),
        p(strong("Catatan."), " Loading yang dipulihkan tidak harus sama persis dengan yang Anda setel. ",
          "Pada cross-loading dan factor correlation yang besar, rotasi memang tidak dapat memulihkan pola pembangkitnya secara tepat — itu sifat rotasi, bukan kesalahan. ",
          "Tabel Factor Loadings menampilkan keduanya berdampingan.")
      ),
      h4("Factor structure simulation"),
      sliderInput("fa_n", "Jumlah responden", 200, 2000, 500, step = 50),
      sliderInput("fa_nf", "Number of factors", 1, 4, 2, step = 1),
      sliderInput("fa_ipf", "Items per factor", 3, 8, 5, step = 1),
      sliderInput("fa_loading", "Primary loading", .30, .90, .70, step = .05),
      sliderInput("fa_cross", "Cross-loading", 0, .40, .10, step = .05),
      sliderInput("fa_fr", "Factor correlation", 0, .80, .30, step = .05),
      numericInput("fa_seed", "Seed", 444, min = 1),
      actionButton("fa_run", "Jalankan simulasi", class = "btn-primary"),
      share_button("share_fa")
    ),
    mainPanel(
      width = 9,
      uiOutput("fa_metrics"),
      tabsetPanel(
        tabPanel("Scree & Parallel Analysis", plotOutput("fa_scree", height = 380),
                 uiOutput("fa_dl")),
        tabPanel("Factor Loadings", DTOutput("fa_loadings"), uiOutput("fa_loading_note")),
        tabPanel("Communalities", plotOutput("fa_communal", height = 380)),
        tabPanel("Interpretasi", uiOutput("fa_interpret")),
        tabPanel("Syntax R", uiOutput("fa_syntax"))
      )
    )
  )
)

# ------------------------------------------------------------
dif_ui <- tabPanel(
  "DIF Explorer", value = "DIF Explorer",
  sidebarLayout(
    sidebarPanel(
      width = 3,
      help_block(
        "Apa yang dipelajari modul ini?",
        p("Differential Item Functioning menanyakan apakah item berfungsi berbeda antar-group ",
          "setelah peserta dibandingkan pada level kemampuan yang sama. Reference dan Focal hanya label; ",
          "Focal tidak berarti group tersebut bermasalah atau lebih rendah."),
        p(strong("Matching variable."), " Inilah keputusan yang paling menentukan. ",
          "True theta hanya tersedia dalam simulasi. Skor total tersedia di data nyata, tetapi ikut terkontaminasi item yang ber-DIF. ",
          "Rest score membuang item yang sedang diperiksa dari skor pembandingnya — versi sederhana dari purification."),
        p(strong("Uniform vs non-uniform."), " Δb menggeser kurva (uniform); Δa mengubah kemiringannya, sehingga besar dan arah selisih berubah menurut theta (non-uniform)."),
        p(strong("Mantel-Haenszel dan ETS."), " ΔMH negatif berarti item lebih menguntungkan Reference group. ",
          "Klasifikasi A/B/C berbasis effect size, sehingga tidak ikut membesar hanya karena N bertambah.")
      ),
      div(class = "theta-box", h4("Theta yang diperiksa"),
          sliderInput("dif_theta", "Theta (θ)", -3, 3, 0, step = .1)),
      h4("1. Item pada Reference group"),
      sliderInput("dif_a", "Discrimination (a)", .4, 3, 1.4, step = .1),
      sliderInput("dif_b", "Difficulty (b)", -2.5, 2.5, 0, step = .1),
      h4("2. Pergeseran pada Focal group"),
      sliderInput("dif_db", "Focal difficulty shift (Δb)", -1.5, 1.5, .5, step = .1),
      sliderInput("dif_da", "Focal discrimination shift (Δa)", -1, 1, 0, step = .1),
      h4("3. Kemampuan dan pencocokan"),
      sliderInput("dif_mean", "Focal latent mean difference", -1.5, 1.5, 0, step = .1),
      div(class = "small-muted",
          "Parameter ini mengubah latent mean group, bukan fungsi item. Gunakan untuk membedakan group difference dari DIF."),
      br(),
      radioButtons("dif_match", "Matching variable",
                   choices = c("Rest score (tanpa item yang diuji)" = "rest",
                               "Skor total (termasuk item yang diuji)" = "total",
                               "True theta (hanya ada di simulasi)" = "theta"),
                   selected = "rest"),
      sliderInput("dif_n", "N per group", 200, 2000, 600, step = 100),
      sliderInput("dif_anchor", "Jumlah item jangkar", 9, 39, 19, step = 2),
      numericInput("dif_seed", "Seed", 606, min = 1),
      actionButton("dif_run", "Jalankan simulasi DIF", class = "btn-primary"),
      share_button("share_dif")
    ),
    mainPanel(
      width = 9,
      uiOutput("dif_metrics"),
      tabsetPanel(
        tabPanel("Item Curves by Group", plotOutput("dif_icc", height = 400)),
        tabPanel("Probability Difference", plotOutput("dif_gap", height = 380)),
        tabPanel("Empirical by Score", plotOutput("dif_empirical", height = 380),
                 uiOutput("dif_dl")),
        tabPanel("Statistical Detection", uiOutput("dif_detect"), DTOutput("dif_coef")),
        tabPanel("Interpretasi", uiOutput("dif_interpret")),
        tabPanel("Syntax R", uiOutput("dif_syntax"))
      )
    )
  )
)

# ------------------------------------------------------------
mi_ui <- tabPanel(
  "Measurement Invariance", value = "Measurement Invariance",
  sidebarLayout(
    sidebarPanel(
      width = 3,
      help_block(
        "Apa yang dipelajari modul ini?",
        p("Measurement Invariance mengevaluasi apakah measurement model memiliki makna yang comparable antar-group. ",
          "Group boleh berbeda pada construct-nya; yang diuji adalah apakah parameter pengukurannya cukup setara."),
        p(strong("Configural."), " Struktur faktor yang sama dapat dipakai pada tiap group."),
        p(strong("Metric."), " Factor loading dibatasi sama. Bila didukung, hubungan faktor dengan indicator cukup comparable."),
        p(strong("Scalar."), " Intercept juga dibatasi sama. Ini syarat penting untuk membandingkan latent mean."),
        p(strong("Strict."), " Residual variance juga dibatasi sama."),
        p(strong("Kenapa item target tidak default ke i1?"), " Pada lavaan, indicator pertama menjadi marker variable dan loading-nya di-fix ke 1 di kedua group. ",
          "Kalau item yang sengaja dibuat berbeda justru item itu, perbedaannya terserap ke skala faktor dan modification index akan menunjuk item yang salah.")
      ),
      h4("1. Model dasar"),
      sliderInput("mi_n", "N per group", 200, 1200, 450, step = 50),
      sliderInput("mi_k", "Number of indicators", 4, 10, 6, step = 1),
      sliderInput("mi_loading", "Baseline factor loading", .40, .90, .70, step = .05),
      h4("2. Perbedaan pada Group B"),
      uiOutput("mi_item_selector"),
      sliderInput("mi_dload", "Factor loading difference (Δloading)", -.35, .35, 0, step = .05),
      sliderInput("mi_dint", "Intercept difference (Δintercept)", -1.2, 1.2, 0, step = .1),
      sliderInput("mi_resid", "Residual SD ratio (B/A)", .50, 1.80, 1, step = .05),
      div(class = "small-muted",
          "Mulai dengan ketiganya netral (0, 0, dan 1). Ubah satu parameter pada satu waktu agar efeknya pada setiap level invariance mudah dilihat."),
      br(),
      sliderInput("mi_latent_mean", "Latent mean difference (Group B)", -1.5, 1.5, .4, step = .1),
      div(class = "small-muted",
          "Latent mean boleh berbeda tanpa menunjukkan measurement non-invariance."),
      br(),
      h4("3. Partial invariance"),
      uiOutput("mi_partial_selector"),
      numericInput("mi_seed", "Seed", 808, min = 1),
      actionButton("mi_run", "Uji measurement invariance", class = "btn-primary"),
      share_button("share_mi")
    ),
    mainPanel(
      width = 9,
      uiOutput("mi_metrics"),
      tabsetPanel(
        tabPanel("Fit Changes", plotOutput("mi_fit_plot", height = 400)),
        tabPanel("Model Comparison", DTOutput("mi_table"), uiOutput("mi_lrt_note")),
        tabPanel("Item mana yang bermasalah?", uiOutput("mi_score_intro"), DTOutput("mi_score")),
        tabPanel("Item target", plotOutput("mi_item_plot", height = 380), DTOutput("mi_params"),
                 uiOutput("mi_dl")),
        tabPanel("Interpretasi", uiOutput("mi_interpret")),
        tabPanel("Syntax R", uiOutput("mi_syntax"))
      )
    )
  )
)

# ------------------------------------------------------------
val_ui <- tabPanel(
  "Validity Lab", value = "Validity Lab",
  sidebarLayout(
    sidebarPanel(
      width = 3,
      help_block(
        "Apa yang dipelajari modul ini?",
        p("Reliability membatasi validity. Korelasi antara dua skor teramati tidak dapat melebihi ",
          "akar dari hasil kali reliability keduanya, betapapun kuat hubungan sebenarnya."),
        p(strong("Attenuation."), " r teramati = ρ × sqrt(rel_x × rel_y). Error pengukuran menekan korelasi ke arah nol."),
        p(strong("Range restriction."), " Bila peserta diseleksi pada prediktor, variasi prediktor mengecil dan korelasi ikut mengecil. ",
          "Ini persis situasi validasi tes seleksi: kriteria hanya terlihat pada mereka yang diterima."),
        p(strong("Koreksi."), " Koreksi attenuation dan koreksi Thorndike Case II mengembalikan estimasi ke nilai sebenarnya. ",
          "Keduanya adalah estimasi, bukan pengukuran ulang — jangan dilaporkan tanpa menyebut nilai tidak terkoreksinya.")
      ),
      h4("Hubungan sebenarnya"),
      sliderInput("val_rho", "Korelasi true score (ρ)", 0, .90, .50, step = .05),
      h4("Kualitas pengukuran"),
      sliderInput("val_relx", "Reliability prediktor (X)", .30, .98, .80, step = .02),
      sliderInput("val_rely", "Reliability kriteria (Y)", .30, .98, .70, step = .02),
      h4("Seleksi"),
      sliderInput("val_select", "Persentase teratas yang diterima", 10, 100, 100, step = 5, post = "%"),
      div(class = "small-muted",
          "100% berarti tanpa seleksi. Turunkan untuk melihat range restriction bekerja."),
      br(),
      sliderInput("val_n", "Jumlah pelamar", 500, 5000, 1200, step = 100),
      numericInput("val_seed", "Seed", 2026, min = 1),
      actionButton("val_run", "Jalankan simulasi", class = "btn-primary"),
      share_button("share_val")
    ),
    mainPanel(
      width = 9,
      uiOutput("val_metrics"),
      tabsetPanel(
        tabPanel("Attenuation", plotOutput("val_atten", height = 380), uiOutput("val_atten_note")),
        tabPanel("Range Restriction", plotOutput("val_scatter", height = 400), uiOutput("val_dl")),
        tabPanel("Ringkasan Koreksi", DTOutput("val_table")),
        tabPanel("Interpretasi", uiOutput("val_interpret")),
        tabPanel("Syntax R", uiOutput("val_syntax"))
      )
    )
  )
)

# ------------------------------------------------------------
scale_ui <- tabPanel(
  "Scale Development", value = "Scale Development",
  sidebarLayout(
    sidebarPanel(
      width = 3,
      help_block(
        "Apa yang dipelajari modul ini?",
        p("Scale development adalah proses membangun dan meninjau item agar skala merepresentasikan target construct secara memadai. ",
          "Item statistics membantu menemukan potensi masalah, tetapi tujuannya bukan memaksimalkan alpha."),
        p(strong("Kenapa 'Review', bukan 'Delete'."), " Corrected item-total correlation yang rendah bisa muncul karena item lemah, ",
          "reverse scoring yang belum benar, wording yang ambigu, variance respons yang sempit, atau karena item mewakili aspek construct yang berbeda namun tetap penting."),
        p(strong("Penghapusan bertahap."), " Menghapus item satu per satu, bukan sekaligus, karena setiap penghapusan mengubah skor total ",
          "dan karena itu mengubah corrected item-total correlation seluruh item lain."),
        p(strong("Sebelum membuang item."), " Periksa kesesuaian dengan blueprint, scoring dan reverse coding, wording, distribusi respons, ",
          "factor loading bila tersedia, konten yang tumpang tindih, expert judgment, dan cakupan domain.")
      ),
      h4("Buat item pool"),
      sliderInput("sc_n", "Jumlah responden", 100, 1500, 400, step = 50),
      sliderInput("sc_k", "Jumlah item awal", 8, 40, 20, step = 1),
      sliderInput("sc_loading", "Primary item loading", .30, .85, .60, step = .05),
      sliderInput("sc_bad", "Number of weak items", 0, 8, 3, step = 1),
      sliderInput("sc_reverse", "Reverse items without recoding", 0, 6, 2, step = 1),
      sliderInput("sc_cut", "Corrected item-total cutoff", .10, .50, .30, step = .05),
      numericInput("sc_seed", "Seed", 777, min = 1),
      actionButton("sc_run", "Generate and Analyze", class = "btn-primary"),
      share_button("share_sc")
    ),
    mainPanel(
      width = 9,
      uiOutput("sc_metrics"),
      tabsetPanel(
        tabPanel("Decision Table", DTOutput("sc_table"), uiOutput("sc_dl")),
        tabPanel("Penghapusan Bertahap", plotOutput("sc_stepwise", height = 400),
                 uiOutput("sc_step_note")),
        tabPanel("Item Diagnostics", plotOutput("sc_diag", height = 400)),
        tabPanel("Interpretasi", uiOutput("sc_interpret")),
        tabPanel("Syntax R", uiOutput("sc_syntax"))
      )
    )
  )
)

# ------------------------------------------------------------
panduan_ui <- tabPanel(
  "Panduan", value = "Panduan",
  div(
    class = "hero",
    h2("Cara menggunakan Psychometrics Learning Lab"),
    p("Penjelasan tiap modul kini berada di dalam modulnya sendiri, pada panel ",
      strong("Apa yang dipelajari modul ini?"), " di bagian atas sidebar — terbuka ketika Anda mengkliknya, ",
      "tertutup ketika tidak diperlukan. Halaman ini hanya peta ringkas dan catatan yang berlaku untuk semua modul."),
    div(class = "small-muted",
        "Angka dan cutoff di aplikasi adalah alat belajar, bukan aturan keputusan otomatis untuk data penelitian.")
  ),
  
  div(
    class = "panel-card",
    h4("Alur yang disarankan"),
    p("Setiap modul dirancang untuk dipakai dengan pola yang sama:"),
    tags$ol(
      tags$li(strong("Tetapkan dugaan."), " Sebelum menggeser slider, putuskan dulu apa yang Anda perkirakan akan terjadi."),
      tags$li(strong("Ubah satu parameter."), " Mengubah dua sekaligus membuat penyebab dan akibat sulit dipisahkan."),
      tags$li(strong("Baca tab Interpretasi."), " Bagian ini membacakan angka yang sedang tampil, bukan teks umum."),
      tags$li(strong("Buka tab Syntax R."), " Salin kodenya, unduh datanya, lalu jalankan sendiri di R untuk memastikan angkanya sama.")
    )
  ),
  
  div(
    class = "panel-card",
    h4("Peta modul"),
    tags$ul(
      tags$li(strong("IRT Playground — "), "hubungan theta, parameter item, information, dan standard error."),
      tags$li(strong("CTT Laboratory — "), "X = T + E, reliability, dan SEM dalam satuan skor."),
      tags$li(strong("Reliability Explorer — "), "alpha dengan intervalnya, omega, inter-item correlation, dan efek panjang tes."),
      tags$li(strong("Factor Analysis Lab — "), "loading, cross-loading, korelasi faktor, communality, dan parallel analysis."),
      tags$li(strong("DIF Explorer — "), "fungsi item antar-group pada level kemampuan yang sama, dengan tiga pilihan matching variable."),
      tags$li(strong("Measurement Invariance — "), "configural sampai strict, chi-square difference, modification index, dan partial invariance."),
      tags$li(strong("Validity Lab — "), "batas yang reliability berikan pada validity, plus range restriction dan koreksinya."),
      tags$li(strong("Scale Development — "), "review item secara bertahap dengan statistik sebagai bahan pertimbangan, bukan penentu.")
    )
  ),
  
  div(
    class = "panel-card",
    h4("Membagikan setelan ke kelas"),
    p("Setiap modul memiliki tombol ", strong("Salin tautan setelan ini"), " di bagian bawah sidebar. ",
      "Tautan tersebut memuat seluruh posisi slider beserta seed, sehingga mahasiswa yang membukanya ",
      "melihat angka yang persis sama. Cocok untuk menuliskan tugas seperti ",
      tags$em("\"buka tautan ini, lalu jelaskan mengapa omega jauh di bawah alpha\""), ".")
  ),
  
  div(
    class = "panel-card",
    h4("Catatan yang berlaku di semua modul"),
    tags$ul(
      tags$li("Data disimulasikan, sehingga hubungan yang sebenarnya diketahui. Ini keunggulan untuk belajar, sekaligus alasan mengapa hasilnya selalu lebih rapi daripada data nyata."),
      tags$li("Setiap p-value dipengaruhi ukuran sampel. Perhatikan juga besar efeknya, bukan hanya apakah p kecil."),
      tags$li("Cutoff apa pun di aplikasi ini adalah alat penyaring awal. Keputusan akhir selalu memerlukan pertimbangan isi dan tujuan pengukuran.")
    )
  ),
  
  div(class = "footer", paste0("© 2026 Hanif Akhtar · versi ", APP_VERSION))
)

# ------------------------------------------------------------
ui <- navbarPage(
  title = "Psychometrics Learning Lab",
  id = "main_nav",
  theme = app_theme,
  header = tagList(
    tags$head(tags$style(HTML(app_css)), tags$script(HTML(app_js))),
    tags$button(id = "pl-theme-toggle", type = "button", class = "pl-theme-toggle",
                onclick = "PsyLab.toggleTheme();", title = "Ganti tema terang/gelap",
                HTML("&#9680;"))
  ),
  home_ui, irt_ui, ctt_ui, rel_ui, fa_ui, dif_ui, mi_ui, val_ui, scale_ui, panduan_ui
)

# ============================================================
# SERVER
# ============================================================

# Input yang dibuat dengan numericInput (sisanya sliderInput).
NUMERIC_INPUT_IDS <- c("cttl_mean", "cttl_sd", "cttl_seed", "rel_seed", "fa_seed",
                       "dif_seed", "mi_seed", "val_seed", "sc_seed")
SELECT_INPUT_IDS <- c("irt_model")
TEXT_INPUT_IDS <- c("irt_common_steps")
RADIO_INPUT_IDS <- c("dif_match")

server <- function(input, output, session) {
  
  # ----------------------------------------------------------
  # Navigasi
  # ----------------------------------------------------------
  go <- function(id, target) {
    observeEvent(input[[id]], updateNavbarPage(session, "main_nav", selected = target))
  }
  go("go_irt", "IRT Playground")
  go("go_ctt", "CTT Laboratory")
  go("go_rel", "Reliability Explorer")
  go("go_fa", "Factor Analysis Lab")
  go("go_dif", "DIF Explorer")
  go("go_mi", "Measurement Invariance")
  go("go_val", "Validity Lab")
  go("go_scale", "Scale Development")
  
  # ----------------------------------------------------------
  # UI-03: berbagi setelan lewat query string
  # ----------------------------------------------------------
  # ------------------------------------------------------------
  # Palet grafik mengikuti tema aplikasi (dilaporkan dari JS via input$pl_theme).
  # ------------------------------------------------------------
  is_dark    <- function() identical(input$pl_theme, "dark")
  pt         <- function(base_size = 12.5) psy_theme(base_size, dark = is_dark())
  col_blue   <- function() if (is_dark()) PSY_DARK$blue   else PSY_BLUE
  col_accent <- function() if (is_dark()) PSY_DARK$accent else PSY_ACCENT
  col_grey   <- function() if (is_dark()) PSY_DARK$grey   else PSY_GREY
  col_ink    <- function() if (is_dark()) PSY_DARK$ink    else PSY_INK
  col_ok     <- function() if (is_dark()) PSY_DARK$ok     else PSY_OK
  col_panel  <- function() if (is_dark()) PSY_DARK$panel  else "#FFFFFF"
  
  build_query <- function() {
    vals <- lapply(SHARE_INPUTS, function(id) input[[id]])
    names(vals) <- SHARE_INPUTS
    vals <- vals[!vapply(vals, is.null, logical(1))]
    vals <- vals[vapply(vals, function(v) length(v) >= 1, logical(1))]
    if (!length(vals)) return("?")
    paste0("?", paste0(
      names(vals), "=",
      vapply(vals, function(v) utils::URLencode(as.character(v)[1], reserved = TRUE),
             character(1)),
      collapse = "&"
    ))
  }
  
  share_settings <- function() {
    qs <- build_query()
    updateQueryString(qs, mode = "replace")
    cd <- session$clientData
    port <- cd$url_port
    full <- paste0(cd$url_protocol, "//", cd$url_hostname,
                   if (!is.null(port) && nzchar(as.character(port))) paste0(":", port) else "",
                   cd$url_pathname, qs)
    session$sendCustomMessage("psylab_copy", list(text = full))
    showNotification("Tautan setelan disalin ke clipboard.", type = "message", duration = 5)
  }
  
  for (btn in c("share_irt", "share_ctt", "share_rel", "share_fa", "share_dif",
                "share_mi", "share_val", "share_sc")) {
    local({
      b <- btn
      observeEvent(input[[b]], share_settings())
    })
  }
  
  observeEvent(session$clientData$url_search, once = TRUE, {
    q <- parseQueryString(session$clientData$url_search)
    if (!length(q)) return()
    restored <- 0
    for (id in intersect(names(q), SHARE_INPUTS)) {
      raw <- q[[id]]
      if (id %in% SELECT_INPUT_IDS) {
        updateSelectInput(session, id, selected = raw)
      } else if (id %in% TEXT_INPUT_IDS) {
        updateTextInput(session, id, value = raw)
      } else if (id %in% RADIO_INPUT_IDS) {
        updateRadioButtons(session, id, selected = raw)
      } else {
        num <- suppressWarnings(as.numeric(raw))
        if (!is.finite(num)) next
        if (id %in% NUMERIC_INPUT_IDS) {
          updateNumericInput(session, id, value = num)
        } else {
          updateSliderInput(session, id, value = num)
        }
      }
      restored <- restored + 1
    }
    if (restored > 0) {
      showNotification(
        paste0("Setelan dari tautan dipulihkan (", restored, " parameter). ",
               "Tekan tombol jalankan pada modul yang ingin dilihat."),
        type = "message", duration = 8
      )
    }
  })
  
  # ----------------------------------------------------------
  # Panel syntax R dan unduhan
  # ----------------------------------------------------------
  register_syntax <- function(out_id, dl_id, code_fun, fname) {
    output[[out_id]] <- renderUI({
      tagList(
        div(class = "help-note",
            "Kode di bawah memakai seed yang sama dengan simulasi di layar, sehingga ",
            "angkanya identik ketika dijalankan di R. Unduh datanya pada tab lain bila ",
            "Anda ingin menganalisis dataset yang persis sama tanpa membangkitkannya ulang."),
        syntax_panel(paste0(out_id, "_code"), code_fun(), dl_id)
      )
    })
    output[[dl_id]] <- downloadHandler(
      filename = function() fname,
      content = function(file) writeLines(code_fun(), file)
    )
  }
  
  register_data_dl <- function(dl_id, fname, data_fun) {
    output[[dl_id]] <- downloadHandler(
      filename = function() fname,
      content = function(file) utils::write.csv(data_fun(), file, row.names = FALSE)
    )
  }
  
  dl_row <- function(...) div(class = "dl-row", ...)
  
  # ----------------------------------------------------------
  # Penyesuaian slider yang saling bergantung
  # ----------------------------------------------------------
  observeEvent(input$rel_k, {
    updateSliderInput(session, "rel_reverse", max = min(6, input$rel_k),
                      value = min(input$rel_reverse %||% 0, input$rel_k))
    updateSliderInput(session, "rel_bad", max = min(6, input$rel_k),
                      value = min(input$rel_bad %||% 0, input$rel_k))
  })
  
  observeEvent(input$sc_k, {
    updateSliderInput(session, "sc_reverse", max = min(6, input$sc_k),
                      value = min(input$sc_reverse %||% 0, input$sc_k))
    updateSliderInput(session, "sc_bad", max = min(8, input$sc_k),
                      value = min(input$sc_bad %||% 0, input$sc_k))
  })
  
  # ==========================================================
  # IRT PLAYGROUND
  # ==========================================================
  output$irt_model_note <- renderUI({
    note <- switch(
      input$irt_model %||% "2PL",
      "Rasch" = "Dichotomous. a = 1 dan c = 0. Ubah b untuk melihat perubahan item location.",
      "2PL" = "Dichotomous. Setiap item memiliki discrimination (a) dan difficulty (b).",
      "3PL" = "Dichotomous. Ditambah lower asymptote (c), misalnya karena guessing.",
      "4PL" = "Dichotomous. Ditambah lower asymptote (c) dan upper asymptote (d).",
      "RSM" = "Polytomous. Semua item berbagi common step deviations; tiap item punya lokasi keseluruhan (b).",
      "PCM" = "Polytomous. Tiap item boleh punya step deviations sendiri; discrimination ditetapkan 1.",
      "GRM" = "Polytomous. Tiap item punya discrimination (a) dan ordered thresholds."
    )
    div(class = "model-note", note)
  })
  
  observeEvent(input$irt_ncat, ignoreInit = TRUE, {
    req(input$irt_ncat)
    updateTextInput(session, "irt_common_steps",
                    value = format_step_deviations(default_step_deviations(input$irt_ncat)))
  })
  
  output$irt_item_controls <- renderUI({
    n_items <- input$irt_n_items %||% 3
    model <- input$irt_model %||% "2PL"
    ncat <- input$irt_ncat %||% 4
    b_def <- c(-1, 0, 1); a_def <- c(1.0, 1.5, 2.0)
    c_def <- c(.10, .15, .20); d_def <- c(.95, .95, .95)
    step_text <- format_step_deviations(default_step_deviations(ncat))
    
    tagList(lapply(seq_len(n_items), function(i) {
      ctrl <- list(h5(paste("Item", i)))
      if (model %in% c("2PL", "3PL", "4PL", "GRM")) {
        ctrl <- c(ctrl, list(sliderInput(paste0("irt_a", i), "Discrimination (a)",
                                         .3, 3, a_def[i], step = .05)))
      }
      ctrl <- c(ctrl, list(sliderInput(paste0("irt_b", i), "Difficulty / location (b)",
                                       -3, 3, b_def[i], step = .1)))
      if (model %in% c("3PL", "4PL")) {
        ctrl <- c(ctrl, list(sliderInput(paste0("irt_c", i), "Lower asymptote (c)",
                                         0, .40, c_def[i], step = .01)))
      }
      if (model == "4PL") {
        ctrl <- c(ctrl, list(sliderInput(paste0("irt_d", i), "Upper asymptote (d)",
                                         .60, 1, d_def[i], step = .01)))
      }
      if (model %in% c("PCM", "GRM")) {
        lab <- if (model == "GRM") "Threshold deviations" else "Step deviations"
        ctrl <- c(ctrl, list(
          textInput(paste0("irt_steps", i), lab, value = step_text),
          div(class = "small-muted", paste0("Masukkan ", ncat - 1, " angka dipisahkan koma."))
        ))
      }
      do.call(div, c(list(class = "item-card"), ctrl))
    }))
  })
  
  irt_specs <- reactive({
    n_items <- input$irt_n_items %||% 3
    model <- input$irt_model %||% "2PL"
    ncat <- input$irt_ncat %||% 4
    warn <- character(0)
    
    common_steps <- NULL
    if (model == "RSM") {
      st <- prepare_step_deviations(input$irt_common_steps, ncat, ordered = FALSE)
      common_steps <- st$values
      if (!st$valid) {
        warn <- c(warn, paste0("Common step harus berisi tepat ", ncat - 1,
                               " angka. Nilai default sementara digunakan."))
      }
    }
    
    items <- lapply(seq_len(n_items), function(i) {
      val <- function(prefix, default) {
        x <- input[[paste0(prefix, i)]]
        if (is.null(x) || !is.finite(x)) default else x
      }
      b <- val("irt_b", c(-1, 0, 1)[i])
      a <- if (model %in% c("2PL", "3PL", "4PL", "GRM")) val("irt_a", c(1, 1.5, 2)[i]) else 1
      cpar <- if (model %in% c("3PL", "4PL")) val("irt_c", c(.10, .15, .20)[i]) else 0
      dpar <- if (model == "4PL") val("irt_d", .95) else 1
      if (dpar <= cpar) {
        dpar <- min(1, cpar + .05)
        warn <<- c(warn, paste0("Item ", i, ": d harus lebih besar dari c; d disesuaikan sementara."))
      }
      steps <- NULL
      if (model %in% c("PCM", "GRM")) {
        st <- prepare_step_deviations(input[[paste0("irt_steps", i)]], ncat,
                                      ordered = (model == "GRM"))
        steps <- st$values
        if (!st$valid) {
          warn <<- c(warn, paste0("Item ", i, ": diperlukan tepat ", ncat - 1,
                                  " step/threshold. Nilai default sementara digunakan."))
        }
        if (st$was_unsorted) {
          warn <<- c(warn, paste0("Item ", i, ": threshold GRM diurutkan otomatis agar category probabilities valid."))
        }
      }
      list(a = a, b = b, c = cpar, d = dpar, steps = steps)
    })
    
    list(model = model, ncat = ncat, items = items,
         common_steps = common_steps, warnings = unique(warn))
  })
  
  output$irt_step_warning <- renderUI({
    z <- irt_specs()
    if (!length(z$warnings)) return(NULL)
    div(class = "warning-note", lapply(z$warnings, div))
  })
  
  irt_test_values <- function(theta, specs) {
    infos <- vapply(specs$items, function(it)
      item_information_general(theta, it, specs$model, specs$common_steps)[1], numeric(1))
    expected <- vapply(specs$items, function(it)
      item_expected_score(theta, it, specs$model, specs$common_steps)[1], numeric(1))
    list(infos = infos, expected = expected,
         test_info = sum(infos), test_expected = sum(expected))
  }
  
  output$irt_metrics <- renderUI({
    z <- irt_specs(); tv <- irt_test_values(input$irt_theta, z)
    max_item <- if (is_dichotomous(z$model)) 1 else z$ncat - 1
    max_test <- length(z$items) * max_item
    se <- if (tv$test_info > 0) 1 / sqrt(tv$test_info) else NA_real_
    lbl <- if (is_dichotomous(z$model)) "Expected correct" else "Expected test score"
    div(class = "metric-grid",
        metric_box("Theta peserta", sprintf("%.2f", input$irt_theta), "garis vertikal pada grafik"),
        metric_box(lbl, sprintf("%.2f / %d", tv$test_expected, max_test), "pada θ peserta"),
        metric_box("Test information", sprintf("%.3f", tv$test_info),
                   paste(length(z$items), "item pada θ peserta")),
        metric_box("Approx. SE", if (is.finite(se)) sprintf("%.3f", se) else "NA",
                   "1/sqrt(test information)"),
        metric_box("Model", z$model))
  })
  
  output$irt_icc <- renderPlot({
    z <- irt_specs(); th <- seq(-4, 4, length.out = 401)
    if (is_dichotomous(z$model)) {
      d <- do.call(rbind, lapply(seq_along(z$items), function(i) {
        data.frame(theta = th,
                   prob = item_probability_matrix(th, z$items[[i]], z$model, z$common_steps)[, 2],
                   item = paste("Item", i), stringsAsFactors = FALSE)
      }))
      pts <- do.call(rbind, lapply(seq_along(z$items), function(i) {
        data.frame(theta = input$irt_theta,
                   prob = item_probability_matrix(input$irt_theta, z$items[[i]], z$model, z$common_steps)[1, 2],
                   item = paste("Item", i), stringsAsFactors = FALSE)
      }))
      ggplot(d, aes(theta, prob, linetype = item)) +
        geom_line(linewidth = 1.05, colour = col_blue()) +
        geom_vline(xintercept = input$irt_theta, linetype = 3, colour = col_grey()) +
        geom_point(data = pts, size = 2.6, colour = col_ink()) +
        coord_cartesian(ylim = c(0, 1)) +
        labs(x = expression(theta), y = "Probability correct", linetype = NULL,
             title = "Item Response Curves") +
        pt()
    } else {
      d <- do.call(rbind, lapply(seq_along(z$items), function(i) {
        p <- item_probability_matrix(th, z$items[[i]], z$model, z$common_steps)
        do.call(rbind, lapply(seq_len(ncol(p)), function(k)
          data.frame(theta = th, prob = p[, k], item = paste("Item", i),
                     category = paste0("Category ", k - 1), stringsAsFactors = FALSE)))
      }))
      ggplot(d, aes(theta, prob, linetype = category)) +
        geom_line(linewidth = .95, colour = col_blue()) +
        geom_vline(xintercept = input$irt_theta, linetype = 3, colour = col_grey()) +
        facet_wrap(~item, ncol = min(length(z$items), 3)) +
        coord_cartesian(ylim = c(0, 1)) +
        labs(x = expression(theta), y = "Category probability", linetype = NULL,
             title = "Category Response Curves") +
        pt(11.5)
    }
  })
  
  output$irt_iif <- renderPlot({
    z <- irt_specs(); th <- seq(-4, 4, length.out = 401)
    d <- do.call(rbind, lapply(seq_along(z$items), function(i)
      data.frame(theta = th,
                 info = item_information_general(th, z$items[[i]], z$model, z$common_steps),
                 item = paste("Item", i), stringsAsFactors = FALSE)))
    pts <- data.frame(
      theta = input$irt_theta,
      info = vapply(z$items, function(it)
        item_information_general(input$irt_theta, it, z$model, z$common_steps)[1], numeric(1)),
      item = paste("Item", seq_along(z$items)), stringsAsFactors = FALSE)
    ggplot(d, aes(theta, info, linetype = item)) +
      geom_line(linewidth = 1.05, colour = col_blue()) +
      geom_vline(xintercept = input$irt_theta, linetype = 3, colour = col_grey()) +
      geom_point(data = pts, size = 2.5, colour = col_ink()) +
      labs(x = expression(theta), y = "Item information", linetype = NULL,
           title = "Item Information Functions") +
      pt()
  })
  
  irt_curve_data <- reactive({
    z <- irt_specs(); th <- seq(-4, 4, length.out = 401)
    info_mat <- sapply(z$items, function(it)
      item_information_general(th, it, z$model, z$common_steps))
    if (is.null(dim(info_mat))) info_mat <- matrix(info_mat, ncol = 1)
    exp_mat <- sapply(z$items, function(it)
      item_expected_score(th, it, z$model, z$common_steps))
    if (is.null(dim(exp_mat))) exp_mat <- matrix(exp_mat, ncol = 1)
    data.frame(theta = th, test_info = rowSums(info_mat), expected = rowSums(exp_mat))
  })
  
  output$irt_tif <- renderPlot({
    d <- irt_curve_data(); z <- irt_specs(); tv <- irt_test_values(input$irt_theta, z)
    ggplot(d, aes(theta, test_info)) +
      geom_line(linewidth = 1.05, colour = col_blue()) +
      geom_vline(xintercept = input$irt_theta, linetype = 3, colour = col_grey()) +
      geom_point(data = data.frame(theta = input$irt_theta, test_info = tv$test_info),
                 size = 2.6, colour = col_ink()) +
      labs(x = expression(theta), y = "Test information", title = "Test Information Function") +
      pt()
  })
  
  output$irt_se <- renderPlot({
    d <- irt_curve_data(); z <- irt_specs(); tv <- irt_test_values(input$irt_theta, z)
    d$se_display <- pmin(1 / sqrt(pmax(d$test_info, 1e-12)), 5)
    se_theta <- if (tv$test_info > 0) min(1 / sqrt(tv$test_info), 5) else NA_real_
    ggplot(d, aes(theta, se_display)) +
      geom_line(linewidth = 1.05, colour = col_accent()) +
      geom_vline(xintercept = input$irt_theta, linetype = 3, colour = col_grey()) +
      geom_point(data = data.frame(theta = input$irt_theta, se_display = se_theta),
                 size = 2.6, colour = col_ink(), na.rm = TRUE) +
      coord_cartesian(ylim = c(0, 5)) +
      labs(x = expression(theta), y = "Approx. SE (tampilan dibatasi 5)",
           title = "Measurement Precision") +
      pt()
  })
  
  output$irt_tcc <- renderPlot({
    d <- irt_curve_data(); z <- irt_specs(); tv <- irt_test_values(input$irt_theta, z)
    max_item <- if (is_dichotomous(z$model)) 1 else z$ncat - 1
    ggplot(d, aes(theta, expected)) +
      geom_line(linewidth = 1.05, colour = col_blue()) +
      geom_vline(xintercept = input$irt_theta, linetype = 3, colour = col_grey()) +
      geom_point(data = data.frame(theta = input$irt_theta, expected = tv$test_expected),
                 size = 2.6, colour = col_ink()) +
      coord_cartesian(ylim = c(0, length(z$items) * max_item)) +
      labs(x = expression(theta), y = "Expected total score",
           title = "Test Characteristic Curve") +
      pt()
  })
  
  irt_summary_table <- reactive({
    z <- irt_specs(); theta <- input$irt_theta
    rows <- lapply(seq_along(z$items), function(i) {
      it <- z$items[[i]]
      probs <- item_probability_matrix(theta, it, z$model, z$common_steps)[1, ]
      info <- item_information_general(theta, it, z$model, z$common_steps)[1]
      ex <- item_expected_score(theta, it, z$model, z$common_steps)[1]
      base <- data.frame(Item = paste("Item", i), b = round(it$b, 2),
                         stringsAsFactors = FALSE)
      if (z$model %in% c("2PL", "3PL", "4PL", "GRM")) base$a <- round(it$a, 2)
      if (z$model %in% c("3PL", "4PL")) base$c <- round(it$c, 2)
      if (z$model == "4PL") base$d <- round(it$d, 2)
      if (z$model %in% c("RSM", "PCM", "GRM")) {
        st <- if (z$model == "RSM") z$common_steps else it$steps
        base[["Step/threshold"]] <- format_step_deviations(st)
      }
      if (is_dichotomous(z$model)) {
        base[["P(correct)"]] <- round(probs[2], 3)
      } else {
        base[["Expected score"]] <- round(ex, 3)
        for (k in seq_along(probs)) base[[paste0("P(Cat ", k - 1, ")")]] <- round(probs[k], 3)
      }
      base$Information <- round(info, 3)
      base
    })
    do.call(rbind, rows)
  })
  
  output$irt_summary <- renderDT({
    datatable(irt_summary_table(), rownames = FALSE,
              options = list(dom = "t", scrollX = TRUE, pageLength = 3))
  })
  
  output$irt_interpret <- renderUI({
    z <- irt_specs(); tv <- irt_test_values(input$irt_theta, z)
    se <- if (tv$test_info > 0) 1 / sqrt(tv$test_info) else NA_real_
    best <- which.max(tv$infos)
    model_text <- switch(
      z$model,
      "Rasch" = "Pada Rasch semua item memiliki discrimination yang sama, sehingga perbedaan utama antar-item adalah lokasi difficulty.",
      "2PL" = "Pada 2PL item dapat berbeda pada difficulty maupun discrimination. Item dengan a lebih tinggi berkurva lebih curam dan dapat memberi information lebih besar pada rentang yang sempit.",
      "3PL" = "Pada 3PL parameter c memberi peluang benar pada kemampuan sangat rendah, dan itu menurunkan information di bagian bawah skala.",
      "4PL" = "Pada 4PL parameter c dan d membatasi minimum dan maksimum response probability, sehingga respons tidak harus mendekati 0 atau 1 pada ekstrem kemampuan.",
      "RSM" = "Pada RSM semua item memakai struktur step yang sama; item terutama berbeda pada lokasi keseluruhannya.",
      "PCM" = "Pada PCM tiap item boleh memiliki struktur step berbeda, sehingga perpindahan antar-kategori dapat terjadi pada lokasi yang berbeda.",
      "GRM" = "Pada GRM discrimination dan ordered thresholds menentukan seberapa tajam dan di mana category probability berubah."
    )
    tagList(
      div(class = "panel-card", h4("Konsep utama"),
          p("IRT memandang respons item sebagai fungsi dari posisi peserta pada latent trait dan karakteristik item. ",
            "Karena itu item yang sama tidak harus sama informatifnya bagi semua peserta."),
          p(model_text)),
      div(class = "panel-card", h4("Membaca hasil saat ini"),
          p(sprintf("Pada θ = %.2f, total information dari %d item adalah %.3f, dan approximate SE sekitar %.3f.",
                    input$irt_theta, length(z$items), tv$test_info, se)),
          p(sprintf("Item %d memberi information terbesar (%.3f), sehingga item itulah yang paling membantu membedakan peserta di sekitar θ yang sedang dipilih.",
                    best, tv$infos[best])),
          p(if (length(z$items) > 1)
            "Test Information bukan rata-rata dan bukan information item terbaik saja; nilainya adalah jumlah kontribusi semua item pada θ yang sama."
            else "Tambahkan Item 2 dan Item 3 untuk melihat bagaimana information dari beberapa item dijumlahkan."),
          p("Karena SE berbanding terbalik dengan akar information, kurva SE turun tepat di tempat kurva Test Information naik.")),
      div(class = "insight", strong("Coba ini: "),
          if (is_dichotomous(z$model))
            "buat tiga item dengan b = −1, 0, dan 1 serta a yang sama, lalu geser θ dari −2 ke +2. Setelah itu naikkan a salah satu item dan perhatikan kurva serta information-nya."
          else
            "gunakan tiga item dengan empat kategori, lalu ubah step atau threshold satu item saja dan amati perubahan category response curves serta expected total score.")
    )
  })
  
  irt_code <- reactive({
    z <- irt_specs()
    par_lines <- paste(vapply(seq_along(z$items), function(i) {
      it <- z$items[[i]]
      if (is_dichotomous(z$model)) {
        sprintf("  list(a = %.2f, b = %.2f, c = %.2f, d = %.2f)", it$a, it$b, it$c, it$d)
      } else if (z$model == "RSM") {
        sprintf("  list(b = %.2f)", it$b)
      } else {
        sprintf("  list(a = %.2f, b = %.2f, steps = c(%s))", it$a, it$b,
                paste(sprintf("%.2f", it$steps), collapse = ", "))
      }
    }, character(1)), collapse = ",\n")
    
    common <- if (z$model == "RSM")
      sprintf("common_steps <- c(%s)\n", paste(sprintf("%.2f", z$common_steps), collapse = ", "))
    else ""
    
    fn <- if (is_dichotomous(z$model)) {
      sprintf('
# Model %s: probabilitas dan information
irt_prob <- function(theta, a, b, c = 0, d = 1) c + (d - c) * plogis(a * (theta - b))
irt_info <- function(theta, a, b, c = 0, d = 1) {
  L  <- plogis(a * (theta - b))
  P  <- c + (d - c) * L
  dP <- (d - c) * a * L * (1 - L)
  P  <- pmin(pmax(P, 1e-10), 1 - 1e-10)
  dP^2 / (P * (1 - P))
}

theta <- %.2f
info  <- sapply(items, function(it) irt_info(theta, it$a, it$b, it$c, it$d))
prob  <- sapply(items, function(it) irt_prob(theta, it$a, it$b, it$c, it$d))

data.frame(item = seq_along(items), P = round(prob, 3), I = round(info, 3))
sum(info)            # test information
1 / sqrt(sum(info))  # approximate SE', z$model, input$irt_theta)
    } else if (z$model == "GRM") {
      sprintf('
# GRM: probabilitas kategori dari selisih cumulative logistic
grm_probs <- function(theta, a, b, dev) {
  th <- b + sort(dev - mean(dev))
  ge <- sapply(th, function(t) plogis(a * (theta - t)))
  k  <- length(th) + 1
  p  <- numeric(k)
  p[1] <- 1 - ge[1]
  if (k > 2) for (j in 2:(k - 1)) p[j] <- ge[j - 1] - ge[j]
  p[k] <- ge[k - 1]
  p / sum(p)
}

theta <- %.2f
t(sapply(items, function(it) round(grm_probs(theta, it$a, it$b, it$steps), 3)))', input$irt_theta)
    } else {
      sprintf('
# %s: probabilitas kategori dari adjacent-category logit
pcm_probs <- function(theta, b, steps) {
  delta <- b + (steps - mean(steps))
  eta   <- c(0, cumsum(theta - delta))
  w     <- exp(eta - max(eta))
  w / sum(w)
}

theta <- %.2f
%s', z$model, input$irt_theta,
if (z$model == "RSM")
  "t(sapply(items, function(it) round(pcm_probs(theta, it$b, common_steps), 3)))"
else
  "t(sapply(items, function(it) round(pcm_probs(theta, it$b, it$steps), 3)))")
    }

paste0("# Psychometrics Learning Lab — IRT Playground\n",
       "# Model: ", z$model, "\n\n",
       common,
       "items <- list(\n", par_lines, "\n)\n", fn, "\n")
  })
  
  register_syntax("irt_syntax", "irt_syntax_dl", irt_code, "irt_playground.R")
  
  # ==========================================================
  # CTT LABORATORY
  # ==========================================================
  cttl_data <- eventReactive(input$cttl_run, ignoreNULL = FALSE, {
    simulate_ctt_scores(input$cttl_n, input$cttl_rel, input$cttl_mean, input$cttl_sd,
                        input$cttl_person_z, input$cttl_repeats, input$cttl_seed)
  })
  
  output$cttl_metrics <- renderUI({
    z <- cttl_data()
    div(class = "metric-grid",
        metric_box("Target reliability", sprintf("%.2f", z$target_reliability)),
        metric_box("Var(T)/Var(X) empiris", sprintf("%.3f", z$empirical_reliability)),
        metric_box("Corr(T,X)²", sprintf("%.3f", z$corr_sq),
                   "secara aljabar sama dengan kolom sebelumnya"),
        metric_box("SEM", sprintf("%.2f", z$sem), "SD dari error score"),
        metric_box("True score peserta", sprintf("%.2f", z$person_true)))
  })
  
  output$cttl_scatter <- renderPlot({
    z <- cttl_data()
    ggplot(z$data, aes(true, observed)) +
      geom_point(alpha = .30, colour = col_blue()) +
      geom_abline(slope = 1, intercept = 0, linetype = 2, colour = col_accent()) +
      labs(x = "True score (T)", y = "Observed score (X)",
           title = "Observed score = True score + Error",
           caption = "Garis putus-putus adalah X = T. Sebaran di sekitarnya adalah measurement error.") +
      pt()
  })
  
  output$cttl_error <- renderPlot({
    z <- cttl_data()
    ggplot(z$data, aes(error)) +
      geom_histogram(bins = 40, fill = col_blue(), colour = col_panel(), linewidth = .2) +
      geom_vline(xintercept = 0, linetype = 2, colour = col_accent()) +
      labs(x = "Measurement error (E)", y = "Frekuensi",
           title = sprintf("Distribusi error, SEM = %.2f", z$sem)) +
      pt()
  })
  
  output$cttl_repeat <- renderPlot({
    z <- cttl_data()
    d <- data.frame(rep = seq_along(z$repeat_scores), score = z$repeat_scores)
    ggplot(d, aes(rep, score)) +
      geom_point(alpha = .6, colour = col_blue()) +
      geom_hline(yintercept = z$person_true, linetype = 2, colour = col_accent()) +
      geom_hline(yintercept = z$person_true + c(-1, 1) * 1.96 * z$sem,
                 linetype = 3, colour = col_grey()) +
      labs(x = "Pengukuran ke-", y = "Observed score",
           title = "Skor berulang untuk orang yang sama",
           caption = "Putus-putus = true score; titik-titik = true score ± 1,96 SEM") +
      pt()
  })
  
  output$cttl_dl <- renderUI({
    dl_row(downloadButton("cttl_data_dl", "Unduh data simulasi (CSV)", class = "btn-default"))
  })
  register_data_dl("cttl_data_dl", "ctt_simulation.csv", function() cttl_data()$data)
  
  output$cttl_interpret <- renderUI({
    z <- cttl_data()
    tagList(
      div(class = "panel-card", h4("Classical Test Theory"),
          p("CTT memakai model X = T + E. Observed score tidak selalu sama dengan true score karena measurement error. ",
            "Dalam kerangka ini, reliability berkaitan dengan proporsi observed-score variance yang dapat dikaitkan dengan true-score variance."),
          p(strong("Dua metric yang mirip. "),
            "Var(T)/Var(X) dan Corr(T,X)² memang hampir identik di sini, dan itu bukan kebetulan: ",
            "ketika error tidak berkorelasi dengan true score, keduanya sama secara aljabar. ",
            "Keduanya ditampilkan justru untuk memperlihatkan bahwa dua definisi reliability yang terdengar berbeda bertemu di titik yang sama.")),
      div(class = "panel-card", h4("Membaca simulasi saat ini"),
          p(sprintf("Reliability disetel %.2f. Dengan SD tes %.2f, SEM = %.2f.",
                    z$target_reliability, z$config$test_sd, z$sem)),
          p(sprintf("Pada sampel ini Var(T)/Var(X) = %.3f. Nilai empiris tidak harus persis sama dengan target karena sampel mengandung variasi acak.",
                    z$empirical_reliability)),
          p(sprintf("Peserta yang dipilih memiliki true score %.2f. Pada grafik Repeated Measurement, setiap titik adalah skor yang mungkin diperoleh ketika true score-nya tetap tetapi error-nya berubah.",
                    z$person_true)),
          p("SEM berada dalam satuan skor tes, sehingga langsung dapat dibandingkan dengan selisih skor yang dianggap bermakna dalam konteks penggunaannya.")),
      div(class = "insight", strong("Coba ini: "),
          "pertahankan mean 50 dan SD 10, lalu bandingkan reliability .50, .80, dan .95. Perhatikan SEM dan sebaran skor berulang, walaupun true score peserta tidak berubah.")
    )
  })
  
  cttl_code <- reactive({
    z <- cttl_data()$config
    sprintf('# Psychometrics Learning Lab — CTT Laboratory
set.seed(%d)

n    <- %d
rel  <- %.2f      # population reliability
mu   <- %s        # mean skor tes
sd_x <- %s        # SD skor tes

sd_true <- sd_x * sqrt(rel)        # SD true score
sem     <- sd_x * sqrt(1 - rel)    # standard error of measurement

true     <- rnorm(n, mu, sd_true)
error    <- rnorm(n, 0, sem)
observed <- true + error

var(true) / var(observed)   # reliability empiris
cor(true, observed)^2       # definisi yang setara
sem

# Satu peserta, diukur berulang kali
person_true <- mu + %.2f * sd_true
repeats     <- person_true + rnorm(%d, 0, sem)
quantile(repeats, c(.025, .975))
person_true + c(-1, 1) * 1.96 * sem   # pembanding teoretis
', z$seed, z$n, z$reliability, format(z$test_mean), format(z$test_sd),
            z$person_z, z$repeats)
  })
  register_syntax("cttl_syntax", "cttl_syntax_dl", cttl_code, "ctt_laboratory.R")
  
  # ==========================================================
  # RELIABILITY EXPLORER
  # ==========================================================
  rel_data <- eventReactive(input$rel_run, ignoreNULL = FALSE, {
    simulate_likert_scale(input$rel_n, input$rel_k, input$rel_loading,
                          input$rel_reverse, input$rel_bad, input$rel_seed)
  })
  
  rel_summary <- reactive({
    s <- rel_data(); dat <- s$data
    alpha <- cronbach_alpha(dat)
    ci <- alpha_feldt_ci(alpha, nrow(dat), ncol(dat))
    total <- rowSums(dat)
    list(
      s = s, dat = dat, total = total,
      alpha = alpha, ci = ci,
      omega = omega_total(dat),
      sem = stats::sd(total) * sqrt(max(1 - alpha, 0)),
      mean_rit = mean(item_analysis(dat)$corrected_item_total, na.rm = TRUE),
      mean_r = mean_interitem_correlation(dat),
      sb_double = spearman_brown(alpha, 2)
    )
  })
  
  output$rel_metrics <- renderUI({
    z <- rel_summary()
    ci_txt <- if (all(is.finite(z$ci))) sprintf("95%% CI [%.3f, %.3f]", z$ci["lo"], z$ci["hi"]) else NULL
    gap <- z$omega - z$alpha
    div(class = "metric-grid",
        metric_box("Cronbach alpha", sprintf("%.3f", z$alpha), ci_txt),
        metric_box("McDonald omega", fmt_n(z$omega),
                   if (is.finite(gap)) sprintf("selisih dari alpha %+.3f", gap) else NULL),
        metric_box("Mean inter-item r", sprintf("%.3f", z$mean_r)),
        metric_box("Mean corrected item-total", sprintf("%.3f", z$mean_rit)),
        metric_box("SEM (dari alpha)", sprintf("%.2f", z$sem), "satuan skor total"),
        metric_box("SB bila digandakan", sprintf("%.3f", z$sb_double), "prediksi Spearman-Brown"))
  })
  
  output$rel_true_obs <- renderPlot({
    z <- rel_summary()
    d <- data.frame(true = z$s$trait, observed = standardize(z$total))
    ggplot(d, aes(true, observed)) +
      geom_point(alpha = .40, colour = col_blue()) +
      geom_smooth(method = "lm", formula = y ~ x, se = FALSE, colour = col_accent()) +
      labs(x = "Latent true score (terstandar)", y = "Observed total score (terstandar)",
           title = "Trait dan skor teramati") +
      pt()
  })
  
  output$rel_items <- renderDT({
    d <- item_analysis(rel_summary()$dat)
    d[, 2:5] <- round(d[, 2:5], 3)
    names(d) <- c("Item", "Mean", "SD", "Corrected item-total", "Alpha if deleted")
    datatable(d, rownames = FALSE, options = list(pageLength = 12, scrollX = TRUE))
  })
  
  rel_curve <- reactive({
    alpha_length_curve(rel_summary()$dat, n_perm = 120, seed = input$rel_seed %||% 1)
  })
  
  # REL-01: kurva panjang tes. Versi lama menambahkan item sesuai urutan kolom,
  # sehingga yang tergambar adalah efek urutan (reverse item di depan, item lemah
  # di belakang), bukan efek panjang tes.
  output$rel_alpha_curve <- renderPlot({
    cv <- rel_curve()
    validate(need(!is.null(cv), "Perlu minimal dua item."))
    d <- rbind(
      data.frame(items = cv$items, alpha = cv$alpha_random, jenis = "Urutan acak (rerata)"),
      data.frame(items = cv$items, alpha = cv$alpha_ordered, jenis = "Urutan kolom apa adanya")
    )
    ggplot(d, aes(items, alpha, linetype = jenis, colour = jenis)) +
      geom_ribbon(data = cv, inherit.aes = FALSE,
                  aes(x = items, ymin = lo, ymax = hi), fill = col_blue(), alpha = .13) +
      geom_line(linewidth = 1.05) +
      geom_hline(yintercept = 0, linewidth = .3, colour = col_grey()) +
      scale_colour_manual(values = c("Urutan acak (rerata)" = col_blue(),
                                     "Urutan kolom apa adanya" = col_accent())) +
      labs(x = "Jumlah item yang disertakan", y = "Cronbach's alpha",
           colour = NULL, linetype = NULL,
           title = "Pengaruh panjang tes terhadap reliability",
           caption = "Pita = rentang 10–90% dari 120 permutasi urutan item.") +
      pt()
  })
  
  output$rel_curve_note <- renderUI({
    cv <- rel_curve(); s <- rel_summary()$s
    if (is.null(cv)) return(NULL)
    mono <- all(diff(cv$alpha_random) > 0)
    gap <- max(abs(cv$alpha_random - cv$alpha_ordered))
    tagList(
      stat_readout(
        sprintf("Kurva acak %s naik monoton; selisih terbesar dari kurva urutan kolom = %.3f",
                if (mono) "" else "tidak", gap),
        paste0(
          "Garis biru menjawab pertanyaan yang sebenarnya ingin diajukan tab ini: apa efek ",
          "menambah item dengan kualitas seperti ini? Garis merah menambahkan item persis ",
          "sesuai urutan kolom simulasi — dan karena reverse item diletakkan di kolom awal ",
          "sementara item lemah di kolom akhir, garis itu lebih menggambarkan komposisi ",
          "item daripada panjang tes.",
          if (length(s$reverse_idx))
            sprintf(" Pada setelan sekarang ada %d reverse item yang belum direcode, sehingga jurangnya terlihat jelas.",
                    length(s$reverse_idx))
          else
            " Setel beberapa reverse item tanpa recoding untuk melihat kedua garis terpisah jauh."
        ),
        "alpha dihitung untuk 2..k item, dirata-ratakan dari 120 urutan acak"
      )
    )
  })
  
  output$rel_dl <- renderUI({
    dl_row(
      downloadButton("rel_data_dl", "Unduh data respons (CSV)", class = "btn-default"),
      downloadButton("rel_item_dl", "Unduh analisis item (CSV)", class = "btn-default")
    )
  })
  register_data_dl("rel_data_dl", "reliability_responses.csv", function() rel_summary()$dat)
  register_data_dl("rel_item_dl", "reliability_item_analysis.csv",
                   function() item_analysis(rel_summary()$dat))
  
  output$rel_interpret <- renderUI({
    z <- rel_summary(); s <- z$s
    quality <- if (is.finite(z$alpha) && z$alpha >= .80) "cukup tinggi dalam simulasi ini"
    else if (is.finite(z$alpha) && z$alpha >= .60) "sedang dalam simulasi ini"
    else "relatif rendah dalam simulasi ini"
    gap <- z$omega - z$alpha
    
    tagList(
      div(class = "panel-card", h4("Reliability"),
          p("Reliability menunjukkan konsistensi dan presisi skor. Reliability yang lebih tinggi umumnya menunjukkan proporsi measurement error yang lebih kecil, ",
            "tetapi reliability bukan validity: skala dapat menghasilkan skor yang konsisten dan tetap tidak merepresentasikan construct yang dimaksud."),
          p("Alpha mengasumsikan seluruh item memberi kontribusi setara terhadap true score. Omega dihitung dari factor loading, sehingga tidak menuntut asumsi itu. ",
            "Ketika asumsi alpha terpenuhi, keduanya berdekatan; ketika tidak, jaraknya melebar.")),
      div(class = "panel-card", h4("Membaca hasil saat ini"),
          p(sprintf("Alpha = %.3f (%s)%s, omega = %s, mean inter-item r = %.3f, dan mean corrected item-total = %.3f.",
                    z$alpha, quality,
                    if (all(is.finite(z$ci))) sprintf(" dengan 95%% CI [%.3f, %.3f]", z$ci["lo"], z$ci["hi"]) else "",
                    fmt_n(z$omega), z$mean_r, z$mean_rit)),
          if (is.finite(gap) && abs(gap) >= .05)
            p(strong(sprintf("Omega %s alpha sebesar %.3f. ", if (gap > 0) "melampaui" else "berada di bawah", abs(gap))),
              "Jarak sebesar ini menandakan item tidak memberi kontribusi setara — periksa tabel Item Analysis untuk melihat item mana penyebabnya.")
          else
            p("Alpha dan omega berdekatan, yang konsisten dengan item yang kontribusinya relatif setara."),
          p(sprintf("SEM berbasis alpha = %.2f dalam satuan skor total; prediksi Spearman-Brown bila panjang tes digandakan = %.3f.",
                    z$sem, z$sb_double)),
          if (length(s$reverse_idx) > 0)
            p(sprintf("Simulasi memasukkan %d reverse item yang sengaja belum direcode. Item tersebut dapat memiliki corrected item-total correlation negatif dan menurunkan alpha maupun omega.",
                      length(s$reverse_idx))),
          if (length(s$bad_idx) > 0)
            p(sprintf("Sebanyak %d item dibuat lemah hubungannya dengan trait. Bandingkan corrected item-total correlation dan alpha if deleted item-item tersebut dengan item lain.",
                      length(s$bad_idx))),
          p("Jangan memakai satu cutoff alpha secara mekanis. Interpretasi perlu mempertimbangkan tujuan pengukuran, keluasan construct, panjang tes, dan bukti isi.")),
      div(class = "insight", strong("Coba ini: "),
          "mulai dengan 8 item dan loading .60, lalu naikkan menjadi 24 item tanpa mengubah loading. Setelah itu kembalikan ke 12 item dan tambahkan reverse item tanpa recoding. Bandingkan alpha, omega, dan jarak antara keduanya.")
    )
  })
  
  rel_code <- reactive({
    s <- rel_data()
    sprintf('# Psychometrics Learning Lab — Reliability Explorer
library(lavaan)
set.seed(%d)

n <- %d; k <- %d; loading <- %.2f
reverse_n <- %d   # item yang arahnya dibalik dan sengaja TIDAK direcode
bad_n     <- %d   # item dengan hubungan lemah terhadap trait

trait    <- rnorm(n)
loadings <- rep(loading, k)
if (bad_n > 0) loadings[(k - bad_n + 1):k] <- runif(bad_n, .05, .25)

dat <- sapply(seq_len(k), function(j) {
  y <- loadings[j] * trait + sqrt(max(1 - loadings[j]^2, .05)) * rnorm(n)
  as.integer(cut(y, breaks = c(-Inf, -.85, -.20, .35, 1.05, Inf), labels = 1:5))
})
colnames(dat) <- sprintf("Item_%%02d", seq_len(k))
dat <- as.data.frame(dat)
if (reverse_n > 0) dat[, 1:reverse_n] <- 6 - dat[, 1:reverse_n]

# --- Cronbach alpha ---
cronbach_alpha <- function(x) {
  k <- ncol(x)
  k / (k - 1) * (1 - sum(sapply(x, var)) / var(rowSums(x)))
}
alpha <- cronbach_alpha(dat); alpha

# --- interval Feldt untuk alpha ---
df1 <- n - 1; df2 <- (n - 1) * (ncol(dat) - 1)
c(lo = 1 - (1 - alpha) * qf(.975, df1, df2),
  hi = 1 - (1 - alpha) * qf(.025, df1, df2))

# --- McDonald omega dari CFA satu faktor ---
mod <- paste0("F =~ ", paste(names(dat), collapse = " + "))
fit <- cfa(mod, data = dat, std.lv = TRUE)
est <- parameterEstimates(fit)
lam <- est$est[est$op == "=~"]
th  <- est$est[est$op == "~~" & est$lhs == est$rhs & est$lhs %%in%% names(dat)]
sum(lam)^2 / (sum(lam)^2 + sum(th))

# --- kurva panjang tes: item ditambah dalam urutan ACAK ---
# Menambahkan item sesuai urutan kolom akan menggambarkan komposisi item,
# bukan efek panjang tes, karena reverse item ada di kolom awal.
set.seed(1)
ks    <- 2:ncol(dat)
perms <- replicate(120, {
  ord <- sample(ncol(dat))
  sapply(ks, function(j) cronbach_alpha(dat[, ord[seq_len(j)], drop = FALSE]))
})
data.frame(items = ks, alpha = round(rowMeans(perms), 3))
', input$rel_seed, input$rel_n, input$rel_k, input$rel_loading,
    input$rel_reverse, input$rel_bad)
  })
  register_syntax("rel_syntax", "rel_syntax_dl", rel_code, "reliability_explorer.R")
  
  # ==========================================================
  # FACTOR ANALYSIS LAB
  # ==========================================================
  fa_data <- eventReactive(input$fa_run, ignoreNULL = FALSE, {
    cfg <- list(n = input$fa_n, nf = input$fa_nf, ipf = input$fa_ipf,
                loading = input$fa_loading, cross = input$fa_cross,
                fr = input$fa_fr, seed = input$fa_seed)
    z <- factor_sim(cfg$n, cfg$nf, cfg$ipf, cfg$loading, cfg$cross, cfg$fr, cfg$seed)
    z$config <- cfg
    z
  })
  
  fa_solution <- reactive({
    z <- fa_data(); d <- z$data; cfg <- z$config
    obs_ev <- eigen(stats::cor(d), symmetric = TRUE, only.values = TRUE)$values
    rnd <- parallel_random_eigen(nrow(d), ncol(d), reps = 80, seed = cfg$seed + 1)
    rotation <- if (cfg$nf > 1) "promax" else "none"
    fit <- tryCatch(stats::factanal(d, factors = cfg$nf, rotation = rotation, scores = "none"),
                    error = function(e) e)
    list(sim = z, obs_ev = obs_ev, rnd = rnd,
         fit = if (inherits(fit, "error")) NULL else fit,
         error = if (inherits(fit, "error")) conditionMessage(fit) else NULL,
         d = d, config = cfg, rotation = rotation)
  })
  
  output$fa_metrics <- renderUI({
    z <- fa_solution()
    retain <- parallel_retain(z$obs_ev, z$rnd)
    mean_comm <- if (!is.null(z$fit)) mean(1 - z$fit$uniquenesses) else NA_real_
    div(class = "metric-grid",
        metric_box("Faktor yang disimulasikan", z$config$nf),
        metric_box("Saran parallel analysis", retain),
        metric_box("Jumlah item", ncol(z$d)),
        metric_box("Rerata communality (hasil)", fmt_n(mean_comm)),
        metric_box("Rerata communality (bangkitan)", fmt_n(mean(z$sim$communality))),
        metric_box("Rotasi", if (z$rotation == "promax") "Promax (oblique)" else "Tanpa rotasi"))
  })
  
  output$fa_scree <- renderPlot({
    z <- fa_solution(); k <- seq_along(z$obs_ev)
    d <- rbind(data.frame(component = k, eigen = z$obs_ev, type = "Data simulasi"),
               data.frame(component = k, eigen = z$rnd, type = "Data acak, persentil 95"))
    ggplot(d, aes(component, eigen, linetype = type, colour = type)) +
      geom_line(linewidth = 1.05) + geom_point(size = 2) +
      scale_x_continuous(breaks = k) +
      scale_colour_manual(values = c("Data simulasi" = col_blue(),
                                     "Data acak, persentil 95" = col_accent())) +
      labs(x = "Nomor komponen / faktor", y = "Eigenvalue", linetype = NULL, colour = NULL,
           title = "Scree plot dan parallel analysis",
           caption = "Faktor dipertahankan selama eigenvalue data masih di atas garis data acak.") +
      pt()
  })
  
  fa_loading_table <- reactive({
    z <- fa_solution()
    req(!is.null(z$fit))
    L <- unclass(z$fit$loadings)
    gen <- z$sim$lambda
    colnames(gen) <- paste0("Bangkitan_F", seq_len(ncol(gen)))
    out <- data.frame(item = rownames(L), round(gen, 3), round(L, 3),
                      check.names = FALSE, stringsAsFactors = FALSE)
    names(out)[1] <- "Item"
    out
  })
  
  output$fa_loadings <- renderDT({
    datatable(fa_loading_table(), rownames = FALSE,
              options = list(pageLength = 14, scrollX = TRUE))
  })
  
  output$fa_loading_note <- renderUI({
    z <- fa_solution()
    if (is.null(z$fit)) {
      return(div(class = "status-bad", strong("factanal tidak dapat menemukan solusi. "),
                 z$error %||% "Coba ubah jumlah faktor atau jumlah item per faktor."))
    }
    tagList(
      stat_readout(
        sprintf("Varians tiap item pada data bangkitan = %.3f", mean(z$sim$implied_var)),
        paste0(
          "Kolom Bangkitan_F* adalah loading yang dipakai membangkitkan data; kolom Factor* ",
          "adalah hasil ekstraksi. Keduanya tidak harus sama persis. Rotasi oblique memulihkan ",
          "pola, bukan angka: makin besar cross-loading dan korelasi antar-faktor, makin jauh ",
          "pattern matrix hasil rotasi dari matriks pembangkitnya. Yang penting untuk dinilai ",
          "adalah apakah setiap item tetap memiliki satu loading dominan pada faktor yang benar.",
          if (isTRUE(z$sim$rescaled))
            " Catatan: pada setelan ini loading pembangkit diskalakan sedikit agar communality tidak melewati .90 dan varians item tetap 1."
          else ""
        ),
        "communality = diag(Lambda %*% Phi %*% t(Lambda)); uniqueness = 1 - communality"
      )
    )
  })
  
  output$fa_communal <- renderPlot({
    z <- fa_solution(); req(!is.null(z$fit))
    comm <- 1 - z$fit$uniquenesses
    d <- data.frame(item = names(comm), hasil = as.numeric(comm),
                    bangkitan = as.numeric(z$sim$communality))
    dl <- rbind(data.frame(item = d$item, value = d$bangkitan, jenis = "Bangkitan"),
                data.frame(item = d$item, value = d$hasil, jenis = "Hasil ekstraksi"))
    dl$item <- factor(dl$item, levels = rev(sort(unique(dl$item))))
    ggplot(dl, aes(item, value, fill = jenis)) +
      geom_col(position = position_dodge(width = .78), width = .72) +
      coord_flip() +
      scale_fill_manual(values = c("Bangkitan" = col_grey(), "Hasil ekstraksi" = col_blue())) +
      labs(x = NULL, y = "Communality", fill = NULL,
           title = "Varians item yang dijelaskan common factors") +
      pt()
  })
  
  output$fa_dl <- renderUI({
    dl_row(
      downloadButton("fa_data_dl", "Unduh data simulasi (CSV)", class = "btn-default"),
      downloadButton("fa_load_dl", "Unduh tabel loading (CSV)", class = "btn-default")
    )
  })
  register_data_dl("fa_data_dl", "factor_analysis_data.csv", function() fa_solution()$d)
  register_data_dl("fa_load_dl", "factor_loadings.csv", function() fa_loading_table())
  
  output$fa_interpret <- renderUI({
    z <- fa_solution(); cfg <- z$config
    retain <- parallel_retain(z$obs_ev, z$rnd)
    tagList(
      div(class = "panel-card", h4("Factor Analysis"),
          p("Factor Analysis mencoba menjelaskan korelasi antar-item menggunakan satu atau beberapa latent factor. ",
            "Item yang mengukur faktor yang sama diharapkan memiliki pola loading yang serupa."),
          p("Tujuan simulasi ini bukan mengejar satu angka fit, melainkan melihat bagaimana primary loading, cross-loading, ",
            "korelasi antar-faktor, dan jumlah indicator memengaruhi kejelasan struktur.")),
      div(class = "panel-card", h4("Membaca hasil saat ini"),
          p(sprintf("Data dibangkitkan dari %d faktor dengan primary loading %.2f, cross-loading %.2f, dan korelasi antar-faktor %.2f.",
                    cfg$nf, cfg$loading, cfg$cross, cfg$fr)),
          p(sprintf("Parallel analysis menyarankan %d faktor. Bandingkan dengan %d faktor yang benar-benar dipakai membangkitkan data.",
                    retain, cfg$nf)),
          if (retain != cfg$nf)
            p(strong("Perhatikan selisihnya. "),
              "Parallel analysis tidak selalu memulihkan jumlah faktor yang sebenarnya, terutama ketika loading rendah, ",
              "cross-loading besar, faktor sangat berkorelasi, atau indicator per faktor sedikit. ",
              "Ini bukan kegagalan metode, melainkan pengingat bahwa penentuan jumlah faktor selalu memerlukan pertimbangan isi."),
          p("Pada tabel Factor Loadings, simple structure terlihat lebih jelas ketika setiap item memiliki satu loading dominan dan cross-loading yang jauh lebih kecil."),
          p("Communality menunjukkan proporsi varians item yang dijelaskan common factors. Nilai rendah berarti sebagian besar varians item belum dijelaskan struktur yang diekstraksi.")),
      div(class = "insight", strong("Coba ini: "),
          "gunakan dua faktor dengan primary loading .75 dan cross-loading 0. Setelah itu naikkan cross-loading ke .35, lalu turunkan primary loading ke .35. Perhatikan scree plot, saran parallel analysis, dan seberapa jauh loading hasil ekstraksi bergeser dari loading bangkitannya.")
    )
  })
  
  fa_code <- reactive({
    cfg <- fa_solution()$config
    sprintf('# Psychometrics Learning Lab — Factor Analysis Lab
set.seed(%d)

n <- %d; n_factors <- %d; items_per_factor <- %d
loading <- %.2f; cross_loading <- %.2f; factor_r <- %.2f
p <- n_factors * items_per_factor

# Matriks korelasi antar-faktor
Phi <- matrix(factor_r, n_factors, n_factors); diag(Phi) <- 1

# Pola loading
Lambda <- matrix(0, p, n_factors)
for (f in seq_len(n_factors)) {
  idx <- ((f - 1) * items_per_factor + 1):(f * items_per_factor)
  Lambda[idx, f] <- loading
  if (n_factors > 1 && cross_loading > 0) {
    other <- if (f == n_factors) 1 else f + 1
    Lambda[idx, other] <- cross_loading
  }
}

# Communality untuk faktor yang BERKORELASI adalah diag(Lambda %%*%% Phi %%*%% t(Lambda)).
# Memakai rowSums(Lambda^2) hanya benar bila faktor orthogonal; bila tidak,
# varians item menjadi lebih besar dari 1 dan loading yang dipulihkan tidak
# lagi sebanding dengan angka yang disetel.
communality <- diag(Lambda %%*%% Phi %%*%% t(Lambda))
too_big     <- communality > .90
if (any(too_big)) {
  Lambda[too_big, ] <- Lambda[too_big, , drop = FALSE] * sqrt(.90 / communality[too_big])
  communality <- diag(Lambda %%*%% Phi %%*%% t(Lambda))
}
uniqueness <- 1 - communality

Fscores <- matrix(rnorm(n * n_factors), n, n_factors) %%*%% chol(Phi)
E       <- matrix(rnorm(n * p), n, p) %%*%% diag(sqrt(uniqueness), p)
X       <- as.data.frame(Fscores %%*%% t(Lambda) + E)
names(X) <- sprintf("Item_%%02d", seq_len(p))

round(apply(X, 2, var), 3)   # seharusnya mendekati 1

# --- Scree dan parallel analysis ---
obs <- eigen(cor(X), symmetric = TRUE, only.values = TRUE)$values
set.seed(%d)
rnd <- apply(replicate(80, {
  z <- matrix(rnorm(n * p), n, p)
  eigen(cor(z), symmetric = TRUE, only.values = TRUE)$values
}), 1, quantile, .95)
data.frame(komponen = seq_along(obs), data = round(obs, 3), acak = round(rnd, 3))

# --- Ekstraksi ---
fit <- factanal(X, factors = n_factors, rotation = %s)
print(fit$loadings, cutoff = 0)
round(1 - fit$uniquenesses, 3)   # communality hasil ekstraksi
', cfg$seed, cfg$n, cfg$nf, cfg$ipf, cfg$loading, cfg$cross, cfg$fr,
cfg$seed + 1, if (cfg$nf > 1) '"promax"' else '"none"')
  })
  register_syntax("fa_syntax", "fa_syntax_dl", fa_code, "factor_analysis_lab.R")
  
  # ==========================================================
  # DIF EXPLORER
  # ==========================================================
  dif_sim <- eventReactive(input$dif_run, ignoreNULL = FALSE, {
    simulate_dif_test(n_per_group = input$dif_n, n_anchor = input$dif_anchor,
                      a = input$dif_a, b = input$dif_b,
                      delta_a = input$dif_da, delta_b = input$dif_db,
                      focal_theta_mean = input$dif_mean, seed = input$dif_seed)
  })
  
  dif_method <- reactive(input$dif_match %||% "rest")
  
  dif_results <- reactive({
    z <- dif_sim(); m <- dif_method()
    mv <- dif_matching_variable(z$data, m)
    list(
      sim = z, method = m, match_value = mv,
      logistic = dif_logistic(z$data, m),
      mh = mantel_haenszel(z$data$studied, z$data$group, dif_strata(mv, m))
    )
  })
  
  dif_probs <- reactive({
    z <- dif_sim(); th <- input$dif_theta
    c(reference = stats::plogis(z$a_ref * (th - z$b_ref)),
      focal = stats::plogis(z$a_focal * (th - z$b_focal)))
  })
  
  method_label <- function(m) {
    switch(m, rest = "rest score", total = "skor total", theta = "true theta", m)
  }
  
  output$dif_metrics <- renderUI({
    r <- dif_results(); pr <- dif_probs()
    raw <- tapply(r$sim$data$studied, r$sim$data$group, mean)
    mh <- r$mh
    div(class = "metric-grid",
        metric_box("P Reference", sprintf("%.3f", pr["reference"]),
                   paste("pada θ =", input$dif_theta)),
        metric_box("P Focal", sprintf("%.3f", pr["focal"]),
                   paste("pada θ =", input$dif_theta)),
        metric_box("Selisih Focal − Reference", sprintf("%+.3f", pr["focal"] - pr["reference"])),
        metric_box("Delta MH", if (!is.null(mh)) sprintf("%+.2f", mh$delta_mh) else "NA",
                   if (!is.null(mh)) mh$ets else "tidak dapat dihitung"),
        metric_box("p group (logistic)", fmt_p(r$logistic$p_group),
                   paste("matching:", method_label(r$method))),
        metric_box("p interaksi (logistic)", fmt_p(r$logistic$p_inter),
                   "komponen non-uniform"),
        metric_box("Selisih proporsi mentah",
                   sprintf("%+.3f", raw[["Focal"]] - raw[["Reference"]]),
                   "tanpa pencocokan kemampuan"))
  })
  
  output$dif_icc <- renderPlot({
    z <- dif_sim(); th <- seq(-4, 4, length.out = 401)
    d <- rbind(
      data.frame(theta = th, p = stats::plogis(z$a_ref * (th - z$b_ref)), group = "Reference"),
      data.frame(theta = th, p = stats::plogis(z$a_focal * (th - z$b_focal)), group = "Focal")
    )
    d$group <- factor(d$group, levels = c("Reference", "Focal"))
    pr <- dif_probs()
    pts <- data.frame(theta = input$dif_theta, p = as.numeric(pr),
                      group = factor(c("Reference", "Focal"),
                                     levels = c("Reference", "Focal")))
    ggplot(d, aes(theta, p, linetype = group, colour = group)) +
      geom_line(linewidth = 1.1) +
      geom_vline(xintercept = input$dif_theta, linetype = 3, colour = col_grey()) +
      geom_point(data = pts, size = 2.9) +
      scale_colour_manual(values = c(Reference = col_blue(), Focal = col_accent())) +
      coord_cartesian(ylim = c(0, 1)) +
      labs(x = expression(theta), y = "P(correct)", linetype = NULL, colour = NULL,
           title = "Kurva item menurut group",
           caption = "Kurva yang berhimpit berarti item berfungsi sama pada level theta yang sama.") +
      pt()
  })
  
  output$dif_gap <- renderPlot({
    z <- dif_sim(); th <- seq(-4, 4, length.out = 401)
    d <- data.frame(theta = th,
                    gap = stats::plogis(z$a_focal * (th - z$b_focal)) -
                      stats::plogis(z$a_ref * (th - z$b_ref)))
    ggplot(d, aes(theta, gap)) +
      geom_hline(yintercept = 0, linetype = 2, colour = col_grey()) +
      geom_line(linewidth = 1.1, colour = col_blue()) +
      geom_vline(xintercept = input$dif_theta, linetype = 3, colour = col_grey()) +
      labs(x = expression(theta), y = "P(Focal) − P(Reference)",
           title = "Selisih probabilitas pada theta yang sama",
           caption = "Selisih yang konstan arahnya menunjukkan uniform DIF; yang berubah tanda menunjukkan komponen non-uniform.") +
      pt()
  })
  
  # Tampilan yang paling menyerupai analisis nyata: proporsi benar teramati
  # pada tiap tingkat skor pencocokan, bukan kurva teoretis.
  output$dif_empirical <- renderPlot({
    r <- dif_results()
    d <- r$sim$data
    d$bin <- dif_strata(r$match_value, r$method, n_bins = 8)
    agg <- stats::aggregate(studied ~ bin + group, data = d, FUN = mean)
    cnt <- stats::aggregate(studied ~ bin + group, data = d, FUN = length)
    names(cnt)[3] <- "n"
    agg <- merge(agg, cnt, by = c("bin", "group"))
    agg <- agg[agg$n >= 5, , drop = FALSE]
    validate(need(nrow(agg) > 0, "Belum cukup data pada tiap strata."))
    
    ggplot(agg, aes(bin, studied, colour = group, group = group, shape = group)) +
      geom_line(linewidth = 1) + geom_point(size = 2.8) +
      scale_colour_manual(values = c(Reference = col_blue(), Focal = col_accent())) +
      coord_cartesian(ylim = c(0, 1)) +
      labs(x = paste0("Strata ", method_label(r$method)), y = "Proporsi benar teramati",
           colour = NULL, shape = NULL,
           title = "Proporsi benar pada tiap tingkat kemampuan",
           caption = "Hanya strata dengan minimal 5 peserta per group yang ditampilkan. Inilah bentuk data yang tersedia pada analisis nyata.") +
      pt() +
      theme(axis.text.x = element_text(angle = 30, hjust = 1, size = rel(.8)))
  })
  
  output$dif_detect <- renderUI({
    r <- dif_results(); mh <- r$mh
    if (is.null(mh)) {
      return(div(class = "status-warn",
                 "Mantel-Haenszel tidak dapat dihitung: sebagian strata tidak memiliki variasi respons pada kedua group. Naikkan N per group atau kurangi jumlah strata."))
    }
    cls <- if (grepl("^A", mh$ets)) "status-ok" else if (grepl("^B", mh$ets)) "status-warn" else "status-bad"
    tagList(
      div(class = cls,
          strong(paste0("Klasifikasi ETS: ", mh$ets)), br(),
          sprintf("Delta MH = %+.2f, alpha MH = %.3f, chi-square MH = %.2f, %s, dari %d strata.",
                  mh$delta_mh, mh$alpha_mh, mh$chisq, fmt_p(mh$p), mh$n_strata),
          br(),
          sprintf("Matching memakai %s.", method_label(r$method))),
      stat_readout(
        "Membaca dua pendekatan berdampingan",
        paste0(
          "Regresi logistik menguji apakah group menambah penjelasan setelah kemampuan diperhitungkan ",
          "(terutama komponen uniform), dan apakah kemiringannya berbeda (komponen non-uniform). ",
          "Mantel-Haenszel memberi ukuran besarnya efek: Delta MH negatif berarti item lebih menguntungkan ",
          "Reference group. Klasifikasi A/B/C dipakai justru karena p-value ikut mengecil hanya karena N bertambah, ",
          "sementara Delta MH tidak."
        ),
        "Delta MH = -2.35 * ln(alpha MH); A jika |Delta MH| < 1 atau tidak signifikan, C jika >= 1.5 dan signifikan"
      ),
      div(class = "small-muted", style = "margin-top:8px;",
          "Catatan: aturan ETS lengkap juga mensyaratkan Delta MH berbeda signifikan dari 1,0 untuk kategori C. ",
          "Versi di sini memakai bentuk ringkas yang lazim dipakai pada bahan ajar.")
    )
  })
  
  output$dif_coef <- renderDT({
    d <- dif_results()$logistic$table
    d$estimate <- round(d$estimate, 3); d$se <- round(d$se, 3)
    d$z <- round(d$z, 3); d$p <- signif(d$p, 3)
    names(d) <- c("Term", "Estimate", "SE", "z", "p")
    datatable(d, rownames = FALSE, options = list(dom = "t", scrollX = TRUE))
  })
  
  output$dif_dl <- renderUI({
    dl_row(downloadButton("dif_data_dl", "Unduh data respons (CSV)", class = "btn-default"))
  })
  register_data_dl("dif_data_dl", "dif_responses.csv", function() {
    z <- dif_sim()
    cbind(z$data, z$anchor)
  })
  
  output$dif_interpret <- renderUI({
    r <- dif_results(); z <- r$sim; pr <- dif_probs(); cfg <- z$config
    raw <- tapply(z$data$studied, z$data$group, mean)
    mh <- r$mh
    
    type <- if (abs(cfg$delta_b) < 1e-8 && abs(cfg$delta_a) < 1e-8) {
      "tidak ada DIF yang sengaja dibangkitkan"
    } else if (abs(cfg$delta_a) < 1e-8) {
      "uniform DIF"
    } else if (abs(cfg$delta_b) < 1e-8) {
      "non-uniform DIF"
    } else {
      "kombinasi uniform dan non-uniform DIF"
    }
    
    # Bandingkan ketiga matching variable pada data yang sama.
    comp <- do.call(rbind, lapply(c("theta", "total", "rest"), function(m) {
      lg <- dif_logistic(z$data, m)
      mhx <- mantel_haenszel(z$data$studied, z$data$group,
                             dif_strata(dif_matching_variable(z$data, m), m))
      data.frame(metode = method_label(m), p_group = fmt_p(lg$p_group),
                 delta_mh = if (!is.null(mhx)) sprintf("%+.2f", mhx$delta_mh) else "NA",
                 ets = if (!is.null(mhx)) mhx$ets else "NA", stringsAsFactors = FALSE)
    }))
    
    tagList(
      div(class = "panel-card", h4("Differential Item Functioning"),
          p("DIF terjadi ketika item berfungsi berbeda antar-group setelah peserta dibandingkan pada level kemampuan yang sama. ",
            "Pertanyaannya bukan apakah rerata skor kedua group berbeda, melainkan apakah probabilitas menjawab benar tetap sama ketika kemampuannya disamakan."),
          p("Reference dan Focal hanya label untuk dua group yang dibandingkan. Label Focal tidak menunjukkan bahwa group tersebut lebih rendah atau bermasalah.")),
      div(class = "panel-card", h4("Membaca konfigurasi saat ini"),
          p(sprintf("Simulasi membangkitkan %s. Pada Reference a = %.2f dan b = %.2f; pada Focal a = %.2f dan b = %.2f.",
                    type, z$a_ref, z$b_ref, z$a_focal, z$b_focal)),
          p(sprintf("Pada θ = %.2f, P(correct) Reference = %.3f dan Focal = %.3f, sehingga selisihnya %+.3f.",
                    input$dif_theta, pr["reference"], pr["focal"], pr["focal"] - pr["reference"])),
          if (!is.null(mh))
            p(sprintf("Mantel-Haenszel dengan matching %s memberi Delta MH = %+.2f, yang masuk kategori %s.",
                      method_label(r$method), mh$delta_mh, mh$ets)),
          p(sprintf("Selisih proporsi benar tanpa pencocokan = %+.3f. Angka ini tidak boleh disamakan dengan DIF: ia dapat muncul hanya karena distribusi kemampuan kedua group berbeda.",
                    raw[["Focal"]] - raw[["Reference"]])),
          if (abs(cfg$focal_theta_mean) > 1e-8)
            p(sprintf("Pada simulasi ini mean theta Focal memang digeser %+.2f. Itu latent mean difference, bukan DIF.",
                      cfg$focal_theta_mean))),
      div(class = "panel-card", h4("Pengaruh pilihan matching variable"),
          p("Data yang sama, tiga cara mencocokkan kemampuan:"),
          simple_table(comp),
          p(strong("Mengapa berbeda. "),
            "True theta hanya tersedia dalam simulasi dan memberi hasil paling bersih. Skor total tersedia dalam analisis nyata, ",
            "tetapi item yang sedang diperiksa ikut menyumbang ke skor itu, sehingga sebagian DIF tersembunyi ke dalam kriteria pencocokannya sendiri. ",
            "Rest score membuang item tersebut dari skor pembanding — versi paling sederhana dari purification, dan biasanya paling mendekati true theta.")),
      div(class = "insight", strong("Urutan belajar yang disarankan: "),
          "(1) set Δb = 0 dan Δa = 0; (2) ubah hanya Δb untuk melihat uniform DIF; (3) kembalikan Δb ke 0 lalu ubah Δa untuk melihat non-uniform DIF; ",
          "(4) kembalikan keduanya ke 0 lalu geser hanya mean kemampuan Focal — perhatikan selisih proporsi mentah membesar sementara Delta MH tetap di kategori A.")
    )
  })
  
  dif_code <- reactive({
    cfg <- dif_sim()$config; m <- dif_method()
    sprintf('# Psychometrics Learning Lab — DIF Explorer
set.seed(%d)

n_per_group <- %d
n_anchor    <- %d
a <- %.2f; b <- %.2f                 # item yang dipelajari, Reference
a_focal <- %.2f; b_focal <- %.2f     # item yang sama, Focal

theta <- c(rnorm(n_per_group, 0, 1), rnorm(n_per_group, %.2f, 1))
group <- factor(rep(c("Reference", "Focal"), each = n_per_group),
                levels = c("Reference", "Focal"))

# Item jangkar: berfungsi identik pada kedua group
anchor_b <- seq(-1.6, 1.6, length.out = n_anchor)
anchor   <- sapply(anchor_b, function(bb) rbinom(length(theta), 1, plogis(theta - bb)))

# Item yang dipelajari
p_studied <- ifelse(group == "Reference",
                    plogis(a * (theta - b)),
                    plogis(a_focal * (theta - b_focal)))
studied <- rbinom(length(theta), 1, p_studied)

rest  <- rowSums(anchor)          # skor tanpa item yang diuji (purified)
total <- rest + studied

# --- Pilih matching variable ---
match_var <- %s

# --- Regresi logistik ---
fit <- glm(studied ~ match_var * group, family = binomial())
summary(fit)$coefficients

# --- Mantel-Haenszel ---
br     <- unique(quantile(match_var, seq(0, 1, length.out = 11), type = 1))
br[1]  <- -Inf; br[length(br)] <- Inf
strata <- cut(match_var, breaks = br, include.lowest = TRUE)

num <- den <- sumA <- sumE <- sumV <- 0
for (s in levels(strata)) {
  k <- strata == s
  N <- sum(k); if (N < 2) next
  nR <- sum(k & group == "Reference"); nF <- N - nR
  n1 <- sum(studied[k] == 1);          n0 <- N - n1
  if (nR == 0 || nF == 0 || n1 == 0 || n0 == 0) next
  A <- sum(k & studied == 1 & group == "Reference"); B <- nR - A
  C <- sum(k & studied == 1 & group == "Focal");     D <- nF - C
  num  <- num + A * D / N
  den  <- den + B * C / N
  sumA <- sumA + A
  sumE <- sumE + nR * n1 / N
  sumV <- sumV + (nR * nF * n1 * n0) / (N^2 * (N - 1))
}
alpha_mh <- num / den
delta_mh <- -2.35 * log(alpha_mh)          # negatif = menguntungkan Reference
chisq_mh <- (abs(sumA - sumE) - 0.5)^2 / sumV
c(alpha_MH = alpha_mh, Delta_MH = delta_mh,
  chisq = chisq_mh, p = pchisq(chisq_mh, 1, lower.tail = FALSE))
', cfg$seed, cfg$n_per_group, cfg$n_anchor, cfg$a, cfg$b,
    clamp(cfg$a + cfg$delta_a, .20, 3.50), cfg$b + cfg$delta_b,
    cfg$focal_theta_mean,
    switch(m, theta = "theta   # hanya tersedia pada simulasi",
           total = "total   # termasuk item yang diuji",
           "rest    # skor total tanpa item yang diuji"))
  })
  register_syntax("dif_syntax", "dif_syntax_dl", dif_code, "dif_explorer.R")
  
  # ==========================================================
  # MEASUREMENT INVARIANCE LAB
  # ==========================================================
  # MI-01: pada lavaan indicator pertama menjadi marker variable dan
  # loading-nya di-fix ke 1 pada kedua group. Karena itu item target
  # dapat dipilih, dan default-nya sengaja bukan i1.
  output$mi_item_selector <- renderUI({
    k <- input$mi_k %||% 6
    choices <- stats::setNames(seq_len(k), paste0("i", seq_len(k),
                                                  ifelse(seq_len(k) == 1, " (marker)", "")))
    tagList(
      selectInput("mi_item", "Item yang dibuat berbeda pada Group B",
                  choices = choices, selected = min(3, k)),
      uiOutput("mi_marker_warning")
    )
  })
  
  output$mi_marker_warning <- renderUI({
    if (!identical(as.integer(input$mi_item %||% 3), 1L)) return(NULL)
    div(class = "status-warn",
        strong("Anda memilih marker variable. "),
        "Loading i1 di-fix ke 1 pada kedua group, sehingga perbedaan yang Anda buat ",
        "tidak dapat muncul sebagai loading yang berbeda: ia terserap ke skala faktor ",
        "dan menyebar ke item lain. Fit tetap memburuk, tetapi lebih kecil dari seharusnya, ",
        "dan modification index akan menunjuk item yang salah. Bandingkan sendiri dengan i3 — ",
        "itu justru pelajaran yang bagus tentang identifikasi model.")
  })
  
  output$mi_partial_selector <- renderUI({
    k <- input$mi_k %||% 6
    opts <- invariance_partial_options(k)
    labels <- c(paste0("Loading ", paste0("i", seq_len(k))),
                paste0("Intercept ", paste0("i", seq_len(k))))
    tagList(
      selectizeInput("mi_partial", "Bebaskan parameter berikut antar-group",
                     choices = stats::setNames(opts, labels), multiple = TRUE,
                     options = list(placeholder = "Kosongkan untuk invariance penuh")),
      div(class = "small-muted",
          "Ketika scalar invariance gagal, langkah nyata seorang peneliti bukan berhenti, ",
          "melainkan membebaskan parameter yang bermasalah lalu memeriksa apakah fit pulih. ",
          "Lihat tab 'Item mana yang bermasalah?' untuk memilih kandidatnya.")
    )
  })
  
  mi_results <- eventReactive(input$mi_run, ignoreNULL = FALSE, {
    sim <- simulate_mi_data(
      n_per_group = input$mi_n %||% 450,
      n_items = input$mi_k %||% 6,
      loading = input$mi_loading %||% .70,
      target_item = as.integer(input$mi_item %||% 3),
      delta_loading = input$mi_dload %||% 0,
      delta_intercept = input$mi_dint %||% 0,
      residual_ratio = input$mi_resid %||% 1,
      latent_mean_diff = input$mi_latent_mean %||% .4,
      seed = input$mi_seed %||% 808
    )
    fits <- fit_invariance_sequence(sim, partial = input$mi_partial %||% character(0))
    list(sim = sim, fits = fits)
  })
  
  output$mi_metrics <- renderUI({
    z <- mi_results(); t <- z$fits$table
    div(class = "metric-grid",
        metric_box("Configural CFI", fmt_n(t$cfi[1])),
        metric_box("Metric ΔCFI", if (is.finite(t$delta_cfi[2])) sprintf("%+.4f", t$delta_cfi[2]) else "NA",
                   if (is.finite(t$lrt_p[2])) paste("chi-sq diff", fmt_p(t$lrt_p[2])) else NULL),
        metric_box("Scalar ΔCFI", if (is.finite(t$delta_cfi[3])) sprintf("%+.4f", t$delta_cfi[3]) else "NA",
                   if (is.finite(t$lrt_p[3])) paste("chi-sq diff", fmt_p(t$lrt_p[3])) else NULL),
        metric_box("Strict ΔCFI", if (is.finite(t$delta_cfi[4])) sprintf("%+.4f", t$delta_cfi[4]) else "NA",
                   if (is.finite(t$lrt_p[4])) paste("chi-sq diff", fmt_p(t$lrt_p[4])) else NULL),
        metric_box("Item target", paste0("i", z$sim$target_item),
                   if (z$sim$target_item == 1) "marker variable" else NULL),
        metric_box("Parameter dibebaskan",
                   if (length(z$fits$partial)) length(z$fits$partial) else 0,
                   if (length(z$fits$partial)) paste(z$fits$partial, collapse = ", ") else "invariance penuh"))
  })
  
  output$mi_fit_plot <- renderPlot({
    t <- mi_results()$fits$table
    d <- rbind(data.frame(model = t$model, value = t$cfi, metric = "CFI"),
               data.frame(model = t$model, value = t$rmsea, metric = "RMSEA"),
               data.frame(model = t$model, value = t$srmr, metric = "SRMR"))
    d$model <- factor(d$model, levels = c("Configural", "Metric", "Scalar", "Strict"))
    ggplot(d, aes(model, value, group = 1)) +
      geom_line(linewidth = 1.05, colour = col_blue()) +
      geom_point(size = 2.7, colour = col_ink()) +
      facet_wrap(~metric, scales = "free_y", ncol = 1) +
      labs(x = NULL, y = NULL, title = "Perubahan fit ketika constraint ditambahkan",
           caption = "Fokusnya bukan nilai absolut, melainkan seberapa jauh fit berubah setelah tiap tahap constraint.") +
      pt()
  })
  
  output$mi_table <- renderDT({
    t <- mi_results()$fits$table
    num <- setdiff(names(t), "model")
    t[num] <- lapply(t[num], function(x) round(x, 4))
    names(t) <- c("Model", "CFI", "RMSEA", "SRMR", "Chi-square", "df",
                  "ΔCFI", "ΔRMSEA", "ΔSRMR", "Chi-sq diff", "df diff", "p diff")
    datatable(t, rownames = FALSE, options = list(dom = "t", scrollX = TRUE))
  })
  
  output$mi_lrt_note <- renderUI({
    t <- mi_results()$fits$table
    stages <- c("Metric vs Configural", "Scalar vs Metric", "Strict vs Scalar")
    lines <- vapply(2:4, function(i) {
      if (!is.finite(t$lrt_p[i])) return(paste0(stages[i - 1], ": tidak dapat dihitung."))
      sprintf("%s: chi-square difference = %.2f dengan df = %d, %s.",
              stages[i - 1], t$lrt_chisq[i], as.integer(t$lrt_df[i]), fmt_p(t$lrt_p[i]))
    }, character(1))
    stat_readout(
      "Uji chi-square difference antar model nested",
      paste0(paste(lines, collapse = " "),
             " Uji ini sensitif terhadap ukuran sampel: pada N besar, pelanggaran kecil pun bisa signifikan. ",
             "Karena itu bacalah bersama ΔCFI dan besar pelanggaran yang sebenarnya, bukan sebagai penentu tunggal."),
      "lavTestLRT(fit_sebelumnya, fit_berikutnya)"
    )
  })
  
  output$mi_score_intro <- renderUI({
    z <- mi_results()
    tagList(
      div(class = "help-note",
          "Tabel ini menjawab pertanyaan yang paling praktis: kalau invariance ditolak, ",
          "parameter mana yang menyebabkannya? Angka X² besar menunjukkan constraint equality ",
          "yang paling menekan model. Bandingkan dengan item yang sengaja Anda buat berbeda."),
      if (z$sim$target_item == 1)
        div(class = "status-warn",
            "Karena item target adalah marker variable, jangan kaget bila daftar di bawah ",
            "tidak menunjuk i1: perbedaannya memang tidak dapat muncul di sana.")
      else NULL
    )
  })
  
  output$mi_score <- renderDT({
    z <- mi_results()
    fit <- z$fits$fits$Scalar %||% z$fits$fits$Metric
    st <- invariance_score_table(fit, top = 10)
    validate(need(!is.null(st), "Modification index tidak tersedia untuk model ini."))
    st$X2 <- round(st$X2, 2)
    st$p.value <- signif(st$p.value, 3)
    names(st) <- c("Parameter", "Constraint", "X²", "df", "p")
    datatable(st, rownames = FALSE, options = list(dom = "t", scrollX = TRUE))
  })
  
  output$mi_item_plot <- renderPlot({
    s <- mi_results()$sim
    j <- s$target_item
    th <- seq(-3, 3, length.out = 201)
    d <- rbind(
      data.frame(theta = th, expected = s$int_a[j] + s$lam_a[j] * th, group = "Group A"),
      data.frame(theta = th, expected = s$int_b[j] + s$lam_b[j] * th, group = "Group B")
    )
    ggplot(d, aes(theta, expected, linetype = group, colour = group)) +
      geom_line(linewidth = 1.1) +
      scale_colour_manual(values = c("Group A" = col_blue(), "Group B" = col_accent())) +
      labs(x = "Latent factor", y = paste0("Expected skor i", j), linetype = NULL, colour = NULL,
           title = paste0("Hubungan pembangkit untuk item i", j),
           caption = "Kemiringan berbeda = pelanggaran metric; posisi vertikal berbeda = pelanggaran scalar.") +
      pt()
  })
  
  mi_param_table <- reactive({
    s <- mi_results()$sim; j <- s$target_item
    data.frame(
      Parameter = c(paste0("Loading i", j), paste0("Intercept i", j),
                    paste0("Residual SD i", j), "Latent mean"),
      Group_A = round(c(s$lam_a[j], s$int_a[j], s$resid_a[j], 0), 3),
      Group_B = round(c(s$lam_b[j], s$int_b[j], s$resid_b[j], s$config$latent_mean_diff), 3),
      stringsAsFactors = FALSE
    )
  })
  
  output$mi_params <- renderDT({
    datatable(mi_param_table(), rownames = FALSE, options = list(dom = "t"))
  })
  
  output$mi_dl <- renderUI({
    dl_row(downloadButton("mi_data_dl", "Unduh data simulasi (CSV)", class = "btn-default"))
  })
  register_data_dl("mi_data_dl", "measurement_invariance_data.csv",
                   function() mi_results()$sim$data)
  
  output$mi_interpret <- renderUI({
    z <- mi_results(); s <- z$sim; t <- z$fits$table; j <- s$target_item
    
    stage_sentence <- function(stage, i, focus) {
      d <- function(col) if (is.finite(t[[col]][i])) sprintf("%+.4f", t[[col]][i]) else "NA"
      lrt <- if (is.finite(t$lrt_p[i]))
        sprintf(" Uji chi-square difference: %s.", fmt_p(t$lrt_p[i])) else ""
      sprintf("%s: ketika %s dipaksa sama, ΔCFI %s, ΔRMSEA %s, ΔSRMR %s.%s",
              stage, focus, d("delta_cfi"), d("delta_rmsea"), d("delta_srmr"), lrt)
    }
    
    tagList(
      div(class = "panel-card", h4("Measurement Invariance"),
          p("Measurement Invariance menilai apakah measurement model cukup setara antar-group. ",
            "Bila parameter pengukuran berbeda secara substantif, perbandingan skor atau latent mean akan mencampurkan ",
            "perbedaan construct yang sesungguhnya dengan perbedaan cara skala bekerja."),
          p("Invariance tidak mensyaratkan skor atau latent mean yang sama. Group boleh berbeda pada construct-nya; ",
            "yang diuji adalah apakah parameter pengukurannya cukup comparable sehingga perbedaan itu dapat diinterpretasikan.")),
      div(class = "panel-card", h4("Membaca simulasi saat ini"),
          p(sprintf("Pada item i%d, Group B dibangkitkan dengan selisih loading %+.2f, selisih intercept %+.2f, dan rasio residual SD %.2f.",
                    j, s$config$delta_loading, s$config$delta_intercept, s$config$residual_ratio)),
          p(stage_sentence("Metric vs Configural", 2, "loading")),
          p(stage_sentence("Scalar vs Metric", 3, "loading dan intercept")),
          p(stage_sentence("Strict vs Scalar", 4, "loading, intercept, dan residual variance")),
          p(sprintf("Latent mean Group B = %+.2f. Ini perbedaan yang sengaja dibuat dan bukan pelanggaran invariance.",
                    s$config$latent_mean_diff)),
          if (length(z$fits$partial))
            p(strong("Partial invariance aktif. "),
              sprintf("Parameter yang dibebaskan: %s. Bandingkan ΔCFI sekarang dengan ketika seluruh parameter dibatasi sama.",
                      paste(z$fits$partial, collapse = ", "))),
          if (j == 1)
            p(strong("Item target adalah marker variable. "),
              "Loading i1 di-fix ke 1 pada kedua group, sehingga sebagian pelanggaran tidak dapat terlihat sebagaimana mestinya. ",
              "Pilih item lain untuk melihat efek yang utuh.")),
      div(class = "insight", strong("Urutan belajar yang disarankan: "),
          "mulai dengan ketiga selisih netral. Ubah hanya loading dan lihat tahap Metric; kembalikan lalu ubah hanya intercept dan lihat Scalar; ",
          "kembalikan lalu ubah hanya rasio residual dan lihat Strict. Setelah itu buka tab 'Item mana yang bermasalah?', ",
          "bebaskan parameter yang ditunjuk, dan perhatikan fit-nya pulih.")
    )
  })
  
  mi_code <- reactive({
    cfg <- mi_results()$sim$config
    partial <- mi_results()$fits$partial
    partial_txt <- if (length(partial))
      paste0('group.partial = c(', paste(sprintf('"%s"', partial), collapse = ", "), '),\n              ')
    else ""
    sprintf('# Psychometrics Learning Lab — Measurement Invariance Lab
library(lavaan)
set.seed(%d)

n <- %d; k <- %d; loading <- %.2f
target <- %d          # item yang sengaja dibuat berbeda pada Group B
# Catatan: indicator PERTAMA menjadi marker variable pada lavaan dan
# loading-nya di-fix ke 1 di kedua group. Bila target = 1, perbedaannya
# terserap ke skala faktor dan modification index akan menunjuk item lain.

lam_a <- rep(loading, k); lam_b <- lam_a
lam_b[target] <- pmin(pmax(loading + (%.2f), .15), .95)
int_a <- rep(0, k);       int_b <- int_a; int_b[target] <- %.2f
res_a <- sqrt(pmax(1 - lam_a^2, .10))
res_b <- sqrt(pmax(1 - lam_b^2, .10)); res_b[target] <- res_b[target] * %.2f

eta_a <- rnorm(n, 0, 1)
eta_b <- rnorm(n, %.2f, 1)     # latent mean difference: BUKAN non-invariance
ya <- sapply(1:k, function(j) int_a[j] + lam_a[j]*eta_a + rnorm(n, 0, res_a[j]))
yb <- sapply(1:k, function(j) int_b[j] + lam_b[j]*eta_b + rnorm(n, 0, res_b[j]))
colnames(ya) <- colnames(yb) <- paste0("i", 1:k)

dat <- rbind(data.frame(group = "Group A", ya), data.frame(group = "Group B", yb))
dat$group <- factor(dat$group, levels = c("Group A", "Group B"))

mod <- paste0("F =~ ", paste(paste0("i", 1:k), collapse = " + "))
cfa_at <- function(eq = NULL) {
  cfa(mod, data = dat, group = "group", meanstructure = TRUE,
      %sgroup.equal = eq)
}

fit_conf   <- cfa_at(NULL)
fit_metric <- cfa_at("loadings")
fit_scalar <- cfa_at(c("loadings", "intercepts"))
fit_strict <- cfa_at(c("loadings", "intercepts", "residuals"))

# Perubahan fit
sapply(list(Configural = fit_conf, Metric = fit_metric,
            Scalar = fit_scalar, Strict = fit_strict),
       function(f) fitMeasures(f, c("cfi", "rmsea", "srmr")))

# Uji chi-square difference antar model nested
lavTestLRT(fit_conf, fit_metric)
lavTestLRT(fit_metric, fit_scalar)
lavTestLRT(fit_scalar, fit_strict)

# Constraint mana yang paling menekan model?
lavTestScore(fit_scalar)$uni

# Partial invariance: bebaskan parameter yang bermasalah lalu periksa ulang
# cfa(mod, data = dat, group = "group", meanstructure = TRUE,
#     group.equal = c("loadings", "intercepts"),
#     group.partial = c("i%d~1"))
', cfg$seed, cfg$n_per_group, cfg$n_items, cfg$loading, cfg$target_item,
    cfg$delta_loading, cfg$delta_intercept, cfg$residual_ratio,
    cfg$latent_mean_diff, partial_txt, cfg$target_item)
  })
  register_syntax("mi_syntax", "mi_syntax_dl", mi_code, "measurement_invariance.R")
  
  # ==========================================================
  # VALIDITY LAB
  # ==========================================================
  val_data <- eventReactive(input$val_run, ignoreNULL = FALSE, {
    simulate_validity(n = input$val_n %||% 1200, rho_true = input$val_rho %||% .50,
                      rel_x = input$val_relx %||% .80, rel_y = input$val_rely %||% .70,
                      select_top_pct = input$val_select %||% 100,
                      seed = input$val_seed %||% 2026)
  })
  
  output$val_metrics <- renderUI({
    z <- val_data()
    div(class = "metric-grid",
        metric_box("Korelasi sebenarnya (ρ)", sprintf("%.3f", z$rho_true), "antar true score"),
        metric_box("Batas atas r teramati", sprintf("%.3f", z$max_possible),
                   "sqrt(rel_x × rel_y)"),
        metric_box("r teramati (semua pelamar)", sprintf("%.3f", z$r_observed)),
        metric_box("r setelah seleksi", fmt_n(z$r_selected),
                   sprintf("n = %d dari %d", z$n_selected, z$config$n)),
        metric_box("Koreksi attenuation", fmt_n(z$r_disattenuated)),
        metric_box("Koreksi range restriction", fmt_n(z$r_range_corrected)),
        metric_box("Kedua koreksi", fmt_n(z$r_both_corrected)))
  })
  
  output$val_atten <- renderPlot({
    z <- val_data()
    rel <- seq(.20, 1, length.out = 200)
    d <- rbind(
      data.frame(rel = rel, r = z$rho_true * sqrt(rel * z$config$rel_y),
                 jenis = sprintf("Reliability kriteria tetap %.2f", z$config$rel_y)),
      data.frame(rel = rel, r = z$rho_true * sqrt(rel * 1),
                 jenis = "Kriteria sempurna (rel = 1)")
    )
    pt <- data.frame(rel = z$config$rel_x, r = z$rho_true * z$max_possible)
    ggplot(d, aes(rel, r, colour = jenis, linetype = jenis)) +
      geom_hline(yintercept = z$rho_true, linetype = 2, colour = col_grey()) +
      geom_line(linewidth = 1.05) +
      geom_point(data = pt, inherit.aes = FALSE, aes(rel, r), size = 3.2, colour = col_ink()) +
      scale_colour_manual(values = stats::setNames(
        c(col_blue(), col_accent()),
        c(sprintf("Reliability kriteria tetap %.2f", z$config$rel_y),
          "Kriteria sempurna (rel = 1)"))) +
      coord_cartesian(ylim = c(0, 1)) +
      labs(x = "Reliability prediktor (X)", y = "Korelasi teramati dengan kriteria",
           colour = NULL, linetype = NULL,
           title = "Attenuation: error pengukuran menekan korelasi",
           caption = "Garis putus-putus mendatar = korelasi sebenarnya. Titik = posisi setelan Anda sekarang.") +
      pt()
  })
  
  output$val_atten_note <- renderUI({
    z <- val_data()
    stat_readout(
      sprintf("Dengan rel_x = %.2f dan rel_y = %.2f, korelasi teramati tidak dapat melebihi %.3f",
              z$config$rel_x, z$config$rel_y, z$max_possible),
      paste0(
        sprintf("Korelasi sebenarnya %.2f muncul sebagai %.3f pada data teramati. ",
                z$rho_true, z$r_observed),
        "Inilah alasan reliability disebut membatasi validity: seberapa pun kuat hubungan yang sesungguhnya, ",
        "error pengukuran pada kedua sisi menekan korelasi ke arah nol. ",
        "Perhatikan juga bahwa menaikkan reliability prediktor memberi hasil yang makin kecil ketika kriteria tetap tidak reliabel — ",
        "memperbaiki alat ukur kriteria sering lebih berdampak daripada terus memoles prediktor."
      ),
      "r_xy teramati = rho * sqrt(rel_x * rel_y)"
    )
  })
  
  output$val_scatter <- renderPlot({
    z <- val_data(); d <- z$data
    d$status <- ifelse(d$selected, "Diterima", "Tidak diterima")
    ggplot(d, aes(x, y, colour = status)) +
      geom_point(alpha = .35, size = 1.5) +
      geom_smooth(data = d[d$selected, ], method = "lm", formula = y ~ x, se = FALSE,
                  colour = col_accent(), linewidth = 1) +
      geom_smooth(method = "lm", formula = y ~ x, se = FALSE,
                  colour = col_blue(), linewidth = 1, linetype = 2) +
      scale_colour_manual(values = c("Diterima" = col_accent(), "Tidak diterima" = col_grey())) +
      labs(x = "Skor prediktor (X)", y = "Skor kriteria (Y)", colour = NULL,
           title = sprintf("Range restriction: hanya %d%% teratas yang terlihat kriterianya",
                           z$config$select_top_pct),
           caption = "Garis putus-putus biru = seluruh pelamar; garis merah = hanya yang diterima. Kemiringan yang lebih landai adalah range restriction.") +
      pt()
  })
  
  val_table <- reactive({
    z <- val_data()
    data.frame(
      Ukuran = c("Korelasi true score (ρ)",
                 "Batas atas karena reliability",
                 "r teramati, semua pelamar",
                 "r teramati, hanya yang diterima",
                 "Dikoreksi attenuation",
                 "Dikoreksi range restriction",
                 "Dikoreksi keduanya"),
      Nilai = round(c(z$rho_true, z$max_possible, z$r_observed, z$r_selected,
                      z$r_disattenuated, z$r_range_corrected, z$r_both_corrected), 3),
      Keterangan = c(
        "Hubungan sebenarnya antar konstruk; hanya diketahui karena ini simulasi",
        "sqrt(rel_x * rel_y): tidak mungkin dilampaui r teramati",
        "Yang akan Anda hitung bila seluruh pelamar diukur kriterianya",
        "Yang biasanya benar-benar tersedia pada studi validasi seleksi",
        "r_obs / sqrt(rel_x * rel_y)",
        "Koreksi Thorndike Case II memakai rasio SD prediktor",
        "Range restriction dikoreksi lebih dulu, baru attenuation"
      ),
      stringsAsFactors = FALSE
    )
  })
  
  output$val_table <- renderDT({
    datatable(val_table(), rownames = FALSE, options = list(dom = "t", scrollX = TRUE))
  })
  
  output$val_dl <- renderUI({
    dl_row(downloadButton("val_data_dl", "Unduh data simulasi (CSV)", class = "btn-default"))
  })
  register_data_dl("val_data_dl", "validity_data.csv", function() val_data()$data)
  
  output$val_interpret <- renderUI({
    z <- val_data()
    restricted <- z$config$select_top_pct < 100
    tagList(
      div(class = "panel-card", h4("Reliability membatasi validity"),
          p("Korelasi antara dua skor teramati tidak dapat melebihi akar dari hasil kali reliability keduanya. ",
            "Karena itu, tes dengan reliability rendah tidak akan menunjukkan hubungan yang kuat dengan kriteria apa pun — ",
            "bukan karena hubungannya tidak ada, melainkan karena alat ukurnya terlalu berisik untuk memperlihatkannya."),
          p("Konsekuensi praktisnya sering terbalik dari dugaan: ketika kriteria sendiri tidak reliabel, ",
            "menyempurnakan prediktor memberi hasil yang makin sedikit. Memperbaiki pengukuran kriteria biasanya lebih berdampak.")),
      div(class = "panel-card", h4("Membaca simulasi saat ini"),
          p(sprintf("Korelasi sebenarnya %.2f, reliability prediktor %.2f, reliability kriteria %.2f. Batas atas r teramati = %.3f, dan yang terhitung %.3f.",
                    z$rho_true, z$config$rel_x, z$config$rel_y, z$max_possible, z$r_observed)),
          p(sprintf("Koreksi attenuation mengembalikan estimasi ke %.3f, yang mendekati %.2f sebagaimana seharusnya.",
                    z$r_disattenuated, z$rho_true)),
          if (restricted)
            p(sprintf("Dengan seleksi %d%% teratas, SD prediktor menyusut dari %.2f menjadi %.2f, dan korelasi turun dari %.3f ke %.3f. Koreksi Thorndike mengembalikannya ke %.3f, lalu koreksi attenuation ke %.3f.",
                      z$config$select_top_pct, z$sd_full, z$sd_selected, z$r_observed,
                      z$r_selected, z$r_range_corrected, z$r_both_corrected))
          else
            p("Belum ada seleksi (100% pelamar dipakai). Turunkan slider seleksi untuk melihat range restriction bekerja — ini persis situasi validasi tes seleksi, ketika kriteria hanya terlihat pada mereka yang diterima."),
          p(strong("Koreksi adalah estimasi, bukan pengukuran ulang. "),
            "Keduanya bergantung pada asumsi: koreksi attenuation mengandaikan estimasi reliability yang tepat, ",
            "dan koreksi Thorndike mengandaikan seleksi hanya terjadi pada prediktor serta hubungan yang linear dan homoscedastic. ",
            "Laporkan nilai terkoreksi berdampingan dengan nilai aslinya, jangan menggantikannya.")),
      div(class = "insight", strong("Coba ini: "),
          "set korelasi sebenarnya .60 dan reliability prediktor .90. Turunkan reliability kriteria dari .90 ke .40 dan perhatikan batas atasnya runtuh. ",
          "Setelah itu kembalikan keduanya ke .80 dan turunkan seleksi ke 20% — bandingkan mana yang lebih merusak, error pengukuran atau range restriction.")
    )
  })
  
  val_code <- reactive({
    cfg <- val_data()$config
    sprintf('# Psychometrics Learning Lab — Validity Lab
set.seed(%d)

n     <- %d
rho   <- %.2f    # korelasi antar TRUE score
rel_x <- %.2f    # reliability prediktor
rel_y <- %.2f    # reliability kriteria

# X = T + E dengan var(T) = rel dan var(E) = 1 - rel, sehingga var(X) = 1
z1 <- rnorm(n)
z2 <- rho * z1 + sqrt(1 - rho^2) * rnorm(n)
x  <- z1 * sqrt(rel_x) + rnorm(n, 0, sqrt(1 - rel_x))
y  <- z2 * sqrt(rel_y) + rnorm(n, 0, sqrt(1 - rel_y))

r_obs <- cor(x, y); r_obs
sqrt(rel_x * rel_y)          # batas atas r teramati
r_obs / sqrt(rel_x * rel_y)  # koreksi attenuation (Spearman)

# --- Range restriction: hanya %d%% teratas yang diterima ---
cutoff   <- quantile(x, 1 - %.2f)
selected <- x >= cutoff
r_sel    <- cor(x[selected], y[selected]); r_sel

# Koreksi Thorndike Case II
ratio <- sd(x) / sd(x[selected])
r_rr  <- (r_sel * ratio) / sqrt(1 - r_sel^2 + r_sel^2 * ratio^2)
r_rr

r_rr / sqrt(rel_x * rel_y)   # dikoreksi keduanya
', cfg$seed, cfg$n, cfg$rho_true, cfg$rel_x, cfg$rel_y,
            cfg$select_top_pct, cfg$select_top_pct / 100)
  })
  register_syntax("val_syntax", "val_syntax_dl", val_code, "validity_lab.R")
  
  # ==========================================================
  # SCALE DEVELOPMENT LAB
  # ==========================================================
  sc_data <- eventReactive(input$sc_run, ignoreNULL = FALSE, {
    simulate_likert_scale(input$sc_n, input$sc_k, input$sc_loading,
                          input$sc_reverse, input$sc_bad, input$sc_seed)
  })
  
  # SC-01: penghapusan bertahap. Menghapus semua item ter-flag sekaligus
  # menyembunyikan fakta bahwa setiap penghapusan mengubah skor total, dan
  # karena itu mengubah corrected item-total correlation seluruh item lain.
  stepwise_review <- function(dat, cutoff, min_items = 3, max_steps = 12) {
    keep <- names(dat)
    rows <- list()
    step <- 0L
    repeat {
      d <- dat[, keep, drop = FALSE]
      ia <- item_analysis(d)
      worst_i <- which.min(ia$corrected_item_total)
      worst <- ia$item[worst_i]
      worst_r <- ia$corrected_item_total[worst_i]
      a <- cronbach_alpha(d)
      stop_now <- !is.finite(worst_r) || worst_r >= cutoff ||
        length(keep) <= min_items || step >= max_steps
      rows[[length(rows) + 1]] <- data.frame(
        langkah = step, n_item = length(keep),
        alpha = a, omega = omega_total(d),
        sem = stats::sd(rowSums(d)) * sqrt(max(1 - a, 0)),
        min_rit = worst_r,
        dibuang = if (stop_now) NA_character_ else worst,
        stringsAsFactors = FALSE
      )
      if (stop_now) break
      keep <- setdiff(keep, worst)
      step <- step + 1L
    }
    list(table = do.call(rbind, rows), keep = keep,
         truncated = step >= max_steps)
  }
  
  sc_analysis <- reactive({
    s <- sc_data(); dat <- s$data
    ia <- item_analysis(dat)
    ia$flag <- ifelse(ia$corrected_item_total < input$sc_cut, "Review", "Keep")
    steps <- stepwise_review(dat, input$sc_cut)
    a0 <- cronbach_alpha(dat)
    list(s = s, dat = dat, ia = ia, steps = steps,
         alpha_before = a0, omega_before = omega_total(dat),
         alpha_after = steps$table$alpha[nrow(steps$table)],
         omega_after = steps$table$omega[nrow(steps$table)],
         keep = steps$keep)
  })
  
  output$sc_metrics <- renderUI({
    z <- sc_analysis()
    div(class = "metric-grid",
        metric_box("Alpha sebelum", fmt_n(z$alpha_before)),
        metric_box("Alpha sesudah", fmt_n(z$alpha_after)),
        metric_box("Omega sebelum", fmt_n(z$omega_before)),
        metric_box("Omega sesudah", fmt_n(z$omega_after)),
        metric_box("Item ter-flag", sum(z$ia$flag == "Review"), "pada tinjauan awal"),
        metric_box("Item tersisa", length(z$keep),
                   sprintf("dari %d item awal", ncol(z$dat))))
  })
  
  output$sc_table <- renderDT({
    d <- sc_analysis()$ia
    d$mean <- round(d$mean, 2); d$sd <- round(d$sd, 2)
    d$corrected_item_total <- round(d$corrected_item_total, 3)
    d$alpha_if_deleted <- round(d$alpha_if_deleted, 3)
    names(d) <- c("Item", "Mean", "SD", "Corrected item-total", "Alpha if deleted", "Keputusan")
    datatable(d, rownames = FALSE, options = list(pageLength = 15, scrollX = TRUE))
  })
  
  output$sc_dl <- renderUI({
    dl_row(
      downloadButton("sc_data_dl", "Unduh data respons (CSV)", class = "btn-default"),
      downloadButton("sc_item_dl", "Unduh tabel keputusan (CSV)", class = "btn-default")
    )
  })
  register_data_dl("sc_data_dl", "scale_development_responses.csv", function() sc_analysis()$dat)
  register_data_dl("sc_item_dl", "scale_development_decisions.csv", function() sc_analysis()$ia)
  
  output$sc_stepwise <- renderPlot({
    z <- sc_analysis(); t <- z$steps$table
    d <- rbind(
      data.frame(langkah = t$langkah, value = t$alpha, ukuran = "Cronbach alpha"),
      data.frame(langkah = t$langkah, value = t$omega, ukuran = "McDonald omega"),
      data.frame(langkah = t$langkah, value = t$sem, ukuran = "SEM (skor total)"),
      data.frame(langkah = t$langkah, value = t$n_item, ukuran = "Jumlah item")
    )
    d$ukuran <- factor(d$ukuran, levels = c("Cronbach alpha", "McDonald omega",
                                            "SEM (skor total)", "Jumlah item"))
    ggplot(d, aes(langkah, value)) +
      geom_line(linewidth = 1.05, colour = col_blue()) +
      geom_point(size = 2.4, colour = col_ink()) +
      facet_wrap(~ukuran, scales = "free_y", ncol = 2) +
      scale_x_continuous(breaks = t$langkah) +
      labs(x = "Langkah penghapusan", y = NULL,
           title = "Konsekuensi menghapus item satu per satu",
           caption = "Setiap sumbu punya skala sendiri: alpha, omega, SEM, dan jumlah item bukan besaran yang sebanding.") +
      pt()
  })
  
  output$sc_step_note <- renderUI({
    z <- sc_analysis(); t <- z$steps$table
    removed <- stats::na.omit(t$dibuang)
    tagList(
      stat_readout(
        sprintf("%d item dibuang dalam %d langkah; alpha %.3f → %.3f, omega %s → %s",
                length(removed), nrow(t) - 1, z$alpha_before, z$alpha_after,
                fmt_n(z$omega_before), fmt_n(z$omega_after)),
        paste0(
          if (length(removed))
            paste0("Urutan pembuangan: ", paste(removed, collapse = " → "), ". ")
          else "Tidak ada item yang di bawah cutoff, sehingga tidak ada yang dibuang. ",
          "Penghapusan dilakukan satu per satu karena setiap pembuangan mengubah skor total, ",
          "dan karena itu mengubah corrected item-total correlation seluruh item lain. ",
          "Item yang tadinya aman bisa menjadi ter-flag, dan sebaliknya — sesuatu yang tidak terlihat ",
          "bila semua item ter-flag dibuang sekaligus.",
          if (isTRUE(z$steps$truncated))
            " Proses dihentikan pada batas 12 langkah agar tetap responsif." else ""
        ),
        "pada tiap langkah: buang item dengan corrected item-total terendah, lalu hitung ulang"
      ),
      div(class = "status-warn",
          strong("Perhatikan naiknya SEM. "),
          "Alpha bisa naik sementara SEM juga naik, karena skor total menjadi lebih pendek. ",
          "Reliability yang lebih tinggi pada tes yang lebih pendek tidak otomatis berarti skor yang lebih presisi ",
          "dalam satuan yang Anda pakai untuk mengambil keputusan.")
    )
  })
  
  output$sc_diag <- renderPlot({
    d <- sc_analysis()$ia
    ggplot(d, aes(corrected_item_total, alpha_if_deleted)) +
      geom_vline(xintercept = input$sc_cut, linetype = 2, colour = col_accent()) +
      geom_point(aes(shape = flag, colour = flag), size = 2.8) +
      geom_text(aes(label = sub("Item_", "", item)), vjust = -0.9, size = 3.1,
                colour = col_ink()) +
      scale_colour_manual(values = c(Keep = col_blue(), Review = col_accent())) +
      labs(x = "Corrected item-total correlation", y = "Alpha bila item dihapus",
           shape = NULL, colour = NULL, title = "Diagnostik item",
           caption = "Angka pada titik adalah nomor item. Garis putus-putus adalah cutoff yang Anda setel.") +
      pt()
  })
  
  output$sc_interpret <- renderUI({
    z <- sc_analysis()
    flagged <- sum(z$ia$flag == "Review")
    removed <- stats::na.omit(z$steps$table$dibuang)
    tagList(
      div(class = "panel-card", h4("Scale Development"),
          p("Scale development adalah proses membangun sekumpulan item yang bersama-sama merepresentasikan target construct. ",
            "Item statistics membantu menemukan potensi masalah, tetapi item yang menaikkan alpha belum tentu memberi representasi construct yang lebih baik."),
          p("Karena itu aplikasi memakai kata Review, bukan Delete. Flag berarti item perlu diperiksa lebih lanjut, bukan langsung dibuang.")),
      div(class = "panel-card", h4("Membaca hasil saat ini"),
          p(sprintf("Dengan cutoff %.2f, %d dari %d item mendapat flag pada tinjauan awal, dan %d item benar-benar terbuang pada proses bertahap.",
                    input$sc_cut, flagged, ncol(z$dat), length(removed))),
          p(sprintf("Alpha bergerak dari %.3f ke %.3f, omega dari %s ke %s.",
                    z$alpha_before, z$alpha_after, fmt_n(z$omega_before), fmt_n(z$omega_after))),
          if (length(z$s$reverse_idx))
            p(sprintf("Simulasi memuat %d reverse item yang belum direcode. Perhatikan apakah item itulah yang pertama terbuang — bila ya, yang sebenarnya Anda temukan adalah kesalahan scoring, bukan item yang buruk.",
                      length(z$s$reverse_idx))),
          p("Corrected item-total correlation yang rendah dapat muncul karena item lemah, reverse scoring yang belum benar, wording ambigu, ",
            "variance respons yang sempit, atau karena item merepresentasikan aspek construct yang berbeda namun tetap penting."),
          p("Alpha if deleted menunjukkan konsekuensi statistik dari penghapusan, bukan kualitas isinya. ",
            "Membuang semua item yang menurunkan alpha dapat mempersempit domain construct dan justru melemahkan bukti content validity.")),
      div(class = "panel-card", h4("Sebelum memutuskan membuang item"),
          p("Periksa setidaknya: kesesuaian item dengan construct blueprint, scoring dan reverse coding, wording, distribusi respons, ",
            "corrected item-total correlation, factor loading dan cross-loading bila tersedia, konten yang tumpang tindih, ",
            "expert judgment, serta cakupan domain.")),
      div(class = "insight", strong("Coba ini: "),
          "gunakan 20 item dengan beberapa item lemah dan dua reverse item tanpa recoding. Catat urutan pembuangannya. ",
          "Lalu set reverse item ke 0 dan jalankan ulang dengan seed yang sama — perhatikan apakah diagnosisnya berubah hanya karena scoring diperbaiki.")
    )
  })
  
  sc_code <- reactive({
    sprintf('# Psychometrics Learning Lab — Scale Development Lab
set.seed(%d)

n <- %d; k <- %d; loading <- %.2f
reverse_n <- %d; bad_n <- %d; cutoff <- %.2f

trait    <- rnorm(n)
loadings <- rep(loading, k)
if (bad_n > 0) loadings[(k - bad_n + 1):k] <- runif(bad_n, .05, .25)
dat <- as.data.frame(sapply(seq_len(k), function(j) {
  y <- loadings[j] * trait + sqrt(max(1 - loadings[j]^2, .05)) * rnorm(n)
  as.integer(cut(y, breaks = c(-Inf, -.85, -.20, .35, 1.05, Inf), labels = 1:5))
}))
names(dat) <- sprintf("Item_%%02d", seq_len(k))
if (reverse_n > 0) dat[, 1:reverse_n] <- 6 - dat[, 1:reverse_n]

cronbach_alpha <- function(x) {
  k <- ncol(x); k / (k - 1) * (1 - sum(sapply(x, var)) / var(rowSums(x)))
}
item_stats <- function(x) {
  total <- rowSums(x)
  data.frame(
    item = names(x),
    rit  = sapply(names(x), function(j) cor(x[[j]], total - x[[j]])),
    a_if_deleted = sapply(names(x), function(j) cronbach_alpha(x[, setdiff(names(x), j)]))
  )
}
item_stats(dat)

# --- Penghapusan BERTAHAP, bukan sekaligus ---
# Setiap pembuangan mengubah skor total, sehingga corrected item-total
# correlation seluruh item lain ikut berubah.
keep <- names(dat)
repeat {
  st    <- item_stats(dat[, keep, drop = FALSE])
  worst <- st$item[which.min(st$rit)]
  if (min(st$rit) >= cutoff || length(keep) <= 3) break
  cat(sprintf("buang %%s (rit = %%.3f), alpha sekarang %%.3f\\n",
              worst, min(st$rit), cronbach_alpha(dat[, keep, drop = FALSE])))
  keep <- setdiff(keep, worst)
}
keep
cronbach_alpha(dat[, keep, drop = FALSE])
', input$sc_seed, input$sc_n, input$sc_k, input$sc_loading,
input$sc_reverse, input$sc_bad, input$sc_cut)
  })
  register_syntax("sc_syntax", "sc_syntax_dl", sc_code, "scale_development.R")
  
}

shinyApp(ui, server)
