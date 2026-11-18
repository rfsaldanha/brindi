# Generates a self-contained static HTML dashboard summarizing BRINDI's
# completeness against the RIPSA Livro Verde indicator matrix. Does not
# require quarto or pandoc; only base R, ggplot2 and base64enc.
#
# Usage (from the package root, after devtools::load_all() or library(brindi)):
#   Rscript dashboards/generate_static_dashboard.R

if (!requireNamespace("brindi", quietly = TRUE)) {
  # fallback for running from the source tree without installing the package
  devtools_ok <- requireNamespace("devtools", quietly = TRUE)
  if (devtools_ok) {
    tryCatch(devtools::load_all(".", quiet = TRUE), error = function(e) NULL)
  }
}

library(ggplot2)

matched <- brindi::ripsa_match_indicators(show_all = TRUE)
summ <- brindi::ripsa_completeness_summary()

total_ind <- nrow(matched)
total_impl <- sum(matched$implementado)
total_sug <- sum(matched$status == "sugerivo")
total_rej <- sum(matched$status == "rejeitado")
pct_geral <- round(100 * total_impl / total_ind, 1)
pct_sug <- round(100 * total_sug / total_ind, 1)

chart_path <- tempfile(fileext = ".png")
p <- ggplot(summ, aes(x = reorder(dimensao, percentual), y = percentual, fill = percentual)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = paste0(implementados, "/", total)), hjust = -0.2, size = 5, fontface = "bold") +
  scale_fill_gradient(low = "#fee5d9", high = "#de2d26", guide = "none") +
  coord_flip(ylim = c(0, 100)) +
  labs(x = NULL, y = "% implementado", title = "Completude por dimensão (RIPSA Livro Verde)") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 16, face = "bold", margin = margin(b = 15)),
    axis.text = element_text(size = 12, color = "#333"),
    axis.title.y = element_text(size = 13, color = "#333"),
    panel.grid.major = element_line(color = "#e0e0e0"),
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 20, 10, 10)
  )
ggsave(chart_path, p, width = 9, height = 5, dpi = 150, bg = "white")
chart_b64 <- base64enc::base64encode(chart_path)

value_box <- function(title, value, color) {
  sprintf(
    '<div class="valuebox" style="background:%s"><div class="vb-title">%s</div><div class="vb-value">%s</div></div>',
    color, title, value
  )
}

summ_rows <- paste(
  sprintf(
    "<tr><td>%s</td><td>%d</td><td>%d</td><td>%d</td><td>%.1f%%</td></tr>",
    summ$dimensao, summ$total, summ$implementados, summ$pendentes, summ$percentual
  ),
  collapse = "\n"
)

# All indicators with status colors
all_ind <- matched[, c("code", "nome", "funcao", "titulo_brindi", "fonte", "similaridade", "status")]
all_ind <- all_ind[order(all_ind$similaridade, decreasing = TRUE), ]

status_color <- function(status) {
  switch(status,
    "confirmado" = "#d4edda",
    "sugerivo" = "#fff3cd",
    "rejeitado" = "#f8d7da",
    "#ffffff"
  )
}

status_text_color <- function(status) {
  switch(status,
    "confirmado" = "#155724",
    "sugerivo" = "#856404",
    "rejeitado" = "#721c24",
    "#000000"
  )
}

all_rows <- paste(
  sprintf(
    '<tr><td>%s</td><td>%s</td><td><code>%s()</code></td><td>%s</td><td>%s</td><td>%.3f</td><td style="background:%s;color:%s;font-weight:bold">%s</td></tr>',
    all_ind$code, all_ind$nome, ifelse(is.na(all_ind$funcao), "-", all_ind$funcao),
    ifelse(is.na(all_ind$titulo_brindi), "-", all_ind$titulo_brindi),
    ifelse(is.na(all_ind$fonte), "-", all_ind$fonte),
    all_ind$similaridade,
    sapply(all_ind$status, status_color),
    sapply(all_ind$status, status_text_color),
    all_ind$status
  ),
  collapse = "\n"
)

html <- sprintf('<!DOCTYPE html>
<html lang="pt-BR">
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width, initial-scale=1.0">
<title>Completude BRINDI x RIPSA (Livro Verde)</title>
<style>
  body { font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, Helvetica, Arial, sans-serif; margin: 0; padding: 30px; background: #f5f7fa; color: #2c3e50; line-height: 1.6; }
  h1 { font-size: 1.8em; margin-bottom: 8px; color: #1a252f; font-weight: 700; }
  .subtitle { color: #6c757d; margin-bottom: 30px; font-size: 0.95em; }
  .valueboxes { display: flex; gap: 20px; margin-bottom: 35px; flex-wrap: wrap; }
  .valuebox { flex: 1; min-width: 200px; border-radius: 12px; padding: 22px 24px; color: white; box-shadow: 0 4px 12px rgba(0,0,0,0.1); transition: transform 0.2s; }
  .valuebox:hover { transform: translateY(-2px); }
  .vb-title { font-size: 0.9em; opacity: 0.95; font-weight: 500; text-transform: uppercase; letter-spacing: 0.5px; }
  .vb-value { font-size: 2.2em; font-weight: 700; margin-top: 8px; }
  .grid { display: flex; gap: 28px; margin-bottom: 35px; flex-wrap: wrap; }
  .card { background: white; border-radius: 12px; padding: 24px 26px; box-shadow: 0 2px 8px rgba(0,0,0,0.06); flex: 1; min-width: 340px; }
  .card h2 { font-size: 1.2em; margin-top: 0; margin-bottom: 18px; color: #1a252f; font-weight: 600; }
  table { width: 100%%; border-collapse: collapse; font-size: 0.9em; }
  th, td { text-align: left; padding: 10px 12px; border-bottom: 1px solid #e9ecef; }
  th { background: #f8f9fa; color: #495057; font-weight: 600; font-size: 0.85em; text-transform: uppercase; letter-spacing: 0.3px; }
  tr:hover { background: #f8f9fa; }
  img { max-width: 100%%; border-radius: 8px; }
  code { background: #e9ecef; padding: 3px 6px; border-radius: 4px; font-family: "SF Mono", Monaco, Consolas, monospace; font-size: 0.9em; color: #d63384; }
  .notes { background: white; border-radius: 12px; padding: 24px 26px; box-shadow: 0 2px 8px rgba(0,0,0,0.06); font-size: 0.9em; color: #495057; line-height: 1.7; }
  .notes strong { color: #1a252f; }
</style>
</head>
<body>
<h1>Completude BRINDI x RIPSA (Livro Verde)</h1>
<div class="subtitle">Gerado automaticamente a partir de ripsa_match_indicators() e ripsa_completeness_summary()</div>

<div class="valueboxes">
  %s
  %s
  %s
  %s
</div>

<div class="grid">
  <div class="card" style="flex: 1.4;">
    <h2>Completude por dimensão</h2>
    <img src="data:image/png;base64,%s" alt="Gráfico de completude por dimensão">
  </div>
  <div class="card">
    <h2>Resumo por dimensão</h2>
    <table>
      <tr><th>Dimensão</th><th>Total</th><th>Implementados</th><th>Pendentes</th><th>%%%% Completo</th></tr>
      %s
    </table>
  </div>
</div>

<div class="card" style="margin-bottom: 35px;">
  <h2>Todos os indicadores (verificação manual)</h2>
  <p style="font-size: 0.9em; color: #6c757d; margin-bottom: 16px; padding: 12px; background: #f8f9fa; border-radius: 8px; border-left: 4px solid #2c7fb8;">
    <strong>Confirmado:</strong> similaridade ≥ 0.55 | <strong>Sugestivo:</strong> 0.3-0.55 | <strong>Rejeitado:</strong> < 0.3
  </p>
  <div style="overflow-x: auto;">
    <table>
      <tr><th>Código</th><th>Indicador RIPSA</th><th>Função</th><th>Título BRINDI</th><th>Fonte</th><th>Similaridade</th><th>Status</th></tr>
      %s
    </table>
  </div>
</div>

<div class="notes">
  <strong>Fonte dos dados:</strong> matriz de indicadores extraída do sumário oficial do Livro Verde da RIPSA
  (168 indicadores nas dimensões DEM, SOC, MRT, MRB, FRP, REC, COB); catálogo com os títulos das 58 funções
  <code>indi_*()</code> implementadas no pacote <code>brindi</code>.<br><br>
  <strong>Critério de correspondência:</strong> similaridade de tokens (Jaccard) entre o nome do indicador RIPSA
  e o título da função BRINDI, após remover termos genéricos (taxa, mortalidade, incidência, proporção etc.)
  repetidos na maioria dos títulos.<br>
  <strong>Confirmado:</strong> similaridade ≥ 0.55 (match forte, específico)<br>
  <strong>Sugestivo:</strong> similaridade entre 0.3 e 0.55 (match possível, requer verificação manual)<br>
  <strong>Rejeitado:</strong> similaridade < 0.3 (match fraco, provavelmente incorreto)<br><br>
  Pareamento heurístico — indicadores implementados sob um título muito diferente do Livro Verde podem não ser
  detectados automaticamente e merecem revisão manual. A tabela mostra todos os indicadores ordenados por similaridade.
</div>

</body>
</html>', value_box("Indicadores RIPSA (Livro Verde)", total_ind, "#2c7fb8"),
   value_box("Confirmados (≥0.55)", total_impl, "#2ca25f"),
   value_box("Sugestivos (0.3-0.55)", total_sug, "#de7c00"),
   value_box("Completude geral", paste0(pct_geral, "%"), "#636363"),
   chart_b64, summ_rows, all_rows)

out_path <- "dashboards/ripsa_completude.html"
writeLines(html, out_path)
cat("Dashboard salvo em:", normalizePath(out_path), "\n")
