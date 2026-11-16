library(gt)
library(scales)
library(lubridate)
library(httr)
library(readxl)
library(tidyverse)
library(stringr)
library(readr)


# 1. Función de descarga y procesamiento
procesar_bma_bcra <- function() {
  url_bcra <- "https://www.bcra.gob.ar/archivos/Pdfs/PublicacionesEstadisticas/series.xlsm"
  archivo_temp <- tempfile(fileext = ".xlsm")
  
  resp <- GET(
    url_bcra,
    config(ssl_verifypeer = FALSE),
    user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64)")
  )
  stop_for_status(resp)
  writeBin(content(resp, "raw"), archivo_temp)
  
  raw_data <- read_excel(
    archivo_temp,
    sheet = "BASE MONETARIA",
    skip = 9,
    col_names = FALSE,
    col_types = "text"
  )
  
  df_diario <- raw_data %>%
    select(
      fecha_raw = 1,
      total_bm = 3,
      divisas_privado_otros = 4,
      divisas_tesoro = 5,
      tesoro_adelantos_transitorios = 6,
      tesoro_transferencia_utilidades = 7,
      tesoro_resto = 8,
      pases = 9,
      leliq_notaliq = 10,
      redescuentos_adelantos = 11,
      intereses_primas_remun = 12,
      lebac_nobac = 13,
      rescate_cuasimonedas = 14,
      operaciones_lefi = 15,
      otros = 16,
      tipo_serie = 33
    ) %>%
    filter(tipo_serie == "D") %>%
    mutate(
      fecha = as.Date(as.numeric(fecha_raw), origin = "1899-12-30"),
      across(total_bm:otros, as.numeric)
    ) %>%
    select(fecha, total_bm:otros) %>%
    arrange(fecha)
  
  df_completo <- df_diario %>%
    mutate(
      anio = year(fecha),
      mes = floor_date(fecha, "month")
    ) %>%
    group_by(mes) %>%
    mutate(across(total_bm:otros, ~ cumsum(replace(., is.na(.), 0)), .names = "{.col}_mtd")) %>%
    ungroup() %>%
    group_by(anio) %>%
    mutate(across(total_bm:otros, ~ cumsum(replace(., is.na(.), 0)), .names = "{.col}_ytd")) %>%
    ungroup() %>%
    select(-anio, -mes)
  
  return(df_completo)
}



# 2. Función de generación de imagen
generar_imagen_bma <- function(df_bma, ruta_png = "prueba_bma.png") {
  ultima_fecha <- max(df_bma$fecha, na.rm = TRUE)
  fila_hoy <- df_bma %>% filter(fecha == ultima_fecha)
  
  meses_es <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio",
                "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")
  nombre_mes <- meses_es[month(ultima_fecha)]
  anio_actual <- year(ultima_fecha)
  
  col_diaria <- paste0("Variación Diaria (", format(ultima_fecha, "%d/%m/%Y"), ")")
  col_mes    <- paste0("Variación Mes de ", nombre_mes)
  col_anio   <- paste0("Variación Año ", anio_actual)
  
  diccionario <- tribble(
    ~variable,                         ~etiqueta,
    "total_bm",                        "Base Monetaria Total",
    "divisas_privado_otros",           "Compras de Divisas - Privado y Otros",
    "divisas_tesoro",                  "Compras de Divisas - Tesoro Nacional",
    "tesoro_adelantos_transitorios",   "Tesoro - Adelantos Transitorios",
    "tesoro_transferencia_utilidades", "Tesoro - Transferencia de Utilidades",
    "tesoro_resto",                    "Tesoro - Resto",
    "pases",                           "Pases",
    "leliq_notaliq",                   "LELIQ y NOTALIQ",
    "redescuentos_adelantos",          "Redescuentos y Adelantos",
    "intereses_primas_remun",          "Intereses, Primas y Remun. Ctas. Ctes.",
    "lebac_nobac",                     "LEBAC y NOBAC",
    "rescate_cuasimonedas",            "Rescate de Cuasimonedas",
    "operaciones_lefi",                "Operaciones con LEFI",
    "otros",                           "Otros"
  )
  
  tabla_datos <- fila_hoy %>%
    select(-fecha) %>%
    pivot_longer(everything(), names_to = "col_raw", values_to = "valor") %>%
    mutate(
      periodo = case_when(
        grepl("_mtd$", col_raw) ~ "mes",
        grepl("_ytd$", col_raw) ~ "anio",
        TRUE ~ "diaria"
      ),
      variable = gsub("(_mtd|_ytd)$", "", col_raw)
    ) %>%
    select(variable, periodo, valor) %>%
    pivot_wider(names_from = periodo, values_from = valor) %>%
    right_join(diccionario, by = "variable") %>%
    select(etiqueta, diaria, mes, anio)
  
  gt_tbl <- tabla_datos %>%
    gt() %>%
    tab_header(
      title = md("**Base Monetaria — Factores de Explicación**"),
      subtitle = paste0("En millones de pesos | Actualizado al ", format(ultima_fecha, "%d/%m/%Y"))
    ) %>%
    cols_label(
      etiqueta = "Concepto",
      diaria   = col_diaria,
      mes      = col_mes,
      anio     = col_anio
    ) %>%
    # Centrar columnas numéricas y alinear conceptos a la izquierda
    cols_align(
      align = "center",
      columns = c(diaria, mes, anio)
    ) %>%
    cols_align(
      align = "left",
      columns = etiqueta
    ) %>%
    # Formato numérico argentino sin decimales
    fmt_number(
      columns = c(diaria, mes, anio),
      decimals = 0,
      sep_mark = ".",
      dec_mark = ","
    ) %>%
    # Colores condicionales ajustados a enteros
    tab_style(
      style = cell_text(color = "#1B7837", weight = "600"),
      locations = list(
        cells_body(columns = diaria, rows = round(diaria) > 0),
        cells_body(columns = mes,    rows = round(mes) > 0),
        cells_body(columns = anio,   rows = round(anio) > 0)
      )
    ) %>%
    tab_style(
      style = cell_text(color = "#B2182B", weight = "600"),
      locations = list(
        cells_body(columns = diaria, rows = round(diaria) < 0),
        cells_body(columns = mes,    rows = round(mes) < 0),
        cells_body(columns = anio,   rows = round(anio) < 0)
      )
    ) %>%
    tab_style(
      style = cell_text(color = "#999999"),
      locations = list(
        cells_body(columns = diaria, rows = round(diaria) == 0),
        cells_body(columns = mes,    rows = round(mes) == 0),
        cells_body(columns = anio,   rows = round(anio) == 0)
      )
    ) %>%
    # Destacar la fila de Base Monetaria Total
    tab_style(
      style = list(
        cell_fill(color = "#EEF3FA"),
        cell_text(weight = "bold", color = "#002060")
      ),
      locations = cells_body(rows = 1)
    ) %>%
    # Jerarquía tipográfica y limpieza visual
    tab_options(
      table.font.names = "Arial",
      table.font.size = px(13),
      heading.background.color = "#002060",
      heading.title.font.size = px(20),
      heading.subtitle.font.size = px(13),
      column_labels.background.color = "#F4F6F9",
      column_labels.font.weight = "bold",
      data_row.padding = px(6),
      table.border.top.color = "#002060",
      table.border.bottom.color = "#002060"
    )
  
  gtsave(gt_tbl, filename = ruta_png, zoom = 2)
  message("Imagen actualizada en: ", normalizePath(ruta_png))
}



# 3. Flujo de automatización: control de novedad, guardado y alerta
ruta_csv <- "data/bma_diaria.csv"
ruta_img <- "alerta_bma.png"

bma_limpia <- procesar_bma_bcra()
ultima_fecha_bcra <- max(bma_limpia$fecha, na.rm = TRUE)

ultima_fecha_local <- if (file.exists(ruta_csv)) {
  df_viejo <- read_csv(ruta_csv, show_col_types = FALSE)
  max(as.Date(df_viejo$fecha), na.rm = TRUE)
} else {
  as.Date("1900-01-01")
}

# Si no hay fecha nueva, terminamos silenciosamente
if (ultima_fecha_bcra <= ultima_fecha_local) {
  message("Sin novedades al ", Sys.time(), ". Última fecha BCRA: ", ultima_fecha_bcra)
  quit(save = "no", status = 0)
}

# Guardamos la base histórica actualizada (la que luego leerá tu Shiny)
dir.create("data", showWarnings = FALSE)
write_csv(bma_limpia, ruta_csv)
message("Nueva fecha detectada: ", ultima_fecha_bcra, ". Base actualizada.")

# Generamos la tabla en imagen
generar_imagen_bma(bma_limpia, ruta_img)

# Enviamos la imagen por Telegram
token   <- Sys.getenv("TELEGRAM_TOKEN")
chat_id <- Sys.getenv("TELEGRAM_CHAT_ID")

if (token != "" && chat_id != "") {
  resp_tg <- POST(
    url = paste0("https://api.telegram.org/bot", token, "/sendPhoto"),
    body = list(
      chat_id = chat_id,
      photo = upload_file(ruta_img),
      caption = paste0("📊 Base Monetaria actualizada al ", format(ultima_fecha_bcra, "%d/%m/%Y"))
    ),
    encode = "multipart"
  )
  stop_for_status(resp_tg)
  message("Alerta enviada por Telegram con éxito.")
}





