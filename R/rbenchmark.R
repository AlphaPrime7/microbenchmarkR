#BENCHMARKING AND TABLES

#some nfd benchmarks I had not done before
require(microbenchmark)
library(microbenchmark)
library(normfluodbf)
help("normfluodat")
fpath <- system.file("extdata", "dat_4.dat", package = "normfluodbf", mustWork = TRUE)

result <- microbenchmark(
  mvp <- normfluodat(dat=fpath, tnp = 3, cycles = 40),
  stripped <- normfluordat(dat=fpath, tnp = 3, cycles = 40),
  full <- normfluodatfull(dat=fpath, tnp = 3, cycles = 40),
  lite <- normfluodatlite(dat=fpath, tnp = 3, cycles = 40),
  times = 5  # Number of times to run each code snippet
)

#print
print(result)
result = summary(result)

#plot
library(ggplot2)
library(ggfortify)
autoplot(result)
names(result)

#make table for more R learning

#GT
library(gt)
result %>% dplyr::select(expr, min, mean, median, max, neval) %>%
  gt() %>%
  tab_header(title = 'normfluodat benchmarks',
             subtitle = 'Nofun tables with R')


#KABLE
library(kableExtra)
library(formattable)

#1
result$expr <- color_tile("white", "orange")(result$expr)
result$min <- ifelse(
  result$min > 50,
  cell_spec(result$min, color = "red", bold = T),
  cell_spec(result$min, color = "green", italic = T)
)

result$mean <- color_bar("lightgreen")(result$mean)
result <- result[c("expr", "min", "lq", "mean", "median", "uq", "max", "neval", "plots")]

kbl(result, escape = F) %>%
  kable_paper("hover", full_width = F) %>%
  column_spec(4, width = "3cm") %>%
  scroll_box(width = "1000px", height = "1000px") %>% #use % as well
  footnote(general = "Benchmark results for variants of normfluodat.") %>%
  save_kable(file = "first_kable.html", self_contained = T)

#2
result[,'plots'] <- " "
bmplot_list <- split(result$mean, result$neval)
result %>%
  kbl(booktabs = TRUE) %>%
  kable_paper(full_width = F, font_size = 20) %>% #kable_classic,kable_material_dark
  column_spec(4, color = "white",
              background = spec_color(result$mean, end = 0.9),
              popover = paste("expr:", result$expr)) %>%
  column_spec(ncol(result), image = spec_plot(bmplot_list)) %>%
  row_spec(3, bold = T, color = "white", background = "#D7261E")

#3
result %>%
  kbl() %>%
  kable_material_dark("hover", full_width = F, position = "left") #kable_classic

#4
result %>%
  kbl() %>%
  kable_styling(fixed_thead = T, bootstrap_options = c("striped", "hover", "condensed"),
                full_width = F,
                position = "float_right",
                font_size = 20)


#DT
install.packages('DT')
library(DT)
DT:::DT2BSClass('display')
DT:::DT2BSClass(c('compact', 'cell-border'))
datatable(result, filter = 'top', rownames = LETTERS[1:nrow(result)],
          class = 'cell-border stripe',
          editable = 'cell',
          caption = 'Table 1: First DT proper.')


#REACTABLE
library(reactable)
reactable(result)

#FLEXTABLE-best for normal humans
library(flextable)
set_flextable_defaults(
  font.family = "Arial", font.size = 10,
  border.color = "gray")

#base table
flextable(result) %>%
  bold(part = "header") %>%
  add_footer_lines("normfluodbf benchmark")

#word embedding
flextable(result) %>%
  theme_vanilla() %>%
  save_as_docx(path = "first_flextable.docx")

#RHANDSONTABLE-editable like DT
library(rhandsontable)
result[,'include'] = TRUE
rhandsontable(result, width = 600, height = 300) %>%
  hot_col("neval", allowInvalid = TRUE) #provides a good tool to control rows based on user input

rhandsontable(result, width = 600, height = 300, useTypes = FALSE) %>%
  hot_context_menu(
    customOpts = list(
      csv = list(name = "Download to CSV",
                 callback = htmlwidgets::JS(
                   "function (key, options) {
                         var csv = csvString(this, sep=',', dec='.');

                         var link = document.createElement('a');
                         link.setAttribute('href', 'data:text/plain;charset=utf-8,' +
                           encodeURIComponent(csv));
                         link.setAttribute('download', 'data.csv');

                         document.body.appendChild(link);
                         link.click();
                         document.body.removeChild(link);
                       }"))))

rhandsontable(result, useTypes = FALSE, search = TRUE, width = 550, height = 300) %>%
  hot_context_menu(
    customOpts = list(
      search = list(name = "Search",
                    callback = htmlwidgets::JS(
                      "function (key, options) {
                         var srch = prompt('Search criteria');

                         this.search.query(srch);
                         this.render();
                       }"))))

rhandsontable(result, useTypes = F,  readOnly = TRUE, width = 750, height = 300) %>%
  hot_cols(renderer = "
           function (instance, td, row, col, prop, value, cellProperties) {
             Handsontable.renderers.NumericRenderer.apply(this, arguments);
             if (td[row,col] >= 65) {
              td.style.background = 'red';
             } else {
             td.style.background = 'lightgreen';
             }
           }")

rhandsontable(result, readOnly = FALSE, width = 750, height = 300) %>%
  hot_heatmap(color_scale = c("#17F556", "#ED6D47")) #use types defines the column types and will be set to text if FALSE

#REACTABLEFMTR
library(reactable)
library(reactablefmtr)
library(htmltools)
library(viridis)

reactable(
  result,
  defaultColDef = colDef(
    cell = data_bars(result,
                     fill_color = viridis(5, direction = -1),
                     background = "lightgrey",
                     text_position = "inside-end"), # "inside-end","outside-base"
  )
)

result %>%
  dplyr::mutate(color_pal = dplyr::case_when(
    (result$mean > 65) ~ "#FF3B28", #str_detect(expr, "fluo") ~ "#FF3B28",
    (result$mean < 65) ~ "#006FEF",
    TRUE ~ "darkgrey"
  )) %>%
  reactable(.,
            pagination = FALSE,
            defaultColDef = colDef(
              cell = data_bars(.,
                               fill_color_ref = "color_pal",
                               text_position = "inside-end",
                               background = "lightgrey")
            ),
            columns = list(color_pal = colDef(show = FALSE) ## hide the color_pal column
            )
  )

reactable(
  result,
  pagination = FALSE,
  defaultColDef = colDef(
    cell = data_bars(result,
                     fill_color = c("#1efffd", "#1e20ff"),
                     fill_gradient = TRUE,
                     background = "lightgrey",
                     brighten_text = FALSE,
                     text_color = "white")
  )
)

result %>%
  dplyr::mutate(Change = round(mean - median,2) ) %>%
  dplyr::select(expr, Change) %>%
  reactable(.,
            pagination = FALSE,
            columns = list(
              Change = colDef(
                cell = data_bars(.,
                                 fill_color = c("red","green") ))
            )
  )
