# UTL ==========================================================================
include_month_year <- function(df, table_label) {
  
  df |>
    dplyr::mutate(
      year = lubridate::year(date),
      month = lubridate::month(date, label = TRUE, abbr = FALSE),
      table_name = table_label
    )
  
}

get_colors <- function(categories) {
  
  pal <- c("#43CD80", "#CD3700", "#CD00CD", "#EEB422", "#0000FF",
           "#6B4226", "#2F2F4F", "#00CED1", "#2F4F4F", "#CD661D")
  
  selected_pal <- pal[seq_len(length(categories))]
  
  setNames(selected_pal, categories)
  
}


change <- function(present_value, past_value) {
  
  round(((present_value - past_value) / past_value) * 100, 3)
}


group_yoy <- function(df, gp_var1, gp_var2 = NULL) {
  
  if (!is.null(gp_var2)) {
    
    f_tbl <- dplyr::group_split(df, .data[[gp_var1]], .data[[gp_var2]])
    
  } else {
    
    f_tbl <- dplyr::group_split(df, .data[[gp_var1]])
  }
  
  f_tbl |>
    purrr::map(\(tbl) {
      
      tbl |>
        mutate(
          YoY = change(avg_price, lag(avg_price, 1)),
          icon_value = dplyr::case_when(
            YoY > 0 ~ "increase", 
            YoY < 0 ~ "decrease",
            YoY == 0 ~ "unchanged"
          )
        )
      
    }) |>
    purrr::list_rbind()
}


clean_label <- function(string) {
  
  stringr::str_replace_all(string, "_", " ") |>
    stringr::str_to_title()
  
}


configure_plotly <- function(plotly) {
  
  plotly::config(
    plotly,
    displaylogo = FALSE, 
    modeBarButtonsToRemove = c(
      "zoomIn2d", "zoomOut2d", "pan2d", "select2d", "lasso2d", "autoScale2d",
      "zoom2d", "hoverClosestCartesian", "hoverCompareCartesian"
    )
  )
  
}


get_overall_anno <- function(label) {
  
  label_min <- paste0(label, "_min")
  label_max <- paste0(label, "_max")
  
  list(
    min = overall_anno_points[[label_min]],
    max = overall_anno_points[[label_max]]
  )
}


get_zone_anno <- function(label, zone) {
  
  if (label == "pms") {
    
    return(
      list(
        min = zone_petroleum_anno_point$all_zones$min,
        max = zone_petroleum_anno_point$all_zones$max
      )
    )
    
  } else {
    
    if (label == "des") {
      
      zone_list <- zone_diesel_anno_point[[zone]]
      
    } else if (label == "lpg_5") {
      
      zone_list <- zone_lpg_5[[zone]]
      
    } else if (label == "lpg_12") {
      
      zone_list <- zone_lpg_12[[zone]]
      
    } else if (label == "kero_l") {
      
      zone_list <- zone_kero_l[[zone]]
      
    } else if (label == "kero_g") {
      
      zone_list <- zone_kero_g[[zone]]
    }
    
    return(
      list(
        min = zone_list$min,
        max = zone_list$max
      )
    )
    
  }
}


plot_title <- function(label) {
  
  switch(
    label,
    "pms" = "PMS",
    "des" = "Diesel",
    "lpg_5" = "5KG LPG",
    "lpg_12" = "12KG LPG",
    "kero_l" = "Liters of Kerosene",
    "kero_g" = "Gallons of Kerosene"
  )
  
}


selected_location <- function(df, zone = TRUE, multi = FALSE) {
  
  if (zone) {
    
    selected_loc <- unique(df$geo_zone)
    
    if (multi) {
      
      selected_loc
      
    } else {
      
      if (length(selected_loc) > 1) {
        
        stop("More than one unique zone value was selected")
        
      } else {
        
        selected_loc
      }
    }
    
  } else {
    
    selected_loc <- unique(df$state_and_capital)
    
    if (multi) {
      
      selected_loc
      
    } else {
      
      if (length(selected_loc) > 1) {
        
        stop("More than one unique state value was selected")
        
      } else {
        
        selected_loc
        
      }
      
    } 
  }
}


prepare_dynamic_zone <- function(df) {
  
  df |>
    dplyr::filter(year != 2019) |>
    dplyr::group_by(date, year, geo_zone, table_name) |>
    dplyr::summarise(avg_price = mean(average_price)) |>
    dplyr::ungroup() |>
    dplyr::group_by(geo_zone) |>
    dplyr::mutate(
      min_max = dplyr::case_when(avg_price == min(avg_price) ~ avg_price,
                          avg_price == max(avg_price) ~ avg_price,
                          .default = NA),
      mm_date = dplyr::case_when(!is.na(min_max) ~ date, .default = NA)
    )
    # group_split(date, geo_zone) |>
    # purrr::map(\(tbl) {
    #   
    #   tbl |>
    #     mutate(
    #       min_max = case_when(avg_price == min(avg_price) ~ avg_price,
    #                           avg_price == max(avg_price) ~ avg_price,
    #                           .default = NA),
    #       mm_date = case_when(!is.na(min_max) ~ date, .default = NA)
    #     )
    #   
    # }) |>
    # purrr::list_rbind()
}

prepare_dynamic_zone_state_df <- function(df, zone = TRUE) {

  # gp_location <- ifelse(zone, "geo_zone", "state_and_capital")
  # 
  # df |>
  #   dplyr::filter(year != 2019) |>
  #   dplyr::group_by(date, year, .data[[gp_location]]) |>
  #   dplyr::summarise(avg_price = mean(average_price), .groups = "drop") |>
  #   dplyr::group_by(.data[[gp_location]]) |>
  #   dplyr::mutate(
  #     min_max = dplyr::case_when(avg_price == min(avg_price) ~ avg_price,
  #                                avg_price == max(avg_price) ~ avg_price,
  #                                .default = NA),
  #     mm_date = dplyr::case_when(!is.na(min_max) ~ date, .default = NA)
  #   ) |>
  #   dplyr::ungroup()
  
  f_tbl <- dplyr::filter(df, year != 2019) 
  
  if (zone) {
    f_tbl <- f_tbl |>
      dplyr::group_by(date, year, geo_zone) |>
      dplyr::summarise(avg_price = mean(average_price), .groups = "drop") |>
      dplyr::group_by(geo_zone)
    
  } else {
    f_tbl <- f_tbl |>
      dplyr::group_by(date, year, geo_zone, state_and_capital) |>
      dplyr::summarise(avg_price = mean(average_price), .groups = "drop") |>
      dplyr::group_by(geo_zone, state_and_capital)
  }
    
  f_tbl |>
    dplyr::mutate(
      min_max = dplyr::case_when(
        avg_price == min(avg_price) ~ avg_price,
        avg_price == max(avg_price) ~ avg_price,
        .default = NA
      ),
      mm_date = dplyr::case_when(!is.na(min_max) ~ date, .default = NA)
    )
}


# Table UTL ====================================================================

# Icon to indicate trend: unchanged, up, down, or new
trend_indicator <- function(value = c("unchanged", "increase", "decrease")) {
  
  value <- match.arg(value)
  
  label <- switch(
    value,
    unchanged = "Unchanged", 
    increase = "Increase",
    decrease = "Decrease", 
  )
  
  # Add img role and tooltip/label for accessibility
  args <- list(role = "img", title = label)
  
  
  if (value == "increase") {
    
    args <- c(
      args, 
      list(
        htmltools::tags$i(class = "bi bi-arrow-up-square-fill"), 
        style = "color: #D80032; font-size: 1rem;"
      )
    )
    
  } else if (value == "decrease") {
    
    args <- c(
      args, 
      list(
        htmltools::tags$i(class = "bi bi-arrow-down-square-fill"), 
        style = "color: #7FFFD4; font-size: 1rem;"
      )
    )
    
  } else {
    
    args <- c(
      args, 
      list(
        htmltools::tags$i(class = "bi bi-dash-square-fill"), 
        style = "color: #8D99AE; font-size: 1rem;"
      )
    )
    
  }
  
  do.call(htmltools::span, args)
  
}


# change text column
trend_text_indicatior <- function(value) {
  
  if (value > 0 ) {
    style <- list(color = "#8B2323") 
  } else if (value < 0) {
    style <- list(color = "#008B45")
  } else {
    style <- list(color = "#666")
  }
  
  htmltools::div(paste0(round(value, 2), "%"), style = style)
  
}


# column bar
data_bar_column <- function(df) {
  
  reactablefmtr::data_bars(
    data = df,
    text_position = "above",
    fill_color = "#575757",
    background = "#F7F7F7",
    number_fmt = scales::label_comma(0.01, prefix = "₦"),
    bar_height = 2
  )
  
}


# badge
trend_badge <- function(value) {
  
  if (value < 0) {
    colr <- "#43CD80"
  } else if (value == 0) {
    colr <- "grey"
  } else {
    colr <- "#D80032"
  }
  
  htmltools::tagList(
    
    htmltools::div(
      style = "
              display: flex;
              align-items: center;
              column-gap: 1.3rem;
              padding-left: 1rem;
            ",
      
      htmltools::span(
        style = glue::glue(
          "
          display: inline-block;
          width: 10px;
          height: 10px;
          background-color: {colr};
          border-radius: 50%;
        "
        )
      ),
      
      scales::label_number(0.01, suffix = "%")(value)
    )
    
  )
}


# highlight repeated row values
highlight_first_value <- function(value, index, selected_index) {
  
  if (index %in% selected_index) {
    
    list(color = "#4A4A4A", fontWeight = "bold")
    
  } else {
    
    list(color = "#CCCCCC")
    
  }
  
}


# table theme and pagination language
table_style <- function(
    style = "theme", 
    search_title = "Search...",
    zero_search_result = "No Result Found",
    pagination_title = "Rows"  
) {
  
  if (style == "theme") {
    
    search_icon <- function(fill_color) {
      
      htmltools::div(
        style = glue::glue("color : {fill_color}"), 
        bsicons::bs_icon("search")
      )
      
    }
    
    bg_color <- "#FFFFFF"
    text_color_dark <- "#000000"
    text_color <- "#2E2E2E"
    text_color_light <- "#333333"
    text_color_lighter <- "#444444"
    
    
    reactable::reactableTheme(
      color = text_color,
      backgroundColor = bg_color,
      borderColor = "#EEEEEE",
      borderWidth = "1px",
      highlightColor = "#FAFAFA",
      cellPadding = "10px 8px",
      
      style = list(
        fontFamily = "Roboto, Helvetica Neue, Helvetica, Arial, sans-serif",
        # fontSize = "0.875rem",
        fontSize = "0.9rem",
        
        "a" = list(
          color = text_color,
          textDecoration = "none",
          "&:hover, &:focus" = list(
            textDecoration = "underline",
            textDecorationThickness = "1px"
          )
        ),
        
        ".number" = list(
          color = text_color_light
          # fontFamily = "Source Code Pro, Consolas, Monaco, monospace"
        ),
        
        ".tag" = list(
          padding = "0.125rem 0.25rem",
          color = "hsl(0, 0%, 40%)",
          fontSize = "0.75rem",
          border = "1px solid hsl(0, 0%, 24%)",
          borderRadius = "2px",
          textTransform = "uppercase"
        )
      ),
      
      headerStyle = list(
        color = text_color_light,
        fontWeight = 400,
        fontSize = "1rem",
        letterSpacing = "1px",
        # textTransform = "uppercase",
        "&:hover, &:focus" = list(color = text_color_dark)
      ),
      
      rowHighlightStyle = list(
        # ".tag" = list(color = text_color_dark, borderColor = text_color_lighter)
        "&:hover, &:focus" = list(color = text_color_dark)
      ),
      
      # Full-width search bar with search icon
      searchInputStyle = list(
        paddingLeft = "1.9rem",
        paddingTop = "0.5rem",
        paddingBottom = "0.5rem",
        width = "100%",
        border = "none",
        backgroundColor = bg_color,
        backgroundImage = search_icon(text_color_lighter),
        backgroundSize = "1rem",
        backgroundPosition = "left 0.5rem center",
        backgroundRepeat = "no-repeat",
        "&:focus" = list(backgroundColor = "rgba(255, 255, 255, 0.1)", border = "none"),
        "&:hover, &:focus" = list(backgroundImage = search_icon(text_color)),
        "::placeholder" = list(color = "#CCCCCC"),
        "&:hover::placeholder, &:focus::placeholder" = list(color = "#EEEEEE")
      ),
      
      paginationStyle = list(color = text_color_light),
      pageButtonHoverStyle = list(backgroundColor = "#F2F2F2"),
      pageButtonActiveStyle = list(border = "1px solid #BDBDBD")
    )
    
  } else if (style == "lang") {
    
    reactable::reactableLang(
      searchPlaceholder = search_title,
      noData = zero_search_result,
      pageInfo = glue::glue("{{rowStart}}\u2013{{rowEnd}} of {{rows}} {pagination_title}"),
      pagePrevious = "\u276e",
      pageNext = "\u276f",
    )
    
  }
  
}

# Tables =======================================================================
overall_year_average <- function(df, other_group = NULL) {
  
  if (is.null(other_group)) {
    
    f_tbl <- df |>
      dplyr::summarise(avg_price = mean(average_price), .by = year) |>
      dplyr::mutate(
        YoY = change(avg_price, lag(avg_price, 1)),
        icon_value = dplyr::case_when(
          YoY > 0 ~ "increase", 
          YoY < 0 ~ "decrease",
          YoY == 0 ~ "unchanged"
        )
      )
    
  } else {
    
    f_tbl <- df |>
      dplyr::rename(cat = dplyr::all_of(other_group)) |>
      dplyr::summarise(avg_price = mean(average_price), .by = c(year, cat)) |>
      dplyr::group_split(cat) |>
      purrr::map(\(tbl) {
        
        tbl |>
          dplyr::mutate(
            YoY = change(avg_price, lag(avg_price, 1)),
            icon_value = dplyr::case_when(
              YoY > 0 ~ "increase", 
              YoY < 0 ~ "decrease",
              YoY == 0 ~ "unchanged"
            )
          )
        
      }) |>
      rev() |>
      purrr::list_rbind()
    
  }
  
  f_tbl <- dplyr::filter(f_tbl, year != 2019)
  
  table_columns <- list(
    year = reactable::colDef(
      name = "Year",
      maxWidth = 100,
      cell = \(value) {
        
        if (value == 2023) {
          
          htmltools::div(paste("*", value))
          
        } else {
          
          htmltools::div(value)
          
        }
      }
    ),
    
    avg_price = reactable::colDef(
      name = "Average Price",
      align = "center",
      format = reactable::colFormat(prefix = "₦", digits = 2, separators = TRUE)
    ),
    
    YoY = reactable::colDef(
      header = htmltools::tagList(
        htmltools::span("YoY", "aria-hidden" = "true", title = "Year-on-Year"),
      ),
      align = "center",
      maxWidth = 180,
      minWidth = 165,
      cell = \(value) trend_badge(value)
    ),
    
    icon_value = reactable::colDef(
      name = "",
      sortable = FALSE,
      align = "center",
      maxWidth = 100,
      cell = \(value) trend_indicator(value)
    )
  )
  
  if (!is.null(other_group)) {
    
    table_columns <- append(
      table_columns,
      list(
        cat = reactable::colDef(
          name = clean_label(other_group)
        )
      )
    )
    
  }
  
  reactable::reactable(
    data = f_tbl,
    highlight = TRUE,
    wrap = FALSE,
    paginationType = "simple",
    resizable = TRUE,
    sortable = FALSE,
    columns = table_columns,
    rowStyle = \(index) {
      
      if (!is.null(other_group)) {
        
        if (index == 4) {
          list(borderBottom = "1px solid #A1A1A1")
        }
        
      } else {
        
        NULL
        
      }
    },
    
    theme = table_style("theme")
  )
  
}


single_overall_state_year_average <- function(df) {
  
  outter_table <- df |>
    dplyr::filter(year == 2019) |>
    dplyr::summarise(avg_price = mean(average_price), .by = state_and_capital)
  
  inner_table <- df |>
    dplyr::summarise(avg_price = mean(average_price), .by = c(state_and_capital, year)) |>
    group_yoy("state_and_capital") |>
    dplyr::filter(year != 2019)
  
  
  reactable::reactable(
    data = outter_table,
    searchable = TRUE,
    highlight = TRUE,
    wrap = FALSE,
    paginationType = "simple",
    onClick = "expand",
    resizable = TRUE,
    
    columns = list(
      state_and_capital = reactable::colDef(
        name = "State & FCT",
        cell = \(value) {
          
          htmltools::div(
            value, 
            style = "text-decoration: underline;"
          )
          
        }
      ),
      
      avg_price = reactable::colDef(
        name = "Average Price (2020 - June 2023)",
        format = reactable::colFormat(prefix = "₦", digits = 2, separators = TRUE)
      )
    ),
    
    details = \(index) {
      
      sub_level <- inner_table[inner_table$state_and_capital  == outter_table$state_and_capital [index], ]
      
      reactable::reactable(
        data = sub_level,
        columns = list(
          state_and_capital = reactable::colDef(show = FALSE),
          
          year = reactable::colDef(
            name = "Year",
            maxWidth = 100,
            cell = \(value) {
              
              if (value == 2023) {
                
                htmltools::div(paste("*", value))
                
              } else {
                
                htmltools::div(value)
                
              }
            }
          ),
          
          avg_price = reactable::colDef(
            name = "Average Price",
            format = reactable::colFormat(prefix = "₦", digits = 2, separators = TRUE)
          ),
          
          YoY = reactable::colDef(
            header = htmltools::tagList(
              htmltools::span("YoY", "aria-hidden" = "true", title = "Year on Year"),
            ),
            align = "center",
            maxWidth = 180,
            minWidth = 165,
            cell = \(value) trend_badge(value)
          ),
          
          icon_value = reactable::colDef(
            name = "",
            sortable = FALSE,
            align = "center",
            maxWidth = 100,
            cell = \(value) trend_indicator(value)
          )
        ),
        
        theme = table_style(style = "theme")
      )
      
    },
    
    theme = table_style(style = "theme"),
    language = table_style(style = "lang", "Filter State")
  )
  
}

m_overall_state_year_average <- function(df, gp_var) {
  
  outter_table <- df |>
    dplyr::filter(year != 2019) |>
    dplyr::summarise(
      avg_price = mean(average_price), 
      .by = c(state_and_capital, dplyr::all_of(gp_var))
    ) |>
    tidyr::pivot_wider(
      id_cols = state_and_capital, 
      names_from = dplyr::all_of(gp_var), 
      values_from = avg_price
    )
  
  inner_table <- df |>
    dplyr::summarise(
      avg_price = mean(average_price), 
      .by = c(state_and_capital, year, dplyr::all_of(gp_var))
    ) |>
    group_yoy("state_and_capital", gp_var) |>
    dplyr::rename(cat = gp_var) |>
    dplyr::filter(year != 2019)
  
  outter_columns <- list(
    state_and_capital = reactable::colDef(
      name = "State & FCT",
      maxWidth = 120,
      cell = \(value) {
        
        htmltools::div(
          value, 
          style = "text-decoration: underline;"
        )
        
      }
    )
  )
  
  if (gp_var == "item") {
    
    outter_columns <- append(
      outter_columns,
      list(
        `Small Cylinder (5KG)` = reactable::colDef(
          format = reactable::colFormat(prefix = "₦", digits = 2, separators = TRUE)
        ),
        
        `Medium Cylinder (10KG or 12.5KG)` = reactable::colDef(
          format = reactable::colFormat(prefix = "₦", digits = 2, separators = TRUE)
        )
      )
    ) 
    
  } else {
    
    outter_columns <- append(
      outter_columns,
      list(
        `1 Liter` = reactable::colDef(
          format = reactable::colFormat(prefix = "₦", digits = 2, separators = TRUE)
        ),
        
        `1 Gallon` = reactable::colDef(
          format = reactable::colFormat(prefix = "₦", digits = 2, separators = TRUE)
        )
      )
    ) 
  }
  
  reactable::reactable(
    data = outter_table,
    searchable = TRUE,
    highlight = TRUE,
    wrap = FALSE,
    paginationType = "simple",
    onClick = "expand",
    resizable = TRUE,
    
    columns = outter_columns,
    
    columnGroups  = list(
      reactable::colGroup(
        name = "Average Price (2020 - June 2023)",
        columns = unique(inner_table$cat),
        align = "right",
        headerStyle = list(
          color = "#444444",
          fontWeight = 300,
          "&:hover, &:focus" = list(color = "#333333")
        )
      )
    ),
    
    details = \(index) {
      
      sub_level <- inner_table[inner_table$state_and_capital == outter_table$state_and_capital[index], ]
      
      reactable::reactable(
        data = sub_level,
        highlight = TRUE,
        wrap = FALSE,
        resizable = TRUE,
        
        columns = list(
          state_and_capital = reactable::colDef(show = FALSE),
          
          year = reactable::colDef(
            name = "Year",
            maxWidth = 100,
            cell = \(value) {
              
              if (value == 2023) {
                
                htmltools::div(paste("*", value))
                
              } else {
                
                htmltools::div(value)
                
              }
            }
          ),
          
          cat = reactable::colDef(
            name = clean_label(gp_var),
            resizable = TRUE
          ),
          
          avg_price = reactable::colDef(
            name = "Average Price",
            format = reactable::colFormat(prefix = "₦", digits = 2, separators = TRUE)
          ),
          
          YoY = reactable::colDef(
            header = htmltools::tagList(
              htmltools::span("YoY", "aria-hidden" = "true", title = "Year on Year"),
            ),
            align = "center",
            maxWidth = 180,
            minWidth = 165,
            cell = \(value) trend_badge(value)
          ),
          
          icon_value = reactable::colDef(
            name = "",
            sortable = FALSE,
            align = "center",
            maxWidth = 100,
            cell = \(value) trend_indicator(value)
          )
        ),
        
        rowStyle = \(index) {
          
          if (index == 4) {
            list(borderBottom = "1px solid #A1A1A1")
          }
        },
        
        theme = table_style(style = "theme")
      )
      
    },
    
    theme = table_style(style = "theme"),
    language = table_style(style = "lang", "Filter State & FCT")
  )
  
}


single_overall_state_year_month_average <- function(df) {
  
  f_tbl <- df |>
    dplyr::summarise(
      avg_price = mean(average_price), 
      .by = c(state_and_capital, year, month)
    ) |> 
    dplyr::mutate(MoM = change(avg_price, dplyr::lag(avg_price, n = 1L)), .after = 4) |>
    dplyr::filter(year != 2019) |>
    dplyr::group_by(state_and_capital, month) |>
    dplyr::summarise(dplyr::across(.cols = c(avg_price, MoM), list), .groups = "keep") 
  
  reactable::reactable(
    data = f_tbl,
    highlight = TRUE,
    searchable = TRUE,
    wrap = FALSE,
    paginationType = "simple",
    resizable = TRUE,
    
    columns = list(
      state_and_capital = reactable::colDef(
        name = "State & FCT",
        style = \(value, index) {
          
          if (!index %in% seq(1, 432, 12)) {
            list(color = "#BBBBBB")
          } else {
            list(fontWeight = 500)
          }
          
        }
      ),
      
      month = reactable::colDef(
        name = "Month"
      ),
      
      avg_price = reactable::colDef(
        name = "Average Price",
        align = "center",
        minWidth = 200,
        cell = reactablefmtr::react_sparkline(
          data = f_tbl,
          height = 30,
          line_color = "#666666",
          line_width = 2,
          line_curve = "linear",
          decimals = 2,
          statline = "mean",
          statline_color = "#2E8B57",
          margin = reactablefmtr::margin(l = 40, t = 10, r = 37, b = 10)
        )
      ),
      
      MoM = reactable::colDef(
        align = "center",
        minWidth = 150,
        cell = reactablefmtr::react_sparkline(
          data = f_tbl,
          height = 30,
          line_color = "#555555",
          line_curve = "linear",
          line_width = 2,
          decimals = 2,
          margin = reactablefmtr::margin(l = 40, t = 10, r = 37, b = 10)
        )
      )
    ),
    
    columnGroups = list(
      reactable::colGroup(
        name = "From 2020 - 2023",
        columns = c("avg_price", "MoM"),
        headerStyle = list(color = "#A1A1A1", fontWeight = 300),
        align = "right"
      )
    ),
    
    theme = table_style(style = "theme"),
    language = table_style(style = "lang")
  )
  
}

m_overall_state_year_month_average <- function(df, gp_var) {
  
  inner_table <- df  |>
    dplyr::rename(cat = gp_var) |>
    dplyr::summarise(
      avg_price = mean(average_price), 
      .by = c(state_and_capital, cat, year, month)
    ) |>
    dplyr::group_split(cat) |>
    purrr::map(\(tbl) {
      
      tbl |>
        dplyr::mutate(
          MoM = change(avg_price, dplyr::lag(avg_price, n = 1L)), .after = 5
        )
      
    }) |>
    purrr::list_rbind() |>
    dplyr::filter(year != 2019) |>
    dplyr::group_by(cat, state_and_capital, month) |>
    dplyr::summarise(dplyr::across(.cols = c(avg_price, MoM), list), .groups = "drop")
  
  outter_table <- dplyr::distinct(inner_table, cat)
  
  
  reactable::reactable(
    data = outter_table,
    highlight = TRUE,
    wrap = FALSE,
    resizable = TRUE,
    onClick = "expand",
    
    columns = list(
      cat = reactable::colDef(
        name = clean_label(gp_var),
        sortable = FALSE,
        style = "text-decoration: underline; font-size: 1rem;"
      )
    ),
    
    details = \(index) {
      
      sub_level <- inner_table[inner_table$cat == outter_table$cat[index], ]
      
      reactable::reactable(
        data = sub_level,
        highlight = TRUE,
        wrap = FALSE,
        searchable = TRUE,
        paginationType = "simple",
        resizable = TRUE,
        
        columns = list(
          cat = reactable::colDef(
            show = FALSE
          ),
          
          state_and_capital = reactable::colDef(
            name = "State & FCT",
            style = \(value, index) {
              
              if (!index %in% seq(1, 888, 12)) {
                list(color = "#BBBBBB")
              } else {
                list(fontWeight = 500)
              }
              
            }
          ),
          
          month = reactable::colDef(
            name = "Month"
          ),
          
          avg_price = reactable::colDef(
            name = "Average Price",
            align = "center",
            minWidth = 200,
            cell = reactablefmtr::react_sparkline(
              data = sub_level,
              height = 30,
              line_color = "#666666",
              line_width = 2,
              line_curve = "linear",
              decimals = 2,
              statline = "mean",
              statline_color = "#2E8B57",
              margin = reactablefmtr::margin(l = 40, t = 10, r = 37, b = 10)
            )
          ),
          
          MoM = reactable::colDef(
            align = "center",
            minWidth = 150,
            cell = reactablefmtr::react_sparkline(
              data = sub_level,
              height = 30,
              line_color = "#555555",
              line_curve = "linear",
              line_width = 2,
              decimals = 2,
              margin = reactablefmtr::margin(l = 40, t = 10, r = 37, b = 10)
            )
          )
        ),
        
        columnGroups = list(
          reactable::colGroup(
            name = "From 2020 - 2023",
            columns = c("avg_price", "MoM"),
            headerStyle = list(color = "#A1A1A1", fontWeight = 300, marginRight = 20),
            align = "right"
          )
        ),
        
        theme = table_style(style = "theme"),
        language = table_style(style = "lang")
      )
    },
    
    
    theme = table_style(style = "theme")
  )
  
}


highest_lowest_table <- function(df) {
  
  get_ce <- function(state = TRUE, expensive = TRUE) {
    
    tbl <- dplyr::filter(df, year != 2019)
    
    loc_variable <- ifelse(state, "state_and_capital", "geo_zone")
    
    tbl <- tbl |>
      dplyr::group_by(year, .data[[loc_variable]]) |>
      dplyr::summarise(
        avg_price = round(mean(average_price), 2), 
        .groups = "drop_last"
      )
    
    if (expensive) {
      tbl <- dplyr::slice_max(tbl, order_by = avg_price, n = 5)
    } else {
      tbl <- dplyr::slice_min(tbl, order_by = avg_price, n = 5)
    }
    
    if (state) {
      
      dplyr::ungroup(tbl)
      
    } else {
      
      tbl |>
        dplyr::ungroup() |>
        dplyr::select(
          zone_year = year,
          geo_zone,
          zone_price = avg_price
        )
    }
    
  }
  
  f_tbl <- lapply(c(FALSE, TRUE), \(lg) {
    
    price_lg <- ifelse(lg, "Highest", "Lowest")
    
    get_ce(state = TRUE, expensive = lg) |>
      dplyr::bind_cols(
        get_ce(state = FALSE, expensive = lg)
      ) |>
      dplyr::mutate(price_cat = price_lg, .before = 1)
    
  }) |>
    purrr::list_rbind()
  
  check <- dplyr::filter(f_tbl, year != zone_year)
  
  if (nrow(check) > 0) {
    stop("Year are not aligned")
  } else {
    inner_level <- dplyr::select(f_tbl, -zone_year)
  }
  
  top_level <- df |>
    dplyr::filter(year != 2019) |>
    dplyr::summarise(
      min = min(average_price),
      max = max(average_price),
      .by = year
    ) |>
    dplyr::mutate(dplyr::across(min:max, \(x) round(x, 2)))
  
  
  # Table ------------------------------------------------------------------|
  naira_text <- function(value) {
    htmltools::div(scales::label_comma(0.01, prefix = "₦")(value))
  }
  
  reactable::reactable(
    data = top_level,
    highlight = TRUE,
    wrap = FALSE,
    resizable = TRUE,
    sortable = FALSE,
    onClick = "expand",
    
    columns = list(
      year = reactable::colDef(
        name = "Year",
        maxWidth = 150,
        style = "text-decoration: underline; font-size: 1.1rem;",
        cell = \(value) {
          
          if (value == 2023) {
            
            htmltools::div(paste("*", value))
            # htmltools::div(
            #   style = "display: flex; justify-content: flex-start;",
            #   htmltools::h3("*"),
            #   htmltools::p(
            #     style = "text-decoration: underline; font-size: 1.1rem;",
            #     value
            #   )
            #   
            # )
            
          } else {
            
            htmltools::div(
              # style = "text-decoration: underline; font-size: 1.1rem;",
              value
            )
            
          }
        }
      ),
      
      min = reactable::colDef(
        name = "Minimum",
        cell = \(value) naira_text(value)
      ),
      
      max = reactable::colDef(
        name = "Maximum",
        cell = \(value) naira_text(value)
      )
    ),
    
    details = \(index) {
      
      sub_level <- inner_level[inner_level$year  == top_level$year [index], ]
      
      reactable::reactable(
        data = sub_level,
        highlight = TRUE,
        wrap = FALSE,
        paginationType = "simple",
        resizable = TRUE,
        sortable = FALSE,
        
        defaultColDef = reactable::colDef(
          style = \(value, index, name) {
            
            all_style <- "font-size: 0.8rem;"
            
            if (name %in% c("state_and_capital", "avg_price")) {
              paste(all_style, "background-color: #FCFCFC")
            } else {
              paste(all_style, "background-color: #F2F2F2")
            }
            
          }
        ),
        
        columns = list(
          year = reactable::colDef(show = FALSE),
          state_and_capital = reactable::colDef(name = "State & FCT"),
          avg_price = reactable::colDef(
            name = "Price",
            cell = \(value) naira_text(value)
          ),
          geo_zone = reactable::colDef(name = "Zone"),
          zone_price = reactable::colDef(
            name = "Price",
            cell = \(value) naira_text(value)
          ),
          
          price_cat = reactable::colDef(
            name = "",
            style = \(value, index, name) {
              
              all_style <- "font-size: 0.8rem;"
              
              if (name == "price_cat" & value == "Lowest") {
                paste(all_style, "background-color: #97FFFF")
              } else if (name == "price_cat" & value == "Highest") {
                paste(all_style, "background-color: #EEA9B8")
              }
              
            }
          )
          
        ),
        
        theme = table_style(style = "theme")
      )
    },
    
    columnGroups = list(
      reactable::colGroup(
        name = "Average Price",
        columns = c("min", "max"),
        align = "right",
        headerStyle = "font-size: 1rem; font-weight: 500; color: #BDBDBD;"
      )
    ),
    
    theme = table_style(style = "theme")
  )
  
}


# plots ========================================================================
# cheap and expensive -------------------------------------------------|
highest_Lowest_locs <- function(df, highest = TRUE, multi = FALSE) {
  
  f_tbl <- df |> 
    dplyr::filter(year == 2022) |>
    dplyr::summarise(avg_price = mean(average_price), .by = state_and_capital) 
  
  if (highest) {
    
    f_tbl <- dplyr::slice_max(f_tbl, order_by = avg_price, n = 5)
    
  } else {
    
    f_tbl <- dplyr::slice_min(f_tbl, order_by = avg_price, n = 5)
    
  }
  
  
  if (highest) {
    
    arrange_bar <- "descending" 
    title <- "Highest"
    bar_color <- "#EEA9B8"
    title_color <- "#8B0000"
    text_color <- "#8B0000"
    
  } else {
    
    arrange_bar <- "ascending" 
    title <- "Lowest"
    bar_color <- "#97FFFF"
    title_color <- "#246A73"
    text_color <- "#0B132B"
  }
  
  if (multi) {
    
    font_size <- "0.6rem"
    plt_height <- 270
    
  } else {
    
    font_size <- "0.8rem"
    plt_height <- 290
    
  }
  
  plotly::plot_ly(
    data = f_tbl,
    x = ~state_and_capital, 
    y = ~avg_price,
    height = plt_height
  ) |>
    plotly::add_bars(
      color = I(bar_color),
      text = ~avg_price,
      name = "",
      hovertemplate = glue::glue(
        "<span style='color: {text_color};'><b>%{{x}}</b><br>Price : ₦%{{text:,.2f}}</span>"
      ),
      texttemplate = glue::glue(
        "<span style='color: {text_color}; font-size: {font_size};'>₦%{{y:,.2f}}</span>"
      )
    ) |>
    plotly::layout(
      title = list(
        text = glue::glue("States/FCT {title} Price on Average In 2022"),
        x = 0.1,
        y = 0.97,
        xanchor = "left",
        font = list(
          color = title,
          weight = 600
        )
      ),
      
      xaxis = list(
        zerolinecolor = "#fff",
        zerolinewidth = 2,
        title = "",
        categoryorder = glue::glue("total {arrange_bar}")
      ),
      
      yaxis = list(
        zerolinecolor = "#fff",
        zerolinewidth = 2,
        showticklabels = FALSE,
        title = "",
        showtitle = FALSE
      ),
      hoverlabel = list(font = list(size = 16)),
      autosize = TRUE
    ) |>
    configure_plotly()
  
}


# Overall -------------------------------------------------------------|

ovarall_average_plot <- function(df) {
  
  # Data summary
  f_tbl <- df |>
    dplyr::filter(year != 2019) |>
    dplyr::summarise(avg_price = mean(average_price), .by = date) 
  
  # Extract min & max average price and their respective date.
  min_max <- f_tbl |>
    dplyr::filter(avg_price == min(avg_price) | avg_price == max(avg_price))
  
  # plot colors
  plt_color <- "#2B2D42"
  min_max_color <- "#0B132B"
  text_color <- "#8D99AE"
  
  # convert df to string
  df_label <- unique(df$table_name)
  

  # plot title
  plt_title <- plot_title(df_label)
  
  
  # The plot
  f_tbl |>
    
    plotly::plot_ly(
      x = ~date, y = ~avg_price
    ) |>
    plotly::add_lines(
      color = I(plt_color),
      hovertemplate = "Date: %{x}<br>Price: ₦%{y:,.2f}<extra></extra>"
    ) |>
    plotly::add_trace(
      x = ~c(min_max$date[1]),
      y = ~c(min_max$avg_price[1]),
      type = 'scatter', 
      mode = 'markers', 
      marker = list(color = min_max_color, size = 9),
      hovertemplate = "<span style='color:#FFF5EE'><b>Minimum</b></span><br>Date: %{x}<br>Price: ₦%{y:,.2f}<extra></extra>"
    ) |> 
    plotly::add_trace(
      x = ~c(min_max$date[2]),
      y = ~c(min_max$avg_price[2]),
      type = 'scatter', 
      mode = 'markers', 
      marker = list(color = min_max_color, size = 9),
      hovertemplate = "<span style='color:#FFDAB9'><b>Maximum</b></span><br>Date: %{x}<br>Price: ₦%{y:,.2f}<extra></extra>"
    ) |>
    plotly::layout(
      title = list(
        text = glue::glue("Average Price of {plt_title} (January 2020 - Jun 2023)"),
        x = 0.1,
        xanchor = "left"
      ),
      xaxis = list(title = ""),
      yaxis = list(title = "", tickprefix = "₦"),
      margin = list(t = 50, r = 40, l = 50, b = 60),
      showlegend = FALSE,
      hoverlabel = list(font = list(size = 16))
    ) |>
    configure_plotly()
  
}


# Zone ----------------------------------------------------------------|
zone_average_plot <- function(filtered_zone_df, plt_title = "add title") {
  
  # plot colors
  plt_color <- "#2B2D42"
  text_color <- "#8D99AE"
  min_max_color <- "#0B132B"
  
  # The plot
  filtered_zone_df |>
    
    plotly::plot_ly(
      x = ~date, y = ~avg_price
    ) |>
    plotly::add_lines(
      color = I(plt_color), 
      hovertemplate = "Date: %{x}<br>Price: ₦%{y:,.2f}<extra></extra>"
    ) |>
    plotly::add_trace(
      x = ~mm_date,
      y = ~min_max,
      mode = "markers",
      type = 'scatter',
      marker = list(color = min_max_color, size = 9),
      hovertemplate = "<b>Date: %{x}</b><br><b>Price: ₦%{y:,.2f}</b><extra></extra>"
    )|>
    plotly::layout(
      title = list(
        text = glue::glue("Average Price of {plt_title} (January 2020 - June 2023)"),
        x = 0.1,
        xanchor = "left"
      ),
      xaxis = list(title = ""),
      yaxis = list(title = "", tickprefix = "₦"),
      margin = list(t = 50, r = 40, l = 50, b = 60),
      showlegend = FALSE,
      hoverlabel = list(font = list(size = 16))
    ) |>
    configure_plotly()
  
}


mutiple_zone_average_plot <- function(filtered_zone_df, plt_title = "add title") {

  
  filtered_zone_df |>
    
    plotly::plot_ly(
      x = ~date, y = ~avg_price, 
      color = ~geo_zone #colors = get_colors(selected_locations)
    ) |>
    plotly::add_lines(
      hovertemplate = "Date: %{x}<br>Price: ₦%{y:,.2f}"
    ) |>
    plotly::layout(
      title = list(
        text = glue::glue("{plt_title} Price"),
        x = 0.1,
        y = 0.97,
        xanchor = "left"
      ),
      xaxis = list(title = ""),
      yaxis = list(title = "", tickprefix = "₦"),
      legend = list(
        title = list(text = "Average Price")
      ),
      hoverlabel = list(font = list(size = 16))
    ) |>
    configure_plotly()
}



# State ---------------------------------------------------------------|
single_location_average <- function(filtered_state_df, plt_title = "add title") {

  plotly::plot_ly(
    data = filtered_state_df,
    x = ~date, y = ~avg_price
  ) |>
    plotly::add_lines(
      color = I("#2B2D42"), 
      showlegend = FALSE,
      hovertemplate = "Date: %{x}<br>Price: ₦%{y:,.2f}<extra></extra>"
    ) |>
    plotly::add_trace(
      x = ~mm_date,
      y = ~min_max,
      mode = "markers",
      type = 'scatter',
      marker = list(color = "#0B132B", size = 9),
      hovertemplate = "<b>Date: %{x}</b><br><b>Price: ₦%{y:,.2f}</b><extra></extra>"
    )|>
    plotly::layout(
      title = list(
        text = glue::glue("Price of {plt_title} (January 2020 - June 2023)"),
        x = 0.1,
        y = 0.97,
        xanchor = "left"
      ),
      xaxis = list(title = ""),
      yaxis = list(title = "", tickprefix = "₦"),
      legend = list(
        title = list(text = "Price")
      ),
      hoverlabel = list(font = list(size = 16))
    ) |>
    configure_plotly()
  
}



multiple_location_average <- function(filtered_state_df, plt_title = "add title") {
  
  filtered_state_df |>
    plotly::plot_ly(
      x = ~date, y = ~avg_price, 
      color = ~state_and_capital #colors = get_colors(selected_locations)
    ) |>
    plotly::add_lines(
      hovertemplate = "Date: %{x}<br>Price: ₦%{y:,.2f}"
    ) |>
    plotly::layout(
      title = list(
        text = glue::glue("{plt_title} Price"),
        x = 0.1,
        y = 0.97,
        xanchor = "left"
      ),
      xaxis = list(title = ""),
      yaxis = list(title = "", tickprefix = "₦"),
      legend = list(
        title = list(text = "Price")
      ),
      hoverlabel = list(font = list(size = 16))
    ) |>
    configure_plotly()
  
}


# YoY ==========================================================================
single_yoy <- function(df) {
  
  wide_tbl <- df |>
    tidyr::pivot_wider(
      id_cols = month,
      names_from = year,
      values_from = avg_price
    ) |>
    dplyr::mutate(
      yoy2020 = change(`2020`, `2019`),
      yoy2021 = change(`2021`, `2020`),
      yoy2022 = change(`2022`, `2021`), 
      yoy2023 = change(`2023`, `2022`)
    )  |>
    dplyr::select(
      month, 
      `2020`, yoy2020, 
      `2021`, yoy2021, 
      `2022`, yoy2022, 
      `2023`, yoy2023,
      -`2019`
    ) |>
    tidyr::pivot_longer(
      cols = `2020`:yoy2023,
      names_to = "year",
      values_to = "value"
    )
  
  long_tbl <- wide_tbl |>
    dplyr::filter(year %in% c('2020', '2021', '2022', '2023')) |>
    dplyr::rename(avg_price = value) |>
    dplyr::bind_cols(
      wide_tbl |>
        dplyr::filter(year %in% c("yoy2020", "yoy2021", "yoy2022", "yoy2023")) |>
        dplyr::rename(YoY = value, yearYoY = year) |>
        dplyr::select(-month) 
    ) 
  
  # Check that the values in the year column if they corresponds with the values
  # in the yearYoY column
  
  check_nrow <- long_tbl |>
    dplyr::mutate(yearYoY = stringr::str_remove(yearYoY, "yoy")) |>
    dplyr::filter(year != yearYoY) |>
    nrow()
  
  if (check_nrow > 0) {
    
    stop("values in the year column does not correspond with the values in the
         yearYoY column")
  }
  
  
  long_tbl |>
    dplyr::mutate(year = as.integer(year)) |>
    dplyr::select(year, month, avg_price, YoY, -yearYoY) |>
    dplyr::filter(year != 2023 | !month %in% c("July", "August", "September", "October", "November", "December"))
  
}


single_yoy_table <- function(df) {
  
  f_tbl <- df |>
    dplyr::summarise(avg_price = mean(average_price), .by = c(year, month)) |>
    single_yoy() |>
    dplyr::mutate(
      icon_value = dplyr::case_when(
        YoY < 0 ~ "decrease",
        YoY == 0 ~ "unchanged",
        YoY > 0 ~ "increase"
      ),
      bar_colors = dplyr::case_match(
        year,
        2020 ~ "#273469",
        2021 ~ "#2F2F4F", 
        2022 ~ "#1C2541",
        2023 ~ "#0B132B"
      )
    )
  
  reactable::reactable(
    data = f_tbl,
    sortable = FALSE,
    highlight = TRUE,
    searchable = TRUE,
    wrap = FALSE,
    paginationType = "simple",
    resizable = TRUE,
    
    columns = list(
      year = reactable::colDef(
        name = "Year",
        maxWidth = 100
      ),
      
      month = reactable::colDef(
        name = "Month",
        style = \(value, index) {
          highlight_first_value(
            value, index,
            c(1, 5, 9, 13, 17, 21, 25, 28, 31, 34, 37, 40)
          )
        }
      ),
      
      avg_price = reactable::colDef(
        name = "Average Price",
        cell = reactablefmtr::data_bars(
          data = f_tbl,
          text_position = "above",
          fill_color_ref = "bar_colors",
          bar_height = 5,
          number_fmt = scales::label_comma(0.01, prefix = "₦")
        )
      ),
      
      YoY = reactable::colDef(
        header = htmltools::tagList(
          htmltools::span("YoY", "aria-hidden" = "true", title = "Year-on-Year"),
        ),
        align = "center",
        maxWidth = 180,
        minWidth = 170,
        cell = \(value) trend_badge(value)
      ),
      
      icon_value = reactable::colDef(
        name = "",
        cell = \(value) trend_indicator(value)
      ),
      
      bar_colors = reactable::colDef(
        show = FALSE
      )
    ),
    
    theme = table_style(style = "theme"),
    language = table_style(style = "lang")
  )
  
}



multi_yoy_table <- function(df, gp_var) {
  
  
  f_tbl <- dplyr::rename(df, cat = gp_var)
  
  inner_table <- purrr::map(unique(df[[gp_var]]), \(x) {
    f_tbl |>
      dplyr::filter(cat == x) |>
      dplyr::summarise(avg_price = mean(average_price), .by = c(year, month)) |>
      single_yoy() |>
      dplyr::mutate(
        icon_value = dplyr::case_when(
          YoY < 0 ~ "decrease",
          YoY == 0 ~ "unchanged",
          YoY > 0 ~ "increase"
        ),
        bar_colors = dplyr::case_match(
          year,
          2020 ~ "#273469",
          2021 ~ "#2F2F4F", 
          2022 ~ "#1C2541",
          2023 ~ "#0B132B"
        ),
        cat = x
      )
  }) |>
    purrr::list_rbind()
  
  
  outter_table <- dplyr::distinct(inner_table, cat)
  
  reactable::reactable(
    data = outter_table,
    sortable = FALSE,
    highlight = TRUE,
    wrap = FALSE,
    resizable = TRUE,
    onClick = "expand",
    
    columns = list(
      cat = reactable::colDef(
        name = clean_label(gp_var),
        style = "text-decoration: underline; font-size: 1.1rem;"
      )
    ),
    
    details = \(index) {
      
      sub_level <- inner_table[inner_table$cat  == outter_table$cat [index], ]
      
      reactable::reactable(
        data = sub_level,
        sortable = FALSE,
        highlight = TRUE,
        searchable = TRUE,
        wrap = FALSE,
        paginationType = "simple",
        resizable = TRUE,
        
        columns = list(
          year = reactable::colDef(
            name = "Year",
            maxWidth = 100
          ),
          
          month = reactable::colDef(
            name = "Month",
            style = \(value, index) {
              highlight_first_value(
                value, index,
                c(1, 5, 9, 13, 17, 21, 25, 28, 31, 34, 37, 40)
              )
            }
          ),
          
          avg_price = reactable::colDef(
            name = "Average Price",
            cell = reactablefmtr::data_bars(
              data = sub_level,
              text_position = "above",
              fill_color_ref = "bar_colors",
              bar_height = 5,
              number_fmt = scales::label_comma(0.01, prefix = "₦")
            )
          ),
          
          YoY = reactable::colDef(
            header = htmltools::tagList(
              htmltools::span("YoY", "aria-hidden" = "true", title = "Year-on-Year"),
            ),
            align = "center",
            maxWidth = 180,
            minWidth = 170,
            cell = \(value) trend_badge(value)
          ),
          
          icon_value = reactable::colDef(
            name = "",
            cell = \(value) trend_indicator(value)
          ),
          
          bar_colors = reactable::colDef(
            show = FALSE
          ),
          
          cat = reactable::colDef(
            show = FALSE
          )
        ),
        
        theme = table_style(style = "theme"),
        language = table_style(style = "lang")
      )
    },
    
    theme = table_style(style = "theme")
  ) 
  
}



# MoM ==========================================================================
single_mom <- function(df) {
  
  df |>
    dplyr::mutate(MoM = change(avg_price, lag(avg_price, 1))) |>
    dplyr::filter(year != 2019)
  
}


single_mom_table <- function(df) {
  
  f_tbl <- df |>
    dplyr::summarise(avg_price = mean(average_price), .by = c(year, month)) |>
    single_mom() |>
    dplyr::mutate(
      icon_value = dplyr::case_when(
        MoM < 0 ~ "decrease",
        MoM == 0 ~ "unchanged",
        MoM > 0 ~ "increase"
      ),
      bar_colors = dplyr::case_match(
        year,
        2020 ~ "#273469",
        2021 ~ "#2F2F4F", 
        2022 ~ "#1C2541",
        2023 ~ "#0B132B"
      )
    )
  
  reactable::reactable(
    data = f_tbl,
    sortable = FALSE,
    highlight = TRUE,
    searchable = TRUE,
    wrap = FALSE,
    paginationType = "simple",
    resizable = TRUE,
    
    columns = list(
      year =  reactable::colDef(
        name = "Year",
        maxWidth = 100,
        style = \(value, index) {
          highlight_first_value(
            value, index,
            c(1, 13, 25, 37)
          )
        }
      ),
      
      month =  reactable::colDef(
        name = "Month"
      ),
      
      avg_price =  reactable::colDef(
        name = "Average Price",
        cell = reactablefmtr::data_bars(
          data = f_tbl,
          text_position = "above",
          fill_color_ref = "bar_colors",
          bar_height = 4,
          number_fmt = scales::label_comma(0.01, prefix = "₦")
        )
      ),
      
      MoM =  reactable::colDef(
        header = htmltools::tagList(
          htmltools::span("MoM", "aria-hidden" = "true", title = "Month-on-Month"),
        ),
        align = "center",
        maxWidth = 180,
        minWidth = 170,
        cell = \(value) trend_badge(value)
      ),
      
      icon_value = reactable::colDef(
        name = "",
        cell = \(value) trend_indicator(value)
      ),
      
      bar_colors = reactable::colDef(
        show = FALSE
      )
    ),
    
    theme = table_style(style = "theme"),
    language = table_style(style = "lang")
  )
  
}


multi_mom_table <- function(df, gp_var) {
  
  
  f_tbl <- dplyr::rename(df, cat = gp_var)
  
  
  inner_table <- purrr::map(unique(df[[gp_var]]), \(x) {
    
    f_tbl |>
      dplyr::filter(cat == x) |>
      dplyr::summarise(avg_price = mean(average_price), .by = c(year, month)) |>
      single_mom() |>
      dplyr::mutate(
        icon_value = dplyr::case_when(
          MoM < 0 ~ "decrease",
          MoM == 0 ~ "unchanged",
          MoM > 0 ~ "increase"
        ),
        bar_colors = dplyr::case_match(
          year,
          2020 ~ "#273469",
          2021 ~ "#2F2F4F", 
          2022 ~ "#1C2541",
          2023 ~ "#0B132B"
        ),
        cat = x
      )
    
  }) |>
    purrr::list_rbind()
  
  
  outter_table <- dplyr::distinct(inner_table, cat)
  
  reactable::reactable(
    data = outter_table,
    sortable = FALSE,
    highlight = TRUE,
    wrap = FALSE,
    resizable = TRUE,
    onClick = "expand",
    
    columns = list(
      cat = reactable::colDef(
        name = clean_label(gp_var),
        style = "text-decoration: underline; font-size: 1.1rem;"
      )
    ),
    
    details = \(index) {
      
      sub_level <- inner_table[inner_table$cat  == outter_table$cat [index], ]
      
      reactable::reactable(
        data = sub_level,
        sortable = FALSE,
        highlight = TRUE,
        searchable = TRUE,
        wrap = FALSE,
        paginationType = "simple",
        resizable = TRUE,
        
        columns = list(
          year =  reactable::colDef(
            name = "Year",
            maxWidth = 100,
            style = \(value, index) {
              highlight_first_value(
                value, index,
                c(1, 13, 25, 37)
              )
            }
          ),
          
          month =  reactable::colDef(
            name = "Month"
          ),
          
          avg_price =  reactable::colDef(
            name = "Average Price",
            cell = reactablefmtr::data_bars(
              data = sub_level,
              text_position = "above",
              fill_color_ref = "bar_colors",
              bar_height = 4,
              number_fmt = scales::label_comma(0.01, prefix = "₦")
            )
          ),
          
          MoM =  reactable::colDef(
            header = htmltools::tagList(
              htmltools::span("MoM", "aria-hidden" = "true", title = "Month-on-Month"),
            ),
            align = "center",
            maxWidth = 180,
            minWidth = 170,
            cell = \(value) trend_badge(value)
          ),
          
          icon_value = reactable::colDef(
            name = "",
            cell = \(value) trend_indicator(value)
          ),
          
          bar_colors = reactable::colDef(
            show = FALSE
          ),
          
          cat = reactable::colDef(
            show = FALSE
          )
        ),
        
        theme = table_style(style = "theme"),
        language = table_style(style = "lang")
      )
    },
    
    theme = table_style(style = "theme")
  ) 
  
}