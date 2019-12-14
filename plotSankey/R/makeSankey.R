#' Make a Sankey Plot
#'
#' This functions takes in a data frame and makes a Sankey Plot
#' It assumes that the first column contains risk factors
#' and following columns are each numerical values for each factor for that year
#'
#' This function can handle any number of years and/or risk factors
#' The presense of horizontal and vertical gaps can be adjusted
#'
#' @param d data fram
#' @param white.horizontal defaults to F
#' @param white.vertical defaults to F
#' @param title defaults to NA which uses d[1,1] as title
#' @export
makeSankey <- function(d, white.horizontal = F, white.vertical = F, title = NA){

  #taking in some useful info
  numrows <- nrow(d)
  numcols <- ncol(d)

  #plot title
  if (is.na(title)){title <- colnames(d)[1]}


  years <- colnames(d)

  #using these default colors
  colfil <- c("coral3", "tan3", "darkseagreen", "slateblue4", "skyblue3")
  extracol <- c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666")
  colfil <- rep(c(colfil,extracol), length.out = numrows)


  #reformatting data to long format with 3 cols, r_factor, year, value
  data_long <- tidyr::gather(d,
                      year,
                      value,
                      years[2] : years[numcols],
                      factor_key = T )

  #renaming 1st column name to 'r_factor'
  colnames(data_long)[1] <- 'r_factor'

  #checking data formatting
  data_long$r_factor <- as.factor(data_long$r_factor)
  data_long$year <- as.factor(data_long$year)
  data_long$value <- as.numeric(data_long$value)

  #for white spacing
  if (white.horizontal){gap1 = .05}
  else{gap1 = 0}

  if (white.vertical){gap2 = 1}
  else{gap2 = 0}

  #plotting
  ggplot2::ggplot(data = data_long,
                  ggplot2::aes(y = value + gap1,
                               x = year,
                               alluvium = r_factor,
                               stratum = r_factor)
                  ) +

    #the curves, gap1 determines if gap exists
    ggalluvial::geom_alluvium(ggplot2::aes(fill = r_factor),
                              color = "white",
                              size =  gap1*30,
                              alpha = 1,
                              decreasing = F
                              ) +

    #adding vertical lines, dependent on gap2
    ggplot2::geom_vline(xintercept = seq(from = .75,
                                         to = (numcols-.75),
                                         by = .5),
                        color = "white",
                        alpha = gap2,
                        show.legend = F
                        ) +

    #labeling
    ggplot2::geom_label(ggplot2::aes(label = formatC(round(data_long$value,2),
                                                     format = 'f',
                                                     digits = 2),
                                     size = value,
                                     fill = r_factor),
                        stat = "alluvium",
                        color = "white",
                        fontface = "bold",
                        label.size = NA,
                        show.legend = F,
                        decreasing = F
                        ) +

    #adjusting color fill
    ggplot2::scale_fill_manual(values = colfil) +

    #adjusting text scale
    ggplot2::scale_size_continuous(range = c(1.5, 3)) +

    #removing labels, adding title
    ggplot2::labs(y = "",
                  x = "",
                  title = title) +

    #classic theme doesn't have gray background or grid
    ggplot2::theme_classic() +

    #removing extra graph parts, and adjusting final positioning
    ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_text(face = "bold"),
                   axis.ticks = ggplot2::element_blank(),
                   axis.line = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(face = "bold",
                                                      hjust = 0.5),
                   legend.position = "bottom",
                   legend.title = ggplot2::element_blank()
    )
}
