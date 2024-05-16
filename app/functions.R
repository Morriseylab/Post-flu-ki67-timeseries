theme_blank <- function(add_coord = TRUE, xlen_npc = 0.15, ylen_npc = 0.15, xlab = "", ylab = "", lab_size = 12, ...) {
  args1 <- list(
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    legend.background = element_blank(),
    legend.box.margin = margin(0, 0, 0, 0),
    legend.margin = margin(0, 0, 0, 0),
    legend.key.size = unit(10, "pt"),
    plot.margin = margin(lab_size + 2, lab_size + 2, lab_size + 2, lab_size + 2, unit = "points")
  )
  args2 <- as.list(match.call())[-1]
  call.envir <- parent.frame(1)
  args2 <- lapply(args2, function(arg) {
    if (is.symbol(arg)) {
      eval(arg, envir = call.envir)
    } else if (is.call(arg)) {
      eval(arg, envir = call.envir)
    } else {
      arg
    }
  })
  for (n in names(args2)) {
    args1[[n]] <- args2[[n]]
  }
  args <- args1[names(args1) %in% formalArgs(theme)]
  out <- do.call(
    what = theme,
    args = args
  )
  if (isTRUE(add_coord)) {
    g <- grobTree(gList(
      linesGrob(x = unit(c(0, xlen_npc), "npc"), y = unit(c(0, 0), "npc"), arrow = arrow(length = unit(0.02, "npc")), gp = gpar(lwd = 2)),
      textGrob(label = xlab, x = unit(0, "npc"), y = unit(0, "npc"), vjust = 4 / 3, hjust = 0, gp = gpar(fontsize = lab_size)),
      linesGrob(x = unit(c(0, 0), "npc"), y = unit(c(0, ylen_npc), "npc"), arrow = arrow(length = unit(0.02, "npc")), gp = gpar(lwd = 2)),
      textGrob(label = ylab, x = unit(0, "npc"), y = unit(0, "npc"), vjust = -2 / 3, hjust = 0, rot = 90, gp = gpar(fontsize = lab_size))
    ))
    return(list(
      list(annotation_custom(g)),
      list(theme_scp() + out),
      list(coord_cartesian(clip = "off"))
    ))
  } else {
    return(list(
      list(theme_scp() + out)
    ))
  }
}

theme_scp <- function(aspect.ratio = NULL, base_size = 12, ...) {
  text_size_scale <- base_size / 12
  args1 <- list(
    aspect.ratio = aspect.ratio,
    text = element_text(size = 12 * text_size_scale, color = "black"),
    plot.title = element_text(size = 14 * text_size_scale, colour = "black", vjust = 1),
    plot.subtitle = element_text(size = 13 * text_size_scale, hjust = 0, margin = margin(b = 3)),
    plot.background = element_rect(fill = "white", color = "white"),
    axis.line = element_blank(),
    axis.title = element_text(size = 13 * text_size_scale, colour = "black"),
    axis.text = element_text(size = 12 * text_size_scale, colour = "black"),
    strip.text = element_text(size = 12.5 * text_size_scale, colour = "black", hjust = 0.5, margin = margin(3, 3, 3, 3)),
    strip.background = element_rect(fill = "transparent", linetype = 0),
    strip.switch.pad.grid = unit(-1, "pt"),
    strip.switch.pad.wrap = unit(-1, "pt"),
    strip.placement = "outside",
    legend.title = element_text(size = 12 * text_size_scale, colour = "black", hjust = 0),
    legend.text = element_text(size = 11 * text_size_scale, colour = "black"),
    legend.key = element_rect(fill = "transparent", color = "transparent"),
    legend.key.size = unit(10, "pt"),
    legend.background = element_blank(),
    panel.background = element_rect(fill = "white", color = "white"),
    panel.border = element_rect(fill = "transparent", colour = "black", linewidth = 1)
  )
  args2 <- as.list(match.call())[-1]
  call.envir <- parent.frame(1)
  args2 <- lapply(args2, function(arg) {
    if (is.symbol(arg)) {
      eval(arg, envir = call.envir)
    } else if (is.call(arg)) {
      eval(arg, envir = call.envir)
    } else {
      arg
    }
  })
  for (n in names(args2)) {
    args1[[n]] <- args2[[n]]
  }
  args <- args1[names(args1) %in% formalArgs(theme)]
  out <- do.call(
    what = theme,
    args = args
  )
  return(out)
}

