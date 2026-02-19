value_box_func <- function(titulo, valor, icone, cor) {
  value_box(
    style = "
            border-radius: 15px;
            background: rgba(255, 255, 255, 0.7);
            border: none;
            box-shadow: none;
            backdrop-filter: blur(8px);
            box-shadow: 0 4px 20px rgba(0, 0, 0, 0.3);
            min-height: 14.81481vh;
          ",
    title = tags$p(
      style = "font-size: 0.78125vw; color: #a3aed0;",
      titulo
    ),
    value = tags$p(
      style = "font-size: 2.604167vw; color: #2b3674; font-weight: bold;",
      format(
        valor,
        big.mark = ".",
        decimal.mark = ","
      )
    ),
    showcase = tags$div(
      icon(
        icone,
        lib = "font-awesome",
        style = paste0("color: ", cor)
      )
    )
  )
}