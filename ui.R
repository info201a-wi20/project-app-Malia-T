ui <- fluidPage (
  theme = shinytheme("slate"),
  includeCSS("style.css"),
  navbarPage(
    title = "Info 201, H4",
    home,
    q1,
    q2,
    q3,
    q4
  )
)
