guide_tab <- tabItem(tabName = "guide",
                     withTags({
                       div(
                         class = "main",
                         checked = NA,
                         h1("Guide"),
                         h2("Filtering"),
                         h2("Network view"),
                         h2("Sankey view"),
                         h2("Raw data page"),
                         p("Shows passed data as a table")
                       )

                     }))
