######## Course info ########
library(tidyverse)

# Start of semester
start_semester <- "2025-07-29"

# Week of mid-semester break
mid_semester_break <- "2025-09-30"

# Schedule
schedule <- tibble(
    Week = seq(12),
    Topic = c(
        "Overview. Why this course? What is EDA?",
        "Learning from history",
        "Initial data analysis and model diagnostics: Model dependent exploration and how it differs from EDA",
        "Using computational tools to determine whether what is seen in the data can be assumed to apply more broadly",
        "Working with a single variable, making transformations, detecting outliers, using robust statistics",
        "Bivariate dependencies and relationships, transformations to linearise",
        "Making comparisons between groups and strata",
        "Going beyond two variables, exploring high dimensions",
        "Exploring data having a space and time context Part I",
        "Exploring data having a space and time context Part II",
        "Sculpting data using models, checking assumptions, co-dependency and performing diagnostics",
        "Help session"
    ),
    Reference = c(
        "The Landscape of R Packages for Automated Exploratory Data Analysis",
        "EDA Case Study: Bay area blues",
        "The initial examination of data",
        "Wickham et al. (2010) Graphical inference for Infovis",
        "Wilke (2019) Ch 6 Visualizing Amounts; Ch 7 Visualizing distributions",
        "Wilke (2019) Ch 12 Visualising associations",
        "Wilke (2019) Ch 9, 10.2-4, 11.2",
        "Cook and Laa (2023) Interactively exploring high-dimensional data and models in R Chapter 1",
        "brolgar: An R package to BRowse Over Longitudinal Data Graphically and Analytically in R",
        "cubble: An R Package for Organizing and Wrangling Multivariate Spatio-temporal Data",
        "How to use a tour to check if your model suffers from multicollinearity",
        ""
    ),
    Reference_URL = c(
        "https://journal.r-project.org/archive/2019/RJ-2019-033/RJ-2019-033.pdf",
        "https://vita.had.co.nz/papers/bay-area-blues.pdf",
        "https://www.jstor.org/stable/2981969?seq=1#metadata_info_tab_contents",
        "https://vita.had.co.nz/papers/inference-infovis.pdf",
        "https://clauswilke.com/dataviz/histograms-density-plots.html",
        "https://clauswilke.com/dataviz/visualizing-associations.html",
        "https://clauswilke.com/dataviz",
        "https://dicook.github.io/mulgar_book/1-intro.html",
        "https://journal.r-project.org/articles/RJ-2022-023/",
        "https://www.jstatsoft.org",
        "https://www.dicook.org/posts/2019-09-13-touring-multicollinearity/",
        ""
    )
)

# Add mid-semester break
calendar <- tibble(
    Date = seq(as.Date(start_semester), by = "1 week", length.out = 13)
) |>
    mutate(
        Week = row_number(),
        Week = if_else(Date <= mid_semester_break, Week, Week - 1),
        # Week =
    )

# Add calendar to schedule
schedule <- schedule |>
    left_join(calendar, by = "Week") |>
    mutate(
        Week = if_else(Date == mid_semester_break, NA, Week),
        Topic = if_else(Date == mid_semester_break, "Mid-semester break", Topic),
        Reference = if_else(Date == mid_semester_break, NA, Reference),
        Reference_URL = if_else(Date == mid_semester_break, NA, Reference_URL)
    ) |>
    select(Week, Date, everything())

# Add assignment details
#lastmon <- function(x) {
#    7 * floor(as.numeric(x - 1 + 4) / 7) + as.Date(1 - 4, origin = "1970-01-01")
#}

assignments <- read_csv(here::here("assignments.csv")) |>
    mutate(
        Date = Due + days(1)
    )

schedule <- schedule |>
    full_join(assignments, by = "Date") |>
    mutate(Week = if_else(is.na(Week) & Date > "2025-10-27", 14, Week)) |>
    mutate(Topic = if_else(Date > "2025-10-27", " ", Topic),
           Reference = if_else(Date > "2025-10-27", " ", Reference)) |>
    #mutate(Assignment = if_else(is.na(Assignment), " ", glue::glue("[{Assignment}]({Link})"))) |>
    select(-Marks)

show_assignments <- function(week) {
    ass <- schedule |>
        filter(
            Week >= week & (week > Week - 3 | week > 8),
            !is.na(Assignment),
        ) |>
        select(Assignment:Link)
    if (NROW(ass) > 0) {
        cat("\n\n## Assignments\n\n")
        for (i in seq(NROW(ass))) {
            cat("* [", ass$Assignment[i], "](", ass$Link[i], ") is due on ",
                format(ass$Due[i], "%A %d %B.\n"),
                sep = ""
            )
        }
    }
}


submit <- function(schedule, assignment) {
    ass <- schedule |>
        filter(Assignment == assignment)
    due <- format(ass$Due, "%e %B %Y") |> stringr::str_trim()
    url <- ass$Moodle
    button <- paste0(
        "<br><br><hr><b>Due: ", due, "</b><br>",
        "<a href=", url, " class = 'badge badge-large badge-blue'>",
        "<font size='+2'>&nbsp;&nbsp;<b>Submit</b>&nbsp;&nbsp;</font><br></a>"
    )
    cat(button)
}
