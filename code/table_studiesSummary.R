library(gt)

df <- data.frame(
  Study = c("1. Spanish","2. German"),
  N = c(length(unique(df1$PID)),length(unique(df2$PID))),
  Female = c(round(femaleS1*100), round(femaleS2*100)),
  Age = c(paste0(round(meanAgeS1, digits = 1),
                 " \u00B1 ",
                 round(sdAgeS1, digits = 1),
                 " (",
                 round(minAgeS1, digits = 1),
                 "-",
                 round(maxAgeS1, digits = 1),
                 ")"),
          paste0(round(meanAgeS2, digits = 1),
                 " \u00B1 ",
                 round(sdAgeS2, digits = 1),
                 " (",
                 round(minAgeS2, digits = 1),
                 "-",
                 round(maxAgeS2, digits = 1),
                 ")")),
  Bistability = c(bistableS1*100,bistableS2*100),
  Days = c(29,21),
  Ocassions = c(6,5),
  LS = c("SWLS.1","SWLS"),
  Res = c("BRS", "BRS"),
  FL = c("FS", "—"),
  Dep = c("PHQ-9", "DASS-21 (Depression)"),
  Anx = c("GAD-7", "DASS-21 (Anxiety)"),
  AAQ = c("AAQ-II", "AAQ-II")
)

colnames(df) <- c("Study", "N", "Female %", "Age", "Bistable %", "Days",
                  "Ocassions", "Life Satisfaction","Resilience","Flourishing",
                  "Depression Symptoms", "Anxiety Symptoms",
                  "Psychological Inflexibility")

# Create the table
tbl <- gt(df)

tbl <- tab_header(tbl, title = md("**Table 1. Summary of the Studies**"))

tbl <- tab_options(tbl,
                   heading.align = "left",
                   row.striping.include_table_body = TRUE,
                   row_group.as_column = TRUE,
                   table.border.top.style = "solid",
                   table.border.top.width = px(1),
                   table.border.bottom.style = "solid",
                   table.border.bottom.width = px(1))

# Group columns under "Study 1" and "Study 2"
tbl <- tab_spanner(tbl, label = "Participants", columns = 2:5)
tbl <- tab_spanner(tbl, label = "EMA Protocol", columns = 6:7)
tbl <- tab_spanner(tbl, label = "Well-being Indicators", columns = 8:13)

tbl <- tab_footnote(tbl, footnote = "FS: Flourishing Scale; BRS: Brief Resilience Scale; SWLS.1: one-item Satisfaction with Life Scale; SWLS: Satisfaction with Life Scale; GAD: Generalized Anxiety Disorder Scale; PHQ-9: Patient Health Questionnaire-9; AAQ-II: Acceptance and Action Questionnaire-II; DASS-21: Depression, Anxiety and Stress Scale.")

tbl