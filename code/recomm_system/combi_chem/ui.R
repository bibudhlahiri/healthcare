shinyUI(fluidPage(
  titlePanel("Choose your optimal combination for chemotherapy"),
  sidebarLayout(
    sidebarPanel(
     fluidRow(
       column(3, radioButtons("gender", label = "Gender",
                 choices = list("Male" = 'MALE', "Female" = 'FEMALE'), selected = 'FEMALE')),
       column(6, radioButtons("ethnicity", label = "Ethnicity",
                 choices = list("Hispanic or Latino" = 'HISPANIC OR LATINO', "Not Hispanic or Latino" = 'NOT HISPANIC OR LATINO'), selected = 'NOT HISPANIC OR LATINO'))
       ),
     fluidRow(

       column(3, radioButtons("race", label = "Race",
                 choices = list("White" = 'WHITE', "Asian" = 'ASIAN', "African-American" = 'BLACK OR AFRICAN AMERICAN'), selected = 'WHITE')),

       column(6, radioButtons("initial_pathologic_diagnosis_method", label = "Initial Pathologic Diagnosis Method",
                 choices = list("Tumor Resection" = 'Tumor resection', "Excisional Biopsy" = 'Excisional Biopsy', "Incisional Biopsy" = 'Incisional Biopsy', 
                       "Fine needle aspiration biopsy" = 'Fine needle aspiration biopsy', "Other method, specify" = 'Other method, specify'), selected = 'Tumor resection')),
      
       column(3, numericInput("age_at_initial_pathologic_diagnosis", "Age at initial pathologic diagnosis:", 59))
    ),
    fluidRow(
       column(6, radioButtons("histological_type", label = "Histological type",
                 choices = list("Treated primary GBM" = 'Treated primary GBM', "Untreated primary (de novo) GBM" = 'Untreated primary (de novo) GBM', 
                                "Glioblastoma Multiforme (GBM)" = 'Glioblastoma Multiforme (GBM)'), selected = 'Untreated primary (de novo) GBM'))
    ),
    fluidRow(
       column(6, radioButtons("history_of_neoadjuvant_treatment", label = "History of Neoadjuvant Treatment",
                 choices = list("Yes" = 'Yes', "No" = 'No'), selected = 'No')),
       column(6, radioButtons("person_neoplasm_cancer_status", label = "Neoplasm Cancer Status",
                 choices = list("With tumor" = 'WITH TUMOR', "Tumor free" = 'TUMOR FREE'), selected = 'WITH TUMOR'))
    ),
    fluidRow(
       column(6, sliderInput("karnofsky_performance_score", label = "Karnofsky Performance Score",
              min = 0, max = 100, value = 80))
    ),
    fluidRow(
       column(6, radioButtons("prior_glioma", label = "Prior Glioma",
                 choices = list("Yes" = 'YES', "No" = 'NO'), selected = 'NO'))
    ),
    fluidRow(  
     
      submitButton("Check Options")
    )
    ),
   mainPanel(
     h3("Ranked Results"),
     tableOutput("ranked_results")
    )
  )
))
