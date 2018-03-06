library(shiny)


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  covariate_list <- c("Inpatient",
                      "Male",
                      "CHF",
                      "FluidsLytes",
                      "Paralysis",
                      "PHTN",
                      "Valvular",
                      "Anemia",
                      "NeuroOther",
                      "BloodLoss",
                      "HTN",
                      "Renal",
                      "WeightLoss",
                      "Coagulopathy",
                      "Pulmonary",
                      "Psychoses",
                      "DMcx",
                      "Mets",
                      "Hypothyroid",
                      "Liver",
                      "Depression",
                      "Obesity",
                      "Tumor",
                      "PVD",
                      "DM",
                      "Alcohol",
                      "Drugs",
                      "former Smoking",
                      "Other organ transplantation",
                      "Insertion, revision, replacement, removal of cardiac pacemaker or cardioverter/defibrillator",
                      "Kidney transplant",
                      "Coronary artery bypass graft (CABG)",
                      "Transurethral resection of prostate (TURP)",
                      "Other non-OR therapeutic cardiovascular procedures",
                      "Varicose vein stripping, lower limb",
                      "Other OR therapeutic procedures of urinary tract",
                      "Incision and drainage, skin and subcutaneous tissue",
                      "Other OR heart procedures",
                      "Procedures on the urethra",
                      "Tracheostomy, temporary and permanent",
                      "Other OR procedures on vessels of head and neck",
                      "Heart valve procedures",
                      "Ileostomy and other enterostomy",
                      "Exploratory laparotomy",
                      "Suture of skin and subcutaneous tissue",
                      "Aortic resection, replacement or anastomosis",
                      "Other OR therapeutic procedures, male genital",
                      "Peripheral vascular bypass",
                      "Endoscopy and endoscopic biopsy of the urinary tract",
                      "Hysterectomy, abdominal and vaginal",
                      "Treatment, fracture or dislocation of hip and femur",
                      "Lobectomy or pneumonectomy",
                      "Transurethral excision, drainage, or removal urinary obstruction",
                      "Other OR therapeutic procedures, female organs",
                      "Other OR upper GI therapeutic procedures",
                      "Other vascular bypass and shunt, not heart",
                      "Other hernia repair",
                      "Small bowel resection",
                      "Colostomy, temporary and permanent",
                      "Other OR therapeutic procedures on respiratory system",
                      "Colorectal resection",
                      "Nephrectomy, partial or complete",
                      "Other OR therapeutic nervous system procedures",
                      "Other OR lower GI therapeutic procedures",
                      "Excision, lysis peritoneal adhesions",
                      "Genitourinary incontinence procedures",
                      "Embolectomy and endarterectomy of lower limbs",
                      "Creation, revision and removal of arteriovenous fistula or vessel-to-vessel cannula for dialysis",
                      "Cholecystectomy and common duct exploration",
                      "Skin graft",
                      "Laminectomy, excision intervertebral disc",
                      "Oophorectomy, unilateral and bilateral",
                      "Hip replacement, total and partial",
                      "Other non-OR therapeutic procedures on skin and breast",
                      "Cesarean section",
                      "Other extraocular muscle and orbit therapeutic procedures",
                      "Arthroplasty knee",
                      "Lumpectomy, quadrantectomy of breast",
                      "Other intraocular therapeutic procedures",
                      "PHONE_SCREEN",
                      "Excision of skin lesion",
                      "Decompression peripheral nerve",
                      "Bunionectomy or repair of toe deformities",
                      "Other therapeutic procedures on muscles and tendons",
                      "Repair of retinal tear, detachment",
                      "Arthroplasty other than hip or knee",
                      "Other therapeutic procedures on eyelids, conjunctiva, cornea",
                      "Corneal transplant",
                      "Glaucoma procedures",
                      "Lens and cataract procedures",
                      "Anticoagulants",
                      "Anesthetics",
                      "Antibiotics",
                      "Cardiac medications",
                      "Analgesics")
  
  coef_list <- c(0.8616,0.0701,0.6739,0.5193,0.4191,0.3892,0.2835,0.2538,0.2446,0.2253,0.2098,0.1898,0.1879,0.1638,0.163,0.1338,0.1092,0.1091,0.1023,0.1015,0.0813,0.0653,0.0605,0.0264,0.0044,0.1484,0.0482,0.0385,2.4852,1.9069,1.6115,1.5969,1.5523,1.4565,1.3149,0.9816,0.9611,0.9316,0.9238,0.7921,0.777,0.7321,0.6643,0.6327,0.554,0.5273,0.4896,0.467,0.3739,0.339,0.3248,0.3048,0.3018,0.2924,0.2135,0.2077,0.2055,0.1995,0.1929,0.1856,0.1777,0.1766,0.1526,0.151,0.144,0.0962,0.0797,0.0534,0.0468,0.0445,0.0024,0.0018,-0.0015,-0.0624,-0.064,-0.0781,-0.0937,-0.1034,-0.158,-0.1856,-0.2086,-0.2168,-0.2466,-0.271,-0.3599,-0.386,-0.3871,-0.4756,-0.4843,-0.5744,0.9773,0.0836,0.0698,0.0495,0.0258)

  
  
  output$risk_score <- renderText({
    intermediate_sum <- 0
    for(name in input$covariates){
      intermediate_sum <- intermediate_sum + coef_list[which(covariate_list == name)]
    }
    
    lp <- exp(-(-3.3679 + input$num_meds*0.0345 + input$age*0.0061 + intermediate_sum))
    risk_score <- lp/(1+lp)
    risk_score
})
})

