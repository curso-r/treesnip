# condition inference trees

    Code
      decision_tree() %>% set_engine("partykit") %>% set_mode("regression")
    Output
      Decision Tree Model Specification (regression)
      
      Computational engine: partykit 
      

---

    Code
      decision_tree() %>% set_engine("partykit", teststat = "maximum") %>% set_mode(
        "classification")
    Output
      Decision Tree Model Specification (classification)
      
      Engine-Specific Arguments:
        teststat = maximum
      
      Computational engine: partykit 
      

# condition inference forests

    Code
      rand_forest() %>% set_engine("partykit") %>% set_mode("regression")
    Output
      Random Forest Model Specification (regression)
      
      Computational engine: partykit 
      

---

    Code
      rand_forest() %>% set_engine("partykit", teststat = "maximum") %>% set_mode(
        "classification")
    Output
      Random Forest Model Specification (classification)
      
      Engine-Specific Arguments:
        teststat = maximum
      
      Computational engine: partykit 
      

