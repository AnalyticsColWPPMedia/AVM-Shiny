
ui <- fluidPage(
  title = "AVM Visualization",
  
  #==================================#
  #### 1. Load Resources (CSS/JS) ####
  #==================================#
  
  # --- Favicon (Browser Tab Icon) ---
  # Ensure 'favicon.ico' (or your .png) is located in the 'www' folder
  tags$link(rel = "shortcut icon", href = "icon.png"),
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$script(src = "custom.js"),
    tags$link(href = "https://fonts.googleapis.com/css?family=Roboto:300,400,500,700&display=swap", 
              rel = "stylesheet")
  ),
  
  #============================#
  #### 2. Custom App Header ####
  #============================#
  
  div(class = "app-header",
      
      # Left Logo
      div(class = "header-left",
          img(src = "wpp_media_logo_white.png", height = "50px") 
      ),
      
      # Center (Titles)
      div(class = "header-center",
          h1(class = "app-title", "AVM Visualization"),
          div(class = "app-subtitle", "By Advanced Analytics Colombia")
      ),
      
      # Right Logo
      div(class = "header-right",
          img(src = "wpp_media_logo_white.png", height = "50px")
      )
  ),
  
  #====================================#
  #### 3. Main Content (Full Width) ####
  #====================================#
  
  div(class = "container-fluid", 
      # Container styling: Full width with padding to align with header
      style = "width: 100%; padding: 0 30px;", 
      
      # Call Module UI
      mod_avm_ui("avm_module_1")
  ),
  
  # Final Spacer
  div(style = "height: 50px;")
)