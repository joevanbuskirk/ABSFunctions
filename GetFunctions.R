
GetFunctions <- function(which = 'all', 
                         LHD_Map = FALSE,
                         ABS_API = FALSE,
                         ABS_Geog = FALSE,
                         CatBins = FALSE){
  if(any(LHD_Map, ABS_API, ABS_Geog, CatBins)){
    which <- ""
  }
  
  if(which == 'all'){
    LHD_Map = TRUE
    ABS_API = TRUE
    ABS_Geog = TRUE
    CatBins = TRUE
  }
  
  if(LHD_Map){source('https://raw.githubusercontent.com/joevanbuskirk/ABSFunctions/main/Get_LHD_Map.R')}
  
  if(ABS_API){source('https://raw.githubusercontent.com/joevanbuskirk/ABSFunctions/main/Setup_ABS_API.R')}
  
  if(ABS_Geog){source('https://raw.githubusercontent.com/joevanbuskirk/ABSFunctions/main/Get_ABS_Geography.R')}
  
  if(CatBins){source('https://raw.githubusercontent.com/joevanbuskirk/ABSFunctions/main/CatBins.R')}
}


