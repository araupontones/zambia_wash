library(extrafont)

#See all fonts available
windowsFonts()

#Check the fonts that exist in the environment
fonts()

#Import new fonts. For example, in case you download from google fonts set the 
#path to your fonts' directory

font_import(paths = "C:/Users/andre/AppData/Local/Microsoft/Windows/Fonts")

# And then load fonts
loadfonts(device = "win")
