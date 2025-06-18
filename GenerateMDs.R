# Load necessary packages
library(fs)
library(stringr)
library(knitr)

# Set base path
basePath <- "~/GitHubStuff/jasp-video-library/"
basePathAssets <- "~/GitHubStuff/jasp-video-library/assets/videos/"

outputPath <- "~/GitHubStuff/jasp-video-library/_qmd"  # Folder for output .md files
dir_create(outputPath)

# Only include folders with numbers (e.g., "01 - ANOVA")
allTopFolders <- dir_ls(basePathAssets, type = "directory")
topFoldersWithNumbers <- allTopFolders[str_detect(path_file(allTopFolders), "[0-9]")]

# Function to create a clean title from folder name
make_title <- function(name) {
  name %>% 
    path_file() %>% 
    str_replace_all("_", " ") %>% 
    str_replace_all("-", " – ") %>% 
    str_trim()
}

# Function to clean titles: remove leading numbers and symbols
make_title <- function(name) {
  name %>%
    path_file() %>%
    str_replace("^[0-9]+\\s*[-–:]?\\s*", "") %>%  # remove leading numbers and optional symbols
    str_replace_all("_", " ") %>%
    str_trim()
}


# Iterate through top-level folders
for (folder in topFoldersWithNumbers) {
  folderName <- path_file(folder)
  folderTitle <- make_title(folderName)
  
  # Get all .mp4 files recursively
  mp4Files <- dir_ls(folder, regexp = "\\.mp4$", recurse = TRUE, type = "file")
  
  # Skip if no mp4s
  if (length(mp4Files) == 0) next
  
  # Group mp4s by subfolder
  relPaths <- path_rel(mp4Files, basePathAssets)
  subfolders <- path_dir(relPaths)
  grouped <- split(mp4Files, subfolders)
  
  # Start Markdown content
  mdLines <- c(
    paste0("# ", folderTitle),
    ""
  )
  
  for (subfolder in names(grouped)) {
    # Subchapter title
    subTitle <- if (subfolder == folderName) make_title(folder)  else make_title(path_rel(subfolder, folder))   
    mdLines <- c(mdLines, paste0("## ", subTitle), "")
    
    videoTable <- data.frame(Title = character(), Link = character())
    
    for (filePath in grouped[[subfolder]]) {
      # Path relative to project root for browser link
      relUrl <- path_rel(filePath, start = basePath)
      
      # Human-readable title
      fileTitle <- path_ext_remove(path_file(filePath)) %>%
        str_replace("^[^0-9]*[0-9]+\\s*", "") %>%
        str_replace_all("[-_]", " ") %>%
        str_squish()
      
      # Markdown link that opens in new tab (HTML workaround)
      link <- paste0('<a href="/', relUrl, '" target="_blank">Open Video</a>')
      
      videoTable <- rbind(videoTable, data.frame(Title = fileTitle, Link = link))
    }
    
    # Convert table to Markdown lines
    mdLines <- c(mdLines, knitr::kable(videoTable, format = "markdown"), "")
    
  }
  
  # Write to .md file
  writeLines(mdLines, file.path(outputPath, paste0(folderName, ".qmd")))
}


# Path where your .md files are saved
mdFiles <- list.files(path =outputPath, pattern = "\\.qmd$", full.names = FALSE)

# Strip .md extension and create bullet list for Quarto toc
yamlEntries <- paste0("  - _qmd/", mdFiles)

# Output to console
cat(yamlEntries, sep = "\n")
