library(tidyverse)
library(httr)
library(xml2)
library(rvest)
library(furrr)

# Getting an artists name
read_html("https://www.vagalume.com.br/green-day/") %>% html_nodes(".darkBG") %>% html_text()

# Scraping data on genre
read_html("https://www.vagalume.com.br/green-day/") %>% html_nodes(".subHeaderTags") %>% 
  as_list() %>% unlist() %>% paste(collapse = "; ")

read_html("https://www.vagalume.com.br/green-day/") %>% html_nodes(".nameMusic") %>% html_text() %>% 
  unique() %>% length()

read_html("https://www.vagalume.com.br/green-day/popularidade/") %>% html_nodes(".text") %>% html_text()

read_html("https://www.vagalume.com.br/green-day/popularidade/") %>% html_nodes(".text") %>% html_text() %>% 
  tail(1) %>% # Extracting last phrase
  str_extract(., "(?<=está em )(.*)(?= pontos)") %>% # Using Regular Expressions to remove the number
  str_replace(",", ".") %>% as.numeric() # Replacing brazilian decimal "," by "." and arsing to numeric

scrap_artist <- function(artist_link){
  # Reading the entire pages
  page <- read_html(paste0("https://www.vagalume.com.br", artist_link))
  pop_page <- read_html(paste0("https://www.vagalume.com.br", artist_link, "popularidade/"))
  
  # Getting desired variables
  A <- page %>% html_nodes(".darkBG") %>% html_text()
    
  G <- page %>% html_nodes(".subHeaderTags") %>% 
    as_list() %>% unlist() %>% paste(collapse = "; ")
  
  S <- page %>% html_nodes(".nameMusic") %>% html_text() %>% 
  unique() %>% length()
  
  P <- pop_page %>% html_nodes(".text") %>% html_text() %>% 
    tail(1) %>% 
    str_extract(., "(?<=está em )(.*)(?= pontos)") %>% 
    str_replace(",", ".") %>% as.numeric() 
  
  # Creating tibble
  res <- tibble(Artist = A, Genres = G, Songs = S, Popularity = P, Link = artist_link)
  return(res)
}

# Testing the scrapper function
scrap_artist("/green-day/")

section_links <- c("/browse/style/rock.html", 
                   "/browse/style/hip-hop.html",
                   "/browse/style/pop.html", 
                   "/browse/style/sertanejo.html", 
                   "/browse/style/funk-carioca.html", 
                   "/browse/style/samba.html")


get_artist_links <- function(section_link){
  # Reading the page
  page <- read_html(paste0("https://www.vagalume.com.br", section_link))
  
  # Removing desired node as a list
  nameList <- page %>% html_nodes(".moreNamesContainer") %>% as_list()
  
  # Removing undesired list levels and extracting 'href' attribute
  # 'NOT ELEGANT AT ALL' ALLERT
  artist_links <- nameList %>% 
    unlist(recursive = F) %>% # Removing first undesired level
    unlist(recursive = F) %>% # Removing second undesired level
    unlist(recursive = F) %>% # Removig third undesired level, ugh
    map(., ~attr(., "href")) %>% # Mapping through list to extract attrs
    unlist() %>% as.character() # Removing names and parsing to a vector
  
  return(unique(artist_links))
}

get_artist_links(section_link = "/browse/style/rock.html") %>% head(10)

# Planning parallel processing
plan(multisession)

# Getting all artists' links from the website
#all_artists_links <- get_artist_links("/browse/style/sertanejo.html")
all_artists_links <- future_map(section_links, ~get_artist_links(.)) %>% unlist()

# De
p_scrap_artist <- possibly(scrap_artist, 
                           otherwise = tibble(Artist = NA, 
                                              Genres = NA, 
                                              Songs = NA, 
                                              Popularity = NA, 
                                              Link = NA))

# Planning parallel processing
plan(multisession)

# Getting all artists' links from the website
all_artists <- future_map_dfr(all_artists_links, ~p_scrap_artist(.))

# Selecting genre labels to keep
genres_keep <- c("Rock", "Hip Hop", "Pop", "Sertanejo", "Funk Carioca", "Samba")


# Removing other genre labels
all_artists_fixed <- all_artists %>% 
  separate(Genres, c("G1", "G2", "G3"), sep = "; ") %>%  # Separating Genres variable
  gather(G1:G3, key = "G", value = "Genre") %>% select(-G) %>% 
  filter(Genre %in% genres_keep)

# Extracts a single lyric from a song link
get_lyric <- function(song_link){
  
  # Reading the html page
  lyric <- read_html(paste0("https://www.vagalume.com.br", song_link)) %>% html_nodes("#lyrics")
  
  # Creating sep to replace linebreaks with ". "
  dummy <- xml_node(read_xml("<doc><span>. </span></doc>"), "span")

  # Replacing line-breaks
  xml_add_sibling(xml_nodes(lyric, "br"), dummy)
  
  res <- lyric %>% html_text()
  
  return(res)
}

# Testing it on Holiday from Green Day
get_lyric("/green-day/holiday.html")

# Extracts all lyrics from an artist link - uses get_lyric()
get_lyrics_links <- function(artist_link){
  
  # Reading the html page on the artist
  page <- read_html(paste0("https://www.vagalume.com.br", artist_link))
  
  # Obtaining all the musics' links -
  music_name_node <- page %>% html_nodes(".nameMusic")
  music_names <- music_name_node %>% html_text()
  music_links <- music_name_node %>% html_attr("href")
  
  # Building final tibble
  res <- tibble(ALink = rep(artist_link, length(music_names)),
                SName = music_names,
                SLink = music_links)
  
  return(res)
}

# Testing final function
get_lyrics_links("/green-day/") %>% head(5)

# Generating safe version of get_lyrics_links()
p_get_lyrics_links <- possibly(get_lyrics_links, 
                               otherwise = tibble(ALink = NA,
                                                  SName = NA))

# Returning all the links to the musics
plan(multisession)
all_songs_links <- future_map_dfr(all_artists_links, ~p_get_lyrics_links(.))

# Gerenating a safe version with possibly()
p_get_lyric <- possibly(get_lyric, otherwise = NA)

# Mapping it through the all_artists_links vector
plan(multisession)
all_lyrics <- future_map_chr(all_songs_links$SLink, ~p_get_lyric(.)[1], .progress = TRUE)

# Adding it to the dataframe
all_songs_links$Lyric <- all_lyrics %>% str_replace_all("\\. \\. ", ". ")
