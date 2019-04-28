
# Necessário instalar R e executar: sudo apt-get install -y libxml2-dev libcurl4-openssl-dev libssl-dev
# Necessário instalar os pacotes abaixo previamente. Ex.: install.packages("tidyverse")

library(tidyverse)
library(httr)
library(xml2)
library(rvest)
library(furrr)


scrap_artist <- function(artist_link){
  # Reading the entire pages
  page <- read_html(paste0("https://www.vagalume.com.br", artist_link))
  pop_page <- read_html(paste0("https://www.vagalume.com.br", artist_link, "popularidade/"))
  
  # Getting desired variables
  A <- page %>% html_nodes(".darkBG") %>% html_text()

  B <- page %>% html_nodes("#artHeaderBreadCrumb") %>% html_children()
  B <- B[2]%>% html_text()
    
  G <- page %>% html_nodes(".subHeaderTags") %>% 
    as_list() %>% unlist() %>% paste(collapse = "; ")
  
  S <- page %>% html_nodes(".nameMusic") %>% html_text() %>% 
  unique() %>% length()
  
  P <- pop_page %>% html_nodes(".text") %>% html_text() %>% 
    tail(1) %>% 
    str_extract(., "(?<=está em )(.*)(?= pontos)") %>% 
    str_replace(",", ".") %>% as.numeric() 
  
  # Creating tibble
  res <- tibble(Artist = A, ParentGenre = B, Genres = G, Songs = S, Popularity = P, Link = artist_link)
  return(res)
}

# Testing the scrapper function

section_links <- c("/browse/style/black-music.html", 
                   "/browse/style/blues.html",
		   "/browse/style/bossa-nova.html",
		   "/browse/style/chillout.html",
		   "/browse/style/classic-rock.html",
		   "/browse/style/classico.html",
		   "/browse/style/country.html",
		   "/browse/style/dance.html",
		   "/browse/style/disco.html",
		   "/browse/style/electro-swing.html",
		   "/browse/style/electronica.html",
		   "/browse/style/emocore.html",
		   "/browse/style/fado.html",
		   "/browse/style/folk.html",
		   "/browse/style/forro.html",
		   "/browse/style/funk.html",
		   "/browse/style/funk-carioca.html",
		   "/browse/style/gospel.html",
		   "/browse/style/gotico.html",
		   "/browse/style/grunge.html",
		   "/browse/style/hard-rock.html",
		   "/browse/style/hardcore.html",
		   "/browse/style/heavy-metal.html",
		   "/browse/style/hip-hop.html",
		   "/browse/style/house.html",
		   "/browse/style/indie.html",
		   "/browse/style/industrial.html",
		   "/browse/style/infantil.html",
		   "/browse/style/instrumental.html",
		   "/browse/style/j-pop.html",
		   "/browse/style/jazz.html",
		   "/browse/style/jovem-guarda.html",
		   "/browse/style/k-pop-k-rock.html",
		   "/browse/style/kizomba.html",
		   "/browse/style/metal.html",
		   "/browse/style/mpb.html",
		   "/browse/style/musicas-gauchas.html",
		   "/browse/style/new-age.html",
		   "/browse/style/new-wave.html",
		   "/browse/style/pagode.html",
		   "/browse/style/piano-rock.html",
		   "/browse/style/pop.html",
		   "/browse/style/pop-punk.html",
		   "/browse/style/pop-rock.html",
		   "/browse/style/pos-punk.html",
		   "/browse/style/post-rock.html",
		   "/browse/style/power-pop.html",
		   "/browse/style/progressivo.html",
		   "/browse/style/psicodelia.html",
		   "/browse/style/punk-rock.html",
		   "/browse/style/r-n-b.html",
		   "/browse/style/rap.html",
		   "/browse/style/reggae.html",
		   "/browse/style/reggaeton.html",
		   "/browse/style/regional.html",
		   "/browse/style/rock.html",
		   "/browse/style/rock-alternativo.html",
		   "/browse/style/rockabilly.html",
		   "/browse/style/romantica.html",
		   "/browse/style/samba.html",
		   "/browse/style/samba-enredo.html",
		   "/browse/style/sertanejo.html",
		   "/browse/style/ska.html",
		   "/browse/style/soft-rock.html",
		   "/browse/style/soul-music.html",
		   "/browse/style/surf-music.html",
		   "/browse/style/tecnopop.html",
		   "/browse/style/trance.html",
		   "/browse/style/trilha-sonora.html",
		   "/browse/style/trip-hop.html",
		   "/browse/style/tropical-house.html",
		   "/browse/style/velha-guarda.html",
		   "/browse/style/world-music.html")

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

# Planning parallel processing
plan(multisession)

# Getting all artists' links from the website
#all_artists_links <- get_artist_links("/browse/style/sertanejo.html")
all_artists_links <- future_map(section_links, ~get_artist_links(.)) %>% unlist()

all_artists_links <- all_artists_links %>% unique()

# De
p_scrap_artist <- possibly(scrap_artist, 
                           otherwise = tibble(Artist = NA, 
					      ParentGenre = NA,
                                              Genres = NA, 
                                              Songs = NA, 
                                              Popularity = NA, 
                                              Link = NA))

# Planning parallel processing
plan(multisession)

# Getting all artists' links from the website
all_artists <- future_map_dfr(all_artists_links, ~p_scrap_artist(.))

write.csv(all_artists,'artists-data.csv',row.names=TRUE)


# Extracts a single lyric from a song link
get_lyric <- function(song_link){

  # Reading the html page
  page  <- read_html(paste0("https://www.vagalume.com.br", song_link))
  lyric <- page %>% html_nodes("#lyrics") 
  
  # Creating sep to replace linebreaks with ". "
  dummy <- xml_node(read_xml("<doc><span>. </span></doc>"), "span")

  # Replacing line-breaks
  xml_add_sibling(xml_nodes(lyric, "br"), dummy)
  
  res <- lyric %>% html_text()

  G <- NA
  

  if(length( page %>% html_nodes(".active"))==1){
    G <- page %>% html_nodes(".active") %>% html_children() %>% html_attr("class")
    G <- G %>% str_replace("lang langBg-", "")
    
  } 
  
  # Creating tibble
  tib <- tibble(Lyric = res, Language = G)
  return(tib)
  
}


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


# Generating safe version of get_lyrics_links()
p_get_lyrics_links <- possibly(get_lyrics_links, 
                               otherwise = tibble(ALink = NA,
                                                  SName = NA))

# Returning all the links to the musics
plan(multisession)
all_songs_links <- future_map_dfr(all_artists_links, ~p_get_lyrics_links(.))

# Gerenating a safe version with possibly()
p_get_lyric <- possibly(get_lyric, otherwise = tibble(Lyric = NA, Language = NA))

# Mapping it through the all_artists_links vector
plan(multisession)
all_lyrics <- future_map_dfr(all_songs_links$SLink, ~p_get_lyric(.), .progress = TRUE)

# Adding it to the dataframe
all_lyrics$Lyric<- all_lyrics$Lyric %>% str_replace_all("\\. \\. ", ". ")
all_songs_links$Lyric <- all_lyrics$Lyric 
all_songs_links$Language <- all_lyrics$Language 

write.csv(all_songs_links,'lyrics-data.csv',row.names=TRUE)





